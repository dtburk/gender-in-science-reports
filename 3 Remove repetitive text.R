library(tm)
library(stringr)
library(texanaaid)
library(ngram)


###############################################################################
######################## Variables to modify ##################################
###############################################################################
INPUT_TEXTS_DIR <- file.path("C:",
                             "Users",
                             "Derek",
                             "Dropbox",
                             "CEWS_Plans", 
                             "converted_to_text")
# Change the value of INPUT_TEXTS_DIR to be the path to the directory 
# containing the plain text files extracted from the PDFs in step 1.

NGRAM_DIR <- file.path("C:", 
                       "Users",
                       "Derek",
                       "Dropbox",
                       "CEWS_Plans",
                       "ngrams")
# Change the value NGRAM_DIR to be the path to the directory where you saved 
# the ngram files (in ".Rdata" format) in step 2.

OUTPUT_TEXTS_DIR <- file.path("C:",
                              "Users",
                              "Derek",
                              "Dropbox",
                              "CEWS_Plans", 
                              "texts_w_repetitive_text_removed")
# Change the value of OUTPUT_TEXTS_DIR to be the path to the directory where
# you want to save the documents after removing repetitive phrases from them.

REPETITIVE_TEXT_DIR <- file.path("C:",
                                 "Users",
                                 "Derek",
                                 "Dropbox",
                                 "CEWS_Plans", 
                                 "repetitive_text_removed")
# Change the value of REPETITIVE_TEXT_DIR to be the path to the directory
# where you want to save a record of the phrases removed from the documents.

HEADER_FOOTER_TEXT_DIR <- file.path("C:",
                                    "Users",
                                    "Derek",
                                    "Dropbox",
                                    "CEWS_Plans", 
                                    "header_footer_text_removed")
# Change the value of HEADER_FOOTER_TEXT_DIR to be the path to the directory
# where you want to save a record of the header/footer phrases removed from 
# the documents.

LANGUAGE <- "de"

###############################################################################
###############################################################################
###############################################################################

# If any dirs don't exist yet, create them
for(DIR in c(OUTPUT_TEXTS_DIR, REPETITIVE_TEXT_DIR, HEADER_FOOTER_TEXT_DIR)) {
    if(!(dir.exists(DIR))) dir.create(DIR)
}

# Load texts
docs <- VCorpus(DirSource(INPUT_TEXTS_DIR, pattern="\\.txt$"), 
                readerControl = list(language=LANGUAGE))

for(i in seq_along(docs)) {
    txt <- docs[[i]]$content
    if(length(txt) > 1) {
        txt <- str_c(txt, collapse=" ")
    }
    docs[[i]]$content <- str_replace_all(txt, "[ \n\t\r]+", " ")
}

doc_ngrams <- list(length(all))

repetitive_text_removed <- list()
repetitive_header_footer_text_removed <- list()

for(i_doc in 1:length(docs)) {
    cat(sprintf("Document %d of %d\n", i_doc, length(docs)))
    doc <- docs[[i_doc]]
    txt <- doc$content
    ngram_file <- file.path(NGRAM_DIR, str_replace(doc$meta$id, "\\.txt$", ".Rdata"))
    n_pages <- str_count(txt, "\\f") + 1
    ngram_vec <- character(0)
    load(ngram_file)
    for(ng in ngram_tabs) {
        ng$ngrams <- sapply(ng$ngrams, function(x) {
            Encoding(x) <- "UTF-8"
            return(x)
        })
        ngram_vec <- c(ngram_vec, ng[ng$freq > max((n_pages/4), 5), "ngrams"])
    }
    ng_order <- order(nchar(ngram_vec), decreasing=TRUE)
    ng_sorted <- ngram_vec[ng_order]
    cat(paste0(ng_sorted, collapse="\n"), file=file.path(REPETITIVE_TEXT_DIR, 
                                                        doc$meta$id))
    repetitive_text_removed[[doc$meta$id]] <- ng_sorted
    if(length(ng_sorted) > 0) {
        for(s in ng_sorted) {
            rplc <- if(str_detect(s, "\\f")) "\f" else ""
            txt <- str_replace_all(txt, fixed(s), rplc)
        }
    }
    
    # Should also remove ngrams that appear too often near the beginning or ends of pages
    # Grab 30 words on either side of form-feed characters (don't distinguish between 
    # before and after, because this seems to get messed up in the conversion process)
    # token_pattern <- "\\b[\\S\\f]+?\\b|\\S+"
    token_pattern <- "[\\S]+|\\f"
    page_breaks <- str_locate_all(txt, "\\f")[[1]][,1]
    all_words <- str_locate_all(txt, token_pattern)[[1]]
    page_break_word_nums <- which(all_words[,1] %in% page_breaks)
    header_footer_text <- paste(sapply(page_break_word_nums, function(i) {
        start_loc <- all_words[max(1, i-40), 1]
        stop_loc <- all_words[min(i+40, nrow(all_words)), 2]
        return(str_sub(txt, start_loc, stop_loc))
        }
    ), collapse=" ")
    header_footer_text <- str_replace_all(header_footer_text, "\\f", "")
    # Get ngrams with n 2-40
    flagged <- matrix(nrow=0, ncol=2)
    for(i in 2:40) {
        ngram_tab <- get.phrasetable(ngram(header_footer_text, n=i))
        # Flag any ngram that appears in more than max(n_pages/4, 5) pages
        flagged <- rbind(flagged, ngram_tab[ngram_tab$freq > max((n_pages/4), 5), c("ngrams", "freq")])
    }
    if(nrow(flagged)==0) next
    # flagged_no_subsets <- matrix(nrow=0, ncol=2)
    # for(i in 1:nrow(flagged)) {
    #     if(sum(str_detect(flagged[ , 1], fixed(flagged[i, 1]))) == 1) {
    #         flagged_no_subsets <- rbind(flagged_no_subsets, flagged[i, ])
    #     }
    # }
    flagged_order <- order(nchar(flagged[ , 1]), decreasing=TRUE)
    flagged_sorted <- flagged[flagged_order, ]
    # Remove characters added by ngram
    flagged_sorted[ , 1] <- str_replace_all(flagged_sorted[ , 1], "Â", "")
    # Remove it (from around page breaks) if more than 80% of occurrences are within
    # 40 words from page breaks
    counts <- str_count(txt, fixed(flagged_sorted[,1]))
    to_remove <- flagged_sorted[flagged_sorted$freq > 0.8*counts, "ngrams"]
    cat(paste0(to_remove, collapse="\n"), file=file.path(HEADER_FOOTER_TEXT_DIR, doc$meta$id))
    repetitive_header_footer_text_removed[[doc$meta$id]] <- to_remove
    
    # Split txt into two vectors: one containing text not within 40 words of a form-feed (in order), 
    # and the other containing text that *is* within 40 words of a form-feed.
    header_footer_text <- character(length(page_break_word_nums))
    other_text <- character(length(page_break_word_nums) + 1)
    for(i in seq_along(page_break_word_nums)) {
        n <- page_break_word_nums[i]
        if(i==length(page_break_word_nums)) {
            final_page_lt_41_words <- nrow(all_words) - n <= 40
            other_text[i+1] <- if(final_page_lt_41_words) "" else str_sub(txt, all_words[n+41, 1], nchar(txt))
            n_plus_1 <- nrow(all_words) + 1
        } else n_plus_1 <- page_break_word_nums[i+1]
        next_page_lt_41_words <- n_plus_1 <= (n + 41)
        head_foot_stop <- if(next_page_lt_41_words) all_words[n_plus_1 - 1, 2] else all_words[n+40, 2]
        if(i==1) {
            first_page_lt_41_words <- n <= 41
            if(first_page_lt_41_words) {
                other_text[1] <- ""
                header_footer_text[1] <- str_sub(txt, 1, head_foot_stop)
                next
            } else {
                other_text[1] <- str_sub(txt, 1, all_words[n-41, 2])
                header_footer_text[1] <- str_sub(txt, all_words[n-40, 1], head_foot_stop)
                next
            }
        } else {
            n_minus_1 <- page_break_word_nums[i-1]
            prev_page_lt_41_words <- (n_minus_1 + 41) >= n
            prev_page_lt_81_words <- (n_minus_1 + 81) >= n
            head_foot_start <- if(prev_page_lt_41_words) all_words[n, 1] else all_words[max((n_minus_1 + 41), (n-40)), 1]
            other_text[i] <- if(prev_page_lt_81_words) "" else str_sub(txt, all_words[n_minus_1 + 41, 1], all_words[n-41, 2])
            header_footer_text[i] <- str_sub(txt, head_foot_start, head_foot_stop)
        }
    }
    
    # Remove to_remove from header_footer_text
    for(s in to_remove) {
        rplc <- if(str_detect(s, "\\f")) "\f" else ""
        header_footer_text <- str_replace_all(header_footer_text, fixed(s), rplc)
    }
    txt <- paste(head(other_text, -1), header_footer_text, sep=" ", collapse=" ")
    cat(txt, file=file.path(OUTPUT_TEXTS_DIR, doc$meta$id))
}
