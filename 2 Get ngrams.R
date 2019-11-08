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
# Change the value of NGRAM_DIR to be the path to the directory where you want
# to save the extracted ngram files (to be used in the next processing step).

LANGUAGE <- "de"

###############################################################################
###############################################################################
###############################################################################

# If NGRAM_DIR doesn't exist yet, create it
if(!(dir.exists(NGRAM_DIR))) dir.create(NGRAM_DIR)


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
rm(txt)

doc_ngrams <- list(length(docs))

for(i_doc in 1:length(docs)) {
    cat(sprintf("Document %d of %d: ", i_doc, length(docs)))
    doc <- docs[[i_doc]]
    txt <- doc$content
    ngram_file <- file.path(NGRAM_DIR, str_replace(doc$meta$id, "\\.txt$", ".Rdata"))
    ngram_vec <- character(0)
    ngram_tabs <- list()
    word_count <- str_count(txt, "\\S+")
    for(i in 8:min(100, word_count)) {
        cat(sprintf("%d ", i))
        ngram_tab <- get.phrasetable(ngram(txt, n=i))
        ngram_tab <- ngram_tab[ngram_tab$freq > 1, ]
        ngram_tabs[[paste0(i, "-grams")]] <- ngram_tab
    }
    cat("\n\n")
    save(ngram_tabs, file=ngram_file)
}