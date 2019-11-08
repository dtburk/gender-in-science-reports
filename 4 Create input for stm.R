library(stm)
library(tm)
library(data.table)
library(stringr)
library(texanaaid)

###############################################################################
######################## Variables to modify ##################################
###############################################################################
INPUT_TEXTS_DIR <- file.path("C:",
                             "Users",
                             "Derek",
                             "OneDrive",
                             "@Work",
                             "Kathrin Zippel", 
                             "Gender and Science Reports",
                             "text_w_named_entities_removed")
# Change the value of INPUT_TEXTS_DIR to be the path to the directory 
# containing the text files with repetitive phrases removed (from step 3).

OUTPUT_DIR <- file.path("C:",
                              "Users",
                              "Derek",
                              "Dropbox",
                              "CEWS_Plans")
# Change the value of OUTPUT_TEXTS_DIR to be the path to the directory where
# you want to save the .Rdata file containing all objects necessary to run 
# the STM analysis.

CUSTOM_STOPWORDS <- c("und", "der")
# Change the value of CUSTOM_STOPWORDS to include any words that may not be 
# in the default stopwords list, but that you want to remove prior to analysis

LANGUAGE <- "de"

MIN_WORD_LENGTH <- 3
# Change the value of MIN_WORD_LENGTH to equal the minimum length (number of 
# characters) of a word for it to be included in the analysis. Words shorter
# than this length will be removed. If you don't want to exclude any words 
# for being too short, set this value to 1.

MAX_WORD_LENGTH <- Inf
# Change the value of MAX_WORD_LENGTH to equal the maximum length of a word
# for it to be included in the analysis. Words longer than this length will 
# be removed. If you don't want to exclude any words for being too long, set
# this value to Inf.

MIN_WORD_FREQ <- 1
# Change the value of MIN_WORD_FREQ to equal the minimum number of documents 
# in which a word must appear to be included in the analysis. Words that appear 
# in fewer documents will be removed prior to analysis. If you don't want to 
# exclude any words for being too rare, set this value to 1.

MAX_WORD_FREQ <- Inf
# Change the value of MIN_WORD_FREQ to equal the maximum number of documents 
# in which a word can appear to be included in the analysis. Words that appear 
# in more documents will be removed prior to analysis. If you don't want to 
# exclude any words for being too common, set this value to Inf.

###############################################################################
###############################################################################
###############################################################################

# If any dirs don't exist yet, create them
for(DIR in OUTPUT_DIR) {
    if(!(dir.exists(DIR))) dir.create(DIR)
}

all <- VCorpus(DirSource(INPUT_TEXTS_DIR), 
               readerControl = list(language=LANGUAGE))

titles <- sapply(all, meta, "id", USE.NAMES=FALSE)

# Remove the space between dash and next word in hyphenated word
all <- tm_map(all, content_transformer(function(x) str_replace_all(x, "(?<=\\w)- (?=\\w)", "-")))

## Remove web-addresses
url_pattern <- "(http)?www[^ ]+|http[^ ]+"

all <- tm_map(all, content_transformer(function(x) str_replace_all(x, url_pattern, "")))

all_pages <- list()
all_pages_titles <- list()
all_pages_report <- character()

for(i in seq_along(all)) {
    doc <- all[[i]]
    pages <- str_split(doc$content, "\\f")[[1]]
    doc_title <- str_replace(doc$meta$id, "\\.txt$", "")
    token_pattern <- "[\\S]+"
    word_count <- 0L
    page_grp <- integer()
    # Aggregate until you have 3000 words
    for(j in seq_along(pages)) {
        word_count <- word_count + str_count(pages[j], token_pattern)
        page_grp <- c(page_grp, j)
        if(word_count >= 3000 | (word_count > 0 & j==length(pages))) {
            page_grp_title <- sprintf("%s_%d_to_%d", doc_title, head(page_grp, 1), tail(page_grp, 1))
            to_add <- PlainTextDocument(x=str_c(pages[page_grp], collapse=" "), id=page_grp_title, language="en")
            all_pages <- c(all_pages, list(to_add))
            all_pages_titles <- c(all_pages_titles, page_grp_title)
            all_pages_report <- c(all_pages_report, doc_title)
            word_count <- 0L
            page_grp <- integer()
        } else {
            next
        }
    }
}

all_pages <- as.VCorpus(all_pages)
raw_pages <- sapply(all_pages, content)
all_pages_metadata <- data.table(title=all_pages_titles, report=factor(all_pages_report))
rm(all_pages_titles, all_pages_report)

## Remove Stopwords
all_pages <- tm_map(all_pages, removeWords, c(stopwords(kind=LANGUAGE), CUSTOM_STOPWORDS))

## Remove Numbers
all_pages <- tm_map(all_pages, removeNumbers)

## Stem
all_pages <- tm_map(all_pages, stemDocument)

custom_tokenizer <- Token_Tokenizer(tokenize)

dtm <- DocumentTermMatrix(all_pages, control=
                              list(
                                  tokenize=custom_tokenizer, 
                                  #stemming=TRUE, 
                                  bounds=list(global=c(MIN_WORD_FREQ, MAX_WORD_FREQ)), 
                                  wordLengths=c(MIN_WORD_LENGTH, MAX_WORD_LENGTH)#,
                                  #stopwords=custom_stopwords
                                  #weighting=weightTfIdf
                              )
)

dim(dtm)

stm_input <- readCorpus(corpus=dtm, type="slam")

stm_input$meta <- all_pages_metadata

stm_input <- prepDocuments(stm_input$documents, stm_input$vocab, stm_input$meta, 
                           lower.thresh=1)

save(stm_input, dtm, raw_pages, 
     file=file.path(OUTPUT_DIR, "input_for_stm.Rdata"))