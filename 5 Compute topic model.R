library(stm)
library(tm)
library(data.table)
library(stringr)
library(texanaaid)

###############################################################################
######################## Variables to modify ##################################
###############################################################################
INPUT_FILE <- file.path("C:",
                             "Users",
                             "Derek",
                             "Dropbox",
                             "CEWS_Plans", 
                             "input_for_stm.Rdata")
# Change the value of INPUT_FILE to be the path to the file containing all 
# the objects necessary to compute the topic model (output from step 4).

NUM_TOPICS <- 5
# Change the value of NUM_TOPICS to equal the number of topics that you want
# STM to find in the documents.

OUTPUT_DIR <- file.path("C:",
                        "Users",
                        "Derek",
                        "Dropbox",
                        "CEWS_Plans", 
                        "results")
# Change the value of OUTPUT_TEXTS_DIR to be the path to the directory in
# which you want to put the subdirectory that will contain output from the 
# STM model

###############################################################################
###############################################################################
###############################################################################

results_dir <- file.path(OUTPUT_DIR, paste0(NUM_TOPICS, " Topics"))
for(DIR in c(OUTPUT_DIR, results_dir)) {
    if(!dir.exists(DIR)) dir.create(DIR)
}


load(file=INPUT_FILE)
# This command loads an object named "stm_input" from a single file
# in .Rdata format. This object contains all the document texts in 
# the preferred STM format as well as the metadata associated with 
# each document.

stm_results <- stm(stm_input$documents, stm_input$vocab, K=NUM_TOPICS, 
                                                         prevalence=~report,
                                                         data=stm_input$meta)


save(stm_results, file=file.path(results_dir, "stm_results.Rdata"))

create_topic_model_report(stm_results, 
                          results_dir, 
                          stm_input$meta, 
                          raw_pages)
