library(tm) # This loads functions from package "tm", for text mining
# Use `install.packages` function to install packages listed above, if 
# not already installed (e.g., `install.packages("tm")`)

###############################################################################
#################### Variables to Modify ######################################
###############################################################################
PDF_DIRECTORY <- file.path("C:", 
                           "Users", 
                           "Derek", 
                           "Dropbox", 
                           "CEWS_Plans")
# Change the value of PDF_DIRECTORY to be the path to the directory containing
# the files to be analyzed, as an absolute path OR relative to the current 
# working directory. 
# The working directory is displayed in the title bar of the "Console" window, 
# or can be displayed with the command `getwd()`.
# Note: the function `file.path` combines all its arguments into a file path, 
# taking into account what operating system you are using.


OUTPUT_DIRECTORY <- file.path("C:", 
                              "Users", 
                              "Derek", 
                              "Dropbox", 
                              "CEWS_Plans", 
                              "converted_to_text")
# Change the value of OUTPUT_DIRECTORY to be the path to the directory 
# in which you want to save the documents as text files. Again, this 
# path can be absolute or relative to the current working directory.

LANGUAGE <- "de"
# Change the value of LANGUAGE to the language of the documents to be analyzed

###############################################################################
###############################################################################
###############################################################################



###############################################################################
# The script below performs the following operations:

# 1. Generates a list of file paths to each PDF document to be converted.
# 2. Creates a custom PDF reader function to convert the PDFs to text.
# 3. Applies the PDF reader function to each document.
# 4. Saves the converted documents to individual text files.
###############################################################################

# 1. Generate a list of file paths to each PDF document to be converted.
file_paths <- list.files(PDF_DIRECTORY, 
                         pattern="\\.pdf$|\\.PDF$", 
                         full.names=TRUE)
# Note: `list.files` takes a folder/directory as its first argument
# and returns a character vector of the filenames in that folder;
# the "pattern" argument is optional, and filters the filenames
# to only include those that match the specified regular expression;
# the "full.names" argument is also optional, and if set to TRUE, the function
# returns the full path to each file (relative to the working directory), 
# rather than just the file name.


# 2. Create a custom PDF reader function to convert the PDFs to text.
my_readPDF <- readPDF(control=list(
                                   info=NULL, 
                                   text=c("-raw")
                                  )
                     )

# 3. Apply the PDF reader function to each document.
all_reports <- lapply(file_paths, function(fp) {
    my_readPDF(elem=list(uri=fp), language=LANGUAGE)
})
# Note: `lapply` (for "list-apply") applies the function specified in its second 
# argument to each element of the sequence (aka "iterable") specified in its 
# first argument, and returns the result as a new sequence (of class "list", to 
# be exact).


# 4. Save the converted documents to individual text files.
for(report in all_reports) {
    cat(paste0(report$content, collapse=" "),
        file=file.path(OUTPUT_DIRECTORY, 
                       gsub("\\.pdf$", ".txt", report$meta$id)
                       )
        )
}
# Notes: 
# `cat` can be used to print raw output to the console, or to a file
# if the file argument is specified (as here). 

# `paste0` pastes together characters/strings. Here, we only specify one 
# object (a sequence of lines read from a PDF file and converted to text) 
# with the collapse argument set to equal a single space, which combines 
# the elements of the sequence into one character string, with the elements 
# separated by single spaces. If we had not specified a value for collapse, 
# `paste0` would have returned our sequence unmodified.

# `gsub` looks in its third argument for the regular expression pattern 
# in its first argument, and replaces it with the string specified in the 
# second argument. Here, we replace the file suffix ".pdf" with ".txt"
