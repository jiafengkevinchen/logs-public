#Write an R function that takes as input a string containing a path to a text file
# Read in the text file, which is assumed to contain Latex
# Remove all lines before a line beginning with \begin{tabular}
# Remove all lines after a line beginning with \end{tabular}
# Save the text between the \begin{tabular} and \end{tabular} lines into the file path

extract_tabular_file <- function(path){
    # Read in the text file, which is assumed to contain Latex
    text <- readLines(path)

    #If the file does not contain a tabular environment, return without doing anything else
    if(!any(grepl("\\\\begin\\{tabular\\}", text))){
        return()
    }

    # Remove all lines before a line beginning with \begin{tabular}
    text <- text[which(grepl("^\\\\begin\\{tabular\\}", text)):length(text)]
    # Remove all lines after a line beginning with \end{tabular}
    text <- text[1:which(grepl("^\\\\end\\{tabular\\}", text))]
    # Save the text between the \begin{tabular} and \end{tabular} lines into the file path
    writeLines(text, path)
}

#Create a function extract tabular that takes a regular expression of filepaths
# and applies the extract_tabular_file function to each file matching the regular expression

extract_tabular <- function(dir = ".",regex){
    # Get all files matching the regular expression
    files <- list.files(path = dir, pattern = regex, full.names = T)
    # Apply the extract_tabular_file function to each file matching the regular expression
    lapply(files, extract_tabular_file)
}


#Write an R function that takes an input a string containing a path to a text file
# Read in the text file, which is assumed to contain Latex
# Convert \begin{longtable} to \begin{tabular}
# Convert \end{longtable} to \end{tabular}
# Save the text into the file path

convert_longtable_file <- function(path){
    # Read in the text file, which is assumed to contain Latex
    text <- readLines(path)

    #If the file does not contain a longtable environment, return without doing anything else
    if(!any(grepl("\\\\begin\\{longtable\\}", text))){
        return()
    }

    # Convert \begin{longtable} to \begin{tabular}
    text <- gsub("\\\\begin\\{longtable\\}", "\\\\begin\\{tabular\\}", text)
    # Convert \end{longtable} to \end{tabular}
    text <- gsub("\\\\end\\{longtable\\}", "\\\\end\\{tabular\\}", text)
    # Save the text into the file path
    writeLines(text, path)
}

#Create a function convert_longtable that takes a regular expression of filepaths

convert_longtable <- function(dir = ".",regex){
    # Get all files matching the regular expression
    files <- list.files(path = dir, pattern = regex, full.names = T)
    # Apply the convert_longtable_file function to each file matching the regular expression
    lapply(files, convert_longtable_file)
}


#Write an R function that takes an input a string containing a path to a text file
# Read in the text file, which is assumed to contain Latex
# Find occurences of numbers inside $s and remove the $s
# Save the text into the file path

remove_mathmode_helper <- function(path){
    # Read in the text file, which is assumed to contain Latex
    text <- readLines(path)

    #If the file does not contain a $, return without doing anything else
    if(!any(grepl("\\$", text))){
        return()
    }

    # Find occurences of numbers, periods, or minuses inside $s and remove the $s
    text <- gsub("\\$([0-9\\.-]+)\\$", "\\1", text)
    # Save the text into the file path
    writeLines(text, path)
}

#Create a function remove_mathmode that takes a regular expression of filepaths
remove_mathmode <- function(dir = ".",regex){
    # Get all files matching the regular expression
    files <- list.files(path = dir, pattern = regex, full.names = T)
    # Apply the remove_mathmode_helper function to each file matching the regular expression
    lapply(files, remove_mathmode_helper)
}
