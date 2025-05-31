#' Find fences in a Gutenberg text and select the text between them.
#' 
#' @param text text downloaded from gutenberg
#' @returns clean text without boilerplate from project Gutenberg texts
clean_gutenberg_text <- function (text) {
  # Find fences in text --------------------------------------------------------
  start_fence = "start of the project gutenberg ebook"
  end_fence = "end of the project gutenberg ebook"
  lower_text = tolower(text)
  start_pos = unlist(gregexpr(pattern = start_fence, text)) + nchar(start_fence)
  
  end_pos = unlist(gregexpr(pattern = end_fence, text))
  # Check that the fences are at reasonable positions within the text ----------
  stopifnot(0.000001 < start_pos / nchar(text) <= 0.1)
  stopifnot(0.9 < end_pos / nchar(text) <= 1.0)
  
  cleaned_text = substr(lower_text, start_pos, end_pos)
}

library(stringr)

#' count words in a .txt file
#' 
#' @param file .txt file
#' @param clean_text whether the text has been cleaned
#' @return number of words in the text file
count_words <- function(file, clean_text = FALSE) {
  file.exists(file)
  text = readLines(file)
  if (clean_text) {
    text = clean_gutenberg_text(text)
  }
    
  
}