#' Search for text in R files
#'
#' This function searches for text in text files with (by default) R and Rmd extension.
#' @param string text string to search for. If fixed = FALSE (the default), regular expressions may be used
#' @param dir directory (by default, the working directory)
#' @param extension file extension of files to search, by default .R and .Rmd
#' @param pattern if you want to search for files with a particular name (takes presedence over extension)
#' @param fixed if TRUE, the literal string will be searched for; if FALSE (the default), regular expressions may be used (see ?grep)
#' @param ignore.case if TRUE (default), character case will be ignored (i.e., if you search for "Md" the function will search for "md", "mD" and "MD" as well)
#' @param deep if TRUE, it will also search in subdirectories
#' @keywords string
#' @export
#' @examples
#' search_text("Mod. 1")
#' search_text("image.plot(", "H:/Documents/seksjon 214/2015_01_FjordBloom/Analyser", fixed = TRUE, deep = TRUE)
#' # Use fixed = TRUE when the search text contains e.g. parantheses:
#' search_text("save(", fixed = TRUE)

search_text <- function(string, dir = ".", extension = c("R","Rmd"), pattern = NULL, fixed = FALSE, ignore.case = TRUE, deep = FALSE){
  if (!is.null(extension) & !is.null(pattern))
    warning("Pattern will take predence over extension")
  if (is.null(pattern)) {
    ext <- paste(extension, collapse = "|")
    pattern <- paste0("\\.(", paste(extension, collapse = '|'), ")$")
	}
  fn <- list.files(path = dir, pattern = pattern, full.names = TRUE, recursive = deep)
  search <- plyr::alply(fn, 1, function(x) grep(string,  readLines(x, warn = FALSE), fixed = fixed, ignore.case = ignore.case), .progress = "text")
  fn[plyr::laply(search, length) > 0]	
  }
  
  