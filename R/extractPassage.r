#' Function extractPassage
#' 
#' Returns the substring of a text between the first match of a regular expression startReg and the first match of a regular expression endReg.
#' @export


extractPassage <- function (text, startReg = "\nReferences\n", endReg = "$") 
{
    if (dim(stringr::str_locate(text, startReg))[1] > 1 | dim(stringr::str_locate(text, 
        endReg))[1] > 1) 
        warning("regular expressions match multiple passages. only first passage will be considered.")
    substr(text, stringr::str_locate(text, startReg)[1, 2] + 
        1, stringr::str_locate(text, endReg)[1, 1] - 1)
}
