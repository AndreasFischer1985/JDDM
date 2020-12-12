#' Function extractPassage
#' 
#' Returns the substring of a text between the first match of a regular expression startReg and the first match of a regular expression endReg.
#' @export


extractPassage <- function (text, startReg = "\nReferences\n", endReg = "$") 
{
    if (dim(JDDM::matchOne(text, startReg, value = F))[1] > 1 | 
        dim(JDDM::matchOne(text, endReg, value = F))[1] > 1) 
        warning("regular expressions match multiple passages. only first passage will be considered.")
    substr(text, JDDM::matchOne(text, startReg, value = F)[1, 
        2] + 1, JDDM::matchOne(text, endReg, value = F)[1, 1] - 
        1)
}
