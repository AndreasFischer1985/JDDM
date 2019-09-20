#' Function addLettrine
#' 
#' Replaces the first character with a LaTeX-letttrine.
#' @param text Character element.
#' @keywords helper
#' @export
#' @examples
#' lettrine("Hello world")

addLettrine <- function (text) 
sub(substr(gsub("(^[ \n]*|[ \n]*$)", "", text), 1, 1), paste0("\\\\lettrine{\\\\sffamily ", 
    substr(gsub("(^[ \n]*|[ \n]*$)", "", text), 1, 1), "}{}"), 
    gsub("(^[ \n]*|[ \n]*$)", "", text))
