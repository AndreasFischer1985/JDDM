#' Function addSections
#' 
#' Declares paragraphs with a few number of words as LaTeX-sections.
#' @param text Character element.
#' @keywords helper
#' @export
#' @examples
#' sections("Hello\nworld")

addSections <- function (text) 
{
    text = strsplit(text, "\n")[[1]]
    spaces = nchar(gsub("[^ ]", "", text))
    text[spaces == 0 & nchar(text) > 0] = paste0("\\section*{", 
        text[spaces == 0 & nchar(text) > 0], "}")
    text[spaces > 0 & spaces <= 5 & nchar(text) > 0] = paste0("\\subsection*{", 
        text[spaces > 0 & spaces <= 5 & nchar(text) > 0], "}")
    paste0(text, collapse = "\n")
}
