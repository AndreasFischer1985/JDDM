#' Function getCitations
#' 
#' Returns a data.frame containing citations formatted according to APA-style, as well as suggestions for labels and LaTeX-Code to exchange the citations with in order to add labels after each citation.
#' @export


getCitations <- function (text) 
{
    matches1 = JDDM::matchAll(text, "[(][\\w& ,]+( & |[ ]?et al[.]?[ ]?)?[\\w]*, [0-9]{4}[a-z]?[)]", 
        perl = T)[[1]][, 1]
    matches1a = JDDM::matchAll(text, "[^(][\\w& ,]+( & |[ ]?et al[.]?[ ]?)?[\\w]*, [0-9]{4}[a-z]?[)]", 
        perl = T)[[1]][, 1]
    matches1b = JDDM::matchAll(text, "[(][\\w& ,]+( & |[ ]?et al[.]?[ ]?)?[\\w]*, [0-9]{4}[a-z]?[^)]", 
        perl = T)[[1]][, 1]
    matches1c = JDDM::matchAll(text, "[^(][\\w& ,]+( & |[ ]?et al[.]?[ ]?)?[\\w]*, [0-9]{4}[a-z]?[^)]", 
        perl = T)[[1]][, 1]
    matches2 = JDDM::matchAll(text, "([A-Z][\\w]+(, )*)+([ ]?& |[ ]?and |[ ]?et al[.]?[ ]?)?[\\w]* [(][0-9]{4}[a-z]?[)]", 
        perl = T)[[1]][, 1]
    patterns = c(gsub("(^[(;,. ]+|[);,. ]+$)", "", c(matches1, 
        matches1a, matches1b, matches1c)), matches2)
    toSeparate = grep("([0-9]{4}[a-z]?[, ]+)+[0-9]{4}[a-z]?", 
        patterns, value = T)
    if (length(toSeparate) > 0) {
        message("\nPlease note: text contains combined enries.\n")
        print(toSeparate)
    }
    levels = levels(as.factor(patterns))
    labels = gsub("\\W", "", levels)
    citations = gsub("[)]$", "", gsub(" [(]", ", ", patterns))
    table = sort(table(citations))
    result = paste0("\\\\hyperref[", labels, "]{", levels, "}")
    message("\nCitations:\n")
    print(levels(as.factor(citations)))
    return(data.frame(levels, labels, result))
}
