#' Function layoutManuscript
#' 
#' Generates a tex-file compatible with JDDMLA03.cls.
#' @export


layoutManuscript <- function (path = "C:/Users/Andreas/Documents/MyLaTeX/texR", doc = "Manuscript.docx", 
    abstractStart = "Abstract\n", abstractEnd = "\nKeywords:", 
    keywordsStart = "\nKeywords:", keywordsEnd = "INTRODUCTION: LITERATURE REVIEW", 
    bodyStart = "INTRODUCTION: LITERATURE REVIEW", bodyEnd = "\nElectronic Supplementary Material\n", 
    referencesStart = "\nReferences\n", referencesEnd = "$", 
    title = "Title", runhead = "Short Title", authors = "Andreas Fischer, Daniel V. Holt, and Joachim Funke", 
    affiliations = "Department of Psychology, Heidelberg University", 
    doi = "10.11588/jddm.2018.1.57846", linenumbers = T, year = as.numeric(substr(Sys.Date(), 
        1, 4)), volume = (as.numeric(substr(Sys.Date(), 1, 4)) - 
        2014), number = 1, type = "Editorial", received = "\\customtoday\n", 
    accepted = "\\customtoday\n", published = "\\customtoday\n", 
    supplement = NULL, declarations = "The authors declare they have no conflict of interests.", 
    contributions = "All authors contributed equally to this paper.", 
    editor = "Andreas Fischer", creativecommons = "This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.", 
    citation = NA, save = T, ...) 
{
    setwd(path)
    file = readtext::readtext(doc)
    text = file[, 2]
    cit = JDDM::getCitations(text)
    text2 = text
    for (i in 1:dim(cit)[1]) text2 = gsub(cit[i, 1], cit[i, 3], 
        text2)
    abstract = JDDM::extractPassage(text2, abstractStart, abstractEnd)
    keywords = JDDM::extractPassage(text2, keywordsStart, keywordsEnd)
    body = JDDM::extractPassage(text2, bodyStart, bodyEnd)
    body = JDDM::addLettrine(body)
    body = JDDM::addSections(body)
    body = JDDM::addFiguresAndTables(body)
    body = gsub("\n", "\n \n", body)
    references = JDDM::extractPassage(text2, referencesStart, 
        referencesEnd)
    references = paste0(paste0("\\label{", cit[, 2], "}", collapse = "\n"), 
        "\n", references)
    references = gsub("\n", "\n \n", references)
    skeleton = JDDM::generateSkeleton(title = title, runhead = runhead, 
        authors = authors, affiliations = affiliations, doi = doi, 
        linenumbers = linenumbers, year = year, volume = volume, 
        number = number, type = type, received = received, accepted = accepted, 
        published = published, supplement = supplement, abstract = abstract, 
        keywords = keywords, introduction = body, references = references, 
        declarations = declarations, contributions = contributions, 
        editor = editor, creativecommons = creativecommons, citation = citation, 
        save = F, ...)
    if (save == T) {
        skeleton = enc2utf8(skeleton)
        skeleton = gsub("<U+200B>", "", skeleton)
        skeleton = gsub("<U+FB00>", "ff", skeleton)
        writeLines(skeleton, con = "Manuscript.tex", useBytes = TRUE)
        message(paste0("Tex-file \"Manuscript.tex\" written to\n", 
            getwd()))
    }
    return(skeleton)
}
