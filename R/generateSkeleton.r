#' Function generateSceleton
#' 
#' Generates a tex-file compatible with JDDMLA03.cls.
#' @export


generateSkeleton <- function (title = "Title", runhead = "Short Title", authors = "Andreas Fischer, Daniel V. Holt, and Joachim Funke", 
    affiliations = "Department of Psychology, Heidelberg University", 
    doi = "10.11588/jddm.2018.1.57846", linenumbers = T, year = 2019, 
    volume = (year - 2014), number = 1, type = "Editorial", received = "\\customtoday\n", 
    accepted = "\\customtoday\n", published = "\\customtoday\n", 
    supplement = NULL, abstract = NULL, keywords = NULL, introduction = NULL, 
    references = NULL, declarations = "The authors declare they have no conflict of interests.", 
    contributions = "All authors contributed equally to this paper.", 
    editor = "Andreas Fischer", creativecommons = "This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.", 
    zitieren = "Fischer, A., Holt, D. V., \\& Funke, J. (2018). Web-Scraping the JDDM Database: Citations, Reads and Downloads. \\textit{Journal of Dynamic Decision Making, 4}, 4.  \\href{https://doi.org/10.11588/jddm.2018.1.57846}{DOI: jddm.2018.1.57846}", 
    save = T) 
{
    if (!is.null(introduction)) {
        introduction = gsub("[\\]*&", "&", introduction)
        introduction = gsub("&", "\\\\&", introduction)
        introduction = gsub("<U+200B>", "", introduction)
        introduction = gsub("<U+FB00>", "ff", introduction)
    }
    if (!is.null(references)) {
        references = gsub("[[\\]*&", "&", references)
        references = gsub("&", "\\\\&", references)
        references = gsub("<U+200B>", "", references)
        references = gsub("<U+FB00>", "ff", references)
    }
    text = paste0("\\documentclass{JDDMLA03}\n", "\\usepackage{graphicx}\n", 
        "\\usepackage{lipsum}\n", "\\usepackage{lettrine}\n", 
        "\\usepackage[switch, modulo]{lineno}\n", ifelse(linenumbers, 
            "\\linenumbers\n", "% \\linenumbers\n"), "\\IssueYear{", 
        year, "}\n", "\\Volume{Volume ", volume, "}\n", "\\ArticleNumber{Article ", 
        number, "}\n", "\\ArticleType{", type, "}\n", ifelse(!is.null(received), 
            paste0("\\Received{", received, "}\n"), ""), ifelse(!is.null(accepted), 
            paste0("\\Accepted{", accepted, "}\n"), ""), ifelse(!is.null(published), 
            paste0("\\Received{", published, "}\n"), ""), ifelse(!is.null(supplement), 
            paste0("\\Supplement{", supplement, "}\n"), ""), 
        "\\title{\\begin{flushleft}", title, "\\end{flushleft}}\n", 
        "\\runhead{", runhead, "}\n", "\\author{", authors, "}\n", 
        "\\affiliations{", affiliations, "}\n", "\\PaperDOI{https://doi.org/", 
        doi, "}\n", "\\PaperDOIshort{", doi, "}\n", "\\begin{document}\n", 
        "\\maketitle\n", ifelse(!is.null(abstract), paste0("\\begin{abstract}\n", 
            abstract, "\n\\end{abstract}\n"), ""), ifelse(!is.null(keywords), 
            paste0("\\begin{keywords}\n", keywords, "\n\\end{keywords}\n"), 
            ""), ifelse(!is.null(introduction), paste0("\\begin{introduction}\n", 
            introduction, "\n\\end{introduction}\n"), ""), ifelse(!is.null(declarations), 
            paste0("\\begin{declarations}\n", declarations, "\n\\end{declarations}\n"), 
            ""), ifelse(!is.null(contributions), paste0("\\begin{contributions}\n", 
            contributions, "\n\\end{contributions}\n"), ""), 
        ifelse(!is.null(supplement), paste0("\\begin{supplement}\n", 
            supplement, "\n\\end{supplement}\n"), ""), ifelse(!is.null(editor), 
            paste0("\\begin{editor}\n", editor, "\n\\end{editor}\n"), 
            ""), ifelse(!is.null(creativecommons), paste0("\\begin{creativecommons}\n", 
            creativecommons, "\n\\end{creativecommons}\n"), ""), 
        ifelse(!is.null(zitieren), paste0("\\begin{zitieren}\n", 
            zitieren, "\n\\end{zitieren}\n"), ""), "\\begin{dates}\n", 
        "\\end{dates}\n", ifelse(!is.null(references), paste0("\\begin{references}\n", 
            references, "\n\\end{references}\n"), ""), "\\urlstyle{sf}\n", 
        "\\vfill\n", "\\end{document}\n")
    if (save) {
        write(text, file = paste(Sys.Date(), "manuscript JDDM.tex"))
        message(paste0("Tex.file \"", Sys.Date(), " manuscript JDDM.tex\" written to\n", 
            getwd()))
    }
    return(text)
}
