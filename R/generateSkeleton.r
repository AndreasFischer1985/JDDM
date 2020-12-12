#' Function generateSceleton
#' 
#' Generates a tex-file compatible with JDDMLA03.cls.
#' @export


generateSkeleton <- function (title = "Title", runhead = "Short Title", authors = "Andreas Fischer, Daniel V. Holt, and Joachim Funke", 
    affiliations = "Department of Psychology, Heidelberg University", 
    doi = "10.11588/jddm.2018.1.57846", linenumbers = T, year = as.numeric(substr(Sys.Date(), 
        1, 4)), volume = (as.numeric(substr(Sys.Date(), 1, 4)) - 
        2014), number = 1, type = "Editorial", received = "\\customtoday\n", 
    accepted = "\\customtoday\n", published = "\\customtoday\n", 
    supplement = NULL, abstract = NULL, keywords = NULL, introduction = NULL, 
    references = NULL, declarations = "The authors declare they have no conflict of interests.", 
    contributions = "All authors contributed equally to this paper.", 
    editor = "Andreas Fischer", creativecommons = "This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.", 
    citation = NA, save = T, replaceUnderscore = T) 
{
    shorten = function(authors) {
        li = strsplit(authors, "([,&]|and)")[[1]]
        li = gsub("(^[ ]*|[ ]*$)", "", li[nchar(li) > 1])
        li = sapply(li, function(x) {
            li2 = strsplit(x, " ")[[1]]
            paste0(li2[length(li2)], ", ", paste(sapply(li2[-length(li2)], 
                function(x) paste0(substr(x, 1, 1), ".")), collapse = ""))
        })
        paste(li, collapse = ", ")
    }
    if (is.na(citation)) {
        citation = paste0(shorten(authors), " (", year, "). ", 
            title, ".\\textit{Journal of Dynamic Decision Making, ", 
            volume, "}, ", number, ".  \\href{https://doi.org/", 
            doi, "}{DOI: ", gsub(".*[^(jddm)]", "", doi), "}")
    }
    if (!is.null(abstract)) {
        abstract = gsub("[\\]*&", "&", abstract)
        abstract = gsub("&", "\\\\&", abstract)
        abstract = gsub("​", "", abstract)
        abstract = gsub("ﬀ", "ff", abstract)
    }
    if (!is.null(keywords)) {
        keywords = gsub("[\\]*&", "&", keywords)
        keywords = gsub("&", "\\\\&", keywords)
        keywords = gsub("​", "", keywords)
        keywords = gsub("ﬀ", "ff", keywords)
    }
    if (!is.null(introduction)) {
        introduction = gsub("[\\]*&", "&", introduction)
        introduction = gsub("&", "\\\\&", introduction)
        introduction = gsub("​", "", introduction)
        introduction = gsub("ﬀ", "ff", introduction)
    }
    if (!is.null(references)) {
        references = gsub("[[\\]*&", "&", references)
        references = gsub("&", "\\\\&", references)
        references = gsub("​", "", references)
        references = gsub("ﬀ", "ff", references)
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
            paste0("\\begin{keywords}\\textbf{Keywords:}\n", 
                keywords, "\n\\end{keywords}\n"), ""), ifelse(!is.null(introduction), 
            paste0("\\begin{introduction}\n", introduction, "\n\\end{introduction}\n"), 
            ""), ifelse(!is.null(declarations), paste0("\\begin{declarations}\n", 
            declarations, "\n\\end{declarations}\n"), ""), ifelse(!is.null(contributions), 
            paste0("\\begin{contributions}\n", contributions, 
                "\n\\end{contributions}\n"), ""), ifelse(!is.null(supplement), 
            paste0("\\begin{supplement}\n", supplement, "\n\\end{supplement}\n"), 
            ""), ifelse(!is.null(editor), paste0("\\begin{editor}\n", 
            editor, "\n\\end{editor}\n"), ""), ifelse(!is.null(creativecommons), 
            paste0("\\begin{creativecommons}\n", creativecommons, 
                "\n\\end{creativecommons}\n"), ""), ifelse(!is.null(citation), 
            paste0("\\begin{zitieren}\n", citation, "\n\\end{zitieren}\n"), 
            ""), "\\begin{dates}\n", "\\end{dates}\n", ifelse(!is.null(references), 
            paste0("\\begin{references}\n", references, "\n\\end{references}\n"), 
            ""), "\\urlstyle{sf}\n", "\\vfill\n", "\\end{document}\n")
    if (replaceUnderscore == T) 
        text = gsub("_", "\\_", text)
    if (save) {
        text = enc2utf8(text)
        text = gsub("​", "", text)
        text = gsub("ﬀ", "ff", text)
        writeLines(text, con = "Manuscript.tex", useBytes = TRUE)
        message(paste0("Tex-file \"Manuscript.tex\" written to\n", 
            getwd()))
    }
    return(text)
}
