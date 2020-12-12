#' Function addFiguresAndTables
#' 
#' Replaces notions like "insert table" or "insert figure" with a blanco LaTeX-figure (fig) oder -table (tab).
#' @param text Character element.
#' @keywords helper
#' @export
#' @examples
#' addFiguresAndTable("please insert Figure 1 about here")

addFiguresAndTables <- function (text, captions = NA, captions2 = NA, format = "pdf") 
{
    m = max(as.numeric(gsub("[^0-9]", "", unlist(JDDM::matchAll(text, 
        "([Ii]nsert|[Aa]dd) [Ff]igure [0-9]+")))))
    if (m <= 0) 
        m = 0
    print(paste(m, "Figure(s) inserted"))
    if (is.na(captions) | length(captions) != m) 
        captions = rep(" ", m)
    if (m > 0) 
        for (i in 1:m) {
            text = gsub(paste0("[^\n]*([Ii]nsert|[Aa]dd) [Ff]igure ", 
                i, "[^\n]*"), paste0("\\\\begin{figure*}[t!]\n\\\\centering\n\\\\includegraphics[width=0.9\\\\textwidth]{figures/fig", 
                i, ".", format[1], "}\n\\\\caption{", captions[i], 
                "}\n\\\\label{fig", i, "}\\\\end{figure*}"), 
                text)
            if (!dir.exists("figures")) 
                dir.create("figures")
            if (!file.exists(paste0("figures/fig", i, ".", format))) {
                pdf(paste0("figures/fig", i, ".", format))
                plot(0, 0, xlab = "", ylab = "", axes = F, type = "n")
                text(0, 0, paste0("fig", i, ".", format))
                dev.off()
            }
        }
    m = max(as.numeric(gsub("[^0-9]", "", unlist(JDDM::matchAll(text, 
        "([Ii]nsert|[Aa]dd) [Tt]able [0-9]+")))))
    if (m <= 0) 
        m = 0
    print(paste(m, "Table(s) inserted"))
    if (is.na(captions2) | length(captions2) != m) 
        captions2 = rep(" ", m)
    if (m > 0) 
        for (i in 1:m) {
            text = gsub(paste0("[^\n]*([Ii]nsert|[Aa]dd) [Tt]able ", 
                i, "[^\n]*"), paste0("\\\\begin{table*}[t!]\n\\\\centering\n\\\\includegraphics[width=0.9\\\\textwidth]{tables/tab", 
                i, ".", format[1], "}\n\\\\caption{", captions2[i], 
                "}\n\\\\label{tab", i, "}\\\\end{table*}"), text)
            if (!dir.exists("tables")) 
                dir.create("tables")
            if (!file.exists(paste0("tables/tab", i, ".", format))) {
                pdf(paste0("tables/tab", i, ".", format))
                plot(0, 0, xlab = "", ylab = "", axes = F, type = "n")
                text(0, 0, paste0("tab", i, ".", format))
                dev.off()
            }
        }
    text
}
