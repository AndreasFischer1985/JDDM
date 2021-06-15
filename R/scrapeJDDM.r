#' Function scrapeJDDM
#' 
#' Downloads and returns data and metadata from the database of the Journal of Dynamic Decision Making.
#' @param plot Logical vakue specifying wether to plot data on reads and downloads.
#' @details Downloads and returns data and metadata from the database of the Journal of Dynamic Decision Making.
#' @keywords scraping
#' @export
#' @examples
#' s=scrapeJDDM();

scrapeJDDM <- function (plot = T) 
{
    archive = JDDM::getHTML("https://journals.ub.uni-heidelberg.de/index.php/jddm/issue/archive")
    issueLinks = unlist(JDDM::matchAll(archive, "https://journals.ub.uni-heidelberg.de/index.php/jddm/issue/view/[0-9/]+"))
    issues = unlist(lapply(issueLinks, function(url) {
        print(url)
        html = getHTML(url)
    }))
    articles = unlist(lapply(issues, function(html) {
        articles = unlist(JDDM::matchAll(html, "https://journals.ub.uni-heidelberg.de/index.php/jddm/article/view/[0-9/]+\\\""))
        articles = gsub("(.*view/|\")", "", grep("view/[0-9]+\\\"", 
            articles, value = T))
    }))
    articleMETADATA = lapply(issues, function(html) {
        x = JDDM::subsetHTML(html, pattern = "obj_article_summary")[, 
            "info"]
        x = gsub("PDF$", "", gsub("[\n\r\t]+", ";;;  ", x))
        x
    })
    articleVOLUME = rep(length(articleMETADATA):1, sapply(articleMETADATA, 
        length))
    numberOfInfos = unlist(lapply(strsplit(unlist(articleMETADATA), 
        ";;;  "), length))
    i1 = unlist(lapply(strsplit(unlist(articleMETADATA), ";;;  "), 
        function(x) x[[1]]))
    i2 = unlist(lapply(strsplit(unlist(articleMETADATA), ";;;  "), 
        function(x) x[[2]]))
    i3 = unlist(lapply(strsplit(unlist(articleMETADATA), ";;;  "), 
        function(x) ifelse(length(x) >= 3, x[[3]], NA)))
    i4 = unlist(lapply(strsplit(unlist(articleMETADATA), ";;;  "), 
        function(x) ifelse(length(x) >= 4, x[[4]], NA)))
    articleTITLE = i1
    articleSUBTITLE = i2
    articleSUBTITLE[numberOfInfos < 4] = NA
    articleAUTHORS = i2
    articleAUTHORS[numberOfInfos >= 4] = i3[numberOfInfos >= 
        4]
    articleISSUE = unlist(lapply(strsplit(unlist(articleMETADATA), 
        ";;;  "), function(x) JDDM::last(x)))
    articleISSUE[numberOfInfos < 3] = NA
    articleMETADATA = data.frame(id = articles, authors = articleAUTHORS, 
        title = articleTITLE, subtitle = articleSUBTITLE, volume = articleVOLUME, 
        issue = articleISSUE)
    articleJSON = lapply(articles, function(id) {
        print(id)
        if (F) 
            h1 = JDDM::getHTML(paste0("https://statistik.ub.uni-heidelberg.de/oa_statistik/doc_id/item_period/?doc_id=ojs:jddm:", 
                id, "&showmonth=yes"))
        if (F) 
            h2 = JDDM::getHTML(paste0("https://statistik.ub.uni-heidelberg.de/oa_statistik/doc_id/item_country/?doc_id=ojs:jddm:", 
                id))
        jsonlite::fromJSON(JDDM::getHTML(paste0("https://statistik.ub.uni-heidelberg.de/oa_statistik/doc_id/item_period/?doc_id=ojs:jddm:", 
            id)))
    })
    names(articleJSON) = articles
    articlesData = lapply(articleJSON, function(json) {
        json[[1]][[1]]$years[, c("id", "investigations", "requests")]
    })
    articlesData = lapply(articlesData, function(x) x[max(sapply(articlesData, 
        function(d) dim(d)[1])):1, ])
    daten = do.call(cbind, articlesData)
    rownames(daten) = max(sapply(articlesData, function(d) dim(d)[1])):1
    years = daten[, length(articleTITLE) * 3 - 2]
    requests = daten[, 1:length(articleTITLE) * 3]
    colnames(requests) = articleAUTHORS
    rownames(requests) = years
    requests[requests == 0] = 0
    requests = requests[, order(colSums(requests, na.rm = T), 
        decreasing = T)]
    investigations = daten[, 1:length(articleTITLE) * 3 - 1]
    colnames(investigations) = articleAUTHORS
    rownames(investigations) = years
    investigations[investigations == 0] = 0
    investigations = investigations[, order(colSums(investigations, 
        na.rm = T), decreasing = T)]
    if (plot == T) {
        dev.new()
        x1 = JDDM::plotMAT(t(requests[, 1:10]), main = "Top 10 requests", 
            cumsum = T, ylim = c(0, max(colSums(requests, na.rm = T), 
                na.rm = T) * 2))
        dev.new()
        x2 = JDDM::plotMAT(t(investigations[, 1:10]), main = "Top 10 investigations", 
            cumsum = T, ylim = c(0, max(colSums(investigations, 
                na.rm = T), na.rm = T) * 2))
    }
    print("requests:")
    print(sort(colSums(requests, na.rm = T)))
    print("intestigations:")
    print(sort(colSums(investigations, na.rm = T)))
    return(articlesData)
}
