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
    getHTML <- function(url = "https://scholar.google.de/citations?user=-TjY7oEAAAAJ&hl=de&oi=sra", 
        encoding = "UTF-8", save = F, filename = NULL, closeCon = F, 
        method = "libcurl", silent = T, curlGetHeaders = F, browseURL = F, 
        ...) {
        if (closeCon) {
            co = as.numeric(rownames(showConnections(all = T)))
            for (i in co[co > 2]) close(getConnection(i))
        }
        if (!silent) 
            message(paste0("Trying to get html from ", url))
        if (is.character(url)) {
            if (length(grep("(^http://|^https://|^ftp://|^file://)", 
                url)) == 0) 
                url = paste0("http://", url)
            if (curlGetHeaders) 
                message(paste(curlGetHeaders(url, verify = F), 
                  collapse = ""))
            if (browseURL) 
                browseURL(url)
            url = url(url, method = method, ...)
        }
        html = paste(readLines(url, encoding = encoding), collapse = "\n")
        if (save) 
            writeLines(gsub("^<\n", "", gsub("[ ]+", " ", paste0("<", 
                strsplit(html, "<")[[1]]))), paste0(ifelse(is.character(filename), 
                filename, as.numeric(Sys.time())), ".txt"))
        if (closeCon) {
            co = as.numeric(rownames(showConnections(all = T)))
            for (i in co[co > 2]) close(getConnection(i))
        }
        return(invisible(html))
    }
    subsetHTML <- function(html, tag = "div", pattern = NULL, 
        edit = F, save = F, plot = F, filename = NULL, trim = T) {
        if (length(html) > 1) 
            if (length(dim(html)) > 2) 
                stop("please provide HTML as a character vector!")
            else if (length(dim(html)) > 1) {
                warning("two-dimensional input detected. Only first column is used.")
                html = paste(as.character(html[, 1]), collapse = "   ")
            }
            else html = paste(as.character(html), collapse = "   ")
        start = paste0("<", tag, "[\n\r> ]")
        end = paste0("</", tag, ">")
        if (is.null(pattern)) 
            pattern = start
        strings = gsub("[ ]+", " ", paste0("<", strsplit(html, 
            "<")[[1]]))
        if (plot) {
            s1 = sort(table(gsub("(<[/]?|(>| ).*)", "", strings)))
            barplot(s1[s1 > 2], main = "Common Tags")
        }
        infos = gsub("^[ ]*$", "", gsub("^<[^<]*>", "", strings))
        links = character(length(infos))
        links[grep("href=[\"'].*?[\"']", strings)] = gsub("[\"'].*$", 
            "", gsub("^.*?href=[\"']", "", grep("href=[\"'].*?[\"']", 
                strings, value = T)))
        loc.pat = grep(pattern, strings)
        loc.div1 = grep(start, strings)
        loc.div2 = grep(end, strings)
        result0 = character(0)
        result1 = character(0)
        result2 = character(0)
        result3 = character(0)
        for (i1 in loc.pat) {
            loc.div3 = sort(c(loc.div1[which(loc.div1 > i1)], 
                loc.div2[which(loc.div2 > i1)]))
            i2 = i1
            for (i in loc.div3) {
                i2 = i
                if (length(loc.div1[which(loc.div1 > i1 & loc.div1 <= 
                  i)]) < length(loc.div2[which(loc.div2 > i1 & 
                  loc.div2 <= i)])) 
                  break
            }
            string = paste(strings[i1:i2], collapse = "\n")
            info = paste(infos[i1:i2], collapse = "\n")
            link = paste(links[i1:i2], collapse = "\n")
            text = gsub("^( )*(\n)*( )*(\n)*", "", gsub("(\n)+( )*(\n)*", 
                "\n", gsub("<.*?>", "", string)))
            if (trim == T) {
                result0 = c(result0, gsub("[\n]+", "\n", qqBaseX::trim(string)))
                result1 = c(result1, gsub("[\n]+", "\n", qqBaseX::trim(text)))
                result2 = c(result2, gsub("[\n]+", "\n", qqBaseX::trim(info)))
                result3 = c(result3, gsub("[\n]+", "\n", qqBaseX::trim(link)))
            }
            else {
                result0 = c(result0, (string))
                result1 = c(result1, (text))
                result2 = c(result2, (info))
                result3 = c(result3, (link))
            }
        }
        result = data.frame(entry = result0, info = result2, 
            links = result3, stringsAsFactors = F)
        result = result[grep(paste0("^", start), result[, 1]), 
            ]
        if (edit) 
            result = edit(result)
        if (save) {
            write.csv2(data.frame(result), paste0(ifelse(is.character(filename), 
                filename, as.numeric(Sys.time())), ".csv"))
        }
        return(invisible(result))
    }
    matchAll <- function(string = NA, pattern = ".*", value = T, 
        ...) {
        y = gregexpr(pattern, string, ...)
        l = (lapply(y, function(x) rbind(x, attr(x, "match.length"))))
        if (value == F) 
            return(lapply(l, t))
        else t = lapply(1:length(l), function(i) {
            l1 = l[[i]]
            cbind(apply(l1, 2, function(x1) if (x1[2] > -1) 
                return(substr(string[[i]], x1[1], x1[1] + x1[2] - 
                  1))
            else return(matrix(NA, ncol = 0, nrow = 0))))
        })
        return(t)
    }
    archive = getHTML("https://journals.ub.uni-heidelberg.de/index.php/jddm/issue/archive")
    issueLinks = unlist(matchAll(archive, "https://journals.ub.uni-heidelberg.de/index.php/jddm/issue/view/[0-9/]+"))
    issues = unlist(lapply(issueLinks, function(url) {
        print(url)
        html = getHTML(url)
    }))
    articles = unlist(lapply(issues, function(html) {
        articles = unlist(matchAll(html, "https://journals.ub.uni-heidelberg.de/index.php/jddm/article/view/[0-9/]+\\\""))
        articles = gsub("(.*view/|\")", "", grep("view/[0-9]+\\\"", 
            articles, value = T))
    }))
    articleMETADATA = lapply(issues, function(html) {
        x = subsetHTML(html, pattern = "obj_article_summary")[, 
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
        ";;;  "), function(x) qqBaseX::last(x)))
    articleISSUE[numberOfInfos < 3] = NA
    articleMETADATA = data.frame(id = articles, authors = articleAUTHORS, 
        title = articleTITLE, subtitle = articleSUBTITLE, volume = articleVOLUME, 
        issue = articleISSUE)
    articleJSON = lapply(articles, function(id) {
        print(id)
        if (F) 
            h1 = getHTML(paste0("https://statistik.ub.uni-heidelberg.de/oa_statistik/doc_id/item_period/?doc_id=ojs:jddm:", 
                id, "&showmonth=yes"))
        if (F) 
            h2 = getHTML(paste0("https://statistik.ub.uni-heidelberg.de/oa_statistik/doc_id/item_country/?doc_id=ojs:jddm:", 
                id))
        jsonlite::fromJSON(getHTML(paste0("https://statistik.ub.uni-heidelberg.de/oa_statistik/doc_id/item_period/?doc_id=ojs:jddm:", 
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
        x1 = qqBaseX::plotMAT(t(requests[, 1:10]), main = "Top 10 requests", 
            cumsum = T, ylim = c(0, max(colSums(requests, na.rm = T), 
                na.rm = T) * 2))
        dev.new()
        x2 = qqBaseX::plotMAT(t(investigations[, 1:10]), main = "Top 10 investigations", 
            cumsum = T, ylim = c(0, max(colSums(investigations, 
                na.rm = T), na.rm = T) * 2))
    }
    print("requests:")
    print(sort(colSums(requests, na.rm = T)))
    print("intestigations:")
    print(sort(colSums(investigations, na.rm = T)))
    return(articlesData)
}
