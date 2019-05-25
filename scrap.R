library(dplyr)
library(lubridate)
library(xml2)
library(selectr)
library(rvest)
library(stringr)

url <- "https://timesofindia.indiatimes.com/topic/Hiv/news"
# url = "https://timesofindia.indiatimes.com/topic/HIV-infections/news"
baseurl <- "https://timesofindia.indiatimes.com"
# artlist <- character()
# content <- list(text=character(), pdate = integer())
req_year <- 2018

get_arts <- function(page, req_year){
    dates <-
        html_nodes(page, css = ".meta") %>% 
        html_text() %>%
        as.Date(format = "%d %b %Y")
    art_index <- year(dates)==req_year
    arts <- html_nodes(page, css = "li.article div a") %>%
        html_attr("href") %>%
        sapply(function(x) paste(baseurl, x, sep = ''), USE.NAMES = F)
    art_index[is.na(art_index)] <- F
    # browser()
    tibble(
        link = arts[art_index],
        # art = sapply(
        #     arts[art_index],function(x) get_content(x),
        #     USE.NAMES = F
        # ),
        pdate = as.Date(dates[art_index])
    ) %>%
        mutate(art = vapply(
                 link, function(x) {k=get_content(x);k},
                 character(1),
                 USE.NAMES = F
                 )
        )
}

get_content <- function(contenturl) {
    tryCatch(
        {
            contentpage <- read_html(contenturl)
            # ignore_text <- "Download The Times of India News App for Latest City .Make sense of the 2019 Lok Sabha elections and results on May 23 with TOI. Follow us to track latest news, live updates, news analysis and cutting-edge data analytics. Track live election results, the big trends and fastest updates on counting day with India's largest news network.#ElectionsWithTimes PreviousElection  Data HubPoll  Fantasy GameTOI Campaign  TrackerData StoriesPoll  ShotsOpinion Next" 
            content <- contentpage %>%
                html_nodes(css = "._3WlLe") %>%
                html_text()
            if(length(content)==0)
                return('')
            # content <- substring(content,1, nchar(content)-nchar(ignore_text))
            # content <- sub(ignore_text, "", content, fixed = T)
            content <- substr(
                content,
                start = 1,
                stop = str_locate(content,"Download The Times of India")[1]-1
            )
            content <- sub("[A-Z]*:", "", content) %>%
                str_squish()
            content
        },
        error = function(e){''}
    )
    
}



page <- read_html(url)
artlist <- get_arts(page, req_year)
for(i in 2:20){
    nurl <- paste(url, i, sep = '/')
    print(nurl)
    page <- read_html(nurl)
    print(page)
    artlist <- rbind(artlist,get_arts(page, req_year))
}

write.csv(artlist, "./scraped-data.csv")

artlist <- artlist %>% filter(nchar(art)>0)

write.csv(artlist, "./scraped-data-clean.csv")
