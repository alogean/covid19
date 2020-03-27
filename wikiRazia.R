library(rvest)
library('WikidataR')
library('WikipediR')

URL <- "https://en.wikipedia.org/wiki/Lists_of_prepared_foods"    
list_of_names <- URL %>% 
  read_html %>%
  html_nodes("#mw-content-text h3+ ul a , .column-width a") %>%  html_text()
list_of_urls <- URL %>% 
  read_html %>%
  html_nodes("#mw-content-text h3+ ul a , .column-width a")  %>% html_attr('href')
# an empty list
listed <- list()

for (i in list_of_urls) {
  # here you create the url made by https... + the scraped urls above
  url <- paste0("https://en.wikipedia.org/",i)
  
  # for each url, you'll have a component of the list with the extracted names
  listed[[i]] <- url %>% 
    read_html %>%
    # be sure to get the correct nodes, they seems these
    html_nodes("h2~ ul li > a:nth-child(1) , a a")  %>% html_text()
  print(listed[[i]])
  #Sys.sleep(5)  # very important: you'll add a 15 sec after each link scraped
  # to not overload of requests the site in a small range of time
}

#Retrieve the categories for the "New Age" article on en.wiki
cats <- categories_in_page("en", "wikipedia", pages = "New Age")

#Retrieve the categories for the "New Age" article on rationalwiki.
rw_cats <- categories_in_page(domain = "rationalwiki.org", pages = "New Age")
