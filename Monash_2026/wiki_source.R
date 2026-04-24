
library(rvest)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

class_of_interest <- ".mw-content-ltr" ## ids are #id-name, classes are .class-name

editurl <- "https://en.wikipedia.org/w/index.php?title=Josie_Baff&action=history&offset=&limit=50"
editclass_of_interest <- ".mw-changeslist-date"

# Save the urls of the full articles
url_list <- editurl %>%
  rvest::read_html() %>%
  rvest::html_nodes(editclass_of_interest) %>%
  purrr::map(., list()) %>%
  tibble::tibble(node = .) %>%
  dplyr::mutate(link = purrr::map_chr(node, html_attr, "href") %>% paste0("https://en.wikipedia.org", .))


# create a data frame with the text of the documents
wiki_pages <- data.frame(page_notes = rep(NA, dim(url_list)[1]))

for (i in 1:dim(url_list)[1]){
  
  wiki_list <-  url_list$link[i] %>%
    rvest::read_html() %>%
    rvest::html_node(class_of_interest) %>%
    rvest::html_children() %>%
    purrr::map(., list()) %>%
    tibble::tibble(node = .) %>%
    dplyr::mutate(type = purrr::map_chr(node, html_name)) %>%
    dplyr::filter(type == "p") %>%
    dplyr::mutate(text = purrr::map_chr(node, html_text)) %>%
    dplyr::mutate(cleantext = stringr::str_remove_all(text, "\\[.*?\\]") %>% stringr::str_trim()) %>%
    plyr::summarise(cleantext = paste(cleantext, collapse = "<br> "))
  
  wiki_pages$page_notes[i] <- wiki_list$cleantext[1]
  
}

View(wiki_pages)

write.csv(wiki_pages, "Monash_2026/data/wiki_pages.csv")
