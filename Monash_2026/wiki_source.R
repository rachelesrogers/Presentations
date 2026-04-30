
library(dplyr)
library(rvest)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)

class_of_interest <- ".mw-content-ltr" ## ids are #id-name, classes are .class-name

# Finding 150 wikipedia edits ending on August 30, 2024
editurl <- "https://en.wikipedia.org/w/index.php?title=Richard_Marles&action=history&offset=&limit=100"
editclass_of_interest <- ".mw-changeslist-date"

# Save the urls of the full articles
url_list <- editurl %>%
  read_html() %>%
  html_nodes(editclass_of_interest) %>%
  map(., list()) %>%
  tibble(node = .) %>%
  mutate(link = map_chr(node, html_attr, "href") %>% paste0("https://en.wikipedia.org", .))

# Finding 150 wikipedia edits ending on August 1, 2024
# editurl2 <- "https://en.wikipedia.org/w/index.php?title=Tim_Walz&action=history&offset=20240801&limit=150"

# Save the urls of the full articles
# url_list2 <- editurl2 %>%
#   read_html() %>%
#   html_nodes(editclass_of_interest) %>%
#   map(., list()) %>%
#   tibble(node = .) %>%
#   mutate(link = map_chr(node, html_attr, "href") %>% paste0("https://en.wikipedia.org", .))

# Combine url list
# url_list <- rbind(url_list1, url_list2)

# Save text from webpages in data frame
walz_pages <- data.frame(page_notes = rep(NA, dim(url_list)[1]))

for (i in 1:dim(url_list)[1]){
  
  wiki_list <-  url_list$link[i] %>%
    read_html() %>%
    html_node(class_of_interest) %>%
    html_children() %>%
    map(., list()) %>%
    tibble(node = .) %>%
    mutate(type = map_chr(node, html_name)) %>%
    filter(type == "p") %>%
    mutate(text = map_chr(node, html_text)) %>%
    mutate(cleantext = str_remove_all(text, "\\[.*?\\]") %>% str_trim()) %>%
    plyr::summarise(cleantext = paste(cleantext, collapse = "<br> "))
  
  walz_pages$page_notes[i] <- wiki_list$cleantext[1]
  
}

# save texts to .csv file
write.csv(walz_pages, "Monash_2026/data/wiki_pages.csv")

# Find and save dates for referenced wikipedia articles
recent_dates <- editurl %>%
  read_html() %>%
  html_elements(editclass_of_interest) %>% html_text2()

previous_dates <- dates <- editurl2 %>%
  read_html() %>%
  html_elements(editclass_of_interest) %>% html_text2()

write.csv(recent_dates, "Monash_2026/data/wiki_recent_dates.csv")
write.csv(previous_dates, "../data/walz_previous_dates.csv")

# Record dates of article edits within the timeframe
date_url <- "https://en.wikipedia.org/w/index.php?title=Tim_Walz&action=history&offset=20240830&limit=1400"

editclass_of_interest <- ".mw-changeslist-date"

all_dates <- date_url %>%
  read_html() %>%
  html_elements(editclass_of_interest) %>% html_text2()

write.csv(all_dates, "../data/walz_all_dates.csv")
