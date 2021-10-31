library(rvest)
library(XMl)
library(stringr)
library(dplyr)
library(RSQLite)
library(DBI)

#db connection
conn = dbConnect(RSQLite::SQLite(), "J:/R PROJECTS/lubimyczytac/lc.db")

#create table if needed
dbExecute(conn,
          "CREATE TABLE IF NOT EXISTS BOOKS  (
          ID INT PRIMARY KEY NOT NULL,
          AUTOR TEXT NOT NULL,
          TYTUL TEXT NOT NULL,
          LICZBA_OCEN INT NOT NULL,
          SREDNIA_OCEN INT NOT NULL,
          SERIA TEXT,
          URL TEXT,
          RODZAJ INT
          )")



extract_sth = function(url, nodes) {
  read_html(url) %>% html_nodes(nodes) %>% html_text() %>% gsub('[\t\n]', '', .) %>% trimws(.)
  
}



url_to_cat = "https://lubimyczytac.pl/katalog/kategoria/53?page=15&listId=booksFilteredList&category[]=53&rating[]=7&rating[]=10&publishedYear[]=2018&publishedYear[]=2021&catalogSortBy=ratings-desc&paginatorType=Standard"
#extract list of categories
list_cats = read_html(url_to_cat) %>% html_nodes('.filtr__item') %>% html_nodes('.filtr__itemLabel') %>% html_elements("input") %>%
  html_attrs() %>% bind_rows(.) %>% filter(class == "filtr__itemCheckbox catsuball js-inputSubcat")
list_cats = list_cats %>% select(value, `category-name`)
dbWriteTable(conn, "CATEGORIES", list_cats)


year_from = 1900
year_to = 2021
min_rating = 6
max_rating = 10
category_id = 53



url = paste0("https://lubimyczytac.pl/katalog/kategoria/53?page=1&listId=booksFilteredList&onlyPublished=1&rating%5B0%5D=",min_rating,
             "&rating%5B1%5D=",max_rating,"&publishedYear%5B0%5D=",year_from,"&publishedYear%5B1%5D=",year_to,
             "&category%5B0%5D=",category_id,"&catalogSortBy=published-desc&paginatorType=Standard")







full_tbl = data.frame()

max_page = extract_sth(url, '.page-link')
max_page = max_page[length(max_page) - 1]

if (length(max_page) == 0) {
  max_page = 1
}


for (i in 1:10) {
  
  url = gsub('(page=)(\\d+)', paste0("page=", i), url)
  print(url)
  
  titles = extract_sth(url, '.authorAllBooks__singleTextTitle')
  authors = extract_sth(url, '.authorAllBooks__singleTextAuthor')
  
  rating = extract_sth(url, '.listLibrary__ratingStarsNumber')
  rating = as.numeric(gsub(",", ".", rating))
  
  rating_count = extract_sth(url, '.listLibrary__ratingAll')
  rating_count = as.numeric(word(rating_count, 1))
  
  links = read_html(url) %>% html_nodes('.authorAllBooks__singleTextTitle') %>% html_attr("href")
  links = unlist(lapply(links, function(x) {paste0("https://lubimyczytac.pl",x)}))
  
  ########find episodes
  episode = read_html(url) %>% html_nodes('.listLibrary__info')
  episode_list = character(0)
  to_remove = numeric(0)
  
  for (j in 1:length(episode)) {
    
    temp_episode = episode[[j]] %>% html_elements('a') %>% html_text() %>% gsub('[\t\n]', '', .) %>% trimws(.)
    
    if (length(temp_episode) > 0) {
      episode_list = append(episode_list, temp_episode)
      to_remove = append(to_remove, j+1)
      
    } else {
      episode_list = append(episode_list, "")
    }
  }
  
  episode_list = episode_list[-to_remove]
  
  
  ids = unlist(lapply(links, function(x) str_split(x, "/")))
  ids = ids[seq(5,length(ids), 6)]
  
  
  
  
  
  temp_tbl = data.frame("ID" = ids, "AUTOR" = authors, "TYTUL" = titles, 
                        "LICZBA_OCEN" = rating_count, "SREDNIA_OCEN" = rating, 
                        "SERIA" = episode_list, "URL" = links, 
                        "RODZAJ" = category_id)
  
  #write table to SQL
  for (k in 1:nrow(temp_tbl)) {
    ifexists = dbGetQuery(conn, paste0("SELECT 1 FROM BOOKS WHERE ID = ", temp_tbl[k,1]))
    if (nrow(ifexists)==0) {
      dbExecute(conn, "INSERT INTO BOOKS VALUES (?, ?, ?, ?, ?, ?, ?, ?)", params = unlist(temp_tbl[k,], use.names = F))
      
    } else {
      dbExecute(conn, "UPDATE BOOKS SET LICZBA_OCEN = ?, SREDNIA_OCEN = ? WHERE ID = ?", params = c(temp_tbl[k,4], temp_tbl[k,5], temp_tbl[k,1]))
      
    }
    
    
    
  }
  
  
  
  
  #full_tbl = rbind(full_tbl, temp_tbl)
  
  Sys.sleep(0.3)
  
  
  
  
}


