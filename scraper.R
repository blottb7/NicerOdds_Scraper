#libraries
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

#global vars
hilo_cols <- 2  #sets the number of highest and lowest odds columns; one for each.

#Function for scraping nicerodds website
nicerodds_scraper <- function(url, hilo_cols = 2) {
  
  #read in and save url
  webpage <- read_html(url)
  
  #read in event
  event <- html_nodes(webpage, '.subItemSelected')
  event <- html_text(event)
  
  #headers
  event_headers <- html_nodes(webpage, 'th')  #read in 'headers' node
  event_headers <- html_text(event_headers)  #extract
  event_headers <- data.frame(matrix(unlist(event_headers), nrow = 1), stringsAsFactors = FALSE)  #convert to df
  
  names(event_headers) <- event_headers[1,]  #set row 1 as column names
  colnames(event_headers)[1] <- "name"  #rename first column to team/athlete name
  columns <- ncol(event_headers)  #save number of columns
  
  #names
  event_names <- html_nodes(webpage, '#mainplh_boAutoOddsTable1_GridView1 td:nth-child(1)')
  event_names <- html_text(event_names)
  event_names <- as.data.frame(event_names)
  
  event_names[,1] <- gsub("\\(.*","", event_names[,1])  #remove country abbreviations and parentheses from name col
  event_names[,1] <- sub("\\s+$", "", event_names[,1])  #remove trailing white space
  
  #odds hi lo
  event_hilo <- html_nodes(webpage, '#mainplh_boAutoOddsTable1_GridView1 td+ td')
  event_hilo <- html_text(event_hilo)
  event_hilo <- data.frame(matrix(as.numeric(unlist(event_hilo)), ncol = hilo_cols, byrow = TRUE))
  
  #odds sites
  event_odds <- html_nodes(webpage, '#mainplh_boAutoOddsTable1_GridView2 td')
  event_odds <- html_text(event_odds)
  event_odds <- data.frame(matrix(as.numeric(unlist(event_odds)), ncol = columns - 3, byrow = TRUE))
  
  #bind columns
  event_df <- event_names %>%
    bind_cols(event_hilo) %>%
    bind_cols(event_odds) %>%
    head(-1)
  
  #name cols
  names(event_df) <- names(event_headers)  #convert to headers df names
  
  #add event col
  event_df$event <- event
  
  #return event df
  event_df
}
