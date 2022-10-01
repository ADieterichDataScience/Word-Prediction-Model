# next word prediction script

library(tidytext)
library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(data.table)

load("ngram_data.RData")

# Clean the text
clean_word <- function (input) {
  input <- tibble(line = 1:(length(input)), text = input) %>%
    unnest_tokens(word, text) %>%
    filter(!str_detect(word, "\\d+")) %>%
    mutate_at("word", str_replace, "[[:punct:]]", "") %>% # remove punctuation
    pull(word)
  input
}

## Return a data table-
clean_text <- function (input) {
  input <- clean_word(input)
  if (length(input) ) {								# check if length is TRUE
    input <- setDT(as.list(input))					# convert to list first then DT
    if ( length(input) == 1 ) {
      input <- data.table(input$V1, " ", " ")		# if length is '1' w2, and w3 = " "
    } else if ( length(input) == 2 ) {
      input <- data.table(input$V1,input$V2," ")	# if length is '2' w3 = " "
    } else {
      input <- setDT(tail(as.list(input),3))		# return last 3 words in the sequence
    }
    names(input) <- c("w1","w2","w3")				# change column names to w1,w2, and w3
  } else {
    input <- NA										# return input as NA
  }
  
  input
}

## Function to return 2grams
bigrams <- function(input, ingram) {
  
  ## first run to check w1 and w2
  firstgram <- ingram
  setkeyv(firstgram, c("w1","w2"))
  
  ## First run to check w1 and return w2
  firstgram[, ret := ifelse((input[,w1] == w1), w2, 0)]
  firstgram <- firstgram[ret !="0"]						## retain rows which are non-zero
  
  ## Second run to check w2 and return w3
  secgram <- ingram
  setkeyv(secgram, c("w2","w3"))
  secgram[, ret := ifelse((input[,w1] == w2), w3, 0)]
  secgram <- secgram[ret !="0"]						## retain rows which are non-zero
  firstgram <- rbind(firstgram,secgram) 				## rowbind
  rm(secgram)										## remove secgram from MEMORY
  
  ## Third run to check w3 and return w4
  trgram <- ingram
  setkeyv(trgram, c("w3","w4"))
  trgram[, ret := ifelse((input[,w1] == w3), w4, 0)]
  trgram <- trgram[ret !="0"]						## retain rows which are non-zero
  firstgram <- rbind(firstgram,trgram) 				## rowbind
  rm(trgram)
  
  firstgram <- firstgram[ret !="0"]						## retain rows which are non-zero
  firstgram <- firstgram[,sum(total),by=c("ret")]
  firstgram <- firstgram[order(-V1)]
  words <- unique(firstgram[! ret %in% input])$ret 		## Return words
  rm(firstgram)
  
  ## Return words
  words
}

## Function to return 3 grams
trigrams <- function(input, ingram) {
  
  trigram <- ingram
  setkeyv(trigram, c("w1", "w2", "w3"))
  
  ## Data Table is edited in place
  trigram[, ret := ifelse((input[,w1] == w1 & input[,w2] == w2), w3, 0)]
  trigram <- trigram[ret !="0"]						## retain rows which are non-zero
  
  ## Second run to check w2 and return w3
  secgram <- ingram
  setkeyv(secgram, c("w1", "w2","w3"))
  secgram[, ret := ifelse((input[,w1] == w2 & input[,w2] == w3), w4, 0)]
  secgram <- secgram[ret !="0"]						## retain rows which are non-zero
  trigram <- rbind(trigram,secgram) 					## rowbind
  rm(secgram)
  
  trigram <- trigram[,sum(total),by=c("ret")]
  trigram <- trigram[order(-V1)]
  words <- unique(trigram[! ret %in% input])$ret 		## Return words
  rm(trigram)
  
  ## check if length of words is '0' - if TRUE invoke Bigram, else return words
  DONT = FALSE
  ifelse ( (words == " " | words == "" | words == NA), DONT <- TRUE, DONT <- FALSE) 
  if ( DONT ) {	
    backoff <- data.frame(w1=input$w2, w2=" ", w3 = " ")
    bigrams(backoff, ingram)
  } else {
    words
  }
  
}

## Function to return 4grams
# Checks w1+w2+w3 against w1 & w2 & w3, returns w4
# If length is '0', invoke btrigram
quadgrams <- function(input, qngram) {
  ingram <- qngram
  setkeyv(ingram,c("w1","w2","w3")) 		# setkey for fast computation					## no quotes for convenience
  ingram <- ingram[.(input$w1,input$w2,input$w3)]	# ALL ROWs - grep w1,w2, and w3 from DT
  ingram <- ingram[,sum(total),by=c("w4")]
  ingram <- ingram[order(-V1)]					    ## sort decreasing
  words <- unique(ingram[! w4 %in% input])$w4 		## Return words
  rm(ingram)
  
  ## check if length of words is '0' - if TRUE invoke trigram, else return words
  DONT = FALSE
  ifelse ( (words == " " | words == "" | words == NA), DONT <- TRUE, DONT <- FALSE) 
  if ( DONT ) {	
    backoff <- data.frame(w1=input$w2, w2=input$w3, w3 = " ")
    trigrams(backoff, qngram)
  } else {
    words
  }
}

## Function predict_words - to predict next list of words
predict_words <- function(input_text, ingram,N){
  input <- clean_text(input_text)							# clean and create a DT of last 3 words
  DONT=FALSE								## Set to False -only set to true in ifelse
  ifelse ( input == " ", DONT <- TRUE, DONT <- FALSE) 
  if ( DONT ) {
    if(input$w2 == " ") {
      words <- bigrams(input, ingram)			# predict bigram
    } else if(input$w3 == " ") {
      words <- trigrams(input, ingram)			# predict trigram
    } else {
      words <- quadgrams(input, ingram)			# preidct ingram
    }
    
    ## Check words returned for NAs and drop them
    if ( sum(is.na(words)) || length(words) == 0 ) {
      if (length(words) > N ) {
        words <- words[1:N]
      }
    }
  }
  
  ## Return words - remove words from input_text before return
  words <- setdiff(words, input)
}

## Main Function - next_word
predict_next<-function(input, N=5) {
  DONT=FALSE								## Set to False -only set to true in ifelse
  ifelse ( (input == " " | input == ""), DONT <- TRUE, DONT <- FALSE) 
  if (DONT) {
    phrase <- "Please enter text to predict next word"
  } else {
    phrase <- predict_words(input,all_ngrams,N)
  }
  phrase
}
