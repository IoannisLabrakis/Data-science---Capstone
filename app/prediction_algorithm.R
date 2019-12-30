library(dplyr)
library(tidyverse)
library(stringi)
library(data.table)


bi <- readRDS("bi_df.RDS")
tri <- readRDS("tri_df.RDS")
quad <- readRDS("quad_df.RDS")
five <- readRDS("five_df.RDS")

#clear input phrase
clearInput <- function(x){
    #make lower case characters
    x <- tolower(x)
    #remove hashtags
    x <- gsub("#\\s+"," ", x)
    #remove ,
    x <- gsub(",+","", x)
    #correct common abbreviations
    x <- stri_replace_all_regex(x,"\\b(i'm|im)\\b", replacement = "i am")
    x <- stri_replace_all_regex(x,"\\bya\\b", replacement = "you")
    x <- stri_replace_all_regex(x,"\\bik\\b", replacement = "i know")
    x <- stri_replace_all_regex(x,"'re\\b", replacement = " are")
    x <- stri_replace_all_regex(x,"'ve\\b", replacement = " have")
    x <- stri_replace_all_regex(x,"\\bcan't\\b", replacement = "can not")
    x <- stri_replace_all_regex(x,"\\bain't\\b", replacement = "is not")
    x <- stri_replace_all_regex(x,"\\bwon't\\b", replacement = "would not")
    x <- stri_replace_all_regex(x,"\\bdoesn't\\b", replacement = "does not")
    x <- stri_replace_all_regex(x,"n't\\b", replacement = " not")
    x <- stri_replace_all_regex(x,"'d\\b", replacement = " would")
    x <- stri_replace_all_regex(x,"'ll\\b", replacement = " will")
    x <- stri_replace_all_regex(x,"\\by\\b", replacement = "why")
    x <- stri_replace_all_regex(x,"\\bu\\b", replacement = "you")
    x <- stri_replace_all_regex(x,"\\bur\\b", replacement = "your")
    x <- stri_replace_all_regex(x,"\\bit's\\b", replacement = "it is")
    x <- stri_replace_all_regex(x,"\\bwhat's\\b", replacement = "what is")
    x <- stri_replace_all_regex(x,"\\bwat\\b", replacement = "what")
    x <- stri_replace_all_regex(x,"\\bthat's\\b", replacement = "that is")
    x <- stri_replace_all_regex(x,"\\bhe's\\b", replacement = "he is")
    x <- stri_replace_all_regex(x,"\\bshe's\\b", replacement = "she is")
    x <- stri_replace_all_regex(x,"\\bwho's\\b", replacement = "who is")
    x <- stri_replace_all_regex(x,"\\blet's\\b", replacement = "let us")
    #remove symbols
    x <- gsub("\\,","",x)
    #remove whitespace
    x <- gsub("\\s+"," ",x)
    return(x)
}

#predict next word
#function calls depending on the number of input  words

bigram <- function(x){
    n <- length(x)
    nextW <- filter(bi, word1 == x[n]) %>% top_n(20)
    nextW <- nextW[,2]
    if(is_empty(nextW)){
        nextW <- c("no match found / wrong input")
    }
    return(nextW)
}

trigram <- function(x){
    n <- length(x)
    nextW <- filter(tri, word1 == x[n-1], word2 == x[n]) %>% top_n(20)
    nextW <- nextW[,3]
    if(is_empty(nextW)){
        nextW <- bigram(x)
    }
    return(nextW)
}

quadgram <- function(x){
    n <- length(x)
    nextW <- filter(quad, word1 == x[n-2], word2 == x[n-1], word3 == x[n]) %>% top_n(20)
    nextW <- nextW[,4]
    if(is_empty(nextW)){
        nextW <- trigram(x)
    }
    return(nextW)
}

fivegram <- function(x){
    n <- length(x)
    nextW <- filter(five, word1 == x[n-3], word2 == x[n-2], word3 == x[n-1], word4 == x[n]) %>% top_n(20)
    nextW <- nextW[,5]
    if(is_empty(nextW)){
        nextW <- quadgram(x)
    }
    return(nextW)
}

#predict next word
nextWord <- function(phrase){
    userInput <- clearInput(phrase)
    words <- str_count(userInput, boundary("word"))
    nextW <- unlist(str_split(userInput, boundary("word")))
    if(words == 0){
        funCall <- bi$word1[1:5]
    }else if(words == 1){
        funCall <- bigram(nextW)
    }else if(words == 2){
        funCall <- trigram(nextW)
    }else if(words == 3){
        funCall <- quadgram(nextW)
    }else{
        funCall <- fivegram(nextW)
    }
    return(funCall)
}
