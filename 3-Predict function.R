# Load packages
library(quanteda)
library(dplyr)
library(data.table)
library(tokenizers)

# Load data
topgram1 <- readRDS("shiny-app/data/topgram1.rds")
G2 <- readRDS("shiny-app/data/G2.rds")
G3 <- readRDS("shiny-app/data/G3.rds")
G4 <- readRDS("shiny-app/data/G4.rds")


# Create a function to clean the input text
cleantext <- function(x) {
        clean <- tokenize_words(x) %>% 
                tokens(remove_symbols=TRUE, remove_url=TRUE)
        clean <- unlist(clean, use.names=F)    
        return(clean)
}

# Create a function to predict the text
predtext <- function(input) {
        x <- cleantext(input)
        
        x3 <- paste0(tail(x, 3), collapse=" ")
        prewords3 <- G4 %>% 
                filter(firstWords==x3) %>% 
                select(predWord, score4) %>% 
                arrange(desc(score4))
        prewords3 <- head(prewords3, 3)
        
        x2 <- paste0(tail(x, 2), collapse=" ")
        prewords2 <- G3 %>% 
                filter(firstWords==x2) %>% 
                select(predWord, score3) %>% 
                mutate(score3=0.4*score3) %>% 
                arrange(desc(score3))
        prewords2 <- head(prewords2, 3)
        
        x1 <- paste0(tail(x, 1), collapse=" ")
        prewords1 <- G2 %>% 
                filter(firstWords==x1) %>% 
                select(predWord, score2) %>% 
                mutate(score2=0.4^2*score2) %>% 
                arrange(desc(score2))
        prewords1 <- head(prewords1, 3)
        
        prewords <- Reduce(function (...) { merge(..., by="predWord", all = TRUE) },   
                           list(prewords3, prewords2, prewords1, topgram1))
        prewords[is.na(prewords)] <- 0
        prewords <- prewords %>% 
                mutate(score=pmax(score1, score2, score3, score4)) %>% 
                select(predWord, score) %>% 
                arrange(desc(score))
        
        return(prewords)
}


predict <- head(predtext(x), 5)$predWord


