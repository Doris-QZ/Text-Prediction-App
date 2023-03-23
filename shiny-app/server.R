# Load packages
library(quanteda)
library(tokenizers)
library(dplyr)
library(shiny)
library(wordcloud)
library(RColorBrewer)

# Load data
topgram1 <- readRDS("data/topgram1.rds")
G2 <- readRDS("data/G2.rds")
G3 <- readRDS("data/G3.rds")
G4 <- readRDS("data/G4.rds")


# Predict function
cleantext <- function(x) {
  clean <- tokenize_words(x) %>% 
    tokens(remove_symbols=TRUE, remove_url=TRUE)
  clean <- unlist(clean, use.names=F)    
  return(clean)
}


predtext <- function(input) {
        x <- cleantext(input)
        
        x3 <- paste0(tail(x, 3), collapse=" ")
        prewords3 <- G4 %>% 
                filter(firstWords==x3) %>% 
                select(predWord, score4) %>% 
                arrange(desc(score4))
        prewords3 <- head(prewords3, 5)
        
        x2 <- paste0(tail(x, 2), collapse=" ")
        prewords2 <- G3 %>% 
                filter(firstWords==x2) %>% 
                select(predWord, score3) %>% 
                mutate(score3=0.4*score3) %>% 
                arrange(desc(score3))
        prewords2 <- head(prewords2, 5)
        
        x1 <- paste0(tail(x, 1), collapse=" ")
        prewords1 <- G2 %>% 
                filter(firstWords==x1) %>% 
                select(predWord, score2) %>% 
                mutate(score2=0.4^2*score2) %>% 
                arrange(desc(score2))
        prewords1 <- head(prewords1, 5)
        
        prewords <- Reduce(function (...) { merge(..., by="predWord", all = TRUE) },   
                           list(prewords3, prewords2, prewords1, topgram1))
        prewords[is.na(prewords)] <- 0
        prewords <- prewords %>% 
                mutate(score=pmax(score1, score2, score3, score4)) %>% 
                select(predWord, score) %>% 
                arrange(desc(score))
        
        return(prewords)
}


# Define server 
shinyServer(function(input, output, session) {

    output$predict <- renderText({
      prewords <- predtext(input$text)
      predict <- head(prewords, 5)$predWord
      predict <- paste0(predict, collapse=" | ")
      return(predict)
    })
    
    output$wordcloud <- renderPlot({
      prewords <- predtext(input$text)
      prewords$S <- ifelse(prewords$score==0, 1, round(prewords$score*1000))
      
      wordcloud(words = prewords$predWord, freq =prewords$S, min.freq=1, scale=c(5, 1),           
                max.words=100, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

    })

})
