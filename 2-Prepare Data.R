# Loading the packages
library(quanteda)
library(readtext)
library(quanteda.textstats)
library(tidyr)  
library(dplyr)
library(stringr)
library(data.table)
library(NCmisc)
library(tokenizers)


# Combine three text files into a "fulltext" file 
conb <- file("final/en_US/en_US.blogs.txt", "r")
blog <- readLines(conb, encoding="UTF-8", skipNul=TRUE)
close(conb)

cont <- file("final/en_US/en_US.twitter.txt", "r")
twi <- readLines(cont, encoding="UTF-8", skipNul=TRUE)
close(cont)

conn <- file("final/en_US/en_US.news.txt", "r")
news <- readLines(conn, encoding="UTF-8", skipNul=TRUE)
close(conn)

fulltext <- c(blog, twi, news)
writeLines(fulltext,"subdata/fulltext.txt" )


# The fulltext is 578.8M, it runs too slow in my laptop, so I split it into 10 text files and process by two laptop in the meantime.
setwd("/Users/Doris/Desktop/capstone project/subdata")
newfile <- file.split("fulltext.txt", size=430000) 
setwd("/Users/Doris/Desktop/capstone project")

# Remove data that is no longer needed to free the memory
rm("newfile", "blog", "news", "twi", "fulltext")


# Process the 10 text files: 
# File No.1
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partad")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # remove non_ASCII characters
writeLines(text,"data/text1.txt" )
corpus <- corpus(readtext("data/text1.txt"))

# Create profanity list 
profanity <- readLines("en_profanity.txt", skipNul=TRUE)

# Tokenization and Data cleaning  
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)


# Create a function to generate n-gram frequency table
ngramF <- function(x, n) {
        tokN <- tokens_ngrams(x, n)
        dfmN <- dfm(tokN)
        dfmN <- dfm_trim(dfmN, min_termfreq=2)
        gramN <- data.table(textstat_frequency(dfmN)[,1:2])
        if(n==1){
                return(gramN)
        } else if (n==2) {
                gramN <- gramN %>% 
                        mutate(firstWords=word(feature, 1, sep="_"), predWord=word(feature,-1, sep="_")) %>% 
                        select(-feature)
                gramN <- gramN[, c(2,3,1)]
        } else {
                gramN <- gramN %>% 
                        mutate(firstWords=word(feature, start=1, end=(n-1), sep="_"), predWord=word(feature,-1, sep="_")) %>% 
                        select(-feature)
                gramN <- gramN[, c(2,3,1)]
                gramN$firstWords <- gsub("_", " ", gramN$firstWords)
                return(gramN)
        }
}

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq0=frequency)
G2 <- rename(G2, freq0=frequency)
G3 <- rename(G3, freq0=frequency)
G4 <- rename(G4, freq0=frequency)
G5 <- rename(G5, freq0=frequency)

# Save data
saveRDS(G1, file="pred_table/G10.rds")
saveRDS(G2, file="pred_table/G20.rds")
saveRDS(G3, file="pred_table/G30.rds")
saveRDS(G4, file="pred_table/G40.rds")
saveRDS(G5, file="pred_table/G50.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.2
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partac")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text2.txt" )
corpus <- corpus(readtext("data/text2.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq1=frequency)
G2 <- rename(G2, freq1=frequency)
G3 <- rename(G3, freq1=frequency)
G4 <- rename(G4, freq1=frequency)
G5 <- rename(G5, freq1=frequency)

# Save data
saveRDS(G1, file="pred_table/G11.rds")
saveRDS(G2, file="pred_table/G21.rds")
saveRDS(G3, file="pred_table/G31.rds")
saveRDS(G4, file="pred_table/G41.rds")
saveRDS(G5, file="pred_table/G51.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.3
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partae")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text3.txt" )
corpus <- corpus(readtext("data/text3.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq2=frequency)
G2 <- rename(G2, freq2=frequency)
G3 <- rename(G3, freq2=frequency)
G4 <- rename(G4, freq2=frequency)
G5 <- rename(G5, freq2=frequency)

# Save data
saveRDS(G1, file="pred_table/G12.rds")
saveRDS(G2, file="pred_table/G22.rds")
saveRDS(G3, file="pred_table/G32.rds")
saveRDS(G4, file="pred_table/G42.rds")
saveRDS(G5, file="pred_table/G52.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.4
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partaf")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text4.txt" )
corpus <- corpus(readtext("data/text4.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq3=frequency)
G2 <- rename(G2, freq3=frequency)
G3 <- rename(G3, freq3=frequency)
G4 <- rename(G4, freq3=frequency)
G5 <- rename(G5, freq3=frequency)

# Save data
saveRDS(G1, file="pred_table/G13.rds")
saveRDS(G2, file="pred_table/G23.rds")
saveRDS(G3, file="pred_table/G33.rds")
saveRDS(G4, file="pred_table/G43.rds")
saveRDS(G5, file="pred_table/G53.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.5
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partag")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text5.txt" )
corpus <- corpus(readtext("data/text5.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq4=frequency)
G2 <- rename(G2, freq4=frequency)
G3 <- rename(G3, freq4=frequency)
G4 <- rename(G4, freq4=frequency)
G5 <- rename(G5, freq4=frequency)

# Save data
saveRDS(G1, file="pred_table/G14.rds")
saveRDS(G2, file="pred_table/G24.rds")
saveRDS(G3, file="pred_table/G34.rds")
saveRDS(G4, file="pred_table/G44.rds")
saveRDS(G5, file="pred_table/G54.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.6
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partah")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text6.txt" )
corpus <- corpus(readtext("data/text6.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq5=frequency)
G2 <- rename(G2, freq5=frequency)
G3 <- rename(G3, freq5=frequency)
G4 <- rename(G4, freq5=frequency)
G5 <- rename(G5, freq5=frequency)

# Save data
saveRDS(G1, file="pred_table/G15.rds")
saveRDS(G2, file="pred_table/G25.rds")
saveRDS(G3, file="pred_table/G35.rds")
saveRDS(G4, file="pred_table/G45.rds")
saveRDS(G5, file="pred_table/G55.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.7
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partaa")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text7.txt" )
corpus <- corpus(readtext("data/text7.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq6=frequency)
G2 <- rename(G2, freq6=frequency)
G3 <- rename(G3, freq6=frequency)
G4 <- rename(G4, freq6=frequency)
G5 <- rename(G5, freq6=frequency)

# Save data
saveRDS(G1, file="pred_table/G16.rds")
saveRDS(G2, file="pred_table/G26.rds")
saveRDS(G3, file="pred_table/G36.rds")
saveRDS(G4, file="pred_table/G46.rds")
saveRDS(G5, file="pred_table/G56.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.8
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partab")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text8.txt" )
corpus <- corpus(readtext("data/text8.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq7=frequency)
G2 <- rename(G2, freq7=frequency)
G3 <- rename(G3, freq7=frequency)
G4 <- rename(G4, freq7=frequency)
G5 <- rename(G5, freq7=frequency)

# Save data
saveRDS(G1, file="pred_table/G17.rds")
saveRDS(G2, file="pred_table/G27.rds")
saveRDS(G3, file="pred_table/G37.rds")
saveRDS(G4, file="pred_table/G47.rds")
saveRDS(G5, file="pred_table/G57.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.9
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partai")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text9.txt" )
corpus <- corpus(readtext("data/text9.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq8=frequency)
G2 <- rename(G2, freq8=frequency)
G3 <- rename(G3, freq8=frequency)
G4 <- rename(G4, freq8=frequency)
G5 <- rename(G5, freq8=frequency)

# Save data
saveRDS(G1, file="pred_table/G18.rds")
saveRDS(G2, file="pred_table/G28.rds")
saveRDS(G3, file="pred_table/G38.rds")
saveRDS(G4, file="pred_table/G48.rds")
saveRDS(G5, file="pred_table/G58.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5")


# File No.10
# Load in data and turn into a corpus 
text <- readLines("subdata/fulltext_partaj")
text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")  # clean non_ASCII characters
writeLines(text,"data/text10.txt" )
corpus <- corpus(readtext("data/text10.txt"))

# Tokenization and Data cleaning 
Tokens <- tokenize_words(corpus, strip_punct=FALSE) %>% 
        tokens(remove_symbols=TRUE, remove_url=TRUE, remove_numbers=TRUE, remove_punct=TRUE, padding=TRUE)

Tokens <- tokens_remove(Tokens,  profanity, padding = TRUE)

# Create n-gram frequency tables
G1 <- ngramF(Tokens, 1)
G2 <- ngramF(Tokens, 2)
G3 <- ngramF(Tokens, 3)
G4 <- ngramF(Tokens, 4)
G5 <- ngramF(Tokens, 5)

# Rename "frequency"
G1 <- rename(G1, freq9=frequency)
G2 <- rename(G2, freq9=frequency)
G3 <- rename(G3, freq9=frequency)
G4 <- rename(G4, freq9=frequency)
G5 <- rename(G5, freq9=frequency)

# Save data
saveRDS(G1, file="pred_table/G19.rds")
saveRDS(G2, file="pred_table/G29.rds")
saveRDS(G3, file="pred_table/G39.rds")
saveRDS(G4, file="pred_table/G49.rds")
saveRDS(G5, file="pred_table/G59.rds")

# Remove data that is no longer needed to free the memory
rm("text", "Tokens", "corpus", "G1", "G2", "G3", "G4", "G5", "profanity", "ngramF")

# Read in G10--G59 
G10 <- readRDS("pred_table/G10.rds")
G11 <- readRDS("pred_table/G11.rds")
G12 <- readRDS("pred_table/G12.rds")
G13 <- readRDS("pred_table/G13.rds")
G14 <- readRDS("pred_table/G14.rds")
G15 <- readRDS("pred_table/G15.rds")
G16 <- readRDS("pred_table/G16.rds")
G17 <- readRDS("pred_table/G17.rds")
G18 <- readRDS("pred_table/G18.rds")
G19 <- readRDS("pred_table/G19.rds")

G20 <- readRDS("pred_table/G20.rds")
G21 <- readRDS("pred_table/G21.rds")
G22 <- readRDS("pred_table/G22.rds")
G23 <- readRDS("pred_table/G23.rds")
G24 <- readRDS("pred_table/G24.rds")
G25 <- readRDS("pred_table/G25.rds")
G26 <- readRDS("pred_table/G26.rds")
G27 <- readRDS("pred_table/G27.rds")
G28 <- readRDS("pred_table/G28.rds")
G29 <- readRDS("pred_table/G29.rds")

G30 <- readRDS("pred_table/G30.rds")
G31 <- readRDS("pred_table/G31.rds")
G32 <- readRDS("pred_table/G32.rds")
G33 <- readRDS("pred_table/G33.rds")
G34 <- readRDS("pred_table/G34.rds")
G35 <- readRDS("pred_table/G35.rds")
G36 <- readRDS("pred_table/G36.rds")
G37 <- readRDS("pred_table/G37.rds")
G38 <- readRDS("pred_table/G38.rds")
G39 <- readRDS("pred_table/G39.rds")

G40 <- readRDS("pred_table/G40.rds")
G41 <- readRDS("pred_table/G41.rds")
G42 <- readRDS("pred_table/G42.rds")
G43 <- readRDS("pred_table/G43.rds")
G44 <- readRDS("pred_table/G44.rds")
G45 <- readRDS("pred_table/G45.rds")
G46 <- readRDS("pred_table/G46.rds")
G47 <- readRDS("pred_table/G47.rds")
G48 <- readRDS("pred_table/G48.rds")
G49 <- readRDS("pred_table/G49.rds")

G50 <- readRDS("pred_table/G50.rds")
G51 <- readRDS("pred_table/G51.rds")
G52 <- readRDS("pred_table/G52.rds")
G53 <- readRDS("pred_table/G53.rds")
G54 <- readRDS("pred_table/G54.rds")
G55 <- readRDS("pred_table/G55.rds")
G56 <- readRDS("pred_table/G56.rds")
G57 <- readRDS("pred_table/G57.rds")
G58 <- readRDS("pred_table/G58.rds")
G59 <- readRDS("pred_table/G59.rds")

# Combine unigram data
G1 <- Reduce(function (...) { merge(..., by="feature", all = TRUE) },   
             list(G10, G11, G12, G13, G14, G15, G16, G17, G18, G19))
G1[is.na(G1)] <- 0
G1 <- G1 %>% 
        mutate(frequency=freq0+freq1+freq2+freq3+freq4+freq5+freq6+freq7+freq8+freq9) %>% 
        select(feature, frequency) %>% 
        arrange(feature)

G1$feature <- str_trim(G1$feature)
G1 <- arrange(G1, feature)
grep("^a", G1$feature)[1]
G1 <- G1[-c(1:3346),]

# Create top5 of unigram table
topgram1 <- G1[1:5, 1]
score <- c(0.00, 0.00, 0.00, 0.00, 0.00)
topgram1 <- data.table(topgram1)
topgram1[, score1 := score]
topgram1 <- rename(topgram1, predWord=feature)

# Combine n-grams data
G2 <- Reduce(function (...) { merge(..., by=c("firstWords","predWord"), all = TRUE) },   
             list(G20, G21, G22, G23, G24, G25, G26, G27, G28, G29))
G2[is.na(G2)] <- 0
G2 <- G2 %>% 
        mutate(frequency=freq0+freq1+freq2+freq3+freq4+freq5+freq6+freq7+freq8+freq9) %>% 
        select(firstWords, predWord, frequency) %>% 
        arrange(desc(frequency))


G3 <- Reduce(function (...) { merge(..., by=c("firstWords","predWord"), all = TRUE) },   
             list(G30, G31, G32, G33, G34, G35, G36, G37, G38, G39))
G3[is.na(G3)] <- 0
G3 <- G3 %>% 
        mutate(frequency=freq0+freq1+freq2+freq3+freq4+freq5+freq6+freq7+freq8+freq9) %>% 
        select(firstWords, predWord, frequency) %>% 
        arrange(desc(frequency))


G4 <- Reduce(function (...) { merge(..., by=c("firstWords","predWord"), all = TRUE) },   
             list(G40, G41, G42, G43, G44, G45, G46, G47, G48, G49))
G4[is.na(G4)] <- 0
G4 <- G4 %>% 
        mutate(frequency=freq0+freq1+freq2+freq3+freq4+freq5+freq6+freq7+freq8+freq9) %>% 
        select(firstWords, predWord, frequency) %>% 
        arrange(desc(frequency))


G5 <- Reduce(function (...) { merge(..., by=c("firstWords","predWord"), all = TRUE) },   
             list(G50, G51, G52, G53, G54, G55, G56, G57, G58, G59))
G5[is.na(G5)] <- 0
G5 <- G5 %>% 
        mutate(frequency=freq0+freq1+freq2+freq3+freq4+freq5+freq6+freq7+freq8+freq9) %>% 
        select(firstWords, predWord, frequency) %>% 
        arrange(desc(frequency))

# Remove data that is no longer needed to free the memory
rm("G10", "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18", "G19",
   "G20", "G21", "G22", "G23", "G24", "G25", "G26", "G27", "G28", "G29",
   "G30", "G31", "G32", "G33", "G34", "G35", "G36", "G37", "G38", "G39",
   "G40", "G41", "G42", "G43", "G44", "G45", "G46", "G47", "G48", "G49", 
   "G50", "G51", "G52", "G53", "G54", "G55", "G56", "G57", "G58", "G59")


# Add a new column "score" to n-gram tables
# G5 -- count the sum of "n-1"grams
total5 <- G5 %>% 
        select(firstWords, frequency) %>% 
        group_by(firstWords) %>% 
        summarize(total=sum(frequency))
total5 <- data.table(total5)

# Remove rows with empty value in total5 and G5 (I should have cleaned the G1-G5 table before adding the new column) 
total5 <- arrange(total5, firstWords)
total5$firstWords <- str_trim(total5$firstWords)
total5 <- subset(total5, total5$firstWords!="")
total5 <- subset(total5, total5$firstWords!="g1")

G5 <- arrange(G5, firstWords)
G5$firstWords <- str_trim(G5$firstWords)
G5 <- subset(G5, G5$firstWords!="")
G5 <- arrange(G5, predWord)
G5$predWord <- str_trim(G5$predWord)
G5 <- subset(G5, G5$predWord!="")
G5 <- G5[-c(1,23,24), ]

#G5 -- merge datatable
G5 <- full_join(G5, total5, by="firstWords")

#G5 -- add new column "score"
G5[, score5:=round(frequency/total,2)]
G5 <- G5[,-4]
G5 <- arrange(G5, desc(frequency))

# G4 -- count the sum of "n-1"grams
total4 <- G4 %>% 
        select(firstWords, frequency) %>% 
        group_by(firstWords) %>% 
        summarize(total=sum(frequency))
total4 <- data.table(total4)

# Remove rows with empty value in G4 and total4
total4 <- arrange(total4, firstWords)
total4$firstWords <- str_trim(total4$firstWords)
total4 <- subset(total4, total4$firstWords!="")
total4 <- subset(total4, total4$firstWords!="ask the")

G4 <- arrange(G4, firstWords)
G4$firstWords <- str_trim(G4$firstWords)
G4 <- subset(G4, G4$firstWords!="")

G4 <- arrange(G4, predWord)
G4$predWord <- str_trim(G4$predWord)
G4 <- subset(G4, G4$predWord!="")
G4 <- G4[-c(2,4,5), ]

#G4 -- merge datatable
G4 <- full_join(G4, total4, by="firstWords")
#G4 -- add new column "score"
G4[, score4:=round(frequency/total,2)]
G4 <- G4[,-4]
G4 <- arrange(G4, desc(frequency))

# G3 -- count the sum of "n-1"grams
total3 <- G3 %>% 
        select(firstWords, frequency) %>% 
        group_by(firstWords) %>% 
        summarize(total=sum(frequency))
total3 <- data.table(total3)

# Remove rows with empty value in G3 and total3
total3 <- arrange(total3, firstWords)
total3$firstWords <- str_trim(total3$firstWords)
total3 <- subset(total3, total3$firstWords != "")
total3 <- subset(total3, total3$firstWords != "ask")

G3 <- arrange(G3, firstWords)
G3$firstWords <- str_trim(G3$firstWords)
G3 <- subset(G3, G3$firstWords!="")

G3 <- arrange(G3, predWord)
G3$predWord <- str_trim(G3$predWord)
G3 <- subset(G3, G3$predWord!="")
G3 <- G3[-c(5:9, 17:19), ]

#G3 -- merge datatable
G3 <- full_join(G3, total3, by="firstWords")
#G3 -- add new column "score"
G3[, score3:=round(frequency/total,2)]
G3 <- G3[,-4]
G3 <- arrange(G3, desc(frequency))

# G2 -- count the sum of "n-1"grams
total2 <- G2 %>% 
        select(firstWords, frequency) %>% 
        group_by(firstWords) %>% 
        summarize(total=sum(frequency))
total2 <- data.table(total2)

# Remove rows with empty value in G2 and total2
total2 <- arrange(total2, firstWords)
total2$firstWords <- str_trim(total2$firstWords)
total2 <- subset(total2, total2$firstWords != "")
total2 <- subset(total2, total2$firstWords != 0)

G2 <- arrange(G2, firstWords)
G2$firstWords <- str_trim(G2$firstWords)
G2 <- subset(G2, G2$firstWords!="")
G2 <- subset(G2, G2$firstWords!=0)

G2 <- arrange(G2, predWord)
G2$predWord <- str_trim(G2$predWord)
G2 <- subset(G2, G2$predWord!="")
G2 <- subset(G2, G2$predWord!=0)
G2 <- G2[-c(7:9, 19:27), ]

#G2 -- merge datatable
G2 <- full_join(G2, total2, by="firstWords")
#G2 -- add new column "score"
G2[, score2:=round(frequency/total,2)]
G2 <- G2[,-4]
G2 <- arrange(G2, desc(frequency))


# Save data
saveRDS(G1, file="Data/G1.rds")
saveRDS(G2, file="Data/G2.rds")
saveRDS(G3, file="Data/G3.rds")
saveRDS(G4, file="Data/G4.rds")
saveRDS(G5, file="Data/G5.rds")
saveRDS(topgram1, file="Data/topgram1.rds")


# Remove data that is no longer needed to free the memory
rm("total2", "total3", "total4", "total5")


# The benchmark test result shows that the accuracy of the model is in a reasonable range.
# But the average runtime is 593.99msec, and the total memory used is 1152.38 MB. 
# I think the problem would be in both the model and the data. So I decided to try the following ways to improve the efficiency: 
# Model: modify the model, not searching gram2 to gram5 data table, but only deal with gram2 to gram4; 
# Data1: Still use 100% corpus, but set the frequency threshold to 4;
# Data2: Only sampling 50% of the data, set the frequency threshold to 3.
# After running the two data with the prediction function through benchmark test, I picked Data1 as the database. 
# The accuracy dropped a tiny little bit, but the runtime and the total memory used improved a lot after I set the frequency threshold of Data1 to 4.

# Data1
G2 <- subset(G2, G2$frequency >= 4)
G3 <- subset(G3, G3$frequency >= 4)
G4 <- subset(G4, G4$frequency >= 4)


