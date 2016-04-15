# START : 2016.03.12

# Loading Packages I need ####
library(tm)
library(stringr)
library(dplyr)
library(ggplot2)
library(SnowballC)
library(RWeka)
library(tidyr)

# 1. Importing the Data & Cleaning the Data ####
dir <- "data/final/en_US/"
files <- list.files(dir)

blogs <- readLines(paste0(dir, files[1]), encoding = "UTF-8")
news <- readLines(paste0(dir, files[2]), encoding = "UTF-8")
twitter <- readLines(paste0(dir, files[3]), encoding = "UTF-8", skipNul = T)
## Storing the raw files
save(blogs, file = "data/blogs.RData")
save(news, file = "data/news.RData")
save(twitter, file = "data/twitter.RData")
## Counting lines each data set
count_lines_total <- data.frame(blogs = length(blogs), 
                                news = length(news), 
                                twitter = length(twitter)) %>% 
    mutate(total = sum(blogs, news, twitter))
## Creating sample data sets containing 20,000 lines for calculating and modeling
sam_blogs <- blogs[sample(1:length(blogs), 20000)]
sam_news <- news[sample(1:length(news), 20000)]
sam_twitter <- twitter[sample(1:length(twitter), 20000)]
## Removing Raw data sets
rm(blogs, news, twitter)
## Extracting words from data sets
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
SimpleWords <- function(x) {
    return(x %>% strsplit(split = " ") %>% 
               unlist() %>% 
               gsub("http[[:alnum:]]*", "", x = .) %>% 
               str_replace_all(pattern = "[:punct:]", "") %>% 
               str_replace_all(pattern = "[:cntrl:]", "") %>% 
               str_replace_all(pattern = "[0-9]", "") %>% 
               str_to_lower() %>% 
               .[. != ""])
}

# 2. EDA ####
## a. Counting 
count_words_sample <- data.frame(blogs = length(SimpleWords(sam_blogs)), 
                                 news = length(SimpleWords(sam_news)), 
                                 twitter = length(SimpleWords(sam_twitter))) %>% 
    mutate(total = sum(blogs, news, twitter))
count_words_sample_nodup <- data.frame(blogs = length(unique(SimpleWords(sam_blogs))), 
                                       news = length(unique(SimpleWords(sam_news))), 
                                       twitter = length(unique(SimpleWords(sam_twitter)))) %>% 
    mutate(total = sum(blogs, news, twitter))
summary_dataset <- rbind(count_lines_total, c(20000, 20000, 20000, 60000), 
                         count_words_sample, count_words_sample_nodup) %>% 
    rbind(round(.[3, ] / .[2, ]))

## b. Frequencies of words
freq_blogs <- as.data.frame(table(SimpleWords(sam_blogs))) %>% arrange(desc(Freq))
freq_news <- as.data.frame(table(SimpleWords(sam_news))) %>% arrange(desc(Freq))
freq_twitter <- as.data.frame(table(SimpleWords(sam_twitter))) %>% arrange(desc(Freq))
names(freq_blogs) <- c("Word", "blogs")
names(freq_news) <- c("Word", "news")
names(freq_twitter) <- c("Word", "twitter")
freq_total <- merge(freq_blogs, freq_news, by = "Word", all = T) %>% 
    merge(x = ., freq_twitter, by = "Word", all = T) %>% 
    mutate(Total = rowSums(.[, 2:4])) %>% 
    arrange(desc(Total))
freq_total$Word <- factor(freq_total$Word)
gg_freq <- freq_total[1:20, 1:5]
gg_freq$Word <- factor(gg_freq$Word, levels = freq_total[20:1, 1])

ggplot(gg_freq[, 1:4] %>% gather(category, value, -Word), aes(x = Word, y = value, col = category)) + 
    geom_point(fill = "white", alpha = .4, size = 4) + 
    ylab("Frequency") + 
    ggtitle("Top 20 words of the most Frequent words") + 
    coord_flip()

# 3. Making a Corpus ####
## Combining the three sample datasets
sam_total <- sample(c(sam_blogs, sam_news, sam_twitter), 30000)
## A function for extraction of words in the sentences
ExtWords <- function(x) {
    return(x %>% iconv(to = "UTF-8") %>% 
               str_replace_all(pattern = "http[[:alnum:]]*", "") %>% 
               VectorSource() %>% 
               Corpus() %>% 
               tm_map(removePunctuation) %>% 
               tm_map(removeNumbers) %>% 
               tm_map(tolower) %>% 
               # tm_map(stemDocument) %>% 
               tm_map(stripWhitespace))
}
word_corpus <- ExtWords(sam_total)
save(file = "data/word_corpus.RData", word_corpus)

# 4. Creating a prediction function
## A function of ngram tokenizer
ngramer <- function(data, n) {
    return(NGramTokenizer(data, Weka_control(min = n, max = n)) %>% 
               table() %>% 
               as.data.frame() %>% 
               arrange(desc(Freq)))
}
## Creating N-grams
gram_1 <- ngramer(word_corpus, 1)
gram_1 <- gram_1 %>% mutate(Prob = Freq / sum(Freq))
gram_2 <- ngramer(word_corpus, 2)
gram_3 <- ngramer(word_corpus, 3)
gram_4 <- ngramer(word_corpus, 4)
## Seperating a cloumn for comparison between the n-grams and inputted words
temp_2 <- sapply(str_split(gram_2[, 1], pattern = " "), "[") %>% 
    t() %>% 
    as.data.frame() %>% 
    cbind(Freq = gram_2[, 2]) %>% 
    # I set up the filter to get only bigram appeared twice in the courpus for the accuracy of the recommendation
    filter(Freq >= 2) %>% 
    mutate(Ngram = 2)
names(temp_2)[1:2] <- c("Lookup", "Recommend")

temp_3 <- sapply(str_split(gram_3[, 1], pattern = " "), "[") %>% 
    t() %>% 
    as.data.frame() %>% 
    unite(lookup, V1, V2, sep = " ") %>% 
    cbind(Freq = gram_3[, 2]) %>% 
    mutate(Ngram = 3)
names(temp_3)[1:2] <- c("Lookup", "Recommend")

temp_4 <- sapply(str_split(gram_4[, 1], pattern = " "), "[") %>% 
    t() %>% 
    as.data.frame() %>% 
    unite(lookup, V1, V2, V3, sep = " ") %>% 
    cbind(Freq = gram_4[, 2]) %>% 
    mutate(Ngram = 4)
names(temp_4)[1:2] <- c("Lookup", "Recommend")
grams <- rbind(temp_2, temp_3, temp_4) %>% arrange(desc(Ngram, Freq))
grams$Lookup <- as.character(grams$Lookup)
grams$Recommend <- as.character(grams$Recommend)
grams$Recommend[grams$Recommend == "i"] <- "I"
rm(temp_4, temp_3, temp_2)

## A prediction Fuction
### A function to return corresponding n-grams
pred_word <- function(x) {clean_word(x)
    recom <- NULL
    word_temp <- str_split(clean_word(x), pattern = " ") %>% unlist() %>% .[. != ""]
    word_count <- word_temp %>% length()
    if (word_count >= 3) {
        word_temp <- paste(word_temp[word_count - 2], word_temp[word_count - 1], word_temp[word_count])
        recom <- pred_gram(word_temp, .5)
        # Searching the matched 4-gram words when the number of results is less than 3
        word_temp <- word_temp %>% str_split(pattern = " ") %>% unlist()
        word_temp <- paste(word_temp[2], word_temp[3])
        recom <- rbind(recom, pred_gram(word_temp, .3))
        # Searching the matched 3-gram words when the number of results is less than 2
        word_temp <- word_temp %>% str_split(pattern = " ") %>% unlist()
        word_temp <- word_temp[2]
        recom <- rbind(recom, pred_gram(word_temp, .2))
    }
    else if (word_count == 2) {
        recom <- pred_gram(word_temp, .7)
        # Searching the matched 2-gram words when the number of results is less than 2 
        word_temp <- str_split(x, pattern = " ") %>% unlist()
        word_temp <- word_temp[2]
        recom <- rbind(recom, pred_gram(word_temp, .3))
    }
    else {
        recom <- pred_gram(word_temp, 1)
    }
    recom <- recom %>% group_by(Recommend) %>% 
        summarise(Prob = sum(Prob)) %>% 
        # Adding the probability of the Recommended words
        merge(x = ., y = gram_1[, c(".", "Prob")], by.x = "Recommend", by.y = ".", all.x = T) %>% 
        # Weight settting : 4/3/2 grams(0.95) + 1gram(0.05)
        mutate(Prob = (.95 * Prob.x) + (.05 * Prob.y)) %>% 
        select(Recommend, Prob) %>% 
        arrange(desc(Prob))
    names(recom) <- c("Word", "Probability")
    # If the word for recommendation is none, 
    # I'm going to recommend the most frequently used three words(the, to, and)
    if (nrow(recom) < 1) {
        top_1_words <- data.frame(Word = c("the", "to", "and"), 
                                  Probability = c(0.05069748, 0.02818105, 0.02580177), 
                                  stringsAsFactors = F)
        recom <- rbind(recom, top_1_words)    
    }
    return(recom[1, 1])
}

pred_gram <- function(x, weight) {
    return(grams_app %>% filter(x == Lookup) %>% 
               mutate(Prob = weight * Freq / sum(Freq)) %>% 
               select(Recommend, Prob))
}

clean_word <- function(x) {
    x %>% str_to_lower() %>% 
        str_replace_all(pattern = "http[[:alnum:]]*", "") %>% 
        str_replace_all(pattern = "[:punct:]", "") %>% 
        str_replace_all(pattern = "[:cntrl:]", "") %>% 
        str_replace_all(pattern = "[0-9]", "") %>% 
        .[. != ""]
}

# 5. Trimming the 'n-gram' data set for app ####
## Replacing the 'i' & 'dont' => 'I' & 'don't' etc
grams_app$Recommend[grams_app$Recommend == "i"] <- "I"
grams_app$Recommend[grams_app$Recommend == "dont"] <- "don't"
grams_app$Recommend[grams_app$Recommend == "didnt"] <- "didn't"
grams_app$Recommend[grams_app$Recommend == "havent"] <- "haven't"
grams_app$Recommend[grams_app$Recommend == "hadnt"] <- "hadn't"
grams_app$Recommend[grams_app$Recommend == "ive"] <- "I've"
grams_app$Recommend[grams_app$Recommend == "id"] <- "I'd"
grams_app$Recommend[grams_app$Recommend == "youve"] <- "you've"
grams_app$Recommend[grams_app$Recommend == "youd"] <- "you'd"
grams_app$Recommend[grams_app$Recommend == "hes"] <- "he's"
grams_app$Recommend[grams_app$Recommend == "shes"] <- "she's"
grams_app$Recommend[grams_app$Recommend == "im"] <- "I'm"


