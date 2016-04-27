library(dplyr)
library(tidyr)
library(stringr)

pred_word <- function(x) {
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
    return(head(recom, 3))
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