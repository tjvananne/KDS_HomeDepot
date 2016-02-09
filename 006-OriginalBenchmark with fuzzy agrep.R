
setwd("C:/Users/tjvan/Documents/Kaggle/HomeDepot_early_2016/homedepotgit")

library(readr)
library(dplyr)
cat("Reading data\n")
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')
desc <- read_csv('../input/product_descriptions.csv')


cat("Merge description with train and test data \n")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

t <- Sys.time()
word_match <- function(words,title,desc){
    n_title <- 0
    n_desc <- 0
    an_title <- 0
    an_desc <- 0
    #print(words)
    words <- gsub("[ ]{2,}", " ", words)
    #print(words)
    words <- unlist(strsplit(words," "))
    nwords <- length(words)
    for(i in 1:length(words)){
        pattern <- paste("(^| )",words[i],"($| )",sep="")
        n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)  
        n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE) 
        #need validation for if the word is blank
        an_title <- an_title + agrepl(words[i],title,ignore.case=TRUE,max.distance = 0.13)   #change
        an_desc <- an_desc + agrepl(words[i],desc,ignore.case=TRUE,max.distance = 0.13)      #change
    }
    return(c(n_title,nwords,n_desc,an_title,an_desc))
}


cat("Get number of words and word matching title in train\n")
train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
train$nmatch_title <- train_words[,1]
train$nwords <- train_words[,2]
train$nmatch_desc <- train_words[,3]
train$fuzzy_match_title <- train_words[,4]
train$fuzzy_match_desc <- train_words[,5]



cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
test$nmatch_title <- test_words[,1]
test$nwords <- test_words[,2]
test$nmatch_desc <- test_words[,3]
test$fuzzy_match_title <- test_words[,4]
test$fuzzy_match_desc <- test_words[,5]


#Bonus Staging!!

        #explore this fuzzy match:
        
        #train.explore <- mutate(train, )
        train.explore <- filter(train, relevance == 3, fuzzy_match_desc == 0, fuzzy_match_title == 0)
        train <- mutate(train, 'percent_title'=(fuzzy_match_title/nwords) * 100, 
                                  'percent_desc'=(fuzzy_match_desc/nwords) * 100)
        test <- mutate(test, 'percent_title'=(fuzzy_match_title/nwords) * 100, 
                        'percent_desc'=(fuzzy_match_desc/nwords) * 100)
        
        
    
        #figure out this "" issue:
    
                #?gsub
                #grepl("/[ ]+", "simpson   strong ties")
                #grepl("[ ]{2,}", "simpson   strong ties", perl=TRUE, ignore.case=TRUE)
                #gsub("[ ]{2,}", " ", "simpson   strong      ties yes")
                #words <- unlist(strsplit("simpson strong  ties"," "))
                #words


rm(train_words,test_words)

cat("A simple linear model on number of words and number of words that match\n")
glm_model <- glm(relevance~nmatch_title+nmatch_desc+nwords,data=train)
summary(glm_model)
test_relevance <- predict(glm_model,test)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

#high score of 0.50164
cat("A simple linear model on number of words and number of words that match\n")
glm_model2 <- glm(relevance~fuzzy_match_title+fuzzy_match_desc+nwords,data=train)
summary(glm_model2)
test_relevance2 <- predict(glm_model2,test)
test_relevance2 <- ifelse(test_relevance2>3,3,test_relevance2)
test_relevance2 <- ifelse(test_relevance2<1,1,test_relevance2)

test$test_relevance2 <- test_relevance2
submission <- data.frame(id=test$id,relevance=test$test_relevance2)
write_csv(submission,"../output/benchmark_submission_C13.csv")

cat("A simple linear model on number of words and number of words that match\n")
glm_model3 <- glm(relevance~percent_title+percent_desc+nwords,data=train)
summary(glm_model3)
test_relevance3 <- predict(glm_model3,test)
test_relevance3 <- ifelse(test_relevance3>3,3,test_relevance3)
test_relevance3 <- ifelse(test_relevance3<1,1,test_relevance3)

test$test_relevance3 <- test_relevance3


submission <- data.frame(id=test$id,relevance=test$test_relevance3)
write_csv(submission,"../output/benchmark_submission_C14.csv")
print(Sys.time()-t)

