
#setwd("~/Analytics/Kaggle/HomeDepot")
setwd("C:/Users/tjvan/Documents/Kaggle/HomeDepot_early_2016/homedepotgit")
#https://www.kaggle.com/dsoreo/home-depot-product-search-relevance/benchmark-score-script

#install.packages("readr")
library(readr)
library(data.table)
library(dplyr)
cat("Reading data\n")
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')
desc <- read_csv('../input/product_descriptions.csv')
attributes <- read_csv('../input/attributes.csv')
brandnames <- filter(attributes, name == "MFG Brand Name") 
colnames(brandnames) <- c("product_uid", "name", "brandname")

rm(attributes)


cat("Merge description with train and test data \n")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

cat("Merge the brand name with train and test data \n")
train <- merge(train,brandnames, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE); train$name = NULL
test <- merge(test,brandnames, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE); test$name = NULL

t <- Sys.time()
#this function takes a search, splits those words up, then attempts to find each of those words in the product title and description
#output is a how many words were matched in the title, how many words were there total in search phrase, how many words matched the description
word_match <- function(words,title,desc,brand){
    n_title <- 0
    n_desc <- 0
    n_brand <- 0
    #print(words)  #mine
    #print(title)  #mine
    #print(desc)   #mine
    words <- unlist(strsplit(words," "))
    nwords <- length(words)
    for(i in 1:length(words)){
        pattern <- paste("(^| )",words[i],"($| )",sep="")
        n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
        n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
        n_brand <- n_brand + grepl(pattern,brand,perl=TRUE,ignore.case=TRUE)
    }
    return(c(n_title,nwords,n_desc, n_brand))
}

cat("Get number of words and word matching title in train\n")
train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description,train$brandname)))
#train2_words <- as.data.frame(t(mapply(word_match, train2$search_term, train2$product_title, train2$product_description))) #mine
train$nmatch_title <- train_words[,1]
train$nwords <- train_words[,2]
train$nmatch_desc <- train_words[,3]
train$nmatch_brand <- train_words[,4]

cat("Get number of words and word matching title in test\n")
test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description,test$brandname)))
test$nmatch_title <- test_words[,1]
test$nwords <- test_words[,2]
test$nmatch_desc <- test_words[,3]
test$nmatch_brand <- test_words[,4]

rm(train_words,test_words)

write_csv(test, "../output/hdp_testwbrand.csv")
write_csv(train, "../output/hdp_trainwbrand.csv")


# ********* Modeling begins here - moving that function to Azure ML ***************


cat("A simple linear model on number of words and number of words that match\n")
glm_model <- glm(relevance~nmatch_title+nmatch_desc+nwords,data=train)

glm_model
summary(glm_model)

#predict takes in a model and then the test data set without the thing being predicted.
test_relevance <- predict(glm_model,test)

#this data set is the test dataset with 
test_relevance

test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

head(test_relevance)


test_relevance <- ifelse(test_relevance>3,3,test_relevance)

summary(test_relevance)

#order!
test$test_relevance <- test_relevance
test <- arrange(test, id)



baseline_submission <- data.frame(id=test$id,relevance=test$test_relevance)
baseline_submission <- arrange(baseline_submission, id)
write_csv(baseline_submission,"../output/baseline_submission.csv")
print(Sys.time()-t)


# ****************** ALL MINE ***************************************

#add ratios to train
    names(train)
    train2 <- mutate(train, "titleRatio"=round((nmatch_title / nwords),2),
                     "descRatio"=round((nmatch_desc / nwords),2),
                     "brandRatio"=round((nmatch_brand / nwords),2))
    head(train2)
    
    train2 <- mutate(train2, "titleRatio100"=titleRatio * 100,
                     "descRatio100"=descRatio * 100,
                     "brandRatio100"=brandRatio * 100)

    #plot these for train (doesn't matter for test)
    plot(train2$titleRatio100, train2$relevance)
    plot(train2$descRatio100, train2$relevance)
    plot(train2$brandRatio100, train2$relevance)
    
    #plot standards
    plot(train2$nmatch_desc, train2$relevance)
    plot(train2$nmatch_title, train2$relevance)
    plot(train2$nmatch_brand, train2$relevance)
    
#add ratios to test
    names(test)
    test2 <- mutate(test, "titleRatio"=round((nmatch_title / nwords),2),
                     "descRatio"=round((nmatch_desc / nwords),2),
                     "brandRatio"=round((nmatch_brand / nwords),2))
    head(test2)
    
    test2 <- mutate(test2, "titleRatio100"=titleRatio * 100,
                     "descRatio100"=descRatio * 100,
                     "brandRatio100"=brandRatio * 100)
    
test2.1 <- test2[, c(1:2, 7:17)]
    
write_csv(test2, "../traintest/test2.csv")
write_csv(test2.1, "../traintest/test2.1.csv")

write_csv(train2, "../traintest/train2.csv")

str(test2)


