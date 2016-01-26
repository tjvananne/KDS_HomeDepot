
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

#this is for C5
for (i in 1:nrow(test)) {
    if (test[i, 10] > 0) {
        test_relevance[i] <- test_relevance[i] * 1.07
    }
}

test_relevance <- ifelse(test_relevance>3,3,test_relevance)

summary(test_relevance)

test$test_relevance <- test_relevance

submission_C9 <- data.frame(id=test$id,relevance=test_relevance)
write_csv(submission_C7,"../output/benchmark_submission_C9.csv")
print(Sys.time()-t)

#*****************MINE AGAIN ***************************************
library(data.table)
#all this is going to do is fit the first submission into the buckets of relevancy
       #attach our test relevance to the test set
       test$test_relevance <- test_relevance
       test$test_relevance2 <- test_relevance
       
       test <- data.table(test, key = "test_relevance")
       
       
       xlist <- c(1, 1.25, 1.33, 1.5, 1.67, 1.75, 2, 2.25, 2.33, 2.5, 2.67, 2.75, 3)        #fit to these numbers
       cleanrefdf <- data.frame(xlist, xlist)
       colnames(cleanrefdf) <- c("cleankey", "clean")
       
       cleanrefdf <- data.table(cleanrefdf, key = "cleankey")
       
       finalcleantest <- cleanrefdf[test, roll = "nearest"]
       
       
       
       submission2 <- data.frame(id=finalcleantest$id, relevance=finalcleantest$clean)
       submission2 <- arrange(submission2, id)
       
       hist(finalcleantest$test_relevance2)
       hist(test$test_relevance2)

write_csv(submission2, "benchmark_submission2.csv")


# ****************** ALL MINE ***************************************
library(dplyr)
library(data.table)


names(train)
withbrandmatch <- filter(train, nmatch_brand > 0)
withoutbrandmatch <- filter(train, nmatch_brand == 0)

mean(withbrandmatch$relevance)
summary(withbrandmatch$relevance)

mean(withoutbrandmatch$relevance)
summary(withoutbrandmatch$relevance)

names(train)

#so these are unreasonably high
train.high <- filter(train, relevance > 2.6, nmatch_title == 0, nmatch_desc == 0)

# need to mutate this
train.low <- mutate(train, 'zero_is_perfect_title'=nwords - nmatch_title, 'zero_is_perfect_desc'= nwords - nmatch_desc)

#and these are unreasonably low
train.low <- filter(train.low, relevance < 1.3, zero_is_perfect_title == 0, zero_is_perfect_desc == 0)





#lets do this bucket fitting on top of the benchmark script and see if it helps or hurts (must be robust!)
#fit into their buckets:
#with this system, ANYTHING over .5 will go to .67, anything .5 and under goes down to .33
#I also want to try a model that treats these as ordinal categories instead of numbers
xlist <- c(1, 1.25, 1.33, 1.5, 1.67, 1.75, 2, 2.25, 2.33, 2.5, 2.67, 2.75, 3)        #fit to these numbers
rawlist <- c(1, 1.11, 1.12, 1.13, 1.14, 1.15, 1.16, 1.28, 1.29, 1.3, 2.12, 2.13, 2.18, 2.27, 2.28, 2.29)           #raw list to test out

mydf <- data.frame(rawlist, rawlist)
colnames(mydf) <- c("key1", "raw")
cleanrefdf <- data.frame(xlist, xlist)
colnames(cleanrefdf) <- c("cleankey", "clean")

mydf <- data.table(mydf, key = "key1")
cleanrefdf <- data.table(cleanrefdf, key = "key2")

#cleandf <- mydf[ mydf2 , roll = "nearest" ]
finalcleanset <- mydf2[ mydf, roll = "nearest"]       #exactly what I want!!!!

#offset needs to be half the distance between the two nearest points, added to the number
cleanlist2 <- numeric()
xoffset1 <- .13
xoffset2 <- .04
xoffset3 <- .09


for(i in 1:length(rawlist)) {
       print(" ")
       print(i)
       #need to figure out what the decimal is...
       temp <- rawlist[i]
       print(temp)
       tempdec <- temp - as.integer(temp)
       print(tempdec)
       
       if(tempdec < 0.25 & tempdec > 0) {
              cleantemp <- xlist[findInterval(temp + xoffset1, xlist)]
              print(cleantemp)
              cleanlist2 <- c(cleanlist2, cleantemp)
       } else if(tempdec < 0.33 & tempdec > .25) {
              cleantemp <- xlist[findInterval(temp + xoffset2, xlist)]
              print(cleantemp)
              cleanlist2 <- c(cleanlist2, cleantemp)
       } else if(tempdec < 0.5 & tempdec > .33) {
              cleantemp <- xlist[findInterval(temp + xoffset3, xlist)]
              print(cleantemp)
              cleanlist2 <- c(cleanlist2, cleantemp)
       } else if(tempdec < 0.67 & tempdec > 0.5) {
              cleantemp <- xlist[findInterval(temp + xoffset3, xlist)]
              print(cleantemp)
              cleanlist2 <- c(cleanlist2, cleantemp)
       } else if(tempdec < 0.75 & tempdec > 0.67) {
              cleantemp <- xlist[findInterval(temp + xoffset2, xlist)]
              print(cleantemp)
              cleanlist2 <- c(cleanlist2, cleantemp)
       } else if(tempdec < 1 & tempdec > 0.75) {
              cleantemp <- xlist[findInterval(temp + xoffset1, xlist)]
              print(cleantemp)
              cleanlist2 <- c(cleanlist2, cleantemp)
       } else {
              cleantemp <- xlist[findInterval(temp, xlist)]
              print(cleantemp)
              cleanlist2 <- c(cleanlist2, cleantemp)
       }
}


cleanlist2

       #issue, findInterval does not round up
       xlist[findInterval(1.24, xlist)]   #1
       xlist[findInterval(1.25, xlist)]   #1.25
       
       
       
       
       #old, this works but not with how the xlist is set up now... this offset worked when I thought they were all .33 apart
       cleanlist <- numeric()
       for (i in 1:length(rawlist)) {
              if( rawlist[i] < .16) {            #this shouldn't be .16 anymore, need to find optimal
                     temp <- 0
                     cleanlist <- c(cleanlist, temp)
              } else {
                     temp <- xlist[findInterval(rawlist[i] + xoffset, xlist)]
                     cleanlist <- c(cleanlist, temp)
              }
       }
       
       print(cleanlist)




