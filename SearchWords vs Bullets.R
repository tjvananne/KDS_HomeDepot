

       #setwd("C:/Users/tjvan/Documents/Kaggle/HomeDepot_early_2016/homedepotgit")
       setwd("~/Analytics/Kaggle/HomeDepot/homedepotgit")
       
       
       library(readr)
       library(dplyr)
       cat("Reading data\n")
       train <- read_csv('../input/train.csv')
       test <- read_csv('../input/test.csv')
       desc <- read_csv('../input/product_descriptions.csv')
       att <- read_csv('../input/attributes.csv')
       
       
           att <- mutate(att, "IsBullet"=grepl("Bullet", name))
           att <- filter(att, IsBullet == TRUE)
           att.bullets <- att %>% group_by(product_uid) %>% summarise(bulletvalues=paste(value, collapse=" "))
           rm(att)
       
       
       cat("Merge description with train and test data \n")
       train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
       test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
       
       cat("Merge bullet points with train and test data \n")
       train <- merge(train, att.bullets, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
       test <- merge(test, att.bullets, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
       
       #put back in order (just in case)
       train <- arrange(train, id)
       test <- arrange(test, id)
       
       t <- Sys.time()
       word_match <- function(words,title,desc,bullets){
           n_title <- 0
           n_desc <- 0
           n_bullets <- 0
           words <- unlist(strsplit(words," "))
           nwords <- length(words)
           for(i in 1:length(words)){
               pattern <- paste("(^| )",words[i],"($| )",sep="")
               n_title <- n_title + grepl(pattern,title,perl=TRUE,ignore.case=TRUE)
               n_desc <- n_desc + grepl(pattern,desc,perl=TRUE,ignore.case=TRUE)
               n_bullets <- n_bullets + grepl(pattern,bullets,perl=TRUE,ignore.case=TRUE)
           }
           return(c(n_title,nwords,n_desc,n_bullets))
       }
       
       cat("Get number of words and word matching title in train\n")
       train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description,train$bulletvalues)))
       train$nmatch_title <- train_words[,1]
       train$nwords <- train_words[,2]
       train$nmatch_desc <- train_words[,3]
       train$nmatch_bullets <- train_words[,4]
       
       train <- arrange(train, id)
       
              #are all description and bullet values the same?
              train.question <- mutate(train, "bulletdescdiff"=nmatch_desc - nmatch_bullets)
              train.question <- filter(train.question, bulletdescdiff > 0 | bulletdescdiff < 0)
              train.question <- mutate(train.question, "diffsqrd"=bulletdescdiff^2)
              train.question <- arrange(train.question, desc(diffsqrd))                    #biggest difference in desc / bullets
       
       cat("Get number of words and word matching title in test\n")
       test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description,test$bulletvalues)))
       test$nmatch_title <- test_words[,1]
       test$nwords <- test_words[,2]
       test$nmatch_desc <- test_words[,3]
       test$nmatch_bullets <- test_words[,4]
       
       test <- arrange(test, id)
       
              #are all description and bullet values the same?
              test.question <- mutate(test, "bulletdescdiff"=nmatch_desc - nmatch_bullets)
              test.question <- filter(test.question, bulletdescdiff > 0 | bulletdescdiff < 0)
       
       #plot(train$nmatch_bullets, train$relevance)
       
       rm(train_words,test_words)
       
       
       
       cat("A simple linear model on number of words and number of words that match\n")
       glm_model <- glm(relevance~nmatch_bullets+nmatch_title+nmatch_desc+nwords,data=train)
       summary(glm_model)
       
              #normal LM?
              cat("A simple linear model on number of words and number of words that match\n")
              lm_model <- lm(relevance~nmatch_bullets+nmatch_title+nmatch_desc+nwords,data=train)
              summary(lm_model)
       
       
       test_relevance <- predict(glm_model,test)
       test_relevance <- ifelse(test_relevance>3,3,test_relevance)
       test_relevance <- ifelse(test_relevance<1,1,test_relevance)
       
       test$test_relevance <- test_relevance
       
       submission_bullets001 <- data.frame(id=test$id,relevance=test$test_relevance)
       write_csv(submission_bullets001,"../output/submission_bullets001.csv")
       print(Sys.time()-t)
       
       head(test)
       
       write_csv(train, "../traintest/HD1_baseline_train_bullets.csv")
       write_csv(test, "../traintest/HD1_baseline_test_bullets.csv")


