

# testing agrep
setwd("C:/Users/tjvan/Documents/Kaggle/HomeDepot_early_2016/testingconcepts/agrep")

dataresult <- read.csv("agreptest.csv", stringsAsFactors = FALSE)


data[1,1]
data[1,2]

agrepl("aaaa", "bcbb")

agrep(data[1,1], data[1,2], ignore.case=TRUE, max.distance = 0.3)
    

    dist01 <- integer()
    for(i in 1:nrow(data)) {
        if(agrepl(data[i,1], data[i,2], ignore.case=TRUE, max.distance = 0.1)) {
            dist01 <- c(dist01, 1)   
        } else {
            dist01 <- c(dist01, 0)
        }
    }; dataresult <- cbind(dataresult, dist01)
    
    dist015 <- integer()
    for(i in 1:nrow(data)) {
        if(agrepl(data[i,1], data[i,2], ignore.case=TRUE, max.distance = 0.15)) {
            dist015 <- c(dist015, 1)   
        } else {
            dist015 <- c(dist015, 0)
        }
    }; dataresult <- cbind(dataresult, dist015)
    
    dist02 <- integer()
    for(i in 1:nrow(data)) {
        if(agrepl(data[i,1], data[i,2], ignore.case=TRUE, max.distance = 0.2)) {
            dist02 <- c(dist02, 1)   
        } else {
            dist02 <- c(dist02, 0)
        }
    }; dataresult <- cbind(dataresult, dist02)
    
    dist03 <- integer()
    for(i in 1:nrow(data)) {
        if(agrepl(data[i,1], data[i,2], ignore.case=TRUE, max.distance = 0.3)) {
            dist03 <- c(dist03, 1)   
        } else {
            dist03 <- c(dist03, 0)
        }
    }; dataresult <- cbind(dataresult, dist03)
    
    dist04 <- integer()
    for(i in 1:nrow(data)) {
        if(agrepl(data[i,1], data[i,2], ignore.case=TRUE, max.distance = 0.4)) {
            dist04 <- c(dist04, 1)   
        } else {
            dist04 <- c(dist04, 0)
        }
    }; dataresult <- cbind(dataresult, dist04)
    
    dist05 <- integer()
    for(i in 1:nrow(data)) {
        if(agrepl(data[i,1], data[i,2], ignore.case=TRUE, max.distance = 0.5)) {
            dist05 <- c(dist05, 1)   
        } else {
            dist05 <- c(dist05, 0)
        }
    }; dataresult <- cbind(dataresult, dist05)

    
    # Next steps from this lesson are: 
        # create new columns for fuzzy searches (at different levels if possible -- .1, .125, .15, .175, .2
        # find difference between words found with / without fuzzy match (inspect the ones strictly)
            #found because of fuzzy match
        # find the difference between frequencies for fuzzy / non-fuzzy matches
    
    

