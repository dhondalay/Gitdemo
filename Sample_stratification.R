library(dplyr)
########################################################################################################################
# input data
data <- read.csv(file.choose(), header= TRUE, sep=",")
rownames(data) <- data[,1]
data[,1] <- NULL
# Declaring the data frame
# set.seed(1) # to initialise
# n <- 1e5 # to be set tot he maximumun number of variables
# d <- data.frame(age = sample(1:5,n,TRUE),
#                lc = rbinom(n,1,.5),
#                ants = rbinom(n,1,.7))
########################################################################################################################
# Sample subset defining
n = nrow(data)
train = round(n*(60/100))
test = round(n*(20/100))
valid = round(n*(20/100))
########################################################################################################################

# Splitting the data frame

sp <- split(data, list(d$age, d$lc))
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 30, FALSE),])
out <- do.call(rbind, samples)
str(out)
head(out)
# OR
out2 <- d %>%
  group_by(age, lc) %>%
  sample_n(30)
str(out2)
head(out2)

########################################################################################################################
train_subset <- data[sample(1:nrow(data), train, replace=FALSE),]

data$class[1:6] <- c("A")
data$class[7:8] <- c("B")
data$class[9:10] <- c("C")

# cumList <- list(data$class)

cumList <- data$class
# probVec <- c(train, test, valid)

probVec <- c(60/100,20/100,20/100)
cumProbVec <- cumsum(probVec)

cumList <- list(data$class)
probVec <- c(train, test, valid)
cumProbVec <- n

ret <- NULL
for( i in 1:nrow(data)){
  rand <- runif(1)
  whichVec <- which(rand < cumProbVec)[1]
  ret <- c(ret,sample(cumList[[whichVec]],1))
}
ret

#Testing the results
length(which(ret %in% "A")) # Almost 1/2*20000 of the values
length(which(ret %in% "B")) # Almost 1/6*20000 of the values
length(which(ret %in% "C")) # Almost 1/6*20000 of the values

data$class <- ret




########################################################################################################################


#load the iris data
data(iris)

# this data has 150 rows
nrow(iris)

# look at the first few
head(iris)

# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#apply the function
splits <- splitdf(iris, seed=808)

#it returns a list - two data frames called trainset and testset
str(splits)

# there are 75 observations in each data frame
lapply(splits,nrow)

#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset
