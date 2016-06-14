
links   <- read.csv("data/ml-latest-small/links.csv",   header=T)
movies  <- read.csv("data/ml-latest-small/movies.csv",  header=T)
ratings <- read.csv("data/ml-latest-small/ratings.csv", header=T)
tags    <- read.csv("data/ml-latest-small/tags.csv",    header=T)

ratingsMatrix <- read.csv("ml-latest-small_ratingsMatrix.csv", header=F)
ratedMatrix   <- read.csv("ml-latest-small_ratedMatrix.csv",   header=F)

rownames(ratingsMatrix) <- c()
rownames(ratedMatrix)   <- c()

ratingsMatrix <- as.matrix(ratingsMatrix)
ratedMatrix   <- as.matrix(ratedMatrix)
movieIdColumn <- seq(1:nrow(ratingsMatrix))

ratingsMatrixFinal <- ratingsMatrix[as.logical(rowSums(ratingsMatrix != 0)), ]
ratedMatrixFinal   <- ratedMatrix  [as.logical(rowSums(ratingsMatrix != 0)), ]
movieIdColumnFinal <- movieIdColumn[as.logical(rowSums(ratingsMatrix != 0))  ]


collaborativeFilterCost <- function (X, Y, Theta, R, lambda) {
    # 1. cost function
    J <- 1/2 * ( (X%*%t(Theta)) - Y )^2 * R;
    
    reguTheta <- lambda/2 * sum(Theta^2) # Theta regularization
    reguX     <- lambda/2 * sum(X^2)     # X regularization
    
    J <-  sum(J) + reguTheta + reguX
    
    # 2. gradients
    X_grad     <- ((X%*%t(Theta)-Y)  * R) %*% Theta + lambda * X;
    Theta_grad <- t((X%*%t(Theta)-Y) * R) %*% X     + lambda * Theta;
    
    list(J,X_grad,Theta_grad)
}


Y <- ratingsMatrixFinal
R <- ratedMatrixFinal

##==============================================================

## My Ratings
myratingsraw <- read.csv("myratings_mod.csv",header=T)
findRow <- function(item,vec) which(vec==item)
myratingsrows <- unlist(
    sapply(myratingsraw$movieId,
           findRow,movieIdColumnFinal))

myratings <- numeric(nrow(Y))

for (i in 1:length(myratingsrows)) {
    r <- myratingsrows[i]
    myratings[r] <- myratingsraw$rating[i]
}
myY <- cbind(myratings, Y)
myR <- cbind(as.integer(myratings != 0), R)
    

myY_sum <- rowSums(myY)
myR_sum <- rowSums(R)
Ymean   <- myY_sum/myR_sum

## Compute
Y <- myY
R <- myR
for (i in 1:ncol(Y)) {
    Y[,i] <- Y[,i] - Ymean
}

nmovies   <- nrow(Y)
nusers    <- ncol(Y)
nfeatures <- 20

lambda <- 1
niter  <- 30000
alpha  <- 0.00004

costvec <- numeric(niter)

set.seed(1234)
X     <- matrix(runif(nmovies*nfeatures),nmovies,nfeatures)
Theta <- matrix(runif(nusers*nfeatures),nusers,nfeatures)

for (i in 1:niter) {
    collabfilt <- collaborativeFilterCost(X,Y,Theta,R,lambda)
    
    costvec[i] <- collabfilt[[1]]
    X_grad     <- collabfilt[[2]]
    Theta_grad <- collabfilt[[3]]
    
    X     <- X     - alpha * X_grad
    Theta <- Theta - alpha * Theta_grad
    
    print(paste0(as.character(c(i,costvec[i])),collapse=" "))
}

## prediction
findMovieNames <- function(item,vec)which(vec==item)
mymovierows <- unlist(
    sapply(movieIdColumnFinal,
           findMovieNames,
           movies$movieId))


p <- X %*% t(Theta) + Ymean
mypred <- data.frame(movieName=movies$title[mymovierows],
                     movieId=movieIdColumnFinal,
                     rating=round(p[,1],1)
                     )
mypred_sort <- mypred[with(mypred, order(-rating)), ]

# get movie year
getMovieYear <- function(text) {
    y1  <- gsub("[^0-9]","",gsub("[^0-9\\(\\)]","",text))
    len <- nchar(y1)
    y2  <- substr(y1, len-3, len)
}
mypred_sort$movieYear <- unlist(sapply(mypred_sort$movieName,getMovieYear))
mypred_sort$movieYear <- as.integer(mypred_sort$movieYear)
mypred_sort <- mypred_sort[with(mypred_sort, order(-movieYear,-rating)), ]

# save some data properties for quick access
totalRatings <- sum(colSums(ratedMatrixFinal))
save(totalRatings,file="total_ratings.RData")

avgNumRatings <- sum(colSums(ratedMatrixFinal))/length(colSums(ratedMatrixFinal))
save(avgNumRatings,file="data_averages.RData")

dimRatings <- dim(ratingsMatrixFinal)
save(dimRatings,file="data_dimensions.RData")
