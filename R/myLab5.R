#In this lab you will use simulation to produce some common  distributions

## Binomial
# sample will replacate a flip of a coin with head and
#tail as the out comes. Odds are 50 50
sample(c("H","T"),size=10,prob=c(1/2,1/2),replace=TRUE)

#Replicates coin flip but outcome is 1 and 0 1 is success
## Binomial with a random variable use
sample(c(1,0),size=10,prob=c(1/2,1/2), replace=TRUE)


## Multinomial
#Boxes
n=5
B=paste(rep("B",n),1:n,sep="")
B
#All boxes (categories) equally likely
sample(B,size=20,prob=c(1/5,1/5,1/5,1/5,1/5),replace=TRUE)

## sampling function
# iter = iterations, n=sample size
# set default values
#' Title
#'
#' @param iter number of success and failure counted
#' @param n size of the count
#' @param p precentage of success
#'
#' @return vector of the count
#' @export
#'
#' @examples mybin(2, 2, .6) = (1,1) (1,0)
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
mybin(iter=1000,n=18, p=0.3)

## Try a multinomial

#' Title
#'
#' @param iter number of smaples built
#' @param n  size of the smaples
#' @param p percent of success
#'
#' @return table of maxix success of each test with multimple
#' varibles included
#' @export
#'
#' @examples mymult(2, 2 , p = c(1,1)/2)
mymult=function(iter=100,n=10, p=c(1,1,1,1)/4){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #The number of categories is k
  k=length(p)
  # Make a matrix that will hold the frequencies in each sample
  tab.mat=matrix(NA,nr=k,nc=iter, byrow=TRUE)


  for(i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(1:k,n,replace=TRUE, prob=p)
    #Collect all the frequencies of each of the k values
    tab.mat[,i]=table(factor(sam.mat[,i],levels=1:k))
  }
  # sum the frequecies
  freq=apply(tab.mat,1,sum)
  # put names to them
  names(freq)=1:k
  #create a barplot of refative freq
  barplot(freq/(n*iter),col=rainbow(k) )
  tab.mat
}
mymult(iter=1000,n=10,p=c(1,2,3,4,2)/12)


## R uses a number of built in distributions
## These all begin with r for random sampling
## Use ?distribution to see a more complete list
?rbinom
?rmultinom
?rpois
?rhyper
#builds a sample of of number from 1 to 10
#returns a vector of the number counted
#graph the sample to bar graph
#' Title
#'
#' @param n  size of the sample
#' @param iter how many time the sample is ran
#' @param time on the interval
#'
#' @return vector of the amount of different graphs ran
#' @export
#'
#' @examples sample(5,5,1)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}

mysample(n=1000, iter=30)

# Some examples of calculation
#4.25
dbinom(2,5,0.25)
dbinom(0:1,5,0.25)
pbinom(1,5,0.25)

#4.35
1-pbinom(8,15,1/5)
pbinom(2999,10000,1/5)
