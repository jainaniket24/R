getwd()
setwd("D:/R/Udemy/Session 4")
setwd("./Weather Data")
Chicago <- read.csv('Chicago-F.csv', row.names = 1)
chicago
NewYork <- read.csv('NewYork-F.csv', row.names = 1)
Houston <- read.csv('houston-F.csv', row.names = 1)
SanFrancisco <- read.csv('SanFrancisco-F.csv', row.names = 1)
Chicago
NewYork
Houston
SanFrancisco
is.data.frame(Chicago)
is.data.frame(SanFrancisco)
#converting to matrices
Chicago <- as.matrix(Chicago)
NewYork <- as.matrix(NewYork)
SanFrancisco <- as.matrix(SanFrancisco)
Houston <- as.matrix(Houston)
is.matrix(Chicago)
weather <- list(Chicago=Chicago, NewYork=NewYork, Houston=Houston, SanFrancisco=SanFrancisco)
weather[4]
weather$Chicago
#Using Apply()
apply(Chicago, 1, mean)
mean(Chicago['DaysWithPrecip',])
#analyze one city
apply(Chicago, 1, max)
apply(Chicago, 1, min)
#Compare Cities
apply(chicago, 1, mean)
apply(NewYork, 1, mean)
apply(Houston, 1, mean)
apply(SanFrancisco, 1, mean)

#recreating apply function with loops
Chicago

#1. via loops

output <- NULL
for(i in 1:nrow(Chicago)){
    output[i] <- mean(Chicago[i,])
}
output
names(output) <- rownames(Chicago)
output

#2. via loops

apply(Chicago, 1, mean)

#using lapply

Chicago
t(Chicago) #taking transpose
weather
#taking transpose of list
lapply(weather, t)
newlist <- lapply(weather, t)
#another example
rbind(Chicago, Newrow = 1:12)
lapply(weather, rbind, Newrow=1:12)

#example 3
rowMeans(Chicago)
apply(Chicago, 1, mean) # same result
#apply(weather, 1, rowMeans) doesn't work
lapply(weather, rowMeans) # list of (rowMeans(weather$Chicago), rowMeans(weather$NewYork), rowMeans(weather$Houston), rowMeans(weather$SanFrancisco))
Chicago
#total preci in chicago
sum(Chicago[4,])
#getting total precipitation for all 4 cities
weather
sum(weather$Chicago[4,])
#getting a list as: sum(weather$Chicago[4,]), sum(weather$NewYork[4,]), sum(weather$Houston[4,]), sum(weather$SanFrancisco[4,])
lapply(weather, function(x){sum(x[4,])})

#combinig lapply with [] operator
weather$Chicago[1,1] #or we can also write
weather[[1]][1,1]
#this is avg high temp in Jan for Chicago; now getting it for all 4 cities
lapply(weather,"[", 1,1)
#what this has basically done is returned a list of:
#weather[[1]][1,1], weather[[2]][1,1], weather[[3]][1,1], weather[[4]][1,1]

#example: getting all weather info for chicago for Jan
Chicago[,1] #or accessing with list operator as
weather[[1]][,1]
#getting Jan weather info for all cities with lapply
lapply(weather, "[", ,1) #basically returns a list of
#weather[[1]][,1], #weather[[2]][,1], #weather[[3]][,1], #weather[[4]][,1]

#getting weather info for Mar:
lapply(weather, "[", ,3)
#getting only the avg high temp for Mar
lapply(weather, "[", 1,3)
#getting same thing with loop:

output <- NULL
for(i in 1:length(weather)){
    output[i] <- weather[[i]][1,3]
  }
output
names(output) <- names(weather)
output
typeof(output)
as.list(output)
as.numeric(lapply(weather, "[", 1,3))

#adding you own functions
weather
lapply(weather, function(x) x[1,]) #gets us the first row of each component of weather
lapply(weather, function(x) x[,12]) #gets us all the data for 12th col i.e. december
#finding difference in avg max an avg min temp
lapply(weather, function(newName) newName[1,]-newName[2,])
#making it as a proportion of avg low temp
lapply(weather, function(any) (any[1,]-any[2,])/any[2,])
#rounding off to 2 decimals
lapply(weather, function(any) round(((any[1,]-any[2,])/any[2,]),2))
#making as %
lapply(weather, function(any) round(((any[1,]-any[2,])/any[2,]),4)*100)
#calculating avg precipitation per day: as avg preciptation/days of precipitation
weather
lapply(weather, function(x) round((x[3,]/x[4,]),2))
#or calculating averagre precipitation per day with precipitation in entire year
lapply(weather, function(z) round((sum(z[3,])/sum(z[4,])),2))

# Using sapply
lapply(weather, "[", 1,7) #avg max temp in July for all cities
sapply(weather, "[", 1,7) #avg max temp in july in a much better format
sapply(weather, "[", 1,10:12) #avg max temp in Oct, nov, dec
#Deliverable 1: average of all weather parameters across years for all cities
#using lapply; already done that before
lapply(weather, rowMeans)
#using sapply for a much better representation
sapply(weather, rowMeans)
round(sapply(weather, rowMeans),2) # <<- final deiverable 1! Hurray!!
#Now on to deliverable 2 - variation in temperatures
lapply(weather, function(x) round((x[1,]-x[2,])/x[2,],2))
#simplify
sapply(weather, function(x) round((x[1,]-x[2,])/x[2,],2)) # << deliverable 2! Hurray!!

#Nesting apply functions
Chicago
apply(Chicago, 1, mean) # for only 1 element in the list
lapply(weather, rowMeans) # for all elements in list
# but there is no pre-defined function like rowMax for finding maximums across rows in a list
apply(Chicago, 1,max)
#for entire weather list:
lapply(weather, function(x) apply(x, 1, max)) # using apply function for individual components as a user-defined function
lapply(weather, apply, 1, max) # better approach,
# bcoz, apply is already defined and uses 1st param from lapply as 1st param for itself. we only need to define remaining params

#tidy display
sapply(weather, apply, 1, max) # << deliverable 3
sapply(weather, apply, 1, min) # << deliverable 4

#which.max (very advanced topic)

Chicago
Chicago[1,] # avg high temp in chicago
which.max(Chicago[1,]) # getting which month - but this returns us the index (as usually which returns)
names(which.max(Chicago[1,])) #returns us the name of the month for max avg high temp in chicago
#deliverable 5
# getting month in which max of each parameter occurs for each city
names(which.max(Chicago[1,])) # gives us just for 1 parameter (iteration #1) and 1 city (iteration #2)
#iteration #1: we need to run same code over all rows of same city -> use apply for applying same function to all rows/cloumns of a matrix
#iteration #2: we need to run same code over all elements of the list -> use lapply or sapply

apply(Chicago, 1, function(x) names(which.max(x))) #Iteration #1
lapply(weather, apply, 1, function(x) names(which.max(x))) #iteration #2
sapply(weather, apply, 1, function(x) names(which.max(x))) #iteration #2 - improved
#another method:
sapply(weather, function(x) apply(x, 1, function(y) (names(which.max(y))))) #using a user defined function and apply inside that function
weather

























