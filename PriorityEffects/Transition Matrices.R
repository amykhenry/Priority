##########################

#January 29, 2018

column <- t0$Column..Letter.[2]

results.tm <- matrix(0, nrow = length(unique(data$Species)), 
                     ncol = length(unique(data$Species)))

UniqueSpecies <- as.vector(unique(data$Species))
nums <- c(1:14)
UniqueSpeciesIndex <- cbind(UniqueSpecies, nums)
colnames(results.tm) <- unique(data$Species)
rownames(results.tm) <- unique(data$Species)

head(data)
SpeciesCode <- c() #makes an empty vector that we'll add as a new column to data
for(j in 1:nrow(data)){ #this starts our for loop and defines how many rows we'll go over
  Spec <- data$Species[j] #This selects what species is present in that row
  Index <- UniqueSpeciesIndex[UniqueSpecies == Spec] #this selects the row of the index that matches our species
  SpeciesCode[j] <- as.numeric(Index[2]) #
}

data <- cbind(data,SpeciesCode)

t0 <- subset(data, Day == 0)
t25 <- subset(data, Day == 25)
t58 <- subset(data, Day == 58)
t84 <- subset(data, Day == 84)
t113 <- subset(data, Day == 113)

for(i in 1:nrow(t0)){
  #column <- t0$Column..Letter.[i]
  #row <- t0$Row..Number.[i]
  species.t0 <- t0$SpeciesCode[i]
  species.t1 <- t25$SpeciesCode[i]
  results.tm[species.t0,species.t1] <- results.tm[species.t0,species.t1] + 1
}

tcount <- function(t0,t1){
  results.tm <- matrix(0, nrow = length(unique(data$Species)), 
                       ncol = length(unique(data$Species)))
  colnames(results.tm) <- unique(data$Species)
  rownames(results.tm) <- unique(data$Species)
  for(i in 1:nrow(t0)){
    #column <- t0$Column..Letter.[i]
    #row <- t0$Row..Number.[i]
    species.t0 <- t0$SpeciesCode[i]
    species.t1 <- t1$SpeciesCode[i]
    results.tm[species.t0,species.t1] <- results.tm[species.t0,species.t1] + 1
  }
  return(results.tm)
}

Booger <- tcount(t25,t58)

Brains <- tcount(t58,t84)
Brawn <- tcount(t84,t113)

Totes <- results.tm + Booger +Brains +Brawn
sum(Totes)
Totally <- Totes/sum(Totes)

eig <- eigen(Totally)
eig$vectors[,1]

barplot(as.numeric(eig$vectors[,1]))