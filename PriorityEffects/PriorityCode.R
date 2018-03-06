###### March 5, 2018

#clear everything from the console for a clean slate
rm(list=ls()) 

#Load the data
data <- read.csv(file = "PriSpatial.csv")

#trim to include only the first 6 columns
data <- data[,1:6]
data <- as.data.frame(data)
#Monster summary tables
tables <- table(data$Species, data$Plate.ID, data$DayNum)

#Serial barplots of species abundances
par(mfrow = c(1,1))
colors <- rainbow(nrow(tables[,1,]))
colors[2] <- "#8B0000"
colors[3] <- "#EE7600" 
colors[4] <- "#FFC125"
colors[5] <- "yellow"
colors[7] <- "#006400"
colors[9] <- "#7EC0EE"
colors[10] <- "#00008B"
colors[11] <- "#00008B"
colors[12] <- "#4169E1"
colors[14] <- "#68228B"
barplot(tables[,2,], col = colors, xlab = "Day")

#make the legend by itself
barplot(tables[1,,])
species <- rownames(tables[,1,])
species <- species[-1]
legend("topleft", legend = species, fill = colors[-1], cex =1)

#each plot as its own timeseries
par(mfrow = c(2,4))
for(i in 2:9){
  barplot(tables[,i,], col = colors, xlab = "Day")
}

#each day a different plot
par(mfrow = c(1,1))

#Plotting August 10
barplot(tables[,,8][,], col = colors, xlab = "Plate")
legend("topleft", legend = species, fill = colors[-1], cex = 1)

#Plot all timepoints in order in a series
for(i in 2:9){
  barplot(tables[,,i][,], col = colors, xlab = "Plate")
}

#Creating metadata for species IDs
#making a list of all the unique species
UniqueSpecies <- as.vector(unique(data$Species))
#putting them in alphabetical order
unspi <- sort(UniqueSpecies)
#creating a number ID column
nums <- c(1:15)
#binding those two columns together
UniqueSpeciesIndex <- cbind(unspi, nums)
UniqueSpeciesIndex <- as.data.frame(UniqueSpeciesIndex)
#
names <- c("Algae" ,"Ascidia ceratodes", "Bowerbankia sp.",
           "Bugula neritina", "Botrylloides schlosseri",
           "Botrylloides violaceus", "Diplosoma listerianum",
           "Distaplia occidentalis","Didemnum lahillei","Eggs",
           "Metridium senile","Obelia sp.","Schizoporella sp.",
           "Watersipora subtorquata", "No Species")
taxa <- c("Algae", "Solitary Asc.", "Bryozoan","Bryozoan",
          "Colonial Asc.","Colonial Asc.", "Colonial Asc.",
          "Colonial Asc.","Colonial Asc.","Mollusca", "Anemone","Hydrozoan",
          "Bryozoan", "Bryozoan", "No Species")
metadata <- cbind(UniqueSpeciesIndex, names, taxa)
metadata$nums <- as.character(metadata$nums)

#generate a vector of colors for taxon groups
colbytaxa <- rainbow(length(unique(taxa)))

#### Transition Matrices

#Generate subsets of data per date
t0 <- subset(data, DayNum == 0)
t11 <- subset(data, DayNum == 11)
t25 <- subset(data, DayNum == 25)
t43 <- subset(data, DayNum == 43)
t58 <- subset(data, DayNum == 58)
t71 <- subset(data, DayNum == 71)
t84 <- subset(data, DayNum == 84)
t101 <- subset(data, DayNum == 101)
t113 <- subset(data, DayNum == 113)

#unclear what needed for
#column <- t0$Column..Letter.[2]

#empty data frame for transition matrix to be generated into
results.tm <- matrix(0, nrow = length(unique(data$Species)), 
                     ncol = length(unique(data$Species)))


#UniqueSpecies <- as.vector(unique(data$Species))
#nums <- c(1:14)
#UniqueSpeciesIndex <- cbind(UniqueSpecies, nums)

#label the rows and columns of the results tm
colnames(results.tm) <- metadata$unspi
rownames(results.tm) <- metadata$unspi

head(data)
SpeciesCode <- c() #makes an empty vector that we'll add as a new column to data
for(j in 1:nrow(data)){ #this starts our for loop and defines how many rows we'll go over
  Spec <- data$Species[j] #This selects what species is present in that row
  Index <- metadata[unspi == Spec,] #this selects the row of the index that matches our species
  SpeciesCode[j] <- as.numeric(Index$nums) #
}

#Adds the column of Species numeric codes onto the big data
data <- cbind(data,SpeciesCode)

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


