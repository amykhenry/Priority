
data <- read.csv(file = "PriorityEffectsSpat_Dec2017.csv")
head(data)
summary(data)

data <- data[1:5960,] #removes last three lines of NAs

tables <- table(data$Species, data$Plate.ID, data$Day)

#table key
# tables[1,,] gives first species, row = Plate, col = Day, no species
# tables[,1,] gives first plate, row = Species, col = Day, no plate
# tables[,,1] gives first date, row = Species, col = Plate

#one barplot by itself
par(mfrow = c(1,1))
colors <- rainbow(nrow(tables[,1,]))
colors[4] <- "yellow"
colors[5] <- "#C0FF3E"
colors[7] <- "#006400"
colors[12] <- "#8470FF"
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
par(mfrow = c(1,5))
barplot(tables[,,1][,-1], col = colors, xlab = "Plate")
}
legend("topleft", legend = species, fill = colors[-1], cex = 1)
for(i in 2:5){
  barplot(tables[,,i][,-1], col = colors, xlab = "Plate")
}
species
names <- c("Algae" ,"Ascidia ceratodes", "Bowerbankia sp.",
           "Bugula neritina", "Botrylloides schlosseri",
           "Botrylloides violaceus", "Diademnum lahillei",
           "Distaplia occidentalis","Didemnum vexillum",
           "Metridium senile","Obelia sp.","Schizoporella sp.",
           "Watersipora subtorquata", "No Species")
taxa <- c("Algae", "Solitary Asc.", "Bryozoan","Bryozoan",
          "Colonial Asc.","Colonial Asc.", "Colonial Asc.",
          "Colonial Asc.","Colonial Asc.","Anemone","Hydrozoan",
          "Bryozoan", "Bryozoan", "No Species")
metadata <- cbind(species, names, taxa)


colbytaxa <- rainbow(length(unique(taxa)))





