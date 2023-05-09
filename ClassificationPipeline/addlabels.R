data <- read.csv2(file="/Users/schmuck/Library/CloudStorage/OneDrive-IndianaUniversity/PhD/TIMP_Classification/data/uniprot3_graph_normalized.csv", sep=",", stringsAsFactors = FALSE)

data[data$Species == "Nothoprocta Pentlandii", 4] <- 0
data[data$Species == "Letharia Columbiana", 4] <- 1

data <- data[, -c(1, 2, 3, 5)]

colnames(data)[1] <- "target"


write.table(data, file="uniprot_norm.csv", sep=",", row.names = FALSE)
