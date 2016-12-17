d <- read.csv("armstrade.csv")

# sample data

# Name          United.States Russia  India   China   Germany
# United States 0             0       3004    0       1037
# Russia        16            0       22279   11132   0
# India         0             0       0       0       0
# China         0             0       0       0       0
# Germany       1393          4       285     69      0
# France        545           5       383     1954    128

# remove name column
d <- d[,-which(colnames(d)=="Name")]

# reformat table for export to graphing software Gephi
Source <- rep(0,5000)
Target <- rep(0,5000)
Weight <- rep(0,5000)
Type <- rep("Undirected",5000)
edgetable <- data.frame(Source,Target,Weight,Type)
index <- 1;

for (i in 1:105){
  for (j in (i + 1):106){
    trade <- d[i,j] + d[j,i]
    if(trade >= 100){
      edgetable$Source[index] <- i;
      edgetable$Target[index] <- j;
      edgetable$Weight[index] <- trade;
      index <- index + 1;
    }
  }
}

edgetable <- edgetable[edgetable$Source>0,]
write.csv(edgetable,"edgestable.csv",row.names=F)

# import the resulting CSV file into Gephic to generate network
