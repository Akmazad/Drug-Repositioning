library(gtools)
datadir <- "C:\\Users\\Azad\\OneDrive - UNSW\\Vafaee Lab\\Projects\\Deep Brain\\Training 2\\protSims\\"
files <- mixedsort(list.files(path = datadir, pattern="*.csv", recursive=TRUE, full.names=TRUE), decreasing = F)

mat <- NULL
for(i in 1:length(files)){
  temp <- read.table(file = files[i], header = T, sep = ",")
  mat = rbind(mat,temp)
}
# colnames(mat) <- paste0("protein.", rep(1, length(temp1), 1))
write.csv(mat, "testCombined.csv", row.names = F)
