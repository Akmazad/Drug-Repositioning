library(msa)
library(Biostrings)
library(seqinr)
library(foreach)
library(doParallel)
library(optparse)
library(logger)

### Option list
option_list = list(
  make_option("--dataDir", type="character", default=NULL, required=T,
              help="Fasta file directory", metavar="character"),
  make_option("--dataFile", type="character", default=NULL, required=T, 
              help="Fasta file name", metavar="character"),
  make_option("--startRow", type="int", default=1, required=T, 
              help="Starting row number"),
  make_option("--offset", type="int", required=T, 
              help="Ending row offset"),
  make_option("--nClusters", type="int", default=122, 
              help="Ending row offset"),
  make_option("--outFile", type="character", default="out", 
              help="output file name [default= %default]", metavar="character")
)
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


### Make Cluster nodes for parallelizing
cl <- makeCluster(opt$nClusters)
registerDoParallel(cl)
log_info(paste0(opt$nClusters, " are created and registered"))

## Read Fasta file
fasta <- readAAStringSet(paste0(opt$dataDir, opt$dataFile))
protSim <- function(seq1,seq2){
  return(Biostrings::pid(Biostrings::pairwiseAlignment(seq1,seq2)))
}

## 
n = opt$offset
startRow = opt$startRow
endRow = opt$startRow + opt$offset -1
endRow = if (endRow > length(fasta)) length(fasta) else endRow
mat = matrix(0, nrow = opt$offset, ncol = length(fasta))

start_time <- Sys.time()
foreach(i = 1:nrow(mat), .combine='rbind') %:% 
  foreach(j = 1:ncol(mat), .combine='cbind', .packages='Biostrings') %dopar% {
    mat[i,j] = protSim(fasta[startRow + i - 1],fasta[j]);
  }
write.csv(mat, paste0(opt$dataDir, opt$outFile,"_",startRow, "_",endRow,".csv"), quote = F, row.names = F)
end_time <- Sys.time()
stopCluster(cl)
log_info(end_time - start_time)
