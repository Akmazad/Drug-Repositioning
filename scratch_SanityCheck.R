drugMetaData = read.csv("./data/DB_full_meta_data.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
DSN <- read.csv("./data/new_net_info.csv",header = TRUE,sep = ",", stringsAsFactors = FALSE)
noStructure <- drugMetaData[which(is.na(drugMetaData["Smiles"])),1] 
# length(noStructure)
# [1] 2434
DSN_dr1_nonEmptyStructures <- DSN[-which(as.vector(DSN[,1]) %in% noStructure),]
DSN_dr1_dr2_nonEmptyStructures <- DSN_dr1_nonEmptyStructures[-which(as.vector(DSN_dr1_nonEmptyStructures[,2]) %in% noStructure),]

onlyApproved <- drugMetaData[which(drugMetaData["Groups"] == "approved"),1]
# length(onlyApproved)
# [1] 1990
DSN_dr1_dr2_nonEmptyStructures_onlyApproved <- DSN_dr1_dr2_nonEmptyStructures[which(as.vector(DSN_dr1_dr2_nonEmptyStructures[,2]) %in% onlyApproved),]

rm(DSN)
rm(DSN_dr1_nonEmptyStructures)
rm(DSN_dr1_dr2_nonEmptyStructures)
