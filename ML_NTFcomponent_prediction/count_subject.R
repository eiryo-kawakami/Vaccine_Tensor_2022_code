library(readr)
library(dplyr)

df <- read_tsv("CUH_SmartDR_20210727_mask_patient_score_with_cluster_normalized.tsv")

basis_list <- c("basis1","basis2","basis3","basis4")
for (b in basis_list){
	actual <- ifelse(pull(df,b)>1,1,0)
	print(b)
	print(paste0("positive: ",sum(actual)))
	print(paste0("negative: ",length(actual)-sum(actual)))
}