library(pROC)
library(readr)
library(dplyr)

n_rep = 10

threshold <- c(8,9,10,11,12,13)

for (th in threshold){

	df <- read_tsv(paste0("CUH_SmartDR_test_rfclass_upper",th,"_rep0_predict.txt"))
	pred <- pull(df,predicted_PostTiterLog2)

	for (i in 1:(n_rep-1)){
		df <- read_tsv(paste0("CUH_SmartDR_test_rfclass_upper",th,"_rep",i,"_predict.txt"))
		pred <- pred + pull(df,predicted_PostTiterLog2)
	}

	pred <- pred / 10
	actual <- ifelse(df$PostTiterLog2 >= th, 1, 0)

	roc.res <- roc(actual, pred)
	pROC::auc(roc.res)
	rocplot <- paste0("CUH_SmartDR_test_ROCplot_rfclass_upper",th,".pdf")
	pdf(rocplot,useDingbats=FALSE)
	plot.roc(roc.res,col="black")
	text(0.3, 0.3, labels=sprintf("AUC: %0.3f", pROC::auc(roc.res)),cex=2)
	text(0.3, 0.15, labels=paste0("positive: ",sum(actual),"\nnegative: ",length(actual)-sum(actual)),cex=2)
	#sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=ROC_colors[i]))
	dev.off()
}