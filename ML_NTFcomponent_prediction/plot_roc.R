library(pROC)
library(readr)
library(dplyr)


basis_list <- c("basis1","basis2","basis3","basis4")

for (b in basis_list){

	df <- read_tsv(paste0("CUH_SmartDR_test_rfclass_",b,"_rep0_predict.txt"))

	pred <- pull(df,"pred_prob")
	actual <- ifelse(pull(df,b)>1,1,0)

	for (i in 1:9){
		df <- read_tsv(paste0("CUH_SmartDR_test_rfclass_",b,"_rep",i,"_predict.txt"))
		pred <- pred + pull(df,"pred_prob")
	}

	pred <- pred / 10

	roc.res <- roc(actual, pred)
	pROC::auc(roc.res)
	rocplot <- paste0("CUH_SmartDR_test_ROCplot_rfclass_",b,".pdf")
	pdf(rocplot,useDingbats=FALSE)
	plot.roc(roc.res,col="black")
	text(0.3, 0.3, labels=sprintf("AUC: %0.3f", pROC::auc(roc.res)),cex=2)
	text(0.3, 0.15, labels=paste0("positive: ",sum(actual),"\nnegative: ",length(actual)-sum(actual)),cex=2)
	#sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=ROC_colors[i]))
	dev.off()
}
