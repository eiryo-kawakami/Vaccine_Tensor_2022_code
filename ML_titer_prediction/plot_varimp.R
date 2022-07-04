library(ggplot2)
library(matrixStats)
library(dplyr)
library(tidyr)
library(RColorBrewer)

cutoff <- function(num,u_lim){
  if(num > u_lim){
    return(u_lim)
  }else return(num)
}

threshold_list <- c(8,9,10,11,12,13)

n_rep = 10

tmp_summary <- read.table(paste0("CUH_SmartDR_rfclass_upper8_rep0_varimp.txt"),header=TRUE,sep="\t",row.names=1)

for (i in 1:(n_rep-1)){

	df <- read.table(paste0("CUH_SmartDR_rfclass_upper8_rep",i,"_varimp.txt"),header=TRUE,sep="\t",row.names=1)

	tmp_summary <- data.frame(tmp_summary,df)
}

imp_summary <- data.frame(rowMedians(as.matrix(tmp_summary)))

for (th in threshold_list[2:6]){

	tmp_summary <- read.table(paste0("CUH_SmartDR_rfclass_upper",th,"_rep0_varimp.txt"),header=TRUE,sep="\t",row.names=1)

	for (i in 1:(n_rep-1)){

		df <- read.table(paste0("CUH_SmartDR_rfclass_upper",th,"_rep",i,"_varimp.txt"),header=TRUE,sep="\t",row.names=1)

		tmp_summary <- data.frame(tmp_summary,df)
	}

	imp_summary <- data.frame(imp_summary,rowMedians(as.matrix(tmp_summary)))
}

colnames(imp_summary) <- paste0("upper",threshold_list)
rownames(imp_summary) <- rownames(tmp_summary)

top_imp <- imp_summary[head(rev(order(rowMaxs(as.matrix(imp_summary)))),15),]

df <- top_imp %>% mutate(variables=rownames(top_imp)) %>% pivot_longer(col=-variables,names_to="threshold")

df$value <- sapply(df$value,u_lim=0.04,cutoff)

df$variables <- factor(df$variables,levels=rev(rownames(top_imp)))
df$threshold <- factor(df$threshold,levels=colnames(top_imp))

p <- ggplot(df, aes(x=threshold, y=variables, fill=value))
p <- p + geom_tile()
p <- p + theme_bw(base_size=16)
p <- p + theme(plot.background = element_blank(),
	panel.grid.minor = element_blank(),
	panel.grid.major = element_blank(),
	panel.background = element_blank(),
	axis.line = element_blank(),
	axis.ticks = element_blank(),
	strip.background = element_rect(fill = "white", colour = "white"),
	axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p <- p + scale_fill_gradientn("value", colours = c("white",brewer.pal(9, "YlGnBu")), na.value = "white", limits=c(0, NA))

ggsave("CUH_SmartDR_rfclass_varimp_heatmap.pdf",plot=p,width=5,height=5)
