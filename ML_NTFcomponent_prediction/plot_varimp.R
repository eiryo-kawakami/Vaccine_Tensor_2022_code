library(ggplot2)
library(matrixStats)
library(dplyr)
library(tidyr)
library(readr)
library(RColorBrewer)

cutoff <- function(num,u_lim){
  if(num > u_lim){
    return(u_lim)
  }else return(num)
}

basis_list <- c("basis1","basis2","basis3","basis4")

n_rep = 10

tmp_summary <- read_tsv(paste0("CUH_SmartDR_rfclass_basis1_rep0_varimp.txt"))

imp_summary <- data.frame(feature=pull(tmp_summary,"X1"))

for (b in basis_list){

		tmp_summary <- read_tsv(paste0("CUH_SmartDR_rfclass_",b,"_rep0_varimp.txt"))
		tmp_imp <- pull(tmp_summary,"0")

		for (i in 1:(n_rep-1)){

			df <- read_tsv(paste0("CUH_SmartDR_rfclass_",b,"_rep",i,"_varimp.txt"))

			tmp_imp <- tmp_imp + pull(tmp_summary,"0")
		}

		imp_summary[b] <- tmp_imp / 10

}

top_imp <- imp_summary[head(rev(order(rowMaxs(as.matrix(imp_summary[,c(2:5)])))),15),]

feature_order <- rev(top_imp$feature)

df <- top_imp %>% mutate(feature=feature) %>% pivot_longer(col=-feature,names_to="basis")

df$value <- sapply(df$value,u_lim=0.04,cutoff)

df$feature <- factor(df$feature,levels=feature_order)
df$basis <- factor(df$basis)

p <- ggplot(df, aes(x=basis, y=feature, fill=value))
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
p <- p + scale_fill_gradientn("value", colours = c("white",brewer.pal(9, "YlGnBu")), na.value = "white", limits=c(0, NA) )

ggsave("CUH_SmartDR_rfclass_varimp_heatmap.pdf",plot=p,width=6,height=8)
