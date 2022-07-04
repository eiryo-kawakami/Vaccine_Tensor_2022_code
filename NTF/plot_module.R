library(ggplot2)
library(readr)
library(gridExtra)
library(RColorBrewer)
library(ggridges)
library(dplyr)
library(tidyr)
library(pheatmap)

cutoff <- function(num,limit){
  if(num > limit){
    return(limit)
  }else if(num < -limit){
    return(-limit)
  }else return(num)
}

save_pheatmap_pdf <- function(x, filename, width=7, height=7) {
    stopifnot(!missing(x))
    stopifnot(!missing(filename))
    pdf(filename, width=width, height=height)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
}

nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

temporal_module <- read_tsv("CUH_SmartDR_20210727_mask_temporal_module.txt")
spatial_module <- read_tsv("CUH_SmartDR_20210727_mask_spatial_module.txt")

colnames(temporal_module) <- c("X1","basis2","basis1","basis3","basis4")
colnames(spatial_module) <- c("X1","basis2","basis1","basis3","basis4")

temporal_module_long <- temporal_module %>% pivot_longer(-X1, names_to = "basis", values_to = "value")
colnames(temporal_module_long) <- c("time","basis","value")
temporal_module_long$time <- rep(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4)),2)
temporal_module_long$dose <- c(rep("1st",28),rep("2nd",28))

spatial_module_long <- spatial_module %>% pivot_longer(-X1, names_to = "basis", values_to = "value")
colnames(spatial_module_long) <- c("symptoms","basis","value")
spatial_module_long$symptoms <- factor(spatial_module_long$symptoms,levels=spatial_module$X1)

chart1 <- ggplot(data=spatial_module_long,aes(x=symptoms,y=value,fill=symptoms)) + geom_bar(stat = "identity",colour="black") + scale_fill_manual(values=mycolors) + theme_bw(base_size=16) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(basis~., scales = "free")
ggsave("CUH_SmartDR_20210727_spatial_module.pdf",plot=chart1,width=6,height=6)

chart2 <- ggplot(data=temporal_module_long,aes(x=time,y=value)) + geom_line(colour="black",size=1) + geom_point(shape=21,stroke=1,fill="white",colour="black") + theme_bw(base_size=16) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits=c(0,NA)) + facet_grid(basis~dose, scales = "free")
ggsave("CUH_SmartDR_20210727_temporal_module.pdf",plot=chart2,width=4,height=5)

subject_score <- read_tsv("CUH_SmartDR_20210727_mask_patient_score_with_cluster_normalized.tsv")

colPal2 <- colorRampPalette(c("white", "red"))

subject_score2 <- data.frame(subject_score[,c("basis1","basis2","basis3","basis4")])
rownames(subject_score2) <- subject_score$CUH

subject_score2 <- apply(subject_score2,c(1,2),cutoff,5)

my_hclust <- hclust(dist(subject_score2), method = "complete")

cluster_df <- data.frame(hclust=factor(cutree(my_hclust, k = 3)),max_cluster=factor(apply(subject_score[,c("basis1","basis2","basis3","basis4")],1,which.max)))

p <- pheatmap(subject_score2, color = colPal2(10), clustering_method = "complete",annotation_row = cluster_df)
save_pheatmap_pdf(p,file="CUH_SmartDR_20210727_mask_patient_score.pdf")