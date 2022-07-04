library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lemon)
library(RColorBrewer)


rank = 4

nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


for (i in 1:rank){
	df <- read_tsv(paste0("CUH_SmartDR_20210727_basis_",i,".txt"))

	colnames(df) <- c("items",1,2,3,4,5,6,7,9,10,11,12,13,14,15)

	df_long <- df %>% pivot_longer(-items, names_to = "time", values_to = "value")
	df_long$time <- rep(c(1:7),24)
	df_long$dose <- rep(c(rep("1st",7),rep("2nd",7)),12)
	df_long$items <- factor(df_long$items,levels=df$items)

	df_long %>% ggplot( aes(y=value, x=time)) +
    geom_bar(aes(fill=items),alpha=0.8,stat = "identity",color="black") +
    facet_grid(items~dose) +
    scale_fill_manual(values=mycolors) +
    scale_y_continuous(breaks=c(0,1))+
    theme_bw(base_size=16) +
    theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
    	panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"))
    # theme(panel.border = element_blank(),
    #     panel.spacing = unit(1, "lines"),
    #     axis.line = element_line(color = "grey40", size = 0.3),
    #     axis.text.x = element_text(size = 8),
    #     axis.text.y = element_text(size = 8),
    #     axis.title = element_text(size = 8),
    #     strip.background = element_blank(),
    #     strip.text = element_text(size = 8, face = "bold"),
    #     axis.ticks = element_blank(),
    #     panel.grid.major = element_blank(), 
    #     panel.grid.minor = element_blank(), 
    #     legend.position = "none"
    # )
    ggsave(paste0("CUH_SmartDR_20210727_basis_",i,"_ridgeline.pdf"),width=8)

    df_long$items <- factor(df_long$items,levels=rev(df$items))

    df_long %>% ggplot( aes(y=items, x=factor(time))) +
    geom_tile(aes(fill=value)) +
    facet_grid(~dose) +
    scale_fill_gradient(low = "white",high = "Red") +
    theme_bw(base_size=16) +
    theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
        panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"))
    # theme(panel.border = element_blank(),
    #     panel.spacing = unit(1, "lines"),
    #     axis.line = element_line(color = "grey40", size = 0.3),
    #     axis.text.x = element_text(size = 8),
    #     axis.text.y = element_text(size = 8),
    #     axis.title = element_text(size = 8),
    #     strip.background = element_blank(),
    #     strip.text = element_text(size = 8, face = "bold"),
    #     axis.ticks = element_blank(),
    #     panel.grid.major = element_blank(), 
    #     panel.grid.minor = element_blank(), 
    #     legend.position = "none"
    # )
    ggsave(paste0("CUH_SmartDR_20210727_basis_",i,"_heatmap.pdf"),height=4,width=6)
}


time_list <- c("1_1","1_2","1_3","1_4","1_5","1_6","1_7","2_1","2_2","2_3","2_4","2_5","2_6","2_7")

col_list <- c()

for (t in time_list){
	col_list <- c(col_list,paste0(t,df$items))
}

subject_data <- read_excel("../../data/CUH_SmartDR_20210727.xlsx")
subject_data <- subject_data[,c("CUH",col_list)]
subject_data <- subject_data[rowSums(is.na(subject_data))<length(col_list)*0.3,]

subject_data_long <- subject_data %>% pivot_longer(-CUH, names_to = "title", values_to = "value")

temporal_module <- read_tsv("CUH_SmartDR_20210727_mask_temporal_module.txt")
spatial_module <- read_tsv("CUH_SmartDR_20210727_mask_spatial_module.txt")
subject_score <- read_tsv("CUH_SmartDR_20210727_mask_patient_score_with_cluster.tsv")

patient_list <- subject_data$CUH

mse <- c()
se_list <- c()
original_elements <- c()
reconstructed_elements <- c()

for(p in patient_list){

	subject_data_long_ind <- subject_data_long[subject_data_long$CUH==p,]
    subject_data_long_ind$time <- sort(rep(c(paste0("1_",c(1:7)),paste0("2_",c(1:7))),12))
	subject_data_long_ind$time2 <- c(rep(sort(rep(c(1:7),12)),2))
	subject_data_long_ind$dose <- c(rep("1st",84),rep("2nd",84))
	subject_data_long_ind$items <- factor(rep(df$items,14),levels=df$items)

	subject_data_long_ind %>% ggplot( aes(y=value, x=time2)) +
    geom_bar(aes(fill=items),alpha=0.8,stat = "identity",color="black") +
    facet_grid(items~dose) +
    scale_fill_manual(values=mycolors) +
    scale_y_continuous(breaks=c(0,1,2,3),limits=c(0,3))+
    theme_bw(base_size=16) +
    theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
    	panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"))
    # theme(panel.border = element_blank(),
    #     panel.spacing = unit(1, "lines"),
    #     axis.line = element_line(color = "grey40", size = 0.3),
    #     axis.text.x = element_text(size = 8),
    #     axis.text.y = element_text(size = 8),
    #     axis.title = element_text(size = 8),
    #     strip.background = element_blank(),
    #     strip.text = element_text(size = 8, face = "bold"),
    #     axis.ticks = element_blank(),
    #     panel.grid.major = element_blank(), 
    #     panel.grid.minor = element_blank(), 
    #     legend.position = "none"
    # )
    ggsave(paste0("./subject_timecourse/CUH_SmartDR_20210727_CUH_",p,"_ridgeline.pdf"),width=8)

    subject_data_long_ind$items <- factor(subject_data_long_ind$items,levels=rev(df$items))

    subject_data_long_ind %>% ggplot( aes(y=items, x=factor(time2))) +
    geom_tile(aes(fill=value)) +
    facet_grid(~dose) +
    scale_fill_gradient(low = "white",high = "Red",limits=c(0,3),na.value = "grey80") +
    theme_bw(base_size=16) +
    theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
        panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"))
    # theme(panel.border = element_blank(),
    #     panel.spacing = unit(1, "lines"),
    #     axis.line = element_line(color = "grey40", size = 0.3),
    #     axis.text.x = element_text(size = 8),
    #     axis.text.y = element_text(size = 8),
    #     axis.title = element_text(size = 8),
    #     strip.background = element_blank(),
    #     strip.text = element_text(size = 8, face = "bold"),
    #     axis.ticks = element_blank(),
    #     panel.grid.major = element_blank(), 
    #     panel.grid.minor = element_blank(), 
    #     legend.position = "none"
    # )
    ggsave(paste0("./subject_timecourse/CUH_SmartDR_20210727_CUH_",p,"_heatmap.pdf"),height=4,width=6)

    basis <- tibble(outer(pull(spatial_module,"0"),pull(temporal_module,"0"),FUN = "*"))
    subject_data_reconstructed <- basis * subject_score[subject_score$CUH==p,"basis1"][[1]]

    for (i in 2:rank){
        df2 <- read_tsv(paste0("CUH_SmartDR_20210727_basis_",i,".txt"))

        basis <- tibble(outer(pull(spatial_module,as.character(i-1)),pull(temporal_module,as.character(i-1)),FUN = "*"))

        subject_data_reconstructed <- subject_data_reconstructed + basis * subject_score[subject_score$CUH==p,paste0("basis",i)][[1]]
    }

    subject_data_reconstructed <- tibble(items=df2$X1,subject_data_reconstructed)

    colnames(subject_data_reconstructed) <- c("items",colnames(df2)[2:ncol(df2)])

    subject_data_ind <- subject_data_long_ind %>% select(c("items","time","value")) %>% pivot_wider(names_from = time, values_from = value)

    original_elements <- c(original_elements,as.vector(as.matrix(subject_data_ind[,c(2:15)])))
    reconstructed_elements <- c(reconstructed_elements,as.vector(as.matrix(subject_data_reconstructed[,c(2:15)])))

    se <- as.vector((as.matrix(subject_data_ind[,c(2:15)]) - as.matrix(subject_data_reconstructed[,c(2:15)]))^2)

    se_list <- c(se_list,se)

    mse <- c(mse,mean(se, na.rm=TRUE))
    
    subject_data_reconstructed_long <- subject_data_reconstructed %>% pivot_longer(-items, names_to = "title", values_to = "value")

    subject_data_reconstructed_long$time <- rep(c(1:7),24)
    subject_data_reconstructed_long$dose <- rep(c(rep("1st",7),rep("2nd",7)),12)
    subject_data_reconstructed_long$items <- factor(subject_data_reconstructed_long$items,levels=rev(df2$X1))

    subject_data_reconstructed_long %>% ggplot( aes(y=items, x=factor(time))) +
    geom_tile(aes(fill=value)) +
    facet_grid(~dose) +
    scale_fill_gradient(low = "white",high = "Green",limits=c(0,3),na.value="Green") +
    theme_bw(base_size=16) +
    theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
        panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"))
    # theme(panel.border = element_blank(),
    #     panel.spacing = unit(1, "lines"),
    #     axis.line = element_line(color = "grey40", size = 0.3),
    #     axis.text.x = element_text(size = 8),
    #     axis.text.y = element_text(size = 8),
    #     axis.title = element_text(size = 8),
    #     strip.background = element_blank(),
    #     strip.text = element_text(size = 8, face = "bold"),
    #     axis.ticks = element_blank(),
    #     panel.grid.major = element_blank(), 
    #     panel.grid.minor = element_blank(), 
    #     legend.position = "none"
    # )
    ggsave(paste0("./subject_timecourse/CUH_SmartDR_20210727_",p,"_reconstructed_heatmap.pdf"),height=4,width=6)

}

total_mse <- mean(se_list,na.rm=TRUE)

print(paste0("total MSE: ", total_mse ))
print(paste0("total R squared: ", cor(original_elements,reconstructed_elements, use = "complete.obs")^2))

mse_summary <- tibble(CUH=patient_list,MSE=mse)

write.table(mse_summary,file="CUH_SmartDR_20210727_tensor_reconstruction_MSE.txt",sep="\t",row.names=FALSE,quote=FALSE)

df_original <- read_excel("../../data/CUH_SmartDR_20210727.xlsx")

highlight_subjects <- c(15, 27, 79, 329, 521)

mse_summary <- mse_summary[order(mse_summary$MSE),]
df_merged2 <- left_join(mse_summary,df_original,by="CUH")
df_merged2$CUH <- factor(df_merged2$CUH,levels=df_merged2$CUH)
df_merged2$order <- c(1:nrow(df_merged2))
df_merged2$Sex <- factor(df_merged2$Sex,levels=c(1,0))

df_merged2 %>% ggplot(aes(y=MSE,x=order)) +
    geom_line(data=df_merged2) +
    # geom_point(data=df_merged2,size=0.5,shape=16)+
    geom_point(size=1,stroke=1,fill="white",shape=21,data=df_merged2[df_merged2$CUH %in% highlight_subjects,]) +
    scale_colour_brewer(palette="Set1")+
    theme_classic(base_size=7)
ggsave("CUH_SmartDR_20210727_tensor_reconstruction_MSE.pdf",height=1,width=10)

# mse_summary <- tibble(CUH=patient_list,MSE=mse)

res_summary <- c()

for (b in c("basis1","basis2","basis3","basis4")){
    res <- cor.test(pull(subject_score[,b]),pull(mse_summary[,"MSE"]),method="spearman")
    res_summary <- rbind(res_summary,c(b,res$p.value,res$estimate))
    print(res$estimate)
    print(res$p.value)
}

res_summary <- data.frame(res_summary)
colnames(res_summary) <- c("basis","pvalue","coef")
res_summary$FDR <- p.adjust(res_summary$pvalue)
write.table(res_summary,file="CUH_SmartDR_20210727_tensor_reconstruction_MSE_tensor_correlation.txt",sep="\t",quote=FALSE,row.names=FALSE)

for (i in 2:5){
  subject_score[i] <- subject_score[i] * sqrt(sum(temporal_module[i]**2)) * sqrt(sum(spatial_module[i]**2))
}

colnames(subject_score) <- c("CUH","basis2","basis1","basis3","basis4","cluster")

subject_score <- subject_score[,c("CUH","basis1","basis2","basis3","basis4","cluster")]

write.table(subject_score,"CUH_SmartDR_20210727_mask_patient_score_with_cluster_normalized.tsv",sep="\t",quote=F,row.names=F)

subject_score_long <- subject_score %>% select(c("CUH","basis1","basis2","basis3","basis4")) %>% pivot_longer(-CUH, names_to = "basis", values_to = "value")
subject_score_long$CUH <- factor(subject_score_long$CUH, levels=mse_summary$CUH)
subject_score_long <- subject_score_long[order(subject_score_long$CUH),]
subject_score_long$order <- sort(rep(c(1:nrow(mse_summary)),4))

subject_score_long %>% ggplot(aes(y=value,x=order)) +
    geom_line(data=subject_score_long,aes(colour=basis)) +
    facet_grid(basis~.,scale="free") +
    geom_point(aes(colour=basis),stroke=1,size=1,fill="white",shape=21,data=subject_score_long[subject_score_long$CUH %in% highlight_subjects,]) +
    theme_bw(base_size=7)
ggsave("CUH_SmartDR_20210727_tensor_basis_for_reconstruction_MSE.pdf",height=3,width=10)


df_merged2 <- left_join(mse_summary,df_original,by="CUH")

# exp_vars <- c(colnames(data.frame(df_original))[2:87],colnames(df_merged)[(ncol(df_merged)-10):ncol(df_merged)])
exp_vars <- colnames(df_original)[c(2,5,6,7,8,10:32,34:39)]
# for (v in exp_vars){
#   if (!(v %in% num_vars)){
#       df_merged[,v] <- factor(unlist(df_merged[,v]))
#   }
# }

age_vec <- as.vector(df_merged2$AgeCategory)

tmp_age <- c()
for (i in 1:nrow(df_merged2)){
    if (age_vec[i] == 70 || age_vec[i] == 60){
        tmp_age <- c(tmp_age,">60")
    } else {
        tmp_age <- c(tmp_age,age_vec[i])
    }
}

df_merged2$AgeCategory <- factor(tmp_age,levels=c("20","30","40","50",">60"))


res_summary <- c()

th <- 0.35

for (v in exp_vars){

    tmp_df <- df_merged2[,c(v,"MSE")]
    colnames(tmp_df) <- c("variable","MSE")
    tmp_df$variable <- factor(tmp_df$variable)

    if (length(levels(tmp_df$variable)) > 1){

        if (length(levels(tmp_df$variable)) > 2){

            res <- coin::kruskal_test(MSE~variable, data=tmp_df)

            res_summary <- rbind(res_summary, c(v, pvalue(res)))
        } else {

            res <- coin::wilcox_test(MSE~variable, data=tmp_df)

            res_summary <- rbind(res_summary,c(v, pvalue(res)))
        }
    }

}

