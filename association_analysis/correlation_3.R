library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
# library(coin)


# cols <-brewer.pal(9, "Set1")

cols <-rev(c(brewer.pal(11, "Spectral")[c(11,10,9,8)],"white",brewer.pal(11, "Spectral")[c(4,3,2,1)]))

df_original <- read_excel("../../data/CUH_SmartDR_20210727.xlsx")

temporal_module <- read_tsv("CUH_SmartDR_20210727_mask_temporal_module.txt")
spatial_module <- read_tsv("CUH_SmartDR_20210727_mask_spatial_module.txt")
df_score <- read_tsv("CUH_SmartDR_20210727_mask_patient_score_with_cluster.tsv")

for (i in 2:5){
  df_score[i] <- df_score[i] * sqrt(sum(temporal_module[i]**2)) * sqrt(sum(spatial_module[i]**2))
}

colnames(df_score) <- c("CUH","basis2","basis1","basis3","basis4","cluster")

df_merged <- left_join(df_score,df_original,by="CUH")

df_merged <- data.frame(df_merged %>% select(-"Job","Smoking","COVID19Contact"),fastDummies::dummy_cols(df_merged[,c("Job")]) %>% select(-"Job"),fastDummies::dummy_cols(df_merged[,"Smoking"],ignore_na = TRUE) %>% select(-"Smoking"),fastDummies::dummy_cols(df_merged[,"COVID19Contact"],ignore_na = TRUE) %>% select(-"COVID19Contact"))

# exp_vars <- c(colnames(data.frame(df_original))[2:87],colnames(df_merged)[(ncol(df_merged)-10):ncol(df_merged)])
exp_vars <- c(colnames(data.frame(df_original))[52:87])
# exp_vars <- exp_vars[-which(exp_vars %in% c("COPD","AgeCategory","AgeQuartile","IntervalToTest","Job","Smoking","COVID19Contact","AlcoholEveryday","PreTiter04"))]
# num_vars <- c("Age","BMICategory","Alcohol","ShotInterval","IntervalToTest","PreTiter")

# for (v in exp_vars){
# 	if (!(v %in% num_vars)){
# 		df_merged[,v] <- factor(unlist(df_merged[,v]))
# 	}
# }

basis <- c("basis1","basis2","basis3","basis4")

coef_summary <- c()
pval_summary <- c()

res_summary <- c()

for (v in exp_vars){
	print(v)
	for (i in basis){
		print(i)
		# if (!(v %in% num_vars)){
		# 	tmp_df <- na.omit(data.frame(basis=df_merged[,i],group=df_merged[,v]))
		# 	colnames(tmp_df) <- c("basis","group")
		# 	tmp_df$group <- factor(tmp_df$group)
		# 	res <- coin::wilcox_test(basis~group, data=tmp_df)
		# 	# pval <- c()
		# 	# for (j in 1:nrow(stat.test)){
		# 	# 	BMres <- brunner.munzel.test(tmp_df[tmp_df$group==stat.test[j,2][[1]],"basis"],tmp_df[tmp_df$group==stat.test[j,3][[1]],"basis"])
		# 	# 	pval <- c(pval,BMres$p.value)
		# 	# }
		# 	# stat.test$p <- pval
		# 	res_summary <- rbind(res_summary,c(v,i,statistic(res),pvalue(res)))

		# } else {
			tmp_df <- na.omit(data.frame(basis=df_merged[,i],value=df_merged[,v]))
			colnames(tmp_df) <- c("basis","value")
			res <- cor.test(tmp_df$basis,tmp_df$value,method="spearman")
			res_summary <- rbind(res_summary,c(v,i,res$estimate,res$p.value))
		# }
	}
}

res_summary <- data.frame(res_summary)
colnames(res_summary) <- c("variable","basis","cor","p")
res_summary$FDR <- p.adjust(res_summary$p)

res_summary$FDRlog <- -log10(as.numeric(as.character(res_summary$FDR)))
res_summary$cor <- as.numeric(as.character(res_summary$cor))
res_summary$FDRlog <- res_summary$FDRlog * ifelse(res_summary$cor > 0,1,-1)
res_summary$variable <- factor(res_summary$variable,levels=exp_vars)

sig <- c()

for (p in res_summary$FDRlog){
	if (is.na(p)){
		sig <- c(sig, NA)
	} else if (p > -log10(0.001)){
		sig <- c(sig, "P<+0.001")
	} else if ( p > -log10(0.005)){
		sig <- c(sig, "P<+0.005")
	} else if ( p > -log10(0.01)){
		sig <- c(sig, "P<+0.01")
	} else if ( p > -log10(0.05)){
		sig <- c(sig, "P<+0.05")
	} else if ( p < log10(0.001)){
		sig <- c(sig, "P<-0.001")
	} else if ( p < log10(0.005)){
		sig <- c(sig, "P<-0.005")
	} else if ( p < log10(0.01)){
		sig <- c(sig, "P<-0.01")
	} else if ( p < log10(0.05)){
		sig <- c(sig, "P<-0.05")
	} else {
		sig <- c(sig, "n.s.")
	}
}

res_summary$sig <- factor(sig, levels=rev(c("P<-0.001","P<-0.005","P<-0.01","P<-0.05","n.s.","P<+0.05","P<+0.01","P<+0.005","P<+0.001")))
res_summary$basis <- factor(res_summary$basis, levels=rev(basis))

res_summary %>% ggplot( aes(y=basis, x=variable)) +
    geom_tile(aes(fill=sig)) +
    scale_fill_manual(values=cols,drop=FALSE,na.value = "gray80") +
    theme_bw(base_size=16) +
    theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
        panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"),
        axis.text.x = element_text(angle = 90, hjust = 1))
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
    ggsave(paste0("CUH_SmartDR_20210727_patient_score_day0_correlation_summary.pdf"),height=6.5,width=10)

write.table(res_summary,file="CUH_SmartDR_20210727_patient_score_day0_correlation_summary.tsv",sep="\t",quote=F,row.names=F)

