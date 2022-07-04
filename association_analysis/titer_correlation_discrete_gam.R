library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(mgcv)
library(statmod)
# library(coin)


cols2 <-brewer.pal(9, "Set1")

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

# df_merged <- data.frame(df_merged %>% select(-"Job","Smoking","COVID19Contact"),fastDummies::dummy_cols(df_merged[,c("Job")]) %>% select(-"Job"),fastDummies::dummy_cols(df_merged[,"Smoking"],ignore_na = TRUE) %>% select(-"Smoking"),fastDummies::dummy_cols(df_merged[,"COVID19Contact"],ignore_na = TRUE) %>% select(-"COVID19Contact"))

df_merged$Smoking <- factor(df_merged$Smoking)
df_merged$Alcohol <- factor(df_merged$Alcohol)

# df_merged <- df_merged[df_merged$PreTiter == 0,]
# df_merged <- df_merged[df_merged$PostTiterLog2 > 6,]

df_merged$cluster <- factor(df_merged$cluster)

# df_merged$basis1_disc <- ifelse(df_merged$basis1 < 1E-2,0,1)
# df_merged$basis2_disc <- ifelse(df_merged$basis2 < 1E-2,0,1)
# df_merged$basis3_disc <- ifelse(df_merged$basis3 < 1E-2,0,1)
# df_merged$basis4_disc <- ifelse(df_merged$basis4 < 1E-2,0,1)
# df_merged$basis5_disc <- ifelse(df_merged$basis5 < 1E-2,0,1)
# df_merged$basis6_disc <- ifelse(df_merged$basis6 < 1E-2,0,1)
# df_merged$basis7_disc <- ifelse(df_merged$basis7 < 1E-2,0,1)

# exp_vars <- c(colnames(data.frame(df_original))[2:87],colnames(df_merged)[(ncol(df_merged)-10):ncol(df_merged)])
exp_vars <- c(colnames(data.frame(df_original))[52:87])
# exp_vars <- exp_vars[-which(exp_vars %in% c("COPD","AgeCategory","AgeQuartile","IntervalToTest","Job","Smoking","COVID19Contact","AlcoholEveryday","PreTiter04"))]
num_vars <- c("Age","BMICategory","Alcohol","ShotInterval","IntervalToTest","PreTiter")

basis <- c("basis1","basis2","basis3","basis4")

df_merged_m <- df_merged[df_merged$Sex=="0",]
df_merged_f <- df_merged[df_merged$Sex=="1",]

est <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + Sex + ShotInterval + Alcohol ,family=gaussian,data=df_merged)
est_basis_m <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol ,family=gaussian,data=df_merged_m)
est_basis_f <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol ,family=gaussian,data=df_merged_f)

est_basis1 <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + Sex + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis1 ,family=gaussian,data=df_merged)
est_basis2 <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + Sex + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis2 ,family=gaussian,data=df_merged)
est_basis3 <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + Sex + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis3 ,family=gaussian,data=df_merged)
est_basis4 <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + Sex + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis4 ,family=gaussian,data=df_merged)

est_basis1_m <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis1,family=gaussian,data=df_merged_m)
est_basis2_m <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis2 ,family=gaussian,data=df_merged_m)
est_basis3_m <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis3 ,family=gaussian,data=df_merged_m)
est_basis4_m <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis4 ,family=gaussian,data=df_merged_m)

est_basis1_f <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis1 ,family=gaussian,data=df_merged_f)
est_basis2_f <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis2 ,family=gaussian,data=df_merged_f)
est_basis3_f <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis3 ,family=gaussian,data=df_merged_f)
est_basis4_f <- gam( PostTiterLog2 ~ s(IntervalToTest) + s(Age) + ShotInterval + Alcohol + Immunosuppressant + COVID19Report + Glucocorticoid + AllergyDrug + basis4 ,family=gaussian,data=df_merged_f)
# est_cluster <- gam( PostTiterLog2 ~ s(IntervalToTest, by=Sex) + cluster,family=gaussian,data=df_merged)

res_summary <- c()

res_summary <- rbind(res_summary,
    c("all","basis1",summary(est_basis1)$p.table["basis1",c(4,1)]),
    c("all","basis2",summary(est_basis2)$p.table["basis2",c(4,1)]),
    c("all","basis3",summary(est_basis3)$p.table["basis3",c(4,1)]),
    c("all","basis4",summary(est_basis4)$p.table["basis4",c(4,1)])
)
res_summary <- rbind(res_summary,
    c("male","basis1",summary(est_basis1_m)$p.table["basis1",c(4,1)]),
    c("male","basis2",summary(est_basis2_m)$p.table["basis2",c(4,1)]),
    c("male","basis3",summary(est_basis3_m)$p.table["basis3",c(4,1)]),
    c("male","basis4",summary(est_basis4_m)$p.table["basis4",c(4,1)])
)
res_summary <- rbind(res_summary,
    c("female","basis1",summary(est_basis1_f)$p.table["basis1",c(4,1)]),
    c("female","basis2",summary(est_basis2_f)$p.table["basis2",c(4,1)]),
    c("female","basis3",summary(est_basis3_f)$p.table["basis3",c(4,1)]),
    c("female","basis4",summary(est_basis4_f)$p.table["basis4",c(4,1)])
)


pred_df1_m <- df_merged_m[,c("IntervalToTest","Age","ShotInterval","Alcohol",basis)]
pred_df1_f <- df_merged_f[,c("IntervalToTest","Age","ShotInterval","Alcohol",basis)]
pred_df2_m <- pred_df1_m
pred_df2_f <- pred_df1_f
pred_df2_m$IntervalToTest <- 21
pred_df2_f$IntervalToTest <- 21
pred_df2_m$Age <- 40
pred_df2_f$Age <- 40
pred_df2_m$ShotInterval <- 21
pred_df2_f$ShotInterval <- 21
# pred_df2_m$Smoking <- "0"
# pred_df2_f$Smoking <- "0"
pred_df2_m$Alcohol <- "0"
pred_df2_f$Alcohol <- "0"
pred_df2_m$Immunosuppressant <- 0
pred_df2_f$Immunosuppressant <- 0
pred_df2_m$COVID19Report <- 0
pred_df2_f$COVID19Report <- 0
pred_df2_m$Glucocorticoid <- 0
pred_df2_f$Glucocorticoid <- 0
pred_df2_m$AllergyDrug <- 0
pred_df2_f$AllergyDrug <- 0

df_merged_m$PostTiterLog2_adjusted <- df_merged_m$PostTiterLog2 + predict(est_basis_m,newdata=pred_df2_m) - predict(est_basis_m,newdata=pred_df1_m)
df_merged_f$PostTiterLog2_adjusted <- df_merged_f$PostTiterLog2 + predict(est_basis_f,newdata=pred_df2_f) - predict(est_basis_f,newdata=pred_df1_f)

df_merged <- tibble(rbind(df_merged_f,df_merged_m))

write.table(df_merged,"CUH_SmartDR_20210727_mask_patient_score_with_cluster_normalized_gam_adjusted.tsv")

df_merged_basis_longer <- df_merged[,c(basis,"Sex","PostTiterLog2_adjusted")] %>% pivot_longer(-c("Sex","PostTiterLog2_adjusted"),names_to = "basis", values_to = "value")
df_merged_basis_longer$Sex <- factor(df_merged_basis_longer$Sex)

p <- ggplot(aes(x=value, y=PostTiterLog2_adjusted),data=df_merged_basis_longer) +
    geom_point(aes(colour=Sex),shape=16,alpha=0.6,size=1) +
   	geom_smooth(aes(colour=Sex,fill=Sex), method = "lm")+
   	# coord_flip() +
   	facet_grid(.~basis,scales="free") +
    # scale_colour_distiller(palette = "Spectral") +
    scale_colour_manual(values = brewer.pal(3, "Set1")[c(2,1)]) + 
    scale_fill_manual(values = brewer.pal(3, "Set1")[c(2,1)]) +
    #scale_shape_manual(values=c(1,17))+
    #stat_ellipse(aes(x=MDS1,y=MDS2,fill=cluster),
    #              geom="polygon", level=0.95, alpha=0.2) +
    #scale_fill_manual(values=rev(cols[c(1,2)]))+
    # coord_fixed(ratio=1) +
    # scale_y_continuous(limits = c(-50, 50))+
    theme_bw(base_size = 16)
    ggsave(file = paste0("CUH_SmartDR_20210727_tensor_PostTiterLog2.pdf"), plot = p,width=10,height=3)

# for (b in basis){
# 	tmp_df <- na.omit(df_merged[,c(b,"AgeCategory","Sex","IntervalToTest","PostTiterLog2","Antiinflammatory", "PreTiter")])
# 	colnames(tmp_df) <- c("basis","Age","Sex","IntervalToTest","PostTiterLog2","Antiinflammatory", "PreTiter")
# 	tmp_df$basis_disc <- ifelse(tmp_df$basis < 1E-2,0,1)
# 	# tmp_df$IntervalToTest <- factor(tmp_df$IntervalToTest)
# 	# est2_raw <- gam( PostTiterLog2 ~ s(Age,by=Sex) + s(IntervalToTest) + ShotInterval + Antiinflammatory + PreTiter + basis,data=tmp_df)
# 	est2_raw <- glm( PostTiterLog2 ~ IntervalToTest + basis,family=gaussian,data=tmp_df)
# 	res_summary <- rbind(res_summary,c(b,"all",summary(est2_raw)$p.coeff["basis"],summary(est2_raw)$p.pv["basis"],summary(est2_raw)$p.coeff["basis_disc"],summary(est2_raw)$p.pv["basis_disc"]))

# 	for (s in c("0","1")){
# 		tmp_df_s <- tmp_df[tmp_df$Sex==s,]
# 		# est2_raw <- gam( PostTiterLog2 ~ s(Age) + s(IntervalToTest) + ShotInterval + Antiinflammatory + PreTiter + basis,data=tmp_df_s)
# 		est2_raw <- gam( PostTiterLog2 ~ s(Age) + s(IntervalToTest) + basis + basis_disc,data=tmp_df_s)
# 		res_summary <- rbind(res_summary,c(b,s,summary(est2_raw)$p.coeff["basis"],summary(est2_raw)$p.pv["basis"],summary(est2_raw)$p.coeff["basis_disc"],summary(est2_raw)$p.pv["basis_disc"]))
# 	}
# }

res_summary <- data.frame(res_summary)
colnames(res_summary) <- c("category","basis","pvalue","coef")
res_summary$FDR <- p.adjust(res_summary$pvalue)

write.table(res_summary,"CUH_SmartDR_20210727_tensor_titer_correlation_gam.txt",sep="\t",quote=FALSE,row.names=FALSE)

res_summary$p <- -log10(res_summary$pvalue) * ifelse(res_summary$coef > 0,1,-1)

sig <- c()

for (p in res_summary$p){
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
res_summary$category <- factor(res_summary$category,levels=c("all","male","female"))

res_summary %>% ggplot( aes(y=basis, x=category)) +
    geom_tile(aes(fill=sig)) +
    scale_fill_manual(values=cols,drop=FALSE,na.value = "gray80") +
    theme_bw(base_size=16) +
    theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
        panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"),
        axis.text.x = element_text(angle = 90, hjust = 1))+
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
    ggsave(paste0("CUH_SmartDR_20210727_tensor_titer_correlation_gam.pdf"),height=4,width=4)


