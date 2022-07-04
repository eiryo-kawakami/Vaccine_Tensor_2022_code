library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(rstatix)
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

df_merged <- tibble(df_merged %>% select(-c("Job","COVID19Contact")),fastDummies::dummy_cols(df_merged[,"Smoking"],ignore_na = TRUE) %>% select(-"Smoking"),fastDummies::dummy_cols(df_merged[,"Alcohol"],ignore_na = TRUE) %>% select(-"Alcohol"),fastDummies::dummy_cols(df_merged[,c("Job")]) %>% select(-"Job"),fastDummies::dummy_cols(df_merged[,"COVID19Contact"],ignore_na = TRUE) %>% select(-"COVID19Contact"))

# exp_vars <- c(colnames(data.frame(df_original))[2:87],colnames(df_merged)[(ncol(df_merged)-10):ncol(df_merged)])
exp_vars <- c(colnames(data.frame(df_merged))[c(11,15:43,47)],"Antiinflammatory",colnames(df_merged)[(ncol(df_merged)-14):ncol(df_merged)])
exp_vars <- exp_vars[-which(exp_vars %in% c("COPD","AgeCategory","AgeQuartile","IntervalToTest","Job","COVID19Contact","AlcoholEveryday","PreTiter04","Nurse","Antiinflammatory","Job_5"))]
num_vars <- c("Age","BMICategory","ShotInterval","IntervalToTest","PreTiter")

basis <- c("basis1","basis2","basis3","basis4")

df_basis <- df_merged[,c("CUH","Sex","Age",exp_vars,basis)]

write.table(df_basis,file="CUH_SmartDR_20210727_prevars_basis.txt",sep="\t",quote=FALSE,row.names=FALSE)

exp_vars2 <- c(exp_vars,"IntervalToTest","PostTiterLog2")

df_titer <- df_merged[,c("CUH","Sex","Age",exp_vars2,basis)]

write.table(df_titer,file="CUH_SmartDR_20210727_mask_posttiter_basis.txt",sep="\t",quote=FALSE,row.names=FALSE)

# for (v in exp_vars){
# 	if (!(v %in% num_vars)){
# 		df_merged[,v] <- factor(unlist(df_merged[,v]))
# 	}
# }

age_vec <- as.vector(df_merged$AgeCategory)

tmp_age <- c()
for (i in 1:nrow(df_merged)){
	if (age_vec[i] == 70 || age_vec[i] == 60){
		tmp_age <- c(tmp_age,">60")
	} else {
		tmp_age <- c(tmp_age,age_vec[i])
	}
}

df_merged$AgeCategory <- factor(tmp_age,levels=c("20","30","40","50",">60"))

df_merged <- tibble(df_merged, fastDummies::dummy_cols(df_merged[,"AgeCategory"],ignore_na = TRUE) %>% select(-"AgeCategory"))

# ###年齢と性別についての比較

# res_summary <- c()
# fisher_res <- c()

# th <- 0.3

# for (b in basis){
# 	tmp_df <- na.omit(df_merged[,c(b,"Sex","Age","AgeCategory")])
# 	colnames(tmp_df) <- c("basis","Sex","Age","AgeCategory")
# 	tmp_df <- tibble(tmp_df %>% select(-"AgeCategory"),fastDummies::dummy_cols(tmp_df[,"AgeCategory"]))
# 	tmp_df$Sex <- factor(tmp_df$Sex)
# 	tmp_df_f <- tmp_df[tmp_df$Sex=="1",]
# 	tmp_df_m <- tmp_df[tmp_df$Sex=="0",]

# 	tmp_df_f_positive <- tmp_df_f[tmp_df_f$basis > th,]
# 	tmp_df_f_negative <- tmp_df_f[tmp_df_f$basis <= th,]
# 	tmp_df_m_positive <- tmp_df_m[tmp_df_m$basis > th,]
# 	tmp_df_m_negative <- tmp_df_m[tmp_df_m$basis <= th,]

# 	ct <- c()

# 	for (a in c("20","30","40","50",">60")){

# 		ct <- c(ct, nrow(tmp_df_f_positive[tmp_df_f_positive$AgeCategory==a,]))
# 		ct <- c(ct, nrow(tmp_df_m_positive[tmp_df_m_positive$AgeCategory==a,]))

# 	}

# 	for (a in c("20","30","40","50",">60")){

# 		ct <- c(ct, nrow(tmp_df_f_negative[tmp_df_f_negative$AgeCategory==a,]))
# 		ct <- c(ct, nrow(tmp_df_m_negative[tmp_df_m_negative$AgeCategory==a,]))

# 	}

# 	mx <- matrix(ct, nrow=2, byrow=T)

# 	res <- fisher.test(mx, simulate.p.value = TRUE)

# 	fisher_res <- rbind(fisher_res,c(b,res$p.value))

# 	# age_sex <- c()

# 	# for (i in 1:nrow(tmp_df)){
# 	# 	age_sex <- c(age_sex,paste0(tmp_df$AgeCategory[i],"_",tmp_df$Sex[i]))
# 	# }

# 	# tmp_df$age_sex <- age_sex

# 	# tmp_df <- tibble(tmp_df %>% select(-"age_sex"),fastDummies::dummy_cols(tmp_df[,"age_sex"]))

# 	# res_sex_20_m <- glm(basis ~  age_sex_20_0,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_30_m <- glm(basis ~  age_sex_30_0,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_40_m <- glm(basis ~  age_sex_40_0,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_50_m <- glm(basis ~  age_sex_50_0,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_60_m <- glm(basis ~  `age_sex_>60_0`,family=tweedie(var.power=0,link.power=0),data=tmp_df)

# 	# res_sex_20_f <- glm(basis ~  age_sex_20_1,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_30_f <- glm(basis ~  age_sex_30_1,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_40_f <- glm(basis ~  age_sex_40_1,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_50_f <- glm(basis ~  age_sex_50_1,family=tweedie(var.power=0,link.power=0),data=tmp_df)
# 	# res_sex_60_f <- glm(basis ~  `age_sex_>60_1`,family=tweedie(var.power=0,link.power=0),data=tmp_df)

# 	# res_summary <- rbind(res_summary,c(b,"20_male",summary(res_sex_20_m)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"30_male",summary(res_sex_30_m)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"40_male",summary(res_sex_40_m)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"50_male",summary(res_sex_50_m)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,">60_male",summary(res_sex_60_m)$coefficients[2,c(4,1)]))

# 	# res_summary <- rbind(res_summary,c(b,"20_female",summary(res_sex_20_f)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"30_female",summary(res_sex_30_f)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"40_female",summary(res_sex_40_f)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"50_female",summary(res_sex_50_f)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,">60_female",summary(res_sex_60_f)$coefficients[2,c(4,1)]))

# 	tmp_df_20 <- tmp_df[tmp_df$AgeCategory=="20",]
# 	tmp_df_30 <- tmp_df[tmp_df$AgeCategory=="30",]
# 	tmp_df_40 <- tmp_df[tmp_df$AgeCategory=="40",]
# 	tmp_df_50 <- tmp_df[tmp_df$AgeCategory=="50",]
# 	tmp_df_60 <- tmp_df[tmp_df$AgeCategory==">60",]

# 	res_sex <- glm(basis ~  Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df)

# 	res_sex_20 <- glm(basis ~  Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df_20)
# 	res_sex_30 <- glm(basis ~  Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df_30)
# 	res_sex_40 <- glm(basis ~  Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df_40)
# 	res_sex_50 <- glm(basis ~  Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df_50)
# 	res_sex_60 <- glm(basis ~  Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df_60)

# 	# res_age_m <- glm(basis ~ AgeCategory,family=tweedie(var.power=0,link.power=1),data=tmp_df_m,start=rep(0,5))
# 	# res_age_f <- glm(basis ~ AgeCategory,family=tweedie(var.power=0,link.power=1),data=tmp_df_f,start=rep(0,5))

# 	res_age20 <- glm(basis ~ AgeCategory_20 + Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df,start=rep(0,3))
# 	res_age30 <- glm(basis ~ AgeCategory_30 + Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df,start=rep(0,3))
# 	res_age40 <- glm(basis ~ AgeCategory_40 + Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df,start=rep(0,3))
# 	res_age50 <- glm(basis ~ AgeCategory_50 + Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df,start=rep(0,3))
# 	res_age60 <- glm(basis ~ `AgeCategory_>60` + Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df,start=rep(0,3))

# 	res_age20_m <- glm(basis ~ AgeCategory_20,family=tweedie(var.power=0,link.power=0),data=tmp_df_m,start=rep(0,2))
# 	res_age30_m <- glm(basis ~ AgeCategory_30,family=tweedie(var.power=0,link.power=0),data=tmp_df_m,start=rep(0,2))
# 	res_age40_m <- glm(basis ~ AgeCategory_40,family=tweedie(var.power=0,link.power=0),data=tmp_df_m,start=rep(0,2))
# 	res_age50_m <- glm(basis ~ AgeCategory_50,family=tweedie(var.power=0,link.power=0),data=tmp_df_m,start=rep(0,2))
# 	res_age60_m <- glm(basis ~ `AgeCategory_>60`,family=tweedie(var.power=0,link.power=0),data=tmp_df_m,start=rep(0,2))

# 	res_age20_f <- glm(basis ~ AgeCategory_20,family=tweedie(var.power=0,link.power=0),data=tmp_df_f,start=rep(0,2))
# 	res_age30_f <- glm(basis ~ AgeCategory_30,family=tweedie(var.power=0,link.power=0),data=tmp_df_f,start=rep(0,2))
# 	res_age40_f <- glm(basis ~ AgeCategory_40,family=tweedie(var.power=0,link.power=0),data=tmp_df_f,start=rep(0,2))
# 	res_age50_f <- glm(basis ~ AgeCategory_50,family=tweedie(var.power=0,link.power=0),data=tmp_df_f,start=rep(0,2))
# 	res_age60_f <- glm(basis ~ `AgeCategory_>60`,family=tweedie(var.power=0,link.power=0),data=tmp_df_f,start=rep(0,2))

# 	res_summary <- rbind(res_summary,c(b,"Sex_all",summary(res_sex)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Sex_20",summary(res_sex_20)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Sex_30",summary(res_sex_30)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Sex_40",summary(res_sex_40)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Sex_50",summary(res_sex_50)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Sex_>60",summary(res_sex_60)$coefficients[2,c(4,1)]))

# 	colnames(res_summary) <- c("basis","category","pvalue","coef")

# 	# res_summary <- rbind(res_summary,c(b,"Age_20_all",summary(res_age20)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"Age_30_all",summary(res_age30)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"Age_40_all",summary(res_age40)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"Age_50_all",summary(res_age50)$coefficients[2,c(4,1)]))
# 	# res_summary <- rbind(res_summary,c(b,"Age_>60_all",summary(res_age60)$coefficients[2,c(4,1)]))

# 	res_summary <- rbind(res_summary,c(b,"Age_20_male",summary(res_age20_m)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_30_male",summary(res_age30_m)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_40_male",summary(res_age40_m)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_50_male",summary(res_age50_m)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_>60_male",summary(res_age60_m)$coefficients[2,c(4,1)]))

# 	res_summary <- rbind(res_summary,c(b,"Age_20_female",summary(res_age20_f)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_30_female",summary(res_age30_f)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_40_female",summary(res_age40_f)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_50_female",summary(res_age50_f)$coefficients[2,c(4,1)]))
# 	res_summary <- rbind(res_summary,c(b,"Age_>60_female",summary(res_age60_f)$coefficients[2,c(4,1)]))
	
# 	# res_summary <- rbind(res_summary,data.frame(basis=b,category=rownames(summary(res_age_m)$coefficients)[2:5],pvalue=summary(res_age_m)$coefficients[c(2:5),4],coef=summary(res_age_m)$coefficients[c(2:5),1]))
# 	# res_summary <- rbind(res_summary,data.frame(basis=b,category=rownames(summary(res_age_f)$coefficients)[2:5],pvalue=summary(res_age_f)$coefficients[c(2:5),4],coef=summary(res_age_f)$coefficients[c(2:5),1]))

# 	# pred_df <- data.frame(Age = seq(min(tmp_df$Age), max(tmp_df$Age), length = 100))
# 	# pred_m <- predict(res_age_m, newdata = pred_df, type = 'link', se.fit = TRUE)
# 	# pred_m <- data.frame(pred_df,as.data.frame(pred_m))
# 	# pred_m <- transform(pred_m,
# 	# 	fitted_response = exp(fit),
# 	# 	fitted_upper = exp(fit + 1.96 * se.fit),
# 	# 	fitted_lower = exp(fit - 1.96 * se.fit),
# 	# 	Sex = "0"
# 	# 	)
# 	# pred_f <- predict(res_age_f, newdata = pred_df, type = 'link', se.fit = TRUE)
# 	# pred_f <- data.frame(pred_df,as.data.frame(pred_f))
# 	# pred_f <- transform(pred_f,
# 	# 	fitted_response = exp(fit),
# 	# 	fitted_upper = exp(fit + 1.96 * se.fit),
# 	# 	fitted_lower = exp(fit - 1.96 * se.fit),
# 	# 	Sex = "1"
# 	# 	)

# 	# pred_merged <- rbind(pred_m,pred_f)

# 		p <- ggplot(aes(x=AgeCategory, y=basis,fill=Sex, color=Sex), data=tmp_df) +
# 		    geom_violin() +
# 		    scale_fill_manual(values=cols2[c(2,1)])+
# 		    scale_colour_manual(values=cols2[c(2,1)])+
# 		    theme_classic(base_size = 16)+
# 		    theme(aspect.ratio=0.4)
# 	    	ggsave(file = paste0("CUH_SmartDR_20210727_tensor_",b,"_Age_Sex_correlation.pdf"), plot = p,width=5,height=5)

# }


# colnames(res_summary) <- c("basis","category","pvalue","coef")
# res_summary <- data.frame(res_summary)
# res_summary$pvalue <- as.numeric(as.character(res_summary$pvalue))


res_summary <- c()

for (b in basis){
	tmp_df <- na.omit(df_merged[,c(b,"AgeCategory","Sex")])
	colnames(tmp_df) <- c("basis","AgeCategory","Sex")

	res <- glm(basis ~  AgeCategory + Sex,family=tweedie(var.power=0,link.power=0),data=tmp_df)
	res_summary <- rbind(res_summary,data.frame(basis=b,category=rownames(summary(res)$coefficients)[c(2:6)],summary(res)$coefficients[c(2:6),c(4,1)]))
}

colnames(res_summary) <- c("basis","category","pvalue","coef")
res_summary <- data.frame(res_summary)
res_summary$FDR <- p.adjust(res_summary$pvalue)

write.table(res_summary,file="CUH_SmartDR_20210727_mask_tensor_Age_Sex_correlation.txt",sep="\t",row.names=FALSE,quote=FALSE)

df_merged_basis_longer <- df_merged[,c(basis,"Sex","Age","AgeCategory")] %>% pivot_longer(-c("Sex","Age","AgeCategory"),names_to = "basis", values_to = "value")
df_merged_basis_longer$Sex <- factor(df_merged_basis_longer$Sex)
# df_merged_basis_longer$basis <- factor(df_merged_basis_longer$basis,levels=c("basis1","basis4","basis2","basis3"))

p <- ggplot(aes(x=AgeCategory, y=value, color=Sex), data=df_merged_basis_longer) +
    geom_boxplot() +
    facet_grid(basis~.,scales="free") +
    scale_fill_manual(values=cols2[c(2,1)])+
    scale_colour_manual(values=cols2[c(2,1)])+
    theme_bw(base_size = 16)
    # theme(aspect.ratio=0.4)
	ggsave(file = paste0("CUH_SmartDR_20210727_mask_tensor_Age_Sex_correlation.pdf"), plot = p,width=5,height=7.5)

# p <- ggplot(aes(x=Age, y=value, color=Sex), data=df_merged_basis_longer) +
#     geom_boxplot() +
#     facet_grid(basis~.,scales="free") +
#     scale_fill_manual(values=cols2[c(2,1)])+
#     scale_colour_manual(values=cols2[c(2,1)])+
#     theme_bw(base_size = 16)
#     # theme(aspect.ratio=0.4)
# 	ggsave(file = paste0("CUH_SmartDR_20210727_mask_tensor_Age_Sex_correlation.pdf"), plot = p,width=5,height=7.5)

df_merged_basis_longer <- df_merged[,c(basis,"Antiinflammatory")] %>% pivot_longer(-c("Antiinflammatory"),names_to = "basis", values_to = "value")
df_merged_basis_longer$Antiinflammatory <- factor(df_merged_basis_longer$Antiinflammatory)
df_merged_basis_longer <- na.omit(df_merged_basis_longer)

p <- ggplot(aes(x=Antiinflammatory, y=value, color=basis), data=df_merged_basis_longer) +
    geom_boxplot() +
    # facet_grid(basis~.,scales="free") +
    # scale_fill_manual(values=cols2[c(2,1)])+
    # scale_colour_manual(values=cols2[c(2,1)])+
    theme_bw(base_size = 16)
    # theme(aspect.ratio=0.4)
	ggsave(file = paste0("CUH_SmartDR_20210727_mask_tensor_Antiinflammatory_correlation.pdf"), plot = p,width=5.5,height=3)

res_summary <- c()

for (b in basis){
	tmp_df <- na.omit(df_merged[,c(b,"Antiinflammatory")])
	colnames(tmp_df) <- c("basis","Antiinflammatory")

	for (i in c(1:3)){
		tmp_df2 <- tmp_df[tmp_df$Antiinflammatory %in% c(0,i),]
		tmp_df2$Antiinflammatory <- factor(tmp_df2$Antiinflammatory)
		res <- glm(basis ~  Antiinflammatory,family=tweedie(var.power=0,link.power=0),data=tmp_df2)
		res_summary <- rbind(res_summary,c(b,i,summary(res)$coefficients[2,c(4,1)]))
	}
}


## SmokingとAlcoholについての比較


res_summary <- c()

for (b in basis){
	tmp_df <- na.omit(df_merged[,c(b,"Smoking","Alcohol")])
	colnames(tmp_df) <- c("basis","Smoking","Alcohol")

	tmp_df$Smoking <- factor(tmp_df$Smoking)
	tmp_df$Alcohol <- factor(tmp_df$Alcohol)

	smoking_alcohol <- c()

	for (i in 1:nrow(tmp_df)){
		if (tmp_df$Smoking[i] == 2){
			tmp_smoke <- 1
		} else {
			tmp_smoke <- 0
		}
		if (tmp_df$Alcohol[i] == 2){
			tmp_alcohol <- 1
		} else {
			tmp_alcohol <- 0
		}
		smoking_alcohol <- c(smoking_alcohol,paste0("Smoking_",tmp_smoke,"_Alcohol_",tmp_alcohol))
	}

	# for (i in 1:nrow(tmp_df)){
	# 		tmp_smoke <- tmp_df$Smoking[i]
	# 		tmp_alcohol <- tmp_df$Alcohol[i]
	# 	smoking_alcohol <- c(smoking_alcohol,paste0("Smoking_",tmp_smoke,"_Alcohol_",tmp_alcohol))
	# }

	tmp_df$smoking_alcohol <- factor(smoking_alcohol,levels=c("Smoking_0_Alcohol_0","Smoking_1_Alcohol_0","Smoking_0_Alcohol_1","Smoking_1_Alcohol_1"))

	# tmp_df$smoking_alcohol <- factor(smoking_alcohol,levels=c("Smoking_0_Alcohol_0","Smoking_1_Alcohol_0","Smoking_2_Alcohol_0","Smoking_0_Alcohol_1","Smoking_1_Alcohol_1","Smoking_2_Alcohol_1","Smoking_0_Alcohol_2","Smoking_1_Alcohol_2","Smoking_2_Alcohol_2"))

	res <- glm(basis ~  smoking_alcohol,family=tweedie(var.power=0,link.power=0),data=tmp_df)
	res_summary <- rbind(res_summary,data.frame(basis=b,category=c("Smoking_1_Alcohol_0","Smoking_0_Alcohol_1","Smoking_1_Alcohol_1"),summary(res)$coefficients[c(2:4),c(4,1)]))

	# res <- glm(basis ~  Smoking,family=tweedie(var.power=0,link.power=0),data=tmp_df)

	# res_summary <- rbind(res_summary,data.frame(basis=b,category=c("Smoking_1","Smoking_2"),summary(res)$coefficients[c(2:3),c(4,1)]))

	# res <- glm(basis ~  Alcohol,family=tweedie(var.power=0,link.power=0),data=tmp_df)

	# res_summary <- rbind(res_summary,data.frame(basis=b,category=c("Alcohol_1","Alcohol_2"),summary(res)$coefficients[c(2:3),c(4,1)]))
	# # colnames(res_summary) <- c("basis","category","pvalue","coef")

}


colnames(res_summary) <- c("basis","category","pvalue","coef")
res_summary <- data.frame(res_summary)
res_summary$basis <- factor(res_summary$basis,levels=c("basis1","basis4","basis2","basis3"))
res_summary$pvalue <- as.numeric(as.character(res_summary$pvalue))


df_smoking_alcohol <- na.omit(df_merged[,c(basis,"Smoking","Alcohol")])

smoking_alcohol <- c()

	for (i in 1:nrow(df_smoking_alcohol)){
		if (df_smoking_alcohol$Smoking[i] == 0){
			tmp_smoke <- 0
		} else {
			tmp_smoke <- 1
		}
		if (df_smoking_alcohol$Alcohol[i] == 0){
			tmp_alcohol <- 0
		} else {
			tmp_alcohol <- 1
		}
		smoking_alcohol <- c(smoking_alcohol,paste0("Smoking_",tmp_smoke,"_Alcohol_",tmp_alcohol))
	}

df_smoking_alcohol$smoking_alcohol <- factor(smoking_alcohol,levels=c("Smoking_0_Alcohol_0","Smoking_1_Alcohol_0","Smoking_0_Alcohol_1","Smoking_1_Alcohol_1"))

df_smoking_alcohol_basis_longer <- df_smoking_alcohol %>% pivot_longer(-c("smoking_alcohol","Smoking","Alcohol"),names_to = "basis", values_to = "value")

p <- ggplot(aes(x=smoking_alcohol, y=value), data=df_smoking_alcohol_basis_longer) +
    geom_boxplot() +
    facet_grid(basis~.,scales="free") +
    # scale_fill_manual(values=cols2[c(2,1)])+
    # scale_colour_manual(values=cols2[c(2,1)])+
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
	ggsave(file = paste0("CUH_SmartDR_20210727_mask_tensor_smoking_alcohol_correlation.pdf"), plot = p,width=5,height=10)


exp_vars_disease <- exp_vars[2:12]

coef_summary <- c()
pval_summary <- c()

res_summary_disease <- c()

for (v in exp_vars_disease){
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
			# tmp_df$basis_disc <- ifelse(tmp_df$basis < 1E-2,0,1)
			# tmp_df_0 <- tmp_df[tmp_df$value=="0",]
			# tmp_df_1 <- tmp_df[tmp_df$value=="1",]
			# # res <- ziw(as.vector(tmp_df[tmp_df$value=="0",]$basis),as.vector(tmp_df[tmp_df$value=="1",]$basis),perm=FALSE)
			# mx <- matrix(c(
			# 	nrow(tmp_df_0[tmp_df_0$basis_disc==0,]),
			# 	nrow(tmp_df_0[tmp_df_0$basis_disc==1,]),
			# 	nrow(tmp_df_1[tmp_df_1$basis_disc==0,]),
			# 	nrow(tmp_df_1[tmp_df_1$basis_disc==1,])
			# 	), nrow=2, byrow=T)
			# print(mx)
			# res <- fisher.test(mx)
			# res_summary <- rbind(res_summary,c(v,i,res$p.value,res$estimate))
			res <- glm(basis ~  value, family=tweedie(var.power=0,link.power=0),data=tmp_df)
			res_summary_disease <- rbind(res_summary_disease,c(v,i,summary(res)$coefficients[2,c(4,1)]))
		# }
	}
}

res_summary_disease <- data.frame(res_summary_disease)
colnames(res_summary_disease) <- c("variable","basis","p","coef")
res_summary_disease$basis <- factor(res_summary_disease$basis,levels=c("basis1","basis4","basis2","basis3"))
res_summary_disease$FDR <- p.adjust(res_summary_disease$p)

res_summary_disease$FDRlog <- -log10(res_summary_disease$FDR)
res_summary_disease$coef <- as.numeric(as.character(res_summary_disease$coef))
res_summary_disease$FDRlog <- res_summary_disease$FDRlog * ifelse(res_summary_disease$coef > 0,1,-1)
res_summary_disease$variable <- factor(res_summary_disease$variable,levels=exp_vars)

sig <- c()

for (p in res_summary_disease$FDRlog){
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

res_summary_disease$sig <- factor(sig, levels=rev(c("P<-0.001","P<-0.005","P<-0.01","P<-0.05","n.s.","P<+0.05","P<+0.01","P<+0.005","P<+0.001")))
res_summary_disease$basis <- factor(res_summary_disease$basis, levels=rev(basis))

res_summary_disease %>% ggplot( aes(y=basis, x=variable)) +
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
    ggsave(paste0("CUH_SmartDR_20210727_mask_patient_score_backgroundfactor_disease_correlation_summary.pdf"),height=4,width=10)

write.table(res_summary_disease,file="CUH_SmartDR_20210727_mask_patient_score_backgroundfactor_disease_correlation_summary.tsv",sep="\t",quote=F,row.names=F)



exp_vars_drug <- exp_vars[13:22]

coef_summary <- c()
pval_summary <- c()

res_summary_drug <- c()

for (v in exp_vars_drug){
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
			# tmp_df$basis_disc <- ifelse(tmp_df$basis < 1E-2,0,1)
			# tmp_df_0 <- tmp_df[tmp_df$value=="0",]
			# tmp_df_1 <- tmp_df[tmp_df$value=="1",]
			# # res <- ziw(as.vector(tmp_df[tmp_df$value=="0",]$basis),as.vector(tmp_df[tmp_df$value=="1",]$basis),perm=FALSE)
			# mx <- matrix(c(
			# 	nrow(tmp_df_0[tmp_df_0$basis_disc==0,]),
			# 	nrow(tmp_df_0[tmp_df_0$basis_disc==1,]),
			# 	nrow(tmp_df_1[tmp_df_1$basis_disc==0,]),
			# 	nrow(tmp_df_1[tmp_df_1$basis_disc==1,])
			# 	), nrow=2, byrow=T)
			# print(mx)
			# res <- fisher.test(mx)
			# res_summary <- rbind(res_summary,c(v,i,res$p.value,res$estimate))
			res <- glm(basis ~  value, family=tweedie(var.power=0,link.power=0),data=tmp_df)
			res_summary_drug <- rbind(res_summary_drug,c(v,i,summary(res)$coefficients[2,c(4,1)]))
		# }
	}
}

res_summary_drug <- data.frame(res_summary_drug)
colnames(res_summary_drug) <- c("variable","basis","p","coef")
res_summary_drug$basis <- factor(res_summary_drug$basis,levels=c("basis1","basis4","basis2","basis3"))
res_summary_drug$FDR <- p.adjust(res_summary_drug$p)

res_summary_drug$FDRlog <- -log10(res_summary_drug$FDR)
res_summary_drug$coef <- as.numeric(as.character(res_summary_drug$coef))
res_summary_drug$FDRlog <- res_summary_drug$FDRlog * ifelse(res_summary_drug$coef > 0,1,-1)
res_summary_drug$variable <- factor(res_summary_drug$variable,levels=exp_vars)

sig <- c()

for (p in res_summary_drug$FDRlog){
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

res_summary_drug$sig <- factor(sig, levels=rev(c("P<-0.001","P<-0.005","P<-0.01","P<-0.05","n.s.","P<+0.05","P<+0.01","P<+0.005","P<+0.001")))
res_summary_drug$basis <- factor(res_summary_drug$basis, levels=rev(basis))

res_summary_drug %>% ggplot( aes(y=basis, x=variable)) +
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
    ggsave(paste0("CUH_SmartDR_20210727_mask_patient_score_backgroundfactor_drug_correlation_summary.pdf"),height=4,width=10)

write.table(res_summary_drug,file="CUH_SmartDR_20210727_mask_patient_score_backgroundfactor_drug_correlation_summary.tsv",sep="\t",quote=F,row.names=F)


exp_vars_others <- exp_vars[c(25,26,30:42)]

coef_summary <- c()
pval_summary <- c()

res_summary_others <- c()

for (v in exp_vars_others){
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
			# tmp_df$basis_disc <- ifelse(tmp_df$basis < 1E-2,0,1)
			# tmp_df_0 <- tmp_df[tmp_df$value=="0",]
			# tmp_df_1 <- tmp_df[tmp_df$value=="1",]
			# # res <- ziw(as.vector(tmp_df[tmp_df$value=="0",]$basis),as.vector(tmp_df[tmp_df$value=="1",]$basis),perm=FALSE)
			# mx <- matrix(c(
			# 	nrow(tmp_df_0[tmp_df_0$basis_disc==0,]),
			# 	nrow(tmp_df_0[tmp_df_0$basis_disc==1,]),
			# 	nrow(tmp_df_1[tmp_df_1$basis_disc==0,]),
			# 	nrow(tmp_df_1[tmp_df_1$basis_disc==1,])
			# 	), nrow=2, byrow=T)
			# print(mx)
			# res <- fisher.test(mx)
			# res_summary <- rbind(res_summary,c(v,i,res$p.value,res$estimate))
			res <- glm(basis ~  value, family=tweedie(var.power=0,link.power=0),data=tmp_df)
			res_summary_others <- rbind(res_summary_others,c(v,i,summary(res)$coefficients[2,c(4,1)]))
		# }
	}
}

res_summary_others <- data.frame(res_summary_others)
colnames(res_summary_others) <- c("variable","basis","p","coef")
res_summary_others$basis <- factor(res_summary_others$basis,levels=c("basis1","basis4","basis2","basis3"))
res_summary_others$FDR <- p.adjust(res_summary_others$p)

res_summary_others$FDRlog <- -log10(res_summary_others$FDR)
res_summary_others$coef <- as.numeric(as.character(res_summary_others$coef))
res_summary_others$FDRlog <- res_summary_others$FDRlog * ifelse(res_summary_others$coef > 0,1,-1)
res_summary_others$variable <- factor(res_summary_others$variable,levels=exp_vars)

sig <- c()

for (p in res_summary_others$FDRlog){
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

res_summary_others$sig <- factor(sig, levels=rev(c("P<-0.001","P<-0.005","P<-0.01","P<-0.05","n.s.","P<+0.05","P<+0.01","P<+0.005","P<+0.001")))
res_summary_others$basis <- factor(res_summary_others$basis, levels=rev(basis))

res_summary_others %>% ggplot( aes(y=basis, x=variable)) +
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
    ggsave(paste0("CUH_SmartDR_20210727_mask_patient_score_backgroundfactor_others_correlation_summary.pdf"),height=4,width=10)

write.table(res_summary_others,file="CUH_SmartDR_20210727_mask_patient_score_backgroundfactor_others_correlation_summary.tsv",sep="\t",quote=F,row.names=F)

