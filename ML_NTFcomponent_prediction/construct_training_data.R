library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(missForest)
library(doParallel)
library(ggplot2)
library(fastDummies)

cores <- detectCores(logical = FALSE)
print(cores)
registerDoParallel(cores = cores-2)


df_original <- read_excel("../../data/CUH_SmartDR_20210727.xlsx")

df_score <- read_tsv("CUH_SmartDR_20210727_mask_patient_score_with_cluster_normalized.tsv")
colnames(df_score) <- c("CUH","basis1","basis2","basis3","basis4","cluster")

df_merged <- left_join(df_score,df_original,by="CUH")
df_merged <- tibble(df_merged %>% select(-"Job","Smoking","COVID19Contact"),fastDummies::dummy_cols(df_merged[,c("Job")]) %>% select(-"Job"),fastDummies::dummy_cols(df_merged[,"Smoking"],ignore_na = TRUE) %>% select(-"Smoking"),fastDummies::dummy_cols(df_merged[,"COVID19Contact"],ignore_na = TRUE) %>% select(-"COVID19Contact"))

obj_vars <- c("basis1","basis2","basis3","basis4")
exp_vars <- c(colnames(df_original)[c(2,3,6:40,42)],colnames(df_merged)[(ncol(df_merged)-10):ncol(df_merged)])
exp_vars <- exp_vars[-which(exp_vars %in% c("COPD","AgeCategory","AgeQuartile","IntervalToTest","Job","Job_5","Nurse","Smoking","Smoking_0","COVID19Contact","AlcoholEveryday","PreTiter04"))]
data_exp <- df_merged[,exp_vars]

numeric_vars <- c("Age","ShotInterval","PreTiter")

data_exp_numeric <- data_exp %>% select(numeric_vars)
data_exp_factor <- data_exp %>% select(-numeric_vars)

data_exp_factor <- data.frame(lapply(data_exp_factor,as.factor))

data_exp <- data.frame(data_exp_numeric,data_exp_factor)

data_exp_imp <- missForest(data_exp, maxiter=10, ntree=1000, mtry = floor(sqrt(ncol(data_exp))), verbose=TRUE)
data_exp_imp <- data_exp_imp$ximp
# data_exp_imp <- dummy_cols(.data=data_exp_imp, select_columns=c("Job"))
# data_exp_imp <- data_exp_imp %>% select(-c("Job","Job_5"))

data_imp <- data.frame(CUH=df_merged[,"CUH"],data_exp_imp,df_merged[,obj_vars,drop=FALSE])

write.table(data_imp,"CUH_SmartDR_20210727_mask_tensor_merged_missforest.txt",sep="\t",quote=FALSE,row.names=FALSE)

# for (v in numeric_vars){
# 	tmp_df <- data.frame(value=dialysis_exp[,v])
# 	tmp_df_m <- melt(tmp_df)
# 	g <- ggplot(tmp_df,aes(x=value,y=..density..))
# 	g <- g + geom_histogram(position="identity", alpha=0.8)
# 	g <- g + geom_density(alpha=0.2, show.legend=FALSE)
# 	g <- g + scale_fill_npg() + scale_color_npg()
# 	ggsave(paste0("Chiba_Dialysis_",v,"_histogram.pdf"))
# }

# for (v in numeric_vars){
# 	tmp_df <- data.frame(original=dialysis_exp[,v],imputed=dialysis_exp_imp[,v])
# 	tmp_df_m <- melt(tmp_df)
# 	g <- ggplot(tmp_df,aes(x=value,y=..density..,fill=variable))
# 	g <- g + geom_histogram(position="identity", alpha=0.8)
# 	g <- g + geom_density(aes(color=variable, alpha=0.2), show.legend=FALSE)
# 	g <- g + scale_fill_npg() + scale_color_npg()
# }

pID_list <- unique(data_imp$CUH)
all_list <- 1:length(pID_list)
train_list <- all_list[all_list %% 5 != 0]
test_list <- all_list[all_list %% 5 == 0]
train_pID <- pID_list[train_list]
test_pID <- pID_list[test_list]

data_imp_train <- data_imp[data_imp$CUH %in% train_pID,]
data_imp_test <- data_imp[data_imp$CUH %in% test_pID,]

write.table(data_imp_train,"CUH_SmartDR_20210727_mask_tensor_merged_missforest_train.txt",sep="\t",quote=FALSE,row.names=FALSE)
write.table(data_imp_test,"CUH_SmartDR_20210727_mask_tensor_merged_missforest_test.txt",sep="\t",quote=FALSE,row.names=FALSE)


