library(readxl)
library(ggplot2)


df <- read_excel("../../data/CUH_SmartDR_20210727.xlsx")
all_vars <- colnames(df)
dose1_vars <- all_vars[grep("1_",all_vars)]
dose1_vars <- dose1_vars[-grep("0day0",dose1_vars)]
dose1_vars <- dose1_vars[-grep("other_symptoms",dose1_vars)]

dose2_vars <- all_vars[grep("2_",all_vars)]
dose2_vars <- dose2_vars[-grep("0day0",dose2_vars)]
dose2_vars <- dose2_vars[-grep("other_symptoms",dose2_vars)]

dose1_response_freq <- c()
dose1_symptomatic_freq <- c()
dose2_response_freq <- c()
dose2_symptomatic_freq <- c()

for (i in 1:14){
	dose1_vars_day <- dose1_vars[grep(paste0("1_",i,"[a-z]"),dose1_vars)]
	dose1_response_freq <- c(dose1_response_freq,nrow(na.omit(df[,dose1_vars_day])))
	dose1_symptomatic_freq <- c(dose1_symptomatic_freq,sum(rowSums(na.omit(df[,dose1_vars_day]))!=0))
	dose2_vars_day <- dose2_vars[grep(paste0("2_",i,"[a-z]"),dose2_vars)]
	dose2_response_freq <- c(dose2_response_freq,nrow(na.omit(df[,dose2_vars_day])))
	dose2_symptomatic_freq <- c(dose2_symptomatic_freq,sum(rowSums(na.omit(df[,dose2_vars_day]))!=0))
}

response_summary <- data.frame(freq=c(dose1_response_freq,dose2_response_freq,dose1_symptomatic_freq,dose2_symptomatic_freq),rate=c(dose1_response_freq,dose2_response_freq,dose1_symptomatic_freq,dose2_symptomatic_freq)/nrow(df),type=c(rep("response",28),rep("symptomatic",28)),dose=c(rep("dose1",14),rep("dose2",14),rep("dose1",14),rep("dose2",14)),days=rep(c(1:14),4))

ggplot(data=response_summary,aes(x=days,y=rate,colour=dose))+
	geom_line(aes(linetype=type))+
	geom_point(aes(shape=type),stroke=1)+
	scale_y_continuous(limits=c(NA,1))+
	theme_classic(base_size=16)
ggsave("CUH_SmartDR_20210727_response_rate.pdf",width=7,height=4)




