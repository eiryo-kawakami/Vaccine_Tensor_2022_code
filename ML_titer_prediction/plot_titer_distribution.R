library(readr)
library(dplyr)
library(ggplot2)


threshold <- c(9,10,11,12,13)

df <- read_tsv(paste0("CUH_SmartDR_test_rfclass_upper8_rep0_predict.txt"))
actual <- ifelse(df$PostTiterLog2 > 8, 1, 0)

freq <- length(actual)-sum(actual)

tmp_count <- sum(actual)

for (th in threshold){

	actual <- ifelse(df$PostTiterLog2 > th, 1, 0)

	freq <- c(freq,abs(sum(actual)-tmp_count))

	tmp_count <- sum(actual)

}

freq <- c(freq,sum(actual))

threshold2 <- c("<8","8-9","9-10","10-11","11-12","12-13","13<")

dat <- data.frame(threshold=threshold2,freq=freq)
dat$threshold <- factor(dat$threshold,levels=threshold2)

g <- ggplot(dat, aes(x = threshold, y = freq, fill = threshold))
g <- g + geom_bar(stat = "identity")
g <- g + theme_classic()
ggsave("CUH_SmartDR_PostTiter_distribution.pdf",width=5,height=3)



