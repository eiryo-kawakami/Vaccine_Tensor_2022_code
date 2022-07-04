library(ggplot2)
library(readr)
library(gridExtra)
library(RColorBrewer)
library(ggridges)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

rank <- 4

nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

time_list <- c("1_1","1_2","1_3","1_4","1_5","1_6","1_7","2_1","2_2","2_3","2_4","2_5","2_6","2_7")

df <- read_excel('/Users/eiryokawakami/Dropbox/DataAnalysis/Chiba_Vaccine/data/CUH_SmartDR_20210727.xlsx')

item_list = c()

for (c in colnames(df)){
    print(c)
    if (str_detect(c,pattern="2_7")){
        item_list <- c(item_list,str_replace(c,pattern="2_7",replacement=""))
    }
}

item_list <- item_list[-which(item_list == "other_symptoms")]

for (t in time_list){

  components_ind <- read_tsv(paste0("CUH_SmartDR_20210727_NMF_time=",t,"_components.txt"))
  components_ind <- components_ind %>% gather(items,value,2:ncol(components_ind))

  colnames(components_ind) <- c("basis","items","value")
  components_ind$items <- factor(components_ind$items,levels=rev(item_list))

  components_ind %>% ggplot( aes(y=items, x=factor(basis))) +
  geom_tile(aes(fill=value)) +
  scale_fill_gradient(low = "white",high = "Red",limits=c(0,5),na.value="Red") +
  theme_bw(base_size=16) +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25, linetype="dotted"),
      panel.grid.minor = element_line(colour = "black", size = 0.25, linetype="dotted"))
  ggsave(paste0("CUH_SmartDR_20210727_NMF_time=",t,"_components_heatmap.pdf"),height=4,width=6)
}
