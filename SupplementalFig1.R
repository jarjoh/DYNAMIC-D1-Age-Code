rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")

##### ###### ######
#### PULL OUT THE DESCRIPTIVES in 20 - 40 y
##### ###### ######
library(latex2exp)
dfD<-read.xlsx("./RegNamesGreve.xlsx")
dfD<-dfD %>%  arrange(desc(RepOrd))
in_regs<-c(paste0(dfD$ROIName,"_Left"),paste0(dfD$ROIName,"_Right"))
subset(dfAllcomp,Study %in% c("DYN"),select=c("ID","Age","Sex","Study",paste0("BP_",in_regs))) %>% 
  as_tibble() %>%
  # mutate(across(paste0("BP_",in_regs), scale)) %>%
  pivot_longer(all_of(paste0("BP_",in_regs)),names_to="region",values_to="BP") %>% mutate(Hemisphere=ifelse(grepl("Left",region),"Left","Right")) %>%
  mutate(region=factor(gsub("BP_|_Bilateral|_Left|_Right","",region),levels=dfD$ROIName)) %>%
  filter(Age<=40) %>%
  ggbarplot("region","BP",color = "Hemisphere",add="mean_se",orientation="horiz",error.plot = "upper_errorbar",position = position_dodge(0.9))+
  scale_x_discrete(label=dfD$Long_Name)+xlab("")+ylab(unname(TeX(r'($^{11}C-SCH23390$  $BP_{ND}$)')))+theme_classic()#+stat_compare_means()

ggsave("./DYNAMIC_SuppFig1.pdf",width = 15,height=20,units = "cm")
