rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")

in_regs<-c("Caudate","Putamen","Accumbens_area","DLPFC","Central","Operculum_ACC","Posterior")
dfl<-
  dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Bilateral")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = starts_with("BP_"),names_to="region",values_to="BP") %>% 
  mutate(region=factor(gsub("BP_|_Bilateral","",region),levels = in_regs),ID=factor(ID))  

###### PLOT CORTICAL DATA
dfl %>% filter(Study=="DYN") %>% 
  filter(region %in% c("Central","DLPFC","Posterior","Operculum_ACC")) %>% mutate(region=factor(gsub("DL","",gsub("_","\n",region)),levels=c("Central","PFC","Operculum\nACC","Posterior"))) %>%
  ggplot(aes(Age,BP))+geom_point(size=0.05,color="gray")+
  geom_smooth(method="gam",formula = y~s(x,bs='tp',k=8),se=T,alpha=0.2,size=0.5,color="black")+
  facet_wrap(~region,ncol = 4)+ #,scales="free_y"
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.1),axis.ticks = element_line(size=0.1),panel.border = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),axis.text = element_text(size=6),
        axis.title = element_text(size=6),strip.text = element_blank(),#axis.line.y = element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        legend.key.size = unit(.1,"cm"),strip.background = element_blank(), strip.text.x = element_text(margin=margin(b=0.1),size=rel(.5), vjust = 1),
        panel.spacing.y = unit(.5,"lines"),panel.spacing.x = unit(0.5,"lines"))#+ 

ggsave("DYNAMIC_Fig1C_CTX.pdf",width=6,height = 2.7,units = "cm")


###### PLOT STRIATAL DATA
dfl %>% filter(Study=="DYN") %>% 
  filter(region %in% c("Caudate","Putamen","Accumbens_area")) %>% mutate(region=factor(gsub("_","\n",region),levels=c("Caudate","Putamen","Accumbens\narea"))) %>%
  ggplot(aes(Age,BP))+geom_point(size=0.05,color="gray")+
  geom_smooth(method="gam",formula = y~s(x,bs='tp',k=8),se=T,alpha=0.2,size=0.5,color="black")+
  facet_wrap(~region,ncol = 4)+ #,scales="free_y"
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.1),axis.ticks = element_line(size=0.1),panel.border = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),axis.text = element_text(size=6),
        axis.title = element_text(size=6),strip.text = element_blank(),#axis.line.y = element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        legend.key.size = unit(.1,"cm"),strip.background = element_blank(), strip.text.x = element_text(margin=margin(b=0.1),size=rel(.5), vjust = 1),
        panel.spacing.y = unit(.5,"lines"),panel.spacing.x = unit(0.5,"lines"))#+ 

ggsave("DYNAMIC_Fig1C_STR.pdf",width=6,height = 2.7,units = "cm")


