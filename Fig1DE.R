rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")

in_regs<-c("Putamen","DLPFC","Caudate","Central","Accumbens_area","Posterior","Operculum_ACC")
dfl<- dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Bilateral")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = ends_with("_Bilateral"),names_to="region",values_to="BP") %>% 
  mutate(region=factor(gsub("BP_|_Bilateral","",region),levels = in_regs),ID=factor(ID))


########### FIG 1D - 1E

######## Collect predictions
preds<-data.frame()
for(r in in_regs){ 
  dat<-filter(dfl,Age<90,region %in% r,Study=="DYN")
  mod_gam<-gam(BP ~ s(Age,k=5,bs='tp',m=2),data=dat)
  preds<-rbind(preds,mutate(cbind(dat[,c("Age","BP")],predict(mod_gam,dat)),mod="gam",region=r,fit=`predict(mod_gam, dat)`,Age=Age,BP=BP,.keep="none"))
}

preds %>% 
  mutate(region=gsub("_"," & ",gsub("_area|_Proper","",region))) %>%
  mutate(region=gsub("DL","",region)) %>%
  mutate(region=gsub("Operculum & ACC","Operculum\nACC",region)) %>%
  mutate(region=factor(region, levels=c("Putamen","Caudate","Accumbens","Thalamus","PFC","Central","Posterior","Operculum\nACC"))) %>%
  arrange(region) %>%
  filter(mod=="gam",region %in% c("PFC","Operculum\nACC")) %>%
  ggplot(aes(Age,BP))+
  geom_line(aes(Age,fit,color=region),size=.5)+
  geom_vline(aes(xintercept=40),size=0.1,linetype="dashed")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.1),axis.ticks = element_line(size=0.1),panel.border = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),axis.text = element_text(size=6),
        axis.title = element_text(size=6),strip.text = element_blank(),
        legend.title = element_blank(),legend.key.size = unit(.01,"cm"),strip.background = element_blank(), strip.text.x = element_text(margin=margin(b=0.1),size=rel(.5), vjust = 1),
        legend.position = c(.75,.9), legend.background = element_rect(fill = "transparent", color = NA),legend.text = element_text(size=4),
        panel.spacing.y = unit(.5,"lines"),panel.spacing.x = unit(0.5,"lines"))#+ 

ggsave("DYNAMIC_Fig1D.pdf",width = 2.5,height = 2.3,units = "cm",bg="transparent")


preds %>% 
  mutate(region=gsub("_"," & ",gsub("_area|_Proper","",region))) %>%
  mutate(region=gsub("DL","",region)) %>%
  mutate(region=gsub("Operculum & ACC","Operculum\nACC",region)) %>%
  mutate(region=factor(region, levels=c("Putamen","Caudate","Accumbens","Thalamus","PFC","Central","Posterior","Operculum\nACC"))) %>%
  arrange(region) %>%
  filter(mod %in% c("gam"),region %in% c("Putamen","Caudate")) %>%
  ggplot(aes(Age,BP))+
  geom_line(aes(Age,fit,color=region),size=.5)+
  geom_vline(aes(xintercept=40),size=0.1,linetype="dashed")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.1),axis.ticks = element_line(size=0.1),panel.border = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),axis.text = element_text(size=6),
        axis.title = element_text(size=6),strip.text = element_blank(),
        legend.title = element_blank(),legend.key.size = unit(.01,"cm"),strip.background = element_blank(), strip.text.x = element_text(margin=margin(b=0.1),size=rel(.5), vjust = 1),
        legend.position = c(.75,.9), legend.background = element_rect(fill = "transparent", color = NA),legend.text = element_text(size=4),
        panel.spacing.y = unit(.5,"lines"),panel.spacing.x = unit(0.5,"lines"))#+ 

ggsave("DYNAMIC_Fig1E.pdf",width = 2.5,height = 2.3,units = "cm",bg="transparent")
