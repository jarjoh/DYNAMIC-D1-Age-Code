rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")

in_regs<-c("Caudate","Putamen","Accumbens_area","DLPFC","Operculum_ACC","Central","Posterior")
dfl<-
  dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Bilateral")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = starts_with("BP_"),names_to="region",values_to="BP") %>% 
  mutate(region=factor(gsub("BP_|_Bilateral","",region),levels = in_regs),ID=factor(ID))  

###### CALCULATE GAMS
mod_list <- list()
for(r in in_regs){
  print(r)
  dat<-filter(dfl,Age<90,region %in% r,Study=="DYN",!is.na(BP))
  mod_list[[paste0("BP_",r,"_gam")]]<-mgcv::gam(BP~s(Age,k=6,bs='tp'),data = dat,method = "REML",family = "gaussian")
}

########### Fig 1F: striatum
derivatives(mod_list[["BP_Caudate_gam"]],order = 2,eps = 1e-2,n=200) %>% mutate(region="Caudate") %>%
  rbind(.,mutate(derivatives(mod_list[["BP_Putamen_gam"]],order = 2,eps = 1e-2,n=200),region="Putamen")) %>%
  rbind(.,mutate(derivatives(mod_list[["BP_Accumbens_area_gam"]],order = 2,eps = 1e-2,n=200),region="Accumbens_area")) %>%
  mutate(region=factor(gsub("DL","",gsub("_","\n",region)),levels = c("Caudate","Putamen","Accumbens\narea"))) %>% 
  filter(region %in% c("Caudate","Putamen","Accumbens\narea")) %>%
  ggplot(aes(data,derivative))+geom_line(size=.3)+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.1)+
  geom_vline(xintercept = c(34,40),linetype="dashed",size=0.1)+
  geom_hline(yintercept = 0,size=0.1)+
  facet_wrap(~region,ncol=4)+
  xlab("Age")+
  ylab("Change in predicted\nage pattern")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.1),axis.ticks = element_line(size=0.1),panel.border = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),axis.text = element_text(size=6),
        axis.text.y=element_blank(), axis.line.y = element_blank(),axis.ticks.y=element_blank(),
        axis.title = element_text(size=6),strip.text = element_blank(),
        legend.key.size = unit(.1,"cm"),strip.background = element_blank(), strip.text.x = element_text(margin=margin(b=0.1),size=rel(.5), vjust = 1),
        panel.spacing.y = unit(.5,"lines"),panel.spacing.x = unit(0.5,"lines"))

ggsave("DYNAMIC_Fig1F_STR.pdf",width = 6,height = 2.7,units = "cm",bg="transparent")


########### Fig 1F: cortex

derivatives(mod_list[["BP_DLPFC_gam"]],order = 2,eps = 1e-2,n=200) %>% mutate(region="DLPFC") %>%
  rbind(.,mutate(derivatives(mod_list[["BP_Central_gam"]],order = 2,eps = 1e-2,n=200),region="Central")) %>%
  rbind(.,mutate(derivatives(mod_list[["BP_Posterior_gam"]],order = 2,eps = 1e-2,n=200),region="Posterior")) %>%
  rbind(.,mutate(derivatives(mod_list[["BP_Operculum_ACC_gam"]],order = 2,eps = 1e-2,n=200),region="Operculum_ACC")) %>%
  mutate(region=factor(gsub("DL","",gsub("_","\n",region)),levels = c("Central","PFC","Operculum\nACC","Posterior"))) %>% 
  filter(region %in% c("Central","PFC","Operculum\nACC","Posterior")) %>%
  ggplot(aes(data,derivative))+geom_line(size=.3)+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.1)+
  geom_vline(xintercept = c(34,40),linetype="dashed",size=0.1)+
  geom_hline(yintercept = 0,size=0.1)+
  facet_wrap(~region,ncol=4)+
  xlab("Age")+
  ylab("Change in predicted\nage pattern")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.1),axis.ticks = element_line(size=0.1),panel.border = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),axis.text = element_text(size=6),
        axis.text.y=element_blank(), axis.line.y = element_blank(),axis.ticks.y=element_blank(),
        axis.title = element_text(size=6),strip.text = element_blank(),#axis.line.y = element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        legend.key.size = unit(.1,"cm"),strip.background = element_blank(), strip.text.x = element_text(margin=margin(b=0.1),size=rel(.5), vjust = 1),
        panel.spacing.y = unit(.5,"lines"),panel.spacing.x = unit(0.5,"lines"))#+

ggsave("DYNAMIC_Fig1F_CTX.pdf",width = 6,height = 2.7,units = "cm",bg="transparent")


