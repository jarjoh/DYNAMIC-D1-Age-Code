rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")


in_regs<-c("Caudate","Putamen","Accumbens_area","DLPFC","Central","Operculum_ACC","Posterior")
dfl<-
  dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Bilateral")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = starts_with("BP_"),names_to="region",values_to="BP") %>% 
  mutate(region=factor(gsub("BP_|_Bilateral","",region),levels = in_regs),ID=factor(ID))  

df_card<-read.xlsx("../Cardio/mainCVD_210601.xlsx") ###### THIS IS PROPRIETARY, ASK FROM THE LEAD CONTACT
df_card<-df_card %>% mutate(logWML=ifelse(log(WML_TLV)==-Inf,NA,log(WML_TLV)))

dat<-
  filter(dfl,Age<=99,region %in% c("Caudate","Putamen","DLPFC","Operculum_ACC","Central","Posterior"),Study=="DYN") %>% 
  pivot_wider(names_from = region, values_from=BP) %>%
  merge(.,df_card[,c("ID","logWML")],by="ID")


###### Fig 2B
dat %>% mutate(logWML=ifelse(Age<40,NA,logWML)) %>%
  mutate(Caudate_raw=Caudate,Putamen_raw=Putamen) %>%
  mutate(Caudate_cor=predict(lm(Caudate~Age+logWML,data=.),newdata=data.frame(Age=.$Age,logWML=rep(0,nrow(.))))) %>%
  mutate(Caudate_cor=ifelse(Age<40,Caudate,Caudate_cor)) %>%
  mutate(Putamen_cor=predict(lm(Putamen~Age+logWML,data=.),newdata=data.frame(Age=.$Age,logWML=rep(0,nrow(.))))) %>%
  mutate(Putamen_cor=ifelse(Age<40,Putamen,Putamen_cor)) %>%
  pivot_longer(all_of(c("Putamen_raw","Putamen_cor","Caudate_raw","Caudate_cor")),names_to=c("region",".value"),names_sep="_") %>%
  pivot_longer(all_of(c("raw","cor")),names_to="type",values_to="BP") %>% 
  mutate(type=factor(type,levels=c("raw","cor")),region=factor(region,levels = c("Putamen","Caudate"))) %>%
  ggplot(aes(Age,BP,color=region,linetype=type))+geom_smooth(method = "gam",formula = y~s(x,bs='tp',k=6),se=F)+ geom_vline(aes(xintercept=40),size=0.1,linetype="dashed")+
  ylab("Predicted D1DR availability")+guides(linetype="none")+theme_pubr()+ theme(legend.title = element_blank(),legend.position = c(0.8,0.8))+
  scale_color_viridis_d(option = "magma",begin = .2,end=.8)

ggsave("DYNAMIC_Fig2B.pdf",width = 10,height = 10,units = "cm")


###### Supplemental Table 7
library(nlme)
a<-dat %>% filter(Age>=40,Age<85,!is.na(Caudate),!is.na(Putamen)) %>% select(ID,Age,Sex,Caudate,Putamen,logWML) %>% 
  pivot_longer(all_of(c("Caudate","Putamen")),names_to="region",values_to="BP") %>%
  lme(BP~Age,data=.,random = list(region = ~1+Age),control=lmeControl(maxIter = 50,opt = "optim")) #%>% summary()

b<-dat %>% filter(Age>=40,Age<85,!is.na(Caudate),!is.na(Putamen)) %>% select(ID,Age,Sex,Caudate,Putamen,logWML) %>% 
  pivot_longer(all_of(c("Caudate","Putamen")),names_to="region",values_to="BP") %>%
  lme(BP~Age,data=.,random = list(region = ~1)) #%>% summary()


c1<-dat %>% filter(Age>=40,Age<85,!is.na(Caudate),!is.na(Putamen),!is.na(logWML)) %>% select(ID,Age,Sex,Caudate,Putamen,logWML) %>% 
  pivot_longer(all_of(c("Caudate","Putamen")),names_to="region",values_to="BP") %>%
  lme(BP~Age,data=.,random = list(region = ~Age,region = ~logWML),control=lmeControl(maxIter = 50,opt = "optim")) #%>% summary()
c2<-dat %>% filter(Age>=40,Age<85,!is.na(Caudate),!is.na(Putamen),!is.na(logWML)) %>% select(ID,Age,Sex,Caudate,Putamen,logWML) %>% #nrow()
  pivot_longer(all_of(c("Caudate","Putamen")),names_to="region",values_to="BP") %>%
  lme(BP~Age,data=.,random = list(region = ~1,region = ~logWML),control=lmeControl(maxIter = 50,opt = "optim")) #%>% summary()

anova(b,a) %>%
  rbind(.,anova(c2,c1)) 
