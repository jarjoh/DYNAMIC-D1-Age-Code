rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")

dfb<-
  dfAllcomp %>% filter(Study == "DYN") %>%
  dplyr::select(ID,Age,BP_Caudate_Bilateral,BP_Putamen_Bilateral,BP_Accumbens_area_Bilateral,BP_DLPFC_Bilateral,BP_Operculum_ACC_Bilateral,
                BP_Central_Bilateral,BP_Posterior_Bilateral) %>%
  rename_with(~gsub("_Bilateral","",.x))


####### COMBINE WML volumes with BPs
df_card<-read.xlsx("../Cardio/mainCVD_210601.xlsx") ###### THIS IS PROPRIETARY, ASK FROM THE LEAD CONTACT
colnames(df_card)[6]<-"CVDrisk"
df_card<-df_card %>% mutate(WML_TLV=ifelse(log(WML_TLV)==-Inf,NA,log(WML_TLV)))

tt<-merge(dfb,dplyr::select(df_card,-Age,-Sex),by="ID")

tt<-filter(tt,Age>=40,complete.cases(tt))

tts<-tt %>% 
  rename_with(~gsub("BP_","",.x,fixed = T)) %>%
  mutate(logWML=WML_TLV) %>%
  filter(Age>=40,Age<85,!is.na(Caudate),!is.na(Putamen),!is.na(logWML)) %>%
  mutate(Caudate=residuals(lm(Caudate~Age,data=.)),Putamen=residuals(lm(Putamen~Age,data=.)),
         Accumbens_area=residuals(lm(Accumbens_area~Age,data=.)),DLPFC=residuals(lm(DLPFC~Age,data=.)),
         Central=residuals(lm(Central~Age,data=.)),Operculum_ACC=residuals(lm(Operculum_ACC~Age,data=.)),
         Posterior=residuals(lm(Posterior~Age,data=.))) %>% 
  mutate(logWML=residuals(lm(logWML~Age,data=.))) 

######### Fig 2A

tts  %>%
  dplyr::select(ID,Age,Caudate,Putamen,logWML) %>%
  pivot_longer(all_of(c("Caudate","Putamen")),names_to="region",values_to="BP") %>%
  ggplot(aes(logWML,BP,shape=region))+geom_point()+geom_smooth(aes(linetype=region),method="lm",se=F,color="black")+stat_cor()+theme_pubr()+
  xlab("White matter lesion volume\n(log-transformed, age-residualized)")+ylab("D1DR availability\n(age-residualized)")+theme(legend.title = element_blank(),legend.position = c(.8,.9))

ggsave("DYNAMIC_Fig2A.pdf",width = 10,height = 10,units = "cm")


######### Calculate age-corrected correlations between WML and regional BPs, and compare the strength of correlation with that in the caudate
Out<-matrix(nrow=7,ncol=2)
rownames(Out)<-c("Caudate","Putamen","Accumbens_area","DLPFC","Operculum_ACC","Posterior","Central")
colnames(Out)<-c("R","DR")
for(reg in c("Caudate","Putamen","Accumbens_area","DLPFC","Operculum_ACC","Posterior","Central")){
  r12<-tts %>% cor_test("Caudate","logWML")  
  r23<-tts %>% cor_test(all_of(reg),"logWML")  
  Out[reg,"R"]<-sprintf("%1.2f (p=%1.3g)",r23$cor,r23$p)
  if(reg != "Caudate"){
    r13<-tts %>% cor_test(all_of(reg),"Caudate") 
    rt<-r.test(n=111,r12=r12$cor,r23=r23$cor,r13=r13$cor)
    Out[reg,"DR"]<-sprintf("t=%1.2f (p=%1.3g)",rt$t,rt$p)
  }
}

data.frame(Out) %>% write.xlsx("DYNAMIC_SupplementalTable6.xlsx",rowNames=T)


