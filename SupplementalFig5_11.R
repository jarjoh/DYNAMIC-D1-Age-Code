rm(list=ls())



####### GET COGNITION AGE ETC
dfcog<-read.xlsx("../ExcelData/Cogtestdata_master_fixed_20210809.xlsx") ### THIS IS PROPRIETARY

tdc<-dfcog %>% dplyr::select(EPM,WM) 
a<-pca(tdc,nfactors = 1,rotate = "none")

##### ADD PC-SCORES
tdc<-cbind(dplyr::select(dfcog,"ID","Sex","Age","WM","EPM","SPEED"),data.frame(Memory=a$scores[,'PC1']))

####### GET BPs
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")
dfb<-
  dfAllcomp %>% filter(Study == "DYN") %>%
  dplyr::select(ID,Age,BP_Caudate_Bilateral,BP_Putamen_Bilateral,BP_Accumbens_area_Bilateral,BP_DLPFC_Bilateral,BP_Operculum_ACC_Bilateral,
                BP_Central_Bilateral,BP_Posterior_Bilateral) %>%
  rename_with(~gsub("_Bilateral","",.x))

####### GET CORTICOCAUDATE CONN
dfcon<-type.convert(read.csv2("../Connectivity_similarity/CaudPutFCstrength_to_Jarkko_20211007.csv",sep = ",")) ### THIS IS PROPRIETARY
dfc<-dfcon %>% dplyr::select(ID,MW_Caudate_Str_pos) %>% mutate(FC_CtxCau = MW_Caudate_Str_pos) 

####### GET CORTICOCORTICAL CONN
dfAR <- read.xlsx("../ExcelData/RDLPFC_Strength_Oct2022.xlsx") ### THIS IS PROPRIETARY
dfAR<-dfAR %>% mutate(FC_RDLPFC_M=CorrStrength_RDLPFC_MOVIE)


###### Merge
dfCo<-merge(tdc,dplyr::select(dfb,-Age),by="ID")
dfCo<-merge(dfCo,dplyr::select(dfc,ID,FC_CtxCau),by="ID")
dfCo<-merge(dfCo,dplyr::select(dfAR,ID,FC_RDLPFC_M),by="ID")

cud<-dfCo %>% dplyr::select(Age,Memory,EPM,WM,starts_with("BP"),starts_with("FC")) %>% 
  filter(complete.cases(.))

cud <- cud %>% mutate(BP_Ctx=BP_DLPFC+BP_Operculum_ACC+BP_Central+BP_Posterior)

###### Standardize
cuds<-as.data.frame(scale(cud))
cuds <- mutate(cuds,Age=cud$Age)

######## Supplemental Fig S5
cuds %>% 
  ggscatter("Age","Memory",alpha=0.1)+
  geom_smooth(method = "lm",color="red",se = F)+
  geom_smooth(data=filter(cuds,Age<40),method = "lm",formula = y~x,color="black",se = F)+
  geom_smooth(data=filter(cuds,Age>=40),method = "lm",formula = y~x,color="black",se = F)+
  xlab("Age")+
  ylab("Memory scores (standardized)")

ggsave("DYNAMIC_SupplementalFig5.pdf",width = 13,height = 8,units = "cm")

### Tests in the legend and MS text
cuds %>% lm(Memory~Age*(Age<40),data=.) %>% summary()
cuds %>% filter(Age<40) %>% lm(Memory~Age,data=.) %>% summary()
cuds %>% filter(Age>=40) %>% lm(Memory~Age,data=.) %>% summary()


######## Supplemental Fig S6
cud %>% 
  ggscatter("Age","FC_CtxCau",alpha=0.1)+
  geom_smooth(method = "lm",color="red",se = F)+
  geom_smooth(data=filter(cud,Age<40),method = "lm",formula = y~x,color="black",se = F)+
  geom_smooth(data=filter(cud,Age>=40),method = "lm",formula = y~x,color="black",se = F)+
  xlab("Age")+
  ylab("Corticocaudate connectivity")

ggsave("DYNAMIC_SupplementalFig6.pdf",width = 13,height = 8,units = "cm")

### Tests in the legend and MS text
cud %>% lm(FC_CtxCau~Age*(Age<40),data=.) %>% summary()

######## Supplemental Fig S7
cud %>% 
  mutate(AgeG=factor(ifelse(Age<40,"20-40","40-80"),levels=c("20-40","40-80"))) %>%
  ggscatter("FC_CtxCau","Memory",alpha=0.1)+
  geom_smooth(method = "lm",color="red",se = F)+
  xlab("Corticocaudate connectivity")+
  ylab("Memory scores (standardized)")+
  facet_wrap(~AgeG)+stat_cor(p.accuracy = 0.001)

ggsave("DYNAMIC_SupplementalFig7.pdf",width = 13,height = 8,units = "cm")


######## Supplemental Fig S8
cud %>% 
  mutate(AgeG=factor(ifelse(Age<40,"20-40","40-80"),levels=c("20-40","40-80"))) %>%
  ggscatter("BP_Caudate","FC_CtxCau",alpha=0.1)+
  geom_smooth(method = "lm",color="red",se = F)+
  ylab("Corticocaudate connectivity")+
  xlab("Caudate D1DR availability")+
  facet_wrap(~AgeG)+stat_cor(p.accuracy = 0.001)

ggsave("DYNAMIC_SupplementalFig8.pdf",width = 13,height = 8,units = "cm")


######## Supplemental Fig S9
cud %>% 
  mutate(AgeG=factor(ifelse(Age<40,"20-40","40-80"),levels=c("20-40","40-80"))) %>%
  ggscatter("Age","FC_RDLPFC_M",alpha=0.1)+
  geom_smooth(method = "lm",color="red",se = F)+
  ylab("Frontoparietal connectivity")+
  xlab("Age")+
  facet_wrap(~AgeG,scales = "free_x")+stat_cor(p.accuracy = 0.001)

ggsave("DYNAMIC_SupplementalFig9.pdf",width = 13,height = 8,units = "cm")

### Tests in the legend and MS text
cud %>% lm(FC_RDLPFC_M~Age*(Age<40),data=.) %>% summary()

######## Supplemental Fig S10
cud %>% 
  mutate(AgeG=factor(ifelse(Age<40,"20-40","40-80"),levels=c("20-40","40-80"))) %>%
  ggscatter("FC_RDLPFC_M","WM",alpha=0.1)+
  geom_smooth(method = "lm",color="red",se = F)+
  xlab("Frontoparietal connectivity")+
  ylab("Working memory scores")+
  facet_wrap(~AgeG,scales = "free_x")+stat_cor(p.accuracy = 0.001)

ggsave("DYNAMIC_SupplementalFig10.pdf",width = 13,height = 8,units = "cm")

### Tests in the legend and MS text
cud %>% lm(WM~Age*(Age<40)+FC_RDLPFC_M*(Age<40),data=.) %>% summary()

######## Supplemental Fig S11
cud %>% 
  mutate(AgeG=factor(ifelse(Age<40,"20-40","40-80"),levels=c("20-40","40-80"))) %>%
  ggscatter("BP_DLPFC","FC_RDLPFC_M",alpha=0.1)+
  geom_smooth(method = "lm",color="red",se = F)+
  ylab("Frontoparietal connectivity")+
  xlab("Frontal D1DR")+
  facet_wrap(~AgeG,scales = "free_x")+stat_cor(p.accuracy = 0.001)

ggsave("DYNAMIC_SupplementalFig11.pdf",width = 13,height = 8,units = "cm")

### Tests in the legend and MS text
cud %>% lm(FC_RDLPFC_M~Age*(Age<40)+BP_DLPFC*(Age<40),data=.) %>% summary()

