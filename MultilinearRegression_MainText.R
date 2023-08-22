rm(list=ls())



####### GET COGNITION AGE ETC
dfcog<-read.xlsx("../Cogtestdata_master_fixed_20210809.xlsx") ### THIS IS PROPRIETARY
library(rstatix)


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
dfAR <- read.xlsx("./RDLPFC_Strength_Oct2022.xlsx") ### THIS IS PROPRIETARY
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


######## Multiple regression models in the main text:
#### Neuropsychological associations
# 1: Age differential effect of caudate BP to memory
cuds %>% lm(Memory~Age*(Age<40)+BP_Caudate*(Age<40),data=.) %>% summary()
cud %>% filter(Age<40) %>% cor_test(Memory,BP_Caudate)
ppcor::pcor.test(cud[which(cud$Age<40),"BP_Caudate"],cud[which(cud$Age<40),"Memory"],cud[which(cud$Age<40),"Age"])
cud %>% filter(Age>=40) %>% cor_test(Memory,BP_Caudate)
ppcor::pcor.test(cud[which(cud$Age>=40),"BP_Caudate"],cud[which(cud$Age>=40),"Memory"],cud[which(cud$Age>=40),"Age"])

# 2: Age differential effect of corticocaudate connectivity to memory
cuds %>% lm(Memory~Age*(Age<40)+FC_CtxCau*(Age<40),data=.) %>% summary()
cud %>% filter(Age<40) %>% cor_test(Memory,FC_CtxCau)
ppcor::pcor.test(cud[which(cud$Age<40),"FC_CtxCau"],cud[which(cud$Age<40),"Memory"],cud[which(cud$Age<40),"Age"])
cud %>% filter(Age>=40) %>% cor_test(Memory,FC_CtxCau)
ppcor::pcor.test(cud[which(cud$Age>=40),"FC_CtxCau"],cud[which(cud$Age>=40),"Memory"],cud[which(cud$Age>=40),"Age"])

# 3: Age differential effect of caudate BP to corticocaudate connectivity
cuds %>% lm(FC_CtxCau~Age+BP_Caudate*(Age<40),data=.) %>% summary()
cud %>% filter(Age<40) %>% cor_test(BP_Caudate,FC_CtxCau)
ppcor::pcor.test(cud[which(cud$Age<40),"FC_CtxCau"],cud[which(cud$Age<40),"BP_Caudate"],cud[which(cud$Age<40),"Age"])
cud %>% filter(Age>=40) %>% cor_test(BP_Caudate,FC_CtxCau)
ppcor::pcor.test(cud[which(cud$Age>=40),"FC_CtxCau"],cud[which(cud$Age>=40),"BP_Caudate"],cud[which(cud$Age>=40),"Age"])

# 4: Three-way interaction effect to memory
cuds %>% lm(Memory~Age*(Age<40)+BP_Caudate*FC_CtxCau*(Age<40),data=.) %>% summary()
cud %>% filter(Age>=40) %>% lm(Memory~Age+BP_Caudate+FC_CtxCau,data=.) %>% summary()
cud %>% filter(Age<40) %>% lm(Memory~Age+BP_Caudate+FC_CtxCau,data=.) %>% summary()
cud %>% filter(Age<40) %>% lm(Memory~Age+BP_Caudate*FC_CtxCau,data=.) %>% summary()


