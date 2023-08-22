rm(list=ls())

library(interactions)
library(stdmod)


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
dfcon<-type.convert(read.csv2("../Connectivity_similarity/CaudPutFCstrength_to_Jarkko_20211007.csv",sep = ","))

dfc<-dfcon %>% dplyr::select(ID,MW_Caudate_Str_pos) %>% mutate(FC_CtxCau = MW_Caudate_Str_pos) 

###### Merge
dfCo<-merge(tdc,dplyr::select(dfb,-Age),by="ID")
dfCo<-merge(dfCo,dplyr::select(dfc,ID,FC_CtxCau),by="ID")
cud<-dfCo %>% dplyr::select(Age,Memory,EPM,WM,starts_with("BP"),starts_with("FC")) %>% 
  filter(complete.cases(.))

###### Standardize
cuds<-as.data.frame(scale(cud))
cuds <- mutate(cuds,Age=cud$Age)



######### Fig 3A:
cuds %>% 
  mutate(AgeG=factor(ifelse(Age<40,"Pre-forties","Post-forties"),levels=c("Pre-forties","Post-forties"))) %>%
  ggscatter("BP_Caudate","Memory",alpha=0.1)+
  geom_smooth(method = "lm",color="black",se = T)+
  xlab("Caudate D1DR (standardized)")+
  ylab("Memory scores (standardized)")+
  facet_wrap(~AgeG,scales = "free")+stat_cor(p.accuracy = 0.001)+
  theme(strip.background = element_blank() ,strip.text = element_text(size=13))

ggsave("DYNAMIC_Fig3A.pdf",width = 13,height = 8,units = "cm")


######### Fig 3C: young

a<-lm(Memory~BP_Caudate*FC_CtxCau,data=filter(cuds,Age<40)) #%>% summary()
a<-lm(Memory~BP_Caudate*FC_CtxCau,data=filter(cuds,Age>=40)) #%>% summary()
plotmod(a,x = BP_Caudate,w = FC_CtxCau,point_size = 0, line_width = 2)+theme_pubr()+
  theme(plot.subtitle = element_blank(),plot.caption = element_blank())+
  ggtitle("Moderation: pre-forties")+
  guides(color=guide_legend(title="FC"))+ylab("Memory score (standardized)")+xlab("Caudate D1DR (standardized)")+
  theme(legend.position = c(0.2,.8),legend.background =  element_rect(colour = NA, fill = "transparent"),plot.title = element_text(hjust = 0.5))

ggsave("DYNAMIC_Fig3C_Young.pdf",width = 7.5,height = 8,units = "cm")


######### Fig 3C: old

a<-lm(Memory~BP_Caudate*FC_CtxCau,data=filter(cuds,Age>=40)) #%>% summary()
plotmod(a,x = BP_Caudate,w = FC_CtxCau,point_size = 0, line_width = 2)+theme_pubr()+
  theme(plot.subtitle = element_blank(),plot.caption = element_blank())+
  ggtitle("Moderation: post-forties")+
  guides(color=guide_legend(title="FC"))+ylab("Memory score (standardized)")+xlab("Caudate D1DR (standardized)")+
  theme(legend.position = c(0.2,.8),legend.background =  element_rect(colour = NA, fill = "transparent"),plot.title = element_text(hjust = 0.5))

ggsave("DYNAMIC_Fig3C_Old.pdf",width = 7.5,height = 8,units = "cm")

######### Fig 3D

cuds %>% filter(Age<40) %>%
  mutate(FCg=ifelse(FC_CtxCau<=median(FC_CtxCau),"Low FC","High FC")) %>%
  mutate(BPg=ifelse(BP_Caudate>=median(BP_Caudate),"High D1DR","Low D1DR")) %>%
  ggplot(aes(FCg,Memory))+geom_violin()+geom_dotplot(binaxis = "y",stackdir = "center",dotsize = 1,fill="grey")+
  stat_summary(fun=median, geom="point", shape=23, size=4)+
  stat_compare_means(method = "t.test",size=4,label.y=3.5)+ylab("Memory scores (standardized)")+facet_wrap(~BPg)+
  theme_pubr()+theme(axis.title.x = element_blank(),strip.background = element_blank(),strip.text = element_text(size=13))

ggsave("DYNAMIC_Fig3D_Young.pdf",width = 9,height = 8,units = "cm")








