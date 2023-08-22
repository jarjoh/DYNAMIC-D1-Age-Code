rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")

dfD<-read.xlsx("./RegNamesGreve.xlsx")

in_regs<-setdiff(dfD$ROIName,c("Pallidum"))


dfl<-
  dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Bilateral")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = starts_with("BP_"),names_to="region",values_to="BP") %>% 
  mutate(region=factor(gsub("BP_|_Bilateral","",region),levels = in_regs),ID=factor(ID)) 

####### RUN HIERARCHICAL GAM ACROSS CORTICAL ROIS 

lBP_GS<-gam(log(BP)~s(Age,k=6,bs='tp',m=2)+s(region,bs='re')+s(Age,region,bs='fs',k=6,m=2)+s(ID,bs='re',m=2),data = filter(dfl,Age<90,region %in% in_regs,is.numeric(BP)),method = "REML",family = "gaussian")

gam(lBP~lBP~s(Age,k=6,bs='tp',m=2)+s(region,bs='re')+s(Age,region,bs='fs',k=6,m=2)+s(ID,bs='re',m=2),data=filter(dfl, region %in% c("caudalanteriorcingulate","rostralanteriorcingulate")))

tidy(lBP_GS) #### Supplemental Table 2

##############################################
###### THIS IS SHAPE ANALYSIS OF DEVIATIONS FROM THE GLOBAL MEAN: Supplemental Table S3
in_regs<-c(dfD[which(dfD$Map=="Cortex"),"ROIName"])

library(kmlShape)
ndat<-data.frame(Age=rep(20:80,length(in_regs)),region=rep(in_regs,length(20:80))) %>% arrange(region,Age) %>% mutate(ID=Age)

gm_pred2 <- cbind(ndat,predict.gam(lBP_GS,ndat,se.fit=TRUE,type="terms",exclude="s(ID)")) %>% 
  group_by(region) %>%
  mutate(fits=((`fit.s(Age,region)`-`fit.s(region)`)))

gm_pred2 %>% ggplot(aes(Age,fits,color=region))+geom_line()

BPClds <-gm_pred2 %>% pivot_wider(id_cols = Age,names_from=region,values_from=fits) %>% dplyr::select(in_regs) %>% t(.) %>% data.frame(.) %>% mutate(ID=rownames(.)) %>% 
  dplyr::select(ID,1:62) %>% cldsWide(.)

plot(BPClds)
kmlShape(BPClds,3)
plot(BPClds)


curCls<-BPClds["clusters"] %>% data.frame(class=.) %>% mutate(region=rownames(.),class=as.numeric(class)) %>% mutate(class=ifelse((class==1 & !(region %in% c("frontalpole","caudalmiddlefrontal","rostralmiddlefrontal","parstriangularis","parsorbitalis"))),4,class)) %>%
  mutate(Aggreg=factor(ifelse(class==1,"DLPFC",ifelse(class==2,"Central",ifelse(class==3,"Operculum_ACC","Posterior"))))) %>%
  merge(.,dfD,by.x="region",by.y="ROIName") %>% arrange(class) %>% dplyr::select(Aggreg,Long_Name,region,class) #%>%
  write.xlsx("./DYNAMIC_SupplementalTable3.xlsx",rowNames=T)


######### THIS IS FOR SELECTED REGIONS SUBMODELS: Supplemental Table S4
mod_list<-list()
for(r in c("Central","Operculum_ACC","DLPFC","Posterior")){
  in_regs<-curCls[which(curCls$Aggreg %in% r),"region"]
  lBP_GSd<-gam(BP~s(Age,k=6,bs='tp',m=2)+s(region,bs='re')+s(Age,region,bs='fs',k=6,m=2)+s(ID,bs='re',m=2),data = filter(dfl,Age<90,region %in% in_regs),method = "REML",family = "gaussian")
  mod_list[[paste0(r,"_GS")]]<-lBP_GSd
}

dT<-data.frame()
for(r in c("DLPFC","Operculum_ACC","Central","Posterior")){
  dT<-rbind(dT,mutate(filter(tidy(mod_list[[paste0(r,"_GS")]]),term %in% c("s(Age)","s(Age,region)")),region=r))
}
dT<-dT %>% pivot_wider(id_cols = region,names_from="term",values_from="p.value")
colnames(dT)[2:3]<-c("pAge","pReg")
dT %>% adjust_pvalue(p.col=c("pReg"),method = "bonferroni") %>% dplyr::select(region,pAge,pReg.adj) %>% 
  write.xlsx("./DYNAMIC_SupplementalTable4.xlsx")



