rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")
# load("./Combined_SCH_DYN_OutliersCleaned_NoPVC_August2023.RData") ### USE THIS TO PRODUCE NON-PVE CORRECTED ANALYSIS (Supplemental Table 5)


in_regs<-c("Caudate","Putamen","Accumbens_area","DLPFC","Central","Operculum_ACC","Posterior")
dfl<-
  dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Bilateral")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = starts_with("BP_"),names_to="region",values_to="BP") %>% 
  mutate(region=factor(gsub("BP_|_Bilateral","",region),levels = in_regs),ID=factor(ID))  

######## COMPARE MODELS
mod_list <- list()
for(r in in_regs){
  print(r)
  dat<-filter(dfl,Age<90,region %in% r,Study=="DYN",!is.na(BP))
  mod_list[[paste0("logBP_",r,"_gam")]]<-mgcv::gam(log(BP)~s(Age,k=6,bs='tp'),data = dat,method = "REML",family = "gaussian")
  mod_list[[paste0("logBP_",r,"_lm")]]<-lm(log(BP)~Age,data = dat)
  mod_list[[paste0("logBP_",r,"_lm2")]]<-lm(log(BP)~Age*AgeGroup2,data = dat)
}
# Out<-data.frame(matrix(nrow = length(in_regs),ncol=8)); rownames(Out)<-in_regs; colnames(Out)<-c("lm","gam","exp","aexp","log_lm","log_lm2","log_lm3","log_gam")#,"log_lme_ri","log_lme_ris","log_lme_ris_s")
Out<-data.frame(matrix(nrow = length(in_regs),ncol=3)); rownames(Out)<-in_regs; colnames(Out)<-c("lm","gam","lm2")#,"log_lme_ri","log_lme_ris","log_lme_ris_s")
for(r in in_regs){
  ins<-c("gam","lm","lm2")#,"lme_ri","lme_ris","lme_ris_s")
  for(mod in ins){
    Out[r,mod]<-round(AIC(mod_list[[paste0("logBP_",r,"_",mod)]]),digits = 1)
  }
  
}
Out %>% write.xlsx("./DYNAMIC_Table1.xlsx",rowNames=T)
