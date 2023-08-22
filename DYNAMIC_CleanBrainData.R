rm(list=ls())


################### FUNCTIONS
outlierfun<-function(x, y) {
  # outl=paste0(y[which(x < median(x) - 3*mad(x) | x > median(x) + 3*mad(x))],collapse = "|")
  if(length(boxplot.stats(x)$out)>=1){
    outl=paste0(y[which(x %in% c(boxplot.stats(x)$out))],collapse = "|")
  }else{
    outl=""
  }
}

outlierfunTop<-function(x, y) {
  outl=paste0(y[which(x > quantile(x)[4] + 1.5*IQR(x))],collapse = "|")
}


########### LIBRARIES
library(tidyr)
library(openxlsx)
library(dplyr)
library(plyr)
library(ggpubr)
library(psych)
#####################################################################################################################
######### MAKE A TABLE OF ROI NAMES
#####################################################################################################################

dfD<-read.xlsx("./RegNamesGreve.xlsx")

########################################################################################
########################################################################################
###################### BEGINNING OF DYNAMIC
########################################################################################
########################################################################################

########## LOAD DYNAMIC BPs
dfbp<-read.xlsx("./DYNAMIC_FitsOn_Greve_TACDATA_Oct2020_PVC2p5_based_60min_lpntPET2_NRMSE.xlsx") ###### THIS IS PROPRIETARY, ASK FROM THE LEAD CONTACT
colnames(dfbp)[1]<-"ID"
dfbp$ID<-gsub("C","40",dfbp$ID)

### Remove 75 right away, problem with tracer injection
dfbp<-dfbp[which(!(dfbp$ID=="40075")),]

######################### CHOOSE THE METHOD of BP calculation
mod<-"gtm_srtm" ## THIS IS FOR PVE-corrected
#mod<-"nopvc_srtm" ## THIS IS FOR no-pvc
dfBP<-dfbp[,grep(paste0("ID|_",mod,"_BP"),colnames(dfbp))]
colnames(dfBP)<-gsub(paste0("_",mod,"_BP"),"",colnames(dfBP))

######################### GET ROI volumes
dfVol<-dfbp[,grep("ID|_gtm_ROIvol",colnames(dfbp))]
colnames(dfVol)<-gsub("_gtm_ROIvol","",colnames(dfVol))

######################### GET Normalized RMSE of model fits
dfNRMSE<-dfbp[,grep(paste0("ID|_",mod,"_RMSE"),colnames(dfbp))]
colnames(dfNRMSE)<-gsub(paste0("_",mod,"_RMSE"),"",colnames(dfNRMSE))

################### LOOK FOR OUTLIER ACCORDING TO BP/NRMSE
dfBP_L<-pivot_longer(dfBP,all_of(setdiff(colnames(dfBP),"ID")),names_to = c("region"),values_to = "BP") %>%
  mutate(hemi=ifelse(grepl("_Left",region),"Left","Right"),region=gsub("_Left|_Right","",region))

dfVol_L<-pivot_longer(dfVol,all_of(setdiff(colnames(dfVol),"ID")),names_to = c("region"),values_to = "Vol") %>%
  mutate(hemi=ifelse(grepl("_Left",region),"Left","Right"),region=gsub("_Left|_Right","",region))

dfNRMSE_L<-pivot_longer(dfNRMSE,all_of(setdiff(colnames(dfNRMSE),"ID")),names_to = c("region"),values_to = "RMSE") %>%
  mutate(hemi=ifelse(grepl("_Left",region),"Left","Right"),region=gsub("_Left|_Right","",region))

dfL<-merge(dfBP_L,dfNRMSE_L,by=c("ID","region","hemi"))
dfL<-merge(dfL,dfVol_L,by=c("ID","region","hemi"))

dfC<-read.xlsx("../ExcelData/Cogtestdata_master.xlsx")   ###### THIS IS PROPRIETARY, ASK FROM THE LEAD CONTACT
dfL2<-merge(dfL,dfC[,c("ID","AgeGroup","Sex")],by="ID")

#### remove extreme extreme outliers right away
dfL2[which(abs(dfL2$BP)>20 | dfL2$BP < -.1),"BP"]<-NA
dfL2$AgeGroup<-as.factor(dfL2$AgeGroup)

############# THESE CAN BE USED TO VISUALIZE Outliers, NOT REQUIRED FOR ANALYSIS
# ggboxplot(subset(dfL2,(region %in% c("Caudate","Putamen","Accumbens_area")) & (hemi=="Right")),"AgeGroup","BP",color = "Sex")+
#   facet_wrap(~region)
# 
# ggviolin(subset(dfL2,(region %in% c("Caudate","Putamen")) & (hemi=="Left")),
#          "AgeGroup","BP",color = "Sex",add="mean_sd")+
#   facet_wrap(~region)

####### FLAG Outliers according to NRMSE and BP
c<-ddply(dfL2,.(region,hemi,AgeGroup,Sex),summarise,
         outlRMSE=outlierfunTop(RMSE,ID),         
         outlBP=outlierfun(BP,ID)   
)

####### SET outlier BPs and Vols to NaN
rownames(c)<-paste0(c$region,"_",c$hemi,"_",c$AgeGroup,"_",c$Sex)

for(r in unique(c$region)){
  for(h in unique(c$hemi)){
    for(a in unique(c$AgeGroup)){
      for(s in unique(c$Sex)){
        otl<-unlist(strsplit(c[paste0(r,"_",h,"_",a,"_",s),"outlBP"],split = "\\|"))
        otl<-c(otl,unlist(strsplit(c[paste0(r,"_",h,"_",a,"_",s),"outlRMSE"],split = "\\|")))
        if(length(otl)>=1){
          print(sprintf("%s %s %s %s: %i outliers found",r,h,a,s,length(unique(otl))))
          dfL2[intersect(which(dfL2$region==r & dfL2$hemi==h & dfL2$Sex==s),which(dfL2$ID %in% unique(otl))),c("BP","Vol")]<-NA
        }
      }
    }    
  }
}

dfWdyn<-pivot_wider(dfL2,names_from = c("region","hemi"),values_from = c("BP","Vol"),id_cols = c("ID")) %>% mutate(Study="DYN")
dfWdyn<-merge(dfWdyn,dfC[,c("ID","Age","AgeGroup","Sex")],by="ID")
dfWdyn<-dfWdyn %>% dplyr::select(-c(grep("Cerebellum",colnames(dfWdyn))))


################################################################################################################################################################
########################################  MORPHOLOGICAL DATA
################################################################################################################################################################
###### READ IN THE ROI VOLUMES
dfsv<-read.xlsx("../BP_data/DYNAMIC_FitsOn_Greve_TACDATA_Oct2020_PVC2p5_based_60min_lpntPET2_NRMSE.xlsx") ###### THIS IS PROPRIETARY, ASK FROM THE LEAD CONTACT
colnames(dfsv)[1]<-"ID"
dfsv$ID<-gsub("C","40",dfsv$ID)
dfVol<-dfsv[,grep("ID|_gtm_ROIvol",colnames(dfsv))]
colnames(dfVol)<-gsub("_gtm_ROIvol","",colnames(dfVol))

###### READ IN other asegs
asegs<-type.convert(read.csv2("../GMD_data/aseg_stats_NoPure.txt",sep = ""))
colnames(asegs)[1]<-"ID"
asegs$ID<-gsub("D","40",gsub("_T1NoPure","",asegs$ID))

asegs$TotalVentVol<-rowSums(asegs[,c("Left.Lateral.Ventricle","Left.Inf.Lat.Vent","X3rd.Ventricle","X4th.Ventricle","Right.Lateral.Ventricle","Right.Inf.Lat.Vent","X5th.Ventricle")])

asegs$CorpusCallosum<-rowSums(asegs[,grep("CC_",colnames(asegs))])

apars<-c("BrainSegVolNotVent","TotalGrayVol","TotalVentVol","CortexVol","CSF","CerebralWhiteMatterVol","WM.hypointensities","SubCortGrayVol","CorpusCallosum")

####### Calculate ETIV-adjusted volumes 
gm.dynamic<-merge(dfVol,asegs[,c("ID","EstimatedTotalIntraCranialVol",apars)],by="ID")
in_regs<-c(dfD$ROIName)
for(ar in c("EstimatedTotalIntraCranialVol")){
  rm<-mean(gm.dynamic[,ar])
  for(r in in_regs){
    for(h in c("Left","Right")){
      beta<-lm(as.formula(paste0(r,"_",h,"~",ar)),data=gm.dynamic)$coefficients[2]
      gm.dynamic[,paste0(r,"_",h,"_adj_",ar)]<-gm.dynamic[,paste0(r,"_",h)]-beta*(gm.dynamic[,ar]-rm)  
    }
  }
  for(r in apars){
    beta<-lm(as.formula(paste0(r,"~",ar)),data=gm.dynamic)$coefficients[2]
    gm.dynamic[,paste0(r,"_adj_",ar)]<-gm.dynamic[,r]-beta*(gm.dynamic[,ar]-rm)  
  }
}

in_regs<-c(dfD$ROIName)

dfVol_L<-pivot_longer(gm.dynamic[,c("ID",paste0(in_regs,"_Left_adj_EstimatedTotalIntraCranialVol"),paste0(in_regs,"_Right_adj_EstimatedTotalIntraCranialVol"))],
                      all_of(c(paste0(in_regs,"_Left_adj_EstimatedTotalIntraCranialVol"),paste0(in_regs,"_Right_adj_EstimatedTotalIntraCranialVol"))),names_to = c("region"),values_to = "Vol") %>%
  mutate(hemi=ifelse(grepl("_Left",region),"Left","Right"),region=gsub("_Left|_Right|_adj_EstimatedTotalIntraCranialVol","",region))

in_regs<-apars
dfVol_L2<-pivot_longer(gm.dynamic[,c("ID",paste0(in_regs,"_adj_EstimatedTotalIntraCranialVol"))],
                      all_of(c(paste0(in_regs,"_adj_EstimatedTotalIntraCranialVol"))),names_to = c("region"),values_to = "Vol") %>%
  mutate(hemi="Bilateral",region=gsub("_Left|_Right|_adj_EstimatedTotalIntraCranialVol","",region))

dfVol_L<-rbind(dfVol_L,dfVol_L2)

dfVol_L<-merge(dfVol_L,dfC[,c("ID","Age","AgeGroup","Sex")])

# ggboxplot(subset(dfVol_L,(region=="TotalVentVol") & (hemi=="Bilateral")),"AgeGroup","Vol",color = "Sex")

c<-ddply(dfVol_L,.(region,hemi,AgeGroup,Sex),summarise,
             outlVol=outlierfun(Vol,ID)         
)

rownames(c)<-paste0(c$region,"_",c$hemi,"_",c$AgeGroup,"_",c$Sex)

##### Don't look for outliers in ""
for(r in setdiff(unique(c$region),c())){
  for(h in unique(c$hemi)){
    for(a in unique(c$AgeGroup)){
      for(s in unique(c$Sex)){
        otl<-unlist(strsplit(c[paste0(r,"_",h,"_",a,"_",s),"outlVol"],split = "\\|"))
        if(length(otl)>=1){
          print(sprintf("%s %s %s %s: %i outliers found",r,h,a,s,length(unique(otl))))
          dfVol_L[intersect(which(dfVol_L$region==r & dfVol_L$hemi==h & dfVol_L$Sex==s),which(dfVol_L$ID %in% unique(otl))),c("Vol")]<-NA
        }
      }
    }    
  }
}


# ########## THIS MAKES GAM FITS for visualization, not needed in analysis
# ggplot(dfVol_L,aes(Age,Vol))+ #Frontal_adj_EstimatedTotalIntraCranialVol
#   geom_point()+
#   geom_smooth(method="gam",formula = y ~ s(x, bs = "cs"))+
#   theme_bw()+
#   facet_wrap(~region+hemi,scales = "free")


dfWdynGMadj<-pivot_wider(dfVol_L,names_from = c("region","hemi"),values_from = c("Vol"),id_cols = c("ID")) %>% mutate(Study="DYN")
dfWdynGMadj<-merge(dfWdynGMadj,dfC[,c("ID","Age","AgeGroup","Sex")],by="ID")
dfWdynGMadj<-dfWdynGMadj %>% dplyr::select(-c(grep("Cerebellum",colnames(dfWdynGMadj))))


########################################################################################
########################################################################################
###################### BEGINNING OF COMBINED
########################################################################################
########################################################################################
########### MAKE AGGREGATE BP DATA
dfBPcomp<-dfWdyn

#### CALCULATE Bilateral average
for(inp in dfD$ROIName){
  dfBPcomp[,paste0("BP_",inp,"_Bilateral")]<-rowSums(dfBPcomp[,paste0("BP_",inp,"_",c("Left","Right"))]*dfBPcomp[,paste0("Vol_",inp,"_",c("Left","Right"))],na.rm = T)/rowSums(dfBPcomp[,paste0("Vol_",inp,"_",c("Left","Right"))],na.rm = T)
  dfBPcomp[,paste0("Vol_",inp,"_Bilateral")]<-rowSums(dfBPcomp[,paste0("Vol_",inp,"_",c("Left","Right"))],na.rm = T)
}


###### DEFINE AGGREGATES ACCORDING TO LOBES AND K-means clustering
dfD<-read.xlsx("./RegNamesGreve.xlsx")
dfD2<-read.xlsx("./Kmeans_3cl_dpfc_Oct2021.xlsx")

Aggs<-setdiff(unique(dfD2[,"region"]),c(NA))
for(inp in Aggs){
  regl<-dfD2[which(dfD2$region==inp),"X1"]
  for(hemi in c("Left","Right","Bilateral")){
    if(length(regl)>1){
      dfBPcomp[,paste0("BP_",inp,"_",hemi)]<-rowSums(dfBPcomp[,paste0("BP_",regl,"_",hemi)]*dfBPcomp[,paste0("Vol_",regl,"_",hemi)],na.rm = T)/rowSums(dfBPcomp[,paste0("Vol_",regl,"_",hemi)],na.rm = T)
      dfBPcomp[,paste0("Vol_",inp,"_",hemi)]<-rowSums(dfBPcomp[,paste0("Vol_",regl,"_",hemi)],na.rm = T)
    }
  }
}


########### MAKE COMBINED AND AGGREGATES MORPHOLOGICAL DATA
dfGMAdjcomp<-dfWdynGMadj
for(inp in dfD$ROIName){
  dfGMAdjcomp[,paste0(inp,"_Bilateral")]<-rowMeans(dfGMAdjcomp[,paste0(inp,"_",c("Left","Right"))],na.rm = T)
}

Aggs<-setdiff(unique(dfD2[,"region"]),c(NA))
for(inp in Aggs){
  regl<-dfD2[which(dfD2$region==inp),"X1"]
  for(hemi in c("Left","Right","Bilateral")){
    if(length(regl)>1){
      dfGMAdjcomp[,paste0(inp,"_",hemi)]<-rowMeans(dfGMAdjcomp[,paste0(regl,"_",hemi)],na.rm = T)
    }
  }
}

colnames(dfGMAdjcomp)[grep("ID|Sex|Study|AgeGroup|Age",colnames(dfGMAdjcomp),invert = T)]<-paste0("AdjVol_",colnames(dfGMAdjcomp)[grep("ID|Sex|Study|AgeGroup|Age",colnames(dfGMAdjcomp),invert = T)])


###################################################
###### COMBINE BP AND GM
###################################################

dfAllcomp<-merge(dfBPcomp,dfGMAdjcomp[,grep("Sex|Study|AgeGroup|Age",colnames(dfGMAdjcomp),invert = T)],by="ID")
dfAllcomp$AgeGroup2<-ifelse(dfAllcomp$Age<40,"Young",ifelse(dfAllcomp$Age<60,"MiddleAge","Old"))

dfAllcompSc<-dfAllcomp %>% mutate_each_(funs(scale),vars=colnames(dfAllcomp)[grep("BP|Vol|QSM",colnames(dfAllcomp))])

save(list=c("dfAllcomp","dfAllcompSc"),file=paste0("./Combined_SCH_DYN_OutliersCleaned_",format(Sys.Date(), "%B%Y"),".RData"))


########################################################################################
########################################################################################
###################### END OF SETTING UP COMBINED
########################################################################################
########################################################################################
