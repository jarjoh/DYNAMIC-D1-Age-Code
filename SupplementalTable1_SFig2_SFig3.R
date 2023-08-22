rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")

dfD<-read.xlsx("./RegNamesGreve.xlsx")

in_regs<-dfD$ROIName
dfl<- dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Left"),paste0("BP_",in_regs,"_Right")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = starts_with("BP_"),names_to="region",values_to="BP") %>% 
  mutate(hemi=ifelse(grepl("_Left",region),"lh","rh")) %>%
  mutate(region=factor(gsub("BP_|_Left|_Right","",region),levels = in_regs),ID=factor(ID))  %>%
  merge(.,dfD[,c("ROIName","Long_Name","RepOrd")],by.x="region",by.y="ROIName") %>% arrange(region) %>%
  mutate(Long_Name=factor(Long_Name,levels = unique(Long_Name)))



####%%%#%#%#%# 
### GENERATE SUPPLEMENTAL TABLE 1
Out<-data.frame()
for(r in in_regs){
  for(h in c("lh","rh")){
    dat<-dfl %>% filter(Study=="DYN",region == r, hemi == h, !is.na(BP))
    m<-gam(BP~s(Age,bs='tp',k=6),data=dat,method = "REML")
    s<-summary(m)  
    Out[r,paste0("r2_",h)]<-s$r.sq
    Out[r,paste0("p_",h)]<-s$s.table[1,"p-value"]
    l<-predict.gam(m,newdata = list(Age=c(20,80)))
    Out[r,paste0("D_",h)]<-l[2]/l[1]-1    
  }
}

Out %>% mutate(region=factor(rownames(Out),levels = in_regs)) %>% merge(.,dfD[,c("ROIName","Region","Long_Name","RepOrd")],by.x="region",by.y="ROIName") %>%
  arrange(RepOrd) %>%
  adjust_pvalue(p.col = "p_lh",method = "bonferroni") %>% add_significance(p.col = "p_lh.adj") %>% adjust_pvalue(p.col = "p_rh",method = "bonferroni") %>% add_significance(p.col = "p_rh.adj") %>%
  mutate(lh=sprintf("%1.2f%s",r2_lh,p_lh.adj.signif),rh=sprintf("%1.2f%s",r2_rh,p_rh.adj.signif)) %>%
  mutate(lhd=sprintf("%1.2f (%1.2f)",100*D_lh,100*D_lh/6),rhd=sprintf("%1.2f (%1.2f)",100*D_rh,100*D_rh/6)) %>%
  dplyr::select(Region,Long_Name,lh,rh,lhd,rhd) %>% write.xlsx("DYNAMIC_SupplementalTable1.xlsx")

##### (Note that the table was manually edited to the final format)

######%#%#%#%#%#%
#%#%#%#%# Supplemental Fig 2
dfl %>% filter(Study=="DYN",hemi=="lh",!(region %in% c("Amygdala","Pallidum"))) %>% 
  arrange(RepOrd) %>%
  ggplot(aes(Age,BP))+geom_point(size=0.05,color="gray")+
  geom_smooth(method="gam",formula = y~s(x,bs='tp',k=8),se=T,alpha=0.2,size=0.5,color="blue")+
  facet_wrap(~region,ncol = 5,scales="free_y")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.1),axis.ticks = element_line(size=0.1),panel.border = element_blank(),panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),axis.text = element_text(size=6),
        axis.title = element_text(size=8),strip.text = element_blank(),
        legend.key.size = unit(.1,"cm"),strip.background = element_blank(), strip.text.x = element_text(margin=margin(b=0.1),size=rel(.5), vjust = .2),
        panel.spacing.y = unit(-.1,"lines"),panel.spacing.x = unit(-.5,"lines"))#+ 

ggsave("DYNAMIC_SupplementalFig2.pdf",width=25,height = 30,units = "cm")


######### Supplemental Fig 3
dfl<-dfl %>% mutate(reg=paste0(region,'_',hemi))
in_regs<- dfl %>% dplyr::select(reg) %>% unique()
in_regs<-in_regs$reg

n<-length(in_regs)
tab1<-data.frame(PD=vector(length = n))
rownames(tab1)<-in_regs
tab1[,]<-NA
for(r in in_regs){
  mod.gam1<-mgcv::gam(BP~1+s(Age,k=4,bs='tp'),data=subset(dfl,reg==r & !is.na(BP)))
  if(summary(mod.gam1)$s.table[1,"p-value"] < 0.05/length(in_regs)){
    lp<-predict.gam(mod.gam1,list(Age=c(20,80)))
    tab1[r,"PD"]<-lp[2]/lp[1]-1
  }
}

tab1 %>% mutate(region=rownames(.)) %>% mutate(Hemisphere=ifelse(grepl("lh",region),"Left","Right")) %>% 
  mutate(PD=PD/6) %>%
  mutate(region=gsub("_lh|_rh","",region)) %>% 
  pivot_wider(id_cols=region,names_from=c(Hemisphere),values_from=c(PD)) %>% 
  ggscatter("Left","Right",add = "reg.line",  # Add regressin line
          add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = T, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "pearson", label.x = -.08, label.sep = "\n"))+
  ylab("Percent BP difference per decade\n (Right Hemisphere)")+xlab("Percent BP difference per decade\n (Left Hemisphere)")+
  xlim(c(-0.08,-0.02))+
  ylim(c(-0.08,-0.02))+
  theme(axis.title = element_text(size=9))
ggsave("DYNAMIC_SupplementalFig3.pdf",width = 8,height = 8,units = "cm")
