library(forestplot)

rm(list=ls())
load("./Combined_SCH_DYN_OutliersCleaned_August2023.RData")


in_regs<-c("Caudate","Putamen","Accumbens_area","DLPFC","Central","Operculum_ACC","Posterior")
dfl<-
  dfAllcomp %>% dplyr::select("ID","Age","Sex","Study",paste0("BP_",in_regs,"_Bilateral")) %>%
  mutate(AgeGroup2=factor(ifelse(Age<40,"20-40","40-80"))) %>% pivot_longer(cols = starts_with("BP_"),names_to="region",values_to="BP") %>% 
  mutate(region=factor(gsub("BP_|_Bilateral","",region),levels = in_regs),ID=factor(ID))  

##########################
#########################
####### BOOTSTRAPPED PIECEWISE LINEAR ANALYSIS
##########################
#########################
calc_PDDB<-function(data, indices){
  ####### USE GLOBAL PARS TO DETERMINE CORRECTION
  mod.lm<-lm(BP~Age*AgeGroup2,data=subset(data,!is.na(BP))[indices,])
  lp<-predict(mod.lm,newdata=list(Age=AgeCur,AgeGroup2=AgeGroupCur))
  return ((lp[2]/lp[1]-1)/((AgeCur[2]-AgeCur[1])/10))
}
get_PDDCIB<-function(data,age1,age2){
  AgeCur<<-c(age1,age2)
  AgeGroupCur<<-c(ifelse(age2>40,'40-80','20-40'),ifelse(age2>40,'40-80','20-40'))
  t<-boot(data=data,calc_PDDB,R=500)
  # return(sprintf("%1.4f (%1.4f - %1.4f)",t$t0,boot.ci(t, type='bca', index = 1)$bca[4],boot.ci(t, type='bca', index = 1)$bca[5]))
  return(c(t$t0,boot.ci(t, type='bca', index = 1,conf=0.975)$bca[4],boot.ci(t, type='bca', index = 1,conf=0.975)$bca[5]))
}



citab0<-dfl %>% filter(region %in% in_regs,Study=="DYN") %>% ddply(.(region),summarise,Young=get_PDDCIB(data=data.frame(Age=Age,BP=BP,AgeGroup2=AgeGroup2),20,40),
                                                                   Old=get_PDDCIB(data=data.frame(Age=Age,BP=BP,AgeGroup2=AgeGroup2),40,80))

###### Fig 1G: Young
citab<-citab0 %>% mutate(meas=rep(c("mean","lower","upper"),length(in_regs))) %>% pivot_longer(cols=c(,"Young","Old"),names_to = c("AgeSeg"),values_to=("PDD")) %>%
  pivot_wider(id_cols = c(region,AgeSeg),names_from=meas,values_from=PDD) %>% mutate(AgeSeg=factor(AgeSeg,levels=c("Young","Old"))) %>% filter(AgeSeg=="Young") %>%
  mutate(region=factor(gsub("_ACC"," & ACC",gsub("_area","",region)), levels=c("Operculum & ACC","Central","Caudate","Putamen","DLPFC","Posterior","Accumbens"))) %>%
  arrange(region)

citab <- mutate(citab,region=gsub("DL","",region))

tabletext <- cbind(c("Region",as.matrix(citab$region),"Summary"), c("Mean [97.5%CI]",paste0(round(100*citab$mean,digits = 1), ' [', round(100*citab$lower,digits = 1), ',', round(100*citab$upper,digits = 1), ']'),
                                                                    paste0(round(mean(100*citab$mean),digits = 1), ' [', round(mean(100*citab$lower),digits = 1), ',', round(mean(100*citab$upper),digits = 1), ']')))


pdf("DYNAMIC_Fig1G_YOUNG.pdf",width = 16/2.54,height = 8/2.54,onefile=F)
g<-forestplot(labeltext = tabletext, graph.pos = 3, mean= c(NA,100*citab$mean,mean(100*citab$mean)), lower = c(NA,100*citab$lower,mean(100*citab$lower)), upper = c(NA,100*citab$upper,mean(100*citab$upper)),
              is.summary = c(rep(TRUE, 1), rep(FALSE, length(in_regs)),rep(T,2)),
              vertices = TRUE, grid=T, graphwidth = unit(75,'mm'), xlab = '% difference per decade', 
              txt_gp = fpTxtGp(xlab = gpar(cex=.9), ticks = gpar(cex=.9), label=gpar(cex=.9)), #cex is font size
              xticks = c(0,-5,-10,-15),clip = c(-15,0))
g
print(g)
dev.off()



###### Fig 1G: Old
citab<-citab0 %>% mutate(meas=rep(c("mean","lower","upper"),length(in_regs))) %>% pivot_longer(cols=c(,"Young","Old"),names_to = c("AgeSeg"),values_to=("PDD")) %>%
  pivot_wider(id_cols = c(region,AgeSeg),names_from=meas,values_from=PDD) %>% mutate(AgeSeg=factor(AgeSeg,levels=c("Young","Old"))) %>% filter(AgeSeg=="Old") %>%
  mutate(region=factor(gsub("_ACC"," & ACC",gsub("_area","",region)), levels=c("Operculum & ACC","Central","Caudate","Putamen","DLPFC","Posterior","Accumbens"))) %>%
  arrange(region)

citab <- mutate(citab,region=gsub("DL","",region))

tabletext <- cbind(c("Region",as.matrix(citab$region),"Summary High","Summary Low"), c("Mean [97.5%CI]",paste0(round(100*citab$mean,digits = 1), ' [', round(100*citab$lower,digits = 1), ',', round(100*citab$upper,digits = 1), ']'),
                                                                                       paste0(round(mean(100*citab$mean[1:3]),digits = 1), ' [', round(mean(100*citab$lower[1:3]),digits = 1), ',', round(mean(100*citab$upper[1:3]),digits = 1), ']'),
                                                                                       paste0(round(mean(100*citab$mean[4:7]),digits = 1), ' [', round(mean(100*citab$lower[4:7]),digits = 1), ',', round(mean(100*citab$upper[4:7]),digits = 1), ']')))


pdf("DYNAMIC_Fig1G_OLD.pdf",width = 16/2.54,height = 8/2.54,onefile=F)
g<-forestplot(labeltext = tabletext, graph.pos = 3, mean= c(NA,100*citab$mean,mean(100*citab$mean[1:3]),mean(100*citab$mean[4:7])),lower = c(NA,100*citab$lower,mean(100*citab$lower[1:3]),mean(100*citab$lower[4:7])), upper = c(NA,100*citab$upper,mean(100*citab$upper[1:3]),mean(100*citab$upper[4:7])),
              is.summary = c(rep(TRUE, 1), rep(FALSE, length(in_regs)),rep(T,2)),
              vertices = TRUE, grid=T, graphwidth = unit(75,'mm'), xlab = '% difference per decade', 
              txt_gp = fpTxtGp(xlab = gpar(cex=.9), ticks = gpar(cex=.9), label=gpar(cex=.9)), #cex is font size
              xticks = c(0,-5,-10,-15),clip = c(-15,0))
g
print(g)
dev.off()

