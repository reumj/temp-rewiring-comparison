

###################################
# Here I take the list of outputs generated in Pre-processSims_TempComp.R and 
# get the data organized for plotting. You should be able to step through the
# code and generate figures in succession. Since all the figure are 
# still being tweaked on the fly, I haven't bothered turning
# this code into specific functions. 

library(ggplot2)


#Load up lists of output summary stats. THe structure is L1: Regional model, L2: Scenario Sim 
load("ModelComp_SSB_Catch_MeanSize.Rdata")
load("ModelComp_RDD_RDI.Rdata")
load("ModelComp_M2_MeanAge.Rdata")




# Function to calculate variables relative baseline run. Some model out 
# are total biomass/ region while others are g/m3. 

relFunc<-function(xlist) { 
  basemod<- xlist[[1]]
  
  return(lapply(xlist, function(x=xlist,  base=basemod) {


    mat<- as.matrix(x / base) 
      rownames(mat)<- rownames(base)
      return(mat)
  }))

}


# unction to calculate RDD variables relative baseline
relFuncRDD<-function(xlist) { 
  basemod<- xlist[[1]]
  
  return(lapply(xlist, function(x=xlist,  base=basemod) {


    mat<- as.matrix(x[,"rdd", drop=FALSE] / base[,"rdd",drop=FALSE])
    mat_rr<- as.matrix(x[,"rdi", drop=FALSE] / x[,"rdd", drop=FALSE])
    colnames(mat)<-"relrdd"
    colnames(mat_rr)<-"rratio"
    x_vars<- x[,c("erepro", "beta","sigma","w_inf"), drop=FALSE]
    mat<- cbind(mat,mat_rr, x_vars)

      rownames(mat)<- rownames(base)
      return(mat)
  }))

}


#Get the relative SSB + CATCH variables 
relvars<-lapply(out_vars, relFunc)

#Get the relative AGE variables 
relmat<-lapply(out_mat, relFunc)

#Get the relative RDD variables 
relrdd<-lapply(out_r, relFuncRDD)

#Get the effect of ca 
#relvars<-lapply(relvars,  get_caEff) #Calculate difference  

relvars<-melt(relvars)
colnames(relvars)<-c("species", "variable", "value", "scen","mod")


relmat<-melt(relmat)
colnames(relmat)<-c("species", "variable", "value", "scen","mod")


relrdd<-melt(relrdd)
colnames(relrdd)<-c("species", "variable", "value", "scen","mod")




############################################
# Plot up the effect of Int, met, and mort # 
############################################


#Select sims to view 
datplot<- relvars[relvars$scen %in% c("warming_met", "warming_int", "warming_mort", "warming_all"), ]

#Set order to plot scenarios
datplot$scen<- factor(datplot$scen, levels = c( "warming_int", "warming_met","warming_mort", "warming_all"))

#Get rid of the EBS recruitment variants - they probably distract from the main message of the paper
datplot<- datplot[ !datplot$mod%in%c("ebs_bh", "ebs_ricker"), ]

#rewmoved EBS variants
datplot$mod<-factor(datplot$mod, 
                    levels=c("trait500" ,"trait2500", "trait10000", "baltic" ,  "northsea" ,  
                     "SEoz" ,"tasman", "Hawaii","ebs_default" )) 



ggplot(data=subset(datplot)) +
  facet_grid(variable~scen, scales="free_y") +
  geom_line(aes(y=I(100*(value-1)), x=mod, group=species), 
            color="grey", alpha=.5) + 
  geom_line(data=subset(datplot, species=="community"), 
            aes(y=I(100*(value-1)), x=mod, group=species), lwd=1, color="grey", alpha=1) +
  geom_point(aes(y=I(100*(value-1)), x=mod, color=mod), alpha=.6, pch=1 ) + 
  geom_point(data=subset(datplot, species=="community"), 
             aes(y=I(100*(value-1)), x=mod, color=mod), pch=19, cex=3 ) +
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Percent change") + 
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2"))



###########################################################
# Plot up the effect of applying the scaled coefficient ### 
###########################################################

#Select data set 
#datplot<-relmat

datplot<-relvars

datplot<- datplot[ !datplot$mod%in%c("ebs_bh", "ebs_ricker"), ]

datplot<- datplot[datplot$scen %in% c("warming_all", "warming_all_eaint","warming_all_ca", "warming_all_eaint_ca"), ]

datplot$scen<-factor(datplot$scen, 
                    levels=c("warming_all" , "warming_all_eaint","warming_all_ca", "warming_all_eaint_ca")) 


datplot$mod<-factor(datplot$mod, 
                    levels=c("trait500" ,"trait2500", "trait10000", "baltic" ,  "northsea" ,  
                     "SEoz" ,"tasman", "Hawaii","ebs_default" )) 




ggplot(data=datplot) +
  facet_wrap(variable~scen, scales="free_y") +
  geom_line(aes(y=(value-1)*100, x=mod, group=species), 
            color="grey", alpha=.5) + 
  geom_line(data=subset(datplot, species=="community"), 
            aes(y=(value-1)*100, x=mod, group=species), lwd=1, color="grey", alpha=1) +
  geom_point(aes(y=(value-1)*100, x=mod, color=mod), alpha=.6, pch=1 ) + 
  geom_point(data=subset(datplot, species=="community"), 
             aes(y=(value-1)*100, x=mod, color=mod), pch=19, cex=3 ) +
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Percent change") + 
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2"))




#############################
# Plot up M2 and age-at-mat #
#############################



#Select data set
datplot<- relmat[relmat$scen %in% c("warming_met", "warming_int", "warming_mort", "warming_all"), ]


datplot$scen<- factor(datplot$scen, levels = c( "warming_int", "warming_met","warming_mort", "warming_all"))

#datplot<- relmat[relmat$scen %in% c("warming_met", "warming_int", "warming_mort"), ]

datplot<- datplot[ !datplot$mod%in%c("ebs_bh", "ebs_ricker"), ]

#rewmoved EBS variants
datplot$mod<-factor(datplot$mod, 
                    levels=c("trait500" ,"trait2500", "trait10000", "baltic" ,  "northsea" ,  
                     "SEoz" ,"tasman", "Hawaii","ebs_default" )) 



ggplot(data=subset(datplot)) +
  facet_grid(variable~scen, scales="free_y") +
  geom_line(aes(y=I(100*(value-1)), x=mod, group=species), 
            color="grey", alpha=.5) + 
  geom_line(data=subset(datplot, species=="community"), 
            aes(y=I(100*(value-1)), x=mod, group=species), lwd=1, color="grey", alpha=1) +
  geom_point(aes(y=I(100*(value-1)), x=mod, color=mod), alpha=.6, pch=1 ) + 
  geom_point(data=subset(datplot, species=="community"), 
             aes(y=I(100*(value-1)), x=mod, color=mod), pch=19, cex=3 ) +
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Percent change") + 
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2"))


########### Scaling effect



datplot<-relmat

datplot<- datplot[ !datplot$mod%in%c("ebs_bh", "ebs_ricker"), ]

datplot<- datplot[datplot$scen %in% c("warming_all", "warming_all_eaint","warming_all_ca", "warming_all_eaint_ca"), ]

datplot$scen<-factor(datplot$scen, 
                    levels=c("warming_all" , "warming_all_eaint","warming_all_ca", "warming_all_eaint_ca")) 


datplot$mod<-factor(datplot$mod, 
                    levels=c("trait500" ,"trait2500", "trait10000", "baltic" ,  "northsea" ,  
                     "SEoz" ,"tasman", "Hawaii","ebs_default" )) 




ggplot(data=datplot) +
  facet_grid(variable~scen, scales="free_y") +
  geom_line(aes(y=(value-1)*100, x=mod, group=species), 
            color="grey", alpha=.5) + 
  geom_line(data=subset(datplot, species=="community"), 
            aes(y=(value-1)*100, x=mod, group=species), lwd=1, color="grey", alpha=1) +
  geom_point(aes(y=(value-1)*100, x=mod, color=mod), alpha=.6, pch=1 ) + 
  geom_point(data=subset(datplot, species=="community"), 
             aes(y=(value-1)*100, x=mod, color=mod), pch=19, cex=3 ) +
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(name = "Percent change") + 
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2"))








####################################################################
# Are relative changes in MeanWt, SSB, and Catches related to changes
# in relative predation rate, recruitment, or growth rate? 
#####################################################################
rel_all<- rbind(relvars, relmat, relrdd)

rel_wide<- dcast(data=rel_all, species + scen + mod ~ variable, value.var = "value", fun.aggregate=mean)



#Select data set
datplot<- rel_wide[!relmat$scen %in% c("baseline"), ]


datplot$scen<- factor(datplot$scen, 
              levels = c( "warming_int", "warming_met","warming_mort", "warming_all","warming_all_ca",
                "warming_all_eaint", "warming_all_eaint_ca"))

#datplot<- relmat[relmat$scen %in% c("warming_met", "warming_int", "warming_mort"), ]

datplot<- datplot[ !datplot$mod%in%c("ebs_bh", "ebs_ricker"), ]

datplot$mod<-factor(datplot$mod, 
                    levels=c("trait500" ,"trait2500", "trait10000", "baltic" ,  "northsea" ,  
                     "SEoz" ,"tasman", "Hawaii","ebs_default" )) 



ggplot(datplot) + geom_point(aes(y=ssb, x=m2, col=mod))+
 facet_wrap(~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=2) + 
  geom_hline(yintercept=1, lty=2) + 
  geom_abline(intercept=0, slope=1, lty=2) +
  geom_smooth(method='lm',aes(y=ssb, x=m2), col="black", lty=1) + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "Percent change SSB") + 
  scale_x_continuous(name = "Percent change Predation mortality rate")
  


# 

ggplot(datplot) + geom_point(aes(y=relrdd, x=agemat, col=mod))+
 facet_wrap(~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=2) + 
  geom_hline(yintercept=1, lty=2) + 
  geom_abline(intercept=0, slope=1, lty=2) +
  geom_smooth(method='lm',aes(y=relrdd, x=agemat), col="black", lty=1) + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "Percent change Mean wt") + 
  scale_x_continuous(name = "Percent change Age at 50% maturation")


# m2 versus recruitment, circles scaled by change in SSB 

ggplot(datplot) + geom_point(aes(y=relrdd, x=m2, col=mod, size=I(ssb^2)), pch=1)+
 facet_wrap(~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=2) + 
  geom_hline(yintercept=1, lty=2) + 
  geom_abline(intercept=0, slope=1, lty=2) +
  geom_smooth(method='lm',aes(y=relrdd, x=m2), col="black", lty=1) + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "Percent change Recruitment") + 
  scale_x_continuous(name = "Percent change Predation Mortality rate")


#Explaining changes in SSB 



ggplot(datplot) + geom_point(aes(y=ssb, x= sigma, col=mod), pch=1)+
 facet_wrap(~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=2) + 
  geom_hline(yintercept=1, lty=2) + 
  geom_abline(intercept=0, slope=1, lty=2) +
  geom_smooth(method='lm',aes(y=ssb, x= sigma), col="black", lty=1) + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "Percent change SSB") + 
  scale_x_continuous(name = "sigma")

#################################################################################
# Look at patterns witin a scenario: compare ssb with predation mort and relRDD #
#################################################################################


datplot_sub<- datplot
datplot_sub<-  datplot_sub[datplot_sub$scen%in%c("warming_int", "warming_met", "warming_mort", "warming_all"), ]


#datplot_sub<-  datplot_sub[datplot_sub$scen%in%c("warming_all", "warming_all_ca", "warming_all_eaint", "warming_all_eaint_ca"), ]


datplot_sub$scen<-factor(datplot_sub$scen)
datplot_sub$mod<-factor(datplot_sub$mod)


ggplot(datplot_sub) + geom_point(aes(y=ssb, x= m2, col=mod, size=w_inf^.5), pch=19)+
 facet_grid(scen~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=1, col="grey") + 
  geom_hline(yintercept=1, lty=1, col="grey") + 
  geom_abline(intercept=0, slope=1, lty=2, col="grey") +
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "Percent change SSB") + 
  scale_x_continuous(name = "Percent change Predation Mortality Rate")



ggplot(subset(datplot_sub, mod=="trait500" | mod=="trait2500" |mod=="trait10000" )) + 
geom_point(aes(y=ssb, x= m2), pch=1)+
 facet_grid(scen~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=1, col="grey") + 
  geom_hline(yintercept=1, lty=1, col="grey") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "% change SSB") + 
  scale_x_continuous(name = "% change predation mort", trans="log10")

  geom_abline(intercept=0, slope=1, lty=2, col="grey") 



ggplot(subset(datplot_sub, mod=="trait500" | mod=="trait2500" |mod=="trait10000" )) + 
geom_point(aes(y=relrdd, x= w_inf), pch=1)+
 facet_grid(scen~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=1, col="grey") + 
  geom_hline(yintercept=1, lty=1, col="grey") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "% change SSB") + 
  scale_x_continuous(name = "winf", trans="log10")



ggplot(subset(datplot_sub, mod=="trait500" | mod=="trait2500" |mod=="trait10000" )) + 
geom_point(aes(y=ssb, x= relrdd, size=log10(w_inf)), pch=1)+
 facet_grid(scen~mod, scales="fixed") +
  scale_color_manual(values=c("gold",
                              "gold",
                              "gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "purple",
                              "skyblue2")) + 
  geom_vline(xintercept=1, lty=1, col="grey") + 
  geom_hline(yintercept=1, lty=1, col="grey") + 
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name = "ssb") + 
  scale_x_continuous(name = "relrdd")




