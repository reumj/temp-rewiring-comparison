
################################################
# To do list: 
# 1A. Send out update email with new deadlines on getting temperture models to me, and set time table for getting supplemental text together as well.  

# 2. Set up equations to get trophic transfer efficiency  
# 3. Set up TL function so we can trace how energy moves through the web
# 4. Get circle plot function running for non EBS models 

# library(mizer) #only call specific mizer:: functions when needed to process 



setwd("C:/Users/Jonathan.Reum/Work/MIZER comparison/modelOutputs")



# EBS processing code 
d<- "C:/Users/Jonathan.Reum/Work/MIZER comparison/EBS-CE code/EBS mizerRcpp_TempWS/"

source(paste0(d, "MizerParams-classDynamBen like pel_v4.2.R"))
source(paste0(d,"MizerSim-class.R"))
source(paste0(d,"wrapper_functions.R"))
source(paste0(d,"summary_methods.R"))
source(paste0(d,"project_methods.R"))
source(paste0(d,"project_abc_funcs_v4.R"))

setwd("C:/Users/Jonathan.Reum/Work/MIZER comparison/EBS-CE code/EBS mizerRcpp_TempWS/")

source(paste0(d,"projectlongSetup_Rcpp_ScaledTEMP_v1.R"))
source(paste0(d,"project_RcppLoops_v2.R"))
source(paste0(d,"plots.R"))

library(devtools)
install_github("astaaudzi/mizer-rewiring", ref = "temp-model-comp")


library("mizerRewire")





#Load up simulations 

#EBS
load("EBS/modList_allSR_scenario_runs_V1.RData")


baltic_baserun<-readRDS("BalticSea/baltic_baserun.rds")
baltic_warming_all_ca<-readRDS("BalticSea/baltic_warming_all_ca.rds")
baltic_warming_all_eaint_ca<-readRDS("BalticSea/baltic_warming_all_eaint_ca.rds")
baltic_warming_all_eaint<-readRDS("BalticSea/baltic_warming_all_eaint.rds")
baltic_warming_all<-readRDS("BalticSea/baltic_warming_all.rds")
baltic_warming_int<-readRDS("BalticSea/baltic_warming_int.rds")
baltic_warming_mort<-readRDS("BalticSea/baltic_warming_mort.rds")
baltic_warming_met<-readRDS("BalticSea/baltic_warming_met.rds")


baltic<-list(baserun=baltic_baserun, 
             warming_all_ca= baltic_warming_all_ca, 
             warming_all_eaint_ca=baltic_warming_all_eaint_ca, 
             warming_all_eaint=baltic_warming_all_eaint, 
             warming_all=baltic_warming_all, 
             warming_int=baltic_warming_int, 
             warming_mort=baltic_warming_mort, 
             warming_met=baltic_warming_met)

# Tasmania
tasm_baserun<-readRDS("Tasmanian/tasm_baserun.rds")
tasm_warming_all_ca<-readRDS("Tasmanian/tasm_warming_all_ca.rds")
tasm_warming_all_eaint_ca<-readRDS("Tasmanian/tasm_warming_all_eaint_ca.rds")
tasm_warming_all_eaint<-readRDS("Tasmanian/tasm_warming_all_eaint.rds")
tasm_warming_all<-readRDS("Tasmanian/tasm_warming_all.rds")
tasm_warming_int<-readRDS("Tasmanian/tasm_warming_int.rds")
tasm_warming_mort<-readRDS("Tasmanian/tasm_warming_mort.rds")
tasm_warming_met<-readRDS("Tasmanian/tasm_warming_met.rds")

tasman<-list(baserun=tasm_baserun, 
             warming_all_ca= tasm_warming_all_ca, 
             warming_all_eaint_ca=tasm_warming_all_eaint_ca, 
             warming_all_eaint=tasm_warming_all_eaint, 
             warming_all=tasm_warming_all, 
             warming_int=tasm_warming_int, 
             warming_mort=tasm_warming_mort, 
             warming_met=tasm_warming_met)

#North Sea
ns_baserun<-readRDS("NorthSea/ns_baserun.rds")
ns_warming_all_ca<-readRDS("NorthSea/ns_warming_all_ca.rds")
ns_warming_all_eaint_ca<-readRDS("NorthSea/ns_warming_all_eaint_ca.rds")
ns_warming_all_eaint<-readRDS("NorthSea/ns_warming_all_eaint.rds")
ns_warming_all<-readRDS("NorthSea/ns_warming_all.rds")
ns_warming_int<-readRDS("NorthSea/ns_warming_int.rds")
ns_warming_mort<-readRDS("NorthSea/ns_warming_mort.rds")
ns_warming_met<-readRDS("NorthSea/ns_warming_met.rds")


northsea<-list(baserun=ns_baserun, 
             warming_all_ca= ns_warming_all_ca, 
             warming_all_eaint_ca=ns_warming_all_eaint_ca, 
             warming_all_eaint=ns_warming_all_eaint, 
             warming_all=ns_warming_all, 
             warming_int=ns_warming_int, 
             warming_mort=ns_warming_mort, 
             warming_met=ns_warming_met)


# SEAustralia


SEAustralia_baserun<-readRDS("SEAustralia/SEAustralia_baserun100.rds")
SEAustralia_warming_all_ca<-readRDS("SEAustralia/SEAustralia_warming_all_ca100.rds")
SEAustralia_warming_all_eaint_ca<-readRDS("SEAustralia/SEAustralia_warming_all_eaint_ca100.rds")
SEAustralia_warming_all_eaint<-readRDS("SEAustralia/SEAustralia_warming_all_eaint100.rds")
SEAustralia_warming_all<-readRDS("SEAustralia/SEAustralia_warming_all100.rds")
SEAustralia_warming_int<-readRDS("SEAustralia/SEAustralia_warming_int100.rds")
SEAustralia_warming_mort<-readRDS("SEAustralia/SEAustralia_warming_mort100.rds")
SEAustralia_warming_met<-readRDS("SEAustralia/SEAustralia_warming_met100.rds")


SEoz<-list(baserun=SEAustralia_baserun, 
               warming_all_ca= SEAustralia_warming_all_ca, 
               warming_all_eaint_ca=SEAustralia_warming_all_eaint_ca, 
               warming_all_eaint=SEAustralia_warming_all_eaint, 
               warming_all=SEAustralia_warming_all, 
               warming_int=SEAustralia_warming_int, 
               warming_mort=SEAustralia_warming_mort, 
               warming_met=SEAustralia_warming_met)

# Trait based model 

baserun<-readRDS("TraitBasedModel/tbm_baserun.rds")
warming_all_ca<-readRDS("TraitBasedModel/tbm_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/tbm_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/tbm_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/tbm_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/tbm_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/tbm_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/tbm_warming_met.rds")

traitmod<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                  warming_all_eaint=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)


#Diet arrays for trait based model 
baserun<-readRDS("TraitBasedModel/diet_baserun.rds")
warming_all_ca<-readRDS("TraitBasedModel/diet_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/diet_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/diet_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/diet_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/diet_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/diet_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/diet_warming_met.rds")

trait_DIET<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                 warming_all_eaint_ca=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)





allmods<- list(traitmod=traitmod, 
               SEoz=SEoz, 
               northsea=northsea, 
               tasman=tasman,
               baltic=baltic, 
               ebs_default=modList_all_SR[[1]],
               ebs_bh=modList_all_SR[[2]],
               ebs_ricker=modList_all_SR[[3]])


save(allmods, file="allmods.Rdata")

rm(list=ls())

######################################
# Load and process simulation models #
######################################


d<-"C:/Users/Jonathan.Reum/Work/MIZER comparison/EBS-CE code/EBS mizerRcpp_TempWS/"


source(paste0(d,"MizerParams-classDynamBen like pel_v4.2.R"))
source(paste0(d,"MizerSim-class.R"))
source(paste0(d,"wrapper_functions.R"))
source(paste0(d,"summary_methods.R"))
source(paste0(d,"project_methods.R"))
source(paste0(d,"project_abc_funcs_v4.R"))
source(paste0(d,"projectlongSetup_Rcpp_ScaledTEMP_v1.R"))
source(paste0(d,"project_RcppLoops_v2.R"))
source(paste0(d,"plots.R"))

library(data.table)



setwd("/Users/jonathanreum/Desktop/MIZER comparison/modelOutputs")

load("allmods.Rdata")

source("scenarios_summary_funcs.R") #loaded all the functions individually


##################################
# GET SSB, CATCHES AND MEAN SIZE #
##################################


#Non ebs 
out_vars<-lapply(allmods[1:5], function (x) { lapply(x, getSSB_Catch_MeanWt) })
#EBS
out_vars_ebs<-lapply(allmods[6:8], function (x) { lapply(x, getSSB_Catch_MeanWt_ebs) })

out_vars<- c(out_vars, out_vars_ebs)  #Include all the models together 

######################################
# Get RDD, RDI, rmax from all models #
######################################


out_trait<-list(traitmod=lapply(allmods[[1]], getRsummaryTrait))

out_r<-lapply(allmods[2:5], function (x) { lapply(x, getRsummary)})
out_rebs<-lapply(allmods[6:7], function (x) { lapply(x, getRsummary_ebs)})
                                                                                          
out_r<- c(out_trait, out_r, out_rebs)                                                                                    


################################
# Get age at maturation and M2 #
################################


traitmod<-lapply(allmods[1], function (x) { lapply(x, getAgeAtMatM2, traitmod=TRUE, camiMod=FALSE) })
SEoz<-lapply(allmods[2], function (x) { lapply(x, getAgeAtMatM2, traitmod=FALSE, camiMod=TRUE) })
mods<-lapply(allmods[3:5], function (x) { lapply(x, getAgeAtMatM2, traitmod=FALSE, camiMod=FALSE, EBS=FALSE) })

EBS<-lapply(allmods[6:8], function (x) { lapply(x, getAgeAtMatM2, traitmod=FALSE, camiMod=FALSE, EBS=TRUE) })

out_matM2<-c(traitmod, SEoz, mods, EBS)
                         







#Species aveMatwt, aveMatAge, aveAge_50
out_mat<-lapply(modList_all_SR, function (x) { lapply(x, getMeanSSBAge) })


#Food web stats, Takes a verly long time (5 min with 3 * 8 models), so maybe parallel this one. 
out_aveFW<-lapply(modList_all_SR, function (x) { lapply(x, getFoodWebStats) })






#Prep ssb, catch, and age responses, here x i
relFunc<-function(xlist) { 
  basemod<- xlist[[1]]
  
  return(lapply(xlist, function(x=xlist,  base=basemod) {as.matrix(x / base) }))

}


#Get the relative SSB + CATCH variables 
relvars<-lapply(out_vars, relFunc)

#Get the relative AGE variables 
relmat<-lapply(out_mat, function (x) { lapply(x, relFunc, basemod=out_mat[[1]][[1]]) })

#Get the effect of ca 


relvars<-lapply(relvars,  get_caEff) #Calculate difference  

relvars<-melt(relvars)
colnames(relvars)<-c("species", "variable", "value", "scen","mod")


#Need to repeat for relmat and FW

relmat<-lapply(relmat,  get_caEff) #Calculate difference  

relmat<-melt(relmat)
colnames(relmat)<-c("species", "variable", "value", "scen","rec")


############################################
# Plot up the effect of Int, met, and mort # 
############################################


#Select data set
datplot<- relvars[relvars$scen %in% c("warming_met", "warming_int", "warming_mort"), ]

#datplot<- relmat[relmat$scen %in% c("warming_met", "warming_int", "warming_mort"), ]

datplot$mod<-factor(datplot$mod, 
                    levels=c("traitmod" , "baltic" ,  "northsea" ,   "SEoz" ,"tasman", "ebs_bh", "ebs_default" ,"ebs_ricker")) 



ggplot(data=datplot) +
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
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Percent change") + 
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "skyblue2",
                              "skyblue2",
                              "skyblue2"))

limits=c(-150,150)
###########################################################
# Plot up the effect of applying the scaled coefficient ### 
###########################################################

#Select data set 
#datplot<-relmat

datplot<-relvars
datplot<- datplot[datplot$scen %in% c("warming_all", "dif_all", "warming_all_eaint","dif_eaint"), ]

datplot$scaledif<-"no"
datplot$scaledif[datplot$scen %in% c("dif_all","dif_eaint")]<-"yes"

datplot$plotvar<- I(100*(datplot$value-1))

datplot$plotvar[datplot$scaledif=="yes"]<- I(100*(datplot$value[datplot$scaledif=="yes"]))

datplot$mod<-factor(datplot$mod, 
                    levels=c("traitmod" , "baltic" ,  "northsea" ,   "SEoz" ,"tasman", "ebs_bh", "ebs_default" ,"ebs_ricker")) 

datplot$scen<-factor(datplot$scen, 
                    levels=c("warming_all" , "dif_all" ,  "warming_all_eaint" , "dif_eaint")) 


ggplot(data=datplot) +
  facet_grid(variable~scen, scales="free_y") +
  geom_line(aes(y=plotvar, x=mod, group=species), 
            color="grey", alpha=.5) + 
  geom_line(data=subset(datplot, species=="community"), 
            aes(y=plotvar, x=mod, group=species), lwd=1, color="grey", alpha=1) +
  geom_point(aes(y=plotvar, x=mod, color=mod), alpha=.6, pch=1 ) + 
  geom_point(data=subset(datplot, species=="community"), 
             aes(y=plotvar, x=mod, color=mod), pch=19, cex=3 ) +
  theme_bw() + 
  theme(axis.text=element_text(size=12), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Percent change") + 
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("gold",
                              "orangered", 
                              "brown",
                              "coral",
                              "seagreen",
                              "skyblue2",
                              "skyblue2",
                              "skyblue2"))












