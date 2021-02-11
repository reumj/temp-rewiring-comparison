################################################
# Here's how I processed all the models
#   Three models require mizerRewire - TAS, SEoz, Baltic.
#   TBM, Hawaii, NS reqiore latest release of mizer
#   EBS requires EBS-specific code. Basically, if you want run the preprocessing yourself, I would just skip the EBS models as that code is hairy
#   Functions for extracting various outputs are in scenarios_summary_funcs.R


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
#install_github("astaaudzi/mizer-rewiring", ref = "temp-model-comp")


library("mizerRewire")
library("mizer")





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
ns_baserun<-readRDS("NorthSea_new/nsSims/ns_baserun.rds")
ns_warming_all_ca<-readRDS("NorthSea_new/nsSims/ns_warming_all_ca.rds")
ns_warming_all_eaint_ca<-readRDS("NorthSea_new/nsSims/ns_warming_all_eaint_ca.rds")
ns_warming_all_eaint<-readRDS("NorthSea_new/nsSims/ns_warming_all_eaint.rds")
ns_warming_all<-readRDS("NorthSea_new/nsSims/ns_warming_all.rds")
ns_warming_int<-readRDS("NorthSea_new/nsSims/ns_warming_int.rds")
ns_warming_mort<-readRDS("NorthSea_new/nsSims/ns_warming_mort.rds")
ns_warming_met<-readRDS("NorthSea_new/nsSims/ns_warming_met.rds")


northsea<-list(baserun=ns_baserun, 
             warming_all_ca= ns_warming_all_ca, 
             warming_all_eaint_ca=ns_warming_all_eaint_ca, 
             warming_all_eaint=ns_warming_all_eaint, 
             warming_all=ns_warming_all, 
             warming_int=ns_warming_int, 
             warming_mort=ns_warming_mort, 
             warming_met=ns_warming_met)



#Diet arrays for NS model 
baserun<-readRDS("NorthSea_new/nsSims/diet_baserun.rds")
warming_all_ca<-readRDS("NorthSea_new/nsSims/diet_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("NorthSea_new/nsSims/diet_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("NorthSea_new/nsSims/diet_warming_all_eaint.rds")
warming_all<-readRDS("NorthSea_new/nsSims/diet_warming_all.rds")
warming_int<-readRDS("NorthSea_new/nsSims/diet_warming_int.rds")
warming_mort<-readRDS("NorthSea_new/nsSims/diet_warming_mort.rds")
warming_met<-readRDS("NorthSea_new/nsSims/diet_warming_met.rds")

ns_DIET<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                 warming_all_eaint_ca=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)



# SEAustralia


SEAustralia_baserun<-readRDS("SEAustralia/SEAustralia_baserun.rds")
SEAustralia_warming_all_ca<-readRDS("SEAustralia/SEAustralia_warming_all_ca.rds")
SEAustralia_warming_all_eaint_ca<-readRDS("SEAustralia/SEAustralia_warming_all_eaint_ca.rds")
SEAustralia_warming_all_eaint<-readRDS("SEAustralia/SEAustralia_warming_all_eaint.rds")
SEAustralia_warming_all<-readRDS("SEAustralia/SEAustralia_warming_all.rds")
SEAustralia_warming_int<-readRDS("SEAustralia/SEAustralia_warming_int.rds")
SEAustralia_warming_mort<-readRDS("SEAustralia/SEAustralia_warming_mort.rds")
SEAustralia_warming_met<-readRDS("SEAustralia/SEAustralia_warming_met.rds")


SEoz<-list(baserun=SEAustralia_baserun, 
               warming_all_ca= SEAustralia_warming_all_ca, 
               warming_all_eaint_ca=SEAustralia_warming_all_eaint_ca, 
               warming_all_eaint=SEAustralia_warming_all_eaint, 
               warming_all=SEAustralia_warming_all, 
               warming_int=SEAustralia_warming_int, 
               warming_mort=SEAustralia_warming_mort, 
               warming_met=SEAustralia_warming_met)

# Trait based model beta500, beta2500, beta10000; Diet matrix is in separate batch of files 

baserun<-readRDS("TraitBasedModel/beta500/tbm_baseRun.rds")
warming_all_ca<-readRDS("TraitBasedModel/beta500/tbm_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/beta500/tbm_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/beta500/tbm_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/beta500/tbm_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/beta500/tbm_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/beta500/tbm_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/beta500/tbm_warming_met.rds")

trait500<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                  warming_all_eaint=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)



# Trait based model beta500, beta2500, beta10000; Diet matrix is in separate batch of files 

baserun<-readRDS("TraitBasedModel/beta2500/tbm_baseRun.rds")
warming_all_ca<-readRDS("TraitBasedModel/beta2500/tbm_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/beta2500/tbm_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/beta2500/tbm_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/beta2500/tbm_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/beta2500/tbm_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/beta2500/tbm_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/beta2500/tbm_warming_met.rds")

trait2500<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                  warming_all_eaint=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)




# Trait based model beta500, beta2500, beta10000; Diet matrix is in separate batch of files 

baserun<-readRDS("TraitBasedModel/beta10000/tbm_baseRun.rds")
warming_all_ca<-readRDS("TraitBasedModel/beta10000/tbm_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/beta10000/tbm_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/beta10000/tbm_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/beta10000/tbm_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/beta10000/tbm_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/beta10000/tbm_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/beta10000/tbm_warming_met.rds")

trait10000<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                  warming_all_eaint=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)


#Diet arrays for trait based model 
baserun<-readRDS("TraitBasedModel/beta500/diet_baserun.rds")
warming_all_ca<-readRDS("TraitBasedModel/beta500/diet_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/beta500/diet_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/beta500/diet_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/beta500/diet_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/beta500/diet_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/beta500/diet_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/beta500/diet_warming_met.rds")

trait_DIET500<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                 warming_all_eaint_ca=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)



#Diet arrays for trait based model 
baserun<-readRDS("TraitBasedModel/beta2500/diet_baserun.rds")
warming_all_ca<-readRDS("TraitBasedModel/beta2500/diet_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/beta2500/diet_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/beta2500/diet_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/beta2500/diet_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/beta2500/diet_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/beta2500/diet_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/beta2500/diet_warming_met.rds")

trait_DIET2500<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                 warming_all_eaint_ca=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)



#Diet arrays for trait based model 
baserun<-readRDS("TraitBasedModel/beta10000/diet_baserun.rds")
warming_all_ca<-readRDS("TraitBasedModel/beta10000/diet_warming_all_ca.rds")
warming_all_eaint_ca<-readRDS("TraitBasedModel/beta10000/diet_warming_all_eaint_ca.rds")
warming_all_eaint<-readRDS("TraitBasedModel/beta10000/diet_warming_all_eaint.rds")
warming_all<-readRDS("TraitBasedModel/beta10000/diet_warming_all.rds")
warming_int<-readRDS("TraitBasedModel/beta10000/diet_warming_int.rds")
warming_mort<-readRDS("TraitBasedModel/beta10000/diet_warming_mort.rds")
warming_met<-readRDS("TraitBasedModel/beta10000/diet_warming_met.rds")

trait_DIET10000<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                 warming_all_eaint_ca=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)



#Diet arrays for trait based model 
load("Hawaii/CNP_baserun_eq.Rdata")
baserun<- baserun_eq

load("Hawaii/CNP_warming_all_ca.Rdata")
 load("Hawaii/CNP_warming_all_eaint_ca.Rdata")
 load("Hawaii/CNP_warming_all_eaint.Rdata")
 load("Hawaii/CNP_warming_all.Rdata")
 load("Hawaii/CNP_warming_int.Rdata")
 load("Hawaii/CNP_warming_mort.Rdata")
 load("Hawaii/CNP_warming_met.Rdata")

Hawaii<-list(baserun=baserun, 
                 warming_all_ca= warming_all_ca, 
                 warming_all_eaint_ca=warming_all_eaint_ca, 
                 warming_all_eaint_ca=warming_all_eaint, 
                 warming_all=warming_all, 
                 warming_int=warming_int, 
                 warming_mort=warming_mort, 
                 warming_met=warming_met)
# To get diets from hawaii, use the newer getDiet function in the latest mizer release. 
# This works
#  getDiet(params = warming_met@params, n=warming_met@n[dim(warming_met@n)[1],,], 
#  n_pp= warming_met@n_pp[dim(warming_met@n_pp)[1],], 
#    n_other = initialNOther(params), proportion = TRUE)


allmods<- list(SEoz=SEoz, 
               northsea=northsea, 
               tasman=tasman,
               baltic=baltic, 
               Hawaii=Hawaii,
               ebs_default=modList_all_SR[[1]],
               ebs_bh=modList_all_SR[[2]],
               ebs_ricker=modList_all_SR[[3]],
               trait500=trait500,
               trait2500=trait2500,
               trait10000=trait10000,
               trait_DIET500=trait_DIET500,
               trait_DIET2500=trait_DIET2500,
               trait_DIET10000=trait_DIET10000)


save(allmods, file="allmods.Rdata")

rm(list=ls())


names(allmods)[14]<- "trait_DIET10000"

#######################################
#######################################
# Load and process simulation models ##
#######################################
#######################################

d<-"C:/Users/Jonathan.Reum/Work/MIZER comparison/EBS-CE code/EBS mizerRcpp_TempWS/"

#These are EBS specfic code and functions 
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

setwd("C:/Users/Jonathan.Reum/Work/MIZER comparison/modelOutputs")

load("allmods.Rdata") # Load list object that has all the regions, and sims for each region 

source("scenarios_summary_funcs.R") #loaded all the functions individually


##################################
# GET SSB, CATCHES AND MEAN SIZE #
##################################


library("mizerRewire")
library("mizer")


#Non-EBS and non-SEoz models 
out_vars<-lapply(allmods[c(2,4,5, 9:11)], function (x) { lapply(x, getSSB_Catch_MeanWt) })

# Just SEoz, catch requires mizerRewire version  
out_varsSE<-lapply(allmods[1], function (x) { lapply(x, getSSB_Catch_MeanWt, SEoz=TRUE) })

tas<-lapply(allmods[3], function (x) { lapply(x, getSSB_Catch_MeanWt, years_averaged=20) })


#EBS
out_vars_ebs<-lapply(allmods[6:8], function (x) { lapply(x, getSSB_Catch_MeanWt_ebs) })

out_vars<- c(out_varsSE, tas, out_vars,  out_vars_ebs)  #Include all the models together 

##############################################################
# Get RDD, RDI, erepro, rmax, Beta, and Lmax from all models #
##############################################################

#trait, Hawaii, NS, 
out_trait<-lapply(allmods[c(2,5,9:11)], function (x) { lapply(x, getRsummaryTrait)})

#SEoz, tasman, baltic
out_r<-lapply(allmods[c(1,3,4)], function (x) { lapply(x, getRsummary)})

out_rebs<-lapply(allmods[6], function (x) { lapply(x, getRsummary_ebs)})
                                                                                          
out_r<- c(out_trait, out_r, out_rebs)                                                                                    


################################
# Get age at maturation and M2 #
################################

traitmod<-lapply(allmods[9:11], function (x) { lapply(x, getAgeAtMatM2, traitmod=TRUE, camiMod=FALSE) })

#NS, Hawaii
mods_nsHI<-lapply(allmods[c(2,5)], function (x) { lapply(x, getAgeAtMatM2, traitmod=TRUE, camiMod=FALSE, EBS=FALSE) })

#Tasman
mods_tas<-lapply(allmods[c(1,3,4)], function (x) { lapply(x, getAgeAtMatM2, traitmod=FALSE, camiMod=FALSE, EBS=FALSE) })

EBS<-lapply(allmods[6:8], function (x) { lapply(x, getAgeAtMatM2, traitmod=FALSE, camiMod=FALSE, EBS=TRUE) })

out_mat<-c(mods_tas, mods_nsHI, EBS, traitmod)
                         



# Food web stats, Takes a verly long time (5 min with 3 * 8 models), so maybe parallel this one. 
# out_aveFW<-lapply(modList_all_SR, function (x) { lapply(x, getFoodWebStats) })



save(out_vars, file="ModelComp_SSB_Catch_MeanSize.Rdata")
save(out_r, file="ModelComp_RDD_RDI.Rdata")
save(out_mat, file="ModelComp_M2_MeanAge.Rdata")
