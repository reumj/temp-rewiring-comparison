

#Using the fitted models, plot up observed and predicted biomass, catch time series, and diet 

###############################################
# Testing for history biomass and catch plots #  
###############################################


#setwd("/Users/julia/Desktop/Reum_mizer/EBS mizer_v1.6")

library(methods)  # Might have to load reshape2 separately for some reason
library(reshape2)
library(plyr)
library(ggplot2)
library(grid)
library(abind)



# Run batches of models scenarios, one after another - may be parallize this if we were doing like 1000 or something 




runSims<-function(scenList= scen_list, 
                  params=params, 
                  effort=effort, 
                  calibrateSetup=FALSE, 
                  initial_n=initial_n, 
                  initial_n_pp = initial_n_pp, 
                  diet_steps=10,
                  dt=1,
                  getDiet4D=TRUE,
                  useRicker=FALSE){
  
  resultList<- scenList
  
  params_scen<- params
  
  for(i in 1:length(scenList)){
    
    params_scen@species_params$ca_met<- scenList[[i]]["ca_met"]
    params_scen@species_params$ca_mat<- scenList[[i]]["ca_mat"]
    params_scen@species_params$ca_mor<- 0
    params_scen@species_params$ca_int<- 0
    
    params_scen@species_params$ea_met<- scenList[[i]]["ea_met"]
    params_scen@species_params$ea_mat<- 0
    params_scen@species_params$ea_mor<- scenList[[i]]["ea_mor"]
    params_scen@species_params$ea_int<- scenList[[i]]["ea_int"]
    
    m_scen<-projectSetup(params_scen, effort=effort, 
                         dt=dt, 
                         calibrateSetup=FALSE, 
                         initial_n = initial_n, 
                         initial_n_pp = initial_n_pp, 
                         temperature_dt=scenList[[i]]["temperature"],
                         temperatureRef_dt=scenList[[i]]["temperatureRef"], 
                         diet_steps=diet_steps,
                         getDiet4D=getDiet4D,
                         useRicker=useRicker)
    
    resultList[[i]]<-m_scen
    print(i)
  }
  return(resultList)
}










#############################################
#           MODELS as a list 
#############################################


#effort<-effort_hs
#dt<- .25

#biodata<-biotot_his
#ssbdata<-spawnb_his
#catchdata<-catch_his

#modTS<- getModelsTS( modls, biodata, ssbdata, catchdata, effort)


############################
# get community indicators 
#############################



getCommInd<-function(mod, time=max(mod@t_dimnames)){
  
  #Indicators, apply to fish greater than 10 g
  # (1) LFI<- Large fish indicator: Biomass fish > 40 cm / Total biomass of all fish  (Blanchard et al. 2014) 
  # (2) MIW<- Mean individual weight 
  # (3) MMW<- Mean maximum weight (See Blanchard et al. 2014) 
  # (4) Slope<- Slope of community size spectrum 

  species<-unique(mod@params@species_params$species)
  species<- species[-which(species=="Benthos")]
  species<- species[-which(species=="Detritus")]

  #Dataframes to hold individuals variables, array to hold output from all models 

    results<-data.frame(year=time, lfi=NA, miw=NA, mmw=NA, slope=NA)

  #Extract LFI, MIW, MMW, and slope from previously fitted models   

    abspec<-getSpectra(mod, time_range=time, biomass=TRUE) #Abundance spectra 
    # abspec<- abspec[(abspec$w>=0.001), ] #use full size range  or not 

    abspec<- abspec[abspec$Species%in%species, ]
      
    ma<-match(abspec$Species, mod@params@species_params$species)
        abspec$Species<- mod@params@species_params$species1[ma]

        #Mean Individual weight, weighted by biomass
       
        #abspec_DT <- data.table(abspec)

        #abspec_DTsp<-abspec_DT[, list(wtave = weighted.mean(w, value), 
        #                              biosum = sum(value)),
        #                              by=.(Species)]

        #abspec_sp<- as.data.frame(abspec_DTsp)



        results$miw<- weighted.mean(abspec$w, abspec$value) # (x, w), body mass weighted by numerical abundance


        #Slope 
        abspectot<- aggregate(value~w, FUN=sum, data=abspec)
        results$slope<- coef(lm(log10(abspectot$value / mod@params@dw_full[ mod@params@w_full>10]) ~ log10(abspectot$w)))[2]
        

        #Mean maximum weight (treat sex as separate species)
        biospec<-getSpectra(mod, time_range=time, biomass=TRUE) #Biomass spectra 
        biospec<- biospec[(biospec$w>=10), ]
        biospec<- biospec[ !biospec$Species%in% c("Benthos", "Detritus","Fish_Crabs", "Background"), ]


        ma<-match( biospec$Species, mod@params@species_params$species)
        biospec$wmax<- mod@params@species_params$w_inf[ma]
        biosum<-aggregate(value~wmax,data=biospec,FUN=sum)

        results$mmw<- weighted.mean( biosum$wmax, biosum$value)

        #Large Fish indicator (this includes crabs in the EBS model)
        

        ma<-match( biospec$Species, mod@params@species_params$species)
       
        a<-mod@params@species_params$a
        b<-mod@params@species_params$b
        biospec$length<- exp( log(biospec$w/a[ma])/b[ma])

        results$lfi<- sum(biospec$value[biospec$length>40])/ sum(biospec$value)



    return(listresults) 
}





#############################
# FUNCTION FOR THE NON EBS  #
#############################



#mod<- allmods[[1]][[1]]

#sp_exclude=c("squalus spp.", 
#             "myctophids")


#mod<-SEAustralia_warming_met




getSSB_Catch_MeanWt<-function(mod, 
  sp_exclude=c("Benthos","Detritus","Fish_Crabs"), 
  remove.background=TRUE) {
  
  if(remove.background==TRUE) { sp_exclude<- c(sp_exclude, "Background")}
  
  
  
  getSSB_temp<-function (sim) {
    ssb <- apply(sweep(sweep(sim@n, c(2, 3), 
                             sim@params@psi, "*"), 3, sim@params@w * sim@params@dw, "*"), c(1, 2),  sum)
    return(ssb)
  }
  
  
  
  ssb<-getSSB_temp(mod)
  catch<-mizer::getYield(mod)
  spectra <- getSpectra(mod, biomass=TRUE)



  #Get community meant size (biomass weighted) 

  spectratot<- spectra
  spectratot<-spectratot[!spectratot$Species%in%sp_exclude, ]

  #Get individual species mean size 

  spectra<- spectra[!spectra$Species%in%sp_exclude, ]
  spectra_DT<- data.table(spectra)
  spectra_DT<- spectra_DT[, list(meanwt = weighted.mean(w,value)), by=.(Species)]
  
  spectra_sp<-as.data.frame(spectra_DT)
 
  #Get sp and comm ssb and and catches 

  ssb<- ssb[dim(ssb)[1]-1, ]
  ssb<- ssb[!names(ssb)%in%sp_exclude]
  
  catch<- catch[dim(catch)[1]-1, ]

  catch<-catch[!names(catch)%in%sp_exclude]
  

  dat<-t(rbind(ssb, catch))

  dat<-cbind(spectra_sp,dat)
  

  colnames(dat)[1]<- "Species"

  dat<- dat[!dat$Species%in%sp_exclude, ]

  
  
 #Add in community value 

  dat2<- dat[1,,drop=FALSE]

  dat2[dim(dat2)[1], ]<- NA

  dat2$Species<- as.character(dat2$Species)

  dat2$Species[dim(dat2)[1]]<- "community"
  dat2$ssb[dim(dat2)[1]]<- sum(dat$ssb, na.rm=TRUE)
  dat2$catch[dim(dat2)[1]]<- sum(dat$catch, na.rm=TRUE)
  dat2$meanwt[dim(dat2)[1]]<-  weighted.mean(spectratot$w, spectratot$value)

  
  
  dat3<- rbind(dat, dat2) 
  
  dat4<- as.matrix(dat3[,2:4])

  rownames(dat4)<- dat3$Species

  return(dat4)
}




##########################################
# This version is for the EBS models #
##########################################


#mod<-allmods[[6]][[1]]


getSSB_Catch_MeanWt_ebs<-function(mod, 
                              sp_exclude=c("Benthos","Detritus", "Background", "Fish_Crabs")) { 
  ssb<-getSSB(mod)
  catch<-getYieldFromSim(mod)
  spectra <- getSpectra(mod, biomass=TRUE)
  
  #Get community meant size (biomass weighted) 
  
  spectratot<- spectra
  spectratot<-spectratot[!spectratot$Species%in%sp_exclude, ]
  
  

  #Get individual species mean size 
  
  ma<-match(spectra$Species, mod@params@species_params$species)
  spectra$Species1<- mod@params@species_params$species1[ma]
    
  spectra_DT<- data.table(spectra)
  
  
  spectra_DT<- spectra_DT[, list(meanwt = weighted.mean(w,value)), by=.(Species1)]
  
  spectra_sp<-as.data.frame(spectra_DT)
  spectra_sp<-na.omit(spectra_sp)
  
  spectra_sp<- spectra_sp[!spectra_sp$Species1%in%sp_exclude, ]
  
  
  #Get sp and comm ssb and and catches 
  
  ssb<- ssb[rownames(ssb)==150,]
  catch<- catch[rownames(catch)==150,]
  
  
  dat<-t(rbind(ssb, catch))
  
  dat<-aggregate( dat, by=list(mod@params@species_params$species1), FUN=sum )
  
  colnames(dat)[1]<- "species"
  
  ma<-match( dat$species, spectra_sp$Species1)
  
  dat$meanwt<- spectra_sp$meanwt[ma]
  
  dat<- dat[!dat$species%in%sp_exclude, ]
  
  #Add in community value 
  
  dat2<-rbind(dat, dat[1,,drop=FALSE])
  
  dat2[dim(dat2)[1], ]<- NA
  
  dat2$species<- as.character(dat2$species)
  
  dat2$species[dim(dat2)[1]]<- "community"
  dat2$ssb[dim(dat2)[1]]<- sum(dat2$ssb, na.rm=TRUE)
  dat2$catch[dim(dat2)[1]]<- sum(dat2$catch, na.rm=TRUE)
  dat2$meanwt[dim(dat2)[1]]<-  weighted.mean(spectratot$w, spectratot$value)
  
  dat3<-as.matrix(dat2[2:4])
  
  rownames(dat3)<- dat2$species
  
  return(dat3)
}





##########################################
#
# Get trophic level for each predator  
#
########################################


#modls<- allmodsF_350yrs
#        time_range=c(2339, 2364)
#        pelTLslope=.1
#        c=2.5
#        benTLslope=0
#        benTLint=2.5

#test
#modls<-modList
#mod<- modList[[1]]


#test<-getTLbyPred(allmods[[6]][[1]], EBS=TRUE)

#test<-getTLbyPred(allmods[[2]][[1]], EBS=FALSE)

#mod<-allmods[[6]][[1]]

#plot(tl~log10(wt), test)



getTLbyPred<-function(mod, EBS=FALSE, 
        time_range=max(as.numeric(dimnames(mod@n)$time)), 
        zoopTL=0, benTLint=0, benTLslope =0, pelTLint = 0, pelTLslope=0,
        sp_exclude=c("Detritus","Benthos","Background")){ 



    time_elements <- get_time_elements(mod,time_range)

    n <- apply(mod@n[time_elements,,,drop=FALSE],c(2,3), mean)   # get n for weighting community mean TL 

    #Get biomass density     

    size_range <- get_size_range_array(mod@params)

    n_dw <- sweep(n*size_range,2,mod@params@dw, "*")
    b_wdw <- sweep(n_dw,2,mod@params@w, "*")
    

    n1<-n_dw
    n1[n1>0]<-1


    #Pull out diet composition 
    
    if(EBS==FALSE){
      dietcomp<-mod@diet_comp
      
    } else {
      dietcomp<-mod@diet_comp4D
    }
    
    tlsum<-dietcomp[1,1,,]
    tlsum<-drop(tlsum)
    names(dimnames(tlsum))<-c("predator","pred_size")

    predator<- dimnames(dietcomp)$predator
    prey<-dimnames(dietcomp)$prey
    pred_size<- dimnames(dietcomp)$pred_size
    prey_size<- dimnames(dietcomp)$prey_size

    tlsum[]<-0

    #Might one day include at TL vs body size for benthos or pelagic resource spectra  
    tlsum[dimnames(tlsum)$predator=="Benthos", ]<- benTLint + benTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    tlsum[dimnames(tlsum)$predator=="background", ]<- pelTLint + pelTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))

    #Load in specific zoop TL if supplied 
    if(all(!is.na(zoopTL))){ tlsum[dimnames(tlsum)$predator=="background", ] <- zoopTL }



    predator<- predator[!predator%in%sp_exclude]
    dietcomp<- dietcomp[!dimnames(dietcomp)$predator%in%sp_exclude, , , ]

    #Calculate the TL of each species weighted mean community TL 
    
    tlcomm<- rep(NA,length(prey_size))

    dimnames(n)[[2]]<-  dimnames(dietcomp)$pred_size
      for(i in 1:length(pred_size)){      #Predator size 
          for (j in 1:length(predator)){  #Predator species

                    tl<- 1 + weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
                    tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
            
              }
       }
    
    if(EBS==TRUE){
    tlsum[1:25,31:130]<- tlsum[1:25,31:130]*size_range
    }
    
    #Add community on to species matrix 
    tlcomm<- matrix(tlcomm, nrow=1)
    rownames(tlcomm)<- "community"
    tlsum<-rbind(tlsum, tlcomm)

    dimnames(tlsum)<-list( predator=dimnames(tlsum)[[1]], 
                              pred_size=dimnames(tlsum)[[2]])
    
    #Get rid of Benthos, Detritus, background
    tlsum<-tlsum[ !dimnames(tlsum)$predator %in% c("Benthos","Detritus","background"), ]
    
    
    
    #Melt the arrays to make data frame with species, wt, biomass, num, tl 
    tldat<-melt(tlsum)
    colnames(tldat)<- c("predator", "wt","tl")
  
    
return(tldat)

} #end function 







##########################################
#
# Get Community mean TL by body size class (weighted av. TL within body size class)
#
########################################


#modls<- mod_ls


#pelTLslope=0; pelTLint=0
#benTLslope=0; benTLint=0
#zoopTL=0


#modls<- modList



getCommMeanTL_List<-function(modls, 
        time_range=max(as.numeric(dimnames(modls[[1]]@n)$time)),
        zoopTL=NA,
        pelTLslope=0, pelTLint=0,
        benTLslope=0, benTLint=0){ 

    #data frame to recieve results 
    dat<-data.frame(wt=0, tls=0, model=0, totbio=0, totnum=0)
    dat<-dat[-1,]
    
    for (h in 1:length(modls)){
    
        time_elements <- get_time_elements(modls[[h]],time_range)
    
        n <- apply(modls[[h]]@n[time_elements,,,drop=FALSE],c(2,3), mean)   # get n for getting weighted community mean TL 
    
        #Get biomass density     
    
        size_range <- get_size_range_array(modls[[h]]@params)
    
        b <- sweep(n*size_range,2,modls[[h]]@params@w * modls[[h]]@params@dw, "*")
        n <- sweep(n*size_range,2,modls[[h]]@params@dw, "*")
    
        n1<-n
        n1[n1>0]<-1
    
    
        #Pull out diet composition 
        dietcomp<-modls[[h]]@diet_comp4D
    
        tlsum<-dietcomp[1,1,,]
        tlsum<-drop(tlsum)
        names(dimnames(tlsum))<-c("predator","pred_size")
    
        predator<- dimnames(dietcomp)$predator
        prey<-dimnames(dietcomp)$prey
        pred_size<- dimnames(dietcomp)$pred_size
        prey_size<- dimnames(dietcomp)$prey_size
    
        tlsum[]<-0
        tlsum[dimnames(tlsum)$predator=="Benthos", ]<- benTLint + benTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
        tlsum[dimnames(tlsum)$predator=="background", ]<- pelTLint + pelTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    
        #Load in specific zoop TL if supplied 
        if(all(!is.na(zoopTL))){ tlsum[dimnames(tlsum)$predator=="background", ] <- zoopTL }
    
        predator<- predator[!predator=="Detritus"]
        predator<- predator[!predator=="Benthos"]
        predator<- predator[!predator=="background"]
    
    
    
          for(i in 1:length(pred_size)){    #Predator size 
              for (j in 1:length(predator)){ #Predator species
    
                        tl<- 1 + weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
                        tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
              }
            }
    
    
        tlsum<-tlsum[1:25, 31:130] * n1
    
        n[dimnames(n)$sp=="Benthos", ]<-0
        n[dimnames(n)$sp=="Detritus", ]<-0
        
        b[dimnames(b)$sp=="Benthos", ]<-0
        b[dimnames(b)$sp=="Detritus", ]<-0
    
        tlsum<-tlsum[, as.numeric(dimnames(tlsum)$pred_size)>.001]
        n<- n[, as.numeric(dimnames(n)$w)>.001]
        b<- b[, as.numeric(dimnames(b)$w)>.001]
    
    
        tlcomm<- data.frame(  wt= as.numeric(dimnames(n)$w), 
                              tls=rep(0, dim(tlsum)[2])) 
    
    
        #Calculate community mean tl 
        for(i in 1:dim(tlsum)[2]){
          tlcomm$tls[i]<- weighted.mean( tlsum[,i], n[,i]) 
          }
          
          tlcomm$totbio<-apply(b, 2, FUN="sum") 
          tlcomm$totnum<-apply(n, 2, FUN="sum") 
           
          tlcomm$model<- names(modls)[h]
    
        dat<- rbind(dat, tlcomm)
    
      } #end h 

return(dat)

} #end function 







####Version for lapply 




getCommSpMeanTL<-function(object, 
                        time_range=max(as.numeric(dimnames(object@n)$time)),
                        zoopTL=0,
                        pelTLslope=0, pelTLint=0,
                        benTLslope=0, benTLint=0){ 
  
  #data frame to recieve results 
  dat<-data.frame(wt=0, tls=0, model=0, totbio=0, totnum=0)
  dat<-dat[-1,]

    
    time_elements <- get_time_elements(object,time_range)
    
    n <- apply(object@n[time_elements,,,drop=FALSE],c(2,3), mean)   # get n for getting weighted community mean TL 
    
    #Get biomass density     
    
    size_range <- get_size_range_array(object@params)
    
    b <- sweep(n*size_range,2, object@params@w * object@params@dw, "*")
    n_dw <- sweep(n*size_range,2, object@params@dw, "*")
    
    
    
    #Pull out diet composition 
    dietcomp<-object@diet_comp4D
    
    tlsum<-dietcomp[1,1,,]
    tlsum<-drop(tlsum)
    names(dimnames(tlsum))<-c("predator","pred_size")
    
    predator<- dimnames(dietcomp)$predator
    prey<-dimnames(dietcomp)$prey
    pred_size<- dimnames(dietcomp)$pred_size
    prey_size<- dimnames(dietcomp)$prey_size
    
    tlsum[]<-0
    tlsum[dimnames(tlsum)$predator=="Benthos", ]<- benTLint + benTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    tlsum[dimnames(tlsum)$predator=="background", ]<- pelTLint + pelTLslope * log10(as.numeric(dimnames(tlsum)$pred_size))
    
    #Load in specific zoop TL if supplied 
    if(all(!is.na(zoopTL))){ tlsum[dimnames(tlsum)$predator=="background", ] <- zoopTL }
    
    predator<- predator[!predator=="Detritus"]
    predator<- predator[!predator=="Benthos"]
    predator<- predator[!predator=="background"]
    
    
    
    for(i in 1:length(pred_size)){    #Predator size 
      for (j in 1:length(predator)){ #Predator species
        
        tl<- 1 + weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
        tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
      }
    }
    


    
    tlsum<-tlsum[1:25, 31:130] * size_range #Remove TLs for individuals outside the size ranges  
    
    tlsum<-rbind(tlsum, tlsum[1,, drop=FALSE])
    rownames(tlsum)[dim(tlsum)[1]]<-"community"
    
    #Calculate community mean tl 
    for(i in 1:dim(tlsum)[2]){
      tlsum[dim(tlsum)[1], i]<- weighted.mean( tlsum[1:(dim(tlsum)[1]-1),i], n_dw[1:(dim(tlsum)[1]-1),i],  na.rm=T) 
    
    }
    

  return(tlsum)
  
} #end function 












####Get the PPMR 

getPPMRsp<-function(object=object, prey=dimnames(object@diet_comp)$prey, 
                   predator=dimnames(object@diet_comp)$predator){
        
        prey_nam<-prey
        pred_nam<-predator
        
        out<-object@diet_comp4D 
  
        prey<-apply(out, c(1,2,4), FUN=sum) #Sum across size classess with in prey 
        tot<-apply(prey, c(1,2), FUN=sum) #Sum across prey weight size classes 
        
        prop_prey<-sweep(prey, 1:2, tot, "/" ) #proportion of prey weight in each each prey size class
        prop_prey[is.na(prop_prey)]<-0
        
        #make matrix of realized PPMR
        ppmr_mat<-outer(object@params@w, object@params@w_full, FUN="/")
        ppmr_frac<-sweep(prop_prey, 2:3, ppmr_mat, "*")
        ppmr_tot<-apply(ppmr_frac, c(1,2), FUN=sum) #Sum across prey weight size classes 
        
        plot_dat<-expand.grid(dimnames(ppmr_tot)[[1]], dimnames(ppmr_tot)[[2]])
        colnames(plot_dat)<-c("predator","predsize")
        plot_dat$predsize<-as.numeric(as.character(log10(as.numeric(as.character(plot_dat$predsize)))))
        
        plot_dat$value<- as.vector(ppmr_tot)
        
  
        species<-object@params@species_params$species
        wmin<-object@params@w[object@params@species_params$w_min_idx]
        wmax<-object@params@w[object@params@species_params$w_max_idx]
        
        for ( i in 1:length(species)){
          plot_dat$value[plot_dat$predator==species[i] & plot_dat$predsize < log10(wmin[i])]<- 0
          plot_dat$value[plot_dat$predator==species[i] & plot_dat$predsize > log10(wmax[i])]<- 0
        }
  
  plot_dat$value[plot_dat$value==0]<-NA
  
  plot_dat<-plot_dat[!plot_dat$predator=="Benthos",]
  plot_dat<-plot_dat[!plot_dat$predator=="Detritus",]
  
  return(na.omit(plot_dat))
}



#c("Benthos","Detritus")  %in% dimnames(out)["predator"]



                                   
 
#object<- modList[[1]]
#allmods[[2]][[1]]

#getPPMRcomm(allmods[[2]][[1]])

#test<-getPPMRcomm(allmods[[6]][[1]])

getPPMRcomm<-function(object=object){
  
  out<-object@diet_comp4D #This is g / m3 / yr, so we're working in biomass-weighted PPMR
  
  #Remove detritus as predator and prey, and benthos as predator 
  out<- out[!dimnames(out)$predator %in% c("Benthos","Detritus"),,!dimnames(out)$prey %in% c("Detritus"),]
  
  preyBiomass<-apply(out, c(2,4), FUN=sum) #Sum up prey encountred in predator size class by prey size class 
  prey_wt<-as.numeric(as.character(dimnames(preyBiomass)$prey_size))
  
  preyNumbers<-sweep(preyBiomass, 2, prey_wt, "/")  #convert g prey to individual prey 

  weighted.meanFunc<-function(bio) {
    weighted.mean(prey_wt , w=bio)
  }
  
  wtpreyBiomass<- apply(preyBiomass, c(1), weighted.meanFunc) #prey bodymass weighted by biomass in predator diet 
  wtpreyNumber<- apply(preyNumbers, c(1), weighted.meanFunc) #prey bodymass weighted by biomass in predator diet 
  
  #Predator to prey body mass ratio; PPMRbio 

  df<-data.frame(ppmrN =log10(as.numeric(names(wtpreyNumber)) / wtpreyNumber),
             ppmrB = log10(as.numeric(names(wtpreyBiomass)) / wtpreyBiomass))

  return(as.matrix(df))
}





##################Function to get TLsmooth 

TLsmooth<-function(biomass, bins, tlsd=.1, binwid=.05){
  biosum<- rep(0,length(biomass))

    for(i in 1:length(biomass)){  # Pull out biomass in bin to distribute
        bio<-biomass[i]
        
        if(bio>0){
            for(j in 1:length(biomass)){ # distribute biomass from bin to surrounding bins
                biosum[j]<- biosum[j] + dnorm(bins[j],bins[i],tlsd)* bio * binwid
            }
        }
    }
    return(biosum)
  }




######################
# Average age of SSB #
######################



library(splines)

#getMeanSSBAge(object)

#object=model
#time_range=dim(object@n)[1]-1 
#print_it = TRUE
#dt=300
#use.kinf=FALSE


getMeanSSBAge_ebs<-function(object=model, time_range=dim(object@n)[1]-1, 
                        print_it = TRUE, dt=300, use.kinf=FALSE){
  
  e_growth<- object@e_growth[-dim(object@n)[1],,]
  
  index<- c(1:I(dim(object@n)[1]-1))
  
  if(length(time_range)>1){
    time_elements <- (index >= time_range[1]) & (index <= time_range[2])
    spec_e_growth <- apply(e_growth[time_elements,,],c(2,3), mean)
  } else {
    time_elements <- (index == time_range[1])
    spec_e_growth <- e_growth[time_elements,,]
  }
  
  
  
  #get SSB for same mean age at maturation calc 
  
  if(length(time_range)>1){
    time_elements <- (index >= time_range[1]) & (index <= time_range[2])
    spec_spawner_ab <- sweep(apply(object@n[time_elements,,],c(2,3), mean), 2, object@params@dw, "*") * object@params@psi
  } else {
    time_elements <- (index == time_range[1])
    spec_spawner_ab <- sweep(object@n[time_elements,,], 2, object@params@dw, "*") * object@params@psi
  }
  
  
  
  #Pick range of WT to get growth rates for
  
  edat <- data.frame(e_gr = c(spec_e_growth), Species = dimnames(spec_e_growth)[[1]], w = rep(object@params@w, each=dim(spec_e_growth)[1]))
  
  species<-object@params@species_params$species
  
  wmin<-object@params@w[object@params@species_params$w_min_idx]
  wmax<-object@params@w[object@params@species_params$w_max_idx]
  
  for ( i in 1:length(species)){
    edat$e_gr[edat$Species==species[i] & edat$w < wmin[i]]<- NA
    edat$e_gr[edat$Species==species[i] & edat$w > wmax[i]]<- NA
  }
  
  edat<-edat[!is.na(edat$e_gr),]
  
  #Estimate functions to get growth 
  modls<-list()
  
  #Make smooth function of growth for each species 
  for(i in 1:length(species)){
    
    dat<-subset(edat, Species==species[i])
    
    if(dim(dat)[1]>2){
      dat=subset(edat, Species==species[i])
      spf<-splinefun(dat$w,dat$e_gr) #Spline interpolation for calculating w+dt 
      modls[[i]]<-spf               
    } else {
      modls[[i]]<-NA
    } 
  }
  
  
  
  #Data frame to hold age at weight calculations
  age<-c(0:100)
  
  adat<-expand.grid(unique(edat$Species), c(0:max(age)))        
  colnames(adat)<-c("Species", "age")
  adat$e_wt<-0
  adat$obs_wt<-0
  adat$age_norm<-0
  
  
  
  
  k<-object@params@species_params$k_vb
  
  if(use.kinf==TRUE){
    w_inf<-object@params@species_params$w_kinf
  } else {
    w_inf<-object@params@species_params$w_inf
  }
  
  
  t0<-object@params@species_params$t0
  b<-object@params@species_params$b
  
  
  for(i in 1:length(species)){
    adat$obs_wt[adat$Species==species[i]]<- (w_inf[i]* (1-exp(-k[i]*(adat$age[adat$Species==species[i]]-t0[i])))^b[i] )/w_inf[i]
  }
  
  adat$obs_wt[adat$obs_wt<=0]<-NA
  
  
  #adat$age_norm[adat$Species==species[i]]<- adat$age[adat$Species==species[i]] / (a*object@params@species_params$w_mat[i]^(1-n)) #n is exponent for maximum food intake (Hartvig et al. 2011) 
  
  
  for(i in 1:length(species)){
    
    incdat<-rep(NA, dt)
    incdat[1]<-object@params@species_params$w_min[i]
    adat$e_wt[adat$Species==species[i] & adat$age==age[1]]<-object@params@species_params$w_min[i] 
    
    if(!is.na(modls[[i]])){
      
      for (ii in 2:length(age)){
        
        for (j in 1:(dt-1)){
          spfun<-modls[[i]]
          gr<-incdat[j]+ spfun(incdat[j]) *(1/dt)
          
          if (gr<w_inf[i]){
            incdat[1+j]<- gr
          } else {
            incdat[1+j]<-w_inf[i]
          }
        }
        adat$e_wt[adat$Species==species[i] & adat$age==age[ii]]<-incdat[dt] 
        incdat[1]<-incdat[dt]
      }
    } else { adat$e_wt[adat$Species==species[i]]<-0}
    
  }
  # Get wt at 50%
  # Make function of Age ~ wt 
  
  modls_afunc<-list()
  modls_psifunc<-list()
  
  for(i in 1:length(species)){
    
    dat<-subset(adat, Species==species[i])
    
    if( species[i]%in%c("Benthos","Detritus")) {  #to deal with benthos
      modls_afunc[[i]]<-function(x){return(0)}
      modls_psifunc[[i]]<-function(x){return(0)}
    } else { 
  
          spf<-splinefun(dat$e_wt,dat$age) #Spline interpolation for calculating w+dt 
          modls_afunc[[i]]<-spf               
        
          wt<-as.numeric(dimnames(spec_spawner_ab)[[2]])
          xx<-object@params@psi[i,]

          spf_psi<-splinefun(x=xx[xx>0 & xx<1], y=wt[xx>0 & xx<1], method="monoH.FC")
          modls_psifunc[[i]]<- spf_psi
    }
  }

  
  
  #Biuld matrix of age vs. weight. 
  
  age_weight<- spec_e_growth
  
  age_weight[]<-0
  
  wt<-as.numeric(dimnames(age_weight)[[2]])
  
  for(i in 1:length(modls_afunc)){
    age_weight[i,]<- modls_afunc[[i]](wt)
  }
  
  age_weight[age_weight<0]<-0
  
  
  avedat<-data.frame(aveMatwt=rep(NA, length(modls_afunc)),
                     aveMatAge=rep(NA, length(modls_afunc)), 
                     aveAge_50=rep(NA, length(modls_afunc)))
                      
  
                      
  rownames(avedat)<-species
  
  #Add average age of spanwers back in (psi * n*dw * age~w)
  
  for(i in 1:length(modls_afunc)){
    
    avedat$aveMatwt[i]<- weighted.mean( as.numeric(dimnames(spec_spawner_ab)[[2]]), spec_spawner_ab[i,])
    avedat$aveMatAge[i]<-    modls_afunc[[i]](avedat$aveMatwt[i]) #the average age of spawners
  
    #get wt at 0.5 maturation 
    wtM<- modls_psifunc[[i]](0.5)
    #get age at weight 
    avedat$aveAge_50[i]<- modls_afunc[[i]](wtM) #the averaget age when psi is 50% 
    
    }
  
  avedat<-as.matrix(avedat)
  rownames(avedat)<- object@params@species_params$species1
  
  
  avedat<- avedat[!object@params@species_params$sex=="male",]
  avedat<-avedat[!rownames(avedat)%in%c("Benthos", "Detritus"), ]
  
  return(avedat)
}


  
################################
# Growth curves for all models #
################################


getAgeAtMatM2<-function (object, 
                         max_age = 200, 
                         traitmod=FALSE, 
                         camiMod=FALSE, 
                         EBS=FALSE) {

      sim <- object
      species <- dimnames(sim@n)$sp
      
      idx_last<- dim(sim@n)[1]
      
    
      idx <- which(dimnames(sim@n)$sp %in% species)
      species <- dimnames(sim@n)$sp[idx]
      age <- seq(0, max_age, length.out = 50)
      ws <- array(dim = c(length(species), length(age)), 
                  dimnames = list(Species = species, Age = age))
     
      if(traitmod==TRUE){
        
        #Trait-based version? But need correction for temp.... 
        g <- mizer::getEGrowth(sim@params, sim@n[dim(sim@n)[1], , ], 
                               sim@n_pp[dim(sim@n)[1], ])
        
      }
      
      if(camiMod==TRUE){
         
      #See adapted_funcs
      g<- getEGrowthCami(sim@params, sim@n[dim(sim@n)[1], , ], 
                           sim@n_pp[dim(sim@n)[1], ], sim@n_bb[dim(sim@n)[1], ],
                           sim@n_aa[dim(sim@n)[1], ], 
                           sim@intTempScalar[,, (dim(sim@temperature)[1])], 
                           sim@metTempScalar[, , dim(sim@temperature)[1]] )
         
      }
      
      if( traitmod==FALSE & camiMod==FALSE & EBS==FALSE){
        
        g <- mizerRewire::getEGrowth(sim@params, sim@n[dim(sim@n)[1], , ], 
                                       sim@n_pp[dim(sim@n)[1], ], sim@n_bb[dim(sim@n)[1], ],
                                       sim@n_aa[dim(sim@n)[1], ], 
                                       sim@intTempScalar[,, (dim(sim@temperature)[1])], 
                                       sim@metTempScalar[, , dim(sim@temperature)[1]] )
      }
      
     
      if(EBS==TRUE){
        
        g<- sim@e_growth[idx_last-1,,]
  
      }
      
      
  
    for (j in 1:length(species)) {
        i <- idx[j]
        g_fn <- stats::approxfun(sim@params@w, g[i, ])
        myodefun <- function(t, state, parameters) {
          return(list(g_fn(state)))
        }
        ws[j, ] <- deSolve::ode(y = sim@params@species_params$w_min[i], 
                                times = age, func = myodefun)[, 2]

    }
     
    # Calculate size at maturation 
    # Get weight at maturation 
    # plug weight into age function to get age at maturation  
     
    agemat<-rep(NA, length(species))
    names(agemat)<-species
     
    for (j in 1:(length(species)-2)) {
      i <- idx[j]
      psi_fn <- stats::approxfun(sim@params@psi[i,], dimnames(sim@params@psi)$w)
      
      age_fn<- stats::approxfun( ws[i, ], as.numeric(colnames(ws)))
      agemat[i]<-age_fn(psi_fn(.5))
        
      }
     
    # To get predation mortality rate: 
    
    
    n<-sim@n[idx_last,,]
    
    if(traitmod==FALSE & EBS==FALSE){
      #m2<- rowSums(n*mizerRewire::getPredMort(sim, 
      #                                        n=n, 
      #                                        n_pp=n_pp,
      #                                        intakeScalar=sim@intTempScalar[,,1]))

      m2<-rowSums(n*mizerRewire::getPredMort(sim, intakeScalar=sim@intTempScalar[,,1])[idx_final,,])
      
    }
      if(traitmod==TRUE){
        
      m2<- rowSums(n*mizer::getPredMort(sim)[idx_final,,])
    
      }
    
      
     if(EBS==TRUE){
       
        m2<- rowSums(n*sim@m2[idx_last-1, -26, c(31:130)])
     }
    
    
      return(cbind(agemat,m2))
  }






###############################################################################
# CALCULATE ENERGY SOURCES 
########################################


#Tas model 

model<-allmods[[4]][[1]]



sp_excludeTL=NA
trace_group = "benthos"
  
#Pull out diet composition, put into proportional contribution witin a predator species and predator size class
tlsum<-model@diet_comp[1,1,,]
tlsum<-drop(tlsum)
names(dimnames(tlsum))<-c("predator","pred_size")


predator<- dimnames(model@diet_comp)$predator
prey<-dimnames(model@diet_comp)$prey
pred_size<- dimnames(model@diet_comp)$pred_size
prey_size<- dimnames(model@diet_comp)$prey_size

tlsum[]<-0

#Load in specific group to trace
# tlsum[dimnames(tlsum)$predator==trace_group, ] <- 1

#Set up for TL calculation
predator<- predator[!predator%in%sp_excludeTL]
dietcomp<- model@diet_comp[!dimnames(model@diet_comp)$predator%in%sp_excludeTL, , , ]


for(i in 1:length(pred_size)){      # Predator size 
  for (j in 1:length(predator)){  # Predator species
    
    tl<-  weighted.mean(tlsum,  dietcomp[dimnames(dietcomp)$predator==predator[j], dimnames(dietcomp)$pred_size==pred_size[i], ,] ) 
    tlsum[dimnames(tlsum)$predator==predator[j], dimnames(tlsum)$pred_size==pred_size[i]]<-tl
  }
}


# Melt the arrays to make data frame with species, wt, biomass, num, tl


tldat<-melt(tlsum)
# tldat<-na.omit(tldat) #shouldn't need

b_ave<-sweep(n_ave, 2, model@params@w *model@params@dw, "*")
predbio<-melt(b_ave)


ma<-match(paste(tldat$predator, signif(tldat$pred_size,3)), paste(predbio$sp, signif(predbio$w,3)))

tldat$predbio<- predbio$value[ma]
tldat$pred_size<- floor(log10(as.numeric(tldat$pred_size)))+.5

DFtl<- data.table(tldat)
tldatave<-DFtl[,list(tl_ave = weighted.mean(value, predbio, na.rm = TRUE)),by=.(predator , pred_size)]

# Add to predator food web metrics 
ma<-match( paste(datPredSize$predator, datPredSize$pred_size), paste(tldatave$predator, tldatave$pred_size))
datPredSize$relTL <- tldatave$tl_ave[ma]

















############################
# Helper functions  ########




getSpectra<-function(object, 
                     time_range = max(as.numeric(dimnames(object@n)$time)), 
                     min_w =min(object@params@w)/100, biomass = TRUE, 
                     print_it = TRUE, grid=FALSE,
                     integrate_over_w=FALSE, ...){
  time_elements <- get_time_elements(object,time_range)
  
  #All species in model
  spec_n <- apply(object@n[time_elements,,,drop=FALSE],c(2,3), mean)
  
  #Remove Benthos and detritus 
  idx_BenDet<-which(object@params@species_params$species=="Benthos"|object@params@species_params$species=="Detritus")
  totFishCrab_n <- colSums( spec_n[-idx_BenDet,])
  
  background_n <- apply(object@n_pp[time_elements,,drop=FALSE],2,mean)
  y_axis_name = "Abundance"
  
  if (biomass){
    spec_n <- sweep(spec_n,2,object@params@w,"*") 
    totFishCrab_n <- totFishCrab_n* object@params@w
    background_n <- background_n * object@params@w_full
    y_axis_name = "Biomass"
  }
  
  
  if(integrate_over_w==TRUE){
    totFishCrab_n <- totFishCrab_n * object@params@dw
    background_n <- background_n * object@params@dw_full
  }
  
  
  
  # Make data.frame for plot
  plot_dat <- data.frame(value = c(spec_n), Species = dimnames(spec_n)[[1]], w = rep(object@params@w, each=nrow(object@params@species_params)))
  plot_dat <- rbind(plot_dat, 
                    data.frame(value = c(background_n), Species = "Background", w = object@params@w_full),
                    data.frame(value = c(totFishCrab_n), Species = "Fish_Crabs", w = object@params@w))
  
  # lop off 0s in background and apply min_w
  plot_dat <- plot_dat[(plot_dat$value > 0) & (plot_dat$w >= min_w),]
  
  return(plot_dat)
}


#####Need for EBS####################

setGeneric('getYieldFromSim', function(object)
  standardGeneric('getYieldFromSim'))
#' @rdname getYield-methods
setMethod('getYieldFromSim', signature(object='MizerSim'),
          function(object){
            # biomass less the first time step
            
            n_catch<- object@f_mort* object@n[2:dim(object@n)[1],,]
            Catch<- sweep(n_catch, c(3),object@params@dw *object@params@w, "*")
            Catch_sum<-apply(Catch, c(1,2), FUN="sum")
            colnames(Catch_sum)<- dimnames(object@n)$sp
            
            #Add up sexes
            Catch_sum<- t(Catch_sum)
            
            Catch_sum<- aggregate(Catch_sum, by=list(object@params@species_params$species1), FUN="sum")
            
            rownames(Catch_sum)<- Catch_sum[,1]
            Catch_sum<- Catch_sum[,-1]
            
            Catch_sum<- as.matrix(Catch_sum) 
            
            dimnames(Catch_sum)<- list(sp=rownames(Catch_sum), time=colnames(Catch_sum))
            
            
            return(t(Catch_sum))
          })


##############################
#




get_caEff<-function(x){
    
  dif_all<- x[["warming_all_ca"]] -x[["warming_all"]] 
  dif_eaint<- x[["warming_all_eaint_ca"]] -x[["warming_all_eaint"]] 

   x<-  c(x, list(dif_all=dif_all, dif_eaint= dif_eaint ))
   return(x)
}
  
  


##########################################################
# Work on getting RDD, RDI, and Rmax from NON EBS models #
##########################################################
# Also get M2 the models, and mizerRewire::getEGrowth() to calculate 
# changes in age at reproduction, average age of spawners, sd of avge of spawners

model<-allmods[[1]][[1]]

getRsummaryTrait(model)

getRsummaryTrait<-function(model, dt=.25){
  
  tmax<-dim(model@n)[1]
  
  rdi<-mizer::getRDI(model@params, model@n[tmax,,],model@n_pp[tmax,])
  e<-model@params@species_params$erepro 
  rmax<- model@params@species_params$R_max
  
  rdd <- rmax*rdi*e/(rdi*e+rmax) #Apply BH

  names(rmax)<- model@params@species_params$species
  
  return(cbind(rdi,rdd,rmax))
  
}



model<-allmods[[2]][[1]]


getRsummary(model)



getRsummary<-function(model, dt=.25){
  
  tmax<-dim(model@n)[1]

  rdi<-mizerRewire::getRDI(model@params, model@n[tmax,,],model@n_pp[tmax,],
                      model@n_bb[tmax,], model@n_aa[tmax,],
                      model@intTempScalar[,,(tmax/dt)], 
                      model@metTempScalar[,,(tmax/dt)])
  
  rdd<-mizerRewire::getRDD(model@params,
                           model@n[tmax,,],
                           model@n_pp[tmax,], 
         model@n_bb[tmax,], 
         model@n_aa[tmax,], 
         sex_ratio = 0.5, 
         model@intTempScalar[,,(tmax/dt)], 
         model@metTempScalar[,,(tmax/dt)])
  
  rmax<- model@params@species_params$r_max
  names(rmax)<- model@params@species_params$species
  
  return(cbind(rdi,rdd,rmax))
  
}



model<-allmods[[6]][[1]]

getRsummary_ebs<-function(model, sp_exclude=c("Benthos","Detritus")){
  
  tmax<-dim(model@n)[1]
  
  rdi<-model@RDI[tmax-1, ]
  
  rdd <-   model@RDI[tmax-1, ] /model@Rratio[tmax-1, ]
  
  rmax<- model@params@species_params$r_max
  names(rmax)<- model@params@species_params$species
  
  dat<-cbind(rdi,rdd,rmax)
  dat[is.na(dat)]<-0
  
  rownames(dat)<- model@params@species_params$species1
  
  dat<- dat[!model@params@species_params$sex=="male", ]
  
  dat<- dat[!rownames(dat)%in%sp_exclude, ]
  
  return(dat)
  
}




















#species_params = model@params@species_params)
 return(rdd)




intakeScalar=tasm_warming_met@intTempScalar


library("mizerRewire")

test0<-mizerRewire::getM2(tasm_baserun, intakeScalar=tasm_baserun@intTempScalar[,,1]) 
test1<-mizerRewire::getM2(tasm_warming_int, intakeScalar=tasm_warming_int@intTempScalar[,,1]) 


plot(test1[101,5,] /test0[101,5,] )


mizerRewire::getRDD(tasm_warming_int, intakeScalar=tasm_warming_int@intTempScalar[,,1]) 

getRDI(params, model@n[tmax,,],model@n_pp[tmax,], model@n_bb[tmax,], model@n_aa[tmax,], model@intTempScalar[,,(tmax/dt)], model@metTempScalar[,,(tmax/dt)])

#get RDD

model<- tasm_warming_met
tmax=100
dt=.25

getRDD(model@params,model@n[tmax,,],model@n_pp[tmax,], 
       model@n_bb[tmax,], model@n_aa[tmax,], sex_ratio = 0.5, model@intTempScalar[,,(tmax/dt)], model@metTempScalar[,,(tmax/dt)])

model@params@species_params$r_max /getRDD(model@params,model@n[tmax,,],model@n_pp[tmax,], 
                                          model@n_bb[tmax,], model@n_aa[tmax,], sex_ratio = 0.5, model@intTempScalar[,,(tmax/dt)], model@metTempScalar[,,(tmax/dt)])


getRDI(params, model@n[tmax,,],model@n_pp[tmax,], model@n_bb[tmax,], model@n_aa[tmax,], model@intTempScalar[,,(tmax/dt)], model@metTempScalar[,,(tmax/dt)])






