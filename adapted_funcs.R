


object<-tasm_baserun
species<-c("C_spectabilis")

highlight=1
max_age = 80
percentage = FALSE


function (object, species, max_age = 20, percentage = FALSE, 
          print_it = TRUE, highlight = NULL) 
{
  if (is(object, "MizerSim")) {
    sim <- object
    if (missing(species)) {
      species <- dimnames(sim@n)$sp
    }
    idx <- which(dimnames(sim@n)$sp %in% species)
    species <- dimnames(sim@n)$sp[idx]
    age <- seq(0, max_age, length.out = 50)
    ws <- array(dim = c(length(species), length(age)), dimnames = list(Species = species, 
                                                                       Age = age))
    
    
    
    
    g <- mizerRewire::getEGrowth(sim@params, sim@n[dim(sim@n)[1], , ], 
                    sim@n_pp[dim(sim@n)[1], ], sim@n_bb[dim(sim@n)[1], 
                                                        ], sim@n_aa[dim(sim@n)[1], ], sim@intTempScalar[, 
                                                                                                        , (dim(sim@temperature)[1])], sim@metTempScalar[, 
                                                                                                                                                        , dim(sim@temperature)[1]])
    for (j in 1:length(species)) {
      i <- idx[j]
      g_fn <- stats::approxfun(sim@params@w, g[i, ])
      myodefun <- function(t, state, parameters) {
        return(list(g_fn(state)))
      }
      ws[j, ] <- deSolve::ode(y = sim@params@species_params$w_min[i], 
                              times = age, func = myodefun)[, 2]
      if (percentage) {
        ws[j, ] <- ws[j, ]/sim@params@species_params$w_inf[i] * 
          100
      }
    }
    plot_dat <- reshape2::melt(ws)
    plot_dat$Species <- as.character(plot_dat$Species)
    p <- ggplot(plot_dat) + geom_line(aes(x = Age, y = value, 
                                          colour = Species, linetype = Species, size = Species))
    y_label <- if (percentage) 
      "Percent of maximum size"
    else "Size [g]"
    linesize <- rep(0.8, length(sim@params@linetype))
    names(linesize) <- names(sim@params@linetype)
    linesize[highlight] <- 1.6
    p <- p + scale_x_continuous(name = "Age [Years]") + scale_y_continuous(name = y_label) + 
      scale_colour_manual(values = sim@params@linecolour) + 
      scale_linetype_manual(values = sim@params@linetype) + 
      scale_size_manual(values = linesize)
    if (length(species) == 1 && !percentage) {
      w_inf <- sim@params@species_params$w_inf[idx[1]]
      p <- p + geom_hline(yintercept = w_inf) + annotate("text", 
                                                         0, w_inf, vjust = -1, label = "Maximum")
      w_mat <- sim@params@species_params$w_mat[idx[1]]
      p <- p + geom_hline(yintercept = w_mat) + annotate("text", 
                                                         0, w_mat, vjust = -1, label = "Maturity")
      if (all(c("a", "b", "k_vb") %in% names(sim@params@species_params))) {
        a <- sim@params@species_params$a[idx[1]]
        b <- sim@params@species_params$b[idx[1]]
        k_vb <- sim@params@species_params$k_vb[idx[1]]
        L_inf <- (w_inf/a)^(1/b)
        vb <- a * (L_inf * (1 - exp(-k_vb * age)))^b
        dat <- data.frame(x = age, y = vb)
        p <- p + geom_line(data = dat, aes(x = x, y = y))
      }
    }
    return(p)
  }
  else {
    sim <- project(object, t_max = 1)
    params <- object
    if (missing(species)) {
      species <- dimnames(params@initial_n)$sp
    }
    idx <- which(dimnames(params@initial_n)$sp %in% species)
    species <- dimnames(params@initial_n)$sp[idx]
    age <- seq(0, max_age, length.out = 50)
    ws <- array(dim = c(length(species), length(age)), dimnames = list(Species = species, 
                                                                       Age = age))
    g <- getEGrowth(params, params@initial_n, params@initial_n_pp, 
                    params@initial_n_bb, params@initial_n_aa, intakeScalar = sim@intTempScalar[, 
                                                                                               , (dim(sim@temperature)[1])], metScalar = sim@metTempScalar[, 
                                                                                                                                                           , (dim(sim@temperature)[1])])
    for (j in 1:length(species)) {
      i <- idx[j]
      g_fn <- stats::approxfun(params@w, g[i, ])
      myodefun <- function(t, state, parameters) {
        return(list(g_fn(state)))
      }
      ws[j, ] <- deSolve::ode(y = params@species_params$w_min[i], 
                              times = age, func = myodefun)[, 2]
      if (percentage) {
        ws[j, ] <- ws[j, ]/params@species_params$w_inf[i] * 
          100
      }
    }
    plot_dat <- reshape2::melt(ws)
    plot_dat$Species <- as.character(plot_dat$Species)
    p <- ggplot(plot_dat) + geom_line(aes(x = Age, y = value, 
                                          colour = Species, linetype = Species, size = Species))
    y_label <- if (percentage) 
      "Percent of maximum size"
    else "Size [g]"
    linesize <- rep(0.8, length(params@linetype))
    names(linesize) <- names(params@linetype)
    linesize[highlight] <- 1.6
    p <- p + scale_x_continuous(name = "Age [Years]") + scale_y_continuous(name = y_label) + 
      scale_colour_manual(values = params@linecolour) + 
      scale_linetype_manual(values = params@linetype) + 
      scale_size_manual(values = linesize)
    if (length(species) == 1 && !percentage) {
      w_inf <- params@species_params$w_inf[idx[1]]
      p <- p + geom_hline(yintercept = w_inf) + annotate("text", 
                                                         0, w_inf, vjust = -1, label = "Maximum")
      w_mat <- params@species_params$w_mat[idx[1]]
      p <- p + geom_hline(yintercept = w_mat) + annotate("text", 
                                                         0, w_mat, vjust = -1, label = "Maturity")
      if (all(c("a", "b", "k_vb") %in% names(params@species_params))) {
        a <- params@species_params$a[idx[1]]
        b <- params@species_params$b[idx[1]]
        k_vb <- params@species_params$k_vb[idx[1]]
        L_inf <- (w_inf/a)^(1/b)
        vb <- a * (L_inf * (1 - exp(-k_vb * age)))^b
        dat <- data.frame(x = age, y = vb)
        p <- p + geom_line(data = dat, aes(x = x, y = y))
      }
    }
    return(p)
  }
}













###############################################################################

mizerRewire::plotBiomass(SEAustralia_baserun)

test0<-mizerRewire::getM2(SEAustralia_baserun, 
                          intakeScalar=SEAustralia_baserun@intTempScalar[,,1]) 


test1<-mizerRewire::getM2(SEAustralia_warming_met, 
                          intakeScalar=SEAustralia_warming_met@intTempScalar[,,1]) 

sim<-SEAustralia_warming_met

sim@n[dim(sim@n)[1], , ]

mizerRewire::plotGrowthCurves(SEAustralia_warming_met, species="myctophids")


plot(test1[200,1,])


prop<-test1[,,200] / test0[,,100]

datp<-melt(prop)

plot(value~X1, data=datp)

model<- SEAustralia_warming_met
tmax=100
dt=.25

mizerRewire::getRDD(model@params,model@n[tmax,,],model@n_pp[tmax,], 
                    model@n_bb[tmax,], model@n_aa[tmax,], sex_ratio = 0.5, model@intTempScalar[,,(tmax/dt)], model@metTempScalar[,,(tmax/dt)])

mizerRewire::getRDI(model@params, model@n[tmax,,],model@n_pp[tmax,], model@n_bb[tmax,], model@n_aa[tmax,], model@intTempScalar[,,(tmax/dt)], model@metTempScalar[,,(tmax/dt)])




plotBiomass(SEoz[[8]])









getEGrowthCami<-function (object, n, n_pp, n_bb, n_aa, intakeScalar, metScalar, 
          e_repro = mizerRewire::getERepro(object, n = n, n_pp = n_pp, n_bb = n_bb, 
                              n_aa = n_aa, intakeScalar = intakeScalar, metScalar = metScalar), 
          e = mizerRewire::getEReproAndGrowth(object, n = n, n_pp = n_pp, n_bb = n_bb, 
                                 n_aa = n_aa, intakeScalar = intakeScalar, metScalar = metScalar)) 
{
  if (!all(dim(e_repro) == c(nrow(object@species_params), length(object@w)))) {
    stop("e_repro argument must have dimensions: no. species (", 
         nrow(object@species_params), ") x no. size bins (", 
         length(object@w), ")")
  }
  if (!all(dim(e) == c(nrow(object@species_params), length(object@w)))) {
    stop("e argument must have dimensions: no. species (", 
         nrow(object@species_params), ") x no. size bins (", 
         length(object@w), ")")
  }
  
  e[e < 0] <- 0
  
  e_growth <- (e - e_repro)*0.6 # * object@species_params$alpha_g
 
   return(e_growth)
}















