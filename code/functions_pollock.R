
#' Wrapper to run and save a single model
run_pollock_model <- function(reps, datlist, replist, model.name, miller=FALSE, clean.files=TRUE){
  ## Run all bootstrap results. The clean.files argument is
  ## helpful b/c it's Nreplicates*Npeels SS runs which gets huge
  ## fast.
  trash <- sfLapply(reps, function(i)
    run_pollock_boot_iteration(boot=i, datlist=datlist, replist=replist, model.name=model.name, clean.files=clean.files,
                               miller=miller))
  ## It fails on some. Not sure why this is happening. But hack is
  ## to loop through and figure out which failed and simply rerun
  ## them. Try 5 loops and break out if they all worked.
  tmp <- ifelse(miller, 'results_miller_rho', 'results_rho')
  for(i in 1:1){
    ff <- list.files(path=file.path('runs', model.name),
                     pattern=tmp, recursive=TRUE, full.names=TRUE)
    results <- lapply(ff, read.csv) %>% bind_rows()
    ind <- which(!reps %in% results$boot)
    if(length(ind)>0){
      warning("Rerunning failed ", model.name, " models= ", paste(ind, collapse=','))
      ## trash <- sfLapply(reps[ind], function(i)
      ##   run_pollock_boot_iteration(boot=i, datlist=datlist, replist=replist, model.name=model.name, clean.files=clean.files,
      ##                              miller=miller))
    }
  }
  message("Replicates finished for model=", model.name,
          " and miller=", miller)
}





run_pollock_boot_iteration <- function(boot, model.name='GOA_pollock',
                                       clean.files=TRUE, miller=FALSE,
                                       datlist, replist){
  ## Some of these are global variables
  library(GOApollock)
  if(!file.exists(file.path('models',model.name))) stop("model not found")
  if(!miller){
    wd <- file.path('runs', model.name, paste0("boot_", boot))
  } else {
    wd <- file.path('runs', model.name, paste0("millerboot_", boot))
  }
  message("Starting boot=", boot, " in directory ", wd)
  #directory for time series and parameter results
  res.wd<-file.path('results')
  ## Prepare folder to run this iteration
  dir.create(wd, showWarnings=TRUE, recursive=TRUE)
  blank.files <- list.files(file.path('models', model.name,'blank'), full.names=TRUE)
  test <- file.copy(from=blank.files, to=wd, overwrite=TRUE)
  if(!all(test)){
    message(paste(blank.files[!test], collapse= '\n'))
    stop("Some blank files failed to copy for iteration ", boot)
  }

  ## boot==0 is the original data, so skip resampling the data
  if(boot==0){
    ## The original data, whether miller or not
    write_dat(datlist=datlist, fileout='goa_pk.dat', path=wd)
  } else {
    ## The two types of bootstraps are implemented here
    if(miller){
      dat <- sim_dat(datlist=datlist, replist=replist,
                     fileout='goa_pk.dat', path=wd,
                     type='data')
    } else {
      dat <- sim_dat(datlist=datlist, replist=replist,
                     fileout='goa_pk.dat', path=wd,
                     type='model')
    }
  }
  newfiles <- list.files(wd, full.names=TRUE) # mark for deletion

  old.wd <- getwd()
  on.exit(setwd(old.wd))
  setwd(wd)
  ts_res<-data.frame();
  mle_res<-data.frame();
  reps <- list(); k <- 1
  for(pl in abs(peels)){
    ## Delete it so if next loop fails we'll know
    trash <- suppressWarnings(file.remove('goa_pk.rep'))
    system(paste('goa_pk -nohess -retro',pl, '-display 0'))
    system(paste('goa_pk -nohess -retro',pl, '-display 0 -binp goa_pk.bar'))
    if(file.exists('goa_pk.rep'))
      reps[[k]] <- read_pk_rep(version=pl, endyr=2022-pl)
    else
      warning("failed with boot=", boot, ", peel=", pl)
    ##   browser()
    #save time series results
    #pull quants of interest
    ssb_ts = as.vector(unlist(reps[[k]]['Expected_spawning_biomass']))
    ts_f = as.vector(unlist(reps[[k]]['Fishing_mortalities']))
    recr_ts = as.vector(unlist(reps[[k]]['Recruits']))
    yrs=as.vector(unlist(reps[[k]]['years']))
    ayr=max(as.numeric(unlist(reps[[k]]['years'])))+pl
    ##Create data frames
    ssb <- data.frame(assess_yr=ayr, name='ssb', peel=pl,
                      year=yrs, value=ssb_ts, boot=boot,
                      model=model.name, miller=miller)
    ap_f <- data.frame(assess_yr=ayr,
                       name='f', peel=pl, year=yrs,
                       value=ts_f, boot=boot,
                       model=model.name, miller=miller)
    recr <- data.frame(assess_yr=ayr, name='recr',
                       peel=pl, year=yrs, value=recr_ts,
                       boot=boot, model=model.name,
                       miller=miller)
    ##Combine all timeseries results
    ts_res <- bind_rows(ts_res, ssb, ap_f, recr)
    nms <- c('mean_log_recruit', 'log_slp1_fsh_mean',
             'inf1_fsh_mean', 'log_slp2_fsh_mean', 'inf2_fsh_mean',
             'log_slp2_srv1', 'inf2_srv1',
             'log_slp1_srv2', 'inf1_srv2',
             'log_slp1_srv3', 'inf1_srv3',
             'log_q1_mean', 'log_q2_mean', 'log_q3_mean',
             'log_q4', 'log_q5', 'log_q6')
    suppressWarnings(mles <- R2admb::read_pars('goa_pk', warn_nonstd_rep = FALSE))
    mles <- data.frame(assess_yr=ayr, peel=pl, boot=k,
                       par=names(mles$coefficients),
                       value=mles$coefficients,
                       model=model.name, miller=miller,
                       maxgrad=mles$maxgrad) %>%
      filter(par %in% nms) %>%
      select(assess_yr,par,peel,value,boot,model,miller,maxgrad)
    mle_res = bind_rows(mle_res, mles)
    k <- k+1
  }
  write.csv(ts_res, file="ts.results.csv", row.names=FALSE)
  write.csv(mle_res, file="par.results.csv", row.names=FALSE)
  setwd(old.wd)
  ## Run retro for this bootstrap one
  saveRDS(reps, file=paste0(file.path(wd,'retroModels.RDS')))
  pdf(file=file.path(wd, 'retro_plots.pdf'), onefile=TRUE,
      width=7, height=9)
  rhos_f <- list()
  rhos <- list(); k <- 1
  ## Read in and save timeseries and parameter estimates for all
  ## peels: TO DO!!
  library(ggplot2)
  library(dplyr)
  theme_set(theme_bw())
  if(length(peels)>8){
  for(peelyr in 0:7){
    peels.tmp <- peels[(1+peelyr):(peelyr+8)] # peel #
    peels.ind <- which(peels %in% peels.tmp)  # peel index
    ## Do some heavy processing to plot quickly
    ## Pluck out a subset of 7 peels and calculate rho
    f <- mymelt(reps[peels.ind], 'Fishing_mortalities') %>%
      rename(peel=model, F_mort=value) %>%
      filter(peel %in% abs(peels.tmp)) %>%
      group_by(year) %>%
      mutate(peel=peel-min(peel)) %>%
      mutate(F_Pct_Diff=100*(F_mort-F_mort[peel==min(peel)])/F_mort[peel==min(peel)]) %>%
      ungroup()

    rho_f <- f %>%
      filter(max(year) - peel == year & year != max(year)) %>%
      pull(F_Pct_Diff)
    rho_f <- mean(rho_f/100)
    rho_f.lab <- paste0("Mohn's rho= ", round(mean(rho_f/100),3))
    #plot Fishing mort rho
    thisyear <- 2022
    g1 <- ggplot(f, aes(year, F_mort, group=peel, color=factor(peel))) + geom_line() +
      labs(x=NULL, y='Fishing mortality', color='Peel')
    g1 <- g1 + geom_point(data=filter(f, thisyear-peel==year), size=2) +
      theme(legend.position='none') + annotate('label', x=2010,y=.5, label=rho_f.lab)
    g2 <- ggplot(f, aes(year, F_Pct_Diff, group=peel, color=factor(peel))) + geom_line() +
      labs(x=NULL, y='Percent difference from peel 0', color=NULL)
    g2 <- g2 + geom_point(data=filter(f, thisyear-peel==year), size=2)+
      theme(legend.position=c(.35,.85)) +
      guides(color=guide_legend(nrow=2))
    g1 <- g1+xlim(1970,2022) +labs(title=paste('Base year=', 2022-peelyr))
    g2 <- g2+xlim(1970,2022)
    g <- cowplot::plot_grid(g1,g2, nrow=2)
    print(g)
    ssb <- mymelt(reps[peels.ind], 'Expected_spawning_biomass') %>%
      rename(peel=model, SSB=value) %>%
      filter(peel %in% abs(peels.tmp)) %>%
      group_by(year) %>%
      mutate(peel=peel-min(peel)) %>%
      mutate(SSB_Pct_Diff=100*(SSB-SSB[peel==min(peel)])/SSB[peel==min(peel)]) %>%
      ungroup()
    rho <- ssb %>%
      filter(max(year) - peel == year & year != max(year)) %>% pull(SSB_Pct_Diff)
    rhos[[k]] <- data.frame(model='GOApollock',
                            baseyear=2022-peelyr, miller=miller,
                            boot=boot, SSB=mean(rho/100),
                            F=rho_f)
    rho.lab <- paste0("Mohn's rho= ", round(mean(rho/100),3))

    ## Plot it
    thisyear <- 2022
    g1 <- ggplot(ssb, aes(year, SSB, group=peel, color=factor(peel))) + geom_line() +
      labs(x=NULL, y='Spawning Biomass (million t)', color='Peel')
    g1 <- g1 + geom_point(data=filter(ssb, thisyear-peel==year), size=2) +
      theme(legend.position='none') + annotate('label', x=2010,y=.5, label=rho.lab)
    g2 <- ggplot(ssb, aes(year, SSB_Pct_Diff, group=peel, color=factor(peel))) + geom_line() +
      labs(x=NULL, y='Percent difference from peel 0', color=NULL)
    g2 <- g2 + geom_point(data=filter(ssb, thisyear-peel==year), size=2)+
      theme(legend.position=c(.35,.85)) +
      guides(color=guide_legend(nrow=2))
    g1 <- g1+xlim(1970,2022) +labs(title=paste('Base year=', 2022-peelyr))
    g2 <- g2+xlim(1970,2022)
    g <- cowplot::plot_grid(g1,g2, nrow=2)
    print(g)
    k <- k+1
  }
  }
  dev.off()
  tmp <- ifelse(miller, 'results_miller_rho.csv', 'results_rho.csv')
  rhos <- do.call(rbind, rhos)
  write.csv(x=rhos, file=file.path(wd, tmp), row.names=FALSE)
  if(clean.files){
    trash <-
      file.remove(list.files(file.path(wd),
                             pattern='.RDS|mceval.dat|.bar|.r0|.b0|.p0|.exe|.par|.rep|.tpl|.log',
                             full.names=TRUE))
  }
  return(rhos)
}
