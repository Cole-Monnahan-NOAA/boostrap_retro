
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
  ## Read in all final results, including those not necessarily
  ## in reps since they could have been run ea
  ff <- list.files(path=file.path('runs', model.name),
                   pattern=tmp, recursive=TRUE, full.names=TRUE)
  results <- lapply(ff, read.csv) %>% bind_rows()
  f <- paste0(model.name,ifelse(miller, '_millerboot_retros.RDS',
                                '_boot_retros.RDS'))
  saveRDS(results, file=file.path('results', f))
  message("All replicates finished for model=", model.name,
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
    if(!miller){
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
  reps <- list(); k <- 1
  for(pl in abs(peels)){
    trash <- suppressWarnings(file.remove('goa_pk.rep'))
    system(paste('goa_pk -retro',pl, '-nohess -display 0'))
    if(file.exists('goa_pk.rep'))
      reps[[k]] <- read_pk_rep(version=pl, endyr=2022-pl)
    else
      warning("failed with boot=", boot, ", peel=", pl)
    k <- k+1
  }
  setwd(old.wd)
  clean_pk_dir(wd)
  ## Run retro for this bootstrap one
  saveRDS(reps, file=paste0(file.path(wd,'retroModels.RDS')))
  pdf(file=file.path(wd, 'retro_plots.pdf'), onefile=TRUE,
      width=7, height=9)
  rhos <- list(); k <- 1
  ## Read in and save timeseries and parameter estimates for all
  ## peels: TO DO!!
  library(ggplot2)
  theme_set(theme_bw())
  for(peelyr in 0:7){
    peels.tmp <- peels[(1+peelyr):(peelyr+8)] # peel #
    peels.ind <- which(peels %in% peels.tmp)  # peel index
    ## Do some heavy processing to plot quickly
    ## Pluck out a subset of 7 peels and calculate rho
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
                            baseyear=2022-peelyr, miller=miller, boot=boot, SSB=mean(rho/100))
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
  dev.off()
  tmp <- ifelse(miller, 'results_miller_rho.csv', 'results_rho.csv')
  rhos <- do.call(rbind, rhos)
  write.csv(x=rhos, file=file.path(wd, tmp), row.names=FALSE)
  if(clean.files){
    unlink(file.path(wd, 'retros'), recursive=TRUE)
    trash <- file.remove(file.path(wd, 'retroModels.RDS'))
    trash <-
      file.remove(list.files(file.path(wd), pattern='.exe|.par|.rep|.dat|.tpl', full.names=TRUE))
    file.remove(newfiles)
  }
  return(rhos)
}
