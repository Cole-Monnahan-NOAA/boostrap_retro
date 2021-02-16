
#' Calculation the retrospective metrics for a single
#' bootstrapped data set. Designed to work with parallel
#' execution.
run_SS_boot_iteration <- function(boot, model.name,
                                  clean.files=TRUE){
  ## Some of these are global variables
  library(r4ss)
  dat <- SS_readdat(file.path('models', model.name,'data.ss_new'), verbose=TRUE, section=2+boot)
  wd <- file.path('runs', model.name, paste0("boot_", boot))
  dir.create(wd, showWarnings=FALSE, recursive=TRUE)
  blank.files <- list.files(file.path('models', model.name,'blank'), full.names=TRUE)
  test <- file.copy(from=blank.files, to=wd, overwrite=TRUE)
  if(!all(test))
    stop("Some blank files failed to copy for iteration ", boot)
  ## Write new data
  SS_writedat(dat, outfile=paste0(wd, '/data.ss'), verbose=FALSE,
              overwrite=TRUE)
  ## Run retro for this bootstrap one
  SS_doRetro(masterdir=getwd(), oldsubdir=wd,
             newsubdir=paste0(wd, '/retros'),
             years=peels, extras='-nohess -nox -iprint 1000')
  dirvec <- file.path(paste0(wd, '/retros'), paste("retro",peels,sep=""))
  if(length(dirvec)!=Npeels+1)
    stop("Some retro runs missing in iteration ", boot)
  retroModels <- SSgetoutput(dirvec=dirvec)
  retroSummary <- SSsummarize(retroModels)
  if(model.name=='BSAI_GT') {
    ## What is going on here?? hack to fix error
    retroSummary$startyrs <- rep(1961, length(peels))
  }
  saveRDS(retroSummary, file=paste0(file.path(wd,'retroSummary.RDS')))
  saveRDS(retroModels, file=paste0(file.path(wd,'retroModels.RDS')))
  endyrvec <- retroSummary$endyrs + peels
  SSplotComparisons(retroSummary, endyrvec=endyrvec, png=TRUE, plot=FALSE,
                    plotdir=wd, legendlabels=paste("Data",peels,"years"))
  ## Calculate Mohn's rho
  rhos <- data.frame(model=model.name, boot=boot,
                     SSmohnsrho(retroSummary, endyrvec=endyrvec))
  write.csv(x=rhos, file=file.path(wd, 'results_rho.csv'),
            row.names=FALSE)
  if(clean.files){
    unlink(file.path(wd, 'retros'), recursive=TRUE)
    trash <-
      file.remove(list.files(file.path(wd), pattern='.exe', full.names=TRUE))
  }
  return(rhos)
}

#' Wrapper to run and save a single model
#' results.list <- sfLapply(1:3, function(i)
#'  run_SS_boot_iteration(boot=i, model.name='fhs', clean.files=TRUE))

run_model <- function(Nreplicates, model.name){
  ## Run all bootstrap results. The clean.files argument is
  ## helpful b/c it's Nreplicates*Npeels SS runs which gets huge
  ## fast.
  results.list <- sfLapply(1:Nreplicates, function(i)
    run_SS_boot_iteration(boot=i, model.name=model.name, clean.files=TRUE))

  ## It fails on some. Not sure why this is happening. But hack is
  ## to loop through and figure out which failed and simply rerun
  ## them. Try 5 loops and break out if they all worked.
  for(i in 1:5){
    ff <- list.files(pattern='results_rho', recursive=TRUE)
    results <- lapply(ff, read.csv) %>% bind_rows()
    ind <- which(!1:Nreplicates %in% results$boot)
    if(length(ind)>0){
      message("Rerunning failed models=", paste(ind, collapse=' '))
      test <- sfLapply(ind, run_SS_retro)
    } else {
      message("All replicates finished")
      break
    }
  }

  ## Read in all final results
  ff <- list.files(pattern='results_rho', recursive=TRUE)
  results <- lapply(ff, read.csv) %>% bind_rows()
  saveRDS(results, file=file.path('results', paste0(model.name,'_boot_retros.RDS')))
}
