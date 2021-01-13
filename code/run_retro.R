
#' Calculation the retrospective metrics for a single
#' bootstrapped data set. Designed to work with parallel
#' execution.
run_SS_boot_iteration <- function(boot, model.name,
                                  clean.files=TRUE){
  ## Some of these are global variables
  library(r4ss)
  dat <- SS_readdat(file.path('models', model.name,'data.ss_new'), verbose=TRUE, section=2+boot)
  wd <- file.path('runs', model.name, 'boots', paste0("boot_", boot))
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
  saveRDS(retroSummary, file=paste0(file.path(wd,'retroSummary.RDS')))
  saveRDS(retroModels, file=paste0(file.path(wd,'retroModels.RDS')))
  endyrvec <- retroSummary$endyrs + peels
  SSplotComparisons(retroSummary, endyrvec=endyrvec, png=TRUE, plot=FALSE,
                    plotdir=wd, legendlabels=paste("Data",peels,"years"))
  ## Calculate Mohn's rho
  rhos <- data.frame(model='BSAI_flathead', boot=boot,
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
