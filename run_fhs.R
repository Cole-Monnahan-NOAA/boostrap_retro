### Run and compare models
library(tidyverse)
library(r4ss)
theme_set(theme_bw())
packageVersion('r4ss') #  '1.40.0'
source('code/run_retro.R')

## ### Run full model and generate 500 bootstrap data sets
## setwd('fhs')
## system('ss -nox -iprint 1000 -nohess')
## setwd('..')

## For each boostrap data set, run a retrospective analysis
Nreplicates <- 500
Npeels <- 10
peels <- 0:-Npeels

## Setup to run parallel, saving a single core free.
library(snowfall)
sfStop()
sfInit( parallel=TRUE, cpus=parallel::detectCores()-1)
sfExportAll()

### Run one in serial as a test
## test <- run_SS_boot_iteration(1, 'fhs', TRUE)
### Run 3 in parallel as a test
## results.list <- sfLapply(1:3, function(i)
##  run_SS_boot_iteration(boot=i, model.name='fhs', clean.files=TRUE))

### Run all bootstrap results. The clean.files argument is
### helpful b/c it's Nreplicates*Npeels SS runs which gets huge
### fast.
results.list <- sfLapply(1:Nreplicates, function(i)
 run_SS_boot_iteration(boot=i, model.name='fhs', clean.files=TRUE))

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
saveRDS(results, file=file.path('results', 'fhs_boot_retros.RDS'))

