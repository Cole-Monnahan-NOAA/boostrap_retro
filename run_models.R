### Run and compare models
library(tidyverse)
library(r4ss)
library(snowfall)
library(ggplot2)
theme_set(theme_bw())
packageVersion('r4ss') #  '1.42.0'
source('code/functions.R')

## For each boostrap data set, run a retrospective analysis
Nreps <- 500
Npeels <- 10
peels <- 0:-Npeels

## Setup to run parallel, saving a single core free.
cpus <- parallel::detectCores()-2
sfStop()
sfInit( parallel=TRUE, cpus=cpus)
sfExportAll()

### Run full in parallel for all models. This assumes that the
### starter file was modifed to produce 500 replicates (502
### total) and was run. The "blank" files should also have
### started file modified to reduce output to speed things
### up. Finally, the data input must be named data.ss in the main
### files, to facilitate the Miller sampling scheme.

## ## Run one in serial as a test
## test <- run_SS_boot_iteration(1, 'GOA_Pcod', TRUE)
run_model(Nreps, model.name='EBS_Pcod')
run_model(Nreps, model.name='fhs')
run_model(Nreps, model.name='GOA_NRS')
run_model(Nreps, model.name='GOA_SRS')
run_model(Nreps, model.name='BSAI_GT')

## Rerun using the Miller approach
run_model(100, model.name='BSAI_FHS', miller=TRUE)


Npeels <- 5
run_SS_boot_iteration(999, 'BSAI_FHS', FALSE, TRUE)
run_SS_boot_iteration(999, 'BSAI_GT', FALSE, TRUE)
run_SS_boot_iteration(999, 'GOA_NRS', FALSE, TRUE)
run_SS_boot_iteration(999, 'GOA_SRS', FALSE, TRUE)
run_SS_boot_iteration(999, 'GOA_Pcod', FALSE, TRUE)
run_SS_boot_iteration(999, 'EBS_Pcod', FALSE, TRUE)


source('code/process_results.R')

source('code/make_plots.R')
