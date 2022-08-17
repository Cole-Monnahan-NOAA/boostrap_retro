
## For each boostrap data set, run a retrospective analysis
Nreps <- 500
reps <- 0:Nreps # 0 is special code for original data
Npeels <- 14
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
## test <- run_SS_boot_iteration(1, 'EBS_Pcod', TRUE)


## run_model(reps, model.name='EBS_Pcod')
## run_model(reps, model.name='EBS_Pcod', miller=TRUE)
### EBS_Pcod3 is the original model but switched away from the
## D-M likelhiood. Did this by updating input N for comps in the
## original data file (based on D-M fit from the bootstrap data)
## and then switched to the multinomial. See the ebs_comparison
## folder for what changed when this happened.
run_model(reps, model.name='EBS_Pcod3')
run_model(reps, model.name='EBS_Pcod3', miller=TRUE)
## run_model(reps, model.name='GOA_Pcod_prior')
## run_model(reps, model.name='GOA_Pcod_prior', miller=TRUE)
run_model(reps, model.name='GOA_Pcod_noprior')
run_model(reps, model.name='GOA_Pcod_noprior', miller=TRUE)
run_model(reps, model.name='GOA_NRS')
run_model(reps, model.name='GOA_NRS', miller=TRUE)
