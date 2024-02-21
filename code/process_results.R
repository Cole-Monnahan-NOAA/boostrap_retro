
message("Processing GOA pollock results: rhos, time series and par estimates...")
## Read in all final results, including those not necessarily
## in reps since they could have been run earlier
ff <- list.files(path='runs/GOA_pollock',
                 pattern="rho.csv", recursive=TRUE,
                 full.names=TRUE)
results <- lapply(ff, read.csv) %>% bind_rows()
saveRDS(results, file=file.path('results', 'GOA_pollock_retros.RDS' ))
ff <- list.files(path='runs/GOA_pollock',
                 pattern="ts.results.csv", recursive=TRUE,
                 full.names=TRUE)
results <- lapply(ff, read.csv) %>% bind_rows()
saveRDS(results, file=file.path('results', 'GOA_pollock_ts_results.RDS' ))
ff <- list.files(path='runs/GOA_pollock',
                 pattern="par.results.csv", recursive=TRUE,
                 full.names=TRUE)
results <- lapply(ff, read.csv) %>% bind_rows()
saveRDS(results, file=file.path('results', 'GOA_pollock_par_results.RDS' ))
message("..done! files in results folder")



### Process all results together and melt into long format for ggplot
results <- list.files('results', pattern='boot_retro',
                      full.names=TRUE) %>%
  lapply(FUN=function(x) cbind(readRDS(x))) %>%
  bind_rows() %>%
  pivot_longer(cols=-c('model', 'boot', 'miller', 'baseyear'),
               names_to='metric',
               values_to='rho') %>%
  ## Split names into three types of Rho's
  mutate(type=case_when(
           grepl('WoodHole', metric) ~ 'WoodsHole',
           grepl('AFSC', metric) ~ 'AFSC',
           TRUE ~ 'Normal'),
         baseyear=factor(baseyear),
         ## Split names into three metrics across types
         metric=gsub('WoodHole_|AFSC_Hurtado_|.all', '',metric))
## %>%
  ## the recruitment ones are not working?
 ##  filter(metric!='Rec')

## Which Rho metric to use?
results_normal <- filter(results, type=='Normal' & boot>0)
results_afsc <- filter(results, type=='AFSC' & boot>0)
results_woodshole <- filter(results, type=='WoodsHole' & boot>0)

## boot 0 is special code to run the real data so split this off
rho_obs <- results %>% filter(boot==0 & type=='AFSC')


results_pk <- list.files('results', pattern='GOA_pollock',
                      full.names=TRUE) %>%
  lapply(FUN=function(x) cbind(readRDS(x))) %>%
  bind_rows() %>%
  pivot_longer(cols=-c('model', 'boot', 'miller', 'baseyear'),
               names_to='metric',
               values_to='rho')
rho_obs_pk<- results_pk %>% filter(boot==0)
ggplot(results_pk, aes(factor(baseyear), rho)) + geom_violin() +
  facet_wrap('miller', nrow=2, scales='free') +
  geom_point(data=rho_obs_pk, color=2)

results_pk %>% filter(!miller) %>% arrange(desc(abs(rho)))

reps <- readRDS("runs/GOA_pollock/boot_0/retroModels.RDS")

ii <- lapply(c(1,2,3,6), function(i)
  cbind(survey=i, mymelt(reps, paste0('Survey_',i,'_expected_index')))) %>%
  bind_rows
qq <- lapply(c(1,2,3,6), function(i)
 cbind(survey=i, mymelt(reps, paste0('Survey_',i,'_q')))) %>%
  bind_rows

rr <- mymelt(reps, 'Recruits')
ggplot(ii, aes(year, value, group=factor(model))) + geom_line() +
  facet_wrap('survey') + scale_y_log10()

ggplot(qq, aes(year, value, group=factor(model))) + geom_line() +
  facet_wrap('survey')

ggplot(rr, aes(year, value, color=(model), group=factor(model))) + geom_line()



out <- lapply(0:20, meltindices) %>% bind_rows()

meltindices <- function(boot) {
  dd <- read_pk_dat(filename='goa_pk.dat', path=paste0('runs/GOA_pollock/boot_',boot), writedat=TRUE)
 rbind(with(dd, data.frame(survey=1, year=srvyrs1, index=indxsurv1)),
                 with(dd, data.frame(survey=2, year=srvyrs2, index=indxsurv2)),
                 with(dd, data.frame(survey=3, year=srvyrs3, index=indxsurv3)),
                 with(dd, data.frame(survey=6, year=srvyrs6,
                                     index=indxsurv6))) %>% cbind(boot=boot)
}

ggplot(out, aes(year,index, group=boot, color=factor(boot))) + geom_line() + facet_wrap('survey')


if(FALSE){
  message("skipping MLE calculations this time")
} else {
  message("Starting MLE processing, very slow...")
  ## get parameters for checking for biases, compared to original
  ## fit. This takes a long time b/c it reads in a lot of files.
  ff <- list.files('runs', pattern='parameter_res',
                   full.names=TRUE, recursive=TRUE)
  par.results <- ff %>% lapply(read.csv) %>% bind_rows()
  ## Only need this b/c of old bugs in get_res function. Delete
  ## next time a full run is done
  par.results <- par.results %>%  rename(peels=peelsl) %>%
    select(-miller.1) %>%
    group_by(model, par, assess_yr, peels, miller) %>%
    mutate(re=(value-value[boot==0])/value[boot==0])
  saveRDS(par.results, file='results/par.results.RDS')

  ## get timeseries
  ff <- list.files('runs', pattern='ts_res',
                   full.names=TRUE, recursive=TRUE)
  ts.results <- ff %>% lapply(read.csv) %>% bind_rows()
  str(ts.results)

  ## again old bugs so update as needed later
  ts.results <- ts.results[,1:8]

  ts.results$model %>% unique
  ## some quick processing, any year after keep_yr has no data so
  ## throw it out
  ts <- ts.results %>%
    group_by(model,name, year, peel,miller) %>%
    mutate(re=(value-value[boot==0])/value[boot==0],keep_yr=assess_yr-peel) %>%
    filter(year<=keep_yr) %>% ungroup
  saveRDS(ts, file='results/ts.results.RDS')

  ## test <- filter(ts.results, name=='ssb' & model=='EBS_Pcod') %>%
  ##   filter(year<=assess_yr-peel)
  ## test2 <- test %>% group_by(model, assess_yr, year, peel) %>%
  ##   mutate(re=(value-value[boot==0])/value)
  ## ggplot(test2, aes(year, re,
  ##                   group=interaction(boot,peel))) +
  ##   geom_line() + facet_wrap('peel')
}

## Check for full replicates for an arbitrary base year
check <- results_afsc %>%
  filter(metric=='SSB' & baseyear==2012) %>%
  group_by(model,miller) %>%
  summarize(count=n(), .groups='drop')
print(check)
