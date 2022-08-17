
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


### old way of doing this
## ## Process the observed rhos
## rho_obs <- read.csv('results/rho_obs.csv') %>%
##   filter(metric!='Rec' & type=='AFSC')
## ## Duplicate for ggplot faceting
## rho_obs <- bind_rows(rho_obs, mutate(rho_obs, miller=FALSE))
## rho_obs <- filter(rho_obs, model %in% c("EBS_Pcod", "GOA_Pcod", "GOA_NRS")) %>%droplevels
## rho_obs_new <- read.csv('../ss-test-models/rho_obs_new.csv')
## rho_obs <- bind_rows(rho_obs, rho_obs_new)
## write.csv(rho_obs, file='results/rho_obs.csv', row.names=FALSE)

## get parameters for checking for biases, compared to original
## fit. This takes a long time b/c it reads in a lot of files.
ff <- list.files('runs', pattern='parameter_res',
                 full.names=TRUE, recursive=TRUE)
par.results <- ff %>% lapply(read.csv) %>% bind_rows()
## Only need this b/c of old bugs in get_res function. Delete
## next time a full run is done
par.results <- par.results %>%  rename(peels=peelsl) %>% select(-miller.1)

## only keep the models we're going to use
par.results$model %>% unique
par.results <- par.results %>%
  filter(model %in% c('EBS_Pcod3', "GOA_NRS", "GOA_Pcod_noprior"))
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
  filter(model %in% c('EBS_Pcod3', "GOA_NRS", "GOA_Pcod_noprior")) %>%
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
