
### Process all results together and melt into long format for ggplot
results <- list.files('results', pattern='boot_retro',
                      full.names=TRUE) %>% lapply(readRDS) %>%
  bind_rows() %>% pivot_longer(cols=-c('model', 'boot', 'miller', 'baseyear'),
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
