
### Process all results together and melt into long format for ggplot
results <- list.files('results', pattern='boot_retro',
                      full.names=TRUE) %>% lapply(readRDS) %>%
  bind_rows() %>% pivot_longer(cols=-c('model', 'boot'),
                               names_to='metric',
                               values_to='rho') %>%
  ## Split names into three types of Rho's
  mutate(type=case_when(
           grepl('WoodHole', metric) ~ 'WoodsHole',
           grepl('AFSC', metric) ~ 'AFSC',
           TRUE ~ 'Normal'),
         ## Split names into three metrics across types
         metric=gsub('WoodHole_|AFSC_Hurtado_|.all', '',metric)) %>%
  ## the recruitment ones are not working?
  filter(metric!='Rec')

## Which Rho metric to use?
results_normal <- filter(results, type=='Normal')
results_afsc <- filter(results, type=='AFSC')
results_woodshole <- filter(results, type=='WoodsHole')
