
## Add in the real data rhos
rho <- readRDS('results/FHS_retro_rhos.RDS') %>% as.data.frame() %>%
  mutate(model='BSAI_flathead', boot=-1)
results <- bind_rows(rho, results)
results.long <- results %>% pivot_longer(-c(model, boot)) %>%
  mutate(type=case_when(
           grepl('WoodHole', name) ~ 'WoodsHole',
           grepl('AFSC', name) ~ 'AFSC_Hurtado',
           TRUE ~ 'Normal'),
         name=gsub('WoodHole_|AFSC_Hurtado_|.all', '',name))


g <- ggplot(filter(results.long, boot>0), aes(y=value, x=type)) + geom_violin() +
  facet_wrap('name') + geom_hline(yintercept=0, color='blue') +
  geom_point(data=filter(results.long, boot==-1), color='red') +
  labs(y='Rho')
ggsave('plots/fhs_all.png', g, width=7, height=5)
## g <- filter(results.long, boot>0 & type=='AFSC_Hurtado') %>%
##   ggplot(aes(y=value, x=name)) + geom_violin() +
##   geom_hline(yintercept=0, color='blue') +
##   geom_point(data= filter(results.long, boot==-1 &
##   type=='AFSC_Hurtado'), color='red')  + labs(y='rho')
## ggsave('plots/fhs_afsc.png', g, width=7, height=5)

g <- filter(results.long, boot>0 & type=='AFSC_Hurtado') %>%
  ggplot(aes(x=value)) + geom_histogram(bins=20) +
  facet_wrap('name', scales='free')+
  geom_vline(xintercept=0) +
  geom_vline(data= filter(results.long, boot==-1 & type=='AFSC_Hurtado'),
             aes(xintercept=value), color='red')  +
  labs(y='Frequency', x='AFSC rho')
ggsave('plots/fhs_afsc.png', g, width=7, height=5)

## ## Calculate percentiles
## obs <- filter(results, boot==-1) %>% select(-boot, -model)
## exp <- filter(results, boot!=-1) %>% select(-boot, -model)
## sapply(names(obs), function(i) round(mean(obs[,i]>exp[,i]),3))
