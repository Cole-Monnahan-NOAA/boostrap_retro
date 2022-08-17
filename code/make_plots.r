if(!exists('results')) stop('run code/process_results.R first')

### Quick plots
g0 <- ggplot(results, aes(x=metric, y=rho)) +
  geom_hline(yintercept=0, color='red') +
  geom_violin() +
  facet_grid(model~type, scales='free_y')
ggsave('plots/null_dist_all.png', g0, width=7, height=5)
g1 <- ggplot(results_normal, aes(x=metric, y=rho)) +
  geom_hline(yintercept=0, color='red') +
  geom_violin() +
  facet_wrap('model')
ggsave('plots/null_dist_normal.png', g1, width=7, height=5)
g2 <- ggplot(results_afsc, aes(x=metric, y=rho)) +
  geom_hline(yintercept=0, color='red') +
  geom_violin() +
  facet_wrap('model')
ggsave('plots/null_dist_afsc.png', g2, width=7, height=5)
g3 <- ggplot(results_woodshole, aes(x=metric, y=rho)) +
  geom_hline(yintercept=0, color='red') +
  geom_violin() +
  facet_wrap('model')
ggsave('plots/null_dist_woodshole.png', g3, width=7, height=5)




## Quick plot of Miller vs SS bootstrap
g <- results_afsc %>%
 # filter(metric!='Rec') %>%
  ggplot(aes(baseyear, y=rho, fill=miller)) + geom_violin() +
  facet_grid(metric~model, scales='free') + geom_hline(yintercept=0, col='red')+
  geom_point(data=rho_obs, col='red',pch='-', size=10) +
  coord_cartesian(ylim=c(-.5,.5))
ggsave('plots/results_miller2.png', g, width=12, height=7)

## ## Are the variances the same?
## results_afsc %>% mutate(miller=replace_na(miller, FALSE),
##                         model=gsub('flathead', 'FHS', model)) %>%
##   group_by(model, metric, miller) %>%
##   summarize(stdev=round(sd(rho),3)) %>% pivot_wider(c(model, metric),
##   names_from=miller, names_prefix='miller=', values_from=stdev)



### old code


## ## Add in the real data rhos
## rho <- readRDS('results/FHS_retro_rhos.RDS') %>% as.data.frame() %>%
##   mutate(model='BSAI_flathead', boot=-1)
## results <- bind_rows(rho, results)
## results.long <- results %>% pivot_longer(-c(model, boot)) %>%
##   mutate(type=case_when(
##            grepl('WoodHole', name) ~ 'WoodsHole',
##            grepl('AFSC', name) ~ 'AFSC_Hurtado',
##            TRUE ~ 'Normal'),
##          name=gsub('WoodHole_|AFSC_Hurtado_|.all', '',name))


## g <- ggplot(filter(results.long, boot>0), aes(y=value, x=type)) + geom_violin() +
##   facet_wrap('name') + geom_hline(yintercept=0, color='blue') +
##   geom_point(data=filter(results.long, boot==-1), color='red') +
##   labs(y='Rho')
## ggsave('plots/fhs_all.png', g, width=7, height=5)
## ## g <- filter(results.long, boot>0 & type=='AFSC_Hurtado') %>%
## ##   ggplot(aes(y=value, x=name)) + geom_violin() +
## ##   geom_hline(yintercept=0, color='blue') +
## ##   geom_point(data= filter(results.long, boot==-1 &
## ##   type=='AFSC_Hurtado'), color='red')  + labs(y='rho')
## ## ggsave('plots/fhs_afsc.png', g, width=7, height=5)

## g <- filter(results.long, boot>0 & type=='AFSC_Hurtado') %>%
##   ggplot(aes(x=value)) + geom_histogram(bins=20) +
##   facet_wrap('name', scales='free')+
##   geom_vline(xintercept=0) +
##   geom_vline(data= filter(results.long, boot==-1 & type=='AFSC_Hurtado'),
##              aes(xintercept=value), color='red')  +
##   labs(y='Frequency', x='AFSC rho')
## ggsave('plots/fhs_afsc.png', g, width=7, height=5)
## ## ## Calculate percentiles
## ## obs <- filter(results, boot==-1) %>% select(-boot, -model)
## ## exp <- filter(results, boot!=-1) %>% select(-boot, -model)
## ## sapply(names(obs), function(i) round(mean(obs[,i]>exp[,i]),3))
