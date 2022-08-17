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



### ------------------------------------------------------------
## Check whether there is bias in the estimates of parameters or SSB

### Meaghan had some special theme modifications that are turned
## off  for now.
## library(extrafont)
## remotes::install_version("Rttf2pt1", version = "1.3.8")
## extrafont::font_import()
## loadfonts(device="win")
## ## add fonts to all text (last line)
## theme_set(
##   theme_light() +
##     theme(
##       panel.grid.major = element_blank(),
##       panel.grid.minor = element_blank(),
##       # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
##       strip.background = element_rect(fill = NA, colour = NA),
##       strip.text.x = element_text(colour = "black"),
##       strip.text.y = element_text(colour = "black"),
##       panel.border = element_rect(fill = NA),
##       legend.key.size = grid::unit(0.9, "lines"),
##       legend.key = element_rect(colour = NA, fill = NA),
##       legend.background = element_rect(colour = NA, fill = NA),
##       text = element_text(size=10,family = "Times New Roman")
##     )
## )

pars <- readRDS('results/par.results.RDS')
ts <- readRDS('results/ts.results.RDS')
## #check on whether rel error is correctly calculated###############################
## b0=ts %>% filter(boot==0)%>% rename(value0=value) %>% select(-boot)
## b=ts %>% filter(boot>0)
## b2=merge(b0,b,all.y=TRUE)
## b2$re=(b2$value-b2$value0)/b2$value0
## mean(b2$re)

##################################################################################

unique(pars$par)

pars <- readRDS('results/par.results.RDS')
pars <- pars%>% group_by(model,assess_yr,peels,par,miller) %>%
  mutate(constant=sd(value)==0, n=n()) %>% ungroup %>% filter(!constant)

test <- pars %>% filter(model == 'EBS_Pcod3' & peels==0) %>%
  droplevels %>% select(-constant, -n) %>%
  group_by(par,miller) %>%
  summarise(x = quantile( re, c(0.025, 0.5, 0.975)),
            q=c(0.025,0.5,0.975)) %>%
  ungroup
test2 <- tidyr::spread(test,q,x)
names(test2)[3:5] <- c('lci','med','uci')

filter(test2, !grepl('dev|DEV|Dev',par)) %>%
  ggplot(aes(miller, y=med, ymin=lci, ymax=uci)) + geom_linerange() +
  facet_wrap("par", scales='free') + geom_point() +
  ## coord_cartesian(ylim=c(-1,1))+
  geom_hline(yintercept=0,color="red")

test <- pars %>% filter(model == 'EBS_Pcod' & peels=0
                !grepl('dev|DEV|Dev', par)) %>%
  group_by(model,assess_yr, peels,par,miller) %>%
  summarise(x = quantile( re, c(0.025, 0.5, 0.975)),
            q=c(0.025,0.5,0.975)) %>%
  ungroup
pars2 <- tidyr::spread(test,q,x)
names(pars2)[6:8] <- c('lci','med','uci')

ggplot(pars2, aes(peels, y=med, ymin=lci, ymax=uci)) + geom_linerange() +
  facet_wrap("par", scales='free') + geom_point() +
  coord_cartesian(ylim=c(-1,1))+geom_hline(yintercept=0,color="red")

ts4 <- ts %>%
  filter(boot>0) %>%
  group_by(model,year,peel,name,miller) %>%
  summarise(x = quantile(re, c(0.025, 0.5, 0.975)), q = c(0.025, 0.5, 0.975))
ts5 <- tidyr::spread(ts4,q,x)
names(ts5)=c(names(ts5[1:5]),'lci','med','uci')
met=sort(unique(ts5$name))
for(j in 1:length(met))
{
    g=ts5 %>% filter(name==met[j] & peel==0) %>% mutate(peel=as.factor(peel))

    ggplot(data=g,aes(x=year, y=med, color=peel)) + geom_line(aes(x=year, y=med, color=peel))+
      geom_ribbon(aes(ymin=lci,ymax=uci, fill=peel),alpha=0.3)+ facet_grid(miller~model)+
      coord_cartesian(ylim=c(-1,1))+geom_hline(yintercept=0,color="red")+
      ylab(paste("Relative error in ", met[j],sep=" ")) + xlab("Year")
   ##  ggsave(here::here('plots',paste0('RelError_',met[j],'.png')),units='in',width=8,height=8)
}


#mods=sort(unique(ts3$model))
#met=sort(unique(ts3$name))
#for(i in 1:length(mods)){
#  for(j in 1:length(met)){
#
#    ts3 %>% filter(boot>0 & name==met[j] & miller==FALSE & model==mods[i]) %>%
#      ggplot(aes(x=as.factor(year), y=re)) +
#      geom_boxplot() + facet_wrap('peel')+ geom_hline(yintercept=0,color="red")+
#      ylab("Relative error") + xlab("Year")
#      ggsave(here::here('plots',paste0(mods[i],'_',met[j],'.png')),units='in',width=8,height=8)
#  }
#}

#mods=sort(unique(ts3$model))
#met=sort(unique(ts3$name))
#for(i in 1:length(mods)){
#  for(j in 1:length(met)){
#
 #   ts3 %>% filter(boot>0 & name==met[j] & miller==TRUE & model==mods[i]) %>%
  #    ggplot(aes(x=as.factor(year), y=re)) +
   #   geom_boxplot() + facet_wrap('peel')+ geom_hline(yintercept=0,color="red")+
    #  ylab("Relative error") + xlab("Year")
#    ggsave(here::here('plots',paste0(mods[i],'_',met[j],'MillerT.png')),units='in',width=8,height=8)
#  }
#}





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
