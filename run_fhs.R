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
Nreplicates <- 300
Npeels <- 10
peels <- 0:-Npeels

## Setup to run parallel
library(snowfall)
sfStop()
sfInit( parallel=TRUE, cpus=18)
sfExportAll()
### Run one in serial as a test
test <- run_SS_boot_iteration(1, 'fhs', TRUE)
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


### Probably not working past here
stop('dont source')



## Add in the real data rhos
rho <- readRDS('FHS_retro_rhos.RDS') %>% as.data.frame() %>%
  mutate(model='BSAI_flathead', boot=-1)
results <- bind_rows(rho, results)

results.long <- results %>% pivot_longer(-c(model, boot)) %>%
  mutate(type=case_when(
           grepl('WoodHole', name) ~ 'WoodsHole',
           grepl('AFSC', name) ~ 'AFSC_Hurtado',
           TRUE ~ 'Normal'),
         name=gsub('WoodHole_|AFSC_Hurtado_|.all', '',name))

dir.create('plots')

g <- ggplot(filter(results.long, boot>0), aes(y=value, x=type)) + geom_violin() +
  facet_wrap('name') + geom_hline(yintercept=0, color='blue') +
  geom_point(data=filter(results.long, boot==-1), color='red') +
  labs(y='Rho')
ggsave('plots/fhs_all.png', g, width=7, height=5)
g <- filter(results.long, boot>0 & type=='AFSC_Hurtado') %>%
  ggplot(aes(y=value, x=name)) + geom_violin() +
  geom_hline(yintercept=0, color='blue') +
  geom_point(data= filter(results.long, boot==-1 &
  type=='AFSC_Hurtado'), color='red')  + labs(y='rho')
ggsave('plots/fhs_afsc.png', g, width=7, height=5)

g <- filter(results.long, boot>0 & type=='AFSC_Hurtado') %>%
  ggplot(aes(x=value)) + geom_histogram(bins=20) +
  facet_wrap('name', scales='free')+
  geom_vline(xintercept=0) +
  geom_vline(data= filter(results.long, boot==-1 & type=='AFSC_Hurtado'),
             aes(xintercept=value), color='red')  +
  labs(y='Frequency', x='AFSC rho')
ggsave('plots/fhs_afsc.png', g, width=7, height=5)

## Calculate percentiles
obs <- filter(results, boot==-1) %>% select(-boot, -model)
exp <- filter(results, boot!=-1) %>% select(-boot, -model)
sapply(names(obs), function(i) round(mean(obs[,i]>exp[,i]),3))
