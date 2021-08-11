### Run and compare models
library(tidyverse)
library(r4ss)
library(snowfall)
library(ggplot2)
theme_set(theme_bw())
packageVersion('r4ss') #  '1.42.0'
source('code/functions.R')

## For each boostrap data set, run a retrospective analysis
Nreps <- 300
reps <- 1:Nreps
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
run_model(reps, model.name='EBS_Pcod')
run_model(reps, model.name='GOA_Pcod')
run_model(reps, model.name='GOA_NRS')


## Rerun using the Miller approach
run_model(reps, model.name='EBS_Pcod', miller=TRUE)
run_model(reps, model.name='GOA_Pcod', miller=TRUE)
run_model(reps, model.name='GOA_NRS', miller=TRUE)

source('code/process_results.R')

## The models have different end years so the baseyears are
## different, but check for full replicates
results_afsc %>% group_by(model,miller, baseyear) %>%
  filter(metric=='SSB') %>% summarize(count=n()) %>%
  pivot_wider(c(model, miller),  names_from='baseyear', values_from='count')

## Quick plot of Miller vs SS bootstrap
g <- results_afsc %>%
  filter(metric!='Rec') %>%
  ggplot(aes(baseyear, y=rho, fill=miller)) + geom_violin() +
  facet_grid(metric~model, scales='free') + geom_hline(yintercept=0, col='red')+
  geom_point(data=rho_obs, col='red',pch='-', size=10) +
  coord_cartesian(ylim=c(-.5,.5))
ggsave('plots/results_miller.png', g, width=12, height=7)

## ## Are the variances the same?
## results_afsc %>% mutate(miller=replace_na(miller, FALSE),
##                         model=gsub('flathead', 'FHS', model)) %>%
##   group_by(model, metric, miller) %>%
##   summarize(stdev=round(sd(rho),3)) %>% pivot_wider(c(model, metric),
##   names_from=miller, names_prefix='miller=', values_from=stdev)

## Needs updating since adding Miller stuff broke it:
## source('code/make_plots.R')
