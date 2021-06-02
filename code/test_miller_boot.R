## test that the bootstrap code for the Miller approach is
## working

datlist <- SS_readdat('models/2020_BSAI_FHS.dat')

## Bootstrap simulations and combine together to get distributions
out <- lapply(1:50, function(boot) sample_miller_boot(boot, datlist, test=TRUE))
indices <- lapply(out, function(x) x$cpue) %>% bind_rows()
lcomps <- lapply(out, function(x) x$lencomp) %>% bind_rows()
tmp <- lapply(out, function(x) x$agecomp) %>% bind_rows()
agecomps <- filter(tmp, Lbin_lo<0)
caalcomps <- filter(tmp, Lbin_lo>0)

## Now get the observed ones to plot on top
lencomp.long <- datlist$lencomp %>%
  pivot_longer(-(1:6), values_to='proportion') %>%
  mutate(rep=-1, sex=substr(name,0,1), len=as.numeric(gsub('f|m','', name)))
## Split the age comps into marginal and conditional age at
## length after normalizing each row.
tmp <- datlist$agecomp
tmp[,-(1:9)] <- tmp[,-(1:9)]/rowSums(tmp[,-(1:9)])
agecomp.long <- tmp  %>% filter(Lbin_lo<0) %>%
  pivot_longer(-(1:9), values_to='proportion') %>%
  mutate(rep=-1, sex=substr(name,0,1),
         age=as.numeric(gsub('f|m','', name)))
caalcomp.long <- tmp  %>% filter(Lbin_lo>0) %>%
  pivot_longer(-(1:9), values_to='proportion') %>%
  mutate(rep=-1, sex=substr(name,0,1),
         age=as.numeric(gsub('f|m','', name)))


g.index <- ggplot(indices, aes(year, log(obs))) + geom_point(alpha=.25) +
  geom_point(data=datlist$CPUE, col='red', size=2.5) +
  facet_wrap('index') + labs(y='index')
if(nrow(lencomp.long)>0){
  g.lcomps <- ggplot(filter(lcomps, Yr==2018), aes(len, proportion)) +
  ##  facet_wrap('Yr', scales='free') +
  facet_grid(Yr+FltSvy~sex) +
  geom_jitter(alpha=.25, width=.5, height=0) +
  geom_point(data=filter(lencomp.long, Yr == 2018),
             col='red', size=2.5) +
    labs(y='length comp')
  } else {g.lcomps <- NULL}
if(nrow(agecomp.long)>0){
g.agecomps <- ggplot(filter(agecomps, Yr>2017), aes(age, proportion)) +
  ##  facet_wrap('Yr', scales='free') +
  facet_grid(Yr+FltSvy~sex) +
  geom_jitter(alpha=.25, width=.15, height=0) +
  geom_point(data=filter(agecomp.long, Yr > 2017),
             col='red', size=2.5) +
  labs(y='age comp')
} else { g.agecomps <- NULL}
if(nrow(caalcomp.long)>0){
  g.caalcomps <-
    filter(caalcomps, sex=='f' & Yr==2018 & Lbin_lo > 20 &
  Lbin_lo <40) %>%
  ggplot(aes(age, proportion)) +
   facet_wrap('Lbin_lo', scales='free') +
  ##facet_grid(Yr+FltSvy~sex+Lbin_lo) +
  geom_jitter(alpha=.25, width=.15, height=0) +
  geom_point(data=filter(caalcomp.long, Yr == 2018 & Lbin_lo > 20 & Lbin_lo <40),
             col='red', size=2.5) +
  labs(y='length comp')
} else { g.caalcomps <- NULL}

g.index
g.lcomps
g.agecomps
g.caalcomps


## numerical checks
indices.E <- indices %>% group_by(year) %>%
  summarize(avg=mean(log(obs))) %>% pull(avg)
(log(datlist$CPUE$obs) - indices.E) # appears unbiased
