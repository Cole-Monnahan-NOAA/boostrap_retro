library(dplyr)
library(ggplot2)
library(here)

dir='\\\\AKC0SS-n086\\AKC_PubliC\\Dropbox\\Monnahan\\'

library(extrafont)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()
y

loadfonts(device="win")

# add fonts to all text (last line)
ggplot2::theme_set(
  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA),
      text = element_text(size=10,family = "Times New Roman")
    )
)

pars=readRDS(paste0(dir,'par.results.RDS'))
ts=readRDS(paste0(dir,'ts.results.RDS'))

pars=pars[,1:7]
test2 <- pars %>%
  dplyr::group_by(model, par, assess_yr, peelsl, miller) %>%
  dplyr::mutate(re=(value-value[boot==0])/value[boot==0])

ts=ts[,1:8]
ts2_tmp <- ts %>%
  dplyr::group_by(model, assess_yr,year, peel,miller) %>%
  dplyr::mutate(re=(value-value[boot==0])/value[boot==0],keep_yr=assess_yr-peel)

ts3 <- ts2 %>% dplyr::filter(year<=keep_yr)

ts4 <- ts3 %>%
  dplyr::filter(boot>0) %>%
  dplyr::group_by(model,year,peel,name,miller) %>%
  dplyr::summarise(x = quantile(re, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))

ts5 <- tidyr::spread(ts4,q,x)
names(ts5)=c(names(ts5[1:5]),'lci','med','uci')

ts5 %>% ggplot(aes(x=year,y=med,color=model))

mill=sort(unique(ts5$miller))
met=sort(unique(ts5$name))
for(i in 1:length(mill)){
  for(j in 1:length(met)){
    print("i="); print(mill[i])
    print("j="); print(met[j])

    g=ts5 %>% filter(name==met[j] & miller==mill[i])

      ggplot(data=g,aes(x=year, y=med, color=model)) + geom_line(aes(x=year, y=med, color=model))+
      geom_ribbon(aes(ymin=lci,ymax=uci, fill=model),color="grey70",alpha=0.3)+ facet_wrap(.~peel)+
      geom_hline(yintercept=0,color="red")+
      ylab(paste("Relative error in ", met[j],sep=" ")) + xlab("Year")
    ggsave(here::here('plots',paste0('RelError_',met[j],'_',mill[i],'.png')),units='in',width=8,height=8)
  }
}


mods=sort(unique(ts3$model))
met=sort(unique(ts3$name))
for(i in 1:length(mods)){
  for(j in 1:length(met)){
    print("i="); print(mods[i])
    print("j="); print(met[j])

    ts3 %>% filter(boot>0 & name==met[j] & miller==FALSE & model==mods[i]) %>%
      ggplot(aes(x=as.factor(year), y=re)) +
      geom_boxplot() + facet_wrap('peel')+ geom_hline(yintercept=0,color="red")+
      ylab("Relative error") + xlab("Year")
      ggsave(here::here('plots',paste0(mods[i],'_',met[j],'.png')),units='in',width=8,height=8)
  }
}

mods=sort(unique(ts3$model))
met=sort(unique(ts3$name))
for(i in 1:length(mods)){
  for(j in 1:length(met)){
    print("i="); print(mods[i])
    print("j="); print(met[j])

    ts3 %>% filter(boot>0 & name==met[j] & miller==TRUE & model==mods[i]) %>%
      ggplot(aes(x=as.factor(year), y=re)) +
      geom_boxplot() + facet_wrap('peel')+ geom_hline(yintercept=0,color="red")+
      ylab("Relative error") + xlab("Year")
    ggsave(here::here('plots',paste0(mods[i],'_',met[j],'MillerT.png')),units='in',width=8,height=8)
  }
}


