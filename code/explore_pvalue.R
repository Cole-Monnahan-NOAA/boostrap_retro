
## Quick exploration of how to get a pvalue from a resulting
## bootstrap test. The basic idea is we have a null distribution
## (x) and want to know whether observed sample (y) came from
## it.

library(dplyr)
library(ggplot2)
library(goftest)
## install.packages("kSamples")
library(kSamples)
test <- ad.test(runif(50), rnorm(30), method='simulated')
test$ad[1,4]

## There is some criticism of the KS test, and maybe the AD test
## is better.
x <- rnorm(500)
y <- seq(-5,5, len=50)
ks <- sapply(y, function(i) ks.test(x,i)$p.value)
ad <- sapply(y, function(i) ad.test(x, i, method='simulated')$ad[1,4])
par(mfrow=c(1,3))
hist(x)
plot(y,log10(ks), ylim=range(log10(c(ks,ad)))); abline(h=log10(.05))
lines(y, log10(ad))
plot(ks, ad)
## I conclude from this that AD is not really different than KS
## for this purpose, and is WAY slower so not using it.

## Simulation test the null distributions. Does it calibrate well
## if the samples all did come from the sample population?
library(snowfall)
ff <- function(seed, df){
  set.seed(seed)
  x <- rnorm(500)
  y <- rt(1, df=df)
  ks <- ks.test(x,y)$p.value
  data.frame(y, ks, df)
}

sfInit(parallel=TRUE, cpus=7)
sfExport('ff')

Nreps <- 1e5
res1 <- sfLapply(1:Nreps, function(i){ff(i,Inf)}) %>% bind_rows
res2 <- sfLapply(1:Nreps, function(i){ff(i,10)}) %>% bind_rows
res3 <- sfLapply(1:Nreps, function(i){ff(i,2)}) %>% bind_rows
res <- bind_rows(res1, res2, res3)


## It's rejected when y is randomly far from zero, and this
## occurs more often with smaller df for the t. The noise around
## the line is b/c we're testing against a random sample of
## N(0,1), not against the analytical one.
ggplot(res, aes(y, ks, color=factor(df)))  + geom_point() +
  facet_wrap('df', scales='free')

## Easy to see that you reject it more often
ggplot(res, aes(ks)) + geom_histogram() + facet_wrap('df')

## Maybe eaiser to look at via cumulative distribution (should be
## 1:1 line if correctly specified)
res.ecdf <- res %>% group_by(df) %>%
  summarize(xx=seq(0,1, len=100),
            ecdf=sapply(xx,function(ii) mean(ks<ii)))
ggplot(res.ecdf, aes(xx,ecdf, color=factor(df))) + geom_line()


### My conclusion is that the KS test is appropriate for this
### usage. It has good properties that (1) if y and x are the
### same distribution, then you reject alpha % of the time with
### an alpha limit, i.e. it is well calibrated. (2) it picks up
### deviations when this is not true. The power will depend on
### how far off.
