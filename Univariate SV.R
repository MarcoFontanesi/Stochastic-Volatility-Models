rm(list = ls())

#Model Setup
library(tidyverse)

#Data frame
setwd("C:/MARCO/UCL/Econ&Stat/Project/Project Working Directiory")
d = read.csv("EURUSD=X.csv")
d = d[-c(1), ]

#Extract column's values of our data frame
log.returns = pull(d, log.returns)

#Mean centering/Mean corrected 
log.returns_cen = scale(log.returns, scale = FALSE)

#Estimation 
stan_data = list(N_t = length(log.returns_cen), y = c(log.returns_cen))
model = rstan::stan_model(file = "Univariate SV.stan")

fit = rstan::sampling(
  object = model,
  data = stan_data,
  iter = 22000,
  cores = 4,
  thin = 20
)

print(
  fit,
  digits = 3,
  par    = c('mu', 'phi', 'sigma'),
  probs  = c(.025, .5, .975)
)

#Visualization 
shinystan::launch_shinystan(fit)

# Create y_rep 'by-hand'
h = rstan::extract(fit, 'h')$h

# get log volatility and data replicates
yrep = apply(h, 1, function(h) rnorm(length(log.returns), 0, exp(h/2)))

# or just extract it
yrep2 = rstan::extract(fit, 'y_rep')$y_rep
dim(yrep)

h = colMeans(h)

library(lubridate)
library(scales)

series = seq(as.Date("2022/01/10"), by = "week", length.out = length(log.returns))

plot(series, h, cex=.5, col='gray50', type='l', bty='n')

plot(series, log.returns_cen, cex=.5, col='gray50', ylim=c(-0.05,0.1), type='l',
     bty='n')

sapply(sample(1:dim(yrep)[2], 100), function(i) lines(series, yrep[,i], 
                                                      cex=.5, col=alpha('#FF5500', .1)) )
lines(series, log.returns_cen, cex=.5, col='gray50', ylim=c(-0.05,0.1), 
      type='l', bty='n')





