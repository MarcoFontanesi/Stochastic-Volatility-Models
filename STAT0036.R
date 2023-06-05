# Implementation of the time series analysis 
install.packages("xts")
library(xts)
install.packages("lubridate")
library(lubridate)

# Data science for data science and data visualization 
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape) 
install.packages("ggfortify")
library(ggfortify)
install.packages("gridExtra")
library(gridExtra)
install.packages("scales")
library(scales)
install.packages("patchwork")
library(patchwork)

############################### Data Analysis ###################################

#Data frame
d1 = read.csv("EURUSD=X.csv")
d1 = d1[-c(1), ] #Remove missing data

d2 = read.csv("GBPUSD=X.csv")
d2 = d2[-c(1), ] #Remove missing data

#Converting the string "Date" to a date variable called "New.date"
d1$Date = as.Date(d1$Date, "%d/%m/%Y")
d2$Date = as.Date(d2$Date, "%d/%m/%Y")

#Converting the data frame into time series
ts_d1 = read.zoo(d1[, c(1,8)])
ts_d2 = read.zoo(d2[, c(1,8)])

#Plot the two time series separately 
par(mfrow = c(2,1), mar = c(2,2,.5,.5) + .5, mgp = c(1.6,.6,0))   # trim the margins 

plot(ts_d1, ylab = 'EUR/USD Returns', xlab = "",
     type = 'n', ylim = c(-5,5))    # set up the plot
grid(lty = 1, col = gray(.9))       # add a grid
lines(ts_d1, type = 'l', col = "black", lwd = 3)    

plot(ts_d2, ylab = 'GBP/USD Returns', xlab = "",
     type = 'n', ylim = c(-6,7))    # set up the plot
grid(lty = 1, col = gray(.9))       # add a grid
lines(ts_d2, type = 'l', col = "black", lwd = 3) 

#Plot the data (combined)
returns.df = data.frame(Time = c(time(ts_d1)), eurusd = c(ts_d1), 
                        gbpusd = c(ts_d2))
returns.df$eurusd = as.numeric(as.character(returns.df$eurusd))
returns.df$gbpusd = as.numeric(as.character(returns.df$gbpusd))

ggplot(data = returns.df, aes(x = Time, y = value, color = variable, 
                              group = 1)) +
  ylab('Exchange Rate Returns') + xlab("")                                +
  geom_line(aes(y = eurusd, col = 'EUR/USD'),  size = 1.5, alpha = .5) +
  geom_line(aes(y = gbpusd, col = 'GBP/USD'), size = 1.5, alpha = .5) + 
  theme_bw()

#Specification of a numeric vector of log returns for the two series
log.returns1 = as.numeric(ts_d1)
log.returns2 = as.numeric(ts_d2)

#Mean centering/Mean corrected 
log.returns_cen1 = scale(log.returns1, scale = FALSE)
log.returns_cen2 = scale(log.returns2, scale = FALSE)

# Calculate the sample standard deviation of the log returns
sd_eurusd = sd(log.returns1)
sd_gbpusd = sd(log.returns2)

# Calculate the standardized centered log-returns by dividing by the sample standard deviation
std_eurusd = log.returns_cen1 / sd_eurusd
std_gbpusd = log.returns_cen2 / sd_gbpusd

# Plot the standardized return density
ggplot(data.frame(x = std_eurusd), aes(x)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.2, color = "black", fill = "deepskyblue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), colour = "red", size = 1) +
  xlab("Standardized log returns") + 
  ylab("Density") +
  ggtitle("Standardized log-returns vs Normal(0,1) density") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data.frame(x = std_gbpusd), aes(x)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.2, color = "black", fill = "deepskyblue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), colour = "red", size = 1) +
  xlab("Standardized log returns") + 
  ylab("Density") +
  ggtitle("Standardized log-returns vs Normal(0,1) density") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

################################ Model Estimation ####################################
install.packages("rstan")
library(rstan)

stan_data = list(N = length(log.returns_cen1), y1 = c(log.returns_cen1),
                 y2 = c(log.returns_cen2))

## Basic MSV Model
MSV = rstan::stan_model(file = "Basic MSV.stan")

fit_MSV = rstan::sampling(
  object = MSV,
  data = stan_data,
  iter = 5000, warmup = 2000, cores = 4)

print(
  fit_MSV,
  digits = 4,
  par    = c("mu1", "mu2", "phi1", "phi2", "sigma1", "sigma2"),
  probs  = c(.05, .5, .95)
)

shinystan::launch_shinystan(fit_MSV)

## Constant Correlation MSV Model
CC_MSV = rstan::stan_model(file = "CC MSV.stan")

fit_CCMSV = rstan::sampling(
  object = CC_MSV,
  data = stan_data,
  iter = 5000, warmup = 2000, cores = 4)

print(
  fit_CCMSV,
  digits = 4,
  par    = c("mu1", "mu2", "phi1", "phi2", "sigma1", "sigma2","rho"),
  probs  = c(.05, .5, .95)
)

shinystan::launch_shinystan(fit_CCMSV)

#Kernel density estimates
stan_dens(fit_CCMSV, par = c("mu1", "mu2", "phi1", "phi2", "sigma1", "sigma2","rho"), 
          fill = "white")

#Traceplot
stan_trace(fit_CCMSV, par = c("mu1", "mu2", "phi1", "phi2", "sigma1", "sigma2","rho"))

#####
#Visualization for Constant Correlation MSV
CC_h1 = rstan::extract(fit_CCMSV, 'h1')$h1
CC_h2 = rstan::extract(fit_CCMSV, "h2")$h2

CC_yrep1 = rstan::extract(fit_CCMSV, 'y_rep1')$y_rep1
CC_yrep2 = rstan::extract(fit_CCMSV, 'y_rep2')$y_rep2

CC_h1 = colMeans(CC_h1)
CC_h2 = colMeans(CC_h2)

series = seq(as.Date("2018/01/8"), by = "week", 
              length.out = length(log.returns1))

par(mfrow = c(1,1), mar = c(2,2,.5,.5) + .5, mgp = c(1.6,.6,0)) 
plot(ts_d1, ylab = 'Log Volatility', xlab = "",
     type = 'n', ylim = c(-1,2)) 
grid(lty = 1, col = gray(.9)) 
lines(series, CC_h1, type = 'l', col = "red", lwd = 3) 
lines(series, CC_h2, type = 'l', col = "blue", lwd = 3) 
#####

## Granger Causality MSV Model
GC_MSV = rstan::stan_model(file = "GC MSV.stan")

fit_GCMSV = rstan::sampling(
  object = GC_MSV,
  data = stan_data,
  iter = 5000, warmup = 2000, cores = 4)

print(
  fit_GCMSV,
  digits = 4,
  par    = c("mu1", "mu2", "phi1", "phi2", "sigma1", "sigma2","rho", "phi21"),
  probs  = c(.05, .5, .95)
)

shinystan::launch_shinystan(fit_GCMSV)

#Kernel density estimates
stan_dens(fit_GCMSV, par = c("mu1", "mu2", "phi1", "phi2", "sigma1", "sigma2","rho", "phi21"), 
          fill = "white")

#Traceplot
stan_trace(fit_GCMSV, par = c("mu1", "mu2", "phi1", "phi2", "sigma1", "sigma2","rho", "phi21"))

###
dens1 = stan_dens(fit_GCMSV, par = c("mu1", "mu2"), fill = "white")
trace1 = stan_trace(fit_GCMSV, par = c("mu1", "mu2"))

dens1 = (dens1 + ggtitle("Kernel Density Estimates\n") + xlab("") + theme(plot.title = element_text(hjust = 0.5)))

trace1 =(trace1 + scale_color_brewer(type = "div") + theme(legend.position = "none") 
+ ggtitle("Trace Plot\n") + xlab("") + theme(plot.title = element_text(hjust = 0.5)))

dens1 + trace1 + plot_layout(nrow = 2)

dens2 = stan_dens(fit_GCMSV, par = c("sigma1", "sigma2", "rho"), fill = "white")
trace2 = stan_trace(fit_GCMSV, par = c("sigma1", "sigma2", "rho"))

dens2 = (dens2 + ggtitle("Kernel Density Estimates\n") + xlab("") + theme(plot.title = element_text(hjust = 0.5)))

trace2 =(trace2 + scale_color_brewer(type = "div") + theme(legend.position = "none") 
         + ggtitle("Trace Plot\n") + xlab("") + theme(plot.title = element_text(hjust = 0.5)))

dens2 + trace2 + plot_layout(nrow = 2)

dens3 = stan_dens(fit_GCMSV, par = c("phi1", "phi2", "phi21"), fill = "white")
trace3 = stan_trace(fit_GCMSV, par = c("phi1", "phi2", "phi21"))

dens3 = (dens3 + ggtitle("Kernel Density Estimates\n") + xlab("") + theme(plot.title = element_text(hjust = 0.5)))

trace3 =(trace3 + scale_color_brewer(type = "div") + theme(legend.position = "none") 
         + ggtitle("Trace Plot\n") + xlab("") + theme(plot.title = element_text(hjust = 0.5)))

dens3 + trace3 + plot_layout(nrow = 2)

#Visualization for Granger Causality MSV
GC_h1 = rstan::extract(fit_GCMSV, 'h1')$h1
GC_h2 = rstan::extract(fit_GCMSV, "h2")$h2

GC_yrep1 = rstan::extract(fit_GCMSV, 'y_rep1')$y_rep1
GC_yrep2 = rstan::extract(fit_GCMSV, 'y_rep2')$y_rep2

GC_h1 = colMeans(GC_h1)
GC_h2 = colMeans(GC_h2)

series = seq(as.Date("2018/01/8"), by = "week", 
             length.out = length(log.returns1))

#####
par(mfrow = c(1,1), mar = c(2,2,.5,.5) + .5, mgp = c(1.6,.6,0)) 
plot(ts_d1, ylab = 'Log Volatility', xlab = "",
     type = 'n', ylim = c(-1,2.5)) 
grid(lty = 1, col = gray(.9)) 
lines(series, GC_h1, type = 'l', col = "red", lwd = 3) 
lines(series, GC_h2, type = 'l', col = "blue", lwd = 3)
legend(x = "topleft", legend = c("Vol of EUR/USD", "Vol of GBP/USD"), 
       col = c("red", "blue"), lty = c(1,1), cex = 0.5)
#####

#Predictive vs Observed returns for EURUSD
par(mfrow = c(2,1), mar = c(2,2,.5,.5) + .5, mgp = c(1.6,.6,0))
plot(series, log.returns_cen1, cex=.5, col='black', type='l',bty='n', lwd = 2, 
     ylim=c(-5,5), ylab = "Returns centered", xlab = "")
sapply(sample(1:dim(GC_yrep1)[1], 100), function(i) lines(series, GC_yrep1[i,],   
                                                       cex=.5, col=alpha('#FF5500', .1)) )
lines(series, log.returns_cen1, cex=.5, col='black', type='l',bty='n', lwd = 2, 
      ylim=c(-5,5))

#Predictive vs Observed returns for GBPUSD
plot(series, log.returns_cen2, cex=.5, col='black', type='l',bty='n', lwd = 2, 
     ylim=c(-6,7), ylab = "Returns centered", xlab = "")
sapply(sample(1:dim(GC_yrep2)[1], 100), function(i) lines(series, GC_yrep2[i,],   
                                                       cex=.5, col=alpha('#FF5500', .1)) )
lines(series, log.returns_cen2, cex=.5, col = 'black', type='l',bty='n', lwd = 2, 
      ylim=c(-6,7))

#Plot the estimated volatility with its confidence interval over time
CC1_05 = apply(exp(rstan::extract(fit_GCMSV, "h1")[[1]]/2), 2, quantile, 0.05)
CC1_50 = apply(exp(rstan::extract(fit_GCMSV, "h1")[[1]]/2), 2, quantile, 0.5)
CC1_95 = apply(exp(rstan::extract(fit_GCMSV, "h1")[[1]]/2), 2, quantile, 0.95)

CC2_05 = apply(exp(rstan::extract(fit_GCMSV, "h2")[[1]]/2), 2, quantile, 0.05)
CC2_50 = apply(exp(rstan::extract(fit_GCMSV, "h2")[[1]]/2), 2, quantile, 0.5)
CC2_95 = apply(exp(rstan::extract(fit_GCMSV, "h2")[[1]]/2), 2, quantile, 0.95)

par(mfrow = c(2,2), mar = c(2,2,.5,.5) + .5, mgp = c(1.6,.6,0))

plot(series, log.returns_cen1, cex=.5, col='black', type='l',bty='n', lwd = 2, 
     ylim=c(-6,7), ylab = "Returns", xlab = "")

plot(series, log.returns_cen2, cex=.5, col='black', type='l',bty='n', lwd = 2, 
     ylim=c(-6,7), ylab = "Returns", xlab = "")

plot(series, CC1_05, cex=.5, col = "green", type='l',bty='n', lwd = 2,
     ylim = c(0, 3), ylab = "Estimated exp volatilities", xlab = "")
lines(series, CC1_50, cex=.5, col = "red", type='l',bty='n', lwd = 2)
lines(series, CC1_95, cex=.5, col = "green", type='l',bty='n', lwd = 2)

plot(series, CC2_05, cex=.5, col = "green", type='l',bty='n', lwd = 2,
     ylim = c(0, 4.5), ylab = "Estimated exp volatilities", xlab = "")
lines(series, CC2_50, cex=.5, col = "red", type='l',bty='n', lwd = 2)
lines(series, CC2_95, cex=.5, col = "green", type='l',bty='n', lwd = 2)

## Multiplicative Factor MSV Model
MFactor_MSV = rstan::stan_model(file = "MFactor-MSV.stan")

fit_MFactorMSV = rstan::sampling(
  object = MFactor_MSV,
  data = stan_data,
  iter = 5000, warmup = 2000, cores = 4)

print(
  fit_MFactorMSV,
  digits = 4,
  par    = c("mu", "phi", "sigma", "rho", "beta"),
  probs  = c(.05, .5, .95)
)


#Visualization of MFactor MSV
shinystan::launch_shinystan(fit_MFactorMSV)

#####
h = rstan::extract(fit_MFactorMSV, 'h')$h
h = colMeans(h)

#Plot of Volatility Factor
par(mfrow = c(1,1), mar = c(2,2,.5,.5) + .5, mgp = c(1.6,.6,0)) 
plot(ts_d1, ylab = 'Volatility', xlab = "",
     type = 'n', ylim = c(-1,2.5)) 
grid(lty = 1, col = gray(.9)) 
lines(series, h, type = 'l', col = "black", lwd = 3) 
lines(series, GC_h1, type = 'l', col = "red", lwd = 3) 
lines(series, GC_h2, type = 'l', col = "blue", lwd = 3) 
legend(x = "topleft", legend = c("Vol of Factor", "Vol of EUR/USD", "Vol of GBP/USD"), 
       col = c("black", "red", "blue"), lty = c(1,1), cex = 0.5)
#####
