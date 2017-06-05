# http://www.magesblog.com/2015/09/bayesian-regression-models-using-stan.html
#
temp <- c(11.9,14.2,15.2,16.4,17.2,18.1,18.5,19.4,22.1,22.6,23.4,25.1)
units <- c(185L,215L,332L,325L,408L,421L,406L,412L,522L,445L,544L,614L)
log_units <- log(units)
n <- length(units)
market.size <- rep(800, n)

plot(temp, units, pch=16)

#
library(brms)
# Linear Gaussian model
lin.mod <- brm(units ~ temp, family="gaussian")
# Log-transformed Linear Gaussian model
log.lin.mod <- brm(log_units ~ temp, family="gaussian")
# Poisson model
pois.mod <- brm(units ~ temp, family="poisson")
# Binomial model
bin.mod <- brm(units | trials(market.size) ~ temp, family="binomial")
