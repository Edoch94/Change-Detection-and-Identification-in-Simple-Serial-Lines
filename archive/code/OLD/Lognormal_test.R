rm(list = ls())

library(magrittr)
library(tidyverse)
library(ggplot)
library(plotly)
library(lognorm)

# OLD
mediaLN <- 1
sigmaLN <- 2
npoints <-100000

vec_rgen <- rlnorm(npoints, meanlog = mediaLN, sdlog = sigmaLN)
mean(vec_rgen) # media campionaria
var(vec_rgen) # varianza campionaria

getLognormMoments(mediaLN,sigmaLN)
exp(mediaLN + (sigmaLN^2)/2) # media distribuzione
( exp( sigmaLN^2 ) - 1 ) * exp( 2*mediaLN + sigmaLN^2 ) # varianza distribuzione

# Usando libreria lognormal
paramLN <- getParmsLognormForMoments(mean = 15, var = 20)
mediaLN_2 <- paramLN[1]
sigmaLN_2 <- paramLN[2]

getLognormMoments(mediaLN_2, sigmaLN_2)

vec_rgen <- rlnorm(npoints, meanlog = mediaLN_2, sdlog = sigmaLN_2)
mean(vec_rgen) # media campionaria
var(vec_rgen) # varianza campionaria


ggplotly(
  ggplot(data.frame(x = vec_rgen), aes(x)) +
    geom_density() +
    stat_function(fun = dlnorm, args = list(mean = mediaLN, sd = sigmaLN), colour = "red") +
    xlim(0,50)
)
