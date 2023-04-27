#Process model functions
#Author: Mary Lofton
#Date: 18APR23

#Purpose: declare 4 temperature functions (from Grimaud et al. 2017) and 6
#light functions (from Baklouti et al. 2006) to combine factorially in forecast
#models

#Grimaud, G.M., Mairet, F., Sciandra, A. et al. Modeling the temperature effect on the specific growth rate of phytoplankton: a review. Rev Environ Sci Biotechnol 16, 625–645 (2017). https://doi.org/10.1007/s11157-017-9443-0
#M. Baklouti, F. Diaz, C. Pinazo, V. Faure, B. Quéguiner, Investigation of mechanistic formulations depicting phytoplankton dynamics for models of marine pelagic ecosystems and description of a new model, Progress in Oceanography, Volume 71, Issue 1, 2006, Pages 1-33, ISSN 0079-6611, https://doi.org/10.1016/j.pocean.2006.05.002.

#Light functions are reproduced from their specification in GLM-AED
#https://github.com/AquaticEcoDynamics/libaed-water/blob/master/src/aed_bio_utils.F90

#Light functions

monod <- function(swr, I_K){
  x = swr/I_K
  fI = x / (1 + x)
  return(fI)
}

steele <- function(swr, I_S){
  x = swr/I_S
  fI = x * exp(1 - x)
  if(swr < 5e-5 | fI < 5e-5){ fI = 0.0 }
  return(fI)
}

webb <- function(swr, I_K){
  x = swr/I_K
  fI = 1 - exp(-x)
  return(fI)
}

jassby_platt <- function(swr, I_K){
  x = swr/I_K
  fI = tanh(x)
  return(fI)
}

chalker <- function(swr, I_K, eps = 0.5){
  x = swr/I_K
  fI = (exp(x * (1 + eps)) - 1) / (exp(x * (1 + eps)) + eps)
  return(fI)
}

klepper_ebenhoh <- function(swr, I_S, A = 5.0){
  x = swr/I_S
  fI = ((2.0 + A) * x) / ( 1 + (A * x) + (x * x) )
  return(fI)
}

#Temperature functions

ctmi <- function(wtemp, Tmin, Topt, Tmax, muopt){
  
  if(wtemp < Tmin){fT = 0}
  if(wtemp > Tmax){fT = 0}
  if(wtemp > Tmin & wtemp < Tmax){
    phi = ((wtemp - Tmax)*((wtemp - Tmin)^2)) / ((Topt - Tmin)*(((Topt - Tmin)*(wtemp - Topt)) - ((Topt - Tmax)*(Topt + Tmin - 2*wtemp))))
    fT = muopt*phi*24
  }
  
  return(fT)
}

blanchard <- function(wtemp, Topt, Tmax, b, muopt){
  mumax = muopt * ((Tmax - wtemp) / (Tmax - Topt))^b * exp(1)^(-b*((Tmax - wtemp) / (Tmax - Topt)))
  fT = mumax*24
  return(fT)
}

hinshelwood <- function(wtemp, A1, E1, A2, E2, R = 8.3145){
  f1T = A1*exp(1)^(-E1*(R/wtemp))
  f2T = A2*exp(1)^(-E2*(R/wtemp))
  mumax = f1T - f2T
  fT = mumax*24
  return(fT)
}

eppley_norberg <- function(wtemp, z, w, a, b){
  mumax = (1 - ((wtemp - z) / w )^2) * a * exp(1)^(b*wtemp)
  fT = mumax*24
  return(fT)
}




#build likelihood model
#build likelihood model
rmse <- function(par, chla, wtemp, swr){
  #calculate rmse
  sqrt(mean((chla - proc_model(par, wtemp, chla, swr))^2))
}

LL_gamma <- function(par, chla, wtemp, swr){
  #calculate log likelihood
  -sum(dgamma(chla, shape = (proc_model(par, wtemp, chla, swr)^2/par[8]), rate =  proc_model(par, wtemp, chla, swr)/par[8], log = TRUE))
}
