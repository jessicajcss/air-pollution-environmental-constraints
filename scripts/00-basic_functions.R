#############################################################################
#############################################################################

# Basic functions ----


###################
# Last update: 2022-04-05
# Author: Santos-Silva, J. C. <jessica.jcss@gmail.com>

#############################################################################
#############################################################################



## WIND COMPONENTS ----

### Create columns to and calculate the u and v wind components to all data ###

u.wind <- function(ws, wd) {
  u <- - ws * sin(2 * pi * wd/360)
  return(u)
}

v.wind <- function(ws, wd) {
  v <- - ws * cos(2 * pi * wd/360)
  return(v)
}



### Calculate wind direction for this hour-long period ###
wind_direction <- function(u, v) {
  x <- (atan2(u, v) * 360/2/pi) + 180
  return(x) 
}

### Calculate vectorial wind speed for this hour-long period ###
wind_speed <- function(u, v) {
  z <- ((u^2 + v^2)^0.5)
  return(z)
}


## HORIZONTAL WIND CIRCULATION: requires library(bazar) ----

### Calculate the discrete integral quantities of the ‘resultant transport distance’ (L), which is the net vector displacement ###

L_it <- function(time_T, u, v) {
  L <- time_T * sqrt((sumNA(u, na.rm = T)^2) + (sumNA(v, na.rm = T)^2))
  return(L)
}


### Calculate the ‘wind run’ (S), which is the wind scalar sum ###

S_it <- function(time_T, u, v) {
  S <- time_T * (sumNA(sqrt((u)^2 + (v)^2), na.rm = T))
  return(S)
}


### Calculate the ‘recirculation factor’ (R), based on the ratio between L and S ###

R_it <- function(L, S) {
  R <- 1 - (L/S)
  return(R)
}







## Risk assessment ----

### Geo-accumulation index ###
# x unit

geo_index <- function(xexp, xcr) {
  y <- log2(xexp/(1.5*xcr))
  return(y)
}


### Ecological Risk Index ###

pot_ecorisk <- function(tr, xexp, xcr) {
  y <- tr*(xexp/xcr)
  return(y)
}
