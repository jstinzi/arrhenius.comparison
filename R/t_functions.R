#' Temperature response functions
#'
#' @param k25 Rate of process at 25 Celsius
#' @param delS Entropy parameter in kJ/mol
#' @param Ea Activation enegy in kJ/mol
#' @param Hd Deactivation energy in kJ/mol
#' @param Q10 Thermal sensitivity of process, representing X-fold change per
#' 10 Celsius change in temperature
#' @param Temp Temperature in Kelvin
#'
#' @return Provides functions for modeling temperature responses of biological
#' processes
#' 
#' ADD REFERENCES HERE
#' @rdname t_functions
#' @export
Arrh_medlyn <- function(k25, delS, Ea, Hd, Temp){
  k25 * exp((Ea*(Temp - 298.15))/(298.15*0.008314*Temp)) * 
    (1+exp((298.15*delS - Hd)/(298.15*0.008314))) / 
    (1+exp((Temp*delS-Hd)/(Temp*0.008314)))
}
#' @rdname t_functions
#' @export
Arrh_new <- function(k25, delS, Ea, Hd, Temp){
  k25 * (Temp / 298.15) * exp((Ea*(Temp - 298.15))/(298.15*0.008314*Temp)) * 
    (1+exp((298.15*delS - Hd)/(298.15*0.008314))) / 
    (1+exp((Temp*delS-Hd)/(Temp*0.008314)))
}

#' @rdname t_functions
#' @export
Q10_funct <- function(k25, Q10, Temp){
  k25 * Q10 ^ ( (Temp - 298.15) / 10)
}