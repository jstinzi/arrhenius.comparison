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
#' processes. Includes Arrhenius (1915), canonical modified Arrhenius (Medlyn
#' et al., 2002 based on Johnson et al. 1942), rederived modified Arrhenius
#' (from Johnson et al., 1942), and a Q10 response for respiration.
#' 
#' Arrhenius S. 1915. Quantitative laws in biological chemistry. Bell.
#' 
#' Johnson, FH, Eyring H, WIlliams RW. 1942. The nature of enzyme
#' inhibitions in bacterial luminescence: sulfanilamide, urethane,
#' temperature and pressure. J Cell Comp Physiol 20:247-268
#' 
#' Medlyn BE, Dreyer E, Ellsworth D, Forstreuter M, Harley PC,
#' Kirschbaum MUF, Le Roux X, Montpied P, Strassemeyer J, Walcroft A,
#' Wang K, Loutstau D. 2002. Temperature response of parameters of a
#' biochemically based model of photosynthesis. II. A review of
#' experimental data. Plant Cell Environ 25:1167-1179
#' 
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

#' @rdname t_functions
#' @export
Arrh_base <- function(k25, Ea, Temp){
  k25 * exp((Ea*(Temp - 298.15))/(298.15*0.008314*Temp))
}