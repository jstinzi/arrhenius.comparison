#' Title
#'
#' @param G25 
#' @param G_Ea 
#' @param Jmax25 
#' @param Jmax_dS 
#' @param Jmax_Ea 
#' @param Jmax_Hd 
#' @param Km25 
#' @param Km_Ea 
#' @param Temp 
#' @param Vcmax25 
#' @param Vcmax_dS 
#' @param Vcmax_Ea 
#' @param Vcmax_Hd 
#'
#' @return
#' @export
#'
#' @examples
calculate_p <- function(G25,
                        G_Ea,
                        Jmax25,
                        Jmax_dS,
                        Jmax_Ea,
                        Jmax_Hd,
                        Km25,
                        Km_Ea,
                        Temp,
                        Vcmax25,
                        Vcmax_dS,
                        Vcmax_Ea,
                        Vcmax_Hd
                        ){
  #Calculate biological parameters based on temperature
  GammaStar <- Arrh_base(k25 = G25,
                         Ea = G_Ea,
                         Temp = Temp)
  
  Jmax_medlyn <- Arrh_medlyn(k25 = Jmax25,
                             delS = Jmax_dS,
                             Ea = Jmax_Ea,
                             Hd = Jmax_Hd,
                             Temp = Temp)
  
  Jmax_new <- Arrh_new(k25 = Jmax25,
                       delS = Jmax_dS,
                       Ea = Jmax_Ea,
                       Hd = Jmax_Hd,
                       Temp = Temp)
  
  Km <- Arrh_base(k25 = Km25,
                  Ea = Km_Ea,
                  Temp = Temp)
  
  Vcmax_medlyn <- Arrh_medlyn(k25 = Vcmax25,
                              delS = Vcmax_dS,
                              Ea = Vcmax_Ea,
                              Hd = Vcmax_Hd,
                              Temp = Temp)
  
  Vcmax_new <- Arrh_new(k25 = Vcmax25,
                        delS = Vcmax_dS,
                        Ea = Vcmax_Ea,
                        Hd = Vcmax_Hd,
                        Temp = Temp)
}