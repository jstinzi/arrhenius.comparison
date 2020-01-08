#' Calculating photosynthetic parameters
#'
#' @param G25 Photorespiratory CO2 compensation point (umol mol-1)
#' @param G_Ea Activation energy of G25 (kJ mol-1)
#' @param Jmax25_m Maximum electron transport rate at 25 Celsius for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Jmax_dS_m Entropy parameter of Jmax (kJ mol-1) for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Jmax_Ea_m Activation energy of Jmax (kJ mol-1) for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Jmax_Hd_m Deactivation energy of Jmax (kJ mol-1) for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Jmax25_n Maximum electron transport rate at 25 Celsius for the
#' rederived modified Arrhenius model
#' @param Jmax_dS_n Entropy parameter of Jmax (kJ mol-1) for the
#' rederived modified Arrhenius model
#' @param Jmax_Ea_n Activation energy of Jmax (kJ mol-1) for the
#' rederived modified Arrhenius model
#' @param Jmax_Hd_n Deactivation energy of Jmax (kJ mol-1) for the
#' rederived modified Arrhenius model
#' @param Km25 Michaelis-Menten coefficient for rubisco at 25 Celsius
#' @param Km_Ea Activation energy of Km (kJ mol-1)
#' @param Temp Temperature in K
#' @param Vcmax25_m Maximum rubisco carboxylation rate at 25 Celsius for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Vcmax_dS_m Entropy parameter of Vcmax (kJ mol-1) for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Vcmax_Ea_m Activation energy of Vcmax (kJ mol-1) for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Vcmax_Hd_m Deactivation energy of Vcmax (kJ mol-1) for the
#' Medlyn et al. 2002 modified Arrhenius model
#' @param Vcmax25_n Maximum rubisco carboxylation rate at 25 Celsius for the
#' rederived modified Arrhenius model
#' @param Vcmax_dS_n Entropy parameter of Vcmax (kJ mol-1) for the
#' rederived modified Arrhenius model
#' @param Vcmax_Ea_n Activation energy of Vcmax (kJ mol-1) for the
#' rederived modified Arrhenius model
#' @param Vcmax_Hd_n Deactivation energy of Vcmax (kJ mol-1) for the
#' rederived modified Arrhenius model
#'
#' @return Calculates photosynthetic parameters for the Farquhar et al. (1980)
#' model of photosynthesis. See ?t_functions for details on temperature scaling.
#' 
#' Farquhar GD, et al. 1980. A biochemical model of photosynthetic CO2 
#' assimilation in leaves of C3 species. Planta 149: 78-90.
#' @export
calculate_p <- function(G25,
                        G_Ea,
                        Jmax25_m,
                        Jmax_dS_m,
                        Jmax_Ea_m,
                        Jmax_Hd_m,
                        Jmax25_n,
                        Jmax_dS_n,
                        Jmax_Ea_n,
                        Jmax_Hd_n,
                        Km25,
                        Km_Ea,
                        Temp,
                        Vcmax25_m,
                        Vcmax_dS_m,
                        Vcmax_Ea_m,
                        Vcmax_Hd_m,
                        Vcmax25_n,
                        Vcmax_dS_n,
                        Vcmax_Ea_n,
                        Vcmax_Hd_n
                        ){
  #Calculate biological parameters based on temperature
  GammaStar <- Arrh_base(k25 = G25,
                         Ea = G_Ea,
                         Temp = Temp)
  
  Jmax_medlyn <- Arrh_medlyn(k25 = Jmax25_m,
                             delS = Jmax_dS_m,
                             Ea = Jmax_Ea_m,
                             Hd = Jmax_Hd_m,
                             Temp = Temp)
  
  Jmax_new <- Arrh_new(k25 = Jmax25_n,
                       delS = Jmax_dS_n,
                       Ea = Jmax_Ea_n,
                       Hd = Jmax_Hd_n,
                       Temp = Temp)
  
  Km <- Arrh_base(k25 = Km25,
                  Ea = Km_Ea,
                  Temp = Temp)
  
  Vcmax_medlyn <- Arrh_medlyn(k25 = Vcmax25_m,
                              delS = Vcmax_dS_m,
                              Ea = Vcmax_Ea_m,
                              Hd = Vcmax_Hd_m,
                              Temp = Temp)
  
  Vcmax_new <- Arrh_new(k25 = Vcmax25_n,
                        delS = Vcmax_dS_n,
                        Ea = Vcmax_Ea_n,
                        Hd = Vcmax_Hd_n,
                        Temp = Temp)
  
  #Create dataframe output for photosynthetic parameters
  df_p <- data.frame(cbind(GammaStar, Jmax_medlyn, Jmax_new,
                                Km, Vcmax_medlyn, Vcmax_new, Temp))
  
  #Return photosynthetic parameters
  return(df_p)
}