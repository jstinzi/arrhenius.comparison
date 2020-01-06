calculate_photo <- function(data_phys, #physiology dataframe
                            data_env, #environmental dataframe
                            varnames = list("GammaStar",
                                            "Jmax",
                                            "Km",
                                            "r_day",
                                            "Ca",
                                            "Oi",
                                            "RH",
                                            "Temp"),
                            gbw,
                            g0,
                            g1
                            ){
  #Calculate biological parameters based on temperature
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