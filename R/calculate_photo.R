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
                            g0,
                            g1
                            ){
  
  A_net <- min(
    Vcmax * (Ci - GammaStar) / (Ci + GammaStar) - r_day,
    Jmax 
  )
  
  #Pseudo code below for calculations
  #Timestep calculations
  A_gross = ifelse(Qinc > 0, min(Wc, Wj), 0)
  
  R = ifelse(Qinc > 0, - r_day, - r_dark)
  
  A_net_leaf = A_gross - R
  
  A_net_plant = A_net - r_root - r_shoot
  
  #Daily sums
  daily_C_plant = sum(A_net_plant * time_step)
  
  daily_photosynthesis = sum(A_gross * time_step)
  
  daily_respiration = sum(r_total * time_step)
  
  #Output into 2-element list with daily total and hourly values
  
}