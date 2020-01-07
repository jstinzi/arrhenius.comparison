#' Title
#'
#' @param data_phys 
#' @param data_env 
#' @param varnames 
#' @param alpha 
#' @param g0 
#' @param g1 
#' @param Oi 
#' @param phi 
#'
#' @return
#' @export
#'
#' @examples
calculate_photo <- function(data_phys, #physiology dataframe
                            data_env, #environmental dataframe
                            varnames = list(GammaStar = "GammaStar",
                                            Jmax = "Jmax",
                                            Km = "Km",
                                            leaf_respiration = "leaf_respiration",
                                            root_respiration = "root_respiration",
                                            shoot_respiration = "shoot_respiration",
                                            total_respiration = "total_respiration",
                                            Ca = "Ca",
                                            Qin = "Qin",
                                            VPD = "VPD",
                                            Temp = "Temp",
                                            Vcmax = "Vcmax"),
                            alpha,
                            g0,
                            g1,
                            Oi,
                            phi
                            ){
  #Set variable names
  #Physiology
  data_phys$GammaStar <- data_phys[, varnames$GammaStar]
  data_phys$Km <- data_phys[, varnames$Km]
  data_phys$Jmax <- data_phys[, varnames$Jmax]
  data_phys$Vcmax <- data_phys[, varnames$Vcmax]
  data_phys$leaf_respiration <- data_phys[, varnames$leaf_respiration]
  data_phys$root_respiration <- data_phys[, varnames$root_respiration]
  data_phys$shoot_respiration <- data_phys[, varnames$shoot_respiration]
  data_phys$total_respiration <- data_phys[, varnames$total_respiration]
  #Environment
  data_env$Ca <- data_env[, varnames$Ca]
  data_env$Qin <- data_env[, varnames$Qin]
  data_env$VPD <- data_env[, varnames$VPD]
  data_env$Temp <- data_env[, varnames$Temp]
  
  #Create empty output dataframe
  output <- data.frame(cbind(rep(0, nrow(data_phys)),
                             ))
  colnames(output) <- c("Wc", "Wj", "Ci", "gs",
                        "A_gross", "A_net_leaf",
                        "A_net_plant")
  output <- cbind(data_env, data_phys, output)
  
  
  #The approach below is based on Farquhar et al. 1980 as
  #implemented by Way et al. 2011
  
  #Need nested loops - j loop for minimization and system closure
  #i loop for output data
  
  #Initial guess for Ci
  Ci1 <- data_env$Ca[i] * 0.99
  
  #Need to start loop here
  for(j in 1:1000){
  if(j == 1){
    Ci[j] <- Ci1
  }
  Wc[i] <- 
    data_phys$Vcmax[i] * (Ci[j] - data_phys$GammaStar[i]) / 
    (Ci[j] + data_phys$Km[i])
  
  Wj[i] <-
    min(data_phys$Jmax[i], alpha * phi * data_env$Qin[i] * 
          (Ci[j] - data_phys$GammaStar[i]) / 
          (2 * data_phys$GammaStar[i] + Ci[j]))
  #Calculate gross photosynthesis as the minimum of Wc, Wj
  A_gross[i] <- min(Wc, Wj)
  
  A_net_leaf[i] <- A_gross[i] - data_phys$leaf_respiration[i]
  
  gs[i] <- g0 + 1.6 * ( 1 + g1 / sqrt(data_env$VPD[i])) * (A_net_leaf[i] / data_env$Ca[i])
  
  Ci[j] <- data_env$Ca[i] - A_net_leaf[i] / (gs[i] / 1.6)
  
  if(j == 1){
    err = Ci1 - Ci[j]
  } else {
    err = Ci[j] - Ci[j - 1]
  }
  
  if(err < 0.01 | j == 1000){
    Ci[i] <- Ci[j]
  }
  
  if(err < 0.01 & j <= 1000){
    convergence[i] <- TRUE
  } else {
    convergence[i] <- FALSE
  }
  
  }#End J loop
  
  A_net_plant <- A_net_leaf - 
    data_phys$root_respiration - data_phys$shoot_respiration
  
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