#' Calculating CO2 exchange and carbon exchange
#'
#' @param data_phys Dataframe containing physiological data
#' @param data_env Dataframe containing environmental data
#' @param varnames List of variable names if different from the default
#' @param alpha Absorbance of photosynthetically active radiation
#' @param g0 Intercept of the Medlyn et al. (2011) stomatal conductance
#' model in mol m-2 s-1
#' @param g1 Slope of the Medlyn et al. (2011) stomatal conductance model
#' @param leaf_area Leaf area in m^2
#' @param phi Maximum quantum efficiency of CO2 assimilation
#' @param theta Curvature of the light response
#' @param time_step Number of seconds per unit time in the environmental data
#'
#' @return Calculates CO2 assimilation on a leaf and whole-plant level. Sums
#' carbon exchange across the entire environment input. Note that this is
#' daily carbon exchange if the environmental input is 1 day, otherwise it
#' is just total carbon exchange for the inputs. Output is a list containing
#' 2 dataframes - element 1 is the gas exchange data, element 2 is the summed
#' carbon data. Carbon exchange outputs are in g/plant/time period. See package
#' vignette for how to use this function.
#' @export
calculate_photo <- function(data_phys, #physiology dataframe
                            data_env, #environmental dataframe
                            varnames = list(GammaStar = "GammaStar",
                                            Jmax = "Jmax",
                                            Km = "Km",
                                            leaf_respiration = "leaf_respiration",
                                            root_respiration = "root_respiration",
                                            shoot_respiration = "shoot_respiration",
                                            total_respiration_plant = "total_respiration_plant",
                                            Ca = "Ca",
                                            Qin = "Qin",
                                            VPD = "VPD",
                                            Temp = "Temp",
                                            Vcmax = "Vcmax"),
                            alpha = 0.8,
                            g0 = 0.0225,
                            g1 = 7.7527,
                            theta = 0.7,
                            leaf_area,
                            phi,
                            time_step
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
  data_phys$total_respiration_plant <- data_phys[, varnames$total_respiration_plant]
  #Environment
  data_env$Ca <- data_env[, varnames$Ca]
  data_env$Qin <- data_env[, varnames$Qin]
  data_env$VPD <- data_env[, varnames$VPD]
  data_env$Temp <- data_env[, varnames$Temp]
  
  #Create empty output dataframe
  output <- data.frame(cbind(rep(0, nrow(data_phys)),
                             rep(0, nrow(data_phys)),
                             rep(0, nrow(data_phys)),
                             rep(0, nrow(data_phys)),
                             rep(0, nrow(data_phys)),
                             rep(0, nrow(data_phys)),
                             rep(0, nrow(data_phys)),
                             rep(0, nrow(data_phys))))
  colnames(output) <- c("Wc", "Wj", "Ci", "gs",
                        "A_gross", "A_gross_plant", "A_net_leaf",
                        "A_net_plant")
  output <- cbind(data_env, data_phys, output)
  
  
  #The approach below is based on Farquhar et al. 1980 as
  #implemented by Way et al. 2011
  
  #Need nested loops - j loop for minimization and system closure
  #i loop for output data
  for(i in 1:nrow(data_phys)){
  #Initial guess for Ci
  Ci1 <- data_env$Ca[i] * 0.99
  Ci <- NULL
  Ci[1] <- Ci1
  gs <- NULL
  convergence <- NULL
  Wc <- NULL
  Wj <- NULL
  A_gross <- NULL
  A_net_leaf <- NULL
  err <- NULL
  #Need to start loop here
  for(j in 2:1000){
  Wc <- 
    data_phys$Vcmax[i] * (Ci[j - 1] - data_phys$GammaStar[i]) / 
    (Ci[j - 1] + data_phys$Km[i])
  #Old equation
  #Wj <-
  #  min(data_phys$Jmax[i], alpha * phi * data_env$Qin[i] * 
  #        (Ci[j - 1] - data_phys$GammaStar[i]) / 
  #        (2 * data_phys$GammaStar[i] + Ci[j - 1]))
  #rewrite equation
  a <- theta
  b <- - (0.5 * alpha * phi * data_env$Qin[i] + data_phys$Jmax[i])
  c <- 0.5 * alpha * phi * theta * data_env$Qin[i] * data_phys$Jmax[i]
  jetr <- min( ((-b + sqrt(b ^ 2 - 4 * a * c)) / (2 * a)),
               ((-b - sqrt(b ^ 2 - 4 * a * c)) / (2 * a)))
  Wj <- jetr * (1 / 4) * (Ci[j - 1] - data_phys$GammaStar[i]) /
    (2 * data_phys$GammaStar[i] + Ci[j - 1])
    
  
  #Calculate gross photosynthesis as the minimum of Wc, Wj
  A_gross <- min(Wc, Wj)
  
  A_net_leaf <- A_gross - data_phys$leaf_respiration[i]
  
  gs <- g0 + 1.6 * ( 1 + g1 / sqrt(data_env$VPD[i])) * (A_net_leaf / data_env$Ca[i])
  Ci[j] <- data_env$Ca[i] - A_net_leaf / (gs / 1.6)
  
  err = Ci[j] - Ci[j - 1]
  
  #if(err < 0.01 | j == 1000){
  #  Ci[i] <- Ci[j]
  #}
  
  #if(err < 0.01 & j <= 1000){
  #  convergence[i] <- TRUE
  #} else {
  #  convergence[i] <- FALSE
  #}
  
  }#End J loop - j loop works
  
  output$Wc[i] <- Wc
  output$Wj[i] <- Wj
  output$A_gross[i] <- A_gross
  output$A_gross_plant[i] <- A_gross * leaf_area
  output$gs[i] <- gs
  output$Ci[i] <- Ci[j]
  
  output$A_net_leaf[i] <- A_net_leaf
  output$A_net_plant[i] <- A_net_leaf * leaf_area - 
    data_phys$root_respiration_plant[i] - data_phys$shoot_respiration_plant[i]
  
}#End I loop

  #Daily sums in g/day
  daily_C_plant <- sum(output$A_net_plant * time_step) * 12.01 / 1000000
  
  daily_photosynthesis <- sum(output$A_gross_plant * time_step) * 12.01 / 1000000
  
  daily_respiration <- sum(output$total_respiration_plant * time_step) * 12.01 / 1000000
  output2 <- data.frame(cbind(daily_C_plant,
                              daily_photosynthesis,
                              daily_respiration))
  
  out <- list(NULL)
  out[[1]] <- output
  out[[2]] <- output2  
  return(out)
}
