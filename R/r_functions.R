#' Functions for calculating respiration
#'
#' @param data Dataframe with temperature and light data
#' @param varnames List of variable names. Should include temperature
#' and light intensity
#' @param leaf_Q10_day Leaf respiration thermal sensitivity during the day
#' @param leaf_Q10_night Leaf respiration thermal sensitivity during the night
#' @param root_Q10 Root thermal sensitivity
#' @param shoot_Q10 Shoot thermal sensitivity
#' @param leaf_k25_day Leaf respiration at 25 Celsius during the day
#' @param leaf_k25_night Leaf respiration at 25 Celsius during the night
#' @param root_k25 Root respiration at 25 Celsius
#' @param shoot_k25 Shoot respiration at 25 Celsius
#' @param leaf_area Leaf area in m2
#' @param root_mass Root mass in g
#' @param shoot_mass Shoot mass in g
#'
#' @return Calculates total respiration
#' @rdname r_functions
#' @export
calculate_r <- function(data,
                        varnames = list(Qin = "Qin",
                                        Temp = "Temp",
                        leaf_Q10_day,
                        leaf_Q10_night,
                        root_Q10,
                        shoot_Q10,
                        leaf_k25_day,
                        leaf_k25_night,
                        root_k25,
                        shoot_k25,
                        leaf_area,
                        root_mass,
                        shoot_mass)){
  #Set variable names
  data$Qin <- data[, varnames$Qin]
  data$Temp <- data[, varnames$Temp]
  
  #Create empty output dataframe
  df_r <- data.frame(rep(0, nrow(data)),
                     rep(0, nrow(data)),
                     rep(0, nrow(data)),
                     rep(0, nrow(data)),
                     data$Temp)
  
  #Add column names
  colnames(df_resp) <- c("leaf_respiration",
                         "root_respiration",
                         "shoot_respiration",
                         "total_respiration",
                         "Temp")
  for(i in 1:nrow(data)){
  #Calculate leaf respiration
  df_r$leaf_respiration[i] <- if(data$Qin[i] > 0){
    Q10_funct(k25 = leaf_k25_day,
              Q10 = leaf_Q10_day,
              Temp = data$Temp[i]) * leaf_area
  } else {
    Q10_funct(k25 = leaf_k25_night,
              Q10 = leaf_Q10_night,
              Temp = data$Temp[i]) * leaf_area
    }
  #Calculate root respiration
  df_r$root_respiration[i] <- Q10_funct(k25 = root_k25,
                         Q10 = root_Q10,
                         Temp = data$Temp[i]) * root_mass
  #Calculate shoot respiration
  df_r$shoot_respiration[i] <- Q10_funct(k25 = shoot_k25,
                         Q10 = shoot_Q10,
                         Temp = data$Temp[i]) * shoot_mass
  }
  
  #Calculate total respiration
  df_r$total_respiration <- leaf_respiration + 
    root_respiration + shoot_respiration
  
  #Return dataframe with respiration values
  return(df_resp)
}