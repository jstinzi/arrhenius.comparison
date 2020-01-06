#' Functions for calculating respiration
#'
#' @param leaf_Q10 Leaf thermal sensitivity
#' @param root_Q10 Root thermal sensitivity
#' @param shoot_Q10 Shoot thermal sensitivity
#' @param leaf_k25 Leaf respiration at 25 Celsius
#' @param root_k25 Root respiration at 25 Celsius
#' @param shoot_k25 Shoot respiration at 25 Celsius
#' @param leaf_area Leaf area in m2
#' @param root_mass Root mass in g
#' @param shoot_mass Shoot mass in g
#' @param Temp Temperature in Kelvin
#'
#' @return Calculates total respiration
#' @rdname r_functions
#' @export
calculate_r <- function(leaf_Q10,
                        root_Q10,
                        shoot_Q10,
                        leaf_k25,
                        root_k25,
                        shoot_k25,
                        leaf_area,
                        root_mass,
                        shoot_mass,
                        Temp){
  #Calculate leaf respiration
  leaf_resp <- Q10_funct(k25 = leaf_k25,
                         Q10 = leaf_Q10,
                         Temp = Temp) * leaf_area
  #Calculate root respiration
  root_resp <- Q10_funct(k25 = root_k25,
                         Q10 = root_Q10,
                         Temp = Temp) * root_mass
  #Calculate shoot respiration
  shoot_resp <- Q10_funct(k25 = shoot_k25,
                         Q10 = shoot_Q10,
                         Temp = Temp) * shoot_mass
  #Calculate total respiration
  total_resp <- leaf_resp + root_resp + shoot_resp
  #Create dataframe with respiration values
  df_resp <- data.frame(cbind(leaf_resp,
                              root_resp,
                              shoot_resp,
                              total_resp))
  #Add column names
  colnames(df_resp) <- c("leaf_respiration",
                         "root_respiration",
                         "shoot_respiration",
                         "total_respiration")
  #Return dataframe with respiration values
  return(df_resp)
}