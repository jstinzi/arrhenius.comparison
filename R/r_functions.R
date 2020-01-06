night_resp <- function(leaf_Q10,
                       root_Q10,
                       shoot_Q10,
                       leaf_k25,
                       root_k25,
                       shoot_k25,
                       leaf_area,
                       root_mass,
                       shoot_mass,
                       Temp){
  leaf_resp <- Q10_funct(k25 = leaf_k25,
                         Q10 = leaf_Q10,
                         Temp = Temp) * leaf_area
  root_resp <- Q10_funct(k25 = root_k25,
                         Q10 = root_Q10,
                         Temp = Temp) * root_mass
  shoot_resp <- Q10_funct(k25 = shoot_k25,
                         Q10 = shoot_Q10,
                         Temp = Temp) * shoot_mass
}