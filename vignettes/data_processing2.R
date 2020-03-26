#Workflows
#Nick Smith data
#####
setwd("C:/Users/Joseph Stinziano/Desktop/DesktopBackUp2018_09_16/Postdoc/Manuscripts/Kumarathunge-photom-d015fd064de7/Data")
library(dplyr)
library(plantecophys)
library(tidyr)
data <- read.csv("nick_smith_aci_data_processed.csv",
                 stringsAsFactors = FALSE)
data <- unite(data, col = "ID", c("Species",
                                  "T_measurement",
                                  "Replicate",
                                  "T_growth"))

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "T_measurement",
                                 "Replicate",
                                 "T_growth"),
                        sep = "_")
write.csv(fitted_data, "./fitted2/nick_smith.csv")
remove(data)
remove(fits)
remove(fitted_data)

#Tarvainen data
#####
data <- read.csv("scots_pine_tarvainen.csv",
                 stringsAsFactors = FALSE)
data <- unite(data, col = "ID", c("species",
                                  "Target.Temp",
                                  "Shoot"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("species",
                                 "Target.Temp",
                                 "Shoot"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/scots_pine_tarvainen.csv")
remove(data)
remove(fits)
remove(fitted_data)

#Walcroft peach data
#####
data <- read.csv("walcroft peach.csv",
                 stringsAsFactors = FALSE)
data$T_measurement <- trunc(data$Tleaf)

data <- unite(data, col = "ID", c("Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}

fitted_data <- coef(fits)

fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/walcroft peach.csv")
remove(data)
remove(fits)
remove(fitted_data)



#wtc1 processed data
#####
data <- read.csv("wtc1_Aci_processed.csv",
                 stringsAsFactors = FALSE)
data <- split(data, data$Curve)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- unite(data, col = "ID", c("Season",
                                  "Chamber",
                                  "Curve",
                                  "T_measurement",
                                  "CO2_Treat",
                                  "Water_treat"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Season",
                                 "Chamber",
                                 "Curve",
                                 "T_measurement",
                                 "CO2_Treat",
                                 "Water_treat"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/wtc1_Aci_processed.csv")
remove(data)
remove(fits)
remove(fitted_data)

#wtc4_photo_temp_all data
#####
data <- read.csv("wtc4_photo_temp_all.csv",
                 stringsAsFactors = FALSE)


data <- unite(data, col = "ID", c("Season",
                                  "Chamber",
                                  "Ttreatment"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Season",
                                  "Chamber",
                                  "Ttreatment"),
              sep = "-")
data <- unite(data, col = "ID", c("Season",
                                  "Chamber",
                                  "Ttreatment",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Season",
                                 "Chamber",
                                 "Ttreatment",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/wtc4_photo_temp_all.csv")
remove(data)
remove(fits)
remove(fitted_data)


#euc_pauciflora_mk data
#####
data <- read.csv("euc_pauciflora_mk.csv",
                 stringsAsFactors = FALSE)
data$T_measurement <- data$Tleaf
data <- unite(data, col = "ID", c("Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/euc_pauciflora_mk.csv")
remove(data)
remove(fits)
remove(fitted_data)



#great_photo_shortterm data
#####
data <- read.csv("great_photo_shortterm.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Code",
                                  "W_treatment",
                                  "TleafFac",
                                  "location"),
              sep = "_")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Code",
                                            "W_treatment",
                                            "TleafFac",
                                            "location"),
                 sep = "_")
data <- unite(data, col = "ID", c("Code",
                                  "W_treatment",
                                  "TleafFac",
                                  "location",
                                  "T_measurement"),
              sep = "_")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}

fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Code",
                                 "W_treatment",
                                 "TleafFac",
                                 "location",
                                 "T_measurement"),
                        sep = "_")
write.csv(fitted_data, "./fitted2/great_photo_shortterm.csv")
remove(data)
remove(fits)
remove(fitted_data)


#gwwACidata_processed_new data
#####
data <- read.csv("gwwACidata_processed_new.csv",
                 stringsAsFactors = FALSE)


data <- unite(data, col = "ID", c("Curve",
                                  "Spp",
                                  "Tree",
                                  "Season"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Curve",
                                            "Spp",
                                            "Tree",
                                            "Season"),
                 sep = "-")
data <- unite(data, col = "ID", c("Curve",
                                  "Spp",
                                  "Tree",
                                  "Season",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Curve",
                                 "Spp",
                                 "Tree",
                                 "Season",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/gwwACidata_processed_new.csv")
remove(data)
remove(fits)
remove(fitted_data)


#hfe_ACidata_processed data
#####
data <- read.csv("hfe_ACidata_processed.csv",
                 stringsAsFactors = FALSE)


data <- unite(data, col = "ID", c("Season",
                                  "Identity",
                                  "Species"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Season",
                                            "Identity",
                                            "Species"),
                 sep = "-")
data <- unite(data, col = "ID", c("Season",
                                  "Identity",
                                  "Species",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Season",
                                 "Identity",
                                 "Species",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/hfe_ACidata_processed.csv")
remove(data)
remove(fits)
remove(fitted_data)


#hfe_An_Tdata_processed data
#####
data <- read.csv("hfe_An_Tdata_processed.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Month",
                                  "Tree"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Month",
                                            "Tree"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Month",
                                  "Tree",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Month",
                                 "Tree",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/hfe_An_Tdata_processed.csv")
remove(data)
remove(fits)
remove(fitted_data)



#hikosaka_aci_data data
#####
data <- read.csv("hikosaka_aci_data.csv",
                 stringsAsFactors = FALSE)


data <- split(data, data$Curve)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- unite(data, col = "ID", c("Curve",
                                  "Year",
                                  "Month",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Curve",
                                 "Year",
                                 "Month",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/hikosaka_aci_data.csv")
remove(data)
remove(fits)
remove(fitted_data)



#kelsey_cater_Aci_PuertoRico data
#####
data <- read.csv("kelsey_cater_Aci_PuertoRico.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Plant.ID",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Plant.ID",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Plant.ID",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Plant.ID",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/kelsey_cater_Aci_PuertoRico.csv")
remove(data)
remove(fits)
remove(fitted_data)



#mike_aspinwall_corymbia_calophylla.V1 data
#####
data <- read.csv("mike_aspinwall_corymbia_calophylla.V1.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("PotID",
                                  "Date",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("PotID",
                                            "Date",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("PotID",
                                  "Date",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Plant.ID",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/mike_aspinwall_corymbia_calophylla.V1.csv")
remove(data)
remove(fits)
remove(fitted_data)



#acitwtc4_cleaned data
#####
data <- read.csv("acitwtc4_cleaned.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Curve",
                                  "Temp_Treatment",
                                  "Species",
                                  "Season"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Curve",
                                            "Temp_Treatment",
                                            "Species",
                                            "Season"),
                 sep = "-")
data <- unite(data, col = "ID", c("Curve",
                                  "Temp_Treatment",
                                  "Species",
                                  "Season",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Curve",
                                 "Temp_Treatment",
                                 "Species",
                                 "Season",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/acitwtc4_cleaned.csv")
remove(data)
remove(fits)
remove(fitted_data)



#alida_mao_Aci_PuertoRico data
#####
data <- read.csv("alida_mao_Aci_PuertoRico.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("species",
                                  "leaf.id",
                                  "Curve",
                                  "date"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("species",
                                            "leaf.id",
                                            "Curve",
                                            "date"),
                 sep = "-")
data <- unite(data, col = "ID", c("species",
                                  "leaf.id",
                                  "Curve",
                                  "date",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("species",
                                 "leaf.id",
                                 "Curve",
                                 "date",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/alida_mao_Aci_PuertoRico.csv")
remove(data)
remove(fits)
remove(fitted_data)


#dillaway_etal data
#####
data <- read.csv("dillaway_etal.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Leaf",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Leaf",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Leaf",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Leaf",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/dillaway_etal.csv")
remove(data)
remove(fits)
remove(fitted_data)



#eucFace_data data
#####
data <- read.csv("eucFace_data.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Tree",
                                  "Curve",
                                  "Name"),
              sep = "_")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Tree",
                                            "Curve",
                                            "Name"),
                 sep = "_")
data <- unite(data, col = "ID", c("Tree",
                                  "Curve",
                                  "Name",
                                  "T_measurement"),
              sep = "_")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Tree",
                                 "Curve",
                                 "Name",
                                 "T_measurement"),
                        sep = "_")
write.csv(fitted_data, "./fitted2/eucFace_data.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Tumbarumba_ACidata_processed data
#####
data <- read.csv("Tumbarumba_ACidata_processed.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Curve",
                                  "Site",
                                  "Species",
                                  "Month"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Curve",
                                            "Site",
                                            "Species",
                                            "Month"),
                 sep = "-")
data <- unite(data, col = "ID", c("Curve",
                                  "Site",
                                  "Species",
                                  "Month",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)
for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Curve",
                                 "Site",
                                 "Species",
                                 "Month",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Tumbarumba_ACidata_processed.csv")
remove(data)
remove(fits)
remove(fitted_data)



#TumbarumbaGasex_Spot_Medlyndata
#####
data <- read.csv("TumbarumbaGasex_Spot_Medlyn.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Tree",
                                  "Season",
                                  "Date"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Tree",
                                            "Season",
                                            "Date"),
                 sep = "-")
data <- unite(data, col = "ID", c("Tree",
                                  "Season",
                                  "Date",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Tree",
                                 "Season",
                                 "Date",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/TumbarumbaGasex_Spot_Medlyn.csv")
remove(data)
remove(fits)
remove(fitted_data)


#WTC2_ACidata_processed
#####
data <- read.csv("WTC2_ACidata_processed.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Temp.treat",
                                  "Chamber"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Temp.treat",
                                            "Chamber"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Temp.treat",
                                  "Chamber",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Temp.treat",
                                 "Chamber",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/WTC2_ACidata_processed.csv")
remove(data)
remove(fits)
remove(fitted_data)



#WTC3_ACidata_processed
#####
data <- read.csv("WTC3_ACidata_processed.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Treatment",
                                  "Chamber",
                                  "Curve"),
              sep = "_")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Treatment",
                                            "Chamber",
                                            "Curve"),
                 sep = "_")
data <- unite(data, col = "ID", c("Species",
                                  "Treatment",
                                  "Chamber",
                                  "Curve",
                                  "T_measurement"),
              sep = "_")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Treatment",
                                 "Chamber",
                                 "Curve",
                                 "T_measurement"),
                        sep = "_")
write.csv(fitted_data, "./fitted2/WTC3_ACidata_processed.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Onoda_etal
#####
data <- read.csv("Onoda_etal.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "_")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "_")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "_")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "_")
write.csv(fitted_data, "./fitted2/Onoda_etal.csv")
remove(data)
remove(fits)
remove(fitted_data)


#Picea_mariana_way_etal
#####
data <- read.csv("Picea_mariana_way_etal.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "_")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "_")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "_")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "_")
write.csv(fitted_data, "./fitted2/Picea_mariana_way_etal.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Pieca_abies_tarvainen_etal
#####
data <- read.csv("Pieca_abies_tarvainen_etal.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Pieca_abies_tarvainen_etal.csv")
remove(data)
remove(fits)
remove(fitted_data)


#Pinus_densiflora_han_etal
#####
data <- read.csv("Pinus_densiflora_han_etal.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Pinus_densiflora_han_etal.csv")
remove(data)
remove(fits)
remove(fitted_data)


#Pinus_radiata_Walcroft
#####
data <- read.csv("Pinus_radiata_Walcroft.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Patm), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Pinus_radiata_Walcroft.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Pteada_Ellsworth
#####
data <- read.csv("Pteada_Ellsworth.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Patm), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Pteada_Ellsworth.csv")
remove(data)
remove(fits)
remove(fitted_data)


#SIOP Leaf gas exchange Cernusak et al
#####
data <- read.csv("SIOP Leaf gas exchange Cernusak et al.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Pressure..kPa.), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/SIOP Leaf gas exchange Cernusak et al.csv")
remove(data)
remove(fits)
remove(fitted_data)

#SlotWinter2017_NewPhyt.Data
#####
data <- read.csv("SlotWinter2017_NewPhyt.Data.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Rep"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Rep"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Rep",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Rep",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/SlotWinter2017_NewPhyt.Data.csv")
remove(data)
remove(fits)
remove(fitted_data)



#SlotWinterPCE_RawData_DiffusionCorrected
#####
data <- read.csv("SlotWinterPCE_RawData_DiffusionCorrected.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                Patm = mean(data$Press), fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/SlotWinterPCE_RawData_DiffusionCorrected.csv")
remove(data)
remove(fits)
remove(fitted_data)



#SSPRUCE_3_cohort_ACi_data
#####
data <- read.csv("SPRUCE_3_cohort_ACi_data.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/SPRUCE_3_cohort_ACi_data.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Strassemeyer
#####
data <- read.csv("Strassemeyer.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Strassemeyer.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Glycine_max_Harley_f2
#####
data <- read.csv("Glycine_max_Harley_f2.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Glycine_max_Harley_f2.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Great_Aci_data_Processed
#####
data <- read.csv("Great_Aci_data_Processed.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Replicate",
                                  "Curve",
                                  "Povanance"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Replicate",
                                            "Curve",
                                            "Povanance"),
                 sep = "-")
data <- unite(data, col = "ID", c("Replicate",
                                  "Curve",
                                  "Povanance",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Replicate",
                                 "Curve",
                                 "Povanance",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Great_Aci_data_Processed.csv")
remove(data)
remove(fits)
remove(fitted_data)



#Medlyn_etal_all
#####
data <- read.csv("Medlyn_etal_all.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "-")
write.csv(fitted_data, "./fitted2/Medlyn_etal_all.csv")
remove(data)
remove(fits)
remove(fitted_data)





#GHS39_GREAT_MAIN_ACiT_20160216-20160227_L0
#####
data <- read.csv("GHS39_GREAT_MAIN_ACiT_20160216-20160227_L0.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Replicate",
                                  "Curve",
                                  "Provenance"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Replicate",
                                            "Curve",
                                            "Provenance"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Replicate",
                                  "Curve",
                                  "Provenance",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE, Patm = mean(data$Press),
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Replicate",
                                 "Curve",
                                 "Provenance",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/GHS39_GREAT_MAIN_ACiT_20160216-20160227_L0.csv")
remove(data)
remove(fits)
remove(fitted_data)


#Betula_pendula_Pinus_sylvestris_wang_etal
#####
data <- read.csv("Betula_pendula_Pinus_sylvestris_wang_etal.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE, 
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/Betula_pendula_Pinus_sylvestris_wang_etal.csv")
remove(data)
remove(fits)
remove(fitted_data)





#Chamaecyparis obtusa_han_etal
#####
data <- read.csv("Chamaecyparis obtusa_han_etal.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/Chamaecyparis obtusa_han_etal.csv")
remove(data)
remove(fits)
remove(fitted_data)






#Daintree_ACidata_processed
#####
data <- read.csv("Daintree_ACidata_processed.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE, Patm = mean(data$Press),
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/Daintree_ACidata_processed.csv")
remove(data)
remove(fits)
remove(fitted_data)


#DreyerSevenSpp_final
#####
data <- read.csv("DreyerSevenSpp_final.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE, Patm = mean(data$Patm),
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/DreyerSevenSpp_final.csv")
remove(data)
remove(fits)
remove(fitted_data)








#GHS30_Euc2-SxTxCO2xW_GEtempresponse_20090105-20090109_L1
#####
data <- read.csv("GHS30_Euc2-SxTxCO2xW_GEtempresponse_20090105-20090109_L1.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Potnum",
                                  "Temp"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Potnum",
                                            "Temp"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Potnum",
                                  "Temp",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Potnum",
                                 "Temp",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/GHS30_Euc2-SxTxCO2xW_GEtempresponse_20090105-20090109_L1.csv")
remove(data)
remove(fits)
remove(fitted_data)


#AmazonACIdata_f
#####
data <- read.csv("AmazonACIdata_f.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve",
                                            "Season"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "Season",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "Season",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/AmazonACIdata_f.csv")
remove(data)
remove(fits)
remove(fitted_data)


#Angelica_Varhammar_tropical_species_with_gs
#####
data <- read.csv("Angelica_Varhammar_tropical_species_with_gs.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("Species",
                                  "Curve"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("Species",
                                            "Curve"),
                 sep = "_-")
data <- unite(data, col = "ID", c("Species",
                                  "Curve",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("Species",
                                 "Curve",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/Angelica_Varhammar_tropical_species_with_gs.csv")
remove(data)
remove(fits)
remove(fitted_data)


#Arctic_A-Ci_curves_2012-2015_test_file
#####
data <- read.csv("Arctic_A-Ci_curves_2012-2015_test_file.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("USDA_Species_Code",
                                  "Sample_ID",
                                  "Curve"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("USDA_Species_Code",
                                            "Sample_ID",
                                            "Curve"),
                 sep = "_-")
data <- unite(data, col = "ID", c("USDA_Species_Code",
                                  "Sample_ID",
                                  "Curve",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("USDA_Species_Code",
                                 "Sample_ID",
                                 "Curve",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/Arctic_A-Ci_curves_2012-2015_test_file.csv")
remove(data)
remove(fits)
remove(fitted_data)


#B4Warmed_Aci_temp_curves_clean
#####
data <- read.csv("B4Warmed_Aci_temp_curves_clean.csv",
                 stringsAsFactors = FALSE)

data <- unite(data, col = "ID", c("nameSp",
                                  "Curve_ID",
                                  "Warm.trt",
                                  "H2O.trt",
                                  "MeasuringSeason"),
              sep = "_-")
data <- split(data, data$ID)
for(i in 1:length(data)){
  data[[i]]$T_measurement <- mean(data[[i]]$Tleaf)
}
data <- do.call("rbind", data)
data <- separate(data, col = "ID", into = c("nameSp",
                                            "Curve_ID",
                                            "Warm.trt",
                                            "H2O.trt",
                                            "MeasuringSeason"),
                 sep = "_-")
data <- unite(data, col = "ID", c("nameSp",
                                  "Curve_ID",
                                  "Warm.trt",
                                  "H2O.trt",
                                  "MeasuringSeason",
                                  "T_measurement"),
              sep = "_-")

fits <- fitacis(data, group = "ID", fitmethod = "bilinear",
                fitTPU = TRUE,
                Tcorrect = FALSE)

for(i in 1:length(fits)){
  if(is.na(fits[i])){
    fits[i] <- NULL
  }
}
fitted_data <- coef(fits)
fitted_data <- separate(fitted_data, col = "ID",
                        into = c("nameSp",
                                 "Curve_ID",
                                 "Warm.trt",
                                 "H2O.trt",
                                 "MeasuringSeason",
                                 "T_measurement"),
                        sep = "_-")
write.csv(fitted_data, "./fitted2/B4Warmed_Aci_temp_curves_clean.csv")
remove(data)
remove(fits)
remove(fitted_data)

