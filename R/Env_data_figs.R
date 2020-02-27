ABQ2 <- read.csv(system.file("extdata", "env_data_ABQ_2.csv", 
                             package = "arrhenius.comparison"),
                 stringsAsFactors = FALSE)
ABQ2 <- read.csv("env_data_ABQ_2.csv")
LDN <- read.csv("env_data_London.csv")

names(ABQ2)
ABQ2$Month_f = factor(ABQ2$Month, levels=c('May','August','October'))

## Diff Environmental Light Intensity
ABQ_light <- ggplot(data = ABQ2, aes(x = Hour, y = Qin))+
  ggtitle(label = expression("e)"~"Environmental"~"Light"~"Intensity"~"in"~"ABQ")) +
  geom_line(mapping = NULL, data = NULL, stat = "identity",
            position = "identity", colour = "Gray45", size = 1,
            lineend = "butt", linejoin = "round",
            linemitre = 10, arrow = NULL, na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE)+
  geom_point(size = 2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x = expression("Hourly Time Points"),
       y = expression("Q"[In]~"("~mu*mol~m^{-2}~s^{-1}*")"))+ 
  ylim(0,2850)+
  facet_grid(~ Month_f)
ABQ_light

LDN <- read.csv(system.file("extdata", "env_data_London.csv", 
                            package = "arrhenius.comparison"),
                stringsAsFactors = FALSE)
LDN$Month_f = factor(LDN$Month, levels=c('May','August','October'))
LDN_light <- ggplot(data = LDN, aes(x = Hour, y = Qin))+
  ggtitle(label = expression("f)"~"Environmental"~"Light"~"Intensity"~"in"~"LDN")) +
  geom_line(mapping = NULL, data = NULL, stat = "identity",
            position = "identity", colour = "Gray45", size = 1,
            lineend = "butt", linejoin = "round",
            linemitre = 10, arrow = NULL, na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE)+
  geom_point(size = 2)+
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(x = expression("Hourly Time Points"),
       y = expression("Q"[In]~"("~mu*mol~m^{-2}~s^{-1}*")"))+ 
  ylim(0,2850)+
  facet_grid(~ Month_f)
LDN_light


## Different Environmental Temp
ABQ_temp <- ggplot(data = ABQ2, aes(x = Hour, y = Temp_C))+
  ggtitle(label = expression("a)"~"Environmental"~"Temperature"~"in"~"ABQ")) +
  geom_line(mapping = NULL, data = NULL, stat = "identity",
            position = "identity", colour = "Gray45", size = 1,
            lineend = "butt", linejoin = "round",
            linemitre = 10, arrow = NULL, na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE)+
  geom_point(size = 2)+
  theme_bw()+
  ylim(-3,46)+
  theme(panel.grid = element_blank())+
  labs(x = expression("Hourly Time Points"),
       y = expression('Temperature'~'('*degree*C*')'))+ 
  facet_grid(~ Month_f)
ABQ_temp

LDN_temp <- ggplot(data = LDN, aes(x = Hour, y = Temp_C))+
  ggtitle(label = expression("b)"~"Environmental"~"Temperature"~"in"~"LDN")) +
  geom_line(mapping = NULL, data = NULL, stat = "identity",
            position = "identity", colour = "Gray45", size = 1,
            lineend = "butt", linejoin = "round",
            linemitre = 10, arrow = NULL, na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE)+
  geom_point(size = 2)+
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(x = expression("Hourly Time Points"),
       y = expression('Temperature'~'('*degree*C*')'))+  
  ylim(-3,46)+
  facet_grid(~ Month_f)
LDN_temp

## Environmental VPD
ABQ_vpd <- ggplot(data = ABQ2, aes(x = Hour, y = VPD))+
  ggtitle(label = expression("c)"~"Environmental"~"VPD"~"in"~"ABQ")) +
  geom_line(mapping = NULL, data = NULL, stat = "identity",
            position = "identity", colour = "Gray45", size = 1,
            lineend = "butt", linejoin = "round",
            linemitre = 10, arrow = NULL, na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE)+
  geom_point(size = 2)+
  theme_bw()+
  ylim(0,10)+
  theme(panel.grid = element_blank())+
  labs(x = expression("Hourly Time Points"),
       y = expression("Vapour Pressure Deficit (kPa)"))+ 
  facet_grid(~ Month_f)
ABQ_vpd

LDN_vpd <- ggplot(data = LDN, aes(x = Hour, y = VPD))+
  ggtitle(label = expression("d)"~"Environmental"~"VPD"~"in"~"LDN")) +
  geom_line(mapping = NULL, data = NULL, stat = "identity",
            position = "identity", colour = "Gray45", size = 1,
            lineend = "butt", linejoin = "round",
            linemitre = 10, arrow = NULL, na.rm = FALSE, show.legend = NA,
            inherit.aes = TRUE)+
  geom_point(size = 2)+
  theme_bw() +
  ylim(0,10)+
  theme(panel.grid = element_blank())+
  labs(x = expression("Hourly Time Points"),
       y = expression("Vapour Pressure Deficit (kPa)"))+ 
  facet_grid(~ Month_f)
LDN_vpd

library(gridExtra)
jpeg("Figure 1.jpeg", width = 10, height = 10, units = "in", res = 600)
grid.arrange(ABQ_temp, LDN_temp, ABQ_vpd, LDN_vpd, ABQ_light, LDN_light,
             ncol = 2, nrow = 3)
dev.off()    
