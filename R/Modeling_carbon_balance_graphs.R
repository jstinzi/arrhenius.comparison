FigData <- read.csv("FigModelingData.csv")
names(FigData)
attach(FigData)
levels(Location)
levels(DOY)
Week1 <- lm(diff_Hdfit200_W1~SD_temp)
plot(Week1)
lm(diff_Hdfit200_W1~SD_temp)
summary(lm(diff_Hdfit200_W1~SD_temp))
plot(diff_Hdfit200_W1~SD_temp, xlab = "Total Daily C Gain Difference",
     ylab = "SD of Temp by Location and DOY", 
     main = "Low Respiration using Hd Fit 200kJ")


W1_Hd200_CGain <- ggplot(data = FigData, aes(x = plant_C_.medlyn_W1_fix, y = plant_C_new_W1_fix)) +
  ggtitle(label = expression("b)"~"Low"~"R"~"With"~"Fixed"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.978097, intercept = 0.001426, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.3,0.3)+
  ylim(-0.3,0.3)+
  labs(x=expression('Old'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W1_Hd200_CGain

W12_Hd200_CGain <- ggplot(data = FigData, aes(x = plant_C_medlyn_W12_fix, y = plant_C_new_W12_fix)) +
  ggtitle(label = expression("d)"~"High"~"R"~"With"~"Fixed"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.979928, intercept = 0.001377, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.3,0.3)+
  ylim(-0.3,0.3)+
  labs(x=expression('Old'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W12_Hd200_CGain

names(FigData)
W1_HdVar_CGain <- ggplot(data = FigData, aes(x = plant_C_.medlyn_W1_var, y = plant_C_.new_W1_var)) +
  ggtitle(label = expression("a)"~"Low"~"R"~"With"~"Variable"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.983546, intercept = 0.001233, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.3,0.3)+
  ylim(-0.3,0.3)+
  labs(x=expression('Old'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W1_HdVar_CGain

W12_HdVar_CGain <- ggplot(data = FigData, aes(x = plant_C_.medlyn_W12_var, y = plant_C_.new_W12_var)) +
  ggtitle(label = expression("c)"~"High"~"R"~"With"~"Variable"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.985405, intercept = 0.001282, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.3,0.3)+
  ylim(-0.3,0.3)+
  labs(x=expression('Old'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W12_HdVar_CGain

library(gridExtra)
jpeg("Figure 6.jpeg", width = 7, height = 7, units = "in", res = 600)
grid.arrange(W1_HdVar_CGain, W1_Hd200_CGain, W12_HdVar_CGain, W12_Hd200_CGain,
             ncol = 2, nrow = 2)
dev.off()

## Daily Photosynthesis Across Models
names(FigData)
W1_Hd200_Photo <- ggplot(data = FigData, aes(x = photo_medlyn_W1_fix, y = photo_new_W1_fix)) +
  ggtitle(label = expression("b)"~"Low"~"R"~"With"~"Fixed"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.978097, intercept = 0.001426, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  xlim(-0.15,0.4)+
  ylim(-0.15,0.4)+
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  labs(x=expression('Old'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W1_Hd200_Photo

W12_Hd200_Photo <- ggplot(data = FigData, aes(x = photo_medlyn_W12_fix, y = photo_new_W12_fix)) +
  ggtitle(label = expression("d)"~"High"~"R"~"With"~"Fixed"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.979928, intercept = 0.001377, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.15,0.4)+
  ylim(-0.15,0.4)+
  labs(x=expression('Old'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W12_Hd200_Photo

names(FigData)
W1_HdVar_Photo <- ggplot(data = FigData, aes(x = photo_medlyn_W1_var, y = photo_new_W1_var)) +
  ggtitle(label = expression("a)"~"Low"~"R"~"With"~"Variable"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.983546, intercept = 0.001233, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.15,0.4)+
  ylim(-0.15,0.4)+
  labs(x=expression('Old'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W1_HdVar_Photo

W12_HdVar_Photo <- ggplot(data = FigData, aes(x = photo_medlyn_W12_var, y = photo_new_W12_var)) +
  ggtitle(label = expression("c)"~"High"~"R"~"With"~"Variable"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.985405, intercept = 0.001282, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.15,0.4)+
  ylim(-0.15,0.4)+
  labs(x=expression('Old'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       y=expression('New'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W12_HdVar_Photo
        
library(gridExtra)
jpeg("Figure 4.jpeg", width = 7, height = 7, units = "in", res = 600)
grid.arrange(W1_HdVar_Photo, W1_Hd200_Photo, W12_HdVar_Photo, W12_Hd200_Photo,
             ncol = 2, nrow = 2)
dev.off()      

## Daily A:R
names(FigData)
W1_Hd200_AR <- ggplot(data = FigData, aes(x = AR_medlyn_W1_fix, y = AR_new_W1_fix)) +
  ggtitle(label = expression("b)"~"Low"~"R"~"With"~"Fixed"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.978097, intercept = 0.001426, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  xlim(-0.5,9.5)+
  ylim(-0.5,9.5)+
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  labs(x=expression('Old'~'Total'~'Daily'~'A/R'),
       y=expression('New'~'Total'~'Daily'~'A/R'))
W1_Hd200_AR

W12_Hd200_AR <- ggplot(data = FigData, aes(x = AR_medlyn_W12_fix, y = AR_new_W12_fix)) +
  ggtitle(label = expression("d)"~"High"~"R"~"With"~"Fixed"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.979928, intercept = 0.001377, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.5,9.5)+
  ylim(-0.5,9.5)+
  labs(x=expression('Old'~'Total'~'Daily'~'A/R'),
       y=expression('New'~'Total'~'Daily'~'A/R'))
W12_Hd200_AR

W1_HdVar_AR <- ggplot(data = FigData, aes(x = AR_medlyn_W1_var, y = AR_new_W1_var)) +
  ggtitle(label = expression("a)"~"Low"~"R"~"With"~"Variable"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.983546, intercept = 0.001233, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.5,9.5)+
  ylim(-0.5,9.5)+
  labs(x=expression('Old'~'Total'~'Daily'~'A/R'),
       y=expression('New'~'Total'~'Daily'~'A/R'))
W1_HdVar_AR

W12_HdVar_AR <- ggplot(data = FigData, aes(x = AR_medlyn_W12_var, y = AR_new_W12_var)) +
  ggtitle(label = expression("c)"~"High"~"R"~"With"~"Variable"~H[d])) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.985405, intercept = 0.001282, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.5,9.5)+
  ylim(-0.5,9.5)+
  labs(x=expression('Old'~'Total'~'Daily'~'A/R'),
       y=expression('New'~'Total'~'Daily'~'A/R'))
W12_HdVar_AR

library(gridExtra)
jpeg("Figure 5.jpeg", width = 7.5, height = 7.5, units = "in", res = 600)
grid.arrange(W1_HdVar_AR, W1_Hd200_AR, W12_HdVar_AR, W12_Hd200_AR,
             ncol = 2, nrow = 2)
dev.off()    
        