FigData <- read.csv("FigModelingData2.csv")
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

library(ggplot2)
W1_Hd200_CGain <- ggplot(data = FigData, aes(x = week1_C_new, y = week1_C_med)) +
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
W1_HdVar_CGain <- ggplot(data = FigData, aes(x = week1_C_new, y = week1_C_med)) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.836, intercept = 0.012, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  xlim(-0.5,0.5)+
  ylim(-0.5,0.5)+
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  labs(y=expression('Old'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       x=expression('New'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W1_HdVar_CGain

W12_HdVar_CGain <- ggplot(data = FigData, aes(x = week12_C_new, y = week12_C_med)) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.859, intercept = 0.011, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  xlim(-0.5,0.5)+
  ylim(-0.5,0.5)+
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  labs(y=expression('Old'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       x=expression('New'~'Total'~'C'~'Gain'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W12_HdVar_CGain

library(gridExtra)
jpeg("Figure 6.jpeg", width = 7, height = 7, units = "in", res = 600)
grid.arrange(W1_HdVar_CGain, W1_Hd200_CGain, W12_HdVar_CGain, W12_Hd200_CGain,
             ncol = 2, nrow = 2)
dev.off()

## Daily Photosynthesis Across Models

names(FigData)
W1_HdVar_Photo <- ggplot(data = FigData, aes(x = week1_photo_new, y = week1_photo_med))+
  ggtitle(label = expression("Low R"))+
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.819, intercept = 0.018, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-0.2,0.7)+
  ylim(-0.2,0.7)+
  labs(y=expression('Old'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       x=expression('New'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W1_HdVar_Photo

W12_HdVar_Photo <- ggplot(data = FigData, aes(x = week12_photo_new, y = week12_photo_med)) +
  ggtitle(label = expression("High R")) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.819, intercept = 0.031, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid = element_blank())+
  xlim(-0.2,0.7)+
  ylim(-0.2,0.7)+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  labs(y=expression('Old'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"),
       x=expression('New'~'Total'~'Daily'~'A'~"(gC"~"plant"^{-1}~"day"^{-1}*")"))
W12_HdVar_Photo

## Daily A:R

W1_HdVar_AR <- ggplot(data = FigData, aes(x = week1_AR_new, y = week1_AR_med)) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.819, intercept = 0.018, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  xlim(-5,20)+
  ylim(-5,20)+
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  labs(y=expression('Old'~'Total'~'Daily'~'A/R'),
       x=expression('New'~'Total'~'Daily'~'A/R'))
W1_HdVar_AR

W12_HdVar_AR <- ggplot(data = FigData, aes(x = week12_AR_new, y = week12_AR_med)) +
  geom_point(size = 3, alpha = 0.7)+
  geom_abline(slope = 1, intercept = 0, size = 1, colour = "Black") +
  geom_abline(slope = 0.985405, intercept = 0.001282, size = 2, linetype = "dashed", colour = "Gray45")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(text = element_text(size = 12, colour = "Black"),
        axis.line=element_line(size=1), axis.ticks=element_line(size=1.5),
        axis.ticks.length=unit(1.5, "mm"), rect=element_rect(size=2))+
  xlim(-5,20)+
  ylim(-5,20)+
  labs(y=expression('Old'~'Total'~'Daily'~'A/R'),
       x=expression('New'~'Total'~'Daily'~'A/R'))
W12_HdVar_AR

library(gridExtra)
jpeg("Figure 4.jpeg", width = 12, height = 18, units = "in", res = 600)
grid.arrange(W1_HdVar_Photo, W12_HdVar_Photo, W1_HdVar_AR, W12_HdVar_AR,
             W1_HdVar_CGain, W12_HdVar_CGain,
             ncol = 2, nrow = 3)
dev.off()    

A <- W1_HdVar_Photo
B <- W12_HdVar_Photo
C <- W1_HdVar_AR
D <- W12_HdVar_AR
E <- W1_HdVar_CGain
F <- W1_HdVar_CGain

gA <- ggplotGrob(A)  
gB <- ggplotGrob(B)
gC <- ggplotGrob(C)
gD <- ggplotGrob(D)
gE <- ggplotGrob(E)
gF <- ggplotGrob(F)

# Arrange the four charts
grid.arrange(gA, gB, gC, gD, gE, gF, ncol=2, nrow=3)

# Combine the plots   
g = cbind(rbind(gA, gC, gE, size = "last"), rbind(gB, gD, gF, size = "last"), size = "first")

library(grid)

# draw it
jpeg("Figure 4.jpeg", width = 20, height = 25, units = "cm", res = 600)
grid.newpage()
grid.draw(g)
dev.off()
        