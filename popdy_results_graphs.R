# ************************************************
#  PopDY simulation results plots
# ************************************************
setwd("C:/Users/danwa/OneDrive/Research/Lake Sturgeon/Chapter3/LST Data/LST ABundance")
library(ggplot2)

# Functions:
colSD = function(x){
  sds = vector("list", length = 36)
  for(n in 1:ncol(x)){
    sds[n] = sd(x[ , n])
  }
  
  return(sds)
}

colXbar = function(x){
  means = vector("list", length = 36)
  for(n in 1:ncol(x)){
    means[n] = mean(x[ , n])
  }
  
  return(means)
}

# Variable N0, Fixed Z
# import data
sim1 = read.csv("Fixed_Z_Varying_N0.csv")
View(sim1)
sim1 = sim1[ , -1]
# rename columns to year classes
years = 2000:2035
colnames(sim1) <- years

# calculate mean, SD for each year class

means1 = as.numeric(colXbar(sim1))
means1 = as.data.frame(means1)
sum(means1) #25,833 individuals present in 2035
View(means1)

# calc number of individuals >15 y.o. at 2035
nrow(means1)
means.old = means1[1:21, ]
sum(means.old) #1025 individuals >15 y.o. at 2035
View(means.old)

# View(sd1)
sd1 = as.numeric(colSD(sim1))

plot.dat1 = as.data.frame(cbind(means1, sd1))
View(plot.dat1)
plot.dat1$Year.class = years
plot.dat1$Sim = ifelse(plot.dat1$Year.class <= 2015, "Real", "Simulated")

p <- ggplot(plot.dat1, aes(x = Year.class, y = means1, fill = Sim)) + 
  geom_bar(stat = "identity", color = "black") + 
  scale_y_log10(expand = c(0,0)) + 
  scale_x_continuous(breaks = round(seq(min(plot.dat1$Year.class), max(plot.dat1$Year.class), by = 5))) + 
  geom_errorbar(aes(ymin = means1 - sd1, ymax = means1 + sd1), width = 0.1)
p + scale_fill_manual(values = c("gray", "white")) + 
  labs(title = "Fixed N0, Fixed Z", x = "Year Class", y = "Mean Population Estimate (+- 1SD)" ) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.line = element_line(color = "black", size = 0.2)) + 
  theme(panel.background = element_rect(fill = "transparent"))
  

# Fixed N0, Variable Z
sim2 = read.csv("Fixed_N0_Varying_Z.csv")
View(sim2)
sim2 = sim2[ , -1]
# rename columns to year classes
years = 2000:2035
colnames(sim2) <- years

# calculate mean, SD for each year class

means2 = as.integer(colXbar(sim2))
# View(means)
sd2 = as.numeric(colSD(sim2))
# View(sd1)

plot.dat2 = as.data.frame(cbind(means2, sd2))
plot.dat2$Year.class = years
plot.dat2$Sim = ifelse(plot.dat2$Year.class <= 2015, "Real", "Simulated")
View(plot.dat2)
plot.dat2 = plot.dat2[which(plot.dat2$means2 > 0), ]

p <- ggplot(plot.dat2, aes(x = Year.class, y = means2, fill = Sim)) + 
  geom_bar(stat = "identity", color = "black") + 
  scale_y_log10(expand = c(0,0)) + 
  scale_x_continuous(breaks = round(seq(min(plot.dat2$Year.class), max(plot.dat2$Year.class), by = 1))) + 
  geom_errorbar(aes(ymin = means2 - sd2, ymax = means2 + sd2), width = 0.1)
p + scale_fill_manual(values = "white") + 
  labs(title = "Fixed N0, Variable Z", x = "Year Class", y = "Mean Population Estimate (+- 1SD)" ) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.line = element_line(color = "black", size = 0.2)) + 
  theme(panel.background = element_rect(fill = "transparent"))

# Doubled stocking numbers
# Variable N0, Fixed Z
# import data
sim3 = read.csv("Fixed_Z_Varying_N0_2xReintro.csv")
View(sim3)
sim3 = sim3[ , -1]
# rename columns to year classes
years = 2000:2035
colnames(sim3) <- years

# calculate mean, SD for each year class

means3 = as.numeric(colXbar(sim3))
means3 = as.data.frame(means3)
sum(means3) #42,222 individuals present in 2035

means3.old = means3[1:21, ]
sum(means3.old)

# View(means)  
sd3 = as.numeric(colSD(sim3))
# View(sd1)

plot.dat3 = as.data.frame(cbind(means3, sd3))
View(plot.dat3)
plot.dat3$Year.class = years
plot.dat3$Sim = ifelse(plot.dat3$Year.class <= 2015, "Real", "Simulated")

p <- ggplot(plot.dat3, aes(x = Year.class, y = means3, fill = Sim)) + 
  geom_bar(stat = "identity", color = "black") + 
  scale_y_log10(expand = c(0,0)) + 
  scale_x_continuous(breaks = round(seq(min(plot.dat3$Year.class), max(plot.dat3$Year.class), by = 5))) + 
  geom_errorbar(aes(ymin = means3 - sd3, ymax = means3 + sd3), width = 0.1)
p + scale_fill_manual(values = c("gray", "white")) + 
  labs(title = "Fixed Z, 2x Reintroduction", x = "Year Class", y = "Mean Population Estimate (+- 1SD)" ) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.line = element_line(color = "black", size = 0.2)) + 
  theme(panel.background = element_rect(fill = "transparent"))










