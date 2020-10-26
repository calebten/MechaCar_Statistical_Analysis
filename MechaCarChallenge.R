# Deliverable 1
setwd("~/Desktop/Bootcamp/R_Analysis/01_Demo")
library(dplyr)
library(tidyverse)
multiple_reg_model <- read.csv(file='MechaCar_mpg.csv', check.names = F)
View(multiple_reg_model)
filtered_reg_model <- multiple_reg_model 
View(filtered_reg_model)
filtered_reg_model$AWD <- factor(filtered_reg_model$AWD) 
View(filtered_reg_model)
aov(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
      +         ground_clearance + AWD, data = filtered_reg_model) 
summary(aov(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
              ground_clearance + AWD, data = filtered_reg_model)) 
lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=filtered_reg_model) 
summary(lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=filtered_reg_model)) 
subset_model <- filtered_reg_model[,c("vehicle_length", "ground_clearance", "mpg")] 
lm(mpg~vehicle_length + ground_clearance, data=subset_model) 
summary(lm(mpg~vehicle_length + ground_clearance, data=subset_model)) 

# Deliverable 2
setwd("~/Desktop/Bootcamp/R_Analysis/01_Demo")
library(dplyr)
library(tidyverse)
suspension_data <- read.csv(file='Suspension_Coil.csv', check.names = F)
summarize_suspension <- suspension_data %>% summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
summarize_suspension_bylot <- suspension_data %>% group_by(Manufacturing_Lot) %>% summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot3subset <-subset(suspension_data, Manufacturing_Lot == "Lot3")
ggplot(suspension_data,aes(x=PSI)) + geom_density() # visualize the PSI data
ggplot(data=lot3subset, aes(x=PSI)) + geom_density(alpha= .2, fill="#FF6655") + geom_vline(aes(xintercept= mean(PSI, na.rm= T)), colour = "blue", linetype= "longdash", size= .8) + geom_vline(aes(xintercept= (mean(PSI, na.rm= T) - var(PSI, na.rm= T))), colour = "red", linetype= "longdash", size= .8) + geom_vline(aes(xintercept= (mean(PSI, na.rm= T) + var(PSI, na.rm= T))), colour = "red", linetype= "longdash", size= .8)

# Deliverable 3
t.test(suspension_data$PSI, mu=1500)
lot1subset <-subset(suspension_data, Manufacturing_Lot == "Lot1")
t.test(lot1subset$PSI, mu=1500)
lot2subset <-subset(suspension_data, Manufacturing_Lot == "Lot2")
t.test(lot2subset$PSI, mu=1500)
lot3subset <-subset(suspension_data, Manufacturing_Lot == "Lot3")
t.test(lot3subset$PSI, mu=1500)
