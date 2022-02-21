# Deliverable 1:
library(dplyr)

library(tidyverse)
mecha_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)) 

# Deliverable 2:
suspension <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

total_summary <- suspension %>% summarize(Mean=mean(PSI),
                                          Median=median(PSI),
                                          Var=var(PSI),
                                          Std_Dev=sd(PSI),
                                          .groups = 'keep') 

lot_summary <- suspension  %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),
                                                                         Median=median(PSI),
                                                                         Var=var(PSI),
                                                                         Std_Dev=sd(PSI),
                                                                         .groups = 'keep')  

# Deliverable 3:
t.test(suspension$PSI,mu=1500)

lot1 <- subset(suspension, Manufacturing_Lot=="Lot1")
lot2 <- subset(suspension, Manufacturing_Lot=="Lot2")
lot3 <- subset(suspension, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)
