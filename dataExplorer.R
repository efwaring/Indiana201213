# Data exploration for data collected in Marshall County, IN in 2012 and 2013 by
# Lizz Waring and Scott Holaday. 

library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)

soil12 <- read.csv("lachat2012.csv")
soil13 <- read.csv("lachat2013.csv")
plants12 <- read.csv("data2012.csv")
plants13 <- read.csv("data2013.csv")

soil12$year <- 2012
soil13$year <- 2013


soil1 <- soil12 %>% separate(Analyte, into=c("Ammonia","NO3+NO2"))
