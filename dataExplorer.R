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



# filter 2013 site that were sampled in 2012
sub13 <- soil13 %>% filter(place==15|place==16)

sub12 <- soil12 %>% group_by(site, rep, month, place) %>%
        summarise(ammonia.g=mean(ammonia.g),
                              ammonia.kg=mean(ammonia.kg),
                              no3.no2.g=mean(no3.no2.g),
                              no3.no2.kg=mean(no3.no2.kg))

sub12$year <- 2012
sub13$year <- 2013

soil <- bind_rows(sub13, sub12)

# comparasion between year

amm.aov <- aov(ammonia.kg~month+place+year, data=soil)
summary(amm.aov)

nit.aov <- aov(no3.no2.kg~month+place+year, data=soil)
summary(nit.aov)

# grouping similar measurments from 2012 and 2013
# get only Cs and Pa in plants 13
plants13 <- plants13 %>% filter(spp=="Cs"|spp=="Pa")

p2013 <- plants13 %>% group_by(site, rep, month, place, spp) %>%
  select(totC,C13,totN,SLA,LMA,ug.gfw_pr)
p2013$year <- "2013"

p2012 <- plants12 %>% group_by(site, rep, month, place, spp) %>%
  select(totC,C13,totN,SLA,LMA,ug.gfw_pr)
p2012$year <- "2012"

plants <- rbind(p2012, p2013)

plants$placef <- factor(plants$place,
                               labels = 1:22)
plants$yearf <- factor(plants$year,
                        labels=c("2012", "2013"))
plants$species <- factor(plants$spp,
                          labels=c("C. stricta", "P. arundinacea"))


#  comparasion between year

totN.aov <- aov(totN~spp+month+place+year, data=plants)
summary(totN.aov)

totC.aov <- aov(totC~spp+month+place+year, data=plants)
summary(totC.aov)

c13.aov <- aov(C13~spp+month+place+year, data=plants)
summary(c13.aov)

SLA.aov <- aov(SLA~spp+month+place+year, data=plants)
summary(SLA.aov)

pr.aov <- aov(ug.gfw_pr~spp+month+place+year, data=plants)
summary(pr.aov)






