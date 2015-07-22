# Data exploration for data collected in Marshall County, IN in 2012 and 2013 by
# Lizz Waring and Scott Holaday. 

# Data being used to answer the following questions
# 1) How do Carex stricta (Cs) and Phalaris arundinacea(Pa) N-use strategies 
# differ seasonally? (2012)
# 2) Do seasonal chainges in leaf phys/morph traits relate to soil N?(2012/2013)
# 3) Does a relationship between soil N and leaf N exist? (2013)

# set working directory
setwd("~/Indiana201213")

# packages needed
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)

# data import
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
# subset of only sites sampled in 2012 and 2013

plantSub <- p2013 %>% filter(place==15|place==16)
plantSub <- rbind(p2012, plantSub)



#  comparasion between year
# do I need a t.test or anova?  Is difference between species important if it 
# is the same species?

totN.aov <- t.test(totN~year, data=plantSub)


totC.aov <- aov(totC~spp+month+place+year, data=plantSub)
summary(totC.aov)

c13.aov <- aov(C13~spp+month+place+year, data=plantSub)
summary(c13.aov)

SLA.aov <- aov(SLA~spp+month+place+year, data=plantSub)
summary(SLA.aov)

pr.aov <- aov(ug.gfw_pr~spp+month+place+year, data=plantSub)
summary(pr.aov)

# to answer question 1 need data from 2012. Using mixed effects model to 
# analyize how physioloigcal traits differed seasonally and what this means
# for both Cs and Pa.  Possibly doing a PCA to compress data like Waring and
# Holaday growth chamber experiment

# data used: ce, amba, vcmax, jmax, NR activity by weight(mgcl2.hr),
# NR activity by chl (nr.hr.chl), protien(ug.gfw_pr), SLA, totN, totC, C13










