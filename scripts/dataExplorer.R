# Data exploration for data collected in Marshall County, IN in 2012 and 2013 by
# Lizz Waring and Scott Holaday. 

# Data being used to answer the following questions
# 1) How do Carex stricta (Cs) and Phalaris arundinacea(Pa) N-use strategies 
# differ seasonally? (2012)
# 2) Do seasonal changes in leaf phys/morph traits relate to soil N?(2012/2013)
# 3) Does a relationship between soil N and leaf N exist? (2013)




# packages needed
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(ade4)


# data import use github folder "data" as working directory
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

amm.aov <- lm(ammonia.kg~month+place+year, data=soil)
summary(amm.aov)
anova(amm.aov)

nit.aov <- lm(no3.no2.kg~month+place+year, data=soil)
summary(nit.aov)
anova(nit.aov)

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

totN.aov <- lm(totN~spp+month+place+year, data=plantSub)
summary(totN.aov)
anova(totN.aov)


totC.aov <- lm(totC~spp+month+place+year, data=plantSub)
summary(totC.aov)
anova(totC.aov)

c13.aov <- lm(C13~spp+month+place+year, data=plantSub)
summary(c13.aov)
anova(c13.aov)

SLA.aov <- lm(SLA~spp+month+place+year, data=plantSub)
summary(SLA.aov)
anova(SLA.aov)

pr.aov <- lm(ug.gfw_pr~spp+month+place+year, data=plantSub)
summary(pr.aov)
anova(pr.aov)

# to answer question 1 need data from 2012. Using mixed effects model to 
# analyize how physioloigcal traits differed seasonally and what this means
# for both Cs and Pa.  Possibly doing a PCA to compress data like Waring and
# Holaday growth chamber experiment

# data used: ce, amba, vcmax, jmax, NR activity by weight(mgcl2.hr),
# NR activity by chl (nr.hr.chl), protien(ug.gfw_pr), SLA, totN, totC, C13

pca <- plants12 %>% select(ce,amba,vcmax,jmax,mgcl2.hr,nr.hr.chl,chl,
                           ug.gfw_pr, 
                           SLA,totN,totC,C13)

pca <- na.omit(pca)

PCA12 <- dudi.pca(pca,scale=T,scannf=F)
sums <- 100 * PCA12$eig/sum(PCA12$eig)
cumsum(sums)
scatter(PCA12)
s.label(PCA12$co,boxes=F)
PCA12$eig #check # of axis with eig>1. Test these.


# constants from Niinemets et al 1998

vcr = 20.5
jmc = 156
cb = 2.15

# convert totN from mass base to area base
plants12$totN2 <- plants$totN/100 # converts cg/g to g/g
plants12$LMA2 <- plants$LMA*10000 # converts g/cm^2 to g/m^2
plants12$Na = plants12$LMA * plants12$totN2

plants12$PC <- plants12$vcmax/(6.25*vcr*plants12$Na)
plants12$PB <- plants12$jmax/(8.06*jmc*plants12$Na)
plants12$PL <- plants12$chl/(plants12$totN*cb)

ggplot(data=plants12, aes(spp, PC)) +
  geom_boxplot()+
  facet_grid(date~rep)


ggplot(data=plants12, aes(spp, PB)) +
  geom_boxplot()+
  facet_grid(date~rep)


ggplot(data=plants12, aes(spp, PL)) +
  geom_boxplot() +
  facet_grid(date~rep)




# to answer question 2, Do seasonal changes in leaf phys/morph traits relate 
# to soil N? Need data from both 2012 and 2013.  Since there was no statistical
# differences between leaf N or C13 or protein in 2012 and 2013 can include 
# both.  Need data on leaf N, C13, leaf C, CN, SLA, and leaf protien as well
# as well soil N

# current plan is a mixed effects model.  Nested by individual.  Time is
# continous. 

#theortical set up:

#lme(var ~ soilN + time + spp + soilN:time + soilN:spp, random =~1|individual,
#   data=data)


# to anser question 3,Does a relationship between soil N and leaf N exist? 
# first, question may need to be rephrased.
# data from 2013 is needed.  Could do simple linear model to examine difference?
# however, from data, might not be a linear fit.  More indepth, does time of 
# year affect relationship between soil and leaf N?  Does it differ between 
# species. Using model selection might be worth while here. 


