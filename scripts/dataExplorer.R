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

# for making figures
source("theme-opts.R")

# data import use github folder "data" as working directory

soil12 <- read.csv("lachat2012.csv")
soil13 <- read.csv("lachat2013.csv")
plants12 <- read.csv("data2012.csv")
plants13 <- read.csv("data2013.csv")



# filter 2013 site that were sampled in 2012


soil12$year <- 2012
soil13$year <- 2013

sub13 <- soil13 %>% filter(place==15|place==16)
sub12 <- soil12 %>% group_by(site, rep, month, place,year) %>%
        summarise(ammonia.g=mean(ammonia.g),
                              ammonia.kg=mean(ammonia.kg),
                              no3.no2.g=mean(no3.no2.g),
                              no3.no2.kg=mean(no3.no2.kg))




soil <- bind_rows(sub13, sub12)
soil$yearf <- factor(soil$year,
                        labels=c("2012", "2013"))
soil$placef <- factor(soil$place,
                         labels = 1:2)


# keep in soil data from other sites besides 82a1 and 82a2
allSoil <- bind_rows(sub12,soil13)
allSoil$placef <- factor(allSoil$place,
                        labels = 1:2)
allSoil$yearf <- factor(allSoil$year,
                       labels=c("2012", "2013"))
# comparasion between year

amm.aov <- lm(ammonia.kg~month+place+year+month:year, data=soil)
summary(amm.aov)
anova(amm.aov)

ggplot(soil, aes(month, ammonia.kg, shape=placef, color=yearf)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_color_manual(name="Year",
                     values = c("black", "gray50")) +
  themeopts

ggsave("ammoniaYear.pdf")

nit.aov <- lm(no3.no2.kg~month+place+year+month:year, data=soil)
summary(nit.aov)
anova(nit.aov)

ggplot(soil, aes(month, no3.no2.kg, shape=placef, color=yearf)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_color_manual(name="Year",
                     values = c("black", "gray50")) +
  themeopts

ggsave("NO2+3Year.pdf")

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
plantSub$placef <- factor(plantSub$place,
                        labels = 1:2)
plantSub$yearf <- factor(plantSub$year,
                       labels=c("2012", "2013"))
plantSub$species <- factor(plantSub$spp,
                         labels=c("C. stricta", "P. arundinacea"))



#  comparasion between year

totN.aov <- lm(totN~spp+month+place+year+month:year, data=plantSub)
summary(totN.aov)
anova(totN.aov)

ggplot(plantSub, aes(month, totN, shape=placef, color=yearf)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_color_manual(name="Year",
                     values = c("black", "gray50")) +
  facet_grid(species~.)+
  themeopts
ggsave("TotalNYear.pdf")


totC.aov <- lm(totC~spp+month+place+year+month:year, data=plantSub)
summary(totC.aov)
anova(totC.aov)

ggplot(plantSub, aes(month, totC, shape=placef, color=yearf)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_color_manual(name="Year",
                     values = c("black", "gray50")) +
  facet_grid(species~.)+
  themeopts
ggsave("TotalCYear.pdf")

c13.aov <- lm(C13~spp+month+place+year+month:year, data=plantSub)
summary(c13.aov)
anova(c13.aov)

ggplot(plantSub, aes(month, C13, shape=placef, color=yearf)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_color_manual(name="Year",
                     values = c("black", "gray50")) +
  facet_grid(species~.)+
  themeopts
ggsave("C13Year.pdf")

SLA.aov <- lm(SLA~spp+month+place+year+month:year, data=plantSub)
summary(SLA.aov)
anova(SLA.aov)

ggplot(plantSub, aes(month, SLA, shape=placef, color=yearf)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_color_manual(name="Year",
                     values = c("black", "gray50")) +
  facet_grid(species~.)+
  themeopts

ggsave("SLAYear.pdf")

pr.aov <- lm(ug.gfw_pr~spp+month+place+year+month:year, data=plantSub)
summary(pr.aov)
anova(pr.aov)

ggplot(plantSub, aes(month, ug.gfw_pr, shape=placef, color=yearf)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_color_manual(name="Year",
                     values = c("black", "gray50")) +
  facet_grid(species~.)+
  themeopts

ggsave("proteinYear.pdf")

# to answer question 1 need data from 2012. Using mixed effects model to 
# analyize how physioloigcal traits differed seasonally and what this means
# for both Cs and Pa.  Possibly doing a PCA to compress data like Waring and
# Holaday growth chamber experiment

# data used: ce, amba, vcmax, jmax, NR activity by weight(mgcl2.hr),
# NR activity by chl (nr.hr.chl), protien(ug.gfw_pr), SLA, totN, totC, C13

pca <- plants12 %>% select(ce,amba,vcmax,jmax,mgcl2.hr,nr.hr.chl,chl,
                           ug.gfw_pr, 
                           SLA,totN,totC,C13)

pca <- cor(pca)

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
plants12$Na = plants12$LMA * plants12$totN

# get vcmax, jmax, and chl on mass basis

plants12$vcmaxM <- plants12$LMA * plants12$vcmax
plants12$jmaxM <- plants12$LMA * plants12$jmax
plants12$chlM <- plants12$LMA * plants12$chl

# from niiements 1997 "A model separating leaf structural and
# physiological effects on carbon gain along light gradients for the 
# shade-tolerant species Acer saccharum".  Also see 
# Oecologia (2007) 153:501–510 by Feng

plants12$PC <- plants12$vcmaxM/(6.25*vcr*plants12$Na)
plants12$PB <- plants12$jmaxM/(8.06*jmc*plants12$Na)
plants12$PL <- plants12$chlM/(plants12$totN*cb)


# will figure out a quicker way to do this
plants12$placef <- factor(plants12$place,
                          labels = 1:2)
plants12$species <- factor(plants12$spp,
                           labels=c("C. stricta", "P. arundinacea"))

ggplot(data=plants12, aes(month, PC, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts
ggsave("PC.pdf")


ggplot(data=plants12, aes(month, PB, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts
ggsave("PB.pdf")


ggplot(data=plants12, aes(month, PL, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts

ggsave("PL.pdf")

# reshape proprotion data for figure making.

allP <- plants12 %>% gather("Npartition", "percentN", 32:34, na.rm=T) %>%
  select(indi, place, month, species, Npartition, percentN)

allPM <- ddply(allP, .(month, species, variable), summarize,
               proportion = mean(value),
               proportionSD = sd(value))

ggplot(data=allPM, aes(month, proportion, color=variable, shape=variable)) +
  geom_pointrange(aes(ymin=proportion-proportionSD,
                      ymax=proportion+proportionSD))+
  scale_color_manual(name="N placement",
                     values = c("black", "gray50", "blue"))+
  facet_grid(.~species)+
  themeopts

ggsave("PALL.pdf")


# stats for questions one

seasonChange <- function(variable) {
  variable.aov <- nlme(variable ~ species+month+place+species:place+species:month, 
                       random=~1|indi, data=p2012)
  return(anova(varible.aov))
}

seasonChange(plants12$tot)
# to answer question 2, Do seasonal changes in leaf phys/morph traits relate 
# to soil N? Need data from both 2012 and 2013.  Since there was no statistical
# differences between leaf N or C13 or protein in 2012 and 2013 can include 
# both.  Need data on leaf N, C13, leaf C, CN, SLA, and leaf protien as well
# as well soil N

# current plan is a mixed effects model.  Nested by individual.  Time is
# continous. 

#theortical set up:



all <- merge(allSoil, plants, by=c("place","month","rep", "year"))
all$soilN <- all$ammonia.kg + all$no3.no2.kg
all$individual <- all$rep + all$place
all$cn <- all$totC/all$totN

n.lme <- lme(totN ~ soilN + month + spp + soilN:month + soilN:spp, random =~1|place,
    data=all, na.action=na.omit)

anova(n.lme)

ggplot(all, aes(soilN, totN, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)+ 
  geom_smooth(method="lm")

sla.lme <- lme(SLA ~ soilN + month + spp + soilN:month + soilN:spp, random =~1|place,
             data=all, na.action=na.omit)

anova(sla.lme)

ggplot(all, aes(soilN, SLA, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)

c13.lme <- lme(C13 ~ soilN + month + spp + soilN:month + soilN:spp, random =~1|place,
               data=all, na.action=na.omit)

anova(c13.lme)

ggplot(all, aes(soilN, C13, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)

cn.lme <- lme(cn ~ soilN + month + spp + soilN:month + soilN:spp, random =~1|place,
               data=all, na.action=na.omit)

anova(cn.lme)

ggplot(all, aes(soilN, cn, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)

pr.lme <- lme(ug.gfw_pr ~ soilN + month + spp + soilN:month + soilN:spp, random =~1|place,
               data=all, na.action=na.omit)

anova(pr.lme)

ggplot(all, aes(soilN, ug.gfw_pr, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)

# to anser question 3,Does a relationship between soil N and leaf N exist? 
# first, question may need to be rephrased.
# data from 2013 is needed.  Could do simple linear model to examine difference?
# however, from data, might not be a linear fit.  More indepth, does time of 
# year affect relationship between soil and leaf N?  Does it differ between 
# species. Using model selection might be worth while here. 


