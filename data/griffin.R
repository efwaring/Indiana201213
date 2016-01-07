library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)


cgData <- read.csv("griffinLicorTraits.csv")

cgData$lma <- 1/cgData$sla


# to answer question 1 need data from 2012. Using mixed effects model to 
# analyize how physioloigcal traits differed seasonally and what this means
# for both Cs and Pa.  Possibly doing a PCA to compress data like Waring and
# Holaday growth chamber experiment

# data used: ce, amba, vcmax, jmax, NR activity by weight(mgcl2.hr),
# NR activity by chl (nr.hr.chl), protien(ug.gfw_pr), SLA, totN, totC, C13


# constants from Niinemets et al 1998

vcr = 20.5
jmc = 156
cb = 2.15

# convert totN from mass base to area base
cgData$Na = cgData$lma * cgData$N

# get vcmax, jmax, and chl on mass basis

cgData$vcmaxM <- cgData$lma * cgData$vcmax
cgData$jmaxM <- cgData$lma * cgData$jmax
cgData$chlM <- cgData$lma * cgData$chl

# from niiements 1997 "A model separating leaf structural and
# physiological effects on carbon gain along light gradients for the 
# shade-tolerant species Acer saccharum".  Also see 
# Oecologia (2007) 153:501–510 by Feng

cgData$PC <- cgData$vcmaxM/(6.25*vcr*cgData$Na)
cgData$PB <- cgData$jmaxM/(8.06*jmc*cgData$Na)
cgData$PL <- cgData$chlM/(cgData$N*cb)



# will figure out a quicker way to do this

cgData$species <- factor(cgData$spp,
                           labels=c("C. lacustris","C. stricta", 
                                    "P. arundinacea"))

pc.lme <- lme(PC ~ spp+month+place+spp:place+spp:month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pc.lme)

ggplot(data=plants12, aes(month, PC, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts
ggsave("PC.pdf")

pb.lme <- lme(PB ~ spp+month+place+spp:place+spp:month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pb.lme)

ggplot(data=plants12, aes(month, PB, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts
ggsave("PB.pdf")

pl.lme <- lme(PL ~ spp+month+place+spp:place+spp:month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(pl.lme)

ggplot(data=plants12, aes(month, PL, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="species",
                     values = c("black", "gray50"))+
  themeopts

ggsave("PL.pdf")

# reshape proprotion data for figure making.

allP <- cgData %>% gather("Npartition", "percentN", 15:17, na.rm=T) %>%
  select(sample, trt, species, Npartition, percentN)

allPM <- ddply(allP, .( species, Npartition,trt), summarize,
               proportion = mean(percentN),
               proportionSD = sd(percentN))


ggplot(data=allPM, aes(trt, proportion, color=Npartition, shape=Npartition)) +
  geom_pointrange(aes(ymin=proportion-proportionSD,
                      ymax=proportion+proportionSD))+
  scale_color_manual(name="Allocation",
                     values = c("black", "gray50", "blue"),
                     labels=c("Carboxylation", "Bioenergics","Light Harvest"))+
  scale_shape_manual(name="Allocation", values=c(1,2,5),
                     labels=c("Carboxylation", "Bioenergics","Light Harvest"))+
  facet_grid(species~.)+
  labs(y="Proportion N")+
  themeopts

ggsave("PALL_cg.pdf")
