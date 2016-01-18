# Data exploration for data collected in Marshall County, IN in 2012 and 2013 by
# Lizz Waring and Scott Holaday. 

# Data being used to answer the following questions
# 1) How do Carex stricta (Cs) and Phalaris arundinacea(Pa) N-use strategies 
# differ seasonally? (2012)
# 2) Do seasonal changes in leaf phys/morph traits relate to soil N?(2012/2013)
# 3) model fits for influence on leafphys/morph traits? 

# factor anaylis for q2? library(psych)


# packages needed
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(AICcmodavg)


# for making figures
source("theme-opts.R")

# data import use github folder "data" as working directory

soil12 <- read.csv("lachat2012.csv")
soil13 <- read.csv("lachat2013.csv")
soilPRS <- read.csv("PRS_Soil.csv")

plants12 <- read.csv("data2012.csv")
plants13 <- read.csv("data2013.csv")



# filter 2013 site that were sampled in 2012                                   


soil12$year <- 2012
soil13$year <- 2013

sub13 <- soil13 %>% filter(place==15|place==16)
sub12 <- soil12 %>% group_by(site, rep, month, place,year) %>%
        summarise(ammonia.kg=mean(ammonia.kg),
                              no3.no2.kg=mean(no3.no2.kg))




soil <- bind_rows(sub13, sub12)
soil$yearf <- factor(soil$year,
                        labels=c("2012", "2013"))
soil$placef <- factor(soil$place,
                         labels = 1:2)
allsoilD <- ddply(soil, .(place, year, month), summarize,
                  ammonia.kg_sd = sd(ammonia.kg),
                  ammonia.kg=mean(ammonia.kg),
                  no3.no2.kg_sd = sd(no3.no2.kg),
                  no3.no2.kg=mean(no3.no2.kg))
write.csv(allsoilD, "allsoil.csv")


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



nit.aov <- lm(no3.no2.kg~month+place+year+month:year, data=soil)
summary(nit.aov)
anova(nit.aov)

# PRS probes

prs.aov <- lm(Total.N~date+site+year+date:year, data=soilPRS)
summary(prs.aov)
anova(prs.aov)

prsAVG <- ddply(soilPRS, .(site, year, date), summarize,
                Total.N.sd = sd(Total.N),
                Total.N = mean(Total.N),
                NO3_sd =sd (NO3),
                NO3=mean(NO3),
                NH4_sd=sd(NH4),
                NH4=mean(NH4))

write.csv(prsAVG, "prsAVG.csv")


# grouping similar measurments from 2012 and 2013
# get only Cs and Pa in plants 13
plants13 <- plants13 %>% filter(spp=="Cs"|spp=="Pa")

p2013 <- plants13 %>% group_by(site, rep, month, place, spp) %>%
  select(totC,C13,totN,SLA,LMA,ug.gfw_pr)
p2013$year <- "2013"
p2013$indi <- length(p2013$site)

p2012 <- plants12 %>% group_by(indi,site, rep, month, place, spp) %>%
  select(totC,C13,totN,SLA,LMA,ug.gfw_pr)
p2012$year <- "2012"

plants <- rbind(p2012, p2013)
plants$indi <- NULL


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
plantSub$LMA <- as.numeric(plantSub$LMA)
plantSub$pro_area <- plantSub$ug.gfw_pr * plantSub$LMA
plantSub$indi <- NULL


plantSub2 <- na.omit(plantSub)

plantSub_dd <- ddply(plantSub2, .(month, spp, site, rep, yearf), summarize,
                     totC=mean(totC),
                     C13=mean(C13),
                     totN=mean(totN),
                     pro_area=mean(pro_area),
                     SLA=mean(SLA))
write.csv(plantSub_dd, "AVgplantsbetweenyears.csv")
#  comparasion between year

totN.aov <- lm(totN~spp+month+place+year+month:year, data=plantSub)
summary(totN.aov)
anova(totN.aov)

totC.aov <- lm(totC~spp+month+place+year+month:year, data=plantSub)
summary(totC.aov)
anova(totC.aov)

c13.aov <- lm(C13~spp+month+place+year+month:year, data=plantSub)
summary(c13.aov)
anova(c13.aov)


SLA.aov <- lm(SLA~spp+month+place+year+month:year, data=plantSub)
summary(SLA.aov)
anova(SLA.aov)

pr.aov <- lm(pre_area~spp+month+place+year+month:year, data=plantSub)
summary(pr.aov)
anova(pr.aov)


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

allP <- plants12 %>% gather("Npartition", "percentN", 32:34, na.rm=T) %>%
  select(indi, place, month, species, Npartition, percentN)

allPM <- ddply(allP, .(month, species, Npartition,place), summarize,
               proportion = mean(percentN),
               proportionSD = sd(percentN))

allPM$site <- factor(allPM$place, labels=c("Site 1","Site 2"))

ggplot(data=allPM, aes(month, proportion, color=Npartition, shape=Npartition)) +
  geom_pointrange(aes(ymin=proportion-proportionSD,
                      ymax=proportion+proportionSD), size=0.75)+
  scale_color_manual(name="Allocation Location",
                     values = c("black", "gray50", "blue"),
                     labels=c("Carboxylation", "Bioenergics","Light Harvest"))+
  scale_shape_manual(name="Allocation Location", values=c(1,2,5),
                     labels=c("Carboxylation", "Bioenergics","Light Harvest"))+
  facet_grid(species~site)+
  labs(y="Proportion of Allocated N", x="Month")+
  themeopts+
  theme(strip.text.y=element_text(face="italic"))

ggsave("PALL_sites_final.pdf")

# 2012 only figures

plants12$placef <- factor(plants12$place,
                          labels = 1:2)
plants12$species <- factor(plants12$spp,
                           labels=c("C. stricta", "P. arundinacea"))
plants12$LMA <- as.numeric(plants12$LMA)
plants12$pro_area <- plants12$ug.gfw_pr * plants12$LMA
plants12$site <- factor(plants12$place, labels=c("Site 1","Site 2"))

ggplot(plants12, aes(month, PNUE, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_shape_manual(name="Species", values=c(1,2))+
  facet_grid(.~site)+
  scale_y_continuous(expression(
    PNUE[] ~ ( mu * mol %.% g^{-1} %.% s^{-1} ) ) )+
  labs(x="Month") +
  themeopts+
  theme(legend.text=element_text(face="italic"))

ggsave("pnue.pdf")


ggplot(plants12, aes(month, vcmax, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_shape_manual(name="Species", values=c(1,2))+
  facet_grid(.~site)+
  scale_y_continuous(expression(
    V[cmax] ~ ( mu * mol %.% m^{-2} %.% s^{-1} ) ) )+
  labs(x="Month") +
  themeopts+
  theme(legend.text=element_text(face="italic"))

ggsave("vcmax.pdf")
vcmax.lme <- lme(vcmax ~ spp+month+place+spp:place+spp:month, 
              random=~1|indi, data=plants12, na.action=na.omit)
anova(vcmax.lme)


ggplot(plants12, aes(month, jmax, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_shape_manual(name="Species", values=c(1,2))+
  facet_grid(.~site)+
  scale_y_continuous(expression(
    J[max] ~ ( mu * mol %.% m^{-2} %.% s^{-1} ) ) )+
  labs(x="Month") +
  themeopts+
  theme(legend.text=element_text(face="italic"))
ggsave("Jmax.pdf")
jmax.lme <- lme(jmax ~ spp+month+place+spp:place+spp:month, 
                 random=~1|indi, data=plants12, na.action=na.omit)
anova(jmax.lme)


ggplot(plants12, aes(month, chl, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_shape_manual(name="Species", values=c(1,2))+
  facet_grid(.~site)+
  scale_y_continuous(expression(Chlorophyll ~ ( mu * g %.% gfw^{-1}) ) )  +
  labs(x="Month")+
  themeopts+
  theme(legend.text=element_text(face="italic"))
ggsave("chl.pdf")

chl.lme <- lme(chl ~ spp+month+place+spp:place+spp:month, 
                 random=~1|indi, data=plants12, na.action=na.omit)
anova(chl.lme)

ggplot(plants12, aes(month, ce, color=species, shape=species)) +
  geom_point(size=3)+
  scale_color_manual(name="Species",
                     values = c("black", "gray50"))+
  scale_shape_manual(name="Species", values=c(1,2))+
  facet_grid(.~site)+
  labs(y=expression(paste(
    CE[]," (", mu * mol %.% m^{-2} %.% s^{-1} %.% kPa^{-1}, ")")))+
  labs(x="Month")+
  themeopts+
  theme(legend.text=element_text(face="italic"))
ggsave("ce.pdf")

ce.lme <- lme(ce ~ spp+month+place+spp:place+spp:month, 
               random=~1|indi, data=plants12, na.action=na.omit)
anova(ce.lme)

# to answer question 2, Do seasonal changes in leaf phys/morph traits relate 
# to soil N? Need data from both 2012 and 2013.  Since there was no statistical
# differences between leaf N or C13 or protein in 2012 and 2013 can include 
# both.  Need data on leaf N, C13, leaf C, CN, SLA, and leaf protien as well
# as well soil N

# current plan is a mixed effects model.  Nested by individual.  Time is
# continous. 


# reshape so only columns needed are there

soil13_1 <- soil13 %>% select(site, place, month, year, ammonia.kg, 
                              no3.no2.kg)
p2013_1 <- p2013 %>% select(site, place, month, year, spp, LMA, SLA, totN,
                            totC,C13, ug.gfw_pr) 
p2013_1$rep <-NULL
p2013_1 <-subset(p2013_1, month!="12")



all <- merge(soil13_1, p2013_1, all=T, by=c("site","place","month", "year"))
all$soilN <- all$ammonia.kg + all$no3.no2.kg
all$ratio <- all$ammonia.kg/all$no3.no2.kg
all$cn <- all$totC/all$totN
all$LMA <- as.numeric(all$LMA)
all$pro_area <- all$ug.gfw_pr * all$LMA

all$place <- as.numeric(all$place)
all$month <- as.numeric(all$month)
all$monthf <-factor(all$month, labels=c("May","July", "October"))
all$species <- factor(all$spp,
                           labels=c("C. stricta", "P. arundinacea"))


# remove MW 4 (place 12).
#This site ended up being in outlier to do problems with  soilN

all <- subset(all, place!="12")
allFig <-na.omit(all)

# subset of 13C and protein for table
Tab3 <- ddply(allFig, .(spp, monthf), summarize,
              C13_sd=sd(C13),
              C13=mean(C13),
              pro_area_sd=sd(pro_area),
              pro_area=mean(pro_area))
write.csv(Tab3, "table_3.csv")

#analysis
n.lme <- lme(totN ~ soilN + month + spp + soilN:month +
               soilN:spp, random =~1|place,
    data=all, na.action=na.omit)
summary(n.lme)
anova(n.lme)


n.gl <- glmer(totN ~ soilN + month + spp + soilN:month +
                       soilN:spp, random =~1|place,
                     data=all, na.action=na.omit)


ggplot(allFig, aes(soilN, totN, shape=monthf, color=monthf)) +
  geom_point(size=3)+
  facet_grid(.~species)+ 
  scale_color_manual(name="Month",
                    values = c("black", "gray50","gray15"))+
  scale_shape_manual(name="Month",values=c(1,2,3))+
  labs(x=("Total soil N (ppm)"), y=("Total leaf N (%)"))+
  themeopts

ggsave("totN13_final.pdf")

sla.lme <- lme(SLA ~ soilN + month + spp + soilN:month + soilN:spp,
               random =~1|place,
             data=all, na.action=na.omit)

anova(sla.lme)

ggplot(allFig, aes(soilN, SLA, shape=monthf, color=monthf)) +
  geom_point(size=3)+
  facet_grid(.~species)+ 
  #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se=F)+
  scale_color_manual(name="Month",
                     values = c("black", "gray50","gray15"))+
  scale_shape_manual(name="Month",values=c(1,2,3))+
  labs(y=expression(paste("SLA (",cm^{2} %.% g^{-1}, ")")))  +
  labs(x=("Total soil N (ppm)"))+
  themeopts+
  theme(strip.text.x=element_text(face="italic"))

ggsave("sla13_final.pdf")

c13.lme <- lme(C13 ~ soilN + month + spp + place+soilN:month + soilN:spp,
               random =~1|place,
               data=all, na.action=na.omit)

anova(c13.lme)

ggplot(allFig, aes(soilN, C13, shape=monthf, color=monthf)) +
  geom_point(size=3)+
  facet_grid(.~species)+ 
  scale_color_manual(name="Month",
                     values = c("black", "gray50","gray15"))+
  scale_shape_manual(name="Month",values=c(1,2,3))+
  labs(x=("Total soil N (ppm)"), y=("13C (‰)"))+
  themeopts+
  theme(strip.text.x=element_text(face="italic"))


ggsave("c13_final.pdf")



pr.lme <- lme(pro_area ~ soilN + month + spp + soilN:month + 
                soilN:spp + place, random =~1|place,
               data=all, na.action=na.omit)

anova(pr.lme)

ggplot(allFig, aes(soilN, log(pro_area), shape=monthf, color=monthf)) +
  geom_point(size=3)+
  facet_grid(.~spp)+ 
  #stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se=F)+
  scale_color_manual(name="Month",
                     values = c("black", "gray50","gray15"))+
  scale_shape_manual(name="Month",values=c(1,2,3))+
  labs(x=("Total soil N (ppm)"), y=("log of Total Soluble Protein (ug/cm^2)"))+
  themeopts

ggsave("protein13.pdf")


# possibly use model selection for quesiton 2

modnames <- c("soilN", "month", "species", "soilN X month", "soilN X species",
              "place")

# start with leaf N
cand.leafN<-list()
cand.leafN[[1]] <- lme(totN ~ soilN,
                        random=~1|place,method="ML", data=all, 
                       na.action=na.omit)
cand.leafN[[2]] <- lme(totN ~ month,
                        random=~1|place,method="ML", data=all,
                       na.action=na.omit)
cand.leafN[[3]] <- lme(totN ~ spp,
                        random=~1|place,method="ML", data=all,
                       na.action=na.omit)
cand.leafN[[4]] <- lme(totN ~ soilN*month,
                        random=~1|place,method="ML", data=all,
                        na.action=na.omit)
cand.leafN[[5]] <- lme(totN ~ soilN*spp,
                        random=~1|place,method="ML", data=all,
                       na.action=na.omit)
cand.leafN[[6]] <- lme(totN ~ place,
                        random=~1|place,method="ML", data=all,
                       na.action=na.omit)

library(AICcmodavg)

aictab(cand.leafN, modnames)
evidence(aictab(cand.leafN, modnames))

# SLA

cand.SLA<-list()
cand.SLA[[1]] <- lme(SLA ~ soilN,
                       random=~1|place,method="ML", data=all, 
                       na.action=na.omit)
cand.SLA[[2]] <- lme(SLA ~ month,
                       random=~1|place,method="ML", data=all,
                       na.action=na.omit)
cand.SLA[[3]] <- lme(SLA ~ spp,
                       random=~1|place,method="ML", data=all,
                       na.action=na.omit)
cand.SLA[[4]] <- lme(SLA ~ soilN*month,
                       random=~1|place,method="ML", data=all,
                       na.action=na.omit)
cand.SLA[[5]] <- lme(SLA ~ soilN*spp,
                       random=~1|place,method="ML", data=all,
                       na.action=na.omit)
cand.SLA[[6]] <- lme(SLA ~ place,
                       random=~1|place,method="ML", data=all,
                       na.action=na.omit)
aictab(cand.SLA, modnames)
evidence(aictab(cand.SLA, modnames))

# cn
cand.cn<-list()
cand.cn[[1]] <- lme(cn ~ soilN,
                     random=~1|place,method="ML", data=all, 
                     na.action=na.omit)
cand.cn[[2]] <- lme(cn ~ month,
                     random=~1|place,method="ML", data=all,
                     na.action=na.omit)
cand.cn[[3]] <- lme(cn ~ spp,
                     random=~1|place,method="ML", data=all,
                     na.action=na.omit)
cand.cn[[4]] <- lme(cn ~ soilN*month,
                     random=~1|place,method="ML", data=all,
                     na.action=na.omit)
cand.cn[[5]] <- lme(cn ~ soilN*spp,
                     random=~1|place,method="ML", data=all,
                     na.action=na.omit)
cand.cn[[6]] <- lme(cn ~ place,
                     random=~1|place,method="ML", data=all,
                     na.action=na.omit)
aictab(cand.cn, modnames)
evidence(aictab(cand.cn, modnames))

cand.pr<-list()
cand.pr[[1]] <- lme(ug.gfw_pr ~ soilN,
                    random=~1|place,method="ML", data=all, 
                    na.action=na.omit)
cand.pr[[2]] <- lme(ug.gfw_pr ~ month,
                    random=~1|place,method="ML", data=all,
                    na.action=na.omit)
cand.pr[[3]] <- lme(ug.gfw_pr ~ spp,
                    random=~1|place,method="ML", data=all,
                    na.action=na.omit)
cand.pr[[4]] <- lme(ug.gfw_pr ~ soilN*month,
                    random=~1|place,method="ML", data=all,
                    na.action=na.omit)
cand.pr[[5]] <- lme(ug.gfw_pr ~ soilN*spp,
                    random=~1|place,method="ML", data=all,
                    na.action=na.omit)
cand.pr[[6]] <- lme(ug.gfw_pr ~ place,
                    random=~1|place,method="ML", data=all,
                    na.action=na.omit)
aictab(cand.pr, modnames)
evidence(aictab(cand.pr, modnames))



