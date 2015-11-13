library(lme4)
library(nlme)
library(ggplot2)
library(dplyr)

# read in data. set working dir to data folder

soil13 <- read.csv("../data/lachat2013.csv")
plants13 <- read.csv("../data/data2013.csv")


# for making clean figures
source("theme-opts.R")


# filtering plant data
plants13 <- plants13 %>% filter(spp=="Cs"|spp=="Pa")

p2013 <- plants13 %>% group_by(site, rep, month, place, spp) %>%
  select(totC,C13,totN,SLA,ug.gfw_pr)
# don't need currently p2013$year <- "2013"

# merging the soil data with the plant data
# have a doubling of soil data as one data point for soil N but 
# possibly two points for species.
allsoil <- merge(soil13, p2013, by=c("place","month","rep","site"))
allsoil$soilN <- allsoil$ammonia.kg + allsoil$no3.no2.kg
allsoil$ratio <- allsoil$ammonia.kg/allsoil$no3.no2.kg
allsoil$cn <- allsoil$totC/allsoil$totN

# here I have set up the mixed effects model with soilN nested in place
# place is a random effect but soilN is fixed, but nested. I want to have 
# soil N nested with the random effect of place.

# DWS: I think you mean that soil N may show an effect of site or rep or
# smapleID. I thinkt he latter?. I'm not sure. I'm sure you don't mean it is
# nested in "place". I don't udnerstand the column headings well enough. Wht
# are place, rep, site, sample and sampleID? This is a lot of factors but they
# seem to be duplicated. I don't think you want this. You have species as a
# fixed effect so it is fine, no duplication.

# using nlme. random intercept by "place"?
n.lme <- lme(totN ~ soilN*month*spp, random = ~ 1 | place,
             data=allsoil, na.action=na.omit)
summary(n.lme)
anova(n.lme)

# DWS: or, are you worried about finer scale random effects? You'll have less
# power and I'm worried since so many sampleIDs are unreplicated. It looks like
# sample id is all you need as it is what groups the two N values per species? But what do you think groups those? is there a fine scale "plot" variable"
n2.lme <- lme(totN ~ soilN + month + spp + soilN:month +
               soilN:spp, random = ~ 1 | Sample.ID,
             data=allsoil, na.action=na.omit)
summary(n2.lme)
anova(n2.lme)


ggplot(allsoil, aes(soilN, totN, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)+ 
  scale_color_manual(name="N species",
                     values = c("black", "gray50"))+
  geom_smooth(method="lm", se=F)+themeopts

# using lme4 package
n.lmer <- lmer(totN ~ spp*month*soilN + (1 | place), data=allsoil,
               na.action=na.omit)
summary(n.lmer)
anova(n.lmer)

# these match. but you don't get p values. Can us likelihoods. note warning.
# SHould apply to lme run above as well. Not sure why you had these tesing
# separate models.
