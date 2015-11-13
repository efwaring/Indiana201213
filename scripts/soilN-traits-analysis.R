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

# using nlme

n.lme <- lme(totN ~ soilN + month + spp + soilN:month +
               soilN:spp, random =~soilN|place,
             data=allsoil, na.action=na.omit)

anova(n.lme)

ggplot(allsoil, aes(soilN, totN, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)+ 
  scale_color_manual(name="N species",
                     values = c("black", "gray50"))+
  geom_smooth(method="lm", se=F)+themeopts

# using lme4 package

n.lmer <- lmer(totN ~ spp + month + soilN +(1|place/soilN), data=allsoil,
               na.action=na.omit)
summary(n.lmer)
 
anova(n.lmer)
