library(lme4)
library(nlme)
library(ggplot2)

# read in data. set working dir to data folder

soil13 <- read.csv("lachat2013.csv")
plants13 <- read.csv("data2013.csv")


# for making clean figures
source("theme-opts.R")


# filtering plant data
plants13 <- plants13 %>% filter(spp=="Cs"|spp=="Pa")

p2013 <- plants13 %>% group_by(site, rep, month, place, spp) %>%
  select(totC,C13,totN,SLA,ug.gfw_pr)
# don't need currently p2013$year <- "2013"

all <- merge(soil13, p2013, by=c("place","month","rep","site"))
all$soilN <- all$ammonia.kg + all$no3.no2.kg
all$ratio <- all$ammonia.kg/all$no3.no2.kg
all$cn <- all$totC/all$totN


n.lme <- lme(totN ~ soilN + month + spp + soilN:month +
               soilN:spp, random =~soilN|place,
             data=all, na.action=na.omit)

anova(n.lme)

ggplot(all, aes(soilN, totN, shape=spp, color=spp)) +
  geom_point()+
  facet_grid(month~.)+ 
  scale_color_manual(name="N species",
                     values = c("black", "gray50"))+
  geom_smooth(method="lm", se=F)+themeopts
