## Re-analysis

# 3 times
# 3 estuaries
# 2 habitats
# many locations nested in habitats

library(nlme)
library(MASS)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(emmeans)
library(car)

library(broom)

mydata <- read.csv("../Hayden_data_July_19v2.csv", header = T)

mydata$After.1...2 <- factor(mydata$After.1...2, levels = c("Before", "After 1", "After 2"))
mydata$Survey <- as.integer(mydata$After.1...2)
mydata$Habitat <- as.character(mydata$Habitat)
mydata$Habitat[mydata$Habitat  == "SD"] <- "AR"
mydata$Habitat[mydata$Habitat  == "AR"] <- "Artifical Reef"
mydata$Habitat[mydata$Habitat  == "NR"] <- "Natural Reef"
mydata$Habitat <- as.factor(mydata$Habitat)
mydata$Site <- as.factor(paste(mydata$Location, mydata$Site2))
#mydata$Time <- as.Date(as.character(mydata$Time, "%d/%m/%Y"))
mydata <- subset(mydata, Season != "Aut")

library(tidyverse)

ggplot(mydata,aes(y=Total.Abund,x=Time,colour=Time))+geom_boxplot()


#glmer approach - lots of errors!
fit1 <- glmer.nb(Total.Abund ~Location + After.1...2*Habitat + (Time|Site), data = mydata)

#glmmtmb approach - residual vs fitted plot in question but checked with dharma looks OK
fit2 <- glmmTMB(Total.Abund ~Location*After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)

#check assumptions
simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
plot(simulationOutput)
plotResiduals(mydata$Site, simulationOutput$scaledResiduals)
#looks alright!

#test for overall interaction effect
Anova(fit2,type="II",test="Chisq")
#test for individual effects
emmeans(fit2, pairwise~After.1...2|Habitat|Location,type="response")
table(mydata$Location, mydata$Habitat, mydata$After.1...2)

#look at all the means
fit_means <- glmmTMB(Total.Abund ~Location*After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
emmeans(fit_means, pairwise~After.1...2:Habitat:Location,type="response")

out1 <- emmeans(fit_means, pairwise~After.1...2:Habitat:Location,type="response")
out1
write.csv(out1$contrasts, "Total Abundance Contrasts.csv", row.names = FALSE)


########## New bit


pairs(emmeans(fit2, ~ After.1...2|Location|Habitat))






# #######################################
# 
# 
# #now test for time effect within artificial habitats
# mydata_noNAT <- subset(mydata, Habitat != "Natural Reef")
# fit2_noNAT <- glmmTMB(Total.Abund ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noNAT)
# 
# #test for overall interaction effect
# Anova(fit2_noNAT,type="II",test="Chisq")
# #test for individual effects
# out2 <- emmeans(fit2_noNAT, pairwise~Location:After.1...2,type="response")
# out2
# write.csv(out2$contrasts, "Total Abundance Artificial Contrasts.csv", row.names = FALSE)
# 
# 
# #test fpor time effect within natural habitats
# mydata_noArt <- subset(mydata,Habitat!="Artifical Reef")
# fit2_noArt <- glmmTMB(Total.Abund ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noArt)
# 
# #test for overall interaction effect
# Anova(fit2_noArt,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noArt, pairwise~Location, type = "response")
# #no evidence for difference, if we look at direction of changes they all seem to be overall positive
# 
# exp(0.139)
# 
# log10(1.163)

### Now for Acanthopagrus
# 
# 
# #glmmtmb approach - residual vs fitted plot in question but checked with dharma looks OK
# fit2 <- glmmTMB(Acanthopagrus.australis ~Location+After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
# 
# #check assumptions
# simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
# plot(simulationOutput)
# plotResiduals(mydata$Site, simulationOutput$scaledResiduals)
# #looks alright!
# 
# #test for overall interaction effect
# Anova(fit2,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2, pairwise~Habitat:After.1...2,type="response")
# 
# #look at all the means
# fit_means <- glmmTMB(Acanthopagrus.australis ~Location*After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
# emmeans(fit_means, pairwise~After.1...2:Habitat:Location,type="response")
# 
# #now test for time effect within artificial habitats
# mydata_noNAT <- subset(mydata, Habitat != "Natural Reef")
# fit2_noNAT <- glmmTMB(Acanthopagrus.australis ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noNAT)
# 
# #test for overall interaction effect
# Anova(fit2_noNAT,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noNAT, pairwise~After.1...2,type="response")
# 
# #test fpor time effect within natural habitats
# mydata_noArt <- subset(mydata,Habitat!="Artifical Reef")
# fit2_noArt <- glmmTMB(Acanthopagrus.australis ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noArt)
# 
# #test for overall interaction effect
# Anova(fit2_noArt,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noArt, pairwise~After.1...2, type = "response")
# #no evidence for difference, if we look at direction of changes they all seem to be overall positive
# 
# 
# 
# 
# ### Now for Rhabdosargus.sarba
# 
# 
# #glmmtmb approach - residual vs fitted plot in question but checked with dharma looks OK
# fit2 <- glmmTMB(Rhabdosargus.sarba ~Location+After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
# 
# #check assumptions
# simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
# plot(simulationOutput)
# plotResiduals(mydata$Site, simulationOutput$scaledResiduals)
# #looks alright!
# 
# #test for overall interaction effect
# Anova(fit2,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2, pairwise~Habitat:Location,type="response")
# 
# #look at all the means
# fit_means <- glmmTMB(Rhabdosargus.sarba ~Location*After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
# emmeans(fit_means, pairwise~After.1...2:Habitat:Location,type="response")
# 
# #now test for time effect within artificial habitats
# mydata_noNAT <- subset(mydata, Habitat != "Natural Reef")
# fit2_noNAT <- glmmTMB(Rhabdosargus.sarba ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noNAT)
# 
# #test for overall interaction effect
# Anova(fit2_noNAT,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noNAT, pairwise~After.1...2,type="response")
# 
# #test fpor time effect within natural habitats
# mydata_noArt <- subset(mydata,Habitat!="Artifical Reef")
# fit2_noArt <- glmmTMB(Rhabdosargus.sarba ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noArt)
# 
# #test for overall interaction effect
# Anova(fit2_noArt,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noArt, pairwise~After.1...2, type = "response")
# #no evidence for difference, if we look at direction of changes they all seem to be overall positive
# 
# 
# 
# ### Now for Pagrus.auratus
# 
# 
# #glmmtmb approach - residual vs fitted plot in question but checked with dharma looks OK
# fit2 <- glmmTMB(Pagrus.auratus ~Location+After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
# 
# #check assumptions
# simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
# plot(simulationOutput)
# plotResiduals(mydata$Site, simulationOutput$scaledResiduals)
# #looks alright!
# 
# #test for overall interaction effect
# Anova(fit2,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2, pairwise~Habitat:Location,type="response")
# 
# #look at all the means
# fit_means <- glmmTMB(Pagrus.auratus ~Location*After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
# emmeans(fit_means, pairwise~After.1...2:Habitat:Location,type="response")
# 
# #now test for time effect within artificial habitats
# mydata_noNAT <- subset(mydata, Habitat != "Natural Reef")
# fit2_noNAT <- glmmTMB(Pagrus.auratus ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noNAT)
# 
# #test for overall interaction effect
# Anova(fit2_noNAT,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noNAT, pairwise~After.1...2,type="response")
# 
# #test fpor time effect within natural habitats
# mydata_noArt <- subset(mydata,Habitat!="Artifical Reef")
# fit2_noArt <- glmmTMB(Pagrus.auratus ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noArt)
# 
# #test for overall interaction effect
# Anova(fit2_noArt,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noArt, pairwise~After.1...2, type = "response")
# #no evidence for difference, if we look at direction of changes they all seem to be overall positive
# 
# 
# 
# 

### Now for Total Sparids

mydata$Total_Sparid <- mydata$Acanthopagrus.australis + mydata$Rhabdosargus.sarba + mydata$Pagrus.auratus


#glmmtmb approach - residual vs fitted plot in question but checked with dharma looks OK
fit2 <- glmmTMB(Total_Sparid ~Location*After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)

#check assumptions
simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
plot(simulationOutput)
plotResiduals(mydata$Site, simulationOutput$scaledResiduals)
#looks alright!

#test for overall interaction effect
Anova(fit2,type="II",test="Chisq")
#test for individual effects
emmeans(fit2, pairwise~Habitat:Location,type="response")

#look at all the means
fit_means <- glmmTMB(Total_Sparid ~Location*After.1...2*Habitat + (1|Site)+(1|Time),family=nbinom2, data = mydata)
out_s <-emmeans(fit_means, pairwise~After.1...2:Habitat:Location,type="response")
out_s
write.csv(out_s$emmeans, "Sparidae means output table.csv", row.names = FALSE)

emmeans(fit2, pairwise~After.1...2|Habitat|Location,type="response")
# #now test for time effect within artificial habitats
# mydata_noNAT <- subset(mydata, Habitat != "Natural Reef")
# fit2_noNAT <- glmmTMB(Total_Sparid ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noNAT)
# 
# #test for overall interaction effect
# Anova(fit2_noNAT,type="II",test="Chisq")
# #test for individual effects
# emmeans(fit2_noNAT, pairwise~After.1...2,type="response")
# emmeans(fit2_noNAT, pairwise~Location,type="response")
# 
# #test fpor time effect within natural habitats
# mydata_noArt <- subset(mydata,Habitat!="Artifical Reef")
# fit2_noArt <- glmmTMB(Total_Sparid ~Location*After.1...2 + (1|Site)+(1|Time),family=nbinom2, data = mydata_noArt)
# 
# #test for overall interaction effect
# Anova(fit2_noArt,type="II",test="Chisq")
# #test for individual effects
# out_sn <- emmeans(fit2_noArt, pairwise~Location*After.1...2, type = "response")
# #no evidence for difference, if we look at direction of changes they all seem to be overall positive
# out_sn
# write.csv(out_sn$contrasts, "Sparidae natural reefs pairwise.csv", row.names = F)
# 
# 
# 
# 
# 






