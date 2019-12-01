# ### Sparidae life stages data prep
# ### 25/10/19
# 
# library(reshape2)
library(dplyr)
# 
# mydata <- read.csv("revised all est worksheet July 2011_LATEST.csv", header = T, row.names = "X")
# 
# mydata_long <- melt(mydata, measure.vars = 1:74, variable.name = "Species", value.name = "Abundance")
# head(mydata_long)
# 
# mydata_long$Time <- as.Date(mydata_long$Time, format = "%d/%m/%Y")
# mydata_long$Species <- gsub(".", " ", mydata_long$Species, fixed=TRUE)
# str(mydata_long)
# 
# stage_dat <- read.csv("Sparida stages data.csv", header = T)
# str(stage_dat)
# stage_dat$Time <- stage_dat$CruiseStartDate
# 
# library(lubridate)
# 
# stage_dat$Time <- parse_date_time(stage_dat$Time, orders = "d! b! Y!")
# stage_dat$Time<- as.Date(stage_dat$Time)
# str(stage_dat)
# stage_dat$Species <- stage_dat$Genus.species
# stage_dat$Site <- stage_dat$Destination
# 
# total_dat <- left_join(mydata_long, stage_dat, by = c("Location", "Species", "Site", "Time"))
# head(total_dat)
# total_dat <- subset(total_dat, Species == "Acanthopagrus australis" | 
#                       Species == "Pagrus auratus" | Species == "Rhabdosargus sarba")
# #table(total_dat$Species)
# #table(total_dat$MAX.N..Dive.N)
# 
# #### I think data is all there now - but how to plot/analyse???
# datX <- total_dat %>% group_by(Location, Site, Time, Before.After.x, Habitat, STAGE) %>% summarise(Abundance = sum(MAX.N..Dive.N, na.rm = T))
# head(datX)
# 
# table(datX$Before.After.x, datX$Habitat, datX$STAGE)
# sum(is.na(datX$STAGE))
# 
# # write to csv and fix so each day/site cobo has juvenile and adult sparidae abundance
# #write.csv(datX, "Life stage data midpoint.csv", row.names = FALSE) # don't run again.
# ### Still to put together (I think it's working - just fill in the gaps)
# 
# mydata <- read.csv("Life stage data midpoint.csv", header = T)
# 
# head(mydata)
# table(mydata$STAGE)
# 
# library(tidyr)
# mydata2 <- spread(mydata, STAGE, Abundance)
# head(mydata2)
# mydata2$Prop_J <- (mydata2$JUV/(mydata2$JUV + mydata2$AD))
# head(mydata2)

# Fix in excel - limit to correct dates, relabel after1 and after2
#write.csv(mydata2, "Life stage proportions.csv", row.names = FALSE)
mydata <- read.csv("Life stage proportions_edited.csv", header = T)
head(mydata)


mydata$Before.After.x <- factor(mydata$Before.After.x, levels = c("Before", "After 1", "After 2"))
mydata$Survey <- as.integer(mydata$Before.After.x)
mydata$Habitat <- as.character(mydata$Habitat)
mydata$Habitat[mydata$Habitat  == "SD"] <- "AR"
mydata$Habitat[mydata$Habitat  == "AR"] <- "Artificial Reef"
mydata$Habitat[mydata$Habitat  == "NR"] <- "Natural Reef"


dat <- mydata %>% group_by(Location, Survey, Habitat) %>% 
  summarise(Props = mean(Prop_J, na.rm = T), n = n(), SD = sd(Prop_J, na.rm = T), SE = SD/sqrt(n))
head(dat)

library(ggplot2)

p1 <- ggplot(dat, aes(x = Survey, y = Props, col = Habitat)) + geom_point() + facet_wrap(~Location) + geom_line() +
  geom_errorbar(aes(ymin = Props-SE, ymax = Props+SE), width = 0.1) +
  theme_classic()
p1


# Juvenile abundances
dat2 <- mydata %>% group_by(Location, Survey, Habitat) %>% 
  summarise(JUVs = mean(JUV, na.rm = T), n = n(), SD = sd(JUV, na.rm = T), SE = SD/sqrt(n))
head(dat)


p2 <- ggplot(dat2, aes(x = Survey, y = JUVs, col = Habitat)) + geom_point() + facet_wrap(~Location) + geom_line() +
  geom_errorbar(aes(ymin = JUVs-SE, ymax = JUVs+SE), width = 0.1) +
  theme_classic()
p2


# Adult abundances
dat3 <- mydata %>% group_by(Location, Survey, Habitat) %>% 
  summarise(Adults = mean(AD, na.rm = T), n = n(), SD = sd(AD, na.rm = T), SE = SD/sqrt(n))
head(dat)


p3 <- ggplot(dat3, aes(x = Survey, y = Adults, col = Habitat)) + geom_point() + facet_wrap(~Location) + geom_line() +
  geom_errorbar(aes(ymin = Adults-SE, ymax = Adults+SE), width = 0.1) +
  theme_classic()
p3

dat3$Location <- factor(dat$Location, levels = c("Lake Macquarie", "St Georges Basin", "Botany Bay"))

strip_labels <- c("Lake Macquarie" = "a) Lake Macquarie", 
                  "St Georges Basin" = "b) St Georges Basin",
                  "Botany Bay" = "c) Botany Bay")

p3 <- ggplot(dat3, aes(x = Survey, y = Adults, linetype = Habitat, shape = Habitat)) + geom_point() + 
  facet_wrap(~Location, labeller = as_labeller(strip_labels)) + geom_line() + xlab("Sample Period") +
  geom_errorbar(aes(ymin = Adults-SE, ymax = Adults+SE), width = 0.1) +
  theme_bw() + ylab("Adult Sparidae Abundance \n(Mean Total MaxN ±SE)") +
  scale_x_continuous(breaks=c(1,2,3), labels= c("   Before", "Year \n1", 'Year    \n2   ')) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour = "black"),
        axis.text.y  = element_text(colour="black", size = 12),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size = 10, face = "bold"),
        legend.position=c(0.825,0.79),
        legend.background=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.key.width = unit(3, "line"))
p3


### Juvs and adults together

dat2$Stage <- "Juvenile"
dat3$Stage <- "Adult"

dat2 <- dat2 %>% rename(MaxN = JUVs)
head(dat2)
dat3 <- dat3 %>% rename(MaxN = Adults)
head(dat3)

full_dat <- rbind(dat2, dat3)
full_dat

full_dat$Location <- factor(full_dat$Location, levels = c("Lake Macquarie", "St Georges Basin", "Botany Bay"))

strip_labels <- c("Lake Macquarie" = "a) Lake Macquarie", 
                  "St Georges Basin" = "b) St Georges Basin",
                  "Botany Bay" = "c) Botany Bay",
                  "Juvenile" = "Juvenile",
                  "Adult" = "Adult")

p4 <- ggplot(full_dat, aes(x = Survey, y = MaxN, linetype = Habitat, shape = Habitat)) + geom_point() + 
  facet_grid(Stage~Location, labeller = as_labeller(strip_labels)) + 
  geom_line() + xlab("Sample Period") +
  geom_errorbar(aes(ymin = MaxN-SE, ymax = MaxN+SE), width = 0.1) +
  theme_bw() + ylab("Sparidae Abundance \n(Mean Total MaxN ±SE)") +
  scale_x_continuous(breaks=c(1,2,3), labels= c("   Before", "Year \n1", 'Year    \n2   ')) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour = "black"),
        axis.text.y  = element_text(colour="black", size = 12),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size = 10, face = "bold"),
        legend.position=c(0.5,0.35),
        legend.background=element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.key.width = unit(3, "line"))
p4


ggsave("Output/Final Figures/Sparid Stages Abundance.pdf", width = 6.3, height = 4, units = "in", dpi=600)
ggsave("Output/Final Figures/Sparid Stages Abundance.png", width = 6.3, height = 4, units = "in", dpi=600)
