library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)

### Load data and make bream plots

mydata <- read.csv("revised all est worksheet July 2011_LATEST.csv", header = T)
str(mydata)
mydata$After.1...2 <- factor(mydata$After.1...2, levels = c("Before", "After 1", "After 2"))
mydata$Survey <- as.integer(mydata$After.1...2)
mydata$Habitat <- as.character(mydata$Habitat)
mydata$Habitat[mydata$Habitat  == "SD"] <- "AR"
mydata$Habitat[mydata$Habitat  == "AR"] <- "Artificial Reef"
mydata$Habitat[mydata$Habitat  == "NR"] <- "Natural Reef"



mydata <- subset(mydata, Time != "Estimation")
mydata <- subset(mydata, Season != "Aut")

dat <- mydata %>% group_by(Location, Survey, Habitat) %>% 
  summarise(mean_MaxN = mean(Acanthopagrus.australis), SD = sd(Acanthopagrus.australis), n = n(), SE = SD/sqrt(n))
dat

#dat2 <- mydata %>% group_by(Location, Before.After, Habitat) %>% 
#  summarise(Start_Date = min(Time), End_Date = max(Time))
#dat2


p1 <- ggplot(dat, aes(x = Survey, y = mean_MaxN, col = Habitat)) + geom_point() + facet_wrap(~Location) + geom_line() +
  geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) +
  theme_classic()
p1

#### I need to melt the dataframes and make the graphs for all shown species and see how many of them match.


species_list <- c("Acanthopagrus.australis", "Rhabdosargus.sarba", "Pagrus.auratus", "Pelates.sexlineatus",
                  "Pseudocaranx.dentex", "Trachurus.novaezelandiae", "Atypichthys.strigatus", 
                  "Microcanthus.strigatus")

species_list_sparid <- c("Acanthopagrus.australis", "Rhabdosargus.sarba", "Pagrus.auratus")
species_list_other <- c("Pelates.sexlineatus", "Pseudocaranx.dentex", "Trachurus.novaezelandiae", 
                        "Atypichthys.strigatus", "Microcanthus.strigatus")


# Make long data
long_data <- melt(mydata, id=c("Location","Impact.Control", "Site", "Before.After", "Year",
                               "Time", "Habitat", "Season", "After.1...2","Survey"), value.name = "MaxN")

#Summarise data
dat <- long_data %>% group_by(Location, Survey, Habitat, variable) %>% 
  summarise(mean_MaxN = mean(MaxN), SD = sd(MaxN), n = n(), SE = SD/sqrt(n))
dat

# Keep only species in the above list
datX <- dat %>% filter(variable %in% species_list_sparid)
datY <- dat %>% filter(variable %in% species_list_other)

## All locations and sparid species in one plot
#Colour blind palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

strip_labels <- c("Acanthopagrus.australis" = "a) Acanthopagrus australis", 
                  "Rhabdosargus.sarba" = "b) Rhabdosargus sarba",
                  "Pagrus.auratus" = "c) Chrysophrys auratus")



pX <- ggplot(datX, aes(x = Survey, y = mean_MaxN, col = Location, linetype = Habitat, shape = Habitat)) +
  geom_point() + geom_line() + xlab("Sample Period") +
  facet_wrap(~variable, scales = "free_y", ncol =2, labeller = as_labeller(strip_labels)) + 
  geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) +
  theme_bw() + ylab("Abundance (Mean MaxN ±SE)") + scale_colour_manual(values=cbbPalette)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        axis.ticks = element_line(colour = "black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size = 10, face = "bold"),
        strip.background = element_rect(colour="black", fill="white"),
        strip.text.x = element_text(size=12, face="bold.italic"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.key.width = unit(3, "line"),
        legend.justification=c(0,0.5), legend.position=c(0.65,0.15)) +
  scale_x_continuous(breaks=c(1,2,3), labels= c("Before", "Year \n1", 'Year \n2'))
pX

ggsave("Output/Final Figures/Species Abundances.pdf", width = 6.3, height = 4.3, units = "in", dpi = 600)
ggsave("Output/Final Figures/Species Abundances.png", width = 6.3, height = 4.3, units = "in", dpi = 600)


# # Make plots for each species, with a panel for each location
# for (i in species_list) {
#   
#   dat2 <- subset(dat, variable == i)
# 
#   p1 <- ggplot(dat2, aes(x = Survey, y = mean_MaxN, col = Habitat)) + geom_point() + facet_wrap(~Location) + geom_line() +
#     geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) +
#     theme_classic()
#   p1
#   #ggsave(filename = paste("Output/",i, ".pdf", sep=""), width = 12, height = 8)
#   
# }

# # Make plots for each location with a panel for each species
# for (i in datX$Location) {
#   
#   dat2 <- subset(datX, Location == i)
#   
#   p1 <- ggplot(dat2, aes(x = Survey, y = mean_MaxN, col = Habitat)) + geom_point() + 
#     facet_wrap(~variable, scales = "free_y") + geom_line() +
#     geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) +
#     theme_classic()
#   p1
#   #ggsave(filename = paste("Output/",i, ".pdf", sep=""), width = 12, height = 8)
#   
# }

#####################################################################################################
### Now only do some species based upon Heath's choices
Lake_Mac_Sp <- c("Pelates.sexlineatus")
Bot_Bay_Sp <- c("Pseudocaranx.dentex", "Trachurus.novaezelandiae", "Atypichthys.strigatus")
St_Geo_Sp <- c("Pelates.sexlineatus", "Trachurus.novaezelandiae", "Microcanthus.strigatus")

LM_List <- paste("Lake Macquarie", "_", Lake_Mac_Sp, sep="")
BB_List <- paste("Botany Bay", "_", Bot_Bay_Sp, sep="")
SG_List <- paste("St Georges Basin", "_", St_Geo_Sp, sep="")

species_list2 <-c(LM_List, BB_List, SG_List)

datY$Loc_Sp <- paste(datY$Location, "_", datY$variable, sep = "")

# Keep only species in the above list
datY2 <- datY %>% filter(Loc_Sp %in% species_list2)

### Now redo graphs


## All locations and other species in one plot
#Colour blind palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pX <- ggplot(datY2, aes(x = Survey, y = mean_MaxN, col = Location, linetype = Habitat, shape = Habitat)) + geom_point() + 
  facet_wrap(~variable, scales = "free_y", ncol =3) + geom_line() +
  geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) +
  theme_classic() + ylab(paste("Mean MaxN (","\U00B1","SE)", sep = "" )) + scale_colour_manual(values=cbbPalette)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 20),
        axis.text.y  = element_text(colour="black", size = 16),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        legend.justification=c(0,0.5), legend.position=c(0.7,0.13),
        #legend.position="bottom",
        legend.key.width = unit(3, "line"),
        strip.text = element_text(face = "italic")) +
  scale_x_continuous(breaks=c(1,2,3), labels= c("Before", "Year \n1", 'Year \n2')) #+
  #guides(col = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
pX




# Make plots for each location with a panel for each species
for (i in datX$Location) {
  
  dat2 <- subset(datX, Location == i)
  
  p1 <- ggplot(dat2, aes(x = Survey, y = mean_MaxN, col = Habitat)) + geom_point() + 
    facet_wrap(~variable, scales = "free_y") + geom_line() +
    geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) +
    theme_classic()
  p1
  #ggsave(filename = paste("Output/2Limited_species_",i, ".pdf", sep=""), width = 12, height = 8)
  
}




### Creating diversity variables
# From Charlie's Code - hence larval fish names
library(vegan)
mydata <- read.csv("revised all est worksheet July 2011_LATEST.csv", header = T)

mydata$After.1...2 <- factor(mydata$After.1...2, levels = c("Before", "After 1", "After 2"))
mydata$Survey <- as.integer(mydata$After.1...2)
mydata$Habitat <- as.character(mydata$Habitat)
mydata$Habitat[mydata$Habitat  == "SD"] <- "AR"
mydata$Habitat[mydata$Habitat  == "AR"] <- "Artificial Reef"
mydata$Habitat[mydata$Habitat  == "NR"] <- "Natural Reef"

mydata <- subset(mydata, Time != "Estimation")
mydata <- subset(mydata, Season != "Aut")

larval.spp<-mydata[1:372,c(2:37,39:75)]
#head(larval.spp)
#write.csv(mydata, "test.csv", row.names = F)


abundance <- rowSums(larval.spp)
richness <- specnumber(larval.spp) # richness
shannon <- diversity(larval.spp) #shannon wevaer index from vegan package
J <- shannon/log(specnumber(larval.spp)) # Evenness
shannon_effective <- exp(diversity(larval.spp))


mydata$Diversity <- shannon
mydata$evenness <- J
mydata$Richness <- richness
mydata$Abundance <- abundance
mydata$shannon_effective <- shannon_effective


# Make long data
long_data <- melt(mydata, id=c("Location","Impact.Control", "Site", "Before.After", "Year",
                               "Time", "Habitat", "Season", "After.1...2","Survey"), value.name = "MaxN")

#Summarise data
dat <- long_data %>% group_by(Location, Survey, Habitat, variable) %>% 
  summarise(mean_MaxN = mean(MaxN, na.rm = TRUE), SD = sd(MaxN), n = n(), SE = SD/sqrt(n))
dat

#var_list <- c("Richness", "Abundance")
var_list <- "Abundance"

# Keep only species in the above list
#datX <- dat %>% filter(variable %in% var_list)
datX <- subset(dat, variable == "Abundance")
datX$Location <- factor(datX$Location, levels = c("Lake Macquarie", "St Georges Basin", "Botany Bay"))

strip_labels <- c("Lake Macquarie" = "a) Lake Macquarie", 
                  "St Georges Basin" = "b) St Georges Basin",
                  "Botany Bay" = "c) Botany Bay")

p1 <- ggplot(datX, aes(x = Survey, y = mean_MaxN, linetype = Habitat, shape = Habitat)) + 
  geom_point() + geom_line() + 
  geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) + xlab("Sample Period") + 
  ylab("Total Abundance \n(Mean Total MaxN ±SE)") + 
  facet_wrap(~ Location, labeller = as_labeller(strip_labels)) +
  theme_bw() +
  scale_x_continuous(breaks=c(1,2,3), labels= c("   Before", "Year \n1", 'Year    \n2   ')) + # note weird spaces to help align the labels on figure
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        axis.ticks = element_line(colour = "black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size = 10, face = "bold"),
        legend.position=c(0.16,0.78),
        strip.background = element_rect(colour="black", fill="white"),
        strip.text.x = element_text(size=12, face="bold"),
        strip.text.y = element_text(size=12, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.key.width = unit(3, "line"))
p1

ggsave("Output/Final Figures/Figure 1.pdf", width = 6.3, height = 3.2, units = "in")
ggsave("Output/Final Figures/Figure 1.png", width = 6.3, height = 3.2, units = "in", dpi = 600)

#### Sparid Graphs ####

mydata <- read.csv("revised all est worksheet July 2011_LATEST.csv", header = T)
str(mydata)
mydata$After.1...2 <- factor(mydata$After.1...2, levels = c("Before", "After 1", "After 2"))
mydata$Survey <- as.integer(mydata$After.1...2)
mydata$Habitat <- as.character(mydata$Habitat)
mydata$Habitat[mydata$Habitat  == "SD"] <- "AR"
mydata$Habitat[mydata$Habitat  == "AR"] <- "Artificial Reef"
mydata$Habitat[mydata$Habitat  == "NR"] <- "Natural Reef"
mydata$Sparids <- mydata$Acanthopagrus.australis + mydata$Pagrus.auratus +
  mydata$Rhabdosargus.sarba


mydata <- subset(mydata, Time != "Estimation")
mydata <- subset(mydata, Season != "Aut")



#mydata <- subset(mydata, Time != "Estimation")
dat <- mydata %>% group_by(Location, Survey, Habitat) %>% 
  summarise(mean_MaxN = mean(Sparids), SD = sd(Sparids), n = n(), SE = SD/sqrt(n))
dat

#dat2 <- mydata %>% group_by(Location, Before.After, Habitat) %>% 
#  summarise(Start_Date = min(Time), End_Date = max(Time))
#dat2

dat$Location <- factor(dat$Location, levels = c("Lake Macquarie", "St Georges Basin", "Botany Bay"))

strip_labels <- c("Lake Macquarie" = "a) Lake Macquarie", 
                  "St Georges Basin" = "b) St Georges Basin",
                   "Botany Bay" = "c) Botany Bay")

p1 <- ggplot(dat, aes(x = Survey, y = mean_MaxN, linetype = Habitat, shape = Habitat)) + geom_point() + 
  facet_wrap(~Location, labeller = as_labeller(strip_labels)) + geom_line() + xlab("Sample Period") +
  geom_errorbar(aes(ymin = mean_MaxN-SE, ymax = mean_MaxN+SE), width = 0.1) +
  theme_bw() + ylab("Sparidae Abundance \n(Mean Total MaxN ±SE)") +
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
p1

ggsave("Output/Final Figures/Sparid Abundance.pdf", width = 6.3, height = 3.2, units = "in", dpi=600)
ggsave("Output/Final Figures/Sparid Abundance.png", width = 6.3, height = 3.2, units = "in", dpi=600)
