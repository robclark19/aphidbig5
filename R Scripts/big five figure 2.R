# Fig 2 ####
# Fig 2 should be differences in preference among biotypes tested
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MASS")
library("tidyverse")

settle.dat <- read.csv("./Data/settle mod.csv", header= TRUE)

hist(settle.dat$Counts)

# First model

new.count.mod <- glm.nb(Counts ~ Biotype*Plant_sp*Virus, data=settle.dat)

mod1_sa <- stepAIC(new.count.mod, direction = "both")

mod1_sa$anova

# Final model

final.count.mod <- glm.nb(Counts ~ Biotype + Plant_sp + Biotype:Plant_sp, data=settle.dat)

Anova(final.count.mod)

count.lsm <- cld(emmeans(final.count.mod, ~ Biotype|Plant_sp), sort=FALSE, adjust="none", type="response")

#edit groupings so they line up better in ggplot
count.lsm$.group=gsub(" ", "", count.lsm$.group)

count.lsm$emmean <- count.lsm$response

#make figure for counts of aphids that move to each host plant
count.lsm.fig <- ggplot(count.lsm, aes(x=Plant_sp, y=emmean, fill=Biotype)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Number of aphids that moved towards host plant", x="Plant Species") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Plant_sp, y = (emmean+SE+2), label = .group), position=position_dodge(width=0.8))
count.lsm.fig
