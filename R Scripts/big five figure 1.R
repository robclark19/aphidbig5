
# Fig 1
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MASS")
library("tidyverse")

bf <- read.csv("./Data/big five Logan check.csv")

bf$Genotype <- as.factor(bf$Genotype)

# First model
both.bio.mod <- glm.nb(Counts ~ Biotype * Plant * virus, data = bf)
summary(both.bio.mod)
Anova(both.bio.mod)

both.bio.aic <- stepAIC(both.bio.mod)
both.bio.aic$anova

#Final model
final.bio.mod <- glm.nb(Counts ~ Biotype + Plant + virus + Biotype:Plant + Plant:virus, data = bf)

Anova(final.bio.mod)

# Fig 1a ########
# Biotype by plant figure

both.bio.cld <- cld(emmeans(final.bio.mod, ~ Biotype|Plant, adjust="none", type="response"), sort=FALSE, Letters=c("abc"))
both.bio.cld

# edit emmean object so it makes ggplot happy
both.bio.cld$.group=gsub(" ", "", both.bio.cld$.group)
both.bio.cld$emmean <- both.bio.cld$response

#make ggplot figure based on post hoc tests above
# hmm how to fix the fact alfalfa isnt there? partition without viruses?

both.bio.fig <- ggplot(both.bio.cld, aes(x=Plant, y=emmean, fill=Biotype)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid count at one week", x="Plant Species") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Plant, y = (emmean+SE+75), label = .group), position=position_dodge(width=0.8))
both.bio.fig



# Fig 1b #####
# Plant by virus figure

both.bio.cld <- cld(emmeans(final.bio.mod, ~ virus|Plant, adjust="none", type="response"), sort=FALSE, Letters=c("abc"))
both.bio.cld

# edit emmean object so it makes ggplot happy
both.bio.cld$.group=gsub(" ", "", both.bio.cld$.group)
both.bio.cld$emmean <- both.bio.cld$response

#make ggplot figure based on post hoc tests above
# hmm how to fix the fact alfalfa isnt there? partition without viruses?

both.bio.fig <- ggplot(both.bio.cld, aes(x=Plant, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid count at one week", x="Plant Species") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Plant, y = (emmean+SE+75), label = .group), position=position_dodge(width=0.8))
both.bio.fig

