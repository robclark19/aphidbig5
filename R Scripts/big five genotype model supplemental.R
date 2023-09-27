# Supplemental figures
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MASS")
library("tidyverse")
library("plotrix")


# data
bf <- read.csv("./Data/big five Logan check.csv") %>%
  mutate(Plant = recode(Plant, "Faba" = "Fava", "Red Clover" = "Red clover", "Hairy Vetch" = "Hairy vetch"))

bf$Genotype <- as.factor(bf$Genotype)
bf$Virus <- bf$virus

# genotype model (not used in manuscript, biotype is)
genotype_mod <- glm.nb(Counts ~ Genotype, data = bf)
Anova(genotype_mod)


genotype_cld <- cld(emmeans(genotype_mod, ~ Genotype, type="response"), sort=FALSE, Letters=c("abc"))
genotype_cld

# testing data model

# attempt to show results are not significantly different if using the core postive tests
# only look at data where were could prove plants were infected
bf2 <- bf %>% filter(Run != "Three", Retain. != "Drop")

#Final model (currently used in manuscript)
table1a_mod <- readRDS("./Models/table1_mod.rds")

# model with the confirmed entries only
final.bio.mod2 <- glm.nb(Counts ~ Biotype + Plant + Virus + Plant*Biotype + Plant*Virus, data = bf2)

Anova(final.bio.mod2)


# model comparison
anova(table1a_mod, final.bio.mod2)
# yes they are significantly different


# does infection status work better than virus treatment (but we drop the third rep ofc)
proof.mod <- glm.nb(Counts ~ Biotype + Plant + Infection.status + Plant*Biotype + Plant*Infection.status, data=bf2)

Anova(proof.mod)

proof.cld <- cld(emmeans(proof.mod, ~ Infection.status|Plant, type="response"), sort=FALSE, Letters=c("abc"))
proof.cld

# no sig differences among plants based on infection status
