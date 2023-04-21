
# Figure three looks at the relationship between aphid pref and perf and whether the virus disrupts this pattern

library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MASS")
library("tidyverse")
library("ggeffects")

# Preference by performance data loaded from prior scripts 
str(settle.dat)
str(bf)

pxp_dat <- settle.dat %>%
  group_by(Plant_sp, Genotype, Biotype, Virus) %>%
  summarize(Avg_Move = mean(Counts), Total_Move = sum(Counts)) %>%
  mutate(Plant_sp = recode(Plant_sp, "Fava" = "Faba")) %>%
  mutate(Virus = recode(Virus, "SHAM" = "Sham")) %>%
  rename(Plant = Plant_sp) %>%
  mutate(Genotype = as.factor(Genotype))

pxp_dat_2 <- bf %>%
  group_by(Plant, Genotype, Biotype, virus) %>%
  summarize(Avg_Count = mean(Counts), Total_Count = sum(Counts)) %>%
  rename(Virus = virus)  %>%
  mutate(Biotype = recode(Biotype, "alf" = "Alfalfa")) %>%
  mutate(Biotype = recode(Biotype, "pea" = "Pea")) %>%
  mutate(Genotype = as.factor(Genotype))

pxp_dat_full <- left_join(pxp_dat, pxp_dat_2, by = c("Plant", "Biotype", "Virus"))

pxp_only <- glm(Total_Move ~ Total_Count, data=pxp_dat_full)
Anova(pxp_only)

pxp_glm <- glm(Total_Move ~ Total_Count * Virus, data=pxp_dat_full)
Anova(pxp_glm)

# Save model for table s2
saveRDS(pxp_glm, "./Models/table3_mod.rds")

dat <- ggpredict(pxp_glm, terms = c("Total_Count", "Virus"))

ggplot(dat, aes(x, predicted, color=group)) + geom_line()

# Fig 3
# Relationship between aphid recruitment based on plants they perform best on


