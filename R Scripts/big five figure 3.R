
# Figure three looks at the relationship between aphid pref and perf and whether the virus disrupts this pattern

library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MASS")
library("tidyverse")
library("ggeffects")
library("effects")

# Preference by performance data loaded from prior scripts 
settle.dat <- read.csv("./Data/settle mod.csv", header= TRUE)
bf <- read.csv("./Data/big five Logan check.csv")

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

pxp_dat_full <- left_join(pxp_dat, pxp_dat_2, by = c("Plant", "Genotype", "Virus")) %>%
  subset(Avg_Count > 1) # for log transform

hist(pxp_dat_full$Avg_Count) #non-normal
hist(pxp_dat_full$Avg_Move) #normal

pxp_dat_full$Log_Count <- log(pxp_dat_full$Avg_Count)

pxp_glm <- glm(Avg_Move ~ Log_Count*Virus, data=pxp_dat_full)
Anova(pxp_glm)


# Save model for table s2
saveRDS(pxp_glm, "./Models/table3_mod.rds")

# messing with some basic models
pxp_fit <- ggpredict(pxp_glm, terms = c("Log_Count","Virus"))

ggplot(pxp_fit, aes(x, predicted, color=group)) + geom_line()


plot(log(pxp_dat_full$Avg_Count+1), pxp_dat_full$Avg_Move)

# Fig 3
# Relationship between aphid recruitment based on plants they perform best on
#The Interaction
dat_1 <- effect('Log_Count*Virus', pxp_glm, se=TRUE, xlevels=100) %>% as.data.frame()



#Create plot
Fig_3 <-ggplot(data=dat_1, aes(x=Log_Count, y=fit, group=Virus))+
  geom_line(size=1, aes(color=Virus))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=Virus),alpha=.2)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(text = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top") +
  #coord_cartesian(ylim = c(0, 18), xlim=c(2, 7)) +
  xlab("Aphid count at one week (log-transformed)") +
  ylab("Number of aphids that moved towards host plant") +
  geom_point(data=pxp_dat_full, aes(x=Log_Count, y=Avg_Move, colour=Virus))
Fig_3

ggsave(filename = "./Figures/Fig 4.png", plot = Fig_3, device = "png",
       width = 6, height = 5, units = "in")