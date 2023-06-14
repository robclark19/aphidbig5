
# Fig 1
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MASS")
library("tidyverse")
library("plotrix")

bf <- read.csv("./Data/big five Logan check.csv") %>%
  mutate(Plant = recode(Plant, "Faba" = "Fava", "Red Clover" = "Red clover", "Hairy Vetch" = "Hairy vetch"))

bf$Genotype <- as.factor(bf$Genotype)
bf$Virus <- bf$virus

# First model
both.bio.mod <- glm.nb(Counts ~ Biotype * Plant * Virus, data = bf)
summary(both.bio.mod)
Anova(both.bio.mod)

both.bio.aic <- stepAIC(both.bio.mod)
both.bio.aic$anova

#Final model
final.bio.mod <- glm.nb(Counts ~ Biotype + Plant + Virus + Plant*Biotype + Plant*Virus, data = bf)

Anova(final.bio.mod)

# Save model for table s1
saveRDS(final.bio.mod, "./Models/table1_mod.rds")
saveRDS(both.bio.mod, "./Models/table1b_mod.rds") #fully specified model


# Means and SE for biotype by plant

Fig1a <- bf %>% group_by(Biotype, Plant) %>%
  summarise(mean= mean(Counts), SEM = std.error(Counts, na.rm=TRUE)) %>% as.data.frame()



# Fig 1a ########
# Biotype by plant figure

both.bio.cld <- cld(emmeans(final.bio.mod, ~ Biotype|Plant, type="response"), sort=FALSE, Letters=c("abc"))
both.bio.cld

# edit emmean object so it makes ggplot happy
both.bio.cld$.group=gsub(" ", "", both.bio.cld$.group)
both.bio.cld$emmean <- both.bio.cld$response


# join cld with raw mean and se
Fig1a <- left_join(x=Fig1a, y=both.bio.cld, by = c("Biotype","Plant"))

# fix tukey letters "a" and "b" in alfalfa manually
Fig1a$.group <- ifelse(Fig1a$.group == "", c("b", "a")[cumsum(Fig1a$.group == "") %% 2 + 1], Fig1a$.group)

# make ggplot figure based on post hoc tests above
plot1_a <- ggplot(Fig1a, aes(x=Biotype, y=mean)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=mean-(SEM), ymax=mean+(SEM)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid count at one week", x="Aphid biotype grouping") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Biotype, y = (mean+SEM+50), label = .group), position=position_dodge(width=0.8)) +
  scale_x_discrete(labels=c("pea" = "Pea", "alf" = "Alfalfa")) +
  facet_wrap(~Plant) 
plot1_a

ggsave(filename = "./Figures/Fig 1.png", plot = plot1_a, device = "png",
       width = 6, height = 5, units = "in")


# Fig 1b #####
# Plant by virus figure

fig1b_cld <- cld(emmeans(final.bio.mod, ~ Virus|Plant, type="response"), sort=FALSE, Letters=c("abc"))
fig1b_cld

# toggle for fig 1c
# fig1b_cld <- cld(emmeans(final.bio.mod, ~ Plant|Virus, type="response"), adjust="none", sort=FALSE, Letters=c("abc"))


# edit emmean object so it makes ggplot happy
fig1b_cld$.group=gsub(" ", "", fig1b_cld$.group)
fig1b_cld$emmean <- fig1b_cld$response


# Means and SE for biotype by plant

Fig1b <- bf %>% group_by(Virus, Plant) %>%
  summarise(mean = mean(Counts), SEM = std.error(Counts, na.rm=TRUE)) %>% as.data.frame()

# join cld with raw mean and se
Fig1b <- left_join(x=Fig1b, y=fig1b_cld, by = c("Virus","Plant"))


# ggplot object

plot_1b <- ggplot(Fig1b, aes(x=Virus, y=mean)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=mean-(SEM), ymax=mean+(SEM)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid count at one week", x="Virus exposure status of host plant") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Virus, y = (mean+SEM+75), label = .group), position=position_dodge(width=0.8)) +
  facet_wrap(~Plant)
plot_1b

ggsave(filename = "./Figures/Fig 2.png", plot = plot_1b, device = "png",
       width = 6, height = 5, units = "in")


# Means and SE for biotype by plant

Fig1c <- bf %>% group_by(Virus) %>%
  summarise(mean = mean(Counts), SEM = std.error(Counts, na.rm=TRUE)) %>% as.data.frame()

Fig1c

plot_1c <- ggplot(Fig1c, aes(x=Virus, y=mean)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=mean-(SEM), ymax=mean+(SEM)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid count at one week", x="Virus exposure status of host plant") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
plot_1c

ggsave(filename = "./Figures/Fig S1.png", plot = plot_1c, device = "png",
       width = 4, height = 3, units = "in")

