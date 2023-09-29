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


# percentages of exposed plants that were actually infected
infection_percentages <- bf %>%
  filter(virus != "Sham") %>%
  group_by(virus) %>%
  summarize(
    Total_Plants = n(),
    Successful_Infections = sum(Infection.status == "YES"),
    Percentage_Successful = (Successful_Infections / Total_Plants) * 100
  )
infection_percentages

# try to split among plant species
# Calculate the percentages of successful infections by plant species
infection_percentages_by_species <- bf %>%
  filter(virus != "Sham") %>%
  group_by(Plant, virus) %>%
  summarize(
    Total_Plants = n(),
    Successful_Infections = sum(Infection.status == "YES"),
    Percentage_Successful = (Successful_Infections / Total_Plants) * 100
  )

infection_percentages_by_species 

# attempt to show results are not significantly different if using the core postive tests
# only look at data where were could prove plants were infected
bf2 <- bf %>% filter(Retain. != "Drop")

#Final model (currently used in manuscript)
table1a_mod <- readRDS("./Models/table1_mod.rds")

# model with the confirmed entries only
final.bio.mod2 <- glm.nb(Counts ~ Biotype + Plant + Virus + Plant*Biotype + Plant*Virus, data = bf2)

Anova(final.bio.mod2)


# model comparison
anova(table1a_mod, final.bio.mod2)
# yes they are significantly different


# does infection status work better than virus treatment (but we drop the third rep ofc)
proof.mod <- glm.nb(Counts ~ Biotype + Plant + Virus + Plant*Biotype + Plant*Virus, data=bf2)

Anova(proof.mod)

proof.cld <- cld(emmeans(proof.mod, ~ Virus|Plant, type="response"), sort=FALSE, Letters=c("abc"))
proof.cld

# very similar conclusion with exception of red clover

FigS2 <- bf2 %>% group_by(Virus, Plant) %>%
  summarise(mean = mean(Counts), SEM = std.error(Counts, na.rm=TRUE)) %>% as.data.frame()

# join cld with raw mean and se
FigS2 <- left_join(x=FigS2, y=proof.cld, by = c("Virus","Plant"))

plot_S2 <- ggplot(FigS2, aes(x=Virus, y=mean)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=mean-(SEM), ymax=mean+(SEM)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid count at one week", x="Verified virus status of host plant") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Virus, y = (mean+SEM+75), label = .group), position=position_dodge(width=0.8)) +
  facet_wrap(~Plant)
plot_S2

ggsave(filename = "./Figures/Fig S2.png", plot = plot_S2, device = "png",
       width = 6, height = 5, units = "in")
