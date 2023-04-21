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

settle.dat$Plant <- settle.dat$Plant_sp

# First model

new.count.mod <- glm.nb(Counts ~ Biotype*Plant*Virus, data=settle.dat)

mod1_sa <- stepAIC(new.count.mod, direction = "both")

mod1_sa$anova

# Final model

final.count.mod <- glm.nb(Counts ~ Biotype + Plant + Plant*Biotype, data=settle.dat)

Anova(final.count.mod)

# Save model for table s2
saveRDS(final.count.mod, "./Models/table2_mod.rds")

# get emmeans for posthoc tests
count.lsm <- cld(emmeans(final.count.mod, ~ Biotype|Plant), sort=FALSE, type="response", Letters=c("abc")) %>% as.data.frame()

#edit groupings so they line up better in ggplot
count.lsm$.group=gsub(" ", "", count.lsm$.group) 

#
Fig2 <- settle.dat %>% group_by(Biotype, Plant) %>%
  summarise(mean = mean(Counts), SEM = std.error(Counts, na.rm=TRUE)) %>% as.data.frame()

# join cld with raw mean and se
Fig2 <- left_join(x=Fig2, y=count.lsm, by = c("Biotype","Plant"))

#

#make figure for counts of aphids that move to each host plant
Fig2_plot <- ggplot(Fig2, aes(x=Biotype, y=mean)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=mean-(SEM), ymax=mean+(SEM)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Number of aphids that moved towards host plant", x="Aphid genotype grouping") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Biotype, y = (mean+SEM+2), label = .group), position=position_dodge(width=0.8)) +
  facet_wrap(~Plant)
Fig2_plot


ggsave(filename = "./Figures/Fig 3.png", plot = Fig2_plot, device = "png",
       width = 6, height = 5, units = "in")

