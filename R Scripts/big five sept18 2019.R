
# code updated by rob 9/10
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MASS")
library("dplyr")
library("effects")
library("sjPlot")

# Data #####
bf <- read.csv("./Data/big five.csv", header = TRUE)
str(bf)

bf$Genotype <- as.factor(bf$Genotype)

# Outline #####

# Due to very low infection rate (effectively zero) BLRV is removed from this study and will be pilot data

bf <- subset(bf, virus != "BLRV")

# attempt to remove alfalfa since it cannot be PEMV+
# bf <- subset(bf, Plant != "Alfalfa")

# Fig 1 ####
# Fig 1 should be aphid performance for sham and PEMV (BLRV excluded)

both.bio.mod <- glm.nb(Counts ~ Biotype*Plant*virus + Run, data=bf)
summary(both.bio.mod)
Anova(both.bio.mod)

#planned contrast with things grouped by virus, plant, then biotype
both.bio.cld <- cld(emmeans(both.bio.mod, ~ Biotype|Plant|virus, adjust="none", type="response"), sort=FALSE, Letters=c("abc"))
both.bio.cld

# edit emmean object so it makes ggplot happy
both.bio.cld$.group=gsub(" ", "", both.bio.cld$.group)
both.bio.cld$emmean <- both.bio.cld$response

#make ggplot figure based on post hoc tests above

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
  geom_text(aes(x = Plant, y = (emmean+SE+75), label = .group), position=position_dodge(width=0.8)) +
  facet_wrap( ~ virus, nrow=2)
both.bio.fig

# Fig 2 ####
# Fig 2 should be differences in preference amoung biotypes tested
# match analysis to the one in Fig 1

settle.dat <- read.csv("./Data/settle mod.csv", header= TRUE)
settle.dat <- subset(settle.dat, Virus != "BLRV")

# attempt to remove alfalfa since it cannot be PEMV+
# settle.dat <- subset(settle.dat, Plant_sp != "AL")

# attempted to remove feb time block but that did not help
#settle.dat <- subset(settle.dat, month != "Feb")

new.count.mod <- glm(Counts ~ Biotype*Plant_sp*Virus, data=settle.dat)
Anova(new.count.mod)

count.lsm <- cld(emmeans(new.count.mod, ~ Biotype|Plant_sp|Virus), sort=FALSE, adjust="none", type="response")
count.lsm

#edit groupings so they line up better in ggplot
count.lsm$.group=gsub(" ", "", count.lsm$.group)

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
  geom_text(aes(x = Plant_sp, y = (emmean+SE+2), label = .group), position=position_dodge(width=0.8)) +
  facet_wrap( ~ Virus, nrow=2)
count.lsm.fig



# Fig 3 ####
# Fig 3 should be preference by performance assay with BLRV excluded. perhaps two models can be run, comparing 23, and 35

# make a data frame that has the lsm/cld for preference and performance
pxp <- as.data.frame(count.lsm)
pxp

#compare to order from both.bio.cld (performance assay)
bbc.dat <- as.data.frame(both.bio.cld)
bbc.dat


pxp$performance <- bbc.dat$response
pxp$preference <- count.lsm$emmean
pxp

# glm for preference by performance

pxp.glm <- glm(preference ~ performance*Virus, data=pxp)
Anova(pxp.glm)

plot_model(pxp.glm, type="int", ci.lvl = 0.90)

# Fig S1: pref PCR #####
choice.pcr <- read.csv("./Data/test choice pcr.csv", header = TRUE)
str(choice.pcr)


#choice glm to see if treatment increases likelihood of infection
choice.pcr.PEMV <- glm(PEMV ~ Virus, family=binomial, data=choice.pcr)
#not promising at all
Anova(choice.pcr.PEMV)
summary(choice.pcr.PEMV)

choice.pemv.lsm <- cld(emmeans(choice.pcr.PEMV, ~ Virus), type="response")
choice.pemv.lsm

choice.pemv.fig <- ggplot(choice.pemv.lsm, aes(x=Virus, y=prob)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=prob-(SE), ymax=prob+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Probability of PEMV infection in choice assay", x="Virus") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
#ylim(0,7.5) +
#facet_wrap( ~ virus, nrow=3)
choice.pemv.fig

#can run, but one positive in a sham treatment does not really instill confidence
choice.pcr.BLRV <- glm(BLRV ~ Virus, family=binomial, data=choice.pcr)
Anova(choice.pcr.BLRV)
summary(choice.pcr.BLRV)

# test to see if positive, negative, or sham pemv alters behavior
choice.pcr.2 <- subset(choice.pcr, Virus != "BLRV")

test.count.mod <- glm(Aphid.counts ~ PEMV.test*Plant_sp, data=choice.pcr.2)
Anova(test.count.mod)

holy.lsm <- cld(emmeans(test.count.mod, ~ PEMV.test|Plant_sp), adjust="none")
holy.lsm

# test to see if preference thingy can work this way

pref.mod.3 <- glm(Aphid.counts ~ PEMV.test*Plant_sp*Biotype, data=choice.pcr.2)
Anova(pref.mod.3)

# nah, it doent look like this will work. there isnt any positive hits for most combinations of biotype*plant species
holy.lsm.3 <- cld(emmeans(pref.mod.3, ~ PEMV.test|Plant_sp|Biotype), adjust="none")
holy.lsm.3

# doesnt look like 

#Fig S2: perf PCR #####
repro.pcr <-read.csv("./Data/test reproduction pcr.csv", header=TRUE)
repro.pcr


#pemv infection higher here
repro.pcr.PEMV <- glm(PEMV ~ Virus, family=binomial, data=repro.pcr)
Anova(repro.pcr.PEMV)
summary(repro.pcr.PEMV)

repro.pcr.lsm <- cld(emmeans(repro.pcr.PEMV, ~ Virus), type="response")


repro.pemv.fig <- ggplot(repro.pcr.lsm, aes(x=Virus, y=prob)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=prob-(SE), ymax=prob+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Probability of PEMV infection in reproduction assay", x="Virus") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))
repro.pemv.fig




# Fig S3 ####
# Fig S3 should show that aphid abundance for infective aphids vs. positive PCR infection are more similar than either to controls


# requires cleaned data from Logan Dean made on sept 12

bflc <- read.csv("./Data/big five Logan check.csv", header = TRUE)
str(bflc)

bf$Genotype <- as.factor(bf$Genotype)

# make all blanks NA
bflc[bflc==""] <- NA

# exclude all but block 3 (because that has pcr tests)

bflc.mod <- glm(Counts ~ PEMV.PCR, data=bflc)
summary(bflc.mod)

cld(emmeans(bflc.mod, ~ PEMV.PCR))

#run again excluded BLRV to solve some model problems and include host plant species

bflc.2 <- subset(bflc, virus != "BLRV")


bflc.mod.2 <- glm(Counts ~ PEMV.PCR*Plant, data=bflc.2)
summary(bflc.mod.2)

cld(emmeans(bflc.mod.2, ~ Plant|PEMV.PCR), adjust="none")







# Old Code Below #####
three.mod <- glm(Counts ~ Plant*virus*Biotype, family=poisson, data=bf)
Anova(three.mod)
summary(three.mod)

plot_model(three.mod, type="int", ci.lvl = 0.67)
hist.count <- hist(settle.dat$Counts)
#

# ggsave(file="test.svg", plot=hist.count, width=10, height=8)

# in pooled biotype model (alfalfa and pea biotypes) use host association as fixed effect and genotype as random
# do model selection to see if random effect has important contibution to variation not explained by biotype


# diagnostic tests for bean aphid effects

bean.glm <- glm(Bean.Aphids ~ Plant + virus, data=bf)
summary(bean.glm)
Anova(bean.glm)

# conclusion is that bean aphids were not impacted by virus, but varied by host plant

#full model on aphid abundance at 1 week (verify date)
mod_1 <- glm.nb(Counts ~ Run, data=bf)
summary(mod_1)
Anova(mod_1)
cld(emmeans(mod_1, ~ Run))

hist(bf$Counts)
hist(log(bf$Counts+1))

#split model

mod_1 <- glm.nb(Counts ~ Biotype*Plant + Run, data=bf)
summary(mod_1)

#run has no significant effect on comparing 1 to 3... strange

mod_2 <- glm.nb(Counts ~ Plant*virus + Run,data=bf)
summary(mod_2)


#AIC = 7525 without third order interaction
#AIC = 7534 with third order interacion
#AIC with neg bin is 6280!
dev.table.1 <- Anova(mod_2, type ="II")
dev.table.1

as.data.frame(dev.table.1) 

mod_3 <- glm.nb(Counts ~ Biotype*virus + Run, data=bf)
summary(mod_3)
Anova(mod_3)

#code to edit

bf.lsm <- emmeans(mod_1, ~ Biotype|Plant, adjust="none")
bf.lsm
bf.cld <- cld(bf.lsm, sort = FALSE, adjust="none", type="response")
bf.cld$LSD <- as.numeric(bf.cld$.group)
bf.cld

as.data.frame(bf.cld)

bf.lsm2 <- emmeans(mod_2, ~ virus|Plant, adjust="none")
bf.cld2 <- cld(bf.lsm2, sort=FALSE)

as.data.frame(bf.cld2)

bf.lsm3 <- emmeans(mod_3, ~ Biotype|virus, adjust="none")
cld.lsm3 <- cld(bf.lsm3, sort=FALSE)
cld.lsm3

#do a model with just vetch
# Vetch #########

vetch.dat <-subset(bf, Plant == "Hairy Vetch")
vetch.dat

vetch.mod <- glm.nb(Counts ~ Genotype*virus + Run, data=vetch.dat)
summary(vetch.mod)

vetch.cld <- cld(emmeans(vetch.mod, ~ Genotype|virus, adjust="tukey", type="response"), sort=FALSE)
vetch.cld

vetch.cld$emmean <- vetch.cld$response
vetch.cld$.group <- as.numeric(vetch.cld$.group)

vetch.five.fig <- ggplot(vetch.cld, aes(x=Genotype, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week On Hairy Vetch", x="Genotype") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Genotype, y = (emmean+SE+100), label = .group)) +
  facet_wrap( ~ virus, nrow=3)

vetch.five.fig

# vetch.table <- as.data.frame(Anova(vetch.mod))
# vetch.table
# write.csv(vetch.table, file = "vetch.csv")

#ok. virus alters the competitive dynamics on weedy hosts. biotypes that would perform poorly without virus do great, etc. etc.

# Vetch (biotypes only) #####

vetch.mod.2 <- glm.nb(Counts ~ Biotype*virus + Run, data=vetch.dat)
summary(vetch.mod.2)

vetch.cld <- cld(emmeans(vetch.mod, ~ Biotype|virus, adjust="tukey", type="response"), sort=FALSE)
vetch.cld

vetch.cld$emmean <- vetch.cld$response
vetch.cld$.group <- as.numeric(vetch.cld$.group)

vetch.three.fig <- ggplot(vetch.cld, aes(x=Biotype, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week On Hairy Vetch", x="Biotype") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Biotype, y = (emmean+SE+100), label = .group)) +
  facet_wrap( ~ virus, nrow=3)

vetch.three.fig


### model with just pea #####

pea.dat <-subset(bf, Plant == "Pea")
str(pea.dat)

pea.mod <- glm.nb(Counts ~ Genotype*virus, data=pea.dat)
summary(pea.mod)

pea.cld <- cld(emmeans(pea.mod, ~ Genotype|virus, adjust="tukey", type="response"), sort=FALSE)
pea.cld
#figure with just pea

pea.cld$emmean <- pea.cld$response
pea.cld$.group <- as.numeric(pea.cld$.group)
pea.cld

pea.table <- as.data.frame(Anova(pea.mod))
pea.table

# write.csv(pea.table, file = "pea.csv")
  
pea.five.fig <- ggplot(pea.cld, aes(x=Genotype, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week On Pea", x="Genotype") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Genotype, y = (emmean+SE+400), label = .group)) +
  facet_wrap( ~ virus, nrow=3)

pea.five.fig





#alfalfa #####

Alfalfa.dat <-subset(bf, Plant == "Alfalfa" & virus != "PEMV") %>%
  mutate(virus = as.character(virus), Plant = as.character(Plant))

str(Alfalfa.dat)

Alfalfa.mod <- glm.nb(Counts ~ Genotype + virus, data=Alfalfa.dat)
summary(Alfalfa.mod)

alfalfa.cld <- cld(emmeans(Alfalfa.mod, ~ Genotype|virus, adjust="tukey", type="response"), sort=FALSE)
alfalfa.cld

alfalfa.cld$emmean <- alfalfa.cld$response
alfalfa.cld$.group <- as.numeric(alfalfa.cld$.group)

alfalfa.five.fig <- ggplot(alfalfa.cld, aes(x=Genotype, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week On Alfalfa", x="Genotype") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Genotype, y = (emmean+SE+100), label = .group)) +
  facet_wrap( ~ virus, nrow=3)

alfalfa.five.fig


# faba ######

Faba.dat <-subset(bf, Plant == "Faba")
Faba.dat

Faba.mod <- glm.nb(Counts ~ Genotype*virus + Run, data=Faba.dat)
summary(Faba.mod)
Anova(Faba.mod)

faba.cld <- cld(emmeans(Faba.mod, ~ Genotype|virus, adjust="tukey", type="response"), sort=FALSE)
faba.cld

faba.cld$emmean <- faba.cld$response
faba.cld$.group <- as.numeric(faba.cld$.group)

faba.five.fig <- ggplot(faba.cld, aes(x=Genotype, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week On Faba", x="Genotype") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Genotype, y = (emmean+SE+100), label = .group)) +
  facet_wrap( ~ virus, nrow=3)

faba.five.fig


# clover #####

Red.Clover.dat <-subset(bf, Plant == "Red Clover")
Red.Clover.dat

Red.Clover.mod <- glm.nb(Counts ~ Genotype*virus + Run, data=Red.Clover.dat)
summary(Red.Clover.mod)

red.clover.cld <- cld(emmeans(Red.Clover.mod, ~ Genotype|virus, adjust="tukey", type="response"), sort=FALSE)
red.clover.cld

red.clover.cld$emmean <- red.clover.cld$response
red.clover.cld$.group <- as.numeric(red.clover.cld$.group)

red.clover.five.fig <- ggplot(red.clover.cld, aes(x=Genotype, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week On Red clover", x="Genotype") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Genotype, y = (emmean+SE+100), label = .group)) +
  facet_wrap( ~ virus, nrow=3)

red.clover.five.fig


# lentil ######

Lentil.dat <-subset(bf, Plant == "Lentil")
Lentil.dat

Lentil.mod <- glm.nb(Counts ~ Genotype*virus, data=Lentil.dat)
summary(Lentil.mod)

lentil.cld <- cld(emmeans(Lentil.mod, ~ Genotype|virus, adjust="tukey", type="response"), sort=FALSE)
lentil.cld

lentil.cld$emmean <- lentil.cld$response
lentil.cld$.group <- as.numeric(lentil.cld$.group)

lentil.five.fig <- ggplot(lentil.cld, aes(x=Genotype, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.1) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week On Lentil", x="Genotype") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  geom_text(aes(x = Genotype, y = (emmean+SE+100), label = .group)) +
  facet_wrap( ~ virus, nrow=3)

lentil.five.fig



#others figure
bf.fig <- ggplot(bf.cld, aes(x=Plant, y=emmean, fill=Biotype)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week", x="Host Plant") + 
  labs(fill="Aphid Biotype") +
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  facet_wrap( ~ virus, nrow=3)
bf.fig








#others figure (all combined?)
# i wouldnt keep this except for visualization

bf.fig <- ggplot(bf.cld, aes(x=Plant, y=emmean, fill=Biotype)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week", x="Host Plant") + 
  labs(fill="Aphid Biotype") +
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  facet_wrap( ~ virus, nrow=3)
bf.fig




# diet-breadth both ####
#one way to test the hypothesis is to see if the plant effect is significant in the absence of virus and insignificant in the presence
####  analysis above with plant*virus split up by each genotype 

sanford.lsm <- emmeans(mod_2, ~ Plant|virus)
sanford.cld <- cld(sanford.lsm, sort=FALSE)
sanford.cld
sanford.cld$tukey <- as.numeric(sanford.cld$.group)

plant.by.virus.fig <- ggplot(sanford.cld, aes(x=Plant, y=emmean, fill=virus)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Aphid Count at One Week", x="Host Plant") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ylim(0,7.5) +
    facet_wrap( ~ virus, nrow=3)
plant.by.virus.fig



# preference analysis ######
#settling behavior
settle.dat <- read.csv("./Data/settle mod.csv", header= TRUE)
settle.dat <- subset(settle.dat, Virus != "BLRV")

settle.dat$Biotype <- as.factor(settle.dat$Biotype)

settle.mod <- glm(Proportion_living ~ Plant_sp*Biotype*Virus, data=settle.dat)

plot_model(settle.mod, type="int", ci.lvl=0.9)

count.mod <- glm(Counts ~ Plant_sp*Virus*Biotype + Fresh.Weight*Biotype, family=poisson, data=settle.dat)
Anova(count.mod)

#why is fresh weight by biotype necessary?
plot_model(count.mod, type="int", ci.lvl = 0.9)


count.lsm <- cld(emmeans(count.mod, ~ Plant_sp*Virus|Biotype), sort=FALSE, adjust="none", type="response")
count.lsm

#figures similar to reproduction, but looking at performance across biotypes (3 genotypes)
b2.dat <-subset(settle.dat, Biotype == "2")

b2.compare.mod <- glm(Counts ~ Virus*Plant_sp, data=b2.dat)
Anova(b2.compare.mod)
plot_model(b2.compare.mod, type="int")

b3.dat <-subset(settle.dat, Biotype == "3")

b3.compare.mod <- glm(Counts ~ Virus*Plant_sp, data=b3.dat)
Anova(b3.compare.mod)
plot_model(b3.compare.mod, type="int")

b5.dat <-subset(settle.dat, Biotype == "5")

b5.compare.mod <- glm(Counts ~ Virus*Plant_sp, data=b5.dat)
Anova(b5.compare.mod)
plot_model(b5.compare.mod, type="int")













# pref by perf #######
##### regression for preference x performance in two models with and without viruses
# part 1: just use parameter estimates estimated across all biotypes

#preference for 2,3,5 aphid biotypes (these were only genotypes in expeirment)
count.all.lsm <- cld(emmeans(count.mod, ~ Plant_sp*Virus), sort=FALSE, adjust="none")
count.all.lsm

#performance for all biotypes
# taken from an earlier glm with the big 5 dataset
# sketchy because its performance acros all 5 biotypes, while preference is on 2,3,5

perf.glm <- glm.nb(Counts ~ Plant*virus, data=bf)
Anova(perf.glm)

perf.lsm <- cld(emmeans(perf.glm, ~ Plant*virus), sort=FALSE, adjust="none")
perf.lsm

#model with 2,3,5 like the preferenace assay

bf235.dat <- filter(bf, Genotype != "1" & Genotype !="4")

perf.235.glm <- glm.nb(Counts ~ Plant*virus, data=bf235.dat)
Anova(perf.235.glm)

#post hoc tests with just 3 biotypes in entire model
perf.235.lsm <- cld(emmeans(perf.235.glm, ~ Plant*virus), sort=FALSE, adjust="none")
perf.235.lsm


#this somewhat works with ALL additional biotypes added (1,2,3,4,5)
count.lsm$performance <- perf.lsm$response

#add only 2 3 5 so its a propor comparison

count.lsm$performance235 <- perf.235.lsm$emmean


#add to preference model
count.lsm$preference <- count.lsm$emmean

count.lsm <-as.data.frame(count.lsm)

count.lsm

# no performance variable ########
pp.glm <- glm(performance ~ preference, data=count.lsm)
summary(pp.glm)
Anova(pp.glm)

# awesome! p-value is significant boss, but only if you run this on the first batch of biotypes
# there is a positive correlation between preference and performance!


# now run with just 2 3 5

pp.glm.a <- glm(performance235 ~ preference*Virus, data=count.lsm)
Anova(pp.glm.a)

#demo figure 0
plot_model(pp.glm.a, type="int", ci.lvl = 0.90)



# demo figure 1 (error structure and regression)
plot_model(pp.glm)
hist(count.lsm$performance)
hist(count.lsm$preference)


#is this modified by virus?

pp.virus.glm <- glm(performance ~ preference*Virus, data=count.lsm)
summary(pp.virus.glm)
Anova(pp.virus.glm)

#demo figure 2
plot_model(pp.virus.glm, type="int", ci.lvl = 0.90)





# try next: use parameter estimates for biotypes 2 and 5 or 3 and 5 (dont pool all biotypes for performance metric)
# there are many many permutations of this so discuss with sanford the best approach for preference x performance regression

# b5 pref by perf ####
# (copied from above)
b5.dat <-subset(settle.dat, Biotype == "5")

#preference for b5
b5.compare.mod <- glm(Counts ~ Virus*Plant_sp, data=b5.dat)
Anova(b5.compare.mod)
plot_model(b5.compare.mod, type="int")

count5.lsm <- cld(emmeans(b5.compare.mod, ~ Plant_sp*Virus), sort=FALSE, adjust="none")
count5.lsm


#subset to just 5
b5.dat.a <-subset(bf, Genotype == "5")

#performance for biotype (genotype) 5
b5.compare.mod.a <- glm(Counts ~ virus*Plant, data=b5.dat.a)
Anova(b5.compare.mod.a)
plot_model(b5.compare.mod.a, type="int")

perf5.lsm <- cld(emmeans(b5.compare.mod.a, ~ Plant*virus), sort=FALSE, adjust="none")
perf5.lsm




#this somewhat works with 5
count5.lsm$performance <- perf5.lsm$emmean
count5.lsm$preference <- count5.lsm$emmean
count5.lsm

pp5.glm <- glm(performance ~ preference, data=count5.lsm)
summary(pp5.glm)
Anova(pp5.glm)

plot_model(pp5.glm)









# PCR data ##########
#analysis of molecular data


#not a single plant was a positive hit for BLRV so this model doesnt even run (100% negative)
repro.pcr.BLRV <- glm(BLRV ~ Virus, family=binomial, data=repro.pcr)
Anova(repro.pcr.BLRV)




