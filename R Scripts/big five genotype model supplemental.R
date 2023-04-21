# genotype model
genotype_mod <- glm.nb(Counts ~ Genotype, data = bf)
Anova(genotype_mod)


genotype_cld <- cld(emmeans(genotype_mod, ~ Genotype, type="response"), sort=FALSE, Letters=c("abc"))
genotype_cld
