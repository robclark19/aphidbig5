# Rob bioassay ANCOVA style approach ####
bioassaymod<-read.csv("benthamianabioassay_formodel.csv",stringsAsFactors=T)

# bioassaymod$Time<- as.factor(bioassaymod$Time)
bioassaymod$totals <- bioassaymod$Remaining + bioassaymod$Emigrating

ba_glm <- glm.nb(Remaining ~ Time*TREAT, weights=totals, data=bioassaymod)
Anova(ba_glm)

plot_model(ba_glm, type = "eff", ci.lvl = 0.67)
plot_model(ba_glm, type = "eff", terms = c("Time","TREAT"))
