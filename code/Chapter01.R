# 1.5 #####################################################################

# sperm.comp1 analysis
library(gamair)
data(sperm.comp1)
str(sperm.comp1)

pairs(sperm.comp1[,-1])
library(car)
scatterplotMatrix(sperm.comp1[,-1], diagonal = "histogram")
sc.mod1 <- lm(count~time.ipc+prop.partner,sperm.comp1)
summary(sc.mod1)
AIC(sc.mod1)

par(mfrow=c(2,2))
plot(sc.mod1)
par(mfrow=c(1,1))

sc.mod2 <- lm(count~time.ipc+I(prop.partner*time.ipc),sperm.comp1)
summary(sc.mod2)
AIC(sc.mod2)

par(mfrow=c(2,2))
plot(sc.mod2)
par(mfrow=c(1,1))

sc.mod3 <- lm(count~prop.partner,sperm.comp1)
summary(sc.mod3)
AIC(sc.mod3)

sc.mod4 <- lm(count~1,sperm.comp1)

AIC(sc.mod1, sc.mod2, sc.mod3, sc.mod4)

# sperm.comp2 analysis
data("sperm.comp2")
summary(sperm.comp2)
scatterplotMatrix(sperm.comp2[,-1], diagonal = "histogram")

sc2.mod1<-lm(count~f.age+f.height+f.weight+m.age+m.height+m.weight+m.vol,sperm.comp2)
summary(sc2.mod1)
par(mfrow=c(2,2))
plot(sc2.mod1)
par(mfrow=c(1,1))

sc2.complete <- complete.cases(sperm.comp2)
sc2.mod1.complete<-lm(count~f.age+f.height+f.weight+m.age+m.height+m.weight+m.vol,sperm.comp2[sc2.complete,])
summary(sc2.mod1.complete)
par(mfrow=c(2,2))
plot(sc2.mod1.complete)
par(mfrow=c(1,1))

AIC(sc2.mod1, sc2.mod1.complete)
# drop the term with the worse p value from the original model
sc2.mod2<-lm(count~f.age+f.height+f.weight+m.height+m.weight+m.vol,sperm.comp2)
summary(sc2.mod2)
par(mfrow=c(2,2))
plot(sc2.mod2)
par(mfrow=c(1,1))

sc2.mod3<-lm(count~f.age+f.height+f.weight+m.height+m.weight,sperm.comp2)
summary(sc2.mod3)
par(mfrow=c(2,2))
plot(sc2.mod3)
par(mfrow=c(1,1))

sc2.mod4<-lm(count~f.age+f.height+f.weight+m.height,sperm.comp2)
summary(sc2.mod4)
par(mfrow=c(2,2))
plot(sc2.mod4)
par(mfrow=c(1,1))

sc2.mod5<-lm(count~f.age+f.height+f.weight,sperm.comp2)
summary(sc2.mod5)
par(mfrow=c(2,2))
plot(sc2.mod5)
par(mfrow=c(1,1))

sc2.mod6<-lm(count~f.age+f.weight,sperm.comp2)
summary(sc2.mod6)
par(mfrow=c(2,2))
plot(sc2.mod6)
par(mfrow=c(1,1))

sc2.mod7<-lm(count~f.weight,sperm.comp2)
summary(sc2.mod7)
par(mfrow=c(2,2))
plot(sc2.mod7)
par(mfrow=c(1,1))

# Observation 9 and 19 are outliers;
# remove 19 first and repeat the analysis process

# 1.6 #####################################################################
data("PlantGrowth")
str(PlantGrowth)
pgm.1 <- lm(weight ~ group, data=PlantGrowth)
summary(pgm.1)
par(mfrow=c(2,2))
plot(pgm.1)
par(mfrow=c(1,1))
model.matrix(pgm.1)

pgm.0 <- lm(weight ~ 1, data=PlantGrowth)
anova(pgm.0, pgm.1)
AIC(pgm.0, pgm.1)
