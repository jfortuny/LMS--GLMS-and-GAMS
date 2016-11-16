# 2.3.1 #####################################################################
# Heart attack and binomial
heart <- read.csv("./1. Materials Linear Modeling and GLMs/data/heart.csv",
                  stringsAsFactors = FALSE)
heart$pHA <- heart$ha / (heart$ha + heart$ok)

plot(heart$ck, heart$pHA,
     xlab = "Creatinine kinase level",
     ylab = "Proportion of Heart Attack")

library(ggplot2)
ggplot(data = heart, aes(x=ck, y=pHA)) +
  geom_point() +
  ylab ("Proportion of Heart Attack") +
  xlab ("Creatinine kinase level")

mod.0 <- glm(cbind(ha,ok)~ck, data = heart, family = binomial)
mod.0
op <- par(mfrow=c(2,2))
plot(mod.0)
par(mfrow=c(1,1))
plot(heart$ck, heart$pHA,
     xlab = "Creatinine kinase level",
     ylab = "Proportion of Heart Attack")
lines(heart$ck, fitted(mod.0))

mod.2 <- glm(cbind(ha,ok)~ck + I(ck^2) + I(ck^3),
             data = heart,
             family = binomial)
mod.2
plot(heart$ck, heart$pHA,
     xlab = "Creatinine kinase level",
     ylab = "Proportion of Heart Attack")
lines(heart$ck, fitted(mod.2))

anova(mod.0, mod.2, test="Chisq")

# 2.3.2 #####################################################################
# AIDS epidemic and Poisson
aids <- c(12,14,33,50,67,74,123,141,165,204,253,246,240)
t <- 1:13
plot(t+1980,aids,xlab="Year",ylab="New AIDS cases",ylim=c(0,280))
aidsCases <- data.frame(cbind(t,aids))
ggplot(data = aidsCases, aes(x=t, y=aids)) +
  geom_point() +
  xlab("Year") +
  ylab("New AIDS cases") +
  ylim(0,280) +
  ggtitle("AIDS Cases in Belgium 1982 to 1992")

m0 <- glm(aids~t, poisson)
m0
par(mfrow=c(2,2))
plot(m0)
par(mfrow=c(1,1))

m1 <- glm(aids ~ t + I(t^2), poisson)
m1
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))
summary(m1)

anova(m0,m1,test="Chisq")

# plot confidence intervals for the estimated values
new.t <- seq(1,13, length = 100)
fv <- predict(m1, data.frame(t=new.t), se=TRUE)
# plot original data
plot(t+1980,aids,xlab="Year",ylab="New AIDS cases",ylim=c(0,280))
# add the fitted values
lines(new.t+1980, exp(fv$fit))
# add the confidence intervals
lines(new.t+1980, exp(fv$fit + 2*fv$se.fit), lty=2)
lines(new.t+1980, exp(fv$fit - 2*fv$se.fit), lty=2)

# 2.3.3 #####################################################################
# Log-linear model for categorical data
al<-data.frame(y=c(435,147,375,134),
               gender=as.factor(c("F","F","M","M")),
               faith=as.factor(c(1,0,1,0)))
al

mod.0 <- glm(y ~ gender+faith, data=al, family = poisson)
mod.0
summary(mod.0)
par(mfrow=c(2,2))
plot(mod.0)
par(mfrow=c(1,1))

mod.1 <- glm(y ~ gender*faith, data=al, family = poisson)
mod.1
summary(mod.1)
par(mfrow=c(2,2))
plot(mod.1)
par(mfrow=c(1,1))

anova(mod.0, mod.1, test="Chisq")
