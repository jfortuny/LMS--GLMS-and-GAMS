# GLMs

# Hubona Exercises Day 1, problem 3
dataVector <- c(0.1,0.5,1,10,20,30,50,70,80,100,150,
                7,1,10,9,2,9,13,1,1,4,3,
                0,0,3,4,0,6,7,0,0,1,7)
df <- data.frame(matrix(data = dataVector, byrow = FALSE, ncol = 3))
names(df) <- c("conc", "NO", "YES")
df

df$prop <- df$YES/(df$NO + df$YES)
df
plot(df$conc, df$prop)

m0 <- glm(data = df, formula = as.matrix(df[,c("YES","NO")]) ~ conc, family = binomial)
m1 <- glm(data = df, formula = cbind(df$YES, df$NO) ~ conc, family = binomial)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

m2 <- glm(data = df, formula = cbind(df$YES, df$NO) ~ conc + I(conc^2) + I(conc^3), family = binomial)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))

anova(m1,m2, test="Chisq")

# Hubona Exercises Day 1, problem 4
library(DAAG)
data(ACF1)
ACF1
plot(count~endtime,data=ACF1)

m1 <- glm(data = ACF1, formula = count~endtime, family = poisson)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

m2 <- glm(data = ACF1, formula = count~endtime + I(endtime^2), family = poisson)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))

anova(m1,m2,test="Chisq")
