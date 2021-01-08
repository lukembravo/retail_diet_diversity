data <- read.csv("/Users/LukeBravo/Documents/• School/2020-21 UT Austin, McCombs/• Fall 2020/Marketing Analytics/Group Project/Diet Diversity/rd4dd_census_clean.csv")
data_abrg <- data

#linear model to test and see which coefs are significant
lm <- lm(OP_time ~ ., data = data_abrg)
summary(lm)

#correlation matrix
res <- cor(data_abrg)
round(res, 2)

# multinomial log reg
library(nnet)

test <- multinom(OP_time ~ as.factor(OILF) + as.factor(ORGANM) + as.factor(Shop_type) + Latitude + Longitude + as.factor(OUT_CER) + as.factor(MILK) , data = data_abrg)
summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(test))
options(scipen=999)

probability.table <- round(fitted(test), digits = 4)
probability.table

library(ggplot2)

ggplot(data_abrg, aes(x=as.factor(data_abrg$OP_time), y=data_abrg$Per_ultra)) + 
  geom_boxplot()

ggplot(data_abrg, aes(x=as.factor(data_abrg$OP_time), y=data_abrg$Shop_type)) + 
  geom_boxplot()

ggplot(data_abrg, aes(x=as.factor(OP_time), y=Ready_food)) + 
  geom_boxplot()
