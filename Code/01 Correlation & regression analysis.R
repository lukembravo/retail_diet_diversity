data <- read.csv("C:/Users/jocel/OneDrive/2020_3_Fall/MIS 382N - Marketing Analysis/Project/rd4dd_census_clean.csv")
data_abrg <- data
#data$store_age_binary <- if_else(data$OP_time < 4, 0, 1)

#data$OP_time <- NULL

#linear model to test and see which coefs are significant
lm <- lm(store_age_binary ~ ., data = data)
summary(lm)

#correlation matrix
res <- cor(data_abrg)
round(res, 2)

# multinomial log reg
library(nnet)
library(MASS)

train <- read.csv("C:/Users/jocel/OneDrive/2020_3_Fall/MIS 382N - Marketing Analysis/Project/train.csv")
test <- read.csv("C:/Users/jocel/OneDrive/2020_3_Fall/MIS 382N - Marketing Analysis/Project/test.csv")

# binary logit
#logitMod <- glm(store_age_binary ~  ., data=train, family=binomial(link="logit"))
#summary(logitMod)
#PseudoR2(logitMod, which = "all")

#predicted <- plogis(predict(logitMod, test))  # predicted scores
# or
#predicted <- predict(logitMod, testData, type="response") 

lin_reg <- lm(OP_time ~ ., data = train)
summary(lin_reg)

#ordered logistic regression
m <- polr(as.factor(OP_time) ~ as.factor(District) + Latitude + Longitude +
            as.factor(Ultra_food) + Per_ultra + as.factor(Ready_food) + as.factor(CERE) +
            as.factor(WHITE) + as.factor(VITAMINV) + as.factor(VITAMINRT) + as.factor(DARKV) +
            as.factor(OTHERV) + as.factor(VITAMINF) + as.factor(OTHERF) + as.factor(ORGANM) +
            as.factor(FLESHM) + as.factor(FISH) + as.factor(INSECTSV) + as.factor(EGGS) +
            as.factor(MILK) + as.factor(NUTS) + as.factor(LEGUM) + as.factor(OILF) + as.factor(SWEETS) +
            as.factor(SAB) + as.factor(OUT_CER) + as.factor(Shop_type), data = train, Hess = TRUE)

#m <- polr(as.factor(OP_time) ~ ., data = train, Hess = TRUE)

m_3 <- polr(as.factor(OP_time) ~ as.factor(District) + Latitude + Longitude +
            Per_ultra + as.factor(CERE) + as.factor(WHITE) + as.factor(VITAMINV) + as.factor(VITAMINRT) + as.factor(DARKV) +
            as.factor(OTHERV) + as.factor(VITAMINF) + as.factor(OTHERF) + as.factor(ORGANM) +
            as.factor(FISH) + as.factor(INSECTSV) + 
            as.factor(MILK) + as.factor(LEGUM) + as.factor(OILF) + as.factor(SWEETS) +
            as.factor(SAB) + as.factor(OUT_CER) + as.factor(Shop_type) + SWEETS:SAB + OTHERV:OILF + VITAMINV:OTHERV + FISH:LEGUM + WHITE:VITAMINV, data = train, Hess = TRUE)

#OTHERV:OILF + VITAMINV:OTHERV + Shop_type:LEGUM + FISH:LEGUM + WHITE:VITAMINV + District:INSECTSV + District:Shop_type
#+ as.factor(SWEETS):as.factor(SAB) +
 # as.factor(OTHERV):as.factor(OILF) + as.factor(VITAMINV):as.factor(OTHERV) +
  #as.factor(Shop_type):as.factor(LEGUM) + as.factor(FISH):as.factor(LEGUM
summary(m_3)

library(DescTools)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, direction = 'both')
PseudoR2(m2, which = "all")

# five features most positive
m <- polr(as.factor(OP_time) ~ as.factor(OILF) + as.factor(Shop_type) + as.factor(CERE) + 
            as.factor(SAB) + as.factor(NUTS), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)
summary(m2)

predictions <- predict(m2, test)
addmargins(table(predicted = predictions,actual = test$OP_time))
# 67.6% train accuracy
# 72.3% test accuracy

# five from RFE
m <- polr(as.factor(OP_time) ~ as.factor(OTHERF) + as.factor(ORGANM) + as.factor(INSECTSV) + 
            as.factor(MILK) + as.factor(OILF), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)
summary(m2)

predictions <- predict(m, train)
addmargins(table(predicted = predictions,actual = train$OP_time))
# 66.9% train 
# 68.75% test

# ten from RFE
m <- polr(as.factor(OP_time) ~ as.factor(Shop_type) + as.factor(Ready_food) + as.factor(DARKV) + 
            as.factor(OTHERF) + as.factor(ORGANM) + as.factor(INSECTSV) + as.factor(MILK) +
            as.factor(NUTS) + as.factor(OILF) + as.factor(SAB), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))
# 67.4% train 
# 74.1% test

# ten from RFE
m <- polr(as.factor(OP_time) ~ as.factor(Shop_type) + as.factor(Ultra_food) + as.factor(Ready_food) + as.factor(DARKV) + 
            + as.factor(VITAMINF) + as.factor(OTHERF) + as.factor(ORGANM) +
            as.factor(OILF) + as.factor(SAB) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))
# 67.6% train 
# 72.3% test

# with STEP_AIC, went up to 74.1%

# RFE 15
m <- polr(as.factor(OP_time) ~ as.factor(District) + as.factor(Shop_type) + as.factor(Ready_food) + 
            as.factor(CERE) + as.factor(WHITE) + as.factor(DARKV) + as.factor(OTHERF) + 
            as.factor(ORGANM) + as.factor(INSECTSV) + as.factor(MILK) + as.factor(NUTS) + as.factor(LEGUM) +
            as.factor(OILF) + as.factor(SAB) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy

# RFE 15
m <- polr(as.factor(OP_time) ~ ., data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy

# RFE 20
m <- polr(as.factor(OP_time) ~ as.factor(District) + as.factor(Shop_type) + as.factor(Ultra_food) +
            as.factor(Ready_food) + 
            as.factor(CERE) + as.factor(WHITE) + as.factor(VITAMINV) + as.factor(DARKV) + as.factor(VITAMINF) +
            as.factor(OTHERF) + 
            as.factor(ORGANM) + as.factor(INSECTSV) + as.factor(MILK) + as.factor(NUTS) + as.factor(LEGUM) +
            as.factor(OILF) + as.factor(SWEETS) + as.factor(SAB) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, direction = "both")

summary(m2)

predictions <- predict(m, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy

# RFE 20
m <- polr(as.factor(OP_time) ~ as.factor(Shop_type) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, direction = "both")

summary(m2)

predictions <- predict(m, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy

#Multinomial - unordered - 75% test accuracy
train$OP_time = factor(train$OP_time, levels = c(1,2,3,4), ordered = FALSE)

model <- multinom(OP_time ~ as.factor(Shop_type) + as.factor(OTHERF) + 
                    as.factor(ORGANM) + as.factor(INSECTSV) + 
                    as.factor(MILK) + as.factor(OILF), data = train)
summary(model)

predictions <- predict(model, test)
addmargins(table(predicted = predictions,actual = test$OP_time))
#75% test accuracy

# BEST - 75% testing accuracy
train$OP_time = factor(train$OP_time, levels = c(1,2,3,4), ordered = TRUE)

m <- polr(OP_time ~ as.factor(Shop_type) + as.factor(OTHERF) + 
            as.factor(ORGANM) + as.factor(INSECTSV) + 
            as.factor(MILK) + as.factor(OILF), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

#m2 <- stepAIC(m, ~.^2, direction = 'both')

predictions <- predict(m, test)
addmargins(table(predicted = predictions,actual = test$OP_time))
# 75% test accuracy
