library(car)
library(MASS)

# Data exploration and model building done by Nilay Varshney

bodyfat <- read.csv('BodyFat.csv')
summary(bodyfat)
bodyfat$BODYFAT_DENSITY <- (495 / bodyfat$DENSITY) - 450
bodyfat$ADIPOSITY_HEIGHT_WEIGHT <- (703 * bodyfat$WEIGHT) / bodyfat$HEIGHT^2
bodyfat$WEIGHT_ADIPOSITY_HEIGHT <- (bodyfat$ADIPOSITY * (bodyfat$HEIGHT^2)) / 703
bodyfat$HEIGHT_ADIPOSITY_WEIGHT <- sqrt((703 * bodyfat$WEIGHT) / bodyfat$ADIPOSITY)
bodyfat <- bodyfat[colnames(bodyfat)[c(2, 18, 3, 4, 5, 20, 6, 21, 7, 19, 8:17)]]
bool1 <- bodyfat$BODYFAT < 2 | bodyfat$BODYFAT > 30
bool2 <- bodyfat$WEIGHT > 250
bool3 <- bodyfat$ADIPOSITY > 35 | bodyfat$ADIPOSITY < 18.5
bool4 <- bodyfat$HEIGHT < 60
bodyfat_abnormal <- bodyfat[bool1 | bool2 | bool3 | bool4,]

bodyfat_nooutlier <- bodyfat[-c(39, 172, 182),]
bodyfat_nooutlier$HEIGHT[41] = 69.5
bodyfat_nooutlier$HEIGHT <- round(2.54 * bodyfat_nooutlier$HEIGHT, 1)

bodyfat_model <- bodyfat_nooutlier[, colnames(bodyfat_nooutlier)[c(1, 4, 5, 7, 11:20)]]
all_model <- lm(BODYFAT ~ ., data = bodyfat_model)
summary(all_model)
vif(all_model)

cor(bodyfat_model$ABDOMEN, bodyfat_model[c("WEIGHT", "CHEST", "HIP", "THIGH")])
plot(bodyfat_model$ABDOMEN, bodyfat_model$WEIGHT, xlab = "Abdomen", ylab = "Weight", main = "Abdomen vs. Weight")
plot(bodyfat_model$ABDOMEN, bodyfat_model$CHEST, xlab = "Abdomen", ylab = "Chest", main = "Abdomen vs. Chest")
plot(bodyfat_model$ABDOMEN, bodyfat_model$HIP, xlab = "Abdomen", ylab = "Hip", main = "Abdomen vs. Hip")
plot(bodyfat_model$ABDOMEN, bodyfat_model$THIGH, xlab = "Abdomen", ylab = "Thigh", main = "Abdomen vs. Thigh")

model1 <- lm(BODYFAT ~ AGE + HEIGHT + NECK + ABDOMEN + KNEE + ANKLE + BICEPS + FOREARM + WRIST, data = bodyfat_model)
summary(model1)
vif(model1)

model2 <- lm(BODYFAT ~ ABDOMEN + WRIST + HEIGHT + AGE, data = bodyfat_model)
summary(model2)
anova(model2)

model3 <- lm(BODYFAT ~ ABDOMEN + WRIST + AGE + HEIGHT, data = bodyfat_model)
summary(model3)
anova(model3)

finalmodel <- lm(BODYFAT ~ ABDOMEN + WRIST + HEIGHT, data = bodyfat_model)
summary(finalmodel)

# Model diagnostics and statistical inference done by Zhifeng Chen

plot(finalmodel$fitted.values, finalmodel$residuals, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Fitted Values vs. Residuals')
qqnorm(finalmodel$residuals, main = 'Normal Q-Q Plot of Residuals')
qqline(finalmodel$residuals)
plot(hatvalues(finalmodel), studres(finalmodel), xlab = 'Leverage', ylab = 'Standardized Residuals', main = 'Residuals vs. Leverage')

confint(finalmodel, 'ABDOMEN')
confint(finalmodel, 'WRIST')
confint(finalmodel, 'HEIGHT')

# Model comparison done by Xiaowei Zhu

bodyfat_model$ADIPOSITY <- bodyfat_nooutlier$ADIPOSITY

usnavymodel <- lm(BODYFAT ~ log(ABDOMEN -NECK) + log(HEIGHT), data = bodyfat_model)
bmimodel <- lm(BODYFAT ~ ADIPOSITY + AGE, data = bodyfat_model)

summary(usnavymodel)
summary(bmimodel)

cv <- function(model, data, k=10, seed=5) {
  set.seed(seed)
  # cvgroup part: see https://blog.csdn.net/yawei_liu1688/article/details/79142088 for reference
  cvlist <- list()
  n <- rep(1:k, ceiling(length(data$BODYFAT)/k))[1:length(data$BODYFAT)]
  temp <- sample(n, length(data$BODYFAT))  
  x <- 1:k
  dataseq <- 1:length(data$BODYFAT)
  cvlist <- lapply(x, function(x) dataseq[temp==x])  
  
  cv.error.k=rep(0,k)
  for (i in 1:k) {
    cv_model = lm(model, data[-cvlist[[i]], ])
    cv_prediction = predict(cv_model, data[cvlist[[i]], ])
    cv.error.k[i] = mean((data[cvlist[[i]],"BODYFAT"]-cv_prediction)^2)
  }
  cv.error = mean(cv.error.k)
  return(cv.error)
}

cv(finalmodel, data = bodyfat_model)
cv(usnavymodel, data = bodyfat_model)
cv(bmimodel, data = bodyfat_model)