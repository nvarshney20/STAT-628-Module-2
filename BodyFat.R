library(car)

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

# We can see that for the most part, calculating bodyfat from density leads to
# more extreme values, and weight, height, and adiposity are all consistent with
# each other, with the exception of row 42, where height = 29.5 in. It makes
# more sense for height to be 69.5 in. in that case. We will get rid of rows
# where bodyfat < 2, or weight = 363 (this guy has maximum values for several
# other columns, so we can conclude that this guy is an outlier by being
# morbidly obese)

bodyfat_nooutlier <- bodyfat[-c(39, 172, 182),]
bodyfat_nooutlier$HEIGHT[41] = 69.5

bodyfat_model <- bodyfat_nooutlier[, colnames(bodyfat_nooutlier)[c(1, 4, 5, 7, 11:20)]]
all_model <- lm(BODYFAT ~ ., data = bodyfat_model)
plot(all_model)
summary(all_model)
vif(all_model)

cor(bodyfat_model$ABDOMEN, bodyfat_model[c("WEIGHT", "CHEST", "HIP", "THIGH")])
plot(bodyfat_model$ABDOMEN, bodyfat_model$WEIGHT, xlab = "Abdomen", ylab = "Weight", main = "Abdomen vs. Weight")
plot(bodyfat_model$ABDOMEN, bodyfat_model$CHEST, xlab = "Abdomen", ylab = "Chest", main = "Abdomen vs. Chest")
plot(bodyfat_model$ABDOMEN, bodyfat_model$HIP, xlab = "Abdomen", ylab = "Hip", main = "Abdomen vs. Hip")
plot(bodyfat_model$ABDOMEN, bodyfat_model$THIGH, xlab = "Abdomen", ylab = "Thigh", main = "Abdomen vs. Thigh")

model1 <- lm(BODYFAT ~ AGE + HEIGHT + NECK + ABDOMEN + KNEE + ANKLE + BICEPS + FOREARM + WRIST, data = bodyfat_model)
plot(model1)
summary(model1)
vif(model1)

model2 <- lm(BODYFAT ~ ABDOMEN + WRIST + HEIGHT + AGE, data = bodyfat_model)
plot(model2)
summary(model2)
anova(model2)

model3 <- lm(BODYFAT ~ ABDOMEN + WRIST + AGE + HEIGHT, data = bodyfat_model)
plot(model3)
summary(model3)
anova(model3)

finalmodel <- lm(BODYFAT ~ ABDOMEN + WRIST + HEIGHT, data = bodyfat_model)
plot(finalmodel)
summary(finalmodel)