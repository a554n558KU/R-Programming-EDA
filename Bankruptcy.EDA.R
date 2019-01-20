# Read in Bankruptcy Data:
bank.data <- read.csv("bankruptcy.csv")
par(mar=c(1,1,1,1))

# get rid of FYEAR and CUSIP
bank.data$FYEAR <- NULL
bank.data$CUSIP <- NULL

# Conducting EDA
names(bank.data)
str(bank.data)
summary(bank.data)
sum(is.na(bank.data))

# Histogram of first variable
hist(bank.data$R1, main = "Distribution of R1",
     ylab = "Bankruptcy Frequency",
     xlab = "Company Working Capital/Total Asset", col = c("blue", "red", "green"))
# Histogram of second variable
hist(bank.data$R2, main = "Distribution of R2",
     ylab = "Bankruptcy Frequency",
     xlab =  "Retained Earning/Total Asset", col = c("blue", "red", "green"))
# Histogram of third variable
hist(bank.data$R3, main = "Distribution of R3",
     ylab = "Bankruptcy Frequency",
     xlab =  "Earning Before Interest & Tax/Total Asset", col = c("blue", "red", "green"))
# Histogram of Fourth Variable
hist(bank.data$R4, main = "Distribution of R4",
     ylab = "Bankruptcy Frequency",
     xlab =  "Market Capital/Total Liability", col = c("blue", "red", "green"))
plot(DLRSN~, R1 + R2)
# Histogram of Fifth Variable
hist(bank.data$R5, main = "Distribution of R5",
     ylab = "Bankruptcy Frequency",
     xlab =  "SALE/Total Asset", col = c("blue", "red", "green"))
# Histogram of Sixth Variable
hist(bank.data$R6, main = "Distribution of R6",
     ylab = "Bankruptcy Frequency",
     xlab =  "Total Liability/Total Asset", col = c("blue", "red", "green"))
# Histogram of Seventh Variable
hist(bank.data$R7, main = "Distribution of R7",
     ylab = "Bankruptcy Frequency",
     xlab =  "Current Asset/Current Liability", col = c("blue", "red", "green"))
# Histogram of Eighth Variable
hist(bank.data$R8, main = "Distribution of R8",
     ylab = "Bankruptcy Frequency",
     xlab =  "Net Income/Total Asset", col = c("blue", "red", "green"))
# Histogram of Ninth Variable
hist(bank.data$R9, main = "Distribution of R9",
     ylab = "Bankruptcy Frequency",
     xlab =  "Log(SALE)", col = c("blue", "red", "green"))
# Histogram of Tenth of Variable
hist(bank.data$R10, main = "Distribution of R10",
     ylab = "Bankruptcy Frequency",
     xlab =  "log(Market Cap)", col = c("blue", "red", "green"))


# Overall bankruptcy probability
mean(bank.data$DLRSN) * 100

# Calcuatling Mean, Standard Deviation for both
# Bankruptcy and Non-Bankruptcy companies
bank1 <- bank.data[bank.data$DLRSN == 1,] # Companies that went bankrupted
bank2 <- bank.data[bank.data$DLRSN == 0,] # Companies that didn't go bankrupted
# Calculate mean and standard deviation
b1 <- c(bank1$DLRSN, bank1$R1, bank1$R2, bank1$R3,
          bank1$R4, bank1$R5, bank1$R6, bank1$R7, bank1$R8,
          bank1$R9, bank1$R10)
mean(b1)
sd(b1)
b2 <- c(bank2$DLRSN, bank2$R1, bank2$R2, bank2$R3,
        bank2$R4, bank2$R5, bank2$R6, bank2$R7, bank2$R8, bank2$R9, bank2$R10)
mean(b2)
sd(b2)

# Split data to 80% training and 20% test
index <- sample(nrow(bank.data), 0.8 * nrow(bank.data))
bank.train <- bank.data[index,]
bank.test <- bank.data[-index,]

# Building Logistic Regression Model for training sample using all predictors
# Logistic Model will estimate the "probability" of the outcome
bank.glm <- glm(DLRSN~., data = bank.train, family = "binomial")

# Calculate AIC, BIC, and Mean Residual Deviance
bank.glm$deviance
AIC(bank.glm)
BIC(bank.glm)

# Explore Dependent Variable: 
# 1 Companies that went Bankrupted
# 0 Companies that didn't go Bankrupted
table(bank.train$DLRSN)

# Estimate the bankruptcy probability for bank.train
bank.train_prob <- predict(bank.glm, type = "response")

# Find the bankruptcy probability of the average company
mean(bank.data$DLRSN)

# Predict bankruptcy if probability of bankruptcy is greater than avg for bank.train
bank.train_pred <- ifelse(bank.train_prob > 0.1421343, 1, 0)

# Calculate the model's accuracy
mean(bank.train$DLRSN == bank.train_pred)

library(pROC)
# Create ROC Curve
ROC <- roc(bank.train$DLRSN, bank.train_prob)
# Plot ROC Curve
plot(ROC, col = "blue", main = "ROC Curve")
# Calculate Area Under the Curve "AUC"
auc(ROC) * 100
# Based on this prediction, the model is doing better than the baseline

# Find Best step_wise model using BIC Criterion.

# Specify a null model with no predictors
null_model <- glm(DLRSN ~ 1, data = bank.train, family = "binomial")

# Specify the full model
full_model <- glm(DLRSN ~ ., data = bank.train, family = "binomial")

# Use stepwise algorithm to build model with BIC criterion
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both", k = log(nrow(bank.train)))
summary(step_model)

# Create and Plot final model. Find ROC curve of final model and find AUC
final_model <- glm(DLRSN ~ + R1 + R2 + R3 + R6 + R7 + R8 + R9 + R10, family = "binomial", data = bank.train)
summary(final_model)
final_prob <- predict(final_model, type = "response")
ROC_Final <- roc(bank.train$DLRSN, final_prob)
plot(ROC_Final, col = "green", main = "ROC Curve")
auc(ROC_Final) * 100
