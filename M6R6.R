library(psych)
library(stats)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(corrplot)

library(dplyr)
library(lmtest)

#import data 
PSID_14 <- read.csv("D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\PSID (14 variables).csv", header=TRUE, stringsAsFactors=FALSE)
PSID_14
wage <- summary(PSID_14$wage2)
experience <- summary(PSID_14$experience)
education <- summary(PSID_14$education)
str(PSID_14)
names(PSID_14)


#part1
# Perform regression analysis
reg_model <- lm(log(wage2) ~ weeks + experience + education, data = PSID_14)
summary(reg_model)   # Print the summary of the regression model
test1 <- broom::tidy(reg_model)
test1 
write.csv(test1, "D:\\MPS_Quater 1\\ALY6010_Probability & Statistics\\sumtable1.txt")

# Predict the wages based on the regression model
PSID_14$predicted_wage <- predict(reg_model)
# Create the scatterplot
ggplot(PSID_14, aes(x = predicted_wage, y = log(wage2))) +
  geom_point() +
  labs(x = "Predicted Wage", y = "Observed Wage") +
  ggtitle("Scatterplot of Observed vs. Predicted Wages")

# Perform diagnostic tests
# Test for heteroscedasticity
bptest(reg_model)
# Test for normality of residuals
shapiro.test(reg_model$residuals)




#part 2
# Subset the data based on a categorical variable (category)
category1_data <- subset(PSID_14, occupation == "white")
category2_data <- subset(PSID_14, occupation == "blue")

# Perform separate regressions for each subset
model_category1 <- lm(log(wage2) ~ experience + weeks, data = category1_data)
model_category2 <- lm(log(wage2) ~ experience + weeks, data = category2_data)

# Display the regression results for each subset
summary(model_category1)
summary(model_category2)



#Part 3
# Create dummy variables from a categorical variable
dummy_variables <- model.matrix(~ occupation - 1, data = PSID_14)
# Add the dummy variables to the dataset
PSID_14 <- cbind(PSID_14, dummy_variables)
# Run the regression with the set of dummy variables
model <- lm(log(wage2) ~ weeks + experience + education + dummy_variables, data = PSID_14)

# Omitting one dummy variable to avoid the dummy variable trap
model_without_trap <- lm(log(wage2) ~ weeks + experience + education + dummy_variables[,-1], data = PSID_14)

# Display the regression results
summary(model)
summary(model_without_trap)
