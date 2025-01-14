library(MASS)

library(tidyverse)     

library(caret)



# Load the dataset
load("mydata.RData")

# Fit a logistic regression model to predict breastfeeding intention 
model <- glm(breastfeeding_intention ~ ., data = dataforanalysis, family = "binomial")
summary(model)

# Fit a more specific model with selected predictors based on theoretical relevance and p-value
model2 <- glm(breastfeeding_intention ~ smoking_status + relationship_status +
                education + work_type + BMI_category + number_of_children
                 + mood_comp + stress_comp + anxiety_comp +
                physical_activity_comp 
              , data = dataforanalysis, family = "binomial")
summary(model2)

model3 <- glm(breastfeeding_intention ~ smoking_status + education + 
                stress_comp + BMI_category + number_of_children + relationship_status +
                physical_activity_comp
              , data = dataforanalysis, family = "binomial")
summary(model3)

# Final model focusing on core predictors for simplicity and ease of interpretation
model4 <- glm(breastfeeding_intention ~ smoking_status + BMI_category + education +
                relationship_status  
              , data = dataforanalysis, family = "binomial")
summary(model4)



# Calculate Adjusted Odds Ratios 
aors <- exp(coef(model4))
aors

# Exponentiate the confidence intervals to get them in terms of odds ratios
exp_conf_int <- exp(confint(model4))
exp_conf_int

# Predict breastfeeding intention using model4
test_data <- expand.grid(
  smoking_status = levels(dataforanalysis$smoking_status),
  relationship_status = levels(dataforanalysis$relationship_status),
  BMI_category = levels(dataforanalysis$BMI_category),
  education = levels(dataforanalysis$education)
)
predicted_probs <- predict(model4, newdata = test_data, type = "response")
predicted_data2 <- data.frame(test_data, breastfeeding_intention = predicted_probs)

# Validation using confusion matrix from the caret package
predicted_response2 <- ifelse(fitted(model4) > 0.5, 1, 0)
actual_response2 <- dataforanalysis$breastfeeding_intention
outcome2 <- table(Predicted = predicted_response2, Actual = actual_response2)
confusionMatrix(outcome2) 

#-------------------------------------------------------------------------------
# Analyze the impact of COVID-19 on breastfeeding intention 
covid_model <- glm(breastfeeding_intention ~ covid_comp, data = Covid_data, family = "binomial")

summary(covid_model)

# Calculate Adjusted Odds Ratios (AORs)
aors2 <- exp(coef(covid_model))
aors2

# Exponentiate the confidence intervals to get them in terms of odds ratios
exp_conf_int2 <- exp(confint(covid_model))

# Inspect the COVID-19 model's predictions against actual responses
predicted_response <- ifelse(fitted(covid_model) > 0.5, 1, 0)
actual_response <- Covid_data$breastfeeding_intention
outcome <- table(Predicted = predicted_response, Actual = actual_response)
outcome

