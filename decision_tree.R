library(tidyverse)      
library(caret)          # Machine learning workflow.
library(rpart)          # Decision trees.
library(rpart.plot)     # DT plots (in colour).


# Load dataset
load("mydata.RData")

# Fit decision tree model for predicting breastfeeding intention
tree_model <- rpart(breastfeeding_intention ~ ., data = dataforanalysis, method = "class")

# Plot the decision tree with detailed node information
rpart.plot(tree_model, type = 4, extra = 102, cex = 0.5)

# Extract and store variable importance
var_importance <- tree_model$variable.importance

# Adjust plot margins for readability and plot variable importance
par(mar = c(5, 12, 4, 2)) 
barplot(var_importance, main = "Variable Importance", horiz = TRUE, las = 1)  