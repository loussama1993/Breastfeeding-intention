library(tidyverse)      
library(survival)       # Survival analysis.
library(survminer)      # Survival plots




# Load the data

load("mydata.RData")

follow_up_data <-read_csv("C:/Users/USER/Desktop/HDS/data modelling/assignment/Born in Wales Data June 2022/Born in Wales Follow-up Data June 2022.csv")

str(follow_up_data)

# Calculate Breastfeeding duration and censored status 
follow_up_data$breastfeeding_duration <- as.numeric(follow_up_data$`Stopped breastfeeding` - follow_up_data$`Started breastfeeding`)
follow_up_data$censored <- ifelse(is.na(follow_up_data$`Stopped breastfeeding`), 0, 1)

# Prepare the datafames
surveydata <- rename(surveydata, STUDY_ID = SYSTEM_ID)
surveydata$STUDY_ID <- as.numeric(surveydata$STUDY_ID)

duplicates_follow_up_data <- follow_up_data[duplicated(follow_up_data[c("STUDY_ID","Start time")]) | duplicated(follow_up_data[c("STUDY_ID","Start time")], fromLast = TRUE), ]
duplicates_surveydata <- surveydata[duplicated(surveydata[c("STUDY_ID","Start time")]) | duplicated(surveydata[c("STUDY_ID","Start time")], fromLast = TRUE), ]

surveydata <- surveydata[!duplicated(surveydata[c("STUDY_ID","Start time")]), ]
follow_up_data <- follow_up_data[!duplicated(follow_up_data[c("STUDY_ID", "Start time")]), ]

# Perform the inner join
merged_df <- inner_join(follow_up_data, surveydata, by = c("STUDY_ID", "Start time")) 

merged_df <- merged_df %>%
  dplyr::select(breastfeeding_duration, censored, relationship_status, work_type) %>%
  filter(!is.na(breastfeeding_duration)) %>%
  filter(relationship_status != "unknown")
#-------------------------------------------------------------------------------

# Kaplan-Meier Survival Analysis
# Create a Surv object for the survival analysis
surv_obj <- Surv(time = merged_df$breastfeeding_duration, event = merged_df$censored)

# Fit Kaplan-Meier survival curve
km_fit <- survfit(surv_obj ~ 1, data = merged_df)

# Calculate median survival time
km_summary <- summary(km_fit)

median_survival_time <- min(km_summary$time[km_summary$surv <= 0.5])


# Plot the survival curve
ggsurvplot(
  km_fit, 
  data = merged_df, 
  conf.int = TRUE,
  surv.median.line = "hv", # Adds a horizontal and vertical line at the median
  ggtheme = theme_minimal(),
  xlab = "Time (days)", 
  ylab = "Survival probability"
)

# Fit a survival model to assess the impact of relationship status on breastfeeding duration
model_relationship <- survfit(surv_obj ~ relationship_status, data = merged_df)


# Plot the survival curves for different relationship statuses to visually compare 
# their effects on breastfeeding duration
ggsurvplot(model_relationship, data = merged_df) +
  guides(col = guide_legend(nrow = 1, title.position = "top", label.position = "bottom"))



# Perform a survival difference test to statistically evaluate if there are significant
# differences in breastfeeding duration across different relationship statuses
survdiff(surv_obj~relationship_status, data=merged_df)

# Fit a survival model to assess the impact of working hours on breastfeeding duration
model_work_type <- survfit(surv_obj ~ work_type, data = merged_df)

# Plot the survival curves for different work types to visually compare 
# their effects on breastfeeding duration
ggsurvplot(model_work_type, data=merged_df) +
  guides(col = guide_legend(nrow = 1, title.position = "top", label.position = "bottom"))
# Perform a survival difference test to statistically evaluate if there are significant
# differences in breastfeeding duration across different work types
survdiff(surv_obj~work_type, data=merged_df)

#-------------------------------------------------------------------------------
# Cox model for relationship status 
cox_model_relationship <- coxph(surv_obj ~ relationship_status, data = merged_df)
summary(cox_model_relationship)

# Cox model for working hours 
cox_model_working_hours <- coxph(surv_obj ~ work_type, data = merged_df)
summary(cox_model_working_hours)


# Cox Proportional Hazards Model
cox_model <- coxph(surv_obj ~ work_type + relationship_status, data = merged_df)

summary(cox_model)

# Check proportional hazards assumption for the model
test_proportional_hazards <- cox.zph(cox_model)
print(test_proportional_hazards)
plot(test_proportional_hazards)


