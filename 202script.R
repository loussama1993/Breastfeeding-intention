library(tidyverse)
source("functions.R")

#-------------------------------------------------------------------------------
#Load the data 

file_path <- "C:/Users/USER/Desktop/HDS/data modelling/assignment/Born in Wales Data June 2022/Born in Wales Data June 2022.csv"
encoding_guess <- guess_encoding(file_path)

# Read the data and replace the problematic character
surveydata <- read_csv(file_path, locale = locale(encoding = encoding_guess$encoding), col_types = cols(.default = col_character())) %>%
  mutate_all(~str_replace_all(., "[œ†]", ""))

#-------------------------------------------------------------------------------
surveydata$"Start time" <- as.Date(surveydata$"Start time", format = "%d/%m/%Y")
surveydata$"Expected date of delivery of your baby" <- as.Date(surveydata$"Expected date of delivery of your baby", format = "%d/%m/%Y")

#-------------------------------------------------------------------------------

  
surveydata <- surveydata %>%
  rename(
    ethnicity = 'What is your ethnic group?',
    language = 'What is the main language spoken in your home?',
    relationship_status = 'What is your current relationship status ?',
    sexual_orientation = 'Would you consider yourself to be:',
    smoking_status = 'Do you smoke?',
    drinking_status = 'Do you drink alcohol?',
    number_of_children = 'Number of children',
    employment_status = `Are you currently working?`,
    education = 'What is the highest level of education you have reached?',
    annual_household_income = 'What is the number that best describes your TOTAL household income BEFORE TAX?',
    breastfeeding_intention = 'How are you planning to feed your baby?',
    covid_symptoms = 'Have you had symptoms that are associated with COVID19 (fever, dry cough, loss of taste or smell, fatigue, muscle pain)?',
    symptoms_severity = 'What symptoms did you have?',
    covid_treatment ='What treatment did you have ?',
    covid_test = 'Have you had a test?',
    low_mood = 'Have you experienced low mood during your pregnancy?',
    interest = 'Little interest or pleasure in doing things?',
    down_depressed_hopeless = 'Feeling down, depressed, or hopeless?',
    sleeping_disorder = 'Trouble falling or staying asleep, or sleeping too much',
    little_energy = 'Feeling tired or having little energy?',
    eating_disorder = 'Poor appetite or overeating?',
    guilt_worth = 'Feeling bad about yourself or that you are a failure of have let yourself or family down?',
    concentration = 'Trouble concentrating on things, such as reading the newspaper or watching television?',
    reactivity = 'Moving or speaking so slowly that other people could have noticed? Or the opposite being so fidgety or restless that you have been moving around a lot more than usual?',
    suicidal_thought = 'Thoughts that you would be better off dead, or of hurting yourself in some way?',
    nervous_anxious = 'Feeling nervous, anxious or on edge?',
    worried = 'Not being able to stop or control worrying?',
    worried2 = 'Worrying too much about different things?',
    not_relaxing= 'Trouble relaxing?',
    restless = 'Being so restless that it is hard to sit still?',
    irritable = 'Becoming easily annoyed or irritable?',
    afraid = 'Feeling afraid as if something awful might happen?',
    work_activity = 'Please tell us the type and amount of physical activity involved in your work',
    exercise = 'Physical exercise such as swimming, jogging, aerobics, football, tennis, gym workout etc.',
    cycling = 'Cycling, including cycling to work and during leisure time',
    walking ='Walking, including walking to work, shopping, for pleasure etc.',
    housework = 'Housework/Childcare',
    gardening = 'Gardening/DIY',
    walking_pace = 'How would you describe your usual walking pace?  Please mark one box only.',
    stress_period = 'Have you had any periods of bad stress in your pregnancy ?',
    relationship_difficulties = 'any serious relationship difficulties with your husband or partner or separated or divorced?',
    legal_problems = 'any serious legal or financial problems?',
    crime_victim = 'were you or someone close to you a victim of violence or crime?',
    close_illness = 'someone close with a serious illness',
    close_death = 'the death of someone close to you ?',
    covid_stress = 'was this stressful event related to coronavirus?',
    support = 'During this time did you have someone who could support you emotional or financially',
    stress_scale = '0n a scale of 1 to 10, how stressful was this time  (1 is not at all, 10 is overwhelming)?',
    care_satisfaction = 'I feel satisfied with the support and care I have received in my pregnancy from my health care team (1 is strongly disagree, 5 is strongly agree)',
    delivery_change = 'Has COVID19 change the type of birth you feel you will have?',
    care_type = 'What type of maternity care are you receiving now?',
    pregnancy_experience = 'How would you describe your experience of this pregnancy (support from midwife, how you feel about being pregnant)?',
    work_type = 'Do you work',
    household_size = 'How many people live in your home (not including you)?'
  ) %>%
  mutate(BMI = round(as.numeric(`My weight (before pregnancy) in Kg is:`) / ((as.numeric(`My height in centimetres is :`) / 100)^2), 1))
#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    BMI_category = factor(case_when(
      is.na(BMI) ~ "unknown",
      BMI < 18.5 ~ "underweight",
      BMI >= 18.5 & BMI < 25 ~ "healthy range",
      BMI >= 25 & BMI < 30 ~ "overweight",
      BMI >= 30 & BMI < 40 ~ "obesity",
      BMI >= 40 ~ "severe obesity"
    ), levels = c("healthy range", "overweight", "obesity", "severe obesity",
                  "underweight", "unknown"))
  )


#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    employment_status = factor(case_when(
      is.na(employment_status) ~ "unknown",
      employment_status %in% c("No, I can not do my work in lockdown (furloughed or business closed in lockdown)",
                                  "No, I can not do my work in lockdown (furlouged or business closed in lockdown)",
                                  "No, I am a stay at home parent", 
                                  "No, I am unemployed") ~ "unemployed",
      employment_status %in% "No, I am a student" ~ "student",
      employment_status %in% c("Yes, working outside the home/ in the office", 
                                  "Yes, I am working from home", 
                                  "No I am on maternity leave now", 
                                  "Yes, I am a key worker so working outside the home") ~ "employed/on maternity leave"
    ), levels = c("unemployed", "student", "employed/on maternity leave", "unknown"))
  )


#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    annual_household_income = factor(case_when(
      is.na(annual_household_income) | annual_household_income %in% c("Perfer not to say", 
                                                                      "Prefer not to say",
                                                                      "Monthly less than 5k") ~ "unknown",
      annual_household_income %in% c("50,000 +",
                                     "Both me and my husband are nurses earning 31,534 each (before tax) yearly",
                                     "150,000", 
                                     "200,000") ~ "50,000 +",
      TRUE ~ annual_household_income 
    ), levels = c("Between 40,000-49,999", "50,000 +", "Less than 10,000",
                  "Between 10,000-19,999", "Between 20,000-29,999",
                  "Between 30,000-39,999", "unknown"))
  )

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    education = factor(case_when(
      is.na(education) | education == "None" ~ "unknown",
      education %in% c("Exams at age 16 (GCSE or equivalent)", "Exams at age 18 (A level or equivalent)") ~ "secondary",
      education %in% c("Vocational qualifications", "Diploma", "NVQ via work", "2 nvqs", "CACHE L5") ~ "further education",
      education %in% c("University higher degree", "University degree", "Higher national diploma", "Phd", "PhD", "PGCE",
                       "Dip he", "DipHE", "Doctorate", "Post grad", "Bachelor of Science in nursing. Qualified nurse in the Philippines and UK",
                       "Clinical doctorate", "Diploma of Higher Education", "Master?s Degree", "Diploma of higher education",
                       "Currently studying Nursing Degree", "Masters", "Currently studying degree") ~ "higher education"
    ), levels = c("higher education","further education", "secondary", "unknown")
  ))

#-------------------------------------------------------------------------------

surveydata$ethnicity <- ifelse(
  surveydata$ethnicity %in% "Prefer not to say" | is.na(surveydata$ethnicity),  
  "Unknown",
  ifelse(surveydata$ethnicity %in% "Chinese", 
  "Asian",
  surveydata$ethnicity  
))

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    language = case_when(
      is.na(language) ~ "unknown",
      language %in% c("Bilingual Welsh and English", "English and Welsh",
                      "Bilingual Wel/Eng", "Equal welsh/English") ~ "English and Welsh",
      language %in% "Waray waray" ~ "Waray",
      TRUE ~ language
    )
  )

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    relationship_status = factor(case_when(
      is.na(relationship_status) | relationship_status == "Prefer not to say" ~ "unknown",
      relationship_status %in% c("Living with partner/civil partnership", 
                                 "Partner- lives in army accommodation and at home when off", 
                                 "Engaged") ~ "Living with partner/civil partnership",
      relationship_status %in% c("Single", "Separated") ~ "Single",
      TRUE ~ relationship_status
    ), levels = c("Living with partner/civil partnership", "Married", "Single",
                  "Dating", "unknown"))
  )

#-------------------------------------------------------------------------------

surveydata$sexual_orientation <- ifelse(
  surveydata$sexual_orientation %in% "Prefer not to say" | is.na(surveydata$sexual_orientation),  
  "unknown", surveydata$sexual_orientation
  )

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    smoking_status = factor(case_when(
      is.na(smoking_status) ~ "unknown",
      smoking_status %in% "No, never smoked" ~ "Never smoked",
      smoking_status %in% c("I have smoked but occasionally when younger.",
                            "Used to casually smoke when drinking but not for many years",
                            "Stopped smoking 17 years ago.",
                            "Quit 5 years ago") ~ "Former smoker",
      smoking_status %in% c("Yes I smoke, more than 5 cigarettes a week", 
                            "I smoke e-cigarettes", 
                            "Yes I smoke, less than 5 cigarettes a day",
                            "I smoke aroung 15 a day",
                            "Yes I smoke, less than 5 cigarettes a week") ~ "Current smoker",
      smoking_status %in% c("I used to, but I stopped when I knew I was pregnant",
                            "I used to but I stopped when I knew I was pregnant",
                            "I used to, but I stopped before I was pregnant",
                            "I used but I stopped before I was pregnant") ~ "Abstaining during pregnancy"
    ), levels = c("Abstaining during pregnancy", "Never smoked", "Former smoker", 
                  "Current smoker", "unknown"))
  )

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    drinking_status = factor(case_when(
      is.na(drinking_status) ~ "unknown",
      drinking_status %in% "No, I have never drunk alcohol" ~ "Lifetime abstainer",
      drinking_status %in% c(
        "Yes, I stopped as soon as I knew I was pregnant", 
        "Never during pregnant but never a massive drinker anway",
        "Hardly drant prior to pregnancy. Had none during pregnancy",
        "On very few occasions, but not since I have been pregnant",
        "Yes, I stopped as soon as I knew I was pregant",
        "I only drink once or twice a year, I doesn't appeal to me but if I'm in the mood I would drink when not pregnant",
        "Occasionally before pregnancy",
        "Occasionally before pregancy",
        "Yes, but I stopped before I was pregnant",
        "Very rarely before pregnancy"
      ) ~ "Abstaining during pregnancy",
      drinking_status %in% c("No, have in the past", "Not for a few years") ~ "Former drinker",
      drinking_status %in% c(
        "Yes, about once per week",
        "Yes, very occasionally now",
        "Yes but maybe 2/3 a year",
        "Just on the odd night out, hardly anything",
        "Rarely, perhaps 3-4 times a year", 
        "Only on special occasions"
      ) ~ "Current drinker"
      # Removed the TRUE condition
    ), levels = c("Abstaining during pregnancy", "Lifetime abstainer", 
                  "Former drinker", "Current drinker", "unknown"))
  )

#-------------------------------------------------------------------------------

surveydata$number_of_children <- as.factor(ifelse(
  is.na(surveydata$number_of_children) | surveydata$number_of_children == 0,
  "Nulliparous", # No children
  ifelse(surveydata$number_of_children == 1, 
         "Primiparous", # One child
         "Multiparous"  # More than one child
  )))

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate( 
    Nationality = case_when(
      Nationality %in% c("Prefer not to say", NA) ~ "unknown",
      Nationality %in% c("British", "Welsh") ~ "British",
      TRUE ~ Nationality
    )
  ) 

#-------------------------------------------------------------------------------

surveydata$breastfeeding_intention <- ifelse(grepl("Breast|breastfeeding", 
           surveydata$breastfeeding_intention, ignore.case = TRUE), 1, 0)


#-------------------------------------------------------------------------------

surveydata$covid_symptoms <- ifelse(is.na(surveydata$covid_symptoms), 0,
                                      ifelse(surveydata$covid_symptoms == "No", 1, 2))

surveydata <- surveydata %>%
  mutate(symptoms_severity = case_when(
    is.na(symptoms_severity) ~ 0,
    grepl("shortness of breath|chest pain", symptoms_severity, ignore.case = TRUE) ~ 2,
    TRUE ~ 1
  ))


values_mapping9 <- c("Stayed at home, took over the counter medicine" = 1,
                     "Stayed at home, did not need medicine" = 1,
                     "Stayed at home took, prescription medicine from GP" = 2)

recode_variable(covid_treatment, values_mapping9)

surveydata <- surveydata %>%
  mutate(covid_comp = covid_symptoms + symptoms_severity + covid_treatment)

Covid_data <- surveydata %>%
  select(breastfeeding_intention, covid_comp) %>%
  filter(covid_comp != 0) %>%
  mutate(covid_comp = factor(case_when(
    covid_comp == 1 ~ "Did not have COVID or had it without symptoms",
    covid_comp %in% 2:4 ~ "Mild symptoms",
    covid_comp >= 5 ~ "Severe symptoms"
  ), levels = c("Did not have COVID or had it without symptoms", "Mild symptoms",
                "Severe symptoms")
  ))

#-------------------------------------------------------------------------------

values_mapping8 <- c("No"= 0, "0" = 0, "Yes" = 1)

recode_variable(low_mood, values_mapping8)

values_mapping1 <- c("More than half the days" = 2, 
                     "Several days" = 1,
                     "Nearly every day" = 3,
                     "Nearly everyday" = 3,
                     "Not at all" = 0)

recode_variable(interest, values_mapping1)

recode_variable(down_depressed_hopeless, values_mapping1)

recode_variable(sleeping_disorder, values_mapping1)

recode_variable(little_energy, values_mapping1)

recode_variable(eating_disorder, values_mapping1)

recode_variable(guilt_worth, values_mapping1)

recode_variable(concentration, values_mapping1)

recode_variable(reactivity, values_mapping1)

recode_variable(suicidal_thought, values_mapping1)

surveydata <- surveydata %>%
  mutate(mood_comp = low_mood + interest + down_depressed_hopeless + sleeping_disorder + 
           little_energy + eating_disorder + guilt_worth + concentration + 
           reactivity + suicidal_thought)


surveydata <- surveydata %>%
  mutate(mood_comp = factor(case_when(
    mood_comp %in% 0:5 ~ "Normal fluctuations in mood",
    mood_comp %in% 6:10 ~ "Mild Depression",
    mood_comp %in% 11:15 ~ "Moderate Depression",
    mood_comp >= 16 ~ "Severe Depression"
  ), levels = c("Normal fluctuations in mood", "Mild Depression",
                "Moderate Depression", "Severe Depression")))

#-------------------------------------------------------------------------------

values_mapping2 <- c("More than half the days" = 2, 
                      "Several days" = 1,
                      "Not at all" = 0,
                      "Nearly everyday" = 3)

recode_variable(irritable, values_mapping2)

recode_variable(nervous_anxious, values_mapping2)

recode_variable(not_relaxing, values_mapping2)

recode_variable(restless, values_mapping2)

recode_variable(worried, values_mapping2)

recode_variable(worried2, values_mapping2)

recode_variable(afraid, values_mapping2)

surveydata <- surveydata %>%
  mutate(anxiety_comp = nervous_anxious + irritable + not_relaxing + 
           restless + worried + worried2 + afraid)


surveydata <- surveydata %>%
  mutate(anxiety_comp = factor(case_when(
    anxiety_comp %in% 0:4 ~ "Minimal Anxiety",
    anxiety_comp %in% 5:9 ~ "Mild Anxiety",
    anxiety_comp %in% 10:14 ~ "Moderate Anxiety",
    anxiety_comp >= 15 ~ "Severe Anxiety"
  ), levels = c("Minimal Anxiety", "Mild Anxiety","Moderate Anxiety", "Severe Anxiety")
  ))

#-------------------------------------------------------------------------------

values_mapping3 <- c("1 hour but less than 3 hours" = 2, 
                      "Some but less than 1 hour" = 1,
                      "3 hours or more" = 3,
                      "None" = 0)
  
recode_variable(exercise, values_mapping3)

recode_variable(cycling, values_mapping3)

recode_variable(walking, values_mapping3)

recode_variable(housework, values_mapping3)

recode_variable(gardening, values_mapping3)

values_mapping4 <- c( "I spend most of my time at work sitting (such as in an office)" = 0,
 "My work involves definite physical effort including handling of heavy objects and use of tools (e.g. cleaner, hospital nurse, gardener, postal delivery workers etc.)" = 2,
 "I spend most of my time at work standing or walking. However, my work does not require much intense physical effort (e.g. shop assistant, hairdresser, security guard, childminder, etc.)" = 1,
 "I am not in employment (e.g. retired, retired for health reasons, unemployed, full-time carer etc.)" = 0,
 "My work involves vigorous physical activity including handling of very heavy objects (e.g. construction worker, refuse collector, etc.)" = 3,
 "I am not in employment (e.g. stay at home parent, unemployed, full-time carer etc.)" = 0)

recode_variable(work_activity, values_mapping4)

values_mapping10 <- c("Brisk pace" = 2, 
                     "Steady average pace" = 1,
                     "Fast pace" = 3,
                     "Slow pace" = 0)
recode_variable(walking_pace, values_mapping10)

table(surveydata$walking_pace)
surveydata <- surveydata %>%
  mutate(physical_activity_comp = exercise + cycling + walking + 
           housework + gardening + work_activity +walking_pace)

surveydata <- surveydata %>%
  mutate(physical_activity_comp = factor(case_when(
    physical_activity_comp %in% 0:5 ~ "Sedentary",
    physical_activity_comp %in% 6:11 ~ "Moderately Inactive",
    physical_activity_comp %in% 12:17 ~ "Moderately Active",
    physical_activity_comp >= 18 ~ "Active"
  ), levels = c("Moderately Inactive", "Sedentary", "Moderately Active")
  ))

#-------------------------------------------------------------------------------

values_mapping5 <- c( "Prefer not to say" = 0, "No" = 0, "Yes" = 1)

recode_variable(stress_period, values_mapping5)

values_mapping6 <- c("Yes" = 1, "No" = 0)

recode_variable(relationship_difficulties, values_mapping6)

recode_variable(legal_problems, values_mapping6)

recode_variable(crime_victim, values_mapping6)

recode_variable(close_illness, values_mapping6)

recode_variable(close_death, values_mapping6)

recode_variable(covid_stress, values_mapping6)

values_mapping7 <- c("Yes" = -1, "No" = 1)

recode_variable(support, values_mapping7)

surveydata$stress_scale <- as.numeric(ifelse(is.na(surveydata$stress_scale),
                                             0, surveydata$stress_scale))


surveydata <- surveydata %>%
  mutate(stress_comp = stress_period +relationship_difficulties + legal_problems
         + crime_victim + close_illness + close_death + covid_stress + support + 
           stress_scale)

surveydata <- surveydata %>%
  mutate(stress_comp = factor(case_when(
    stress_comp == 0 ~ "No stressful period reported",
    stress_comp %in% 1:6 ~ "Moderate stress",
    stress_comp >= 7 ~ "High stress"
  ), levels = c("No stressful period reported", "Moderate stress", "High stress")
  ))
#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(work_type = factor(case_when(
    is.na(work_type) ~ "Not working/On maternity Leave",
    work_type %in% c("Fixed regular hours part-time", "Variable hours part-time",
                     "When required with no guarantee of hours",
                     "Zero hour contract","I choose my own hours","Self employed. Whenever I.l can"
                     ) ~ "Part time",
    work_type %in% c("Full time",
                     "Usually full time but signed off with hyperemisis gravidarum currently",
                     "35 hours a week") ~ "Full time"
  ), levels = c("Full time", "Part time", "Not working/On maternity Leave")
  ))

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(care_satisfaction = factor(case_when(
    is.na(care_satisfaction) ~ "No information provided",
    care_satisfaction %in% c("4", "5") ~ "Satisfied",
    care_satisfaction == "3" ~ "Neutral",  
    care_satisfaction %in% c("1", "2") ~ "Not satisfied"
  ), levels = c("Satisfied", "Neutral", "Not satisfied", "No information provided")
  ))

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(delivery_change = case_when(
    delivery_change %in% c("No, it has not changed the type of birth I intend to have (home birth, midwife unit, water birth, c-section, hospital consultant led)", 
                           "I have not decided on my birth plan but my choices are being heavily swayed by Covid related worries",
                           "I was able to have the birth I wanted, though the neighbouring hospital shut down the midwife led birth centre and gone birthing team as they were short staffed. I was also warned that the team here were short staffed and so needed to be aware that I might not be able to have what I want when it comes to delivery. I was able to proceed as normal, but this obviously causes some worries when approaching birth.") ~ "didn't change",
    delivery_change %in% c("Yes, my options have been removed and I do not have the choice to have the birth I would chose.", 
                           "Yes, my options have been removed and I do not have the choice to have the birth I would choose.", 
                           "Yes, I have changed the type of birth I intend to have (e.g. home birth, midwife unit, water birth, C-section, hospital consultant lead)", 
                           "Yes, because I can't have who I want with me and have to suffer alone") ~ "changed",
    delivery_change %in% c("I donåt know yet", "Dont know yet", "I am not sure what I want at this current time", 
                           "I donåt know they wonåt talk to me about it until Iåm closer to my due date", 
                           "Not sure", "Not sure how it will affect my birth choices.", "Not sure as of yet", 
                           "I dont know if my option will be removed by the time I am due, but at the moment they have been", 
                           "Hadnåt thought much about type of brith I wanted but main thing is restrictions relating to partner being able to stay throughout", 
                           "I feel that it is too soon for me to answer this question as I am only 10 weeks pregnant") ~ "Not sure",
    is.na(delivery_change) ~ "No information"
  ))

#-------------------------------------------------------------------------------

surveydata <- surveydata %>%
  mutate(
    care_type = case_when(
      care_type %in% c("Midwife led care", "Community midwife/health visitor", 
                       "Early 5 weeks yet to meet midwife", 
                       "Diabetic midwife led care", 
                       "Mainly midwife with consultant towards the end",
                       "Consultant on occassion but midwife led care",
                       "Both. Started with midwife, then referred to consultant and then discharged back to midwife.") ~ "Midwife led care",
      care_type %in% c("Consultant led care", "Both midwife and consultant",
                       "Shared care", "Midwife and Consultant", 
                       "Dual care plus fetal medicine care", 
                       "First appointment next week but will be consultant lead",
                       "Was consultant care", 
                       "Consultant led but at 30 weeks yet to see a consultant", 
                       "Mixture of both", "Midwife and consultant led care", 
                       "Consultant led then midwife led in last weeks", 
                       "Both") ~ "Consultant led care",
      care_type %in% c("I donåt know yet", "Dont know yet", "Not sure yet", 
                       "Not sure yet, I'm yet to have my 12 week scan", 
                       "Awaiting decision based on previous birth", 
                       "Currently both. Will be confirmed which one leads at next scan",
                       "Too early not yet booked will be CLC",
                       "Midwife and present but meeting consultant in june") ~ "Not sure yet",
      is.na(care_type) | care_type %in% c("N/A", "Already have baby", 
                                          "Health visitor", "Rainbow clinic",
                                          "Gave birth 10 months ago", "None",
                                          "Given birth already",
                                          "But both parties believe the other should be caring") ~ "No information"
    )
  )

#-------------------------------------------------------------------------------
# Get a demographic table

# Relationship status
relationship_status_counts <- table(surveydata$relationship_status)
relationship_status_percentages <- prop.table(relationship_status_counts) * 100

# Nationality
nationality_counts <- table(surveydata$Nationality)
nationality_percentages <- prop.table(nationality_counts) * 100

# Ethnic Group
ethnic_group_counts <- table(surveydata$ethnicity)
ethnic_group_percentages <- prop.table(ethnic_group_counts) * 100

# Language
language_counts <- table(surveydata$language)
language_percentages <- prop.table(language_counts) * 100

# Education
education_counts <- table(surveydata$education)
education_percentages <- prop.table(education_counts) * 100

# Employment Status
employment_status_counts <- table(surveydata$employment_status)
employment_status_percentages <- prop.table(employment_status_counts) * 100

# Sexual orientation
sexual_orientation_counts <- table(surveydata$sexual_orientation)
sexual_orientation_percentages <- prop.table(sexual_orientation_counts) * 100

# Annual Household income
annual_household_income_counts <- table(surveydata$annual_household_income)
annual_household_income_percentages <- prop.table(annual_household_income_counts) * 100

# Combine counts and percentages 
demographic_table <- data.frame(
  Characteristic = c("Relationship Status", names(relationship_status_counts),
                     "Nationality", names(nationality_counts),
                     "Ethnic Group", names(ethnic_group_counts),
                     "Language", names(language_counts),
                     "Education", names(education_counts),
                     "Employment Status", names(employment_status_counts),
                     "Sexual Oriention", names(sexual_orientation_counts),
                     "Annual Household income", names(annual_household_income_counts)),
  Count = c("", as.numeric(relationship_status_counts), 
            "", as.numeric(nationality_counts), 
            "", as.numeric(ethnic_group_counts),
            "", as.numeric(language_counts),
            "", as.numeric(education_counts),
            "", as.numeric(employment_status_counts),
            "", as.numeric(sexual_orientation_counts),
            "", as.numeric(annual_household_income_counts)),
  Percentage = c("", round(relationship_status_percentages, 2), 
                 "", round(nationality_percentages, 2), 
                 "", round(ethnic_group_percentages, 2),
                 "", round(language_percentages, 2),
                 "", round(education_percentages, 2),
                 "", round(employment_status_percentages, 2),
                 "", round(sexual_orientation_percentages, 2),
                 "", round(annual_household_income_percentages,2))
)


dataforanalysis <- surveydata %>%
  select(relationship_status, smoking_status, education,BMI_category, work_type
         , mood_comp,anxiety_comp, stress_comp, physical_activity_comp, 
         number_of_children, breastfeeding_intention, annual_household_income)

# Create a frequency table for breastfeeding intentions
table(dataforanalysis$breastfeeding_intention)

save(surveydata ,dataforanalysis, Covid_data, file = "mydata.RData")
