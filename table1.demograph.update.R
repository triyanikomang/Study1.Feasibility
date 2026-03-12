# Date: January 30, 2026

# Authors: Komang Triyani Kartinawati
#
# OBJECTIVES: Descriptive analysis of participants characteristics, recruitment,
# and study completion.

# File: migrowd_final.RDS

migrowd_final <- readRDS("C:/Users/ameth/Desktop/THESIS/Study 1/diet.data/migrowd_final.rds")
View(migrowd_final)

###================= Table 1. Sociodemographics characteristics =============####
#1.Age of participants
df1 <- migrowd_final #Read from CSV file
df1$date_birth <- as.Date(df1$date_birth) #Convert character columns to Date format
df1$enrol_date <- as.Date(df1$enrol_date) #Convert character columns to Date format
df1$age_participants <- as.numeric(difftime(df1$enrol_date, df1$date_birth, units = "days")) / 365.25 #Calculate age for all rows
round(df1$age_participants) #round age
df2 <- round(df1$age_participants)
table(df2) #count numbers in each age
prop.table(table(df2)) *100 #percentage
round(prop.table(table(df2)) *100, 1)
sum(is.na(df2)) #count numbers of NAs

#checking on mean, sd, 95% CI
mean(df2, na.rm = TRUE)
mean_val <- mean(df2, na.rm = TRUE)
sd_val <- sd(df2, na.rm = TRUE)
n <- sum(!is.na(df2))
se <- sd_val / sqrt(n)
ci_lower <- mean_val - 1.96 * se
ci_upper <- mean_val + 1.96 * se
mean_val
sd_val
n
se
ci_lower
ci_upper

#2.Child gender
View(migrowd_final) #Read from CSV file
table(migrowd_final$CHILDGENDER1) #count numbers in each gender
prop.table(table(migrowd_final$CHILDGENDER1)) *100 #percentage
round(prop.table(table(migrowd_final$CHILDGENDER1)) *100, 1)
sum(is.na(migrowd_final$CHILDGENDER1)) #count numbers of NAs

#3.Child BMI
table(migrowd_final$label.x) #count numbers in each gender
prop.table(table(migrowd_final$label.x)) *100 #percentage
round(prop.table(table(migrowd_final$label.x)) *100, 1)
sum(is.na(migrowd_final$label.x)) #count numbers of NAs

#4.Child Weight
table(migrowd_final$child_weight_kg) 
prop.table(table(migrowd_final$label.x)) *100 #percentage
round(prop.table(table(migrowd_final$label.x)) *100, 1)
sum(is.na(migrowd_final$child_weight_kg)) #count numbers of NAs

#5. Education Level
table(migrowd_final$EDUCATIONLEVEL) #count numbers in each gender
round(prop.table(table(migrowd_final$EDUCATIONLEVEL)) *100, 1)
sum(is.na(migrowd_final$EDUCATIONLEVEL)) #count numbers of NAs

#6. Mother Employment
table(migrowd_final$MOM_EMPLOYED_YN) 
round(prop.table(table(migrowd_final$MOM_EMPLOYED_YN)) *100, 1)
sum(is.na(migrowd_final$MOM_EMPLOYED_YN)) #count numbers of NAs

#7. Father Ethnicity
#Refer to Ethnicity Code
table(migrowd_final$dadethnicity)
round(prop.table(table(migrowd_final$dadethnicity)) *100, 1)
sum(is.na(migrowd_final$dadethnicity))

#8. Mother Ethnicity
#Refer to Ethnicity Code
table(migrowd_final$momethnicity)
round(prop.table(table(migrowd_final$momethnicity)) *100, 1)
sum(is.na(migrowd_final$momethnicity))

#9. FAMILY INCOME
table(migrowd_final$FAMILY_INCOM)
library(dplyr)
df6 <- data.frame(FAMILY_INCOM = migrowd_final$FAMILY_INCOM)
df6 <- df6 %>%
  mutate(INCOME = case_when(
    FAMILY_INCOM %in% c("$10, 000 to $19, 999", "$20, 000 to $29, 999", "$30, 000 to $39, 999", "$40, 000 to $49, 999", "$50, 000 to $59, 999", "$60, 000 to $79, 999", "$80, 000 to $99, 999", "Less than $10, 000") ~ "Less than $100, 000",
    FAMILY_INCOM %in% c("$100, 000 to $149, 999") ~ "$100, 000 to $149, 999",
    FAMILY_INCOM %in% c("$150, 000 or more", "$150, 000 to $199, 999") ~ "$150, 000 to $199, 999",
    FAMILY_INCOM %in% c("$200, 000 to $299, 999") ~ "$200, 000 to $299, 999",
    FAMILY_INCOM %in% c("$300, 000 to $499, 999") ~ "$300, 000 to $499, 999",
    FAMILY_INCOM %in% c("$500, 000 or more") ~ "$500, 000 or more",
    TRUE ~ "Other"  # For any unclassified countries
  ))
table(df6$INCOME)
round(prop.table(table(df6$INCOME)) *100, 1)
sum(is.na(df6$INCOME)) #count numbers of NAs
sum(is.na(migrowd_final$FAMILY_INCOM)) #count numbers of NAs

###================= Table 2. Recruitment and Withdrew =============####
View(migrowd_final)

#1 Recruitment approach
table(migrowd_final$recruit_type)
round(prop.table(table(migrowd_final$recruit_type)) *100, 1)
sum(is.na(migrowd_final$recruit_type)) #missing value = 10

#2 Recruitment issues
table(migrowd_final$recruit_issues)
migrowd_final$recruit_issues <- ifelse(migrowd_final$recruit_issues %in% c(8, 9, 10, 11), 7, migrowd_final$recruit_issues)
table(migrowd_final$recruit_issues)
round(prop.table(table(migrowd_final$recruit_issues)) *100, 1)
sum(is.na(migrowd_final$recruit_issues)) #missing value = 13

#3 Withdrawal
recruitment_log <- read_csv("C:/Users/ameth/Desktop/THESIS/Study 1/diet.data/recruitment_log.csv")
View(recruitment_log)
table(recruitment_log$withdrew) #error category of "11/23/2020", "2020-10-20\n2020-11-04"
recruitment_log$withdrew <- ifelse(recruitment_log$withdrew %in% c("11/23/2020", "2020-10-20\n2020-11-04"), 
                                   TRUE, 
                                   recruitment_log$withdrew)
table(recruitment_log$withdrew) #17 withdrew from study
round(prop.table(table(recruitment_log$withdrew)) *100, 1)

#4 Lost to follow up
table(recruitment_log$subject_status) #lost to follow up (code 4) = 72
sum(is.na(recruitment_log$subject_status)) #missing value = 10

#5 Followup Issues 
table(recruitment_log$followup_issues)
library(dplyr)
recruitment_log <- recruitment_log %>%
  mutate(followup_issues = case_when(
    followup_issues == "3, 5" ~ "3",
    followup_issues == "3, 6" ~ "6",
    followup_issues == "5, 7" ~ "7",
    followup_issues == "6, 7" ~ "7",
    followup_issues == "6.7" ~ "7",
    TRUE ~ followup_issues
  ))
round(prop.table(table(recruitment_log$followup_issues)) *100, 1)

###================= Table 3. Completion characteristics for stool sample collection  =============####
View(migrowd_final)

#6 Completed stool collection
View(migrowd_final)
table(migrowd_final$stool_collection_status)
round(prop.table(table(migrowd_final$stool_collection_status)) *100, 1)

#7 Stool collection issues
table(migrowd_final$sample_issues)
migrowd_final$sample_issues <- ifelse(migrowd_final$sample_issues %in% c(9, 10, 11), 8, migrowd_final$sample_issues)
table(migrowd_final$sample_issues)
round(prop.table(table(migrowd_final$sample_issues)) *100, 1)

#8 Number of days to complete stool collection
df7 <- migrowd_final #Read from CSV file
df7$enrol_date <- as.Date(df7$enrol_date) #Convert character columns to Date format
df7$stool_received <- as.Date(df7$stool_received) #Convert character columns to Date format
df7$days_difference <- as.numeric(difftime(df7$stool_received, df7$enrol_date, units = "days")) #Calculate age for all rows
table(df7$days_difference)
library(dplyr)
df7 <- df7 %>%
  mutate(days_category = case_when(
    days_difference >= 0 & days_difference <= 169 ~ "180",
    days_difference >= 205 & days_difference <= 249 ~ "365",
    days_difference > 365 ~ "more than 365",
    TRUE ~ as.character(days_difference)  
  ))
table(df7$days_category)
round(prop.table(table(df7$days_category)) *100, 1)
sum(is.na(df7$days_category)) #NAs = 159

#9 Interval of ASA24 and Stool collection
df8 <- migrowd_final 
df8$asa1_completed<- as.Date(df8$asa1_completed) #Convert character columns to Date format
df8$stool_received <- as.Date(df8$stool_received) #Convert character columns to Date format
df8$days_difference <- as.numeric(difftime(df8$stool_received, df8$asa1_completed, units = "days")) #Calculate age for all rows
table(df8$days_difference)
library(dplyr)
df8 <- df8 %>%
  mutate(days_category = case_when(
    days_difference >= 0 & days_difference <= 7 ~ "1 week",
    days_difference >= 7 & days_difference <= 14 ~ "2 week",
    days_difference >= 14 & days_difference <= 30 ~ "4 week",
    days_difference >= 30 & days_difference <= 60 ~ "8 week",
    days_difference >= 60 & days_difference <= 90 ~ "12 week",
    days_difference > 90 ~ "more than 3 months",
    days_difference < 0 ~ "incorrect protocols",
    TRUE ~ as.character(days_difference)  
  ))
table(df8$days_category) #stool completed 1 - 326 days before ASA24 = 14
round(prop.table(table(df8$days_category)) *100, 1)
sum(is.na(df7$days_category)) #NAs = 159

###================= Table 4. Completion of ASA24 =============####
View(migrowd_final)

#1 Completed first ASA24 and second ASA24
#1a. First ASA24
table(migrowd_final$asa1_status)
round(prop.table(table(migrowd_final$asa1_status)) *100, 1)
#1b. Second ASA24
table(migrowd_final$asa2_status)
round(prop.table(table(migrowd_final$asa2_status)) *100, 1)

#2 Number of recalls
#2a. First ASA24
table(migrowd_final$asa1_remind_ct) #participants with no recalls will be count as 1
migrowd_final$asa1_remind_ct <- ifelse(migrowd_final$asa1_remind_ct %in% c(0), 1, migrowd_final$asa1_remind_ct)
table(migrowd_final$asa1_remind_ct)
round(prop.table(table(migrowd_final$asa1_remind_ct)) *100, 1)

#1b. Second ASA24
table(migrowd_final$asa2_remind_ct)
migrowd_final$asa2_remind_ct <- ifelse(migrowd_final$asa2_remind_ct %in% c(0), 1, migrowd_final$asa2_remind_ct)
table(migrowd_final$asa2_remind_ct)
round(prop.table(table(migrowd_final$asa2_remind_ct)) *100, 1)

#3. Number of recalls to complete ASA24
#3a. First ASA24: Contingency table
table(migrowd_final$asa1_remind_ct, migrowd_final$asa1_status)
prop.table(table(migrowd_final$asa1_remind_ct, migrowd_final$asa1_status), margin = 1) * 100

#3b. Second ASA24: Contingency table
table(migrowd_final$asa2_remind_ct, migrowd_final$asa2_status)
prop.table(table(migrowd_final$asa1_remind_ct, migrowd_final$asa1_status), margin = 1) * 100

#4 Interval between enrolment to first ASA24 completion
df9 <- migrowd_final #Read from CSV file
df9$asa1_completed<- as.Date(df9$asa1_completed) #Convert character columns to Date format
df9$enrol_date <- as.Date(df9$enrol_date) #Convert character columns to Date format
df9$days_difference <- as.numeric(difftime(df9$asa1_completed, df9$enrol_date, units = "days")) #Calculate age for all rows
table(df9$days_difference)
library(dplyr)
df9 <- df9 %>%
  mutate(days_category = case_when(
    days_difference >= 0 & days_difference <= 7 ~ "1 week",
    days_difference >= 7 & days_difference <= 14 ~ "2 week",
    days_difference >= 14 & days_difference <= 30 ~ "4 week",
    days_difference >= 30 & days_difference <= 60 ~ "8 week",
    days_difference > 60 ~ "More than 2 months",
    days_difference < 0 ~ "incorrect protocols",
    TRUE ~ as.character(days_difference)  
  ))
table(df9$days_category) #stool completed 1 - 326 days before ASA24 = 4
round(prop.table(table(df9$days_category)) *100, 1)
sum(is.na(df9$days_category)) #NAs = 141

#5 Interval between enrolment to second ASA24 completion
df9 <- migrowd_final #Read from CSV file
df9$asa2_completed<- as.Date(df9$asa2_completed) #Convert character columns to Date format
df9$enrol_date <- as.Date(df9$enrol_date) #Convert character columns to Date format
df9$days_interval <- as.numeric(difftime(df9$asa2_completed, df9$enrol_date, units = "days")) #Calculate age for all rows
table(df9$days_interval)
library(dplyr)
df9 <- df9 %>%
  mutate(days_interval = case_when(
    days_interval >= 0 & days_interval <= 14 ~ "2 week",
    days_interval >= 14 & days_interval <= 30 ~ "4 week",
    days_interval >= 30 & days_interval <= 60 ~ "8 week",
    days_interval > 60 ~ "More than 2 months",
    days_interval < 0 ~ "incorrect protocols",
    TRUE ~ as.character(days_interval)  
  ))
table(df9$days_interval) #stool completed 1 - 326 days before ASA24 = 1
round(prop.table(table(df9$days_interval)) *100, 1)
sum(is.na(df9$days_category)) #NAs = 141

#6. ASA24 issues
table(migrowd_final$asa_issues)
df9 <- migrowd_final
library(dplyr)
df9 <- df9 %>%
  mutate(asa_issues = case_when(
    asa_issues == "1, 4" ~ "4",
    asa_issues == "2, 5" ~ "2",
    asa_issues == "4, 7, 9" ~ "9",
    asa_issues == "4, 9" ~ "4",
    asa_issues == "5, 9" ~ "9",
    TRUE ~ asa_issues
  ))
table(df9$asa_issues)
round(prop.table(table(df9$asa_issues)) *100, 1)