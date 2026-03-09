# Date: March 6, 2026

# Authors: Komang Triyani Kartinawati
#
# OBJECTIVES: Comparing stool sample collection (Completed and Incomplete) among subgroups

# File: migrowd_final.RDS

migrowd_final <- readRDS("C:/Users/ameth/Desktop/THESIS/Study 1/diet.data/migrowd_final.rds")
View(migrowd_final)

### -------------------------- Age ----------------------------------###
# Include Age as new column in the "migrowd_final" dataset
library(dplyr)
df1 <- migrowd_final %>%
  mutate(
    date_birth               = as.Date(date_birth),
    enrol_date               = as.Date(enrol_date),
    age_participants         = as.numeric(difftime(enrol_date, date_birth, units = "days")) / 365.25,
    age_participants_rounded = round(age_participants)   
  )
# Check first few rows
head(df1[, c("date_birth", "enrol_date", "age_participants", "age_participants_rounded")])
# Quick summary
summary(df1$age_participants_rounded)

# Compliance across age: 8, 9, 10, 11, 12, >12
compliance_by_age <- df1 %>%                          
  group_by(age_participants_rounded) %>%
  summarise(
    n          = n(),
    stool_collection_status = sum(stool_collection_status, na.rm = TRUE),        
    rate       = round(mean(stool_collection_status) * 100, 1)       
  )

print(compliance_by_age)

# Contingency table and Chisquare Test
age_table <- table(df1$age_participants_rounded, df1$stool_collection_status)
print(age_table)
chisq.test(age_table)
mean(age_table)

### -------------------------- Child sex ----------------------------------###
male   <- df1 %>% filter(CHILDGENDER1 == "Male")
female <- df1 %>% filter(CHILDGENDER1 == "Female")

# prop.test: successes and total n per group
childsex_table <- table(df1$CHILDGENDER1, df1$stool_collection_status)                                   
print(childsex_table)

prop.test(childsex_table)


### -------------------------- Child BMI ----------------------------------###
# Compliance across BMI: Normal. Overweight, Obesity
compliance_by_BMI <- df1 %>%                          
  group_by(label.x) %>%
  summarise(
    n          = n(),
    stool_collection_status = sum(stool_collection_status, na.rm = TRUE),        
    rate       = round(mean(stool_collection_status) * 100, 1)       
  )

print(compliance_by_BMI)

# Contingency table
BMI_table <- table(df1$label.x, df1$stool_collection_status)
print(BMI_table) #the numbers of Obese < 5
fisher.test(BMI_table)

### -------------------------- Education Level ----------------------------------###
# Compliance across Education Level: College, High School, Public School
compliance_by_education <- df1 %>%                          
  group_by(EDUCATIONLEVEL) %>%
  summarise(
    n          = n(),
    stool_collection_status = sum(stool_collection_status, na.rm = TRUE),        
    rate       = round(mean(stool_collection_status) * 100, 1)       
  )

print(compliance_by_education)
education_table <- table(df1$EDUCATIONLEVEL, df1$stool_collection_status)
print(education_table) #the numbers of Public School <5

fisher.test(education_table)

### -------------------------- Mother Employment ----------------------------------###
Emplyed   <- df1 %>% filter(MOM_EMPLOYED_YN == "Yes")
Unemplyed <- df1 %>% filter(MOM_EMPLOYED_YN == "No")

# prop.test: successes and total n per group
MomEmployed_table <- table(df1$MOM_EMPLOYED_YN, df1$stool_collection_status)                                   
print(MomEmployed_table)

prop.test(MomEmployed_table)

### -------------------------- Mother Ethnicity ----------------------------------###
# Compliance across ethnicity: 
compliance_by_momethnicity <- df1 %>%                          
  group_by(momethnicity) %>%
  summarise(
    n          = n(),
    stool_collection_status = sum(stool_collection_status, na.rm = TRUE),        
    rate       = round(mean(stool_collection_status) * 100, 1)       
  )

print(compliance_by_momethnicity)
momethnicity_table <- table(df1$momethnicity, df1$stool_collection_status)
print(momethnicity_table) #some ethnicities numbers are <5

fisher.test(momethnicity_table)

### -------------------------- Dad Ethnicity ----------------------------------###
# Compliance across ethnicity: 
compliance_by_dadethnicity <- df1 %>%                          
  group_by(dadethnicity) %>%
  summarise(
    n          = n(),
    stool_collection_status = sum(stool_collection_status, na.rm = TRUE),        
    rate       = round(mean(stool_collection_status) * 100, 1)       
  )

print(compliance_by_dadethnicity)
dadethnicity_table <- table(df1$dadethnicity, df1$stool_collection_status)
print(dadethnicity_table) #some ethnicities numbers are <5

fisher.test(dadethnicity_table)

### -------------------------- Family Income ----------------------------------###
# Categorizing Family Income
df1 <- migrowd_final
library(dplyr)
df1 <- df1 %>%
  mutate(INCOME = case_when(
    FAMILY_INCOM %in% c("$10, 000 to $19, 999", "$20, 000 to $29, 999", "$30, 000 to $39, 999", "$40, 000 to $49, 999", "$50, 000 to $59, 999", "$60, 000 to $79, 999", "$80, 000 to $99, 999", "Less than $10, 000") ~ "Less than $100, 000",
    FAMILY_INCOM %in% c("$100, 000 to $149, 999") ~ "$100, 000 to $149, 999",
    FAMILY_INCOM %in% c("$150, 000 or more", "$150, 000 to $199, 999") ~ "$150, 000 to $199, 999",
    FAMILY_INCOM %in% c("$200, 000 to $299, 999") ~ "$200, 000 to $299, 999",
    FAMILY_INCOM %in% c("$300, 000 to $499, 999") ~ "$300, 000 to $499, 999",
    FAMILY_INCOM %in% c("$500, 000 or more") ~ "$500, 000 or more",
    TRUE ~ "Other"  
  ))
df1$FAMILY_INCOM <- factor(df1$FAMILY_INCOM, 
                           levels = c("Less than $100, 000", "$100, 000 to $149, 999", "$150, 000 to $199, 999", 
                                      "$200, 000 to $299, 999", "$300, 000 to $499, 999", "$500, 000 or more")) 
table(df1$FAMILY_INCOM)

# Compliance proportion by income group
compliance_by_income <- df1 %>%
  group_by(FAMILY_INCOM) %>%
  summarise(
    n         = n(),
    collected = sum(stool_collection_status, na.rm = TRUE),   
    not_collected = n - collected,
    rate      = round(mean(stool_collection_status, na.rm = TRUE) * 100, 1)
  )

print(compliance_by_income)
FamilyIncome_table <- table(df1$FAMILY_INCOM, df1$stool_collection_status)
print(FamilyIncome_table) # Numbers in the "Less than $100, 000" and "$500, 000 or more" are <5
fisher.test(FamilyIncome_table)

### -------------------------- Site Recruitment ----------------------------------###
df3 <- migrowd_final
clinic   <- df3 %>% filter(recruit_type == "Clinic")
remote <- df3 %>% filter(recruit_type == "Home")

# prop.test: successes and total n per group
site_table <- table(df3$recruit_type, df3$stool_collection_status)                                   
print(site_table)

prop.test(site_table)

