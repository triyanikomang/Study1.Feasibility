# Version: 2.0
#
# Date: October 14, 2025

# Authors: Paraskevi Massara (email: massaraevi@yahoo.com), and edited by Isabella Oberto Monasterios
#
# OBJECTIVES: Complete metadata preparation, calculate descriptive statistics, imputation

# Data from the 2 growth analysis projects (clustering and parental-child BMI
# associations were used, see "Growth Analysis" Folder in the encrypted devices).
# To facilitate this project reproducibility, the intermediate files are provided.
# The data is not publicly available due to privacy concerns.

# ================ Dependencies ============================================

# Packages needed
packages <- c("ggplot2", "dplyr", 'sas7bdat', "lubridate", "arsenal",'vtable', "janitor",
              "gtsummary", "openxlsx", 'tidyr', 'mice', 'naniar', 'glue', 'forcats',"tibble",
              "purrr","stringr")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# ================ 1. Load all data ============================================
# This dataset is produced from scripts 1a and 1b
recruitment <- read.csv("G:/MiGrowD/merged_participants_25-Apr-2025.csv") # merged recruitment file Isabella and Triyani worked on
migrowd <-readRDS("G:/MiGrowD/migrowd_final.rds")
unimputed_crossectional_parents <- readRDS("G:/MiGrowD/Parents_data/unimputed_crossectional_parents.rds")
unimputed_crossectional_parents[, 1] <- paste0("26-", unimputed_crossectional_parents[, 1])  
migrowd2 <- left_join(migrowd, unimputed_crossectional_parents, by = "subject") %>%
  select(-matches("\\.y$"))  # Removes columns from the second dataset that have suffix ".y"
stool_samples <- read.csv("G:/ASA24/stool_samples.csv") #subjects that collected stool samples (104)


# Columns to keep
 
var_names <- c("subject",
               "health_conditions","prebiotics","prebiotics_duration","prebiotics_type",
                 "supplements","other_supplements","special_diet","type_of_restriction_diet",
               "duration_diet_restriction","autism","adhd","prematurity",
                "age_mother_frperiod","exclusion_antibiotics","exclusion_laxatives",
                "exclusion_probiotics","exclusion_gidisorders","pubertal_score","dropout",
                 "class","label","NHQ_GESTATION_AGE","retro_gest","GESTATION_AGE",
                 "FULLTERM_YN","mom_birth_age.x","dad_birth_age.x","birthweight_kilo.x",
                  "CHILDDOBDT1_YYYY","CHILDGENDER1","dadethnicity.x","momethnicity.x",
                  "CHILDIMMIGRATIONSTATUS","COUNTRYBIOFATHERBORN","COUNTRYBIOMOMBORN",
                  "COUNTRYCHILDBORN","DietMom_YN_Red_meat__beef__veal_",
                  "DietMom_YN_Poultry__chicken__tur.x","DietMom_YN_Fish__salmon__halibut.x",
                  "DietMom_YN_Shellfish__lobster__c.x","DietMom_YN_Eggs.x","DietMom_YN_Milk.x",
                  "DietMom_YN_Fruits.x","DietMom_YN_Vegetables.x","DietMom_YN_Cheese.x",
                  "DietMom_YN_Yogurt.x","DietMom_YN_Margarine.x","DietMom_YN_Honey.x",
                  "DietMom_YN_Vegetarian__did_not_e.x","DietMom_YN_Vegan__did_not_eat_re.x",
                  "DietMom_YN_Child_is_adopted__unk","FATHER_EMPLOY_TYPE",
                  "FATHER_EMPLOYED_YN","MOM_EMPLOYED_YN","MOM_EMPLOYMENT_TYPE",
                  "date_birth","age_enrollment","EDUCATIONLEVEL.x","MOTHER_LEVEL_EDUCATION.x",
                  "FAMILY_INCOM.x","TIMEBREASTFED.x","NUTRITIONSCORE.x",
                  "HOURSSLEEPING_145_216.x","HOURSSLEEPING_61_144.x","ZBMI","PARENTBMI",
                  "merged_class","GESTATIONALDIABETES_YN","HIGHBLOODPRESSURE",
                  "NONPRESCRIBEDMEDS_Cigarette","DIAGNOSED_Overweight",
                  "TYPICALWEEKDAYFREEPLAY","MomDiagn_Multiple_Sclerosis","MomDiagn_Diabetes",
                  "MomDiagn_Osteoporosis","MomDiagn_Heart_Disease","MomDiagn_Hypertension",
                  "MomDiagn_High_Cholesterol","MomDiagn_Cancer","MomDiagn_Asthma",
                  "MomDiagn_Depression_Anxiety","MomDiagn_Stroke",
                  "MomDiagn_Alcohol_Drugs_Misuse","MomDiagn_ADHD","MomDiagn_Autism_PDD",
                  "MomDiagn_Learning_Disability","MomDiagn_Overweight_Obesity",
                  "PAST3DAYS_RedMeat","PAST3DAYS_Poultry","PAST3DAYS_Fish",
                  "PAST3DAYS_ShellFish","PAST3DAYS_Egg","PAST3DAYSDAIRY","PAST3DAYS_Fruits",
                  "PAST3DAYS_Veg","PAST3DAYS_Cheese","PAST3DAYS_Yogurt",
                  "PAST3DAYS_GrainProducts","PAST3DAYS_FastFood","PAST3DAYS_Vegetarian",
                  "PAST3DAYS_Vegan","FOODRUNOUT","PAST3DAYS_RedMeat_MOST_OFTEN",
                  "PAST3DAYS_Poultry_MOST_OFTEN","PAST3DAYS_Fish_MOST_OFTEN",
                  "PAST3DAYS_ShellFish_MOST_OFTEN","PAST3DAYS_Egg_MOST_OFTEN",
                  "PAST3DAYSDAIRY_MOST_OFTEN","PAST3DAYS_Fruits_MOST_OFTEN",
                  "PAST3DAYS_Veg_MOST_OFTEN","PAST3DAYS_Cheese_MOST_OFTEN",
                  "PAST3DAYS_Yogurt_MOST_OFTEN","PAST3DAYS_GrainProducts_MOST_OFTEN",
                  "PAST3DAYS_FastFood_MOST_OFTEN","PAST3DAYS_Vegetarian_MOST_OFTEN",
                  "PAST3DAYS_Vegan_MOST_OFTEN","FOODRUNOUT_MOST_OFTEN","DadDiagn_Diabetes",
                  "DadDiagn_Osteoporosis","DadDiagn_Heart_Disease","DadDiagn_Hypertension",
                  "DadDiagn_High_Cholesterol","DadDiagn_Cancer","DadDiagn_Asthma",
                  "DadDiagn_Depression_Anxiety","DadDiagn_Stroke",
                  "DadDiagn_Alcohol_Drugs_Misuse","DadDiagn_ADHD","DadDiagn_Autism_PDD",
                  "DadDiagn_Learning_Disability","DadDiagn_Overweight_Obesity",
                  "HOURSSLEEPING_37_60"
                )

var_types <- c("Categorical",    # subject
     rep("Categorical", 22),   # up to NHQ_GESTATION_AGE
     "Numeric",                # retro_gest
     "Categorical","Categorical",
     rep("Numeric", 4),        # mom_birth_age.x -> CHILDDOBDT1_YYYY
     rep("Categorical", 33),   # diet vars, employ vars, education, income
     "Numeric","Categorical",  # TIMEBREASTFED.x, NUTRITIONSCORE.x
     rep("Numeric", 3),        # sleeping, ZBMI, PARENTBMI
     rep("Categorical", 3),    # merged_class, GDM, BP
     rep("Categorical", 1),    # NONPRESCRIBEDMEDS_Cigarette
     rep("Categorical", 1),    # DIAGNOSED_Overweight
     "Numeric",                # TYPICALWEEKDAYFREEPLAY
     rep("Categorical", 15),   # MomDiagn
     rep("Categorical", 31),   # PAST3DAYS
     rep("Categorical", 15),   # DadDiagn
     "Numeric"                 # HOURSSLEEPING_37_60
   )

migrowd2 <- migrowd2[var_names]

  for (i in seq_along(var_names)) {
       var <- var_names[i]
       if (var %in% names(migrowd2)) {
           if (var_types[i] == "Categorical") {
               migrowd2[[var]] <- as.factor(migrowd2[[var]])
             } else if (var_types[i] == "Numeric") {
               migrowd2[[var]] <- as.numeric(migrowd2[[var]])
               }
         }
  }


# List of columns to convert
numeric_vars <- c(
  "HOURSSLEEPING_37_60",
  "TYPICALWEEKDAYFREEPLAY",
  "HOURSSLEEPING_61_144.x",
  "FAMILY_INCOM.x",
  "TIMEBREASTFED.x",
  "NUTRITIONSCORE.x",
  "date_birth",
  "age_enrollment",
  'age_mother_frperiod'
)


migrowd2$age_mother_frperiod <- gsub("15 YEARS OLD", "15", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("about 15 years", "15", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("12 years old", "12", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("13 years old", "13", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("11 years 6 months", "11.5", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("11-12", "11.5", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("11  or 12", "11.5", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("Not available. Participant filled out assesesment.", " ", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- gsub("13-14", "13.5", migrowd2$age_mother_frperiod, ignore.case = TRUE)
migrowd2$age_mother_frperiod <- as.numeric(migrowd2$age_mother_frperiod)



for (var in numeric_vars) {
  if (var %in% names(migrowd2)) {
    migrowd2[[var]] <- as.numeric(migrowd2[[var]])
  } else {
    message(paste("Column", var, "not found in dataset"))
  }
}


# List of columns to convert to factor
factor_vars <- c("merged_class", "MomDiagn_Multiple_Sclerosis")


for (var in factor_vars) {
  if (var %in% names(migrowd2)) {
    migrowd2[[var]] <- as.factor(migrowd2[[var]])
  } else {
    message(paste("Column", var, "not found in dataset"))
  }
}

migrowd2 <- left_join(migrowd2, stool_samples, by = "subject")
migrowd2$Stool_sample[is.na(migrowd2$Stool_sample)] <- "N"

migrowd2 <- migrowd2 %>%
  left_join(migrowd %>% select(subject, subject_status), by = "subject")

migrowd2 %>%
  group_by(subject_status) %>% 
  count(subject_status) %>%
  arrange(desc(n))# 104 provided sample, 61 no sample, 71 lost-to-follow-up, 17 withdrew, 48 potential participants (not consented/enrolled)

# Remove participants that withdrew, were lost to follow up or not enrolled 

migrowd2 <- migrowd2 %>%
  filter(!subject_status %in% c(3, 4, 5))

migrowd2$age_enrollment <- NULL
migrowd2 <- migrowd2 %>%
  left_join(migrowd %>% select(subject, age_enrollment), by = "subject")



# ================ 2. Explore data =============================================

# Separate numeric and categorical columns
numeric_vars <- migrowd2 %>% select(where(is.numeric))
categorical_vars <- migrowd2 %>% select(where(~!is.numeric(.)))

# ---- Numeric Summary with Normality ----


numeric_summary <- data.frame(
  Variable = character(),
  p_value = numeric(),
  Normal = logical(),
  Mean = numeric(),
  Median = numeric(),
  SD = numeric(),
  SEM = numeric(),
  MissingPct = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(numeric_vars)) {
  x <- numeric_vars[[var]]
  x <- x[!is.na(x)]
  
  if (length(x) > 3) {
    test <- shapiro.test(x)
    is_normal <- test$p.value > 0.05
    
    mean_val <- ifelse(is_normal, round(mean(x), 2), NA)
    median_val <- ifelse(!is_normal, round(median(x), 2), NA)
    sd_val <- round(sd(x), 2) 
    sem_val <- ifelse(!is_normal, round(sd(x) / sqrt(length(x)), 2), NA) 
    missing_pct <- round(sum(is.na(numeric_vars[[var]])) / length(numeric_vars[[var]]) * 100, 2)
    
    numeric_summary <- rbind(numeric_summary,
                             data.frame(
                               Variable = var,
                               p_value = round(test$p.value, 4),
                               Normal = is_normal,
                               Mean = mean_val,
                               Median = median_val,
                               SD = sd_val,
                               SEM = sem_val,
                               MissingPct = missing_pct
                             )
    )
  }
}

# ---- Categorical Summary ----
categorical_summary <- data.frame(
  Variable = character(),
  Levels = character(),
  MissingPct = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(categorical_vars)) {
  x <- categorical_vars[[var]]
  freq <- prop.table(table(x)) * 100
  freq_str <- paste(names(freq), paste0(round(freq, 1), "%"), collapse = "; ")
  missing_pct <- round(sum(is.na(x)) / length(x) * 100, 2)
  
  categorical_summary <- rbind(categorical_summary,
                               data.frame(
                                 Variable = var,
                                 Levels = freq_str,
                                 MissingPct = missing_pct
                               )
  )
}

# ---- Export to Excel with two sheets ----
#write.xlsx(list(Numeric = numeric_summary, Categorical = categorical_summary),
        #   "~/GitHub/MiGrowD/Tables/descriptive_summary.xlsx")


##### ----- Descriptive for participants that provided a stool sample --------##

# Keep only participants with stool samples
migrowd_stool <- migrowd2 %>% filter(Stool_sample == "Y")

# Drop NHQ_GESTATION_AGE if present
if ("NHQ_GESTATION_AGE" %in% names(migrowd_stool)) {
  migrowd_stool$NHQ_GESTATION_AGE <- NULL
}


# Summary 

numeric_summary <- data.frame(
  Variable = character(),
  p_value = numeric(),
  Normal = logical(),
  Mean = numeric(),
  Median = numeric(),
  SD = numeric(),
  SEM = numeric(),
  MissingPct = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(numeric_vars)) {
  x <- numeric_vars[[var]]
  x <- x[!is.na(x)]
  
  if (length(x) > 3) {
    # Check if all values are identical
    if (length(unique(x)) == 1) {
      # Skip Shapiro test, set defaults
      is_normal <- NA
      p_val <- NA
      mean_val <- round(mean(x), 2)
      median_val <- round(median(x), 2)
      sd_val <- 0
      sem_val <- 0
    } else {
      test <- shapiro.test(x)
      p_val <- round(test$p.value, 4)
      is_normal <- p_val > 0.05
      
      mean_val <- ifelse(is_normal, round(mean(x), 2), NA)
      median_val <- ifelse(!is_normal, round(median(x), 2), NA)
      sd_val <- round(sd(x), 2)
      sem_val <- ifelse(!is_normal, round(sd(x) / sqrt(length(x)), 2), NA)
    }
    
    missing_pct <- round(sum(is.na(numeric_vars[[var]])) / length(numeric_vars[[var]]) * 100, 2)
    
    numeric_summary <- rbind(numeric_summary,
                             data.frame(
                               Variable = var,
                               p_value = p_val,
                               Normal = is_normal,
                               Mean = mean_val,
                               Median = median_val,
                               SD = sd_val,
                               SEM = sem_val,
                               MissingPct = missing_pct
                             )
    )
  }
}


# ---- Export to Excel with two sheets ----
#write.xlsx(list(Numeric = numeric_summary, Categorical = categorical_summary), "~/GitHub/MiGrowD/Tables/descriptive_summary_stool_only.xlsx")

# ================ 3. Missingness & Imputation =================================

always_keep <- c("subject","prebiotics_type",  "other_supplements" , "special_diet", "type_of_restriction_diet",
"duration_diet_restriction","exclusion_antibiotics","exclusion_laxatives", "exclusion_probiotics","exclusion_gidisorders")

# 1) Compute per-column missingness (proportion 0–1) and reshape to long format
miss_df <- migrowd_stool %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "prop_missing") %>%
  arrange(desc(prop_missing))

# 2) Select columns to drop: > 50% missing and not in always_keep
to_drop <- miss_df %>%
  filter(prop_missing > 0.50, !variable %in% always_keep) %>%
  pull(variable)

# 3) Create reduced dataset
migrowd_stool_reduced <- migrowd_stool %>%
  select(-all_of(to_drop))

# 4) Report
message(glue("Dropped {length(to_drop)} columns (>50% missing): {paste(to_drop, collapse = ', ')}"))
message(glue("Original columns: {ncol(migrowd_stool)} | Kept: {ncol(migrowd_stool_reduced)}"))

# Optional: inspect remaining missingness
miss_remaining <- miss_df %>% filter(!variable %in% to_drop)
print(miss_remaining)

# Missingness pattern (rows truncated for large datasets)
md.pattern(migrowd_stool, plot = TRUE)


# Detect variable classes and levels
is_numeric <- sapply(migrowd_stool_reduced, is.numeric)
is_factor  <- sapply(migrowd_stool_reduced, is.factor)

# Identify categorical levels
n_levels <- sapply(migrowd_stool_reduced[, is_factor, drop = FALSE], function(x) length(levels(x)))

# OPTIONAL: list ordinal variables explicitly (set as ordered factors beforehand)
ordinal_vars <- c(
  "MOTHER_LEVEL_EDUCATION.x", "FAMILY_INCOM.x",  "EDUCATIONLEVEL.x", "pubertal_score"
)


migrowd_stool_reduced[ordinal_vars] <- lapply(migrowd_stool_reduced[ordinal_vars], function(x) {
  if (!is.ordered(x)) ordered(x)
})

# Build mice method vector and predictor matrix
meth <- make.method(migrowd_stool_reduced)
pred <- make.predictorMatrix(migrowd_stool_reduced)
subject_var <- "subject"
class_var   <- "class"  

# 1) Do not impute IDs or pure labels
vars_no_impute <- c( 
    "subject",
    "health_conditions",
    "prebiotics",
    "prebiotics_duration",
    "prebiotics_type",
    "supplements",
    "other_supplements",
    "special_diet",
    "type_of_restriction_diet",
    "duration_diet_restriction",
    "exclusion_antibiotics",
    "exclusion_laxatives",
    "exclusion_probiotics",
    "exclusion_gidisorders"
  )

meth[vars_no_impute] <- ""        # not imputed
pred[vars_no_impute, ] <- 0       # do not use them to impute others (ID)
pred[, "subject"] <- 0              # ID does not help predict
pred[, "class"] <- 1          # allow outcome to help impute predictors
pred["class", ] <- 0          # but the outcome itself is not imputed

# 2) Assign methods based on type and number of levels
for (v in names(migrowd_stool_reduced)) {
  if (v %in% vars_no_impute) next
  
  if (is_numeric[v]) {
    meth[v] <- "pmm"
  } else if (is_factor[v]) {
    lv <- n_levels[v]
    if (v %in% ordinal_vars) {
      meth[v] <- "polr"           # ordinal logistic
    } else if (lv == 2) {
      meth[v] <- "logreg"         # binary logistic
    } else {
      meth[v] <- "polyreg"        # multinomial logistic
    }
  } else {
    # default fallback (rare): treat as pmm if numeric-like
    meth[v] <- "pmm"
  }
}


# Run multiple imputations
set.seed(42)
imp <- mice(
  migrowd_stool_reduced,
  m = 20,                # number of imputations
  method = meth,
  predictorMatrix = pred,
  maxit = 20,
  printFlag = TRUE
)

# Diagnostics
plot(imp)            # chain convergence
densityplot(imp)     # numeric distribution checks
stripplot(imp)       # categorical checks
# Save imputed dataset
#saveRDS(imp, "G:/MiGrowD/imputed_migrowd_stool.rds")

densityplot(imp, ~ age_mother_frperiod)      
densityplot(imp, ~ ZBMI)     

# Check the summary statistics 

check_imputation_continuous <- function(vars, imp, original_df) {
  out <- data.frame(
    variable   = character(),
    mean_obs   = numeric(),
    sd_obs     = numeric(),
    mean_imp   = numeric(),
    sd_imp     = numeric(),
    ks_stat    = numeric(),
    ks_p       = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (v in vars) {
    # Skip if variable not in data
    if (!v %in% names(original_df)) {
      warning(v, " not found in original data; skipping.")
      next
    }
    
    # Observed
    obs <- original_df[[v]]
    obs <- obs[!is.na(obs)]
    if (length(obs) < 5) {
      warning(v, " has too few observed values (", length(obs), "); KS may be unreliable.")
    }
    
    # Completed values from one imputation for KS
    comp1 <- complete(imp, 1)[[v]]
    
    # Concatenate across all imputations for mean/SD
    comp_list <- lapply(1:imp$m, function(i) complete(imp, i)[[v]])
    comp_all  <- unlist(comp_list)
    
    mean_obs <- mean(obs); sd_obs <- sd(obs)
    mean_imp <- mean(comp_all); sd_imp <- sd(comp_all)
    
    # KS test (guard against NA / low length)
    ks_stat <- NA; ks_p <- NA
    if (length(obs) > 0 && length(na.omit(comp1)) > 0) {
      ks_out <- try(ks.test(obs, comp1), silent = TRUE)
      if (!inherits(ks_out, "try-error")) {
        ks_stat <- as.numeric(ks_out$statistic)
        ks_p    <- as.numeric(ks_out$p.value)
      }
    }
    
    cat("\n=== ", v, " ===\n", sep = "")
    cat("Observed   : mean = ", round(mean_obs, 3), ", sd = ", round(sd_obs, 3), "\n", sep = "")
    cat("Imputed(all): mean = ", round(mean_imp, 3), ", sd = ", round(sd_imp, 3), "\n", sep = "")
    cat("KS stat    = ", round(ks_stat, 3), ", p = ", format.pval(ks_p, digits = 3), "\n", sep = "")
    
    out <- rbind(out, data.frame(
      variable = v,
      mean_obs = mean_obs,
      sd_obs   = sd_obs,
      mean_imp = mean_imp,
      sd_imp   = sd_imp,
      ks_stat  = ks_stat,
      ks_p     = ks_p,
      stringsAsFactors = FALSE
    ))
  }
  rownames(out) <- NULL
  out
}

# ---- USAGE ----

check_imputation_continuous <- function(vars, imp, original_df) {
  # Coerce to character vector of names
  if (is.data.frame(vars) || is.matrix(vars)) vars <- colnames(vars)
  vars <- as.character(vars)
  vars <- intersect(vars, names(original_df))
  if (length(vars) == 0) stop("No valid variable names found in 'vars' that exist in 'original_df'.")
  
  # Helper to coerce numeric safely
  as_numeric_safe <- function(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x))  return(as.numeric(as.character(x)))
    if (is.character(x)) return(suppressWarnings(as.numeric(x)))
    x
  }
  
  out <- data.frame(
    variable   = character(),
    mean_obs   = numeric(),
    sd_obs     = numeric(),
    mean_imp   = numeric(),
    sd_imp     = numeric(),
    ks_stat    = numeric(),
    ks_p       = numeric(),
    prop_imputed = numeric(),
    was_imputed  = logical(),
    stringsAsFactors = FALSE
  )
  
  # Precompute completed datasets
  completed_list <- lapply(1:imp$m, function(i) complete(imp, i))
  
  for (v in vars) {
    # Observed values
    obs <- as_numeric_safe(original_df[[v]])
    obs_nonmiss <- obs[!is.na(obs)]
    
    # Gather completed values across all imputations
    comp_list_v <- lapply(completed_list, function(df) as_numeric_safe(df[[v]]))
    comp_all    <- unlist(comp_list_v)
    
    # Compute means/SDs with NA removal
    mean_obs <- mean(obs_nonmiss, na.rm = TRUE); sd_obs <- sd(obs_nonmiss, na.rm = TRUE)
    mean_imp <- mean(comp_all, na.rm = TRUE);    sd_imp <- sd(comp_all, na.rm = TRUE)
    
    # KS test using first completed dataset (guarded)
    ks_stat <- NA; ks_p <- NA
    comp1 <- comp_list_v[[1]]
    if (length(obs_nonmiss) > 1 && length(na.omit(comp1)) > 1) {
      ks_out <- try(ks.test(obs_nonmiss, comp1), silent = TRUE)
      if (!inherits(ks_out, "try-error")) {
        ks_stat <- as.numeric(ks_out$statistic)
        ks_p    <- as.numeric(ks_out$p.value)
      }
    }
    
    # Was this variable imputed?
    was_imputed <- v %in% names(imp$imp)
    # Proportion of imputed cells in original data
    prop_imputed <- mean(is.na(original_df[[v]]))
    
    cat("\n=== ", v, " ===\n", sep = "")
    cat("Observed     : mean = ", round(mean_obs, 3), ", sd = ", round(sd_obs, 3), "\n", sep = "")
    cat("Completed(all): mean = ", round(mean_imp, 3), ", sd = ", round(sd_imp, 3), "\n", sep = "")
    cat("KS stat      = ", ifelse(is.na(ks_stat), "NA", round(ks_stat, 3)),
        ", p = ", ifelse(is.na(ks_p), "NA", format.pval(ks_p, digits = 3)), "\n", sep = "")
    cat("Missing (orig): ", round(prop_imputed, 3), 
        "; Was imputed by mice: ", was_imputed, "\n", sep = "")
    
    out <- rbind(out, data.frame(
      variable     = v,
      mean_obs     = mean_obs,
      sd_obs       = sd_obs,
      mean_imp     = mean_imp,
      sd_imp       = sd_imp,
      ks_stat      = ks_stat,
      ks_p         = ks_p,
      prop_imputed = prop_imputed,
      was_imputed  = was_imputed,
      stringsAsFactors = FALSE
    ))
  }
  
  rownames(out) <- NULL
  out
}


vars_to_check <- c(
  "age_mother_frperiod","mom_birth_age.x","dad_birth_age.x",
  "birthweight_kilo.x","CHILDDOBDT1_YYYY","date_birth"
)
results_df <- check_imputation_continuous(vars_to_check, imp, migrowd_stool_reduced)

completed_data <- complete(imp, 1)

#write.csv(completed_data, "G:/MiGrowD/imputed_dataset.csv")

# Further trim and polish the imputed dataset as needed for analysis


completed_data <- completed_data[, !names(completed_data) %in% c("X", "X.1")]

# special conditions 
cols <- c(
  "health_conditions",
  "prebiotics",
  "supplements",
  "special_diet",
  "exclusion_antibiotics",
  "exclusion_laxatives",
  "exclusion_probiotics",
  "exclusion_gidisorders"
)

mat <- suppressWarnings(sapply(completed_data[cols], function(x) as.numeric(as.character(x))))
mat[is.na(mat)] <- 0

# 1 if any of the selected columns equals 1 in a row else 0
completed_data$special_cond <- as.integer(rowSums(mat == 1) > 0)


# Remove only a trailing ".x" 
names(completed_data) <- sub("\\.x$", "", names(completed_data))
names(completed_data) <- gsub("_+", "_", names(completed_data))
names(completed_data) <- sub("^_+", "", names(completed_data))
names(completed_data) <- sub("_+$", "", names(completed_data))

# Save final imputed dataset
#write.csv(completed_data, "G:/MiGrowD/imputed_dataset.csv", row.names = FALSE)

completed_data <- read.csv("G:/MiGrowD/imputed_dataset.csv")














