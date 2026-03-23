# Version: 2.0
#
# Date: March 12, 2026

# Authors: Komang Triyani (email: triyanikomang.kartinawati@mail.utoronto.ca)
# OBJECTIVES: Preprocess MiGrowD data to prepare them for data cleaning, handling missing
# data, and imputation

# RESOURCES: migrowd_final.RDS

View(migrowd_final)

#Create a new column for Age of participants
df1 <- migrowd_final #Read from CSV file
df1$date_birth <- as.Date(df1$date_birth) #Convert character columns to Date format
df1$enrol_date <- as.Date(df1$enrol_date) #Convert character columns to Date format
df1$age_participants <- as.numeric(difftime(df1$enrol_date, df1$date_birth, units = "days")) / 365.25 #Calculate age for all rows
round(df1$age_participants) #round age
library(dplyr)
df1 <- df1 |> mutate(age_rounded = round(age_participants))
summary(df1)

# Selecting column for predictors and outcome
# Columns to keep
migrowd_prep <- df1
names(migrowd_prep)
cols_to_keep <- c("subject", "enrol_date", "recruit_type", "stool_collection_status",
                  "child_weight_kg", "pubertal_score", "sex", "home_address", "special_diet",
                  "label.x", "GESTATION_AGE", "dadethnicity", "momethnicity", "MOM_EMPLOYED_YN",
                  "EDUCATIONLEVEL", "FAMILY_INCOM", "age_rounded")

migrowd_prep <- migrowd_prep[, cols_to_keep]
summary(migrowd_prep)

#save the new dataset
saveRDS(migrowd_prep, "migrowd_prep.rds")

# Review  missing values per column
missing_summary <- data.frame(
  missing_count = colSums(is.na(migrowd_prep)),
  missing_pct   = round(colSums(is.na(migrowd_prep)) / nrow(migrowd_prep) * 100, 1)
)
missing_summary[order(-missing_summary$missing_count), ]

# Visual overview (optional)
install.packages("visdat")
library(visdat)
vis_miss(migrowd_prep)

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

### A. Imputation with median and mode
migrowd_prep_clean <- migrowd_prep |>
# Impute numeric columns with median
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
# Impute character columns with mode
  mutate(across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .))) |>
# Impute factor columns with mode
  mutate(across(where(is.factor), ~ {
    mode_val <- get_mode(as.character(.))
    replace(., is.na(.), mode_val)
  }))

colSums(is.na(migrowd_prep_clean))
migrowd_prep_clean$enrol_date <- NULL
migrowd_prep_clean$GESTATION_AGE <- NULL
saveRDS(migrowd_prep_clean, "migrowd_prep_clean.rds")


### B.Imputation with KNN
install.packages("VIM")
library(VIM)
migrowd_prep$enrol_date <- NULL
migrowd_prep$GESTATION_AGE <- NULL
migrowd_prep_KNN <- kNN(migrowd_prep, k = 5, imp_var = FALSE)
saveRDS(migrowd_prep_KNN, "migrowd_prep_KNN.rds")

### C.Imputation with MICE
install.packages("mice")
library(mice)
migrowd_prep$enrol_date <- NULL
migrowd_prep$GESTATION_AGE <- NULL
imputed <- mice(migrowd_prep, m = 5, method = "pmm", seed = 123)
migrowd_prep_mice <- complete(imputed, 1)
# Check imputation
plot(imputed)
saveRDS(migrowd_prep_mice, "migrowd_prep_mice.rds")


# ============================================================
# IMPUTATION COMPARISON — MiGrowD
# Compares: migrowd_prep_clean, migrowd_prep_KNN,migrowd_prep_mice
#           against original: migrowd_prep
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# ── Colour palette (your original scheme) ───────────────────
method_colors <- c(
  "Original"     = "#607D8B",
  "Mode/Median"  = "#2196F3",
  "KNN"          = "#FF5722",
  "MICE"         = "#4CAF50"
)

# ============================================================
# STEP 0 — ALIGN COLUMNS
# migrowd_prep had enrol_date & GESTATION_AGE removed during
# imputation, so drop them from the original before comparing
# ============================================================

original <- migrowd_prep %>%
  select(-any_of(c("enrol_date", "GESTATION_AGE")))

# ============================================================
# STEP 1 — MISSING VALUE CHECK
# Confirm all imputed datasets have zero missing values
# ============================================================

missing_check <- data.frame(
  Method   = c("Original", "Mode/Median", "KNN", "MICE"),
  Missing  = c(
    sum(is.na(original)),
    sum(is.na(migrowd_prep_clean)),
    sum(is.na(migrowd_prep_KNN)),
    sum(is.na(migrowd_prep_mice))
  )
)
print(missing_check)

# ============================================================
# STEP 2 — NUMERIC DISTRIBUTION SUMMARY
# Compare mean, median, SD per numeric column per method
# ============================================================
summarise_numeric <- function(df, label) {
  df %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(),
                     list(mean   = ~ mean(., na.rm = TRUE),
                          median = ~ median(., na.rm = TRUE),
                          sd     = ~ sd(., na.rm = TRUE)),
                     .names = "{.col}__{.fn}")) %>%
    mutate(method = label)
}

# Check which dataset is NULL
is.null(original)
is.null(migrowd_prep_clean)
is.null(migrowd_prep_KNN)
is.null(migrowd_prep_mice)


numeric_summary <- bind_rows(
  summarise_numeric(original,           "Original"),
  summarise_numeric(migrowd_prep_clean, "Mode/Median"),
  summarise_numeric(migrowd_prep_KNN,   "KNN"),
  summarise_numeric(migrowd_prep_mice,  "MICE")
) %>%
  pivot_longer(-method, names_to = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(numeric_summary)

# ============================================================
# STEP 3 — JS DIVERGENCE (numeric columns only)
# Measures how much each imputed distribution shifted
# from the original. Lower = better.
# ============================================================

js_divergence <- function(p, q) {
  p <- p + 1e-10; q <- q + 1e-10
  p <- p / sum(p); q <- q / sum(q)
  m <- 0.5 * (p + q)
  0.5 * sum(p * log(p / m)) + 0.5 * sum(q * log(q / m))
}

compute_jsd <- function(orig_df, imp_df, label) {
  num_cols <- names(orig_df)[sapply(orig_df, is.numeric)]
  
  purrr::map_dfr(num_cols, function(col) {
    orig_vals <- na.omit(orig_df[[col]])
    imp_vals  <- imp_df[[col]]
    breaks    <- seq(min(c(orig_vals, imp_vals)),
                     max(c(orig_vals, imp_vals)), length.out = 31)
    p <- hist(orig_vals, breaks = breaks, plot = FALSE)$counts
    q <- hist(imp_vals,  breaks = breaks, plot = FALSE)$counts
    tibble(method = label, variable = col, JSD = js_divergence(p, q))
  })
}

jsd_results <- bind_rows(
  compute_jsd(original, migrowd_prep_clean,  "Mode/Median"),
  compute_jsd(original, migrowd_prep_KNN,    "KNN"),
  compute_jsd(original, migrowd_prep_mice,   "MICE")
)

# Summary: mean JSD per method (lower = closer to original)
jsd_summary <- jsd_results %>%
  group_by(method) %>%
  summarise(mean_JSD = round(mean(JSD), 4)) %>%
  arrange(mean_JSD)

print(jsd_summary)

# ============================================================
# STEP 4 — CATEGORICAL DISTRIBUTION COMPARISON
# For each factor/character column, compare category proportions
# ============================================================

compare_categorical <- function(orig_df, imp_df, label) {
  cat_cols <- names(orig_df)[sapply(orig_df, function(x)
    is.factor(x) | is.character(x))]
  
  purrr::map_dfr(cat_cols, function(col) {
    orig_tbl <- prop.table(table(orig_df[[col]], useNA = "no"))
    imp_tbl  <- prop.table(table(imp_df[[col]],  useNA = "no"))
    all_cats <- union(names(orig_tbl), names(imp_tbl))
    
    orig_vec <- as.numeric(orig_tbl[all_cats]); orig_vec[is.na(orig_vec)] <- 0
    imp_vec  <- as.numeric(imp_tbl[all_cats]);  imp_vec[is.na(imp_vec)]   <- 0
    
    tibble(
      method   = label,
      variable = col,
      TVD      = 0.5 * sum(abs(orig_vec - imp_vec))  # Total Variation Distance
    )
  })
}

cat_results <- bind_rows(
  compare_categorical(original, migrowd_prep_clean,  "Mode/Median"),
  compare_categorical(original, migrowd_prep_KNN,    "KNN"),
  compare_categorical(original, migrowd_prep_mice,   "MICE")
)

# Summary: mean TVD per method (lower = closer to original)
cat_summary <- cat_results %>%
  group_by(method) %>%
  summarise(mean_TVD = round(mean(TVD), 4)) %>%
  arrange(mean_TVD)

print(cat_summary)

# ============================================================
# STEP 5 — VISUALISATIONS
# ============================================================

# Build combined long dataframe for plotting
num_cols <- names(original)[sapply(original, is.numeric)]

plot_data <- bind_rows(
  original            %>% select(all_of(num_cols)) %>% mutate(method = "Original"),
  migrowd_prep_clean  %>% select(all_of(num_cols)) %>% mutate(method = "Mode/Median"),
  migrowd_prep_KNN    %>% select(all_of(num_cols)) %>% mutate(method = "KNN"),
  migrowd_prep_mice   %>% select(all_of(num_cols)) %>% mutate(method = "MICE")
) %>%
  pivot_longer(-method, names_to = "variable", values_to = "value") %>%
  mutate(method = factor(method,
                         levels = c("Original","Mode/Median","KNN","MICE")))

# 5a. Density plots — shape of distribution per numeric column
ggplot(plot_data, aes(x = value, color = method, fill = method)) +
  geom_density(alpha = 0.12, linewidth = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  scale_color_manual(values = method_colors) +
  scale_fill_manual(values  = method_colors) +
  labs(title    = "Density comparison: original vs imputed (MiGrowD)",
       subtitle = "Closer overlap with Original = better imputation",
       color = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# 5b. Boxplots — spread and outliers
ggplot(plot_data, aes(x = method, y = value, fill = method)) +
  geom_boxplot(outlier.size = 0.6, alpha = 0.8) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = method_colors) +
  labs(title = "Distribution spread by imputation method (MiGrowD)",
       x = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_blank(), legend.position = "bottom")

# 5c. JSD bar chart — which method preserves numeric distributions best
ggplot(jsd_results, aes(x = reorder(method, JSD), y = JSD, fill = method)) +
  geom_col(alpha = 0.85, width = 0.6) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = method_colors) +
  coord_flip() +
  labs(title    = "JS Divergence per variable (lower = closer to original)",
       x = NULL, y = "JSD", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# 5d. TVD bar chart — which method preserves categorical distributions best
ggplot(cat_results, aes(x = reorder(method, TVD), y = TVD, fill = method)) +
  geom_col(alpha = 0.85, width = 0.6) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = method_colors) +
  coord_flip() +
  labs(title    = "Total Variation Distance per variable (lower = closer to original)",
       x = NULL, y = "TVD", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# ============================================================
# STEP 6 — FINAL COMPOSITE RANKING
# Combines JSD (numeric) + TVD (categorical) into one score
# ============================================================

normalise <- function(x) (x - min(x)) / (max(x) - min(x) + 1e-10)

final_ranking <- jsd_summary %>%
  left_join(cat_summary, by = "method") %>%
  mutate(
    norm_jsd = normalise(mean_JSD),
    norm_tvd = normalise(mean_TVD),
    composite_score = round((norm_jsd + norm_tvd) / 2, 4)
  ) %>%
  arrange(composite_score)

print(final_ranking)
cat("\n>>> Recommended imputation method:", final_ranking$method[1], "\n")

# Final composite score chart
ggplot(final_ranking,
       aes(x = reorder(method, composite_score),
           y = composite_score, fill = method)) +
  geom_col(alpha = 0.85, width = 0.55) +
  geom_text(aes(label = round(composite_score, 3)),
            hjust = -0.2, size = 3.5,
            color = "black") +
  scale_fill_manual(values = method_colors) +
  coord_flip() +
  labs(title    = "Composite imputation score — MiGrowD",
       subtitle = "Lower = more similar to original data",
       x = NULL, y = "Composite score") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ============================================================
# FINAL IMPUTATION DATA: Using MICE
# ============================================================
migrowd_imputation_mice <-migrowd_prep_mice
head(migrowd_imputation_mice)
names(migrowd_imputation_mice) <- c("Subject_ID", "Recruitment", "Stool_Completion", 
                                    "Child_Weight", "Puberty", "Sex", "Address",
                                    "Special_Diet", "Nutritional_Status", "Father_Ethnicity",
                                    "Mother_Ethnicity", "Mother_Employment", "Education",
                                    "Household_Income", "Child_Age")
View(migrowd_imputation_mice)

migrowd_imputation_mice <- migrowd_imputation_mice %>%
  select(Subject_ID, Recruitment, Address, Stool_Completion, Sex, Child_Age,
         Child_Weight, Nutritional_Status, Puberty, Special_Diet, Father_Ethnicity,
         Mother_Ethnicity, Mother_Employment, Education, Household_Income) 

View(migrowd_imputation_mice)

write.csv(migrowd_imputation_mice, "migrowd_final_imputation.csv", row.names = FALSE)
