# ============================================================
# HDB RESALE PRICE – CLEAN, MODULAR ANALYTICS PIPELINE
# ============================================================

rm(list = ls())
set.seed(123)

# ------------------------
# 1. Load Libraries
# ------------------------
library(tidyverse)
library(corrplot)
library(car)
library(scales)

# ------------------------
# 2. Load Raw Data
# ------------------------
getwd()
setwd("C:/Users/Chief Principal/OneDrive - The University of South Dakota/2025 Fall - Data Mining for Managers/Project/WD/")
getwd()
data <- read_csv("Flats_Resale_Prices_Original.csv")


# ============================================================
# 3. CLEANING FUNCTION
# ============================================================
clean_data <- function(df) {
  
  # Convert remaining lease to numeric years
  lease_years <- str_extract(df$remaining_lease, "\\d+")
  lease_months <- str_extract(df$remaining_lease, "(?<=years )\\d+")
  lease_years <- as.numeric(lease_years)
  lease_months <- ifelse(is.na(lease_months), 0, as.numeric(lease_months) / 12)
  
  df$remaining_lease_years <- lease_years + lease_months
  
  # Extract year and month
  df$year <- as.numeric(substr(df$month, 1, 4))
  df$month_num <- as.numeric(substr(df$month, 6, 7))
  
  # Convert categorical
  df$town <- as.factor(df$town)
  df$flat_type <- as.factor(df$flat_type)
  
  # Drop rows missing essential modeling values
  df <- df[complete.cases(df[c(
    "resale_price", "floor_area_sqm", "remaining_lease_years",
    "year", "month_num", "town", "flat_type"
  )]), ]
  
  return(df)
}

# Apply cleaning
model_data <- clean_data(data)
cat("Final cleaned sample size:", nrow(model_data), "\n")


# ============================================================
# 4. TRAIN / TEST SPLIT (70/30)
# ============================================================
split_data <- function(df, train_ratio = 0.7) {
  n <- nrow(df)
  train_idx <- sample(1:n, size = floor(train_ratio * n))
  list(
    train = df[train_idx, ],
    test = df[-train_idx, ]
  )
}

splits <- split_data(model_data)
train_data <- splits$train
test_data <- splits$test

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows :", nrow(test_data), "\n")


# ============================================================
# 5. EXPLORATORY DATA ANALYSIS
# ============================================================
continuous_vars <- c(
  "resale_price", "floor_area_sqm",
  "remaining_lease_years", "year", "month_num"
)

# ------------ Summary Statistics ------------
summary_stats <- summarise(
  select(model_data, all_of(continuous_vars)),
  across(everything(), list(
    Min = ~min(.),
    Q1 = ~quantile(., 0.25),
    Median = ~median(.),
    Q3 = ~quantile(., 0.75),
    Max = ~max(.),
    Mean = ~mean(.),
    SD = ~sd(.)
  ))
)

write_csv(summary_stats, "summary_statistics.csv")
print(summary_stats)


# ------------ Correlation Matrix ------------
cor_mat <- cor(model_data[continuous_vars], use = "pairwise.complete.obs")

png("correlation_heatmap.png", width = 600, height = 600)
corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8)
dev.off()


# ============================================================
# 6. FACETED BOXPLOTS (Your preferred version)
# ============================================================
data_long <- model_data[, continuous_vars]
data_long <- pivot_longer(data_long, cols = everything(),
                          names_to = "variable", values_to = "value")
data_long <- na.omit(data_long)

p_box <- ggplot(data_long, aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(title = "Box Plots of Continuous Variables", y = "Value") +
  theme_minimal()

ggsave("boxplots_combined.png", p_box, width = 8, height = 6, dpi = 300)


# ============================================================
# 7. SCATTERPLOTS
# ============================================================
dir.create("scatterplots", showWarnings = FALSE)

for (v in continuous_vars[-1]) {
  p <- ggplot(model_data, aes(x = .data[[v]], y = resale_price)) +
    geom_point(alpha = 0.3, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Resale Price vs", v),
         x = v, y = "Resale Price (SGD)") +
    scale_y_continuous(labels = dollar_format(prefix = "S$")) +
    theme_minimal()
  
  ggsave(paste0("scatterplots/", v, "_scatter.png"),
         p, width = 8, height = 5, dpi = 300)
}

dir.create("scatterplots", showWarnings = FALSE)

for (v in continuous_vars[-1]) {
  p <- ggplot(model_data, aes(x = .data[[v]], y = resale_price)) +
    geom_point(alpha = 0.3, color = "steelblue") +
    
    # Linear fit (red line)
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    
    # LOESS curve (dark blue line)
    geom_smooth(method = "loess", se = FALSE, color = "darkblue", linewidth = 1) +
    
    labs(title = paste("Resale Price vs", v),
         x = v, y = "Resale Price (SGD)") +
    scale_y_continuous(labels = dollar_format(prefix = "S$")) +
    theme_minimal()
  
  ggsave(paste0("scatterplots/", v, "_scatter.png"),
         p, width = 8, height = 5, dpi = 300)
}



# ============================================================
# 8. BOXPLOTS (CATEGORICAL)
# ============================================================
dir.create("boxplots", showWarnings = FALSE)

# Flat type boxplot
p_flat <- ggplot(model_data,
                 aes(x = reorder(flat_type, resale_price, median),
                     y = resale_price)) +
  geom_boxplot(fill = "skyblue") +
  coord_flip() +
  labs(title = "Resale Price by Flat Type", x = "", y = "Resale Price (SGD)") +
  scale_y_continuous(labels = dollar_format(prefix = "S$")) +
  theme_minimal()

ggsave("boxplots/flat_type_boxplot.png", p_flat, width = 9, height = 5, dpi = 300)


# Top/Bottom towns
town_med <- model_data %>%
  group_by(town) %>%
  summarise(med = median(resale_price)) %>%
  arrange(med) %>%
  pull(town)

# FIX: Remove duplicates in selected towns
sel_towns <- unique(c(head(town_med, 15), tail(town_med, 15)))

p_town <- model_data %>%
  filter(town %in% sel_towns) %>%
  mutate(town = factor(town, levels = sel_towns)) %>%
  ggplot(aes(x = town, y = resale_price)) +
  geom_boxplot(fill = "lightcoral") +
  coord_flip() +
  labs(title = "Resale Price by Town (Top & Bottom 15)",
       x = "", y = "Resale Price (SGD)") +
  theme_minimal()

ggsave("boxplots/town_boxplot.png", p_town, width = 12, height = 8, dpi = 300)


# ============================================================
# 9. BASELINE MODEL
# ============================================================
build_model <- function(df, formula_str) {
  lm(as.formula(formula_str), data = df)
}

baseline_formula <- "resale_price ~ floor_area_sqm + remaining_lease_years +
                     year + month_num + town + flat_type"

baseline_model <- build_model(train_data, baseline_formula)
summary(baseline_model)

# RMSE function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

rmse_train <- rmse(train_data$resale_price,
                   predict(baseline_model, train_data))
rmse_test <- rmse(test_data$resale_price,
                  predict(baseline_model, test_data))

cat("Baseline Train RMSE:", round(rmse_train), "\n")
cat("Baseline Test  RMSE:", round(rmse_test), "\n")


# ============================================================
# 10. NONLINEAR MODELS
# ============================================================
m_log <- build_model(train_data,
                     "resale_price ~ log(floor_area_sqm) + remaining_lease_years +
                      year + month_num + town + flat_type")

m_quad_lease <- build_model(train_data,
                            "resale_price ~ floor_area_sqm +
                             remaining_lease_years + I(remaining_lease_years^2) +
                             year + month_num + town + flat_type")

m_quad_year <- build_model(train_data,
                           "resale_price ~ floor_area_sqm +
                            remaining_lease_years + year + I(year^2) +
                            month_num + town + flat_type")


# ============================================================
# 11. MODEL COMPARISON
# ============================================================
compare_models <- tibble(
  Model = c("Baseline", "Log(area)", "Quad(lease)", "Quad(year)"),
  Adj_R2 = c(
    summary(baseline_model)$adj.r.squared,
    summary(m_log)$adj.r.squared,
    summary(m_quad_lease)$adj.r.squared,
    summary(m_quad_year)$adj.r.squared
  ),
  RMSE_test = c(
    rmse(test_data$resale_price, predict(baseline_model, test_data)),
    rmse(test_data$resale_price, predict(m_log, test_data)),
    rmse(test_data$resale_price, predict(m_quad_lease, test_data)),
    rmse(test_data$resale_price, predict(m_quad_year, test_data))
  )
)

write_csv(compare_models, "model_comparison.csv")
print(compare_models)


# Select best model (example: quadratic year)
final_model <- m_quad_year
summary(final_model)


# ============================================================
# 12. VIF CHECK
# ============================================================
vif_values <- vif(final_model)
write_csv(as.data.frame(vif_values), "vif_results.csv")
print(vif_values)


# ============================================================
# 13. DIAGNOSTIC PLOTS
# ============================================================
dir.create("diagnostics", showWarnings = FALSE)

png("diagnostics/diagnostics.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2))
plot(final_model, which = 1)
plot(final_model, which = 2)
plot(final_model, which = 3)
plot(final_model, which = 5)
dev.off()


# ============================================================
# 14. SAVE CLEAN DATASET
# ============================================================
write_csv(model_data, "hdb_clean.csv")

