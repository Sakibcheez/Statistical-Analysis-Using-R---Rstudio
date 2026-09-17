rm(list = ls())
options(stringsAsFactors = FALSE)

# 0. Setup and import 

survey_file <- "survey_data.csv"
time_series_file <- "time_series_data.csv"
codebook_file <- "variable_codebook.csv"

if (!dir.exists("graphs")) dir.create("graphs", recursive = TRUE)
if (!dir.exists("tables")) dir.create("tables", recursive = TRUE)

# Consistent visual palette used across all report figures.
col_data <- "#2F6F9F"
col_fill <- "#8FBAD9"
col_fit <- "#D97706"
col_smooth <- "#4C956C"
col_alt <- "#7A5195"

project <- read.csv(survey_file)
project$gender <- factor(project$gender)
series <- read.csv(time_series_file)
codebook <- read.csv(codebook_file, fileEncoding = "UTF-8")

# Use the submitted period column as the time index.
stopifnot(all(c("period", "study_hours") %in% names(series)))
series$time <- series$period

# Verify that the submitted codebook covers all raw survey variables.
raw_vars <- c("participant_id", "gender", "gpa", "study_hours",
              "social_media", "sleep_hours")
stopifnot(all(raw_vars %in% names(project)))
stopifnot(all(raw_vars %in% codebook$r_variable))

# 1. Data audit and cleaning 

head(project)
dim(project)
names(project)
str(project)
summary(project)

missing_counts <- colSums(is.na(project))
duplicate_count <- sum(duplicated(project))
missing_counts
duplicate_count

stopifnot(nrow(project) >= 30)
stopifnot(!anyNA(project[raw_vars]))
stopifnot(duplicate_count == 0)
stopifnot(all(project$gpa >= 0 & project$gpa <= 4))
stopifnot(all(project$study_hours >= 0))
stopifnot(all(project$social_media >= 0 & project$social_media <= 24))
stopifnot(all(project$sleep_hours > 0 & project$sleep_hours <= 24))

# Time-series audit
stopifnot(nrow(series) >= 20)
stopifnot(!anyNA(series$period))
stopifnot(!anyNA(series$study_hours))
stopifnot(all(diff(series$period) == 1))

# Derived category for the required categorical association.
# Sleep is categorized at 7 hours because this threshold corresponds to the
# same research-based adult sleep benchmark used for the one-sample inference,
# rather than being selected based on the observed data.
project$sleep_category <- factor(
  ifelse(project$sleep_hours >= 7, "At least 7 h", "Below 7 h"),
  levels = c("Below 7 h", "At least 7 h")
)

# 2. Descriptive statistics 

num_vars <- c("gpa", "study_hours", "social_media", "sleep_hours")

describe_one <- function(x) {
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q1 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
    q3 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
    max = max(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE)
  )
}

descriptive <- as.data.frame(t(sapply(project[num_vars], describe_one)))
descriptive$variable <- rownames(descriptive)
rownames(descriptive) <- NULL
descriptive <- descriptive[, c("variable", "n", "mean", "median", "variance",
                               "sd", "min", "q1", "q3", "max", "IQR")]
descriptive
write.csv(descriptive, "tables/01_descriptive_statistics.csv", row.names = FALSE)

gender_summary <- data.frame(
  gender = levels(project$gender),
  mean_gpa = as.numeric(aggregate(gpa ~ gender, project, mean)$gpa),
  sd_gpa = as.numeric(aggregate(gpa ~ gender, project, sd)$gpa)
)
gender_summary
write.csv(gender_summary, "tables/02_gpa_by_gender_summary.csv", row.names = FALSE)

# 3. Cross-sectional graphs 

# Histogram helper used for the four main quantitative variables.
plot_hist_density <- function(x, main, xlab, filename) {
  png(filename, 900, 650)
  hist(x,
       probability = TRUE,
       main = main,
       xlab = xlab,
       col = col_fill,
       border = "white")
  lines(density(x, na.rm = TRUE), lwd = 2, col = col_fit)
  dev.off()
}

plot_hist_density(project$gpa, "Distribution of GPA", "GPA (0-4)",
                  "graphs/01_gpa_hist_density.png")
plot_hist_density(project$study_hours, "Distribution of Weekly Study Hours",
                  "Study hours per week",
                  "graphs/02_study_hours_hist_density.png")
plot_hist_density(project$social_media, "Distribution of Daily Social-Media Use",
                  "Social-media hours per day",
                  "graphs/03_social_media_hist_density.png")
plot_hist_density(project$sleep_hours, "Distribution of Nightly Sleep Hours",
                  "Sleep hours per night",
                  "graphs/04_sleep_hours_hist_density.png")

png("graphs/05_gpa_by_gender_boxplot.png", 900, 650)
boxplot(gpa ~ gender,
        data = project,
        main = "GPA by Gender",
        xlab = "Gender",
        ylab = "GPA (0-4)",
        col = col_fill,
        border = col_data)
dev.off()

scatter_specs <- list(
  list(v = "study_hours", xlab = "Weekly Study Hours",
       title = "GPA vs Weekly Study Hours", file = "graphs/06_gpa_vs_study_hours.png"),
  list(v = "social_media", xlab = "Social-Media Use (hours/day)",
       title = "GPA vs Social-Media Use", file = "graphs/07_gpa_vs_social_media.png"),
  list(v = "sleep_hours", xlab = "Sleep Hours per Night",
       title = "GPA vs Nightly Sleep Hours", file = "graphs/08_gpa_vs_sleep_hours.png")
)

for (s in scatter_specs) {
  png(s$file, 900, 650)
  plot(project[[s$v]], project$gpa,
       pch = 19,
       col = col_data,
       xlab = s$xlab,
       ylab = "GPA (0-4)",
       main = s$title)
  abline(lm(project$gpa ~ project[[s$v]]), lwd = 2, col = col_fit)
  dev.off()
}

# Scatterplot matrix with histograms on the diagonal
# structure while remaining base-R reproducible.
panel_hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  y <- h$counts / max(h$counts)
  rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], y,
       col = col_fill, border = "white")
}

png("graphs/09_scatterplot_matrix.png", 1000, 900)
pairs(project[num_vars],
      pch = 19,
      col = col_data,
      cex = 0.65,
      diag.panel = panel_hist,
      main = "Scatterplot Matrix of Quantitative Variables")
dev.off()

# 4. Inferential procedures 

alpha <- 0.05

# 4.1 One-sample t-test: mean sleep vs 7 hours
sleep_test <- t.test(
  project$sleep_hours,
  mu = 7,
  alternative = "two.sided",
  conf.level = 0.95
)
sleep_test

sleep_test_table <- data.frame(
  parameter = "Mean nightly sleep",
  null_value = 7,
  sample_mean = unname(sleep_test$estimate),
  t_statistic = unname(sleep_test$statistic),
  df = unname(sleep_test$parameter),
  p_value = sleep_test$p.value,
  ci_low = sleep_test$conf.int[1],
  ci_high = sleep_test$conf.int[2],
  alpha = alpha
)
write.csv(sleep_test_table, "tables/03_one_sample_sleep_test.csv", row.names = FALSE)

# 4.2 Welch two-sample t-test: GPA by gender
gender_test <- t.test(
  gpa ~ gender,
  data = project,
  var.equal = FALSE,
  conf.level = 0.95
)
gender_test

gender_test_table <- data.frame(
  comparison = "Female - Male mean GPA",
  mean_female = unname(gender_test$estimate[1]),
  mean_male = unname(gender_test$estimate[2]),
  mean_difference = unname(gender_test$estimate[1] - gender_test$estimate[2]),
  t_statistic = unname(gender_test$statistic),
  df = unname(gender_test$parameter),
  p_value = gender_test$p.value,
  ci_low = gender_test$conf.int[1],
  ci_high = gender_test$conf.int[2],
  alpha = alpha
)
write.csv(gender_test_table, "tables/04_welch_gender_test.csv", row.names = FALSE)

# 4.3 Categorical association: gender x 7-hour sleep benchmark status
association_table <- table(project$gender, project$sleep_category)
association_test <- chisq.test(association_table, correct = FALSE)

association_table
association_test
association_test$expected
round(association_test$stdres, 2)

write.csv(as.data.frame.matrix(association_table),
          "tables/05_sleep_category_contingency.csv")
write.csv(as.data.frame.matrix(association_test$expected),
          "tables/06_sleep_category_expected_counts.csv")
write.csv(as.data.frame.matrix(round(association_test$stdres, 4)),
          "tables/07_sleep_category_standardized_residuals.csv")

association_summary <- data.frame(
  chi_square = unname(association_test$statistic),
  df = unname(association_test$parameter),
  p_value = association_test$p.value,
  minimum_expected_count = min(association_test$expected),
  alpha = alpha
)
write.csv(association_summary, "tables/08_categorical_association_test.csv", row.names = FALSE)

# 5. Correlation and simple regression 

quantitative <- project[num_vars]
correlation_matrix <- cor(quantitative, use = "complete.obs")
correlation_matrix
write.csv(correlation_matrix, "tables/09_correlation_matrix.csv")

cor_tests <- list(
  study_hours = cor.test(project$gpa, project$study_hours),
  social_media = cor.test(project$gpa, project$social_media),
  sleep_hours = cor.test(project$gpa, project$sleep_hours)
)

correlation_tests <- do.call(rbind, lapply(names(cor_tests), function(v) {
  x <- cor_tests[[v]]
  data.frame(
    predictor = v,
    r = unname(x$estimate),
    t_statistic = unname(x$statistic),
    df = unname(x$parameter),
    p_value = x$p.value,
    ci_low = x$conf.int[1],
    ci_high = x$conf.int[2]
  )
}))
correlation_tests
write.csv(correlation_tests, "tables/10_correlation_tests.csv", row.names = FALSE)

study_model <- lm(gpa ~ study_hours, data = project)
social_model <- lm(gpa ~ social_media, data = project)
sleep_model <- lm(gpa ~ sleep_hours, data = project)

simple_models <- list(
  study_hours = study_model,
  social_media = social_model,
  sleep_hours = sleep_model
)

simple_regression <- do.call(rbind, lapply(names(simple_models), function(v) {
  m <- simple_models[[v]]
  sm <- summary(m)
  ci <- confint(m)[2, ]
  data.frame(
    predictor = v,
    slope = coef(m)[2],
    slope_p = sm$coefficients[2, 4],
    slope_ci_low = ci[1],
    slope_ci_high = ci[2],
    r_squared = sm$r.squared
  )
}))
simple_regression
write.csv(simple_regression, "tables/11_simple_regression_models.csv", row.names = FALSE)

summary(study_model); confint(study_model)
summary(social_model); confint(social_model)
summary(sleep_model); confint(sleep_model)

# 6. Multiple regression and model comparison 

reduced_model <- lm(gpa ~ study_hours + social_media, data = project)
full_model <- lm(gpa ~ study_hours + social_media + sleep_hours, data = project)

summary(full_model)
coef(full_model)
confint(full_model, level = 0.95)

full_coef <- as.data.frame(summary(full_model)$coefficients)
full_coef$term <- rownames(full_coef)
rownames(full_coef) <- NULL
names(full_coef)[1:4] <- c("estimate", "std_error", "t_value", "p_value")
full_ci <- confint(full_model, level = 0.95)
full_coef$ci_low <- full_ci[, 1]
full_coef$ci_high <- full_ci[, 2]
full_coef <- full_coef[, c("term", "estimate", "std_error", "t_value",
                           "p_value", "ci_low", "ci_high")]
full_coef
write.csv(full_coef, "tables/12_multiple_regression_coefficients.csv", row.names = FALSE)

full_summary <- summary(full_model)
model_fit <- data.frame(
  r_squared = full_summary$r.squared,
  adjusted_r_squared = full_summary$adj.r.squared,
  residual_standard_error = full_summary$sigma,
  f_statistic = unname(full_summary$fstatistic[1]),
  df_model = unname(full_summary$fstatistic[2]),
  df_residual = unname(full_summary$fstatistic[3]),
  overall_p_value = pf(full_summary$fstatistic[1],
                       full_summary$fstatistic[2],
                       full_summary$fstatistic[3],
                       lower.tail = FALSE)
)
model_fit
write.csv(model_fit, "tables/13_multiple_regression_fit.csv", row.names = FALSE)

reduced_adj_r2 <- summary(reduced_model)$adj.r.squared
full_adj_r2 <- summary(full_model)$adj.r.squared
nested_test <- anova(reduced_model, full_model)
reduced_adj_r2
full_adj_r2
nested_test

model_comparison <- data.frame(
  reduced_adjusted_r2 = reduced_adj_r2,
  full_adjusted_r2 = full_adj_r2,
  partial_f = nested_test$F[2],
  partial_f_p_value = nested_test$`Pr(>F)`[2]
)
write.csv(model_comparison, "tables/14_model_comparison.csv", row.names = FALSE)

# VIF using auxiliary regressions (base R)
vif_manual <- sapply(
  c("study_hours", "social_media", "sleep_hours"),
  function(v) {
    others <- setdiff(c("study_hours", "social_media", "sleep_hours"), v)
    aux <- lm(reformulate(others, response = v), data = project)
    1 / (1 - summary(aux)$r.squared)
  }
)
vif_table <- data.frame(predictor = names(vif_manual), vif = as.numeric(vif_manual))
vif_table
write.csv(vif_table, "tables/15_vif.csv", row.names = FALSE)
# 7. Diagnostics 

png("graphs/10_regression_diagnostics.png", 1000, 900)
par(mfrow = c(2, 2))
plot(full_model, pch = 19, col = col_data)
par(mfrow = c(1, 1))
dev.off()

std_resid <- rstandard(full_model)
cook <- cooks.distance(full_model)
std_resid
cook

diagnostic_summary <- data.frame(
  metric = c("largest_absolute_standardized_residual", "maximum_cooks_distance"),
  value = c(max(abs(std_resid)), max(cook)),
  participant_id = c(
    project$participant_id[which.max(abs(std_resid))],
    project$participant_id[which.max(cook)]
  )
)
diagnostic_summary
write.csv(diagnostic_summary, "tables/16_diagnostic_summary.csv", row.names = FALSE)

# 8. Confidence and prediction intervals 

scenario <- data.frame(
  study_hours = c(
    round(mean(project$study_hours), 1),
    round(mean(project$study_hours) + sd(project$study_hours), 1)
  ),
  social_media = c(
    round(mean(project$social_media), 1),
    round(mean(project$social_media) - sd(project$social_media), 1)
  ),
  sleep_hours = c(
    round(mean(project$sleep_hours), 1),
    round(mean(project$sleep_hours) + 0.5, 1)
  )
)
scenario_names <- c("Typical profile", "Higher-study / lower-social profile")

ci_predictions <- predict(full_model, newdata = scenario,
                          interval = "confidence", level = 0.95)
pi_predictions <- predict(full_model, newdata = scenario,
                          interval = "prediction", level = 0.95)

scenario_results <- data.frame(
  scenario = scenario_names,
  scenario,
  mean = ci_predictions[, "fit"],
  mean_ci_lower = ci_predictions[, "lwr"],
  mean_ci_upper = ci_predictions[, "upr"],
  obs_ci_lower = pi_predictions[, "lwr"],
  obs_ci_upper = pi_predictions[, "upr"]
)
scenario_results
write.csv(scenario_results, "tables/17_prediction_scenarios.csv", row.names = FALSE)

# 9. Time-series supplement 

y_ts <- ts(series$study_hours, frequency = 1)

# 9.1 Three-period trailing moving average
moving_average <- stats::filter(
  y_ts,
  filter = rep(1 / 3, 3),
  sides = 1
)

png("graphs/11_time_series_moving_average.png", 1000, 650)
plot(
  series$period,
  series$study_hours,
  type = "o",
  pch = 19,
  col = col_data,
  xlab = "Day",
  ylab = "Study hours/day",
  main = "Daily Study Hours with 3-Day Moving Average"
)
lines(series$period, as.numeric(moving_average), lwd = 2, col = col_smooth)
legend(
  "topleft",
  legend = c("Observed", "3-day moving average"),
  col = c(col_data, col_smooth),
  lty = c(1, 1),
  pch = c(19, NA),
  bty = "n"
)
dev.off()

# 9.2 Final-four-period holdout comparison
n <- nrow(series)
train <- series[1:(n - 4), ]
test <- series[(n - 3):n, ]

trend_model <- lm(study_hours ~ time, data = train)
trend_forecast <- predict(trend_model, newdata = test)

train_ts <- ts(train$study_hours, frequency = 1)
ses_model <- HoltWinters(train_ts, beta = FALSE, gamma = FALSE)
ses_forecast <- as.numeric(predict(ses_model, n.ahead = 4))

rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2))
}

trend_rmse <- rmse(test$study_hours, trend_forecast)
ses_rmse <- rmse(test$study_hours, ses_forecast)
trend_rmse
ses_rmse

holdout_results <- data.frame(
  period = test$period,
  study_hours = test$study_hours,
  trend_forecast = as.numeric(trend_forecast),
  ses_forecast = ses_forecast
)
holdout_results
write.csv(holdout_results, "tables/18_holdout_forecasts.csv", row.names = FALSE)

rmse_results <- data.frame(
  model = c("Linear trend", "Simple exponential smoothing"),
  holdout_rmse = c(trend_rmse, ses_rmse)
)
rmse_results
write.csv(rmse_results, "tables/19_holdout_rmse.csv", row.names = FALSE)

# Full-series plot plus holdout actual and both holdout forecasts, matching
# the report's visual structure.
png("graphs/12_time_series_holdout_comparison.png", 1000, 650)
plot(
  train$period,
  train$study_hours,
  type = "o",
  pch = 19,
  col = col_data,
  xlim = range(series$period),
  xlab = "Day",
  ylab = "Study hours/day",
  main = "Holdout Forecast Comparison",
  ylim = range(c(series$study_hours, trend_forecast, ses_forecast))
)
lines(test$period, test$study_hours, type = "o", pch = 16, lwd = 2, col = col_data)
lines(test$period, trend_forecast, type = "o", pch = 15, lty = 2,
      lwd = 2, col = col_fit)
lines(test$period, ses_forecast, type = "o", pch = 17, lty = 3,
      lwd = 2, col = col_alt)
legend(
  "topleft",
  legend = c("Training observations", "Holdout actual",
             "Trend forecast", "SES forecast"),
  col = c(col_data, col_data, col_fit, col_alt),
  lty = c(1, 1, 2, 3),
  pch = c(19, 16, 15, 17),
  bty = "n"
)
dev.off()

# 9.3 Refit the winning model and forecast the next three periods
if (trend_rmse <= ses_rmse) {
  selected_model_name <- "Linear trend"
  final_model <- lm(study_hours ~ time, data = series)
  future <- data.frame(time = (n + 1):(n + 3))
  final_forecast <- as.numeric(predict(final_model, newdata = future))
} else {
  selected_model_name <- "Simple exponential smoothing"
  final_model <- HoltWinters(y_ts, beta = FALSE, gamma = FALSE)
  final_forecast <- as.numeric(predict(final_model, n.ahead = 3))
}

future_period <- (n + 1):(n + 3)
forecast_results <- data.frame(
  period = future_period,
  forecast_study_hours = final_forecast,
  selected_model = selected_model_name
)
forecast_results
write.csv(forecast_results, "tables/20_three_period_forecast.csv", row.names = FALSE)

png("graphs/13_time_series_three_period_forecast.png", 1000, 650)
all_periods <- c(series$period, future_period)
all_values <- c(series$study_hours, final_forecast)
plot(
  series$period,
  series$study_hours,
  type = "o",
  pch = 19,
  col = col_data,
  xlim = range(all_periods),
  ylim = range(all_values),
  xlab = "Day",
  ylab = "Study hours/day",
  main = paste("Three-Period Forecast using", selected_model_name)
)
lines(
  c(tail(series$period, 1), future_period),
  c(tail(series$study_hours, 1), final_forecast),
  type = "o",
  pch = 15,
  lty = 2,
  lwd = 2,
  col = col_fit
)
abline(v = n + 0.5, lty = 3, col = "grey45")
legend(
  "topleft",
  legend = c("Observed", "3-day forecast"),
  col = c(col_data, col_fit),
  lty = c(1, 2),
  pch = c(19, 15),
  bty = "n"
)
dev.off()

# 10. Final reproducibility message 

cat("\nAnalysis completed successfully.\n")
cat("Survey file:", survey_file, "\n")
cat("Time-series file:", time_series_file, "\n")
cat("Codebook file:", codebook_file, "\n")
cat("Tables were written to the 'tables' folder.\n")
cat("Graphs were written to the 'graphs' folder.\n")
cat("Selected time-series model:", selected_model_name, "\n")
cat("Three-period forecasts:", round(final_forecast, 3), "\n")
