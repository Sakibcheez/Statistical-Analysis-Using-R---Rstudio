options(scipen = 999)


# 1. Import and inspect data
data <- read.csv("household_spending_data.csv", stringsAsFactors = FALSE)
head(data)
str(data)
summary(data)
dim(data)
names(data)

# 2. Data preparation
data$Gender <- factor(data$Gender)

data$Household_Size_Group <- cut(
  data$Household_Size,
  breaks = c(-Inf, 3, 5, Inf),
  labels = c("Small (2-3)", "Medium (4-5)", "Large (6+)"),
  right = TRUE
)

data$High_Spending <- factor(
  ifelse(data$Monthly_Spending_BDT >= 65000, "Yes", "No"),
  levels = c("No", "Yes")
)

# Missing-value check
missing_values <- colSums(is.na(data))
missing_values

# Select important variables
important_variables <- data[, c("Monthly_Spending_BDT", "Monthly_Income_BDT", "Age", "Household_Size", "Gender")]
head(important_variables)

# Meaningful filter
high_spending_households <- data[data$Monthly_Spending_BDT >= 65000, ]
head(high_spending_households)
nrow(high_spending_households)

# 3. Frequency tables and graphs
gender_freq <- table(data$Gender)
gender_pct <- 100 * prop.table(gender_freq)
gender_freq
gender_pct

size_group_freq <- table(data$Household_Size_Group)
size_group_pct <- 100 * prop.table(size_group_freq)
size_group_freq
size_group_pct

if (!dir.exists("figures")) dir.create("figures")

# Figure 1: Bar chart - Gender
png("figures/01_gender_bar.png", width = 1600, height = 1000, res = 180)
barplot(gender_freq, main = "Distribution of Respondents by Gender",
        xlab = "Gender", ylab = "Frequency", col = "steelblue3")
dev.off()

# Figure 2: Pie chart - Gender
png("figures/02_gender_pie.png", width = 1400, height = 1100, res = 180)
pie(gender_freq, labels = paste0(names(gender_freq), ": ", round(gender_pct, 1), "%"),
    main = "Percentage Distribution of Respondents by Gender",
    col = c("steelblue3", "darkorange2"))
dev.off()

# Figure 3: Histogram - Monthly Spending
png("figures/03_spending_histogram.png", width = 1600, height = 1000, res = 180)
hist(data$Monthly_Spending_BDT, breaks = 8,
     main = "Distribution of Monthly Household Spending",
     xlab = "Monthly Spending (BDT)", ylab = "Frequency", col = "grey85")
abline(v = mean(data$Monthly_Spending_BDT), lty = 2, lwd = 2, col = "steelblue3")
abline(v = median(data$Monthly_Spending_BDT), lty = 3, lwd = 2, col = "darkorange2")
legend("topright",
       legend = c(paste0("Mean = ", format(round(mean(data$Monthly_Spending_BDT)), big.mark = ",")),
                  paste0("Median = ", format(round(median(data$Monthly_Spending_BDT)), big.mark = ","))),
       lty = c(2, 3), lwd = 2, col = c("steelblue3", "darkorange2"), bty = "n")
dev.off()

# Figure 4: Box plot - Spending by Household Size Group
png("figures/04_spending_boxplot_household_size.png", width = 1600, height = 1000, res = 180)
boxplot(Monthly_Spending_BDT ~ Household_Size_Group, data = data,
        main = "Monthly Household Spending by Household Size Group",
        xlab = "Household Size Group", ylab = "Monthly Spending (BDT)",
        col = "lightblue2")
dev.off()

# Figure 5: Box plot - Spending by Gender (NEW: completes the gender comparison
# that was named as an explanatory variable but never actually tested)
png("figures/05_spending_boxplot_gender.png", width = 1600, height = 1000, res = 180)
boxplot(Monthly_Spending_BDT ~ Gender, data = data,
        main = "Monthly Household Spending by Gender",
        xlab = "Gender", ylab = "Monthly Spending (BDT)",
        col = "lightgoldenrod2")
dev.off()

# Figure 6: Scatter plot - Income vs Spending
png("figures/06_income_spending_scatter.png", width = 1600, height = 1000, res = 180)
plot(data$Monthly_Income_BDT, data$Monthly_Spending_BDT,
     main = "Monthly Income and Monthly Household Spending",
     xlab = "Monthly Household Income (BDT)",
     ylab = "Monthly Household Spending (BDT)", pch = 19, col = "steelblue3")
abline(lm(Monthly_Spending_BDT ~ Monthly_Income_BDT, data = data), lty = 2, lwd = 2)
dev.off()

# 4. Descriptive statistics
get_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

spending_stats <- c(
  Mean = mean(data$Monthly_Spending_BDT),
  Median = median(data$Monthly_Spending_BDT),
  Min = min(data$Monthly_Spending_BDT),
  Max = max(data$Monthly_Spending_BDT),
  Range = diff(range(data$Monthly_Spending_BDT)),
  Variance = var(data$Monthly_Spending_BDT),
  SD = sd(data$Monthly_Spending_BDT),
  Q1 = unname(quantile(data$Monthly_Spending_BDT, 0.25)),
  Q3 = unname(quantile(data$Monthly_Spending_BDT, 0.75)),
  IQR = IQR(data$Monthly_Spending_BDT),
  P90 = unname(quantile(data$Monthly_Spending_BDT, 0.90))
)
spending_stats

spending_mode <- get_mode(data$Monthly_Spending_BDT)
spending_mode
table(data$Monthly_Spending_BDT)[as.character(spending_mode)]

income_stats <- c(
  Mean = mean(data$Monthly_Income_BDT),
  Median = median(data$Monthly_Income_BDT),
  Min = min(data$Monthly_Income_BDT),
  Max = max(data$Monthly_Income_BDT),
  Range = diff(range(data$Monthly_Income_BDT)),
  Variance = var(data$Monthly_Income_BDT),
  SD = sd(data$Monthly_Income_BDT),
  Q1 = unname(quantile(data$Monthly_Income_BDT, 0.25)),
  Q3 = unname(quantile(data$Monthly_Income_BDT, 0.75)),
  IQR = IQR(data$Monthly_Income_BDT),
  P90 = unname(quantile(data$Monthly_Income_BDT, 0.90))
)
income_stats
get_mode(data$Monthly_Income_BDT)

# Group comparison: Household Size Group (quantitative-derived categorical)
aggregate(Monthly_Spending_BDT ~ Household_Size_Group, data = data, FUN = mean)
aggregate(Monthly_Spending_BDT ~ Household_Size_Group, data = data, FUN = median)
aggregate(Monthly_Spending_BDT ~ Household_Size_Group, data = data, FUN = sd)

# Group comparison: Gender (this closes the gap where Gender was named as an

aggregate(Monthly_Spending_BDT ~ Gender, data = data, FUN = mean)
aggregate(Monthly_Spending_BDT ~ Gender, data = data, FUN = median)
aggregate(Monthly_Spending_BDT ~ Gender, data = data, FUN = sd)
gender_ttest <- t.test(Monthly_Spending_BDT ~ Gender, data = data)
gender_ttest

# Connected correlations
cor(data$Monthly_Income_BDT, data$Monthly_Spending_BDT)
cor(data$Household_Size, data$Monthly_Spending_BDT)
cor(data$Age, data$Monthly_Spending_BDT)

# z-score example
z_70000 <- (70000 - mean(data$Monthly_Spending_BDT)) / sd(data$Monthly_Spending_BDT)
z_70000

# 5. Discrete probability distribution: Binomial
p_hat <- mean(data$High_Spending == "Yes")
p_hat
prob_exactly_4 <- dbinom(4, size = 10, prob = p_hat)
prob_exactly_4
prob_at_least_4 <- 1 - pbinom(3, size = 10, prob = p_hat)
prob_at_least_4

# 6. Normal distribution analysis
mu <- mean(data$Monthly_Spending_BDT)
sigma <- sd(data$Monthly_Spending_BDT)
mu
sigma
p_below_45000 <- pnorm(45000, mean = mu, sd = sigma)
p_below_45000
p_above_85000 <- 1 - pnorm(85000, mean = mu, sd = sigma)
p_above_85000
p_50000_to_75000 <- pnorm(75000, mean = mu, sd = sigma) - pnorm(50000, mean = mu, sd = sigma)
p_50000_to_75000

# 7. Sampling distribution and CLT
set.seed(172)
sample_means <- replicate(500, mean(sample(data$Monthly_Spending_BDT, size = 10, replace = TRUE)))
mean(sample_means)
sd(sample_means)
mean(data$Monthly_Spending_BDT)
sd(data$Monthly_Spending_BDT) / sqrt(10)

# Figure 7: Sampling distribution histogram
png("figures/07_clt_sampling_distribution.png", width = 1600, height = 1000, res = 180)
hist(sample_means, breaks = 14,
     main = "Sampling Distribution of Mean Monthly Spending (n = 10, 500 Resamples)",
     xlab = "Sample Mean Monthly Spending (BDT)", ylab = "Frequency", col = "grey85")
abline(v = mean(data$Monthly_Spending_BDT), lty = 2, lwd = 2, col = "steelblue3")
legend("topright",
       legend = paste0("Original mean = ", format(round(mean(data$Monthly_Spending_BDT)), big.mark = ",")),
       lty = 2, lwd = 2, col = "steelblue3", bty = "n")
dev.off()

# Optional sample-size extension
set.seed(172)
means_n5 <- replicate(500, mean(sample(data$Monthly_Spending_BDT, 5, replace = TRUE)))
set.seed(172)
means_n20 <- replicate(500, mean(sample(data$Monthly_Spending_BDT, 20, replace = TRUE)))
sd(means_n5)
sd(means_n20)

# 8. Confidence intervals
mean_ci <- t.test(data$Monthly_Spending_BDT, conf.level = 0.95)
mean_ci
successes <- sum(data$High_Spending == "Yes")
n_obs <- nrow(data)
prop_ci <- prop.test(successes, n_obs, conf.level = 0.95, correct = FALSE)
prop_ci

# 9. Final reproducibility checks
stopifnot(nrow(data) == 35)
stopifnot(sum(is.na(data)) == 0)
stopifnot(all(data$Monthly_Income_BDT > 0))
stopifnot(all(data$Monthly_Spending_BDT > 0))
stopifnot(length(get_mode(data$Monthly_Spending_BDT)) == 1)
cat("Analysis completed successfully for exactly 35 observations.\n")
