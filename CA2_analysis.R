# =============================================================================
# CA2 - Data Analysis 2026
# What drives Irish residential property asking prices?
# Dataset: Ireland House Properties Dataset 2024 (Khalid, 2024) - Daft.ie listings
# Author: Stuart Farry (L00204884)
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
pkgs <- c("dplyr", "ggplot2", "tidyr", "car")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)

set.seed(2026)
dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))
save_fig <- function(name, plot, w = 7, h = 4.5) {
  ggsave(file.path("figures", name), plot, width = w, height = h, dpi = 200, bg = "white")
}

# =============================================================================
# 1. DATA PREPARATION
# =============================================================================

# ---- 1.1 Load and inspect ---------------------------------------------------
raw <- read.csv("daft_housing_data.csv", check.names = FALSE,
                stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A"))
dim(raw)                 # 14,289 rows x 14 columns
str(raw)
colSums(is.na(raw))      # missing values per column

# ---- 1.2 Rename variables to snake_case -------------------------------------
df <- raw %>%
  rename(title        = Title,
         price_raw    = Price,
         bedrooms_raw = `Number of Bedrooms`,
         bathrooms    = `Number of Bathrooms`,
         property_type = `Property Type`,
         floor_area   = `Floor Area (m2)`,
         ber_raw      = `BER Rating`,
         latitude     = Latitude,
         longitude    = Longitude,
         views        = `Listing Views`,
         area         = Area,
         county       = County,
         features     = Features,
         year_built   = `Date of Construction`)

# Record row counts at each cleaning step for the report
steps <- data.frame(step = "Raw dataset", rows = nrow(df))
log_step <- function(d, label) {
  steps <<- rbind(steps, data.frame(step = label, rows = nrow(d)))
  d
}

# ---- 1.3 Remove duplicates --------------------------------------------------
df <- df %>% distinct() %>% log_step("Exact duplicate rows removed")
df <- df %>% distinct(title, price_raw, .keep_all = TRUE) %>%
  log_step("Duplicate listings (same address and price) removed")

# ---- 1.4 Clean price (stored as text) ---------------------------------------
# "Price on Application" listings have no price and are removed.
# Prefixes "AMV: " (advised market value) and "From " are stripped.
# Sterling-priced listings (Northern Ireland border) are removed.
df <- df %>%
  mutate(price_txt = gsub("^(AMV: |From )", "", price_raw),
         price = suppressWarnings(as.numeric(price_txt))) %>%
  filter(!is.na(price)) %>%
  log_step("Listings without a numeric euro price removed (POA, N/A, GBP)")

# ---- 1.5 Clean bedrooms (some values have a trailing comma) -----------------
df <- df %>% mutate(bedrooms = as.numeric(gsub(",", "", bedrooms_raw)),
                    floor_area = suppressWarnings(as.numeric(floor_area)),
                    year_built = suppressWarnings(as.numeric(year_built)))

# ---- 1.6 Property type: drop sites, group categories ------------------------
table(df$property_type)
df <- df %>% filter(property_type != "Site") %>%
  log_step("Sites (land with no dwelling) removed")

df <- df %>%
  mutate(property_type = case_when(
    property_type == "Detached"                                 ~ "Detached",
    property_type == "Semi-D"                                   ~ "Semi-detached",
    property_type %in% c("Terrace", "End of Terrace", "Townhouse") ~ "Terraced",
    property_type %in% c("Apartment", "Duplex", "Studio")       ~ "Apartment",
    property_type == "Bungalow"                                 ~ "Bungalow",
    TRUE ~ NA_character_)) %>%          # "Houses", "House", "New Homes", "Property" are unspecified
  filter(!is.na(property_type)) %>%
  log_step("Unspecified property types removed (Houses, New Homes, Property)") %>%
  mutate(property_type = factor(property_type,
         levels = c("Apartment", "Terraced", "Semi-detached", "Bungalow", "Detached")))

# ---- 1.7 Plausibility filters (outliers / data-entry errors) ----------------
# Floor area: values < 30 m2 are mostly acreage or typos (e.g. 0.5);
# values > 1,000 m2 are not plausible for a residential dwelling (max was 1.8m).
# Bedrooms/bathrooms: 1-10 kept. Price: EUR 30,000 - 3,000,000 (approx. 1st-99.5th pct).
df <- df %>% filter(!is.na(floor_area), floor_area >= 30, floor_area <= 1000) %>%
  log_step("Missing or implausible floor area removed (<30 or >1,000 m2)")
df <- df %>% filter(bedrooms >= 1, bedrooms <= 10, bathrooms >= 1, bathrooms <= 10) %>%
  log_step("Implausible bedroom/bathroom counts removed (outside 1-10)")
df <- df %>% filter(price >= 30000, price <= 3000000) %>%
  log_step("Extreme prices removed (outside EUR 30k-3m)")
df <- df %>% filter(!is.na(county)) %>%
  log_step("Listings with no county removed")

# ---- 1.8 BER rating: clean and band -----------------------------------------
# SI_666 = exempt from BER (e.g. protected structures); BER_PENDING = not yet rated.
# Combined labels (e.g. "A2A3") take the first rating.
df <- df %>%
  mutate(ber = ifelse(ber_raw %in% c("SI_666", "BER_PENDING"), NA, ber_raw),
         ber_letter = substr(ber, 1, 1),
         ber_band = case_when(ber_letter == "A" ~ "A",
                              ber_letter == "B" ~ "B",
                              ber_letter == "C" ~ "C",
                              ber_letter == "D" ~ "D",
                              ber_letter %in% c("E", "F", "G") ~ "E-G"),
         ber_band = factor(ber_band, levels = c("A", "B", "C", "D", "E-G")))

# ---- 1.9 Feature engineering ------------------------------------------------
df <- df %>%
  mutate(log_price = log(price),
         price_per_m2 = price / floor_area,
         region = factor(ifelse(county == "Dublin", "Dublin", "Rest of Ireland"),
                         levels = c("Dublin", "Rest of Ireland")),
         build_era = cut(year_built, breaks = c(-Inf, 1979, 1999, 2009, Inf),
                         labels = c("Pre-1980", "1980-1999", "2000-2009", "2010 onwards")),
         county = factor(county))

# Keep only the variables used in the analysis
clean <- df %>% select(title, price, log_price, price_per_m2, floor_area, bedrooms,
                       bathrooms, property_type, ber_band, county, region,
                       year_built, build_era, views)

steps$removed <- c(NA, -diff(steps$rows))
print(steps)
write.csv(steps, "output/cleaning_steps.csv", row.names = FALSE)
write.csv(clean, "output/daft_clean.csv", row.names = FALSE)

# ---- 1.10 Descriptive statistics --------------------------------------------
num_summary <- clean %>%
  select(price, floor_area, price_per_m2, bedrooms, bathrooms, year_built) %>%
  pivot_longer(everything()) %>%
  group_by(variable = name) %>%
  summarise(n = sum(!is.na(value)), missing = sum(is.na(value)),
            mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE))
print(num_summary)
write.csv(num_summary, "output/numeric_summary.csv", row.names = FALSE)

cat_counts <- list(property_type = table(clean$property_type),
                   ber_band = table(clean$ber_band, useNA = "ifany"),
                   region = table(clean$region),
                   build_era = table(clean$build_era, useNA = "ifany"))
print(cat_counts)
capture.output(cat_counts, file = "output/categorical_counts.txt")

# Missing values remaining in the clean data
missing_clean <- colSums(is.na(clean))
print(missing_clean)

# Correlation matrix of numeric variables (Spearman, as variables are skewed)
cor_mat <- cor(clean %>% select(price, floor_area, bedrooms, bathrooms, year_built, views),
               method = "spearman", use = "pairwise.complete.obs")
print(round(cor_mat, 2))
write.csv(round(cor_mat, 3), "output/spearman_correlation_matrix.csv")

# ---- 1.11 Exploratory charts ------------------------------------------------
f1 <- ggplot(clean, aes(price / 1000)) +
  geom_histogram(bins = 60, fill = "#2a6f97", colour = "white") +
  labs(title = "Distribution of asking price", x = "Asking price (EUR thousands)", y = "Listings")
save_fig("fig01_price_hist.png", f1)

f2 <- ggplot(clean, aes(log_price)) +
  geom_histogram(bins = 60, fill = "#2a6f97", colour = "white") +
  labs(title = "Distribution of log(asking price)", x = "log(price)", y = "Listings")
save_fig("fig02_logprice_hist.png", f2)

cor_long <- as.data.frame(as.table(cor_mat))
f3 <- ggplot(cor_long, aes(Var1, Var2, fill = Freq)) + geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 3.5) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", limits = c(-1, 1)) +
  labs(title = "Spearman correlation matrix", x = NULL, y = NULL, fill = "rho") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("fig03_correlation.png", f3, 6.5, 5)

county_med <- clean %>% group_by(county) %>%
  summarise(median_price = median(price), n = n()) %>% arrange(median_price)
f4 <- ggplot(county_med, aes(reorder(county, median_price), median_price / 1000)) +
  geom_col(fill = "#2a6f97") + coord_flip() +
  labs(title = "Median asking price by county", x = NULL, y = "Median price (EUR thousands)")
save_fig("fig04_county_median.png", f4, 7, 7)

f5 <- ggplot(clean, aes(property_type, price / 1000)) +
  geom_boxplot(fill = "#a9d6e5", outlier.alpha = 0.2) + scale_y_log10() +
  labs(title = "Asking price by property type (log scale)", x = NULL, y = "Price (EUR thousands)")
save_fig("fig05_type_box.png", f5)

# ---- 1.12 Normality checks --------------------------------------------------
# shapiro.test() accepts at most 5,000 values, so a random sample is used.
samp <- sample(clean$price, 5000)
sw_price <- shapiro.test(samp)
sw_logprice <- shapiro.test(log(samp))
sw_area <- shapiro.test(sample(clean$floor_area, 5000))
print(sw_price); print(sw_logprice); print(sw_area)

png("figures/fig06_qq.png", width = 1800, height = 900, res = 200)
par(mfrow = c(1, 2))
qqnorm(clean$price, main = "Q-Q plot: price", pch = 20, cex = 0.4); qqline(clean$price, col = "red")
qqnorm(clean$log_price, main = "Q-Q plot: log(price)", pch = 20, cex = 0.4); qqline(clean$log_price, col = "red")
dev.off()

# Helper: epsilon-squared effect size for Kruskal-Wallis
eps_sq <- function(H, n) as.numeric(H) / ((n^2 - 1) / (n + 1))

# =============================================================================
# 2. RQ1 - Does asking price differ across BER bands?
#    H0: the price distributions are the same in every BER band
#    H1: at least one BER band differs
#    Test: Kruskal-Wallis (non-normal price, 5 independent groups)
#          + pairwise Wilcoxon with Bonferroni correction
# =============================================================================
rq1 <- clean %>% filter(!is.na(ber_band))
rq1_desc <- rq1 %>% group_by(ber_band) %>%
  summarise(n = n(), median_price = median(price), mean_price = mean(price),
            median_area = median(floor_area), median_price_m2 = median(price_per_m2))
print(rq1_desc)
kw1 <- kruskal.test(price ~ ber_band, data = rq1)
print(kw1)
eps1 <- eps_sq(kw1$statistic, nrow(rq1))
pw1 <- pairwise.wilcox.test(rq1$price, rq1$ber_band, p.adjust.method = "bonferroni")
print(pw1)

f7 <- ggplot(rq1, aes(ber_band, price / 1000, fill = ber_band)) +
  geom_boxplot(outlier.alpha = 0.15, show.legend = FALSE) + scale_y_log10() +
  scale_fill_manual(values = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59")) +
  labs(title = "RQ1: Asking price by BER band (log scale)", x = "BER band", y = "Price (EUR thousands)")
save_fig("fig07_rq1_ber_box.png", f7)

# Also: price per m2 by BER band (controls for size)
kw1b <- kruskal.test(price_per_m2 ~ ber_band, data = rq1)
print(kw1b)

# =============================================================================
# 3. RQ2 - Is floor area correlated with asking price?
#    H0: rho = 0 (no monotonic association)   H1: rho != 0
#    Test: Spearman rank correlation (both variables continuous, non-normal)
# =============================================================================
sp2 <- cor.test(clean$floor_area, clean$price, method = "spearman", exact = FALSE)
print(sp2)
pe2 <- cor.test(log(clean$floor_area), clean$log_price, method = "pearson")
print(pe2)   # for comparison on the log-log scale

f8 <- ggplot(clean, aes(floor_area, price / 1000)) +
  geom_point(alpha = 0.15, size = 0.8, colour = "#2a6f97") +
  geom_smooth(method = "loess", colour = "#c1121f", se = FALSE) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "RQ2: Floor area vs asking price (log-log)", x = "Floor area (m2)", y = "Price (EUR thousands)")
save_fig("fig08_rq2_area_scatter.png", f8)

# =============================================================================
# 4. RQ3 - Are asking prices higher in Dublin than in the rest of Ireland?
#    H0: the price distributions are the same in Dublin and elsewhere
#    H1: Dublin prices are higher (one-sided)
#    Test: Wilcoxon rank-sum / Mann-Whitney U (two independent groups, non-normal)
# =============================================================================
rq3_desc <- clean %>% group_by(region) %>%
  summarise(n = n(), median_price = median(price), mean_price = mean(price),
            median_area = median(floor_area), median_price_m2 = median(price_per_m2))
print(rq3_desc)
w3 <- wilcox.test(price ~ region, data = clean, alternative = "greater",
                  conf.int = TRUE, exact = FALSE)
print(w3)
z3 <- qnorm(w3$p.value / 2)
r3 <- abs(z3) / sqrt(nrow(clean))      # effect size r
w3b <- wilcox.test(price_per_m2 ~ region, data = clean, alternative = "greater", exact = FALSE)
print(w3b)

f9 <- ggplot(clean, aes(region, price / 1000, fill = region)) +
  geom_violin(alpha = 0.5, show.legend = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA, show.legend = FALSE) + scale_y_log10() +
  scale_fill_manual(values = c("#2a6f97", "#a9d6e5")) +
  labs(title = "RQ3: Asking price, Dublin vs rest of Ireland", x = NULL, y = "Price (EUR thousands, log scale)")
save_fig("fig09_rq3_region.png", f9)

# =============================================================================
# 5. RQ4 - Is BER band associated with the era in which a home was built?
#    H0: BER band and build era are independent
#    H1: BER band and build era are associated
#    Test: Pearson chi-square test of independence (two categorical variables)
# =============================================================================
rq4 <- clean %>% filter(!is.na(ber_band), !is.na(build_era))
tab4 <- table(rq4$build_era, rq4$ber_band)
print(tab4)
chi4 <- chisq.test(tab4)
print(chi4)
print(round(chi4$expected, 1))                 # assumption: all expected counts >= 5
cramer_v <- sqrt(as.numeric(chi4$statistic) / (sum(tab4) * (min(dim(tab4)) - 1)))
print(round(prop.table(tab4, 1) * 100, 1))     # row percentages
print(round(chi4$stdres, 1))                   # standardised residuals

f10 <- as.data.frame(prop.table(tab4, 1)) %>%
  ggplot(aes(Var1, Freq, fill = Var2)) + geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59")) +
  labs(title = "RQ4: BER band by build era", x = "Build era", y = "Share of listings", fill = "BER band")
save_fig("fig10_rq4_ber_era.png", f10)

# =============================================================================
# 6. RQ5 - Is there a BER price premium after controlling for size, type and location?
#    H0: all BER band coefficients = 0 (no premium once other features are held constant)
#    H1: at least one BER band coefficient != 0
#    Test: multiple linear regression on log(price) + partial F-test (nested models)
# =============================================================================
rq5 <- clean %>% filter(!is.na(ber_band)) %>%
  mutate(ber_band = relevel(ber_band, ref = "D"),     # D-rated is the reference, as in Hyland et al. (2013)
         log_area = log(floor_area))

# 80/20 train/test split
idx <- sample(seq_len(nrow(rq5)), size = round(0.8 * nrow(rq5)))
train <- rq5[idx, ]
test  <- rq5[-idx, ]

m0 <- lm(log_price ~ log_area, data = train)                                    # baseline
m1 <- lm(log_price ~ log_area + bedrooms + bathrooms + property_type + county, data = train)
m2 <- lm(log_price ~ log_area + bedrooms + bathrooms + property_type + county + ber_band, data = train)
summary(m2)
f_test5 <- anova(m1, m2)        # partial F-test for adding BER band
print(f_test5)

rmse <- function(m, d) sqrt(mean((d$price - exp(predict(m, d)))^2))
model_cmp <- data.frame(
  model = c("Baseline: log(area)", "Full without BER", "Full with BER"),
  adj_r2 = sapply(list(m0, m1, m2), function(m) summary(m)$adj.r.squared),
  test_rmse_eur = sapply(list(m0, m1, m2), rmse, d = test))
print(model_cmp)
write.csv(model_cmp, "output/rq5_model_comparison.csv", row.names = FALSE)

ber_coef <- as.data.frame(summary(m2)$coefficients) %>%
  tibble::rownames_to_column("term") %>% filter(grepl("ber_band", term)) %>%
  mutate(premium_pct = (exp(Estimate) - 1) * 100)
ci <- confint(m2)[ber_coef$term, ]
ber_coef$ci_low_pct <- (exp(ci[, 1]) - 1) * 100
ber_coef$ci_high_pct <- (exp(ci[, 2]) - 1) * 100
print(ber_coef)
write.csv(ber_coef, "output/rq5_ber_premium.csv", row.names = FALSE)

other_coef <- as.data.frame(summary(m2)$coefficients) %>%
  tibble::rownames_to_column("term") %>%
  filter(!grepl("county|ber_band", term)) %>%
  mutate(effect_pct = (exp(Estimate) - 1) * 100)
print(other_coef)
write.csv(other_coef, "output/rq5_other_coefficients.csv", row.names = FALSE)

# Assumption checks
vif5 <- car::vif(m2)
print(vif5)
sw_resid <- shapiro.test(sample(residuals(m2), 5000))
print(sw_resid)
png("figures/fig11_rq5_diagnostics.png", width = 1800, height = 1600, res = 200)
par(mfrow = c(2, 2)); plot(m2, pch = 20, cex = 0.4); dev.off()

f12 <- ggplot(ber_coef, aes(sub("ber_band", "", term), premium_pct)) +
  geom_col(fill = "#2a6f97") +
  geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct), width = 0.2) +
  geom_hline(yintercept = 0) +
  labs(title = "RQ5: Price premium vs D-rated homes, other features held constant",
       x = "BER band", y = "Premium (%) with 95% CI")
save_fig("fig12_rq5_ber_premium.png", f12)

# =============================================================================
# 7. Results summary table
# =============================================================================
fmt_p <- function(p) format.pval(p, digits = 3, eps = 2.2e-16)
results <- data.frame(
  RQ = paste0("RQ", 1:5),
  test = c("Kruskal-Wallis", "Spearman correlation", "Wilcoxon rank-sum (one-sided)",
           "Chi-square test of independence", "Partial F-test (multiple regression)"),
  statistic = c(sprintf("H = %.1f, df = %d", kw1$statistic, kw1$parameter),
                sprintf("rho = %.3f", sp2$estimate),
                sprintf("W = %.0f", w3$statistic),
                sprintf("X2 = %.1f, df = %d", chi4$statistic, chi4$parameter),
                sprintf("F = %.1f, df = %d, %d", f_test5$F[2], f_test5$Df[2], f_test5$Res.Df[2])),
  p_value = fmt_p(c(kw1$p.value, sp2$p.value, w3$p.value, chi4$p.value, f_test5$`Pr(>F)`[2])),
  effect_size = c(sprintf("epsilon2 = %.3f", eps1), sprintf("rho = %.3f", sp2$estimate),
                  sprintf("r = %.3f", r3), sprintf("Cramer's V = %.3f", cramer_v),
                  sprintf("Adj R2 %.3f -> %.3f", model_cmp$adj_r2[2], model_cmp$adj_r2[3])),
  n = c(nrow(rq1), nrow(clean), nrow(clean), sum(tab4), nrow(train)))
print(results)
write.csv(results, "output/results_summary.csv", row.names = FALSE)

# Key test objects for reference
sink("output/all_test_output.txt")
cat("=== Cleaning steps ===\n"); print(steps)
cat("\n=== Numeric summary ===\n"); print(as.data.frame(num_summary))
cat("\n=== Categorical counts ===\n"); print(cat_counts)
cat("\n=== Spearman matrix ===\n"); print(round(cor_mat, 2))
cat("\n=== Normality ===\n"); print(sw_price); print(sw_logprice); print(sw_area)
cat("\n=== RQ1 ===\n"); print(as.data.frame(rq1_desc)); print(kw1); cat("epsilon2:", eps1, "\n"); print(pw1); print(kw1b)
cat("\n=== RQ2 ===\n"); print(sp2); print(pe2)
cat("\n=== RQ3 ===\n"); print(as.data.frame(rq3_desc)); print(w3); cat("r:", r3, "\n"); print(w3b)
cat("\n=== RQ4 ===\n"); print(tab4); print(chi4); print(round(chi4$expected, 1)); cat("Cramer's V:", cramer_v, "\n")
print(round(prop.table(tab4, 1) * 100, 1)); print(round(chi4$stdres, 1))
cat("\n=== RQ5 ===\n"); print(summary(m2)); print(f_test5); print(model_cmp); print(ber_coef); print(other_coef); print(vif5); print(sw_resid)
cat("\n=== Results summary ===\n"); print(results)
sink()

cat("\nDone. Figures in ./figures, tables in ./output\n")
