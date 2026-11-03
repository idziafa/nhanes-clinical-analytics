# Generate analysis tables reproducibly from the saved participant-level dataset.
# Run from project root after 01_prepare_data.R.
if (!requireNamespace("dplyr", quietly = TRUE)) stop("Install dplyr")
library(dplyr)
path <- "data/adult_unique_data.rds"
if (!file.exists(path)) stop("First run: Rscript scripts/01_prepare_data.R")
d <- readRDS(path)
dir.create("outputs", showWarnings = FALSE)

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)

summary <- d |>
  summarise(participants = n(), valid_bmi = sum(!is.na(BMI)),
            valid_sbp = sum(!is.na(BPSysAve)), valid_dbp = sum(!is.na(BPDiaAve)),
            mean_age = round(safe_mean(Age), 2), median_age = median(Age, na.rm = TRUE),
            mean_bmi = round(safe_mean(BMI), 2), sd_bmi = round(safe_sd(BMI), 2),
            mean_sbp = round(safe_mean(BPSysAve), 2),
            mean_dbp = round(safe_mean(BPDiaAve), 2),
            mean_pulse = round(safe_mean(Pulse), 2))

bmi <- d |>
  mutate(bmi_status = if_else(is.na(bmi_status), "Missing", bmi_status)) |>
  count(bmi_status, name = "participants") |>
  mutate(percent_all_rows = round(100 * participants / sum(participants), 2))

by_sex <- d |>
  group_by(Sex) |>
  summarise(participants = n(), valid_bmi = sum(!is.na(BMI)),
            mean_bmi = round(safe_mean(BMI), 2), median_bmi = median(BMI, na.rm = TRUE),
            sd_bmi = round(safe_sd(BMI), 2), .groups = "drop")

by_age <- d |>
  mutate(age_group = factor(case_when(Age < 40 ~ "18–39", Age < 65 ~ "40–64", TRUE ~ "65+"),
                            levels = c("18–39", "40–64", "65+"))) |>
  group_by(age_group) |>
  summarise(participants = n(), valid_sbp = sum(!is.na(BPSysAve)),
            valid_dbp = sum(!is.na(BPDiaAve)),
            mean_sbp = round(safe_mean(BPSysAve), 2),
            mean_dbp = round(safe_mean(BPDiaAve), 2), .groups = "drop")

missing <- data.frame(variable = names(d),
                      missing_n = vapply(d, function(x) sum(is.na(x)), integer(1))) |>
  mutate(missing_percent = round(100 * missing_n / nrow(d), 2))

write.csv(summary, "outputs/participant_summary.csv", row.names = FALSE)
write.csv(bmi, "outputs/bmi_distribution.csv", row.names = FALSE)
write.csv(by_sex, "outputs/bmi_by_sex.csv", row.names = FALSE)
write.csv(by_age, "outputs/bp_by_age.csv", row.names = FALSE)
write.csv(missing, "outputs/missingness_by_variable.csv", row.names = FALSE)
message("Five summary tables saved to outputs/")
