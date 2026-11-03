# NHANES Clinical Analytics — reproducible preparation
# Run from the project root: Rscript scripts/01_prepare_data.R
needed <- c("NHANES", "dplyr")
missing_packages <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
library(dplyr)

if (!dir.exists("data") || !dir.exists("scripts")) {
  stop("Run this script from nhanes_clinical_analytics/ (the project root).")
}
dir.create("outputs", showWarnings = FALSE)
data("NHANES", package = "NHANES")

selected_columns <- c("ID", "Age", "Sex", "Weight", "Height", "BMI", "BPSysAve", "BPDiaAve", "Pulse")
stopifnot(all(selected_columns %in% names(NHANES)))
clinical_data <- NHANES |> select(all_of(selected_columns))
stopifnot(!anyNA(clinical_data$ID), !anyNA(clinical_data$Age))

adult_clinical_data <- clinical_data |>
  filter(Age >= 18) |>
  mutate(bmi_status = case_when(
    is.na(BMI) ~ NA_character_,
    BMI < 18.5 ~ "Underweight",
    BMI < 25 ~ "Normal",
    BMI < 30 ~ "Overweight",
    TRUE ~ "Obese"
  ))

# Validate agreement across repeated IDs BEFORE selecting one record per ID.
# Count NA as a distinct value: an NA/non-NA disagreement is a conflict.
measurement_cols <- setdiff(selected_columns, "ID")
id_consistency <- adult_clinical_data |>
  group_by(ID) |>
  summarise(across(all_of(measurement_cols), ~ n_distinct(.x, na.rm = FALSE)), .groups = "drop") |>
  mutate(conflict = if_any(all_of(measurement_cols), ~ .x > 1))
conflicting_ids <- id_consistency |> filter(conflict)
write.csv(conflicting_ids, "outputs/conflicting_ids.csv", row.names = FALSE, na = "")
if (nrow(conflicting_ids) > 0) {
  stop("Conflicting measurements found for ", nrow(conflicting_ids),
       " IDs. Inspect outputs/conflicting_ids.csv before deduplicating.")
}

adult_unique_data <- adult_clinical_data |> distinct(ID, .keep_all = TRUE)
stopifnot(nrow(adult_unique_data) == n_distinct(adult_unique_data$ID))

saveRDS(clinical_data, "data/clinical_data.rds")
saveRDS(adult_clinical_data, "data/adult_clinical_data.rds")
saveRDS(adult_unique_data, "data/adult_unique_data.rds")

prep_audit <- data.frame(
  metric = c("Source rows", "Source unique IDs", "Adult resampled rows",
             "Adult unique participant IDs", "Extra adult resampled occurrences",
             "Adult IDs with conflicting measurements", "Adult records missing BMI",
             "Adult records missing SBP", "Adult records missing DBP"),
  value = c(nrow(clinical_data), n_distinct(clinical_data$ID),
            nrow(adult_clinical_data), nrow(adult_unique_data),
            nrow(adult_clinical_data) - nrow(adult_unique_data),
            nrow(conflicting_ids), sum(is.na(adult_unique_data$BMI)),
            sum(is.na(adult_unique_data$BPSysAve)), sum(is.na(adult_unique_data$BPDiaAve)))
)
write.csv(prep_audit, "outputs/preparation_audit.csv", row.names = FALSE)
print(prep_audit, row.names = FALSE)
message("Data prepared successfully. Now render reports/clinical_health_report.qmd")
