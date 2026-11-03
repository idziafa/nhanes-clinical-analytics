library(NHANES)
library(dplyr)
library(ggplot2)

data("NHANES")

clinical_data <- NHANES |>
  select(
    ID,
    Age,
    Sex,
    Weight,
    Height,
    BMI,
    BPSysAve,
    BPDiaAve,
    Pulse
  )

## DATA INSPECTION
str(clinical_data)
nrow(clinical_data)
ncol(clinical_data)

colSums(is.na(clinical_data))

#Total missing cells
clinical_data |>
  summarise(
    total_records = n(),
    total_misisng_records = sum(is.na(clinical_data)),
    pct_missing_records = 
      round(sum(is.na(clinical_data))/(nrow(clinical_data) * ncol(clinical_data)) *100, 2)
    
  )

##Total distinct participants
clinical_data |>
  summarise(unique_participants = n_distinct(ID))

clinical_data |>
filter(duplicated(ID))

##Percentage of records with at least a missing value

clinical_data|>
  summarise(total_records = n(),
            
            incomplete_records = sum(if_any(
              everything(), is.na
            )),
            incomplete_pct = round(incomplete_records/total_records *100, 2)
            )

clinical_data |>
  summarise(
    across(
      everything(), \(x) round(mean(is.na(x)) * 100, 2),
      .names = "{.col}_missing_pct"
    )
  )

##MISSING BP VRS AGE ASSOCIATION
clinical_data |>
  mutate(age_group =
           case_when(
             Age < 18 ~ "Children",
             Age < 65 ~ "Adults",
             TRUE     ~ "Older adults"
           )) |>
  group_by(age_group) |>
  summarise(
    total_records = n(),
    total_missing_sbp = sum(is.na(BPSysAve)),
    .groups = "drop"
  )

##MISSING SBP AMONG CHILDREN GROUPS
clinical_data |>
  filter(Age < 18) |>
  mutate(
    children_age_group = case_when(
      Age < 6 ~ "Under 6 Years",
      Age < 12 ~ "6-11 Years",
      TRUE     ~ "12- 17 Years"
    )
  ) |>
  group_by(children_age_group) |>
  summarise(
    total_records = n(),
    total_missing_sbp = sum(is.na(BPSysAve)),
    pct = round(total_missing_sbp/total_records * 100, 2)
  )

##MISSING SBP BETWEEN AGE 5-12
clinical_data |>
  filter(between(Age, 5, 12)) |>
  group_by(Age) |>
  summarise(
    total_records = n(),
    total_missing_sbp = sum(is.na(BPSysAve)),
    pct = round(total_missing_sbp/total_records * 100, 2),
    .groups = "drop"
  ) |>
  arrange(Age)

##PHASE 2 - PREPARING OUR ANALYTICAL DATASET
adult_clinical_data <- clinical_data |>
  filter(Age> 17) |>
  mutate(bmi_status = 
           case_when(
             is.na(BMI) ~ NA_character_,
             BMI <18.5 ~ "Underweight",
             BMI < 25  ~ "Normal",
             BMI < 30  ~  "Overweight",
             TRUE      ~  "Obese"
           ))

dim(adult_clinical_data)

adult_clinical_data |>
  filter(bmi_status == "Missing") |>
  count(bmi_status)

adult_clinical_data |>
  count(bmi_status)

##creating adult dataset of disntinct IDs
adult_unique_data <- adult_clinical_data |>
  distinct(ID, .keep_all = TRUE)

dim(adult_unique_data)


###PHASE 3 - DESCRIPTIVE STATISTICS
adult_unique_data |>
  summarise(
    total_adults = n_distinct(ID),
    avg_age      = mean(Age, na.rm =TRUE),
    min_age      = min(Age, na.rm =TRUE),
    max_age      = max(Age, na.rm =TRUE),
    avg_sbp      = round(mean(BPSysAve, na.rm = TRUE),2),
    avg_dbp      = round(mean(BPDiaAve, na.rm = TRUE),2),
    avg_pulse    = round(mean(Pulse, na.rm = TRUE),2),
    total_valid_BMI = sum(!is.na(BMI)),
    total_valid_sbp = sum(!is.na(BPSysAve)),
    median_age = median(Age, na.rm = TRUE),
    
    avg_bmi = round(mean(BMI, na.rm = TRUE),2),
    
    sd_bmi = round(sd(BMI, na.rm = TRUE),2),
    
    total_valid_dbp = sum(!is.na(BPDiaAve))
    
  )

adult_unique_data|>
  count(bmi_status)


saveRDS(
  adult_unique_data,
  file = "data/adult_unique_data.rds"
)
saveRDS(
  adult_clinical_data,
  file = "data/adult_clinical_data.rds"
)
saveRDS(
  clinical_data,
  file = "data/clinical_data.rds"
)
getwd()
list.files("data")
save.image(file = "analysis_checkpoint.RData")



library(dplyr)
library(ggplot2)

clinical_data <- readRDS("data/clinical_data.rds")

adult_clinical_data <- readRDS("data/adult_clinical_data.rds")

adult_unique_data <- readRDS("data/adult_unique_data.rds")

dim(adult_unique_data)


##PHASE 4 - EPLORATORY DATA VISUALIZATION WITH ggplot2
ggplot(
  data = adult_unique_data,
  aes(x = BMI)
) +
  geom_histogram(
    binwidth = 2,
    fill = "#38BDF8",
    colour = "#0F172A"
  ) +
  labs(
    title = "Distribution of BMI Among Adult Participants",
    x = "BMI (kg/m²)",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#0F172A", colour = NA),
    panel.background = element_rect(fill = "#0F172A", colour = NA),
    panel.grid.major = element_line(colour = "#334155"),
    panel.grid.minor = element_blank(),
    text = element_text(colour = "#E2E8F0"),
    axis.text = element_text(colour = "#CBD5E1"),
    plot.title = element_text(face = "bold", size = 14)
  )




theme_nhanes_dark <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#0F172A", colour = NA),
      panel.background = element_rect(fill = "#0F172A", colour = NA),
      panel.grid.major = element_line(colour = "#334155"),
      panel.grid.minor = element_blank(),
      text = element_text(colour = "#E2E8F0"),
      axis.text = element_text(colour = "#CBD5E1"),
      plot.title = element_text(face = "bold", size = 14)
    )
}
##INVESTIGATING UNUSUAL BMI VALUES
adult_unique_data|>
  summarise(
    min_bmi = min(BMI, na.rm = TRUE),
    max_bmi = max(BMI, na.rm = TRUE)
  )
    adult_unique_data |>
      filter(BMI> 50) |>
      summarise(total_participants = n())
  
    adult_unique_data|>
      arrange(desc(BMI))|>
      select(Age, Sex,Height, BMI) |>
      slice_head(n=5)
    
    adult_bmi_check <- adult_unique_data |>
      mutate(
        bmi_calculated = Weight / (Height / 100)^2,
        bmi_difference = abs(BMI - bmi_calculated)
      )
    
    
    ## PHASE 4 - BMI VALIDATION
    
    adult_bmi_check <- adult_unique_data |>
      mutate(
        bmi_calculated = Weight / (Height / 100)^2,
        bmi_difference = abs(BMI - bmi_calculated)
      )
    
    # 1. Summary of BMI discrepancies
    adult_bmi_check |>
      summarise(
        total_records = n(),
        valid_comparisons = sum(!is.na(bmi_difference)),
        max_difference = round(max(bmi_difference, na.rm = TRUE), 4),
        mean_difference = round(mean(bmi_difference, na.rm = TRUE), 4),
        differences_above_0_1 = sum(bmi_difference > 0.1, na.rm = TRUE)
      )
    
    # 2. Five largest BMI discrepancies
    adult_bmi_check |>
      filter(!is.na(bmi_difference)) |>
      arrange(desc(bmi_difference)) |>
      select(
        ID, Age, Sex, Weight, Height,
        BMI, bmi_calculated, bmi_difference
      ) |>
      slice_head(n = 5)
    
    ggplot(
      data = adult_unique_data,
      mapping = aes(x = Sex, y = BMI, fill = Sex)
    ) +
      geom_boxplot(
        na.rm = TRUE,
        colour = "#E2E8F0",
        linewidth = 0.7,
        outlier.colour = "#FACC15",
        outlier.fill = "#FACC15",
        outlier.shape = 21,
        outlier.size = 2,
        outlier.alpha = 0.8,
        outlier.stroke = 0.3
      ) +
      scale_fill_manual(
        values = c(
          "female" = "#F87171",
          "male" = "#06B6D4"
        )
      ) +
      labs(
        title = "BMI Distribution by Sex",
        subtitle = "Adult participants aged 18 years and above",
        x = "Sex",
        y = "BMI (kg/m²)"
      ) +
      theme_nhanes_dark() +
      theme(legend.position = "none")
    
    
    adult_unique_data |>
      group_by(Sex) |>
      summarise(
        total_participants = n(),
        mean_BMI = round(mean(BMI, na.rm = TRUE), 2),
        median_BMI = round(median(BMI, na.rm = TRUE),2),
        sd_BMI    = round(sd(BMI, na.rm = TRUE), 2),
        min_BMI  = min(BMI, na.rm = TRUE),
        max_BMI = max(BMI, na.rm = TRUE),
        .groups = "drop"
      )
    
    # PHASE 4.5 - INVESTIGATING RELATIONSHIP BETWEEN AGE AND BMI
    #------- DOES BMI APPEAR TO CHANGE WITH INCREASING AGE AMONG THE ADULTS
    ggplot(
      adult_unique_data,
      aes(x = Age, y = BPDiaAve)
    ) +
      geom_point(
        colour = "#38BDF8",
        alpha = 0.35,
        size = 1.5,
        na.rm = TRUE
      ) +
      geom_smooth(
        method = "lm",
        colour = "#FACC15",
        se = FALSE,
        na.rm = TRUE
      ) +
      labs(
        title = "Relationship Between Age and BPSysAve",
        subtitle = "Adult participants aged 18 years and above",
        x = "Age (years)",
        y = "BPDiaAve (mmHg)"
      ) +
      theme_nhanes_dark()
    
    adult_unique_data |>
      summarise(
        correlation = round(
          cor(Age, BMI, use = "complete.obs"),
          3
        )
      )
    
    adult_unique_data |>
      summarise(
        correlation_age_sbp = round(
          cor(Age, BPSysAve, use = "complete.obs"),
          3
        )
      )
    
    adult_unique_data |>
      summarise(
        age_sbp = round(
          cor(Age, BPSysAve, use = "complete.obs"),
          3
        ),
        age_dbp = round(
          cor(Age, BPDiaAve, use = "complete.obs"),
          3
        )
      )
    
    ## COMPARING BP ACROSS AGES
    adult_bp_data <- adult_unique_data |>
      mutate(
        age_group = case_when(
          Age < 40 ~ "Young adults",
          Age < 65 ~ "Middle-aged adults",
          TRUE ~ "Older adults"
        ),
        age_group = factor(
          age_group,
          levels = c(
            "Young adults",
            "Middle-aged adults",
            "Older adults"
          )
        )
      )
    
    adult_bp_data|>
      group_by(age_group) |>
      summarise(
        total_participants = n(),
        mean_sbp = round(mean(BPSysAve, na.rm =TRUE),2),
        mean_dbp = round(mean(BPDiaAve, na.rm =TRUE),2),
        valid_SBP = sum(!is.na(BPSysAve)),
        valid_dbp = sum(!is.na(BPDiaAve))
      )
    
    
    library(tidyr)
    
    bp_summary <- adult_bp_data |>
      group_by(age_group) |>
      summarise(
        mean_sbp = round(mean(BPSysAve, na.rm = TRUE), 2),
        mean_dbp = round(mean(BPDiaAve, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    bp_long <- bp_summary |>
      pivot_longer(
        cols = c(mean_sbp, mean_dbp),
        names_to = "bp_type",
        values_to = "mean_bp"
      )
    
    bp_long
    
    
    ##COMPARATIVE CHART
    ggplot(
      bp_long,
      aes(
        x = age_group,
        y = mean_bp,
        colour = bp_type,
        group = bp_type
      )
    ) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      geom_text(
        aes(label = round(mean_bp, 1)),
        vjust = -1,
        size = 3.5,
        show.legend = FALSE
      ) +
      scale_colour_manual(
        values = c(
          "mean_sbp" = "#38BDF8",
          "mean_dbp" = "#FACC15"
        ),
        labels = c(
          "mean_dbp" = "Diastolic BP",
          "mean_sbp" = "Systolic BP"
        )
      ) +
      labs(
        title = "Mean Blood Pressure Across Adult Age Groups",
        subtitle = "Educational NHANES participant-level dataset",
        x = "Age Group",
        y = "Mean Blood Pressure (mmHg)",
        colour = "Measurement"
      ) +
      theme_nhanes_dark()
    
