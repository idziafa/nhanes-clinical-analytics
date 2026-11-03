# NHANES Clinical Health Dashboard — Version 2
# Compatible design for Shiny 1.8.0 / bslib 0.6.1 / dplyr / ggplot2
# Place this file at: nhanes_clinical_analytics/dashboard/app.R
# Start from the project root: shiny::runApp('dashboard')

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. READ & PREPARE THE DATA
# ------------------------------------------------------------
# Shiny uses dashboard/ as the working directory while app.R runs.
source_file <- "../data/adult_unique_data.rds"
if (!file.exists(source_file)) {
  stop("Cannot find ", source_file,
       ". Put adult_unique_data.rds in the project's data/ directory.")
}
adult_data <- readRDS(source_file)
required_columns <- c("ID", "Age", "Sex", "Weight", "Height", "BMI",
                      "BPSysAve", "BPDiaAve", "Pulse")
if (!all(required_columns %in% names(adult_data))) {
  stop("The saved dataset is missing: ",
       paste(setdiff(required_columns, names(adult_data)), collapse = ", "))
}

adult_data <- adult_data |>
  mutate(
    Sex = as.character(Sex),
    bmi_status = case_when(
      is.na(BMI) ~ NA_character_,
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Normal",
      BMI < 30 ~ "Overweight",
      TRUE ~ "Obese"
    ),
    age_group = factor(
      case_when(
        Age < 40 ~ "18–39",
        Age < 65 ~ "40–64",
        TRUE ~ "65+"
      ),
      levels = c("18–39", "40–64", "65+")
    )
  )

# The unique-ID file should contain one row per participant.
unique_id_check <- nrow(adult_data) == n_distinct(adult_data$ID)

# Optional audit of the original adult rows, if the saved object exists.
# This only checks agreement in selected columns; it never alters the data.
original_file <- "../data/adult_clinical_data.rds"
audit_available <- file.exists(original_file)
if (audit_available) {
  original_adults <- readRDS(original_file)
  audit_cols <- intersect(required_columns[-1], names(original_adults))
  conflict_ids <- original_adults |>
    group_by(ID) |>
    summarise(across(all_of(audit_cols),
                     ~ n_distinct(.x, na.rm = FALSE)), .groups = "drop") |>
    filter(if_any(all_of(audit_cols), ~ .x > 1))
  conflict_count <- nrow(conflict_ids)
  duplicate_occurrences <- nrow(original_adults) - n_distinct(original_adults$ID)
}

# ------------------------------------------------------------
# 2. HELPERS AND DESIGN SYSTEM
# ------------------------------------------------------------
navy <- "#0F172A"
cyan <- "#38BDF8"
gold <- "#FACC15"
coral <- "#F87171"
green <- "#34D399"
muted <- "#94A3B8"
bmi_palette <- c("Underweight" = cyan, "Normal" = green,
                 "Overweight" = gold, "Obese" = coral,
                 "Missing" = muted)
sex_palette <- c("female" = coral, "male" = "#06B6D4")

fmt_num <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("N/A")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

safe_avg <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_cor <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3 || n_distinct(x[ok]) < 2 || n_distinct(y[ok]) < 2) {
    return(NA_real_)
  }
  cor(x[ok], y[ok])
}

cor_words <- function(r) {
  if (is.na(r)) return("Insufficient variation or observations for a correlation estimate.")
  direction <- if (r > 0) "positive" else if (r < 0) "negative" else "zero"
  strength <- if (abs(r) < 0.10) "negligible" else if (abs(r) < 0.30) "weak" else
    if (abs(r) < 0.50) "moderate" else "strong"
  paste0("Pearson's r = ", fmt_num(r, 3), " (", strength, " ", direction,
         " linear association). This does not establish causation.")
}

theme_nhanes_dark <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = navy, colour = NA),
      panel.background = element_rect(fill = navy, colour = NA),
      panel.grid.major = element_line(colour = "#334155", linewidth = .35),
      panel.grid.minor = element_blank(),
      text = element_text(colour = "#E2E8F0"),
      axis.text = element_text(colour = "#CBD5E1"),
      axis.title = element_text(colour = "#CBD5E1"),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = muted),
      legend.background = element_rect(fill = navy, colour = NA),
      legend.key = element_rect(fill = navy, colour = NA),
      legend.text = element_text(colour = "#E2E8F0"),
      legend.title = element_text(colour = "#E2E8F0")
    )
}

insight_box <- function(output_id) {
  div(class = "insight", div(class = "insight-heading", "◈  ANALYTICAL INSIGHT"),
      uiOutput(output_id))
}

chart_panel <- function(title, plot_id, note_id, height = "350px") {
  div(class = "chart-panel",
      h4(title), plotOutput(plot_id, height = height), insight_box(note_id))
}

kpi_card <- function(label, output_id, unit, accent) {
  div(class = "kpi", style = paste0("border-top: 3px solid ", accent, ";"),
      div(class = "kpi-label", label),
      div(class = "kpi-value", textOutput(output_id, inline = TRUE)),
      div(class = "kpi-unit", unit))
}

# ------------------------------------------------------------
# 3. USER INTERFACE
# ------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bg = navy, fg = "#E2E8F0", primary = cyan),
  tags$head(tags$style(HTML('
    body { background: #0F172A; color: #E2E8F0; }
    .container-fluid { padding: 0; max-width: 100%; }
    .app-shell { display: flex; align-items: stretch; min-height: 100vh; }
    .sidebar-panel-v2 { background: #111D30; border-right: 1px solid #334155;
      width: 270px; flex: 0 0 270px; padding: 25px 18px; }
    .side-brand { color: #38BDF8; font-weight: 800; font-size: .77rem;
      letter-spacing: .13em; }
    .side-title { color: #F8FAFC; font-weight: 750; font-size: 1.25rem;
      line-height: 1.35; margin: 10px 0 21px; }
    .side-label { color: #94A3B8; letter-spacing: .08em; font-size: .72rem;
      font-weight: 700; margin: 20px 0 9px; }
    .sidebar-panel-v2 .form-label, .sidebar-panel-v2 .control-label { color: #CBD5E1; }
    .sidebar-panel-v2 .radio { margin-bottom: 10px; }
    .sidebar-panel-v2 input[type=radio] { accent-color: #38BDF8; }
    .sidebar-panel-v2 .irs-bar, .sidebar-panel-v2 .irs-single { background: #38BDF8; }
    .sidebar-panel-v2 .irs-handle { border-color: #38BDF8; }
    .sidebar-panel-v2 .btn { width: 100%; margin-top: 8px; }
    .main-v2 { flex: 1; min-width: 0; padding: 28px 32px 36px; }
    .eyebrow { color: #38BDF8; font-size: .76rem; letter-spacing: .14em;
      font-weight: 800; }
    .dashboard-title { color: #F8FAFC; font-weight: 800; margin: 9px 0 5px; }
    .dashboard-subtitle { color: #94A3B8; margin-bottom: 20px; }
    .kpi-row { display: grid; grid-template-columns: repeat(5,minmax(140px,1fr));
      gap: 12px; margin: 18px 0 24px; }
    .kpi { background: #1E293B; border: 1px solid #334155; border-radius: 12px;
      padding: 15px 14px; min-height: 112px; }
    .kpi-label { color: #CBD5E1; font-size: .82rem; }
    .kpi-value { font-size: 1.65rem; font-weight: 800; color: #F8FAFC; margin: 8px 0 3px; }
    .kpi-unit { color: #94A3B8; font-size: .74rem; }
    .chart-panel { background: #111D30; border: 1px solid #334155;
      border-radius: 13px; padding: 20px; margin-bottom: 20px; }
    .chart-panel h4 { color: #F8FAFC; font-size: 1.06rem; font-weight: 750;
      margin: 0 0 14px; }
    .insight { background: #1E293B; border-left: 3px solid #FACC15;
      border-radius: 8px; padding: 13px 16px; margin-top: 12px; }
    .insight-heading { color: #FACC15; letter-spacing: .07em;
      font-weight: 800; font-size: .73rem; margin-bottom: 6px; }
    .insight p { color: #CBD5E1; margin: 0; font-size: .90rem; line-height: 1.5; }
    .section-title { font-weight: 750; margin: 8px 0 17px; color: #F8FAFC; }
    .small-note { color: #94A3B8; font-size: .84rem; line-height: 1.55; }
    .table { color: #E2E8F0; }
    .table > :not(caption) > * > * { background-color: #111D30; color: #E2E8F0;
      border-color: #334155; }
    .selectize-input, .form-control { background-color: #1E293B !important;
      border-color: #475569 !important; color: #F8FAFC !important; }
    @media (max-width: 1120px) {
      .kpi-row { grid-template-columns: repeat(3,minmax(150px,1fr)); }
    }
    @media (max-width: 760px) {
      .app-shell { display: block; }
      .sidebar-panel-v2 { width: auto; padding: 20px; border-right: none;
        border-bottom: 1px solid #334155; }
      .main-v2 { padding: 20px 14px; }
      .kpi-row { grid-template-columns: repeat(2,minmax(130px,1fr)); }
    }
  '))),
  
  div(class = "app-shell",
      div(class = "sidebar-panel-v2",
          div(class = "side-brand", "NHANES • ANALYTICS"),
          div(class = "side-title", "Clinical Health Explorer"),
          div(class = "side-label", "NAVIGATION"),
          radioButtons("section", NULL,
                       choices = c("Overview", "BMI Analysis", "Blood Pressure", "Data Quality"),
                       selected = "Overview"),
          div(class = "side-label", "FILTERS"),
          selectInput("sex_filter", "Sex", c("All", "female", "male")),
          sliderInput("age_filter", "Age range (years)",
                      min = 18, max = 80, value = c(18, 80), step = 1, sep = ""),
          selectInput("bmi_filter", "BMI category",
                      c("All", "Underweight", "Normal", "Overweight", "Obese", "Missing")),
          actionButton("reset_filters", "Reset filters", class = "btn-outline-info"),
          hr(),
          div(class = "small-note",
              "Educational NHANES data. Filter changes update the figures and notes throughout this dashboard.")
      ),
      div(class = "main-v2",
          div(class = "eyebrow", "INTERACTIVE CLINICAL DATA ANALYTICS"),
          h2(class = "dashboard-title", "Adult Health Indicators"),
          div(class = "dashboard-subtitle",
              "Explore BMI, blood pressure, age patterns and measurement completeness."),
          div(class = "kpi-row",
              kpi_card("Participants", "total_adults", "unique IDs in selection", cyan),
              kpi_card("Mean BMI", "mean_bmi", "kg/m²", gold),
              kpi_card("Mean systolic BP", "mean_sbp", "mmHg", green),
              kpi_card("Mean diastolic BP", "mean_dbp", "mmHg", coral),
              kpi_card("Mean pulse", "mean_pulse", "beats/min", "#A78BFA")
          ),
          conditionalPanel("input.section == 'Overview'",
                           h3(class = "section-title", "Overview"),
                           chart_panel("Distribution of BMI", "bmi_hist", "bmi_hist_insight"),
                           chart_panel("BMI category breakdown", "bmi_categories", "bmi_categories_insight")
          ),
          conditionalPanel("input.section == 'BMI Analysis'",
                           h3(class = "section-title", "BMI Analysis"),
                           chart_panel("BMI by sex", "bmi_boxplot", "bmi_box_insight"),
                           chart_panel("Age and BMI", "age_bmi_scatter", "age_bmi_insight")
          ),
          conditionalPanel("input.section == 'Blood Pressure'",
                           h3(class = "section-title", "Blood Pressure"),
                           chart_panel("Mean systolic and diastolic BP by age group",
                                       "bp_age_group", "bp_age_insight", "380px"),
                           chart_panel("Age and systolic BP", "age_sbp_scatter", "age_sbp_insight")
          ),
          conditionalPanel("input.section == 'Data Quality'",
                           h3(class = "section-title", "Data Quality & Downloads"),
                           div(class = "chart-panel", h4("Missing measurements under current filters"),
                               tableOutput("missing_table"), insight_box("missing_insight")),
                           div(class = "chart-panel", h4("Deduplication audit"),
                               uiOutput("audit_summary")),
                           div(class = "chart-panel", h4("Preview of selected records"),
                               tableOutput("sample_table"),
                               downloadButton("download_data", "Download selected records (CSV)"),
                               downloadButton("download_summary", "Download summary (CSV)"))
          ),
          div(class = "small-note",
              "SOURCE & LIMITATIONS — NHANES R educational dataset, adult unique-ID subset. ",
              "Figures are descriptive, filter-specific, and not US population prevalence estimates ",
              "or a basis for diagnosis. Missingness can be related to examination protocols. ",
              "This demonstration does not implement NHANES survey weights or complex survey design.")
      )
  )
)

# ------------------------------------------------------------
# 4. SERVER: REACTIVE FILTERS, KPIs, CHARTS AND INSIGHTS
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "sex_filter", selected = "All")
    updateSliderInput(session, "age_filter", value = c(18, 80))
    updateSelectInput(session, "bmi_filter", selected = "All")
  })
  
  filtered_data <- reactive({
    df <- adult_data |>
      filter(Age >= input$age_filter[1], Age <= input$age_filter[2])
    if (input$sex_filter != "All") df <- df |> filter(Sex == input$sex_filter)
    if (input$bmi_filter == "Missing") {
      df <- df |> filter(is.na(bmi_status))
    } else if (input$bmi_filter != "All") {
      df <- df |> filter(bmi_status == input$bmi_filter)
    }
    df
  })
  
  # KPI numbers update whenever any filter changes.
  output$total_adults <- renderText(format(nrow(filtered_data()), big.mark = ","))
  output$mean_bmi <- renderText(fmt_num(safe_avg(filtered_data()$BMI)))
  output$mean_sbp <- renderText(fmt_num(safe_avg(filtered_data()$BPSysAve)))
  output$mean_dbp <- renderText(fmt_num(safe_avg(filtered_data()$BPDiaAve)))
  output$mean_pulse <- renderText(fmt_num(safe_avg(filtered_data()$Pulse)))
  
  # BMI distribution.
  output$bmi_hist <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BMI))
    validate(need(nrow(df) > 0, "No recorded BMI values for these filters."))
    ggplot(df, aes(x = BMI)) +
      geom_histogram(binwidth = 2, fill = cyan, colour = navy, boundary = 0) +
      labs(x = "BMI (kg/m²)", y = "Participants") + theme_nhanes_dark()
  }, bg = navy)
  output$bmi_hist_insight <- renderUI({
    x <- filtered_data()$BMI
    x <- x[!is.na(x)]
    if (!length(x)) return(tags$p("No recorded BMI measurements in this selection."))
    tags$p(paste0("Based on ", format(length(x), big.mark = ","),
                  " recorded BMI values: median ", fmt_num(median(x)),
                  ", mean ", fmt_num(mean(x)), ", range ", fmt_num(min(x)), "–",
                  fmt_num(max(x)), " kg/m². Compare the mean with the median and inspect the right tail."))
  })
  
  # Distinctive colours for BMI categories.
  output$bmi_categories <- renderPlot({
    df <- filtered_data() |>
      mutate(category = if_else(is.na(bmi_status), "Missing", bmi_status)) |>
      count(category)
    validate(need(nrow(df) > 0, "No participants match these filters."))
    df$category <- factor(df$category, levels = names(bmi_palette))
    ggplot(df, aes(category, n, fill = category)) +
      geom_col(width = .68) +
      geom_text(aes(label = n), vjust = -.45, colour = "#F8FAFC", size = 3.6) +
      scale_fill_manual(values = bmi_palette, drop = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0, .14))) +
      labs(x = "BMI category", y = "Participants") +
      theme_nhanes_dark() + theme(legend.position = "none")
  }, bg = navy)
  output$bmi_categories_insight <- renderUI({
    df <- filtered_data()
    if (!nrow(df)) return(tags$p("No participants match these filters."))
    valid <- sum(!is.na(df$bmi_status))
    combined <- sum(df$bmi_status %in% c("Overweight", "Obese"), na.rm = TRUE)
    if (!valid) return(tags$p("All BMI values in the current selection are missing."))
    tags$p(paste0(format(combined, big.mark = ","), " of ",
                  format(valid, big.mark = ","), " participants with measured BMI (",
                  fmt_num(100 * combined / valid, 1),
                  "%) fall in the overweight or obese categories. ",
                  "This is a sample description, not a population prevalence estimate."))
  })
  
  # BMI by sex.
  output$bmi_boxplot <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BMI))
    validate(need(nrow(df) > 0, "No BMI values under these filters."))
    ggplot(df, aes(Sex, BMI, fill = Sex)) +
      geom_boxplot(colour = "#E2E8F0", linewidth = .7,
                   outlier.colour = gold, outlier.size = 2, outlier.alpha = .85) +
      scale_fill_manual(values = sex_palette) +
      labs(x = "Sex", y = "BMI (kg/m²)") + theme_nhanes_dark() +
      theme(legend.position = "none")
  }, bg = navy)
  output$bmi_box_insight <- renderUI({
    df <- filtered_data() |> filter(!is.na(BMI))
    if (!nrow(df)) return(tags$p("No BMI values to compare."))
    parts <- df |> group_by(Sex) |>
      summarise(median_bmi = median(BMI), sd_bmi = sd(BMI), n = n(), .groups = "drop")
    if (nrow(parts) == 1) {
      return(tags$p(paste0("Only ", parts$Sex[1], " participants match these filters (n = ",
                           parts$n[1], "). Median BMI: ", fmt_num(parts$median_bmi[1]), ".")))
    }
    female_med <- parts$median_bmi[parts$Sex == "female"]
    male_med <- parts$median_bmi[parts$Sex == "male"]
    tags$p(paste0("Median BMI: female ", fmt_num(female_med),
                  "; male ", fmt_num(male_med),
                  ". Compare box heights for variability and points beyond the whiskers for potential outliers."))
  })
  
  # Age and BMI.
  output$age_bmi_scatter <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BMI))
    validate(need(nrow(df) > 0, "No BMI values under these filters."))
    p <- ggplot(df, aes(Age, BMI)) +
      geom_point(alpha = .32, size = 1.5, colour = cyan) +
      labs(x = "Age (years)", y = "BMI (kg/m²)") + theme_nhanes_dark()
    if (nrow(df) >= 3 && n_distinct(df$Age) >= 2) {
      p <- p + geom_smooth(method = "lm", se = FALSE, colour = gold)
    }
    p
  }, bg = navy)
  output$age_bmi_insight <- renderUI({
    df <- filtered_data()
    tags$p(cor_words(safe_cor(df$Age, df$BMI)))
  })
  
  # Blood pressure across age groups.
  output$bp_age_group <- renderPlot({
    df <- filtered_data() |>
      group_by(age_group) |>
      summarise(sbp = safe_avg(BPSysAve), dbp = safe_avg(BPDiaAve), .groups = "drop")
    long <- bind_rows(
      transmute(df, age_group, bp_type = "Systolic BP", mean_bp = sbp),
      transmute(df, age_group, bp_type = "Diastolic BP", mean_bp = dbp)
    ) |> filter(!is.na(mean_bp))
    validate(need(nrow(long) > 0, "No recorded BP values under these filters."))
    ggplot(long, aes(age_group, mean_bp, fill = bp_type)) +
      geom_col(position = position_dodge(width = .8), width = .66) +
      geom_text(aes(label = sprintf("%.1f", mean_bp)),
                position = position_dodge(width = .8), vjust = -.5,
                colour = "#F8FAFC", size = 3.2) +
      scale_fill_manual(values = c("Systolic BP" = cyan, "Diastolic BP" = gold)) +
      scale_y_continuous(expand = expansion(mult = c(0, .18))) +
      labs(x = "Adult age group", y = "Mean BP (mmHg)", fill = "Measurement") +
      theme_nhanes_dark()
  }, bg = navy)
  output$bp_age_insight <- renderUI({
    df <- filtered_data()
    groups <- df |> group_by(age_group) |>
      summarise(sbp = safe_avg(BPSysAve), dbp = safe_avg(BPDiaAve), .groups = "drop")
    groups <- groups[!is.na(groups$sbp), , drop = FALSE]
    if (!nrow(groups)) return(tags$p("No valid BP measurements for this selection."))
    hi_sbp <- groups$age_group[which.max(groups$sbp)]
    hi_dbp <- groups$age_group[which.max(groups$dbp)]
    if (all(is.na(groups$dbp))) {
      return(tags$p(paste0("The highest observed group mean SBP is in ages ", hi_sbp,
                           ". DBP is unavailable for these filters.")))
    }
    tags$p(paste0("Highest mean systolic BP is in ages ", hi_sbp,
                  " (", fmt_num(max(groups$sbp)), " mmHg). Highest mean diastolic BP is in ages ",
                  hi_dbp, " (", fmt_num(max(groups$dbp, na.rm = TRUE)),
                  " mmHg). These compare groups, not changes within individuals over time."))
  })
  
  # Age and systolic blood pressure.
  output$age_sbp_scatter <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BPSysAve))
    validate(need(nrow(df) > 0, "No systolic BP values under these filters."))
    p <- ggplot(df, aes(Age, BPSysAve)) +
      geom_point(alpha = .35, size = 1.5, colour = cyan) +
      labs(x = "Age (years)", y = "Systolic BP (mmHg)") + theme_nhanes_dark()
    if (nrow(df) >= 3 && n_distinct(df$Age) >= 2) {
      p <- p + geom_smooth(method = "lm", se = FALSE, colour = gold)
    }
    p
  }, bg = navy)
  output$age_sbp_insight <- renderUI({
    df <- filtered_data()
    tags$p(cor_words(safe_cor(df$Age, df$BPSysAve)))
  })
  
  # Missingness applies to CURRENT filtered rows, not all original records.
  missing_summary <- reactive({
    df <- filtered_data()
    fields <- c("Weight", "Height", "BMI", "BPSysAve", "BPDiaAve", "Pulse")
    data.frame(
      Measurement = fields,
      Missing = vapply(df[fields], function(x) sum(is.na(x)), integer(1)),
      `Missing (%)` = if (!nrow(df)) rep(NA_real_, length(fields)) else
        round(vapply(df[fields], function(x) mean(is.na(x)), numeric(1)) * 100, 2),
      check.names = FALSE
    )
  })
  output$missing_table <- renderTable(missing_summary(), striped = TRUE)
  output$missing_insight <- renderUI({
    df <- filtered_data()
    if (!nrow(df)) return(tags$p("No records match these filters."))
    fields <- c("Weight", "Height", "BMI", "BPSysAve", "BPDiaAve", "Pulse")
    incomplete <- sum(!complete.cases(df[fields]))
    tags$p(paste0(format(incomplete, big.mark = ","), " of ",
                  format(nrow(df), big.mark = ","), " selected records (",
                  fmt_num(100 * incomplete / nrow(df), 1),
                  "%) have at least one missing measurement among the six displayed clinical variables. ",
                  "This is record incompleteness, not overall cell missingness."))
  })
  output$audit_summary <- renderUI({
    if (!unique_id_check) {
      return(tags$p("WARNING: More than one row per ID occurs in the unique-ID dataset."))
    }
    if (!audit_available) {
      return(tags$p("The unique-ID dataset contains one row per ID. ",
                    "To check agreement between repeated source rows, save adult_clinical_data.rds in data/."))
    }
    tags$p(paste0("Unique-ID dataset: ", format(nrow(adult_data), big.mark = ","),
                  " rows, with no repeated IDs. The original adult sample contains ",
                  format(duplicate_occurrences, big.mark = ","), " additional repeated-ID occurrences. ",
                  "IDs with inconsistent values across selected measurements: ",
                  format(conflict_count, big.mark = ","), ". ",
                  if (conflict_count == 0) "Repeated IDs agree on the checked fields." else
                    "Review inconsistent IDs before treating the first row as authoritative."))
  })
  output$sample_table <- renderTable({
    filtered_data() |>
      select(all_of(required_columns)) |>
      head(10)
  }, digits = 2, striped = TRUE)
  
  output$download_data <- downloadHandler(
    filename = function() paste0("nhanes_filtered_adults_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
  output$download_summary <- downloadHandler(
    filename = function() paste0("nhanes_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- filtered_data()
      summary_table <- data.frame(
        Indicator = c("Participants", "Mean BMI", "Mean SBP", "Mean DBP", "Mean pulse"),
        Value = c(nrow(df), safe_avg(df$BMI), safe_avg(df$BPSysAve),
                  safe_avg(df$BPDiaAve), safe_avg(df$Pulse))
      )
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
