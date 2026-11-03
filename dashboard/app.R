# NHANES Clinical Health Dashboard
# Educational portfolio application -- Shiny 1.8.0 / bslib 0.6.1
# Save this file as: nhanes_clinical_analytics/dashboard/app.R

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# The app runs from the dashboard/ directory.
# Therefore, ../data/ means the data folder one level above it.
data_file <- "../data/adult_unique_data.rds"
if (!file.exists(data_file)) {
  stop("Dataset not found at ", data_file,
       ". Check that adult_unique_data.rds exists in the project's data/ folder.")
}
adult_data <- readRDS(data_file)

# Keep the BMI categories consistent with our exploratory analysis.
adult_data <- adult_data |>
  mutate(
    bmi_status = case_when(
      is.na(BMI) ~ NA_character_,
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Normal",
      BMI < 30 ~ "Overweight",
      TRUE ~ "Obese"
    ),
    age_group = factor(
      case_when(
        Age < 40 ~ "Young adults (18-39)",
        Age < 65 ~ "Middle-aged adults (40-64)",
        TRUE ~ "Older adults (65+)"
      ),
      levels = c("Young adults (18-39)",
                 "Middle-aged adults (40-64)",
                 "Older adults (65+)")
    )
  )

# Reusable plotting style.
theme_nhanes_dark <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#0F172A", colour = NA),
      panel.background = element_rect(fill = "#0F172A", colour = NA),
      panel.grid.major = element_line(colour = "#334155"),
      panel.grid.minor = element_blank(),
      text = element_text(colour = "#E2E8F0"),
      axis.text = element_text(colour = "#CBD5E1"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "#94A3B8"),
      legend.background = element_rect(fill = "#0F172A", colour = NA),
      legend.key = element_rect(fill = "#0F172A", colour = NA)
    )
}

safe_mean <- function(x) {
  if (all(is.na(x)) || length(x) == 0) return("N/A")
  sprintf("%.2f", mean(x, na.rm = TRUE))
}

# UI: what people see in the browser.
ui <- page_fluid(
  theme = bs_theme(version = 5, bg = "#0F172A", fg = "#E2E8F0",
                   primary = "#38BDF8"),
  tags$head(tags$style(HTML("
    body { background: #0F172A; }
    .app-title { font-size: 1.9rem; font-weight: 750; margin-top: 22px; }
    .app-subtitle { color: #94A3B8; margin-bottom: 18px; }
    .filter-panel { background: #1E293B; border: 1px solid #334155;
                    border-radius: 12px; padding: 18px; margin-bottom: 20px; }
    .kpi { background: #1E293B; border: 1px solid #334155;
           border-radius: 12px; padding: 18px; min-height: 122px;
           margin-bottom: 16px; }
    .kpi-label { color: #94A3B8; font-size: .9rem; }
    .kpi-value { color: #F8FAFC; font-size: 1.8rem; font-weight: 750; }
    .panel-box { background: #111D30; border: 1px solid #334155;
                 border-radius: 12px; padding: 14px; margin-bottom: 18px; }
    .note { color: #94A3B8; font-size: .9rem; margin-top: 10px; }
    .nav-tabs .nav-link { color: #CBD5E1; }
    .nav-tabs .nav-link.active { background: #1E293B; color: #F8FAFC;
                                border-color: #334155; }
    table { color: #E2E8F0 !important; }
  "))),
  
  div(class = "app-title", "NHANES Clinical Health Dashboard"),
  div(class = "app-subtitle",
      "Interactive exploration of adult health indicators | Educational dataset"),
  
  div(class = "filter-panel",
      fluidRow(
        column(4,
               selectInput("sex_filter", "Sex",
                           choices = c("All", "female", "male"))),
        column(4,
               sliderInput("age_filter", "Age range (years)",
                           min = 18, max = 80, value = c(18, 80), step = 1,
                           sep = "")),
        column(4,
               selectInput("bmi_filter", "BMI category",
                           choices = c("All", "Underweight", "Normal",
                                       "Overweight", "Obese", "Missing")))
      )
  ),
  
  fluidRow(
    column(4, div(class = "kpi", div(class = "kpi-label", "Adult participants"),
                  div(class = "kpi-value", textOutput("total_adults")))),
    column(4, div(class = "kpi", div(class = "kpi-label", "Mean BMI (kg/m²)"),
                  div(class = "kpi-value", textOutput("mean_bmi")))),
    column(4, div(class = "kpi", div(class = "kpi-label", "Mean systolic BP (mmHg)"),
                  div(class = "kpi-value", textOutput("mean_sbp")))),
    column(4, div(class = "kpi", div(class = "kpi-label", "Mean diastolic BP (mmHg)"),
                  div(class = "kpi-value", textOutput("mean_dbp")))),
    column(4, div(class = "kpi", div(class = "kpi-label", "Mean pulse (beats/min)"),
                  div(class = "kpi-value", textOutput("mean_pulse"))))
  ),
  
  tabsetPanel(
    id = "dashboard_tab",
    tabPanel("Overview",
             div(class = "panel-box", h4("BMI distribution"),
                 plotOutput("bmi_hist", height = "390px")),
             div(class = "panel-box", h4("BMI categories"),
                 plotOutput("bmi_categories", height = "350px"))),
    
    tabPanel("BMI analysis",
             div(class = "panel-box", h4("BMI distribution by sex"),
                 plotOutput("bmi_boxplot", height = "400px")),
             div(class = "panel-box", h4("Age and BMI"),
                 plotOutput("age_bmi_scatter", height = "400px"))),
    
    tabPanel("Blood pressure",
             div(class = "panel-box", h4("Blood pressure across age groups"),
                 plotOutput("bp_age_group", height = "400px")),
             div(class = "panel-box", h4("Age and systolic blood pressure"),
                 plotOutput("age_sbp_scatter", height = "400px"))),
    
    tabPanel("Data quality",
             div(class = "panel-box", h4("Missing measurements in selected records"),
                 tableOutput("missing_table")),
             div(class = "panel-box", h4("Selected record summary"),
                 tableOutput("sample_table"),
                 downloadButton("download_data", "Export selected records (CSV)")),
             p(class = "note",
               "Missingness is calculated for the current filters. Missing BP values may reflect survey eligibility or examination procedures. Do not automatically interpret them as errors."))
  ),
  
  p(class = "note",
    "Source: NHANES R educational dataset, adult unique-ID subset. This app is for learning and demonstration, not nationally representative prevalence estimation or clinical decision-making. Repeated-ID consistency and survey methodology require documentation before formal publication.")
)

# SERVER: calculations that respond to user selections.
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- adult_data |>
      filter(Age >= input$age_filter[1], Age <= input$age_filter[2])
    
    if (input$sex_filter != "All") {
      df <- df |> filter(Sex == input$sex_filter)
    }
    
    if (input$bmi_filter == "Missing") {
      df <- df |> filter(is.na(bmi_status))
    } else if (input$bmi_filter != "All") {
      df <- df |> filter(bmi_status == input$bmi_filter)
    }
    df
  })
  
  output$total_adults <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  output$mean_bmi <- renderText({ safe_mean(filtered_data()$BMI) })
  output$mean_sbp <- renderText({ safe_mean(filtered_data()$BPSysAve) })
  output$mean_dbp <- renderText({ safe_mean(filtered_data()$BPDiaAve) })
  output$mean_pulse <- renderText({ safe_mean(filtered_data()$Pulse) })
  
  output$bmi_hist <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BMI))
    validate(need(nrow(df) > 0, "No recorded BMI values for these filters."))
    ggplot(df, aes(x = BMI)) +
      geom_histogram(binwidth = 2, fill = "#38BDF8", colour = "#0F172A") +
      labs(x = "BMI (kg/m²)", y = "Number of participants") +
      theme_nhanes_dark()
  }, bg = "#0F172A")
  
  output$bmi_categories <- renderPlot({
    df <- filtered_data() |>
      mutate(category = if_else(is.na(bmi_status), "Missing", bmi_status)) |>
      count(category)
    validate(need(nrow(df) > 0, "No participants match the selected filters."))
    df$category <- factor(df$category,
                          levels = c("Underweight", "Normal", "Overweight", "Obese", "Missing"))
    ggplot(df, aes(x = category, y = n)) +
      geom_col(fill = "#38BDF8") +
      labs(x = "BMI category", y = "Number of participants") +
      theme_nhanes_dark()
  }, bg = "#0F172A")
  
  output$bmi_boxplot <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BMI))
    validate(need(nrow(df) > 0, "No recorded BMI values for these filters."))
    ggplot(df, aes(x = Sex, y = BMI, fill = Sex)) +
      geom_boxplot(colour = "#E2E8F0", linewidth = 0.6,
                   outlier.colour = "#FACC15", outlier.size = 1.8,
                   outlier.alpha = 0.85) +
      scale_fill_manual(values = c(female = "#F87171", male = "#06B6D4")) +
      labs(x = "Sex", y = "BMI (kg/m²)") +
      theme_nhanes_dark() + theme(legend.position = "none")
  }, bg = "#0F172A")
  
  output$age_bmi_scatter <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BMI))
    validate(need(nrow(df) > 0, "No recorded BMI values for these filters."))
    p <- ggplot(df, aes(x = Age, y = BMI)) +
      geom_point(colour = "#38BDF8", alpha = 0.3, size = 1.5) +
      labs(x = "Age (years)", y = "BMI (kg/m²)") + theme_nhanes_dark()
    if (nrow(df) >= 3 && n_distinct(df$Age) >= 2) {
      p <- p + geom_smooth(method = "lm", se = FALSE, colour = "#FACC15")
    }
    p
  }, bg = "#0F172A")
  
  output$bp_age_group <- renderPlot({
    df <- filtered_data() |>
      group_by(age_group) |>
      summarise(sbp = if (all(is.na(BPSysAve))) NA_real_ else mean(BPSysAve, na.rm = TRUE),
                dbp = if (all(is.na(BPDiaAve))) NA_real_ else mean(BPDiaAve, na.rm = TRUE),
                .groups = "drop")
    bp_long <- bind_rows(
      transmute(df, age_group, bp_type = "Systolic BP", mean_bp = sbp),
      transmute(df, age_group, bp_type = "Diastolic BP", mean_bp = dbp)
    ) |> filter(!is.na(mean_bp))
    validate(need(nrow(bp_long) > 0, "No BP measurements for these filters."))
    ggplot(bp_long, aes(x = age_group, y = mean_bp, colour = bp_type,
                        group = bp_type)) +
      geom_line(linewidth = 1.1, na.rm = TRUE) +
      geom_point(size = 3) +
      geom_text(aes(label = sprintf("%.1f", mean_bp)), vjust = -0.8,
                size = 3.5, show.legend = FALSE) +
      scale_colour_manual(values = c("Systolic BP" = "#38BDF8",
                                     "Diastolic BP" = "#FACC15")) +
      scale_y_continuous(expand = expansion(mult = c(0.12, 0.18))) +
      labs(x = "Age group", y = "Mean BP (mmHg)", colour = "Measurement") +
      theme_nhanes_dark() +
      theme(axis.text.x = element_text(size = 9))
  }, bg = "#0F172A")
  
  output$age_sbp_scatter <- renderPlot({
    df <- filtered_data() |> filter(!is.na(BPSysAve))
    validate(need(nrow(df) > 0, "No recorded SBP measurements for these filters."))
    p <- ggplot(df, aes(x = Age, y = BPSysAve)) +
      geom_point(colour = "#38BDF8", alpha = 0.35, size = 1.4) +
      labs(x = "Age (years)", y = "Systolic BP (mmHg)") + theme_nhanes_dark()
    if (nrow(df) >= 3 && n_distinct(df$Age) >= 2) {
      p <- p + geom_smooth(method = "lm", se = FALSE, colour = "#FACC15")
    }
    p
  }, bg = "#0F172A")
  
  output$missing_table <- renderTable({
    df <- filtered_data()
    cols <- c("Weight", "Height", "BMI", "BPSysAve", "BPDiaAve", "Pulse")
    data.frame(
      Measurement = cols,
      Missing = vapply(df[cols], function(x) sum(is.na(x)), integer(1)),
      `Missing (%)` = if (nrow(df) == 0) rep(NA_real_, length(cols)) else
        round(vapply(df[cols], function(x) mean(is.na(x)), numeric(1)) * 100, 2),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE)
  
  output$sample_table <- renderTable({
    filtered_data() |>
      select(ID, Age, Sex, Weight, Height, BMI, BPSysAve, BPDiaAve, Pulse) |>
      head(12)
  }, digits = 2, striped = TRUE, bordered = FALSE)
  
  output$download_data <- downloadHandler(
    filename = function() paste0("nhanes_selected_adults_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
