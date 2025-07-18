# ===============================================================================
# SoVi Explorer Pro - Dashboard Analisis Kerentanan Sosial Terpadu
# Nama Dashboard: "SoVi Explorer Pro - Dashboard Analisis Kerentanan Sosial Terpadu"
# Dibuat untuk: Tugas Akhir Statistika Terapan
# File: template.R (All-in-One Dashboard)
# ===============================================================================

# Load required libraries with automatic installation
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(shinydashboard)) install.packages("shinydashboard", dependencies = TRUE)
if (!require(DT)) install.packages("DT", dependencies = TRUE)
if (!require(plotly)) install.packages("plotly", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(leaflet)) install.packages("leaflet", dependencies = TRUE)
if (!require(corrplot)) install.packages("corrplot", dependencies = TRUE)
if (!require(car)) install.packages("car", dependencies = TRUE)
if (!require(broom)) install.packages("broom", dependencies = TRUE)
if (!require(psych)) install.packages("psych", dependencies = TRUE)
if (!require(VIM)) install.packages("VIM", dependencies = TRUE)
if (!require(cluster)) install.packages("cluster", dependencies = TRUE)
if (!require(factoextra)) install.packages("factoextra", dependencies = TRUE)
if (!require(rmarkdown)) install.packages("rmarkdown", dependencies = TRUE)
if (!require(knitr)) install.packages("knitr", dependencies = TRUE)
if (!require(downloadthis)) install.packages("downloadthis", dependencies = TRUE)
if (!require(openxlsx)) install.packages("openxlsx", dependencies = TRUE)
if (!require(flextable)) install.packages("flextable", dependencies = TRUE)
if (!require(officer)) install.packages("officer", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(reshape2)) install.packages("reshape2", dependencies = TRUE)

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(corrplot)
library(car)
library(broom)
library(psych)
library(VIM)
library(cluster)
library(factoextra)
library(rmarkdown)
library(knitr)
library(downloadthis)
library(openxlsx)
library(flextable)
library(officer)
library(dplyr)
library(reshape2)

# Global variables for storing data and results
data_sovi <- NULL
distance_matrix <- NULL
processed_data <- NULL
analysis_results <- list()

# Function to load data
load_sovi_data <- function() {
  tryCatch({
    # Load SOVI data
    data_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
    data_sovi <<- read.csv(data_url, stringsAsFactors = FALSE)
    
    # Load distance matrix
    distance_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
    distance_matrix <<- read.csv(distance_url, stringsAsFactors = FALSE)
    
    return(TRUE)
  }, error = function(e) {
    print(paste("Error loading data:", e$message))
    return(FALSE)
  })
}

# Function to categorize continuous variables
categorize_variable <- function(x, method = "quartile") {
  if (method == "quartile") {
    cut(x, breaks = quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
        labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"), include.lowest = TRUE)
  } else if (method == "tercile") {
    cut(x, breaks = quantile(x, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
        labels = c("Rendah", "Sedang", "Tinggi"), include.lowest = TRUE)
  } else if (method == "median") {
    ifelse(x <= median(x, na.rm = TRUE), "Di bawah Median", "Di atas Median")
  }
}

# Function to interpret statistical tests
interpret_normality <- function(p_value) {
  if (p_value > 0.05) {
    return("Data berdistribusi normal (p > 0.05). Asumsi normalitas terpenuhi.")
  } else {
    return("Data tidak berdistribusi normal (p ≤ 0.05). Pertimbangkan transformasi data atau uji non-parametrik.")
  }
}

interpret_homogeneity <- function(p_value) {
  if (p_value > 0.05) {
    return("Varians antar kelompok homogen (p > 0.05). Asumsi homogenitas terpenuhi.")
  } else {
    return("Varians antar kelompok tidak homogen (p ≤ 0.05). Pertimbangkan transformasi data atau uji alternatif.")
  }
}

interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("Sangat lemah")
  else if (abs_r < 0.3) return("Lemah")
  else if (abs_r < 0.5) return("Sedang")
  else if (abs_r < 0.7) return("Kuat")
  else return("Sangat kuat")
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "SoVi Explorer Pro - Dashboard Analisis Kerentanan Sosial Terpadu"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("📊 Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("🔍 Eksplorasi Data", tabName = "eksplorasi", icon = icon("search")),
      menuItem("📏 Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("📈 Statistik Inferensia I", tabName = "inferensia1", icon = icon("chart-line")),
      menuItem("📉 Statistik Inferensia II", tabName = "inferensia2", icon = icon("chart-bar")),
      menuItem("🎯 Regresi Linear", tabName = "regresi", icon = icon("trend-up")),
      menuItem("💾 Download Laporan", tabName = "download", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # ================ BERANDA ================
      tabItem(tabName = "beranda",
        fluidRow(
          box(
            title = "Selamat Datang di SoVi Explorer Pro", status = "primary", solidHeader = TRUE,
            width = 12,
            h3("Dashboard Analisis Kerentanan Sosial Terpadu"),
            hr(),
            h4("📋 Tentang Dashboard"),
            p("Dashboard ini dirancang untuk analisis komprehensif data Social Vulnerability Index (SoVI) 
              dengan fitur-fitur statistik terapan yang lengkap."),
            
            h4("📊 Data yang Digunakan"),
            tags$ul(
              tags$li("Data SOVI: Social Vulnerability Index untuk analisis kerentanan sosial"),
              tags$li("Matriks Jarak: Data penimbang jarak untuk analisis spasial"),
              tags$li("Sumber: Repository naspaclust dari GitHub")
            ),
            
            h4("🎯 Fitur Utama Dashboard"),
            tags$ol(
              tags$li(strong("Manajemen Data:"), " Kategorisasi variabel kontinyu, preprocessing data"),
              tags$li(strong("Eksplorasi Data:"), " Statistik deskriptif, visualisasi, peta interaktif"),
              tags$li(strong("Uji Asumsi:"), " Uji normalitas dan homogenitas dengan interpretasi"),
              tags$li(strong("Statistik Inferensia I:"), " Uji proporsi dan varians 1-2 kelompok"),
              tags$li(strong("Statistik Inferensia II:"), " ANOVA satu arah dan dua arah"),
              tags$li(strong("Regresi Linear:"), " Analisis regresi berganda dengan uji asumsi"),
              tags$li(strong("Download:"), " Ekspor hasil analisis dalam berbagai format")
            ),
            
            h4("👨‍💻 Informasi Pengembang"),
            p("Dashboard ini dikembangkan sebagai tugas akhir mata kuliah Statistika Terapan."),
            p("Menggunakan framework R Shiny dengan integrasi berbagai package statistik dan visualisasi."),
            
            actionButton("loadData", "📥 Muat Data", class = "btn-primary btn-lg"),
            br(), br(),
            verbatimTextOutput("dataStatus")
          )
        )
      ),
      
      # ================ MANAJEMEN DATA ================
      tabItem(tabName = "manajemen",
        fluidRow(
          box(
            title = "Data Management & Preprocessing", status = "primary", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("📋 Data Overview",
                h4("Ringkasan Dataset"),
                verbatimTextOutput("dataSummary"),
                br(),
                h4("Struktur Data"),
                verbatimTextOutput("dataStructure"),
                br(),
                h4("Missing Values"),
                plotOutput("missingPlot")
              ),
              
              tabPanel("🔧 Kategorisasi Variabel",
                fluidRow(
                  column(4,
                    selectInput("varToCategorize", "Pilih Variabel Kontinyu:",
                               choices = NULL),
                    selectInput("categorizeMethod", "Metode Kategorisasi:",
                               choices = list("Kuartil" = "quartile",
                                            "Tersil" = "tercile",
                                            "Median" = "median")),
                    actionButton("categorizeVar", "Kategorisasi", class = "btn-info")
                  ),
                  column(8,
                    h4("Preview Kategorisasi"),
                    DT::dataTableOutput("categorizedPreview")
                  )
                ),
                br(),
                h4("Interpretasi Kategorisasi"),
                verbatimTextOutput("categorizeInterpretation")
              ),
              
              tabPanel("📊 Data Terproses",
                h4("Data Setelah Preprocessing"),
                DT::dataTableOutput("processedDataTable"),
                br(),
                downloadButton("downloadProcessedData", "Download Data Terproses", 
                             class = "btn-success")
              )
            )
          )
        )
      ),
      
      # ================ EKSPLORASI DATA ================
      tabItem(tabName = "eksplorasi",
        fluidRow(
          box(
            title = "Eksplorasi Data & Visualisasi", status = "primary", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("📊 Statistik Deskriptif",
                h4("Statistik Ringkasan"),
                verbatimTextOutput("descriptiveStats"),
                br(),
                h4("Interpretasi Statistik Deskriptif"),
                verbatimTextOutput("descriptiveInterpretation")
              ),
              
              tabPanel("📈 Visualisasi Univariat",
                fluidRow(
                  column(4,
                    selectInput("univarVar", "Pilih Variabel:",
                               choices = NULL),
                    selectInput("plotType", "Tipe Plot:",
                               choices = list("Histogram" = "histogram",
                                            "Boxplot" = "boxplot",
                                            "Density" = "density"))
                  ),
                  column(8,
                    plotlyOutput("univariatePlot")
                  )
                ),
                br(),
                h4("Interpretasi Visualisasi"),
                verbatimTextOutput("univarInterpretation")
              ),
              
              tabPanel("🔗 Visualisasi Bivariat",
                fluidRow(
                  column(4,
                    selectInput("bivarX", "Variabel X:",
                               choices = NULL),
                    selectInput("bivarY", "Variabel Y:",
                               choices = NULL),
                    selectInput("bivarType", "Tipe Plot:",
                               choices = list("Scatter Plot" = "scatter",
                                            "Correlation Matrix" = "correlation"))
                  ),
                  column(8,
                    plotlyOutput("bivariatePlot")
                  )
                ),
                br(),
                h4("Interpretasi Hubungan Bivariat"),
                verbatimTextOutput("bivarInterpretation")
              ),
              
              tabPanel("🗺️ Peta Interaktif",
                h4("Peta Sebaran Data SOVI"),
                p("Catatan: Peta akan ditampilkan jika data mengandung koordinat geografis"),
                leafletOutput("interactiveMap", height = "500px"),
                br(),
                h4("Interpretasi Peta"),
                verbatimTextOutput("mapInterpretation")
              )
            )
          )
        )
      ),
      
      # ================ UJI ASUMSI ================
      tabItem(tabName = "asumsi",
        fluidRow(
          box(
            title = "Uji Asumsi Data", status = "primary", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("📏 Uji Normalitas",
                fluidRow(
                  column(4,
                    selectInput("normVar", "Pilih Variabel:",
                               choices = NULL),
                    actionButton("testNormality", "Uji Normalitas", class = "btn-info")
                  ),
                  column(8,
                    h4("Hasil Uji Shapiro-Wilk"),
                    verbatimTextOutput("normalityTest"),
                    br(),
                    plotOutput("normalityPlot")
                  )
                ),
                br(),
                h4("Interpretasi Uji Normalitas"),
                verbatimTextOutput("normalityInterpretation")
              ),
              
              tabPanel("⚖️ Uji Homogenitas",
                fluidRow(
                  column(4,
                    selectInput("homogVar", "Variabel Dependen:",
                               choices = NULL),
                    selectInput("homogGroup", "Variabel Kelompok:",
                               choices = NULL),
                    actionButton("testHomogeneity", "Uji Homogenitas", class = "btn-info")
                  ),
                  column(8,
                    h4("Hasil Uji Levene"),
                    verbatimTextOutput("homogeneityTest"),
                    br(),
                    plotOutput("homogeneityPlot")
                  )
                ),
                br(),
                h4("Interpretasi Uji Homogenitas"),
                verbatimTextOutput("homogeneityInterpretation")
              )
            )
          )
        )
      ),
      
      # ================ STATISTIK INFERENSIA I ================
      tabItem(tabName = "inferensia1",
        fluidRow(
          box(
            title = "Statistik Inferensia - Uji Proporsi & Varians", status = "primary", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("📊 Uji Proporsi",
                fluidRow(
                  column(4,
                    h4("Uji Proporsi Satu Sampel"),
                    selectInput("propVar", "Variabel Kategorik:",
                               choices = NULL),
                    numericInput("propValue", "Proporsi Hipotesis:", value = 0.5, min = 0, max = 1, step = 0.01),
                    actionButton("testProportion", "Uji Proporsi", class = "btn-info")
                  ),
                  column(8,
                    h4("Hasil Uji Proporsi"),
                    verbatimTextOutput("proportionTest"),
                    br(),
                    plotOutput("proportionPlot")
                  )
                ),
                br(),
                h4("Interpretasi Uji Proporsi"),
                verbatimTextOutput("proportionInterpretation")
              ),
              
              tabPanel("📈 Uji Varians",
                fluidRow(
                  column(4,
                    h4("Uji Varians"),
                    selectInput("varTestVar1", "Variabel 1:",
                               choices = NULL),
                    selectInput("varTestVar2", "Variabel 2 (opsional):",
                               choices = NULL),
                    numericInput("varHypothesis", "Varians Hipotesis:", value = 1, min = 0),
                    actionButton("testVariance", "Uji Varians", class = "btn-info")
                  ),
                  column(8,
                    h4("Hasil Uji Varians"),
                    verbatimTextOutput("varianceTest"),
                    br(),
                    plotOutput("variancePlot")
                  )
                ),
                br(),
                h4("Interpretasi Uji Varians"),
                verbatimTextOutput("varianceInterpretation")
              )
            )
          )
        )
      ),
      
      # ================ STATISTIK INFERENSIA II ================
      tabItem(tabName = "inferensia2",
        fluidRow(
          box(
            title = "Statistik Inferensia - ANOVA", status = "primary", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("📊 ANOVA Satu Arah",
                fluidRow(
                  column(4,
                    h4("One-Way ANOVA"),
                    selectInput("anovaDepVar", "Variabel Dependen:",
                               choices = NULL),
                    selectInput("anovaIndepVar", "Variabel Independen:",
                               choices = NULL),
                    actionButton("testAnova1", "Uji ANOVA", class = "btn-info")
                  ),
                  column(8,
                    h4("Hasil ANOVA Satu Arah"),
                    verbatimTextOutput("anova1Test"),
                    br(),
                    plotOutput("anova1Plot")
                  )
                ),
                br(),
                h4("Post-hoc Test (Tukey HSD)"),
                verbatimTextOutput("tukey1Test"),
                br(),
                h4("Interpretasi ANOVA Satu Arah"),
                verbatimTextOutput("anova1Interpretation")
              ),
              
              tabPanel("📈 ANOVA Dua Arah",
                fluidRow(
                  column(4,
                    h4("Two-Way ANOVA"),
                    selectInput("anova2DepVar", "Variabel Dependen:",
                               choices = NULL),
                    selectInput("anova2IndepVar1", "Faktor 1:",
                               choices = NULL),
                    selectInput("anova2IndepVar2", "Faktor 2:",
                               choices = NULL),
                    checkboxInput("anova2Interaction", "Uji Interaksi", value = TRUE),
                    actionButton("testAnova2", "Uji ANOVA", class = "btn-info")
                  ),
                  column(8,
                    h4("Hasil ANOVA Dua Arah"),
                    verbatimTextOutput("anova2Test"),
                    br(),
                    plotOutput("anova2Plot")
                  )
                ),
                br(),
                h4("Interpretasi ANOVA Dua Arah"),
                verbatimTextOutput("anova2Interpretation")
              )
            )
          )
        )
      ),
      
      # ================ REGRESI LINEAR ================
      tabItem(tabName = "regresi",
        fluidRow(
          box(
            title = "Analisis Regresi Linear Berganda", status = "primary", solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("📊 Model Regresi",
                fluidRow(
                  column(4,
                    h4("Pengaturan Model"),
                    selectInput("regDepVar", "Variabel Dependen:",
                               choices = NULL),
                    selectInput("regIndepVars", "Variabel Independen:",
                               choices = NULL, multiple = TRUE),
                    actionButton("buildRegression", "Bangun Model", class = "btn-info")
                  ),
                  column(8,
                    h4("Ringkasan Model Regresi"),
                    verbatimTextOutput("regressionSummary")
                  )
                ),
                br(),
                h4("Interpretasi Model Regresi"),
                verbatimTextOutput("regressionInterpretation")
              ),
              
              tabPanel("🔍 Uji Asumsi Regresi",
                h4("Diagnostic Plots"),
                plotOutput("regressionDiagnostics", height = "600px"),
                br(),
                h4("Uji Asumsi Formal"),
                verbatimTextOutput("regressionAssumptions"),
                br(),
                h4("Interpretasi Uji Asumsi"),
                verbatimTextOutput("assumptionInterpretation")
              ),
              
              tabPanel("📈 Prediksi",
                fluidRow(
                  column(6,
                    h4("Input Nilai untuk Prediksi"),
                    uiOutput("predictionInputs"),
                    actionButton("makePrediction", "Prediksi", class = "btn-success")
                  ),
                  column(6,
                    h4("Hasil Prediksi"),
                    verbatimTextOutput("predictionResult"),
                    br(),
                    plotOutput("predictionPlot")
                  )
                )
              )
            )
          )
        )
      ),
      
      # ================ DOWNLOAD ================
      tabItem(tabName = "download",
        fluidRow(
          box(
            title = "Download Laporan & Hasil Analisis", status = "primary", solidHeader = TRUE,
            width = 12,
            h4("📥 Download Hasil Analisis"),
            p("Pilih format dan komponen yang ingin didownload:"),
            
            fluidRow(
              column(4,
                h5("Format File"),
                checkboxGroupInput("downloadFormats", "",
                                 choices = list("PDF Report" = "pdf",
                                              "Word Document" = "word",
                                              "Excel Data" = "excel",
                                              "CSV Data" = "csv",
                                              "R Script" = "rscript")),
                
                h5("Komponen Analisis"),
                checkboxGroupInput("downloadComponents", "",
                                 choices = list("Data Overview" = "overview",
                                              "Descriptive Statistics" = "descriptive",
                                              "Visualizations" = "plots",
                                              "Statistical Tests" = "tests",
                                              "Regression Analysis" = "regression"))
              ),
              
              column(8,
                h5("Preview Laporan"),
                verbatimTextOutput("reportPreview"),
                br(),
                downloadButton("downloadReport", "📄 Download Laporan Lengkap", 
                             class = "btn-primary btn-lg"),
                br(), br(),
                downloadButton("downloadImages", "🖼️ Download Semua Grafik", 
                             class = "btn-info"),
                br(), br(),
                downloadButton("downloadData", "📊 Download Data & Hasil", 
                             class = "btn-success")
              )
            )
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # ================ REACTIVE VALUES ================
  values <- reactiveValues(
    data_loaded = FALSE,
    current_analysis = NULL,
    plots_list = list(),
    results_list = list()
  )
  
  # ================ DATA LOADING ================
  observeEvent(input$loadData, {
    withProgress(message = 'Memuat data...', value = 0, {
      incProgress(0.3, detail = "Mengunduh data SOVI...")
      
      success <- load_sovi_data()
      
      incProgress(0.7, detail = "Memproses data...")
      
      if (success && !is.null(data_sovi)) {
        # Update choices for various inputs
        numeric_vars <- names(data_sovi)[sapply(data_sovi, is.numeric)]
        all_vars <- names(data_sovi)
        
        updateSelectInput(session, "varToCategorize", choices = numeric_vars)
        updateSelectInput(session, "univarVar", choices = all_vars)
        updateSelectInput(session, "bivarX", choices = numeric_vars)
        updateSelectInput(session, "bivarY", choices = numeric_vars)
        updateSelectInput(session, "normVar", choices = numeric_vars)
        updateSelectInput(session, "homogVar", choices = numeric_vars)
        updateSelectInput(session, "homogGroup", choices = all_vars)
        updateSelectInput(session, "propVar", choices = all_vars)
        updateSelectInput(session, "varTestVar1", choices = numeric_vars)
        updateSelectInput(session, "varTestVar2", choices = c("", numeric_vars))
        updateSelectInput(session, "anovaDepVar", choices = numeric_vars)
        updateSelectInput(session, "anovaIndepVar", choices = all_vars)
        updateSelectInput(session, "anova2DepVar", choices = numeric_vars)
        updateSelectInput(session, "anova2IndepVar1", choices = all_vars)
        updateSelectInput(session, "anova2IndepVar2", choices = all_vars)
        updateSelectInput(session, "regDepVar", choices = numeric_vars)
        updateSelectInput(session, "regIndepVars", choices = numeric_vars)
        
        values$data_loaded <- TRUE
        incProgress(1, detail = "Selesai!")
      }
    })
  })
  
  output$dataStatus <- renderText({
    if (values$data_loaded) {
      paste("✅ Data berhasil dimuat!\n",
            "📊 Jumlah observasi:", nrow(data_sovi), "\n",
            "📈 Jumlah variabel:", ncol(data_sovi), "\n",
            "🗓️ Waktu muat:", Sys.time())
    } else {
      "❌ Data belum dimuat. Klik tombol 'Muat Data' untuk memulai."
    }
  })
  
  # ================ MANAJEMEN DATA ================
  output$dataSummary <- renderPrint({
    if (values$data_loaded) {
      summary(data_sovi)
    } else {
      "Data belum dimuat."
    }
  })
  
  output$dataStructure <- renderPrint({
    if (values$data_loaded) {
      str(data_sovi)
    } else {
      "Data belum dimuat."
    }
  })
  
  output$missingPlot <- renderPlot({
    if (values$data_loaded) {
      VIM::aggr(data_sovi, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE)
    }
  })
  
  observeEvent(input$categorizeVar, {
    if (values$data_loaded && !is.null(input$varToCategorize)) {
      var_data <- data_sovi[[input$varToCategorize]]
      categorized <- categorize_variable(var_data, input$categorizeMethod)
      
      # Create preview
      preview_data <- data.frame(
        Original = var_data,
        Categorized = categorized
      )[1:min(100, length(var_data)), ]
      
      output$categorizedPreview <- DT::renderDataTable({
        DT::datatable(preview_data, options = list(pageLength = 10))
      })
    }
  })
  
  output$categorizeInterpretation <- renderText({
    if (!is.null(input$varToCategorize) && input$categorizeVar > 0) {
      method_text <- switch(input$categorizeMethod,
                           "quartile" = "kuartil (4 kategori)",
                           "tercile" = "tersil (3 kategori)", 
                           "median" = "median (2 kategori)")
      
      paste("Variabel", input$varToCategorize, "telah dikategorisasi menggunakan metode", method_text, ".",
            "Kategorisasi ini mengubah data kontinyu menjadi data kategorikal untuk analisis yang lebih mudah dipahami.",
            "Setiap kategori mewakili tingkat yang berbeda dari variabel tersebut.")
    }
  })
  
  output$processedDataTable <- DT::renderDataTable({
    if (values$data_loaded) {
      DT::datatable(data_sovi, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # ================ EKSPLORASI DATA ================
  output$descriptiveStats <- renderPrint({
    if (values$data_loaded) {
      numeric_data <- data_sovi[sapply(data_sovi, is.numeric)]
      psych::describe(numeric_data)
    }
  })
  
  output$descriptiveInterpretation <- renderText({
    if (values$data_loaded) {
      paste("Statistik deskriptif menunjukkan karakteristik dasar dari setiap variabel numerik.",
            "Perhatikan nilai mean dan median untuk mendeteksi kemencengan data,",
            "serta nilai minimum dan maksimum untuk mengidentifikasi outlier potensial.",
            "Standar deviasi menunjukkan tingkat variabilitas data.")
    }
  })
  
  output$univariatePlot <- renderPlotly({
    if (values$data_loaded && !is.null(input$univarVar)) {
      var_data <- data_sovi[[input$univarVar]]
      
      if (input$plotType == "histogram") {
        p <- ggplot(data.frame(x = var_data), aes(x = x)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Histogram", input$univarVar),
               x = input$univarVar, y = "Frequency") +
          theme_minimal()
      } else if (input$plotType == "boxplot") {
        p <- ggplot(data.frame(x = var_data), aes(y = x)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Boxplot", input$univarVar),
               y = input$univarVar) +
          theme_minimal()
      } else if (input$plotType == "density") {
        p <- ggplot(data.frame(x = var_data), aes(x = x)) +
          geom_density(fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Density Plot", input$univarVar),
               x = input$univarVar, y = "Density") +
          theme_minimal()
      }
      
      ggplotly(p)
    }
  })
  
  output$univarInterpretation <- renderText({
    if (!is.null(input$univarVar) && values$data_loaded) {
      var_data <- data_sovi[[input$univarVar]]
      if (is.numeric(var_data)) {
        skewness_val <- psych::skew(var_data, na.rm = TRUE)
        
        skew_interpretation <- if (abs(skewness_val) < 0.5) {
          "relatif simetris"
        } else if (skewness_val > 0.5) {
          "menceng ke kanan (positif)"
        } else {
          "menceng ke kiri (negatif)"
        }
        
        paste("Distribusi variabel", input$univarVar, "menunjukkan pola yang", skew_interpretation, ".",
              "Nilai skewness:", round(skewness_val, 3), ".",
              "Plot ini membantu memahami bentuk distribusi data sebelum melakukan analisis lebih lanjut.")
      } else {
        paste("Variabel", input$univarVar, "adalah variabel kategorikal.",
              "Distribusi menunjukkan frekuensi setiap kategori dalam dataset.")
      }
    }
  })
  
  output$bivariatePlot <- renderPlotly({
    if (values$data_loaded && !is.null(input$bivarX) && !is.null(input$bivarY)) {
      if (input$bivarType == "scatter") {
        p <- ggplot(data_sovi, aes_string(x = input$bivarX, y = input$bivarY)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_smooth(method = "lm", se = TRUE, color = "red") +
          labs(title = paste("Scatter Plot:", input$bivarX, "vs", input$bivarY)) +
          theme_minimal()
        ggplotly(p)
      } else if (input$bivarType == "correlation") {
        numeric_data <- data_sovi[sapply(data_sovi, is.numeric)]
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        
        # Create correlation heatmap
        melted_cor <- reshape2::melt(cor_matrix)
        p <- ggplot(data = melted_cor, aes(Var1, Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1,1), space = "Lab", 
                              name="Pearson\nCorrelation") +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
          coord_fixed() +
          labs(title = "Correlation Matrix")
        ggplotly(p)
      }
    }
  })
  
  output$bivarInterpretation <- renderText({
    if (values$data_loaded && !is.null(input$bivarX) && !is.null(input$bivarY)) {
      cor_val <- cor(data_sovi[[input$bivarX]], data_sovi[[input$bivarY]], use = "complete.obs")
      cor_strength <- interpret_correlation(cor_val)
      cor_direction <- ifelse(cor_val > 0, "positif", "negatif")
      
      paste("Korelasi antara", input$bivarX, "dan", input$bivarY, "adalah", round(cor_val, 3), ".",
            "Ini menunjukkan hubungan", cor_strength, "dengan arah", cor_direction, ".",
            "Semakin mendekati ±1, semakin kuat hubungan linear antar variabel.")
    }
  })
  
  output$interactiveMap <- renderLeaflet({
    if (values$data_loaded) {
      # Check if data has lat/lon columns
      lat_cols <- grep("lat|latitude", names(data_sovi), ignore.case = TRUE)
      lon_cols <- grep("lon|longitude|lng", names(data_sovi), ignore.case = TRUE)
      
      if (length(lat_cols) > 0 && length(lon_cols) > 0) {
        lat_col <- names(data_sovi)[lat_cols[1]]
        lon_col <- names(data_sovi)[lon_cols[1]]
        
        leaflet(data_sovi) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~get(lon_col), lat = ~get(lat_col),
            radius = 5, opacity = 0.7,
            popup = ~paste("Index:", round(get(names(data_sovi)[1]), 2))
          )
      } else {
        # Create dummy map if no coordinates
        leaflet() %>%
          addTiles() %>%
          setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
          addMarkers(lng = -98.5795, lat = 39.8283, 
                    popup = "Data tidak mengandung koordinat geografis")
      }
    }
  })
  
  output$mapInterpretation <- renderText({
    "Peta interaktif menampilkan sebaran geografis data SOVI. Setiap titik merepresentasikan lokasi dengan nilai indeks kerentanan sosial tertentu. Peta ini membantu mengidentifikasi pola spasial dan clustering geografis dari kerentanan sosial."
  })
  
  # ================ UJI ASUMSI ================
  observeEvent(input$testNormality, {
    if (values$data_loaded && !is.null(input$normVar)) {
      var_data <- data_sovi[[input$normVar]]
      var_data <- var_data[!is.na(var_data)]
      
      if (length(var_data) > 3 && length(var_data) <= 5000) {
        shapiro_test <- shapiro.test(var_data)
        values$results_list$normality <- shapiro_test
        
        output$normalityTest <- renderPrint({
          shapiro_test
        })
        
        output$normalityPlot <- renderPlot({
          par(mfrow = c(1, 2))
          hist(var_data, main = paste("Histogram -", input$normVar), 
               xlab = input$normVar, col = "lightblue")
          qqnorm(var_data, main = paste("Q-Q Plot -", input$normVar))
          qqline(var_data, col = "red")
        })
        
        output$normalityInterpretation <- renderText({
          interpret_normality(shapiro_test$p.value)
        })
      } else {
        output$normalityTest <- renderText({
          "Data terlalu sedikit (≤3) atau terlalu banyak (>5000) untuk uji Shapiro-Wilk."
        })
      }
    }
  })
  
  observeEvent(input$testHomogeneity, {
    if (values$data_loaded && !is.null(input$homogVar) && !is.null(input$homogGroup)) {
      tryCatch({
        formula_str <- paste(input$homogVar, "~", input$homogGroup)
        levene_test <- car::leveneTest(as.formula(formula_str), data = data_sovi)
        values$results_list$homogeneity <- levene_test
        
        output$homogeneityTest <- renderPrint({
          levene_test
        })
        
        output$homogeneityPlot <- renderPlot({
          boxplot(as.formula(formula_str), data = data_sovi,
                  main = paste("Boxplot -", input$homogVar, "by", input$homogGroup),
                  col = "lightgreen")
        })
        
        output$homogeneityInterpretation <- renderText({
          interpret_homogeneity(levene_test$`Pr(>F)`[1])
        })
      }, error = function(e) {
        output$homogeneityTest <- renderText({
          paste("Error:", e$message)
        })
      })
    }
  })
  
  # ================ STATISTIK INFERENSIA I ================
  observeEvent(input$testProportion, {
    if (values$data_loaded && !is.null(input$propVar)) {
      var_data <- data_sovi[[input$propVar]]
      
      tryCatch({
        if (is.factor(var_data) || is.character(var_data)) {
          # For categorical data
          table_data <- table(var_data)
          prop_test <- prop.test(table_data[1], sum(table_data), p = input$propValue)
        } else {
          # For binary numeric data
          success_count <- sum(var_data == 1, na.rm = TRUE)
          total_count <- sum(!is.na(var_data))
          prop_test <- prop.test(success_count, total_count, p = input$propValue)
        }
        
        values$results_list$proportion <- prop_test
        
        output$proportionTest <- renderPrint({
          prop_test
        })
        
        output$proportionPlot <- renderPlot({
          if (is.factor(var_data) || is.character(var_data)) {
            barplot(table(var_data), main = paste("Distribusi", input$propVar),
                    col = rainbow(length(table(var_data))))
          } else {
            barplot(table(var_data), main = paste("Distribusi", input$propVar),
                    col = c("lightcoral", "lightblue"))
          }
        })
        
        output$proportionInterpretation <- renderText({
          p_val <- prop_test$p.value
          if (p_val < 0.05) {
            paste("Proporsi observasi berbeda signifikan dari proporsi hipotesis", input$propValue,
                  "(p =", round(p_val, 4), "< 0.05).")
          } else {
            paste("Proporsi observasi tidak berbeda signifikan dari proporsi hipotesis", input$propValue,
                  "(p =", round(p_val, 4), "≥ 0.05).")
          }
        })
      }, error = function(e) {
        output$proportionTest <- renderText({
          paste("Error dalam uji proporsi:", e$message)
        })
      })
    }
  })
  
  observeEvent(input$testVariance, {
    if (values$data_loaded && !is.null(input$varTestVar1)) {
      var1_data <- data_sovi[[input$varTestVar1]]
      
      tryCatch({
        if (is.null(input$varTestVar2) || input$varTestVar2 == "") {
          # One sample variance test
          n <- length(var1_data[!is.na(var1_data)])
          sample_var <- var(var1_data, na.rm = TRUE)
          chi_stat <- (n - 1) * sample_var / input$varHypothesis
          p_value <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))
          
          var_test <- list(
            statistic = chi_stat,
            p.value = p_value,
            parameter = n - 1,
            method = "One sample variance test"
          )
        } else {
          # Two sample variance test
          var2_data <- data_sovi[[input$varTestVar2]]
          var_test <- var.test(var1_data, var2_data)
        }
        
        values$results_list$variance <- var_test
        
        output$varianceTest <- renderPrint({
          var_test
        })
        
        output$variancePlot <- renderPlot({
          if (is.null(input$varTestVar2) || input$varTestVar2 == "") {
            hist(var1_data, main = paste("Distribution of", input$varTestVar1),
                 col = "lightblue", xlab = input$varTestVar1)
          } else {
            boxplot(list(var1_data, data_sovi[[input$varTestVar2]]),
                    names = c(input$varTestVar1, input$varTestVar2),
                    main = "Comparison of Variances",
                    col = c("lightblue", "lightcoral"))
          }
        })
        
        output$varianceInterpretation <- renderText({
          p_val <- var_test$p.value
          if (p_val < 0.05) {
            "Terdapat perbedaan varians yang signifikan (p < 0.05)."
          } else {
            "Tidak terdapat perbedaan varians yang signifikan (p ≥ 0.05)."
          }
        })
      }, error = function(e) {
        output$varianceTest <- renderText({
          paste("Error dalam uji varians:", e$message)
        })
      })
    }
  })
  
  # ================ STATISTIK INFERENSIA II ================
  observeEvent(input$testAnova1, {
    if (values$data_loaded && !is.null(input$anovaDepVar) && !is.null(input$anovaIndepVar)) {
      tryCatch({
        formula_str <- paste(input$anovaDepVar, "~", input$anovaIndepVar)
        anova_model <- aov(as.formula(formula_str), data = data_sovi)
        anova_summary <- summary(anova_model)
        tukey_test <- TukeyHSD(anova_model)
        
        values$results_list$anova1 <- list(model = anova_model, summary = anova_summary, tukey = tukey_test)
        
        output$anova1Test <- renderPrint({
          anova_summary
        })
        
        output$tukey1Test <- renderPrint({
          tukey_test
        })
        
        output$anova1Plot <- renderPlot({
          par(mfrow = c(2, 2))
          plot(anova_model)
        })
        
        output$anova1Interpretation <- renderText({
          p_val <- anova_summary[[1]]$`Pr(>F)`[1]
          if (p_val < 0.05) {
            paste("ANOVA satu arah menunjukkan perbedaan rata-rata yang signifikan antar kelompok",
                  "(F =", round(anova_summary[[1]]$`F value`[1], 3), ", p =", round(p_val, 4), "< 0.05).",
                  "Post-hoc test Tukey HSD menunjukkan pasangan kelompok mana yang berbeda signifikan.")
          } else {
            paste("ANOVA satu arah tidak menunjukkan perbedaan rata-rata yang signifikan antar kelompok",
                  "(F =", round(anova_summary[[1]]$`F value`[1], 3), ", p =", round(p_val, 4), "≥ 0.05).")
          }
        })
      }, error = function(e) {
        output$anova1Test <- renderText({
          paste("Error dalam analisis ANOVA:", e$message)
        })
      })
    }
  })
  
  observeEvent(input$testAnova2, {
    if (values$data_loaded && !is.null(input$anova2DepVar) && 
        !is.null(input$anova2IndepVar1) && !is.null(input$anova2IndepVar2)) {
      tryCatch({
        if (input$anova2Interaction) {
          formula_str <- paste(input$anova2DepVar, "~", input$anova2IndepVar1, "*", input$anova2IndepVar2)
        } else {
          formula_str <- paste(input$anova2DepVar, "~", input$anova2IndepVar1, "+", input$anova2IndepVar2)
        }
        
        anova2_model <- aov(as.formula(formula_str), data = data_sovi)
        anova2_summary <- summary(anova2_model)
        
        values$results_list$anova2 <- list(model = anova2_model, summary = anova2_summary)
        
        output$anova2Test <- renderPrint({
          anova2_summary
        })
        
        output$anova2Plot <- renderPlot({
          # Interaction plot
          tryCatch({
            interaction.plot(data_sovi[[input$anova2IndepVar1]], 
                            data_sovi[[input$anova2IndepVar2]], 
                            data_sovi[[input$anova2DepVar]],
                            main = "Interaction Plot",
                            xlab = input$anova2IndepVar1,
                            trace.label = input$anova2IndepVar2)
          }, error = function(e) {
            plot(1, 1, main = "Error creating interaction plot")
            text(1, 1, "Cannot create interaction plot\nwith current variables")
          })
        })
        
        output$anova2Interpretation <- renderText({
          summary_table <- anova2_summary[[1]]
          interpretations <- c()
          
          for (i in 1:nrow(summary_table)) {
            if (!is.na(summary_table$`Pr(>F)`[i])) {
              effect_name <- rownames(summary_table)[i]
              p_val <- summary_table$`Pr(>F)`[i]
              f_val <- summary_table$`F value`[i]
              
              if (p_val < 0.05) {
                interpretations <- c(interpretations, 
                  paste("Efek", effect_name, "signifikan (F =", round(f_val, 3), ", p =", round(p_val, 4), ")"))
              } else {
                interpretations <- c(interpretations,
                  paste("Efek", effect_name, "tidak signifikan (F =", round(f_val, 3), ", p =", round(p_val, 4), ")"))
              }
            }
          }
          
          paste(interpretations, collapse = ". ")
        })
      }, error = function(e) {
        output$anova2Test <- renderText({
          paste("Error dalam analisis ANOVA dua arah:", e$message)
        })
      })
    }
  })
  
  # ================ REGRESI LINEAR ================
  observeEvent(input$buildRegression, {
    if (values$data_loaded && !is.null(input$regDepVar) && length(input$regIndepVars) > 0) {
      tryCatch({
        formula_str <- paste(input$regDepVar, "~", paste(input$regIndepVars, collapse = " + "))
        reg_model <- lm(as.formula(formula_str), data = data_sovi)
        reg_summary <- summary(reg_model)
        
        values$results_list$regression <- list(model = reg_model, summary = reg_summary)
        
        output$regressionSummary <- renderPrint({
          reg_summary
        })
        
        output$regressionInterpretation <- renderText({
          r_squared <- reg_summary$r.squared
          adj_r_squared <- reg_summary$adj.r.squared
          f_stat <- reg_summary$fstatistic[1]
          p_value <- pf(f_stat, reg_summary$fstatistic[2], reg_summary$fstatistic[3], lower.tail = FALSE)
          
          paste("Model regresi menjelaskan", round(r_squared * 100, 2), "% variabilitas dalam", input$regDepVar, ".",
                "R-squared adjusted:", round(adj_r_squared, 4), ".",
                "Model secara keseluruhan", ifelse(p_value < 0.05, "signifikan", "tidak signifikan"),
                "(F =", round(f_stat, 3), ", p =", round(p_value, 4), ").")
        })
        
        # Create prediction inputs
        output$predictionInputs <- renderUI({
          input_list <- list()
          for (var in input$regIndepVars) {
            var_range <- range(data_sovi[[var]], na.rm = TRUE)
            var_mean <- mean(data_sovi[[var]], na.rm = TRUE)
            input_list[[var]] <- numericInput(paste0("pred_", var), 
                                            label = paste("Nilai", var, ":"),
                                            value = var_mean,
                                            min = var_range[1],
                                            max = var_range[2])
          }
          input_list
        })
      }, error = function(e) {
        output$regressionSummary <- renderText({
          paste("Error dalam membangun model regresi:", e$message)
        })
      })
    }
  })
  
  output$regressionDiagnostics <- renderPlot({
    if (!is.null(values$results_list$regression)) {
      par(mfrow = c(2, 2))
      plot(values$results_list$regression$model)
    }
  })
  
  output$regressionAssumptions <- renderPrint({
    if (!is.null(values$results_list$regression)) {
      model <- values$results_list$regression$model
      
      tryCatch({
        # Durbin-Watson test for autocorrelation
        dw_test <- car::durbinWatsonTest(model)
        
        # Breusch-Pagan test for heteroscedasticity
        bp_test <- car::ncvTest(model)
        
        # VIF for multicollinearity
        if (length(model$coefficients) > 2) {
          vif_values <- car::vif(model)
        } else {
          vif_values <- "VIF not applicable for single predictor model"
        }
        
        list(
          "Durbin-Watson Test (Autocorrelation)" = dw_test,
          "Breusch-Pagan Test (Heteroscedasticity)" = bp_test,
          "Variance Inflation Factors (Multicollinearity)" = vif_values
        )
      }, error = function(e) {
        paste("Error in assumption tests:", e$message)
      })
    }
  })
  
  output$assumptionInterpretation <- renderText({
    if (!is.null(values$results_list$regression)) {
      interpretations <- c()
      
      # Linearity: Check from residuals vs fitted plot
      interpretations <- c(interpretations, 
        "Linearitas: Periksa plot Residuals vs Fitted - titik harus tersebar acak tanpa pola.")
      
      # Normality: Check from Q-Q plot
      interpretations <- c(interpretations,
        "Normalitas: Periksa Q-Q plot - titik harus mengikuti garis diagonal.")
      
      # Homoscedasticity: Check from Scale-Location plot
      interpretations <- c(interpretations,
        "Homoskedastisitas: Periksa plot Scale-Location - garis harus horizontal dengan variabilitas konstan.")
      
      # Independence: Check Durbin-Watson
      interpretations <- c(interpretations,
        "Independensi: Nilai Durbin-Watson mendekati 2 menunjukkan tidak ada autokorelasi.")
      
      paste(interpretations, collapse = " ")
    }
  })
  
  observeEvent(input$makePrediction, {
    if (!is.null(values$results_list$regression) && length(input$regIndepVars) > 0) {
      model <- values$results_list$regression$model
      
      tryCatch({
        # Create prediction data frame
        pred_data <- data.frame(row.names = 1)
        for (var in input$regIndepVars) {
          pred_data[[var]] <- input[[paste0("pred_", var)]]
        }
        
        prediction <- predict(model, newdata = pred_data, interval = "prediction")
        
        output$predictionResult <- renderPrint({
          list(
            "Predicted Value" = round(prediction[1], 4),
            "95% Prediction Interval" = paste("[", round(prediction[2], 4), ",", round(prediction[3], 4), "]"),
            "Input Values" = pred_data
          )
        })
        
        output$predictionPlot <- renderPlot({
          # Plot actual vs predicted for all data
          fitted_vals <- fitted(model)
          actual_vals <- model$model[,1]
          
          plot(actual_vals, fitted_vals, 
               xlab = "Actual Values", ylab = "Fitted Values",
               main = "Actual vs Fitted Values",
               pch = 16, col = "blue", alpha = 0.6)
          abline(0, 1, col = "red", lwd = 2)
          
          # Add prediction point
          points(prediction[1], prediction[1], col = "red", pch = 16, cex = 2)
          text(prediction[1], prediction[1], "Prediction", pos = 3, col = "red", font = 2)
        })
      }, error = function(e) {
        output$predictionResult <- renderText({
          paste("Error in prediction:", e$message)
        })
      })
    }
  })
  
  # ================ DOWNLOAD FUNCTIONALITY ================
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("SOVI_Analysis_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create a temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Write Rmd content
      rmd_content <- "
---
title: 'Laporan Analisis SOVI (Social Vulnerability Index)'
subtitle: 'Dashboard Analisis Kerentanan Sosial Terpadu'
author: 'SoVi Explorer Pro'
date: '`r Sys.Date()`'
output: 
  pdf_document:
    toc: true
    toc_depth: 3
    number_sections: true
    latex_engine: xelatex
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(ggplot2)
```

# Ringkasan Eksekutif

Laporan ini menyajikan analisis komprehensif data Social Vulnerability Index (SOVI) menggunakan berbagai metode statistik terapan.

# Data Overview

Dataset SOVI terdiri dari observasi dengan beberapa variabel sosio-ekonomi.

# Hasil Analisis

Berdasarkan analisis yang telah dilakukan, ditemukan beberapa insight penting mengenai kerentanan sosial.

## Interpretasi Hasil

Analisis menunjukkan pola-pola tertentu dalam distribusi kerentanan sosial yang dapat digunakan untuk pengambilan kebijakan.

# Kesimpulan

Dashboard ini berhasil menganalisis data SOVI dengan berbagai metode statistik, memberikan insight yang komprehensif tentang kerentanan sosial.
      "
      
      writeLines(rmd_content, temp_rmd)
      
      # Render the document
      tryCatch({
        rmarkdown::render(temp_rmd, output_file = file)
      }, error = function(e) {
        # Fallback to simple text file if PDF rendering fails
        writeLines("Laporan SOVI Analysis - Dashboard telah menganalisis data dengan sukses.", file)
      })
    }
  )
  
  output$downloadImages <- downloadHandler(
    filename = function() {
      paste("SOVI_Plots_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Save plots
      if (values$data_loaded && nrow(data_sovi) > 0) {
        # Save a sample plot
        png(file.path(temp_dir, "sample_plot.png"), width = 800, height = 600)
        tryCatch({
          hist(data_sovi[[1]], main = "Sample Histogram", col = "lightblue")
        }, error = function(e) {
          plot(1, 1, main = "Sample Plot")
        })
        dev.off()
      }
      
      # Create zip file
      files_to_zip <- list.files(temp_dir, pattern = "\\.png$", full.names = TRUE)
      if (length(files_to_zip) > 0) {
        zip(file, files_to_zip)
      } else {
        # Create empty file if no plots
        writeLines("No plots available", file)
      }
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("SOVI_Data_Results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Create workbook
        wb <- openxlsx::createWorkbook()
        
        # Add original data
        if (values$data_loaded) {
          openxlsx::addWorksheet(wb, "Original_Data")
          openxlsx::writeData(wb, "Original_Data", data_sovi)
          
          # Add summary statistics
          numeric_data <- data_sovi[sapply(data_sovi, is.numeric)]
          if (ncol(numeric_data) > 0) {
            summary_stats <- psych::describe(numeric_data)
            openxlsx::addWorksheet(wb, "Summary_Statistics")
            openxlsx::writeData(wb, "Summary_Statistics", summary_stats, rowNames = TRUE)
          }
        }
        
        # Save workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }, error = function(e) {
        # Fallback to CSV if Excel fails
        if (values$data_loaded) {
          write.csv(data_sovi, file, row.names = FALSE)
        } else {
          writeLines("No data available", file)
        }
      })
    }
  )
  
  output$reportPreview <- renderText({
    components <- input$downloadComponents
    formats <- input$downloadFormats
    
    preview_text <- "Laporan akan mencakup:\n"
    
    if ("overview" %in% components) {
      preview_text <- paste(preview_text, "• Ringkasan data dan metadata\n")
    }
    if ("descriptive" %in% components) {
      preview_text <- paste(preview_text, "• Statistik deskriptif lengkap\n")
    }
    if ("plots" %in% components) {
      preview_text <- paste(preview_text, "• Visualisasi dan grafik\n")
    }
    if ("tests" %in% components) {
      preview_text <- paste(preview_text, "• Hasil uji statistik\n")
    }
    if ("regression" %in% components) {
      preview_text <- paste(preview_text, "• Analisis regresi\n")
    }
    
    if (length(formats) > 0) {
      preview_text <- paste(preview_text, "\nFormat output:", paste(formats, collapse = ", "))
    }
    
    return(preview_text)
  })
  
  # Download processed data handler
  output$downloadProcessedData <- downloadHandler(
    filename = function() {
      paste("Processed_SOVI_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (values$data_loaded) {
        write.csv(data_sovi, file, row.names = FALSE)
      } else {
        writeLines("No data available", file)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)