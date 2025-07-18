# ==============================================================================
# ANALITIKA VULNERABILITAS SOSIAL INDONESIA (AVSI) DASHBOARD
# Dashboard Analisis Data Social Vulnerability Index (SOVI) Indonesia
# ==============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(corrplot)
library(VIM)
library(psych)
library(car)
library(nortest)
library(rstatix)
library(broom)
library(dplyr)
library(readr)
library(cluster)
library(factoextra)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(shinycssloaders)
library(shinyWidgets)
library(fresh)

# Create custom theme
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#3498DB"
  ),
  adminlte_sidebar(
    dark_bg = "#2C3E50",
    dark_hover_bg = "#34495E",
    dark_color = "#ECF0F1"
  ),
  adminlte_global(
    content_bg = "#F8F9FA",
    box_bg = "#FFFFFF", 
    info_box_bg = "#FFFFFF"
  )
)

# Data loading function
load_data <- function() {
  tryCatch({
    # Load SOVI data
    sovi_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")
    
    # Load distance matrix
    distance_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")
    distance_matrix <- as.matrix(distance_data[,-1])
    rownames(distance_matrix) <- distance_data$...1
    
    return(list(sovi = sovi_data, distance = distance_matrix))
  }, error = function(e) {
    return(NULL)
  })
}

# Helper functions for analysis
create_categorical <- function(x, breaks = 3, labels = c("Rendah", "Sedang", "Tinggi")) {
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

perform_normality_test <- function(data, variable) {
  x <- data[[variable]]
  x <- x[!is.na(x)]
  
  if(length(x) < 3) return(list(method = "Insufficient data", p.value = NA))
  
  if(length(x) < 5000) {
    shapiro_test <- shapiro.test(x)
    return(list(method = "Shapiro-Wilk", statistic = shapiro_test$statistic, p.value = shapiro_test$p.value))
  } else {
    ks_test <- lillie.test(x)
    return(list(method = "Lilliefors", statistic = ks_test$statistic, p.value = ks_test$p.value))
  }
}

perform_homogeneity_test <- function(data, numeric_var, group_var) {
  formula_str <- paste(numeric_var, "~", group_var)
  levene_test <- car::leveneTest(as.formula(formula_str), data = data)
  return(list(statistic = levene_test$`F value`[1], p.value = levene_test$`Pr(>F)`[1]))
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "ANALITIKA VULNERABILITAS SOSIAL INDONESIA (AVSI)"),
  
  dashboardSidebar(
    use_theme(my_theme),
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("📊 Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem("🔍 Eksplorasi Data", tabName = "exploratory", icon = icon("chart-line")),
      menuItem("✅ Uji Asumsi", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("📈 Statistik Inferensia I", tabName = "inference1", icon = icon("calculator")),
      menuItem("📊 Statistik Inferensia II", tabName = "inference2", icon = icon("chart-bar")),
      menuItem("🔗 Regresi Linear", tabName = "regression", icon = icon("trending-up"))
    )
  ),
  
  dashboardBody(
    use_theme(my_theme),
    
    tabItems(
      # BERANDA TAB
      tabItem(tabName = "beranda",
        fluidRow(
          box(width = 12, title = "Selamat Datang di Dashboard AVSI", status = "primary", solidHeader = TRUE,
            h3("Analitika Vulnerabilitas Sosial Indonesia (AVSI)"),
            hr(),
            
            h4("📋 Metadata Dataset"),
            tags$div(
              style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #3498db; margin: 10px 0;",
              h5("🔹 Dataset SOVI (Social Vulnerability Index)"),
              p("Dataset ini berisi indeks vulnerabilitas sosial untuk berbagai wilayah di Indonesia dengan 16 variabel:"),
              tags$ul(
                tags$li("DISTRICTCODE: Kode distrik/wilayah"),
                tags$li("CHILDREN: Persentase anak-anak dalam populasi"),
                tags$li("FEMALE: Persentase populasi perempuan"),
                tags$li("ELDERLY: Persentase populasi lanjut usia"),
                tags$li("FHEAD: Persentase kepala keluarga perempuan"),
                tags$li("FAMILYSIZE: Rata-rata ukuran keluarga"),
                tags$li("NOELECTRIC: Persentase tanpa akses listrik"),
                tags$li("LOWEDU: Persentase pendidikan rendah"),
                tags$li("GROWTH: Tingkat pertumbuhan populasi"),
                tags$li("POVERTY: Persentase kemiskinan"),
                tags$li("ILLITERATE: Persentase buta huruf"),
                tags$li("NOTRAINING: Persentase tanpa pelatihan"),
                tags$li("DPRONE: Persentase rawan bencana"),
                tags$li("RENTED: Persentase rumah sewa"),
                tags$li("NOSEWER: Persentase tanpa akses sanitasi"),
                tags$li("TAPWATER: Persentase akses air bersih"),
                tags$li("POPULATION: Total populasi")
              )
            ),
            
            tags$div(
              style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #e74c3c; margin: 10px 0;",
              h5("🔹 Matriks Penimbang Jarak"),
              p("Matriks jarak spasial antar wilayah yang digunakan untuk analisis clustering dan pemetaan spasial.")
            ),
            
            h4("🎯 Fitur Dashboard"),
            fluidRow(
              valueBox(
                value = "7",
                subtitle = "Menu Analisis",
                icon = icon("layer-group"),
                color = "blue", width = 4
              ),
              valueBox(
                value = "16",
                subtitle = "Variabel Data",
                icon = icon("database"),
                color = "green", width = 4
              ),
              valueBox(
                value = "511",
                subtitle = "Observasi",
                icon = icon("chart-line"),
                color = "yellow", width = 4
              )
            ),
            
            h4("📊 Kemampuan Analisis"),
            fluidRow(
              column(6,
                tags$div(
                  style = "background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 5px 0;",
                  h5("✅ Manajemen Data"),
                  p("• Kategorisasi variabel kontinyu", br(),
                    "• Transformasi data", br(),
                    "• Cleaning dan preprocessing")
                ),
                tags$div(
                  style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 5px 0;",
                  h5("✅ Uji Asumsi"),
                  p("• Uji normalitas (Shapiro-Wilk/Lilliefors)", br(),
                    "• Uji homogenitas (Levene)", br(),
                    "• Interpretasi hasil")
                ),
                tags$div(
                  style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 5px 0;",
                  h5("✅ Regresi Linear Berganda"),
                  p("• Analisis regresi multivariat", br(),
                    "• Uji asumsi regresi", br(),
                    "• Model diagnostics")
                )
              ),
              column(6,
                tags$div(
                  style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin: 5px 0;",
                  h5("✅ Eksplorasi Data"),
                  p("• Statistik deskriptif", br(),
                    "• Visualisasi interaktif", br(),
                    "• Pemetaan spasial")
                ),
                tags$div(
                  style = "background-color: #e2e3e5; padding: 15px; border-radius: 5px; margin: 5px 0;",
                  h5("✅ Statistik Inferensia"),
                  p("• Uji proporsi dan varians", br(),
                    "• Uji beda rata-rata (t-test)", br(),
                    "• ANOVA satu dan dua arah")
                ),
                tags$div(
                  style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 5px 0;",
                  h5("✅ Export & Download"),
                  p("• Download hasil analisis", br(),
                    "• Export visualisasi", br(),
                    "• Laporan terintegrasi")
                )
              )
            ),
            
            hr(),
            h4("👨‍💻 Informasi Pengembang"),
            p("Dashboard ini dikembangkan untuk analisis komprehensif data vulnerabilitas sosial Indonesia menggunakan R Shiny dengan fokus pada kemudahan penggunaan dan interpretasi hasil yang mendalam."),
            
            tags$div(
              style = "background-color: #e9ecef; padding: 10px; border-radius: 5px; text-align: center;",
              p(strong("© 2024 ANALITIKA VULNERABILITAS SOSIAL INDONESIA (AVSI)"))
            )
          )
        )
      ),
      
      # DATA MANAGEMENT TAB
      tabItem(tabName = "data_management",
        fluidRow(
          box(width = 12, title = "Manajemen dan Transformasi Data", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("📊 Data Overview",
                h4("Data SOVI Indonesia"),
                withSpinner(DT::dataTableOutput("data_overview")),
                hr(),
                fluidRow(
                  column(4, 
                    h5("📈 Ringkasan Dataset"),
                    verbatimTextOutput("data_summary")
                  ),
                  column(4,
                    h5("🔍 Informasi Missing Data"),
                    withSpinner(plotOutput("missing_plot", height = "300px"))
                  ),
                  column(4,
                    h5("📋 Struktur Data"),
                    verbatimTextOutput("data_structure")
                  )
                ),
                br(),
                downloadButton("download_original", "📥 Download Data Asli", class = "btn btn-primary")
              ),
              
              tabPanel("🔄 Kategorisasi Variabel",
                h4("Transformasi Variabel Kontinyu ke Kategorik"),
                fluidRow(
                  column(4,
                    selectInput("var_to_categorize", "Pilih Variabel:",
                      choices = NULL
                    ),
                    numericInput("n_categories", "Jumlah Kategori:", value = 3, min = 2, max = 5),
                    textAreaInput("category_labels", "Label Kategori (pisahkan dengan koma):",
                      value = "Rendah, Sedang, Tinggi", rows = 3
                    ),
                    actionButton("create_categorical", "🔄 Buat Kategorisasi", class = "btn btn-success"),
                    br(), br(),
                    h5("📊 Interpretasi:"),
                    div(id = "categorization_interpretation",
                      p("Kategorisasi memungkinkan analisis data kontinyu dalam bentuk kelompok diskrit, memudahkan interpretasi dan analisis komparatif antar kategori.")
                    )
                  ),
                  column(8,
                    h5("Distribusi Variabel Asli vs Kategorik"),
                    withSpinner(plotOutput("categorization_plot", height = "400px")),
                    br(),
                    h5("Tabel Frekuensi Kategori"),
                    withSpinner(DT::dataTableOutput("category_table"))
                  )
                ),
                br(),
                downloadButton("download_categorized", "📥 Download Data Terkategorisasi", class = "btn btn-info")
              ),
              
              tabPanel("🧹 Data Cleaning",
                h4("Pembersihan dan Preprocessing Data"),
                fluidRow(
                  column(6,
                    h5("🔍 Deteksi Outliers"),
                    selectInput("outlier_var", "Pilih Variabel:", choices = NULL),
                    selectInput("outlier_method", "Metode Deteksi:",
                      choices = list("IQR Method" = "iqr", "Z-Score" = "zscore", "Modified Z-Score" = "modified_z")
                    ),
                    actionButton("detect_outliers", "🔍 Deteksi Outliers", class = "btn btn-warning"),
                    br(), br(),
                    h6("📋 Interpretasi Outliers:"),
                    div(id = "outlier_interpretation",
                      p("Outliers adalah nilai yang secara signifikan berbeda dari mayoritas data. Deteksi outliers penting untuk memastikan kualitas analisis statistik.")
                    )
                  ),
                  column(6,
                    h5("📈 Visualisasi Outliers"),
                    withSpinner(plotOutput("outlier_plot", height = "350px"))
                  )
                ),
                hr(),
                fluidRow(
                  column(12,
                    h5("📊 Summary Outliers yang Terdeteksi"),
                    withSpinner(DT::dataTableOutput("outlier_table"))
                  )
                )
              )
            )
          )
        )
      ),
      
      # EXPLORATORY DATA ANALYSIS TAB
      tabItem(tabName = "exploratory",
        fluidRow(
          box(width = 12, title = "Analisis Eksplorasi Data", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("📊 Statistik Deskriptif",
                fluidRow(
                  column(4,
                    selectInput("desc_var", "Pilih Variabel:", choices = NULL),
                    checkboxInput("by_group", "Kelompokkan berdasarkan variabel kategorik", FALSE),
                    conditionalPanel(
                      condition = "input.by_group == true",
                      selectInput("group_var_desc", "Variabel Pengelompokan:", choices = NULL)
                    )
                  ),
                  column(8,
                    h5("📈 Statistik Deskriptif"),
                    withSpinner(verbatimTextOutput("descriptive_stats"))
                  )
                ),
                hr(),
                fluidRow(
                  column(6,
                    h5("📊 Histogram & Density Plot"),
                    withSpinner(plotOutput("histogram_plot", height = "350px"))
                  ),
                  column(6,
                    h5("📦 Box Plot"),
                    withSpinner(plotOutput("boxplot_plot", height = "350px"))
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    h5("📋 Interpretasi Statistik Deskriptif"),
                    div(id = "desc_interpretation",
                      p("Statistik deskriptif memberikan gambaran umum tentang distribusi data, termasuk ukuran pemusatan (mean, median) dan ukuran penyebaran (standar deviasi, range).")
                    )
                  )
                ),
                br(),
                downloadButton("download_descriptive", "📥 Download Hasil Deskriptif", class = "btn btn-success")
              ),
              
              tabPanel("📈 Visualisasi Lanjutan",
                fluidRow(
                  column(4,
                    selectInput("viz_type", "Jenis Visualisasi:",
                      choices = list(
                        "Scatter Plot" = "scatter",
                        "Correlation Plot" = "correlation",
                        "Pair Plot" = "pairs",
                        "Density Plot" = "density"
                      )
                    ),
                    conditionalPanel(
                      condition = "input.viz_type == 'scatter'",
                      selectInput("x_var", "Variabel X:", choices = NULL),
                      selectInput("y_var", "Variabel Y:", choices = NULL),
                      selectInput("color_var", "Warna berdasarkan:", choices = NULL)
                    ),
                    conditionalPanel(
                      condition = "input.viz_type == 'pairs'",
                      checkboxGroupInput("pair_vars", "Pilih Variabel (max 6):", choices = NULL)
                    ),
                    actionButton("generate_viz", "📊 Generate Visualisasi", class = "btn btn-info")
                  ),
                  column(8,
                    withSpinner(plotOutput("advanced_plot", height = "500px"))
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    h5("📋 Interpretasi Visualisasi"),
                    div(id = "viz_interpretation",
                      p("Visualisasi lanjutan membantu mengidentifikasi pola, hubungan, dan struktur dalam data yang tidak terlihat dari statistik deskriptif sederhana.")
                    )
                  )
                ),
                br(),
                downloadButton("download_visualization", "📥 Download Visualisasi", class = "btn btn-primary")
              ),
              
              tabPanel("🗺️ Pemetaan Spasial",
                h4("Analisis Spasial Vulnerabilitas"),
                fluidRow(
                  column(4,
                    selectInput("map_var", "Variabel untuk Pemetaan:", choices = NULL),
                    selectInput("cluster_method", "Metode Clustering:",
                      choices = list("K-Means" = "kmeans", "Hierarchical" = "hclust", "PAM" = "pam")
                    ),
                    numericInput("n_clusters", "Jumlah Cluster:", value = 3, min = 2, max = 8),
                    actionButton("perform_clustering", "🎯 Lakukan Clustering", class = "btn btn-warning"),
                    br(), br(),
                    h6("📍 Interpretasi Clustering:"),
                    div(id = "clustering_interpretation",
                      p("Clustering mengelompokkan wilayah berdasarkan kesamaan karakteristik vulnerabilitas sosial, membantu identifikasi pola spasial.")
                    )
                  ),
                  column(8,
                    h5("🗺️ Peta Clustering Wilayah"),
                    withSpinner(plotOutput("cluster_plot", height = "400px")),
                    br(),
                    h5("📊 Karakteristik Cluster"),
                    withSpinner(DT::dataTableOutput("cluster_summary"))
                  )
                ),
                br(),
                downloadButton("download_clustering", "📥 Download Hasil Clustering", class = "btn btn-success")
              )
            )
          )
        )
      ),
      
      # ASSUMPTIONS TESTING TAB
      tabItem(tabName = "assumptions",
        fluidRow(
          box(width = 12, title = "Uji Asumsi Data", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("📊 Uji Normalitas",
                h4("Pengujian Normalitas Distribusi Data"),
                fluidRow(
                  column(4,
                    selectInput("norm_var", "Pilih Variabel:", choices = NULL),
                    checkboxInput("by_group_norm", "Uji per kelompok", FALSE),
                    conditionalPanel(
                      condition = "input.by_group_norm == true",
                      selectInput("group_var_norm", "Variabel Pengelompokan:", choices = NULL)
                    ),
                    actionButton("test_normality", "🔍 Uji Normalitas", class = "btn btn-primary"),
                    br(), br(),
                    h6("📋 Interpretasi Uji Normalitas:"),
                    div(id = "normality_interpretation",
                      p("Uji normalitas menentukan apakah data mengikuti distribusi normal. P-value < 0.05 menunjukkan data tidak normal.")
                    )
                  ),
                  column(8,
                    h5("📈 Q-Q Plot & Histogram"),
                    withSpinner(plotOutput("normality_plot", height = "400px"))
                  )
                ),
                hr(),
                fluidRow(
                  column(12,
                    h5("📊 Hasil Uji Normalitas"),
                    withSpinner(verbatimTextOutput("normality_results"))
                  )
                ),
                br(),
                downloadButton("download_normality", "📥 Download Hasil Normalitas", class = "btn btn-info")
              ),
              
              tabPanel("⚖️ Uji Homogenitas",
                h4("Pengujian Homogenitas Varians"),
                fluidRow(
                  column(4,
                    selectInput("homo_numeric_var", "Variabel Numerik:", choices = NULL),
                    selectInput("homo_group_var", "Variabel Pengelompokan:", choices = NULL),
                    actionButton("test_homogeneity", "⚖️ Uji Homogenitas", class = "btn btn-success"),
                    br(), br(),
                    h6("📋 Interpretasi Uji Homogenitas:"),
                    div(id = "homogeneity_interpretation",
                      p("Uji homogenitas (Levene) menguji kesamaan varians antar kelompok. P-value < 0.05 menunjukkan varians tidak homogen.")
                    )
                  ),
                  column(8,
                    h5("📦 Box Plot per Kelompok"),
                    withSpinner(plotOutput("homogeneity_plot", height = "400px"))
                  )
                ),
                hr(),
                fluidRow(
                  column(6,
                    h5("📊 Hasil Uji Levene"),
                    withSpinner(verbatimTextOutput("homogeneity_results"))
                  ),
                  column(6,
                    h5("📈 Statistik Deskriptif per Kelompok"),
                    withSpinner(DT::dataTableOutput("group_desc_stats"))
                  )
                ),
                br(),
                downloadButton("download_homogeneity", "📥 Download Hasil Homogenitas", class = "btn btn-warning")
              ),
              
              tabPanel("📋 Ringkasan Asumsi",
                h4("Ringkasan Pengujian Asumsi"),
                fluidRow(
                  column(12,
                    h5("📊 Status Asumsi Data"),
                    withSpinner(DT::dataTableOutput("assumption_summary")),
                    br(),
                    h5("🎯 Rekomendasi Analisis"),
                    div(id = "analysis_recommendations",
                      p("Berdasarkan hasil uji asumsi, berikut rekomendasi metode analisis yang sesuai untuk data Anda.")
                    ),
                    br(),
                    h5("📋 Interpretasi Komprehensif"),
                    div(id = "comprehensive_interpretation",
                      tags$ul(
                        tags$li("Jika data normal dan homogen: gunakan parametric tests"),
                        tags$li("Jika data tidak normal: pertimbangkan transformasi atau non-parametric tests"),
                        tags$li("Jika varians tidak homogen: gunakan Welch's t-test atau robust methods")
                      )
                    )
                  )
                ),
                br(),
                downloadButton("download_assumptions", "📥 Download Ringkasan Asumsi", class = "btn btn-primary")
              )
            )
          )
        )
      ),
      
      # INFERENTIAL STATISTICS I TAB
      tabItem(tabName = "inference1",
        fluidRow(
          box(width = 12, title = "Statistik Inferensia I: Uji Proporsi dan Varians", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("📊 Uji Proporsi",
                h4("Pengujian Proporsi Populasi"),
                fluidRow(
                  column(4,
                    selectInput("prop_var", "Pilih Variabel Kategorik:", choices = NULL),
                    selectInput("prop_category", "Kategori yang Diuji:", choices = NULL),
                    numericInput("prop_null", "Proporsi Null Hypothesis:", value = 0.5, min = 0, max = 1, step = 0.01),
                    selectInput("prop_alternative", "Hipotesis Alternatif:",
                      choices = list("Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less")
                    ),
                    numericInput("confidence_level", "Confidence Level:", value = 0.95, min = 0.90, max = 0.99, step = 0.01),
                    actionButton("test_proportion", "📊 Uji Proporsi", class = "btn btn-primary")
                  ),
                  column(8,
                    h5("📈 Visualisasi Proporsi"),
                    withSpinner(plotOutput("proportion_plot", height = "350px")),
                    br(),
                    h5("📊 Hasil Uji Proporsi"),
                    withSpinner(verbatimTextOutput("proportion_results"))
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    h5("📋 Interpretasi Uji Proporsi"),
                    div(id = "proportion_interpretation",
                      p("Uji proporsi menguji apakah proporsi sampel berbeda signifikan dari nilai yang dihipotesiskan. P-value < α menunjukkan proporsi berbeda signifikan.")
                    )
                  )
                ),
                br(),
                downloadButton("download_proportion", "📥 Download Hasil Proporsi", class = "btn btn-success")
              ),
              
              tabPanel("📏 Uji Varians - 1 Kelompok",
                h4("Pengujian Varians Satu Kelompok"),
                fluidRow(
                  column(4,
                    selectInput("var_test_var", "Pilih Variabel:", choices = NULL),
                    numericInput("var_null", "Varians Null Hypothesis:", value = 1, min = 0, step = 0.1),
                    selectInput("var_alternative", "Hipotesis Alternatif:",
                      choices = list("Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less")
                    ),
                    actionButton("test_variance_one", "📏 Uji Varians", class = "btn btn-info")
                  ),
                  column(8,
                    h5("📊 Distribusi Data"),
                    withSpinner(plotOutput("variance_one_plot", height = "350px")),
                    br(),
                    h5("📈 Hasil Uji Chi-Square untuk Varians"),
                    withSpinner(verbatimTextOutput("variance_one_results"))
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    h5("📋 Interpretasi Uji Varians"),
                    div(id = "variance_one_interpretation",
                      p("Uji chi-square untuk varians menguji apakah varians populasi sama dengan nilai yang dihipotesiskan.")
                    )
                  )
                ),
                br(),
                downloadButton("download_variance_one", "📥 Download Hasil Uji Varians", class = "btn btn-warning")
              ),
              
              tabPanel("📏 Uji Varians - 2 Kelompok",
                h4("Pengujian Perbandingan Varians Dua Kelompok"),
                fluidRow(
                  column(4,
                    selectInput("var_test_numeric", "Variabel Numerik:", choices = NULL),
                    selectInput("var_test_group", "Variabel Pengelompokan:", choices = NULL),
                    selectInput("var_test_alternative", "Hipotesis Alternatif:",
                      choices = list("Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less")
                    ),
                    actionButton("test_variance_two", "📏 Uji F-Test", class = "btn btn-success")
                  ),
                  column(8,
                    h5("📦 Box Plot Perbandingan"),
                    withSpinner(plotOutput("variance_two_plot", height = "350px")),
                    br(),
                    h5("📊 Hasil F-Test untuk Varians"),
                    withSpinner(verbatimTextOutput("variance_two_results"))
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    h5("📋 Interpretasi F-Test"),
                    div(id = "variance_two_interpretation",
                      p("F-test membandingkan varians dua kelompok independent. P-value < α menunjukkan varians berbeda signifikan.")
                    )
                  )
                ),
                br(),
                downloadButton("download_variance_two", "📥 Download Hasil F-Test", class = "btn btn-primary")
              ),
              
              tabPanel("🔢 Uji Beda Rata-rata",
                h4("Pengujian Beda Rata-rata (T-Test)"),
                fluidRow(
                  column(4,
                    selectInput("ttest_type", "Jenis T-Test:",
                      choices = list("One Sample" = "one_sample", "Two Sample" = "two_sample", "Paired" = "paired")
                    ),
                    selectInput("ttest_var", "Variabel Numerik:", choices = NULL),
                    conditionalPanel(
                      condition = "input.ttest_type == 'one_sample'",
                      numericInput("mu_null", "Mean Null Hypothesis:", value = 0)
                    ),
                    conditionalPanel(
                      condition = "input.ttest_type != 'one_sample'",
                      selectInput("ttest_group", "Variabel Pengelompokan:", choices = NULL)
                    ),
                    checkboxInput("var_equal", "Asumsi varians sama", TRUE),
                    actionButton("perform_ttest", "🔢 Lakukan T-Test", class = "btn btn-danger")
                  ),
                  column(8,
                    h5("📊 Visualisasi T-Test"),
                    withSpinner(plotOutput("ttest_plot", height = "350px")),
                    br(),
                    h5("📈 Hasil T-Test"),
                    withSpinner(verbatimTextOutput("ttest_results"))
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    h5("📋 Interpretasi T-Test"),
                    div(id = "ttest_interpretation",
                      p("T-test menguji perbedaan rata-rata. P-value < α menunjukkan perbedaan rata-rata yang signifikan.")
                    )
                  )
                ),
                br(),
                downloadButton("download_ttest", "📥 Download Hasil T-Test", class = "btn btn-info")
              )
            )
          )
        )
      ),
      
      # INFERENTIAL STATISTICS II TAB (ANOVA)
      tabItem(tabName = "inference2",
        fluidRow(
          box(width = 12, title = "Statistik Inferensia II: ANOVA", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("📊 ANOVA Satu Arah",
                h4("Analysis of Variance - One Way"),
                fluidRow(
                  column(4,
                    selectInput("anova_dep_var", "Variabel Dependen:", choices = NULL),
                    selectInput("anova_indep_var", "Variabel Independen:", choices = NULL),
                    checkboxInput("perform_posthoc", "Post-hoc Test (Tukey HSD)", TRUE),
                    actionButton("perform_anova_one", "📊 Lakukan ANOVA", class = "btn btn-primary"),
                    br(), br(),
                    h6("📋 Interpretasi ANOVA:"),
                    div(id = "anova_one_interpretation",
                      p("ANOVA satu arah menguji perbedaan rata-rata antar 3+ kelompok. F-value yang besar dan p-value < α menunjukkan perbedaan signifikan.")
                    )
                  ),
                  column(8,
                    h5("📦 Box Plot ANOVA"),
                    withSpinner(plotOutput("anova_one_plot", height = "350px")),
                    br(),
                    h5("📊 Hasil ANOVA"),
                    withSpinner(verbatimTextOutput("anova_one_results"))
                  )
                ),
                hr(),
                fluidRow(
                  column(6,
                    h5("📈 Post-hoc Test Results"),
                    withSpinner(DT::dataTableOutput("posthoc_results"))
                  ),
                  column(6,
                    h5("📊 Effect Size"),
                    withSpinner(verbatimTextOutput("effect_size_one"))
                  )
                ),
                br(),
                downloadButton("download_anova_one", "📥 Download Hasil ANOVA", class = "btn btn-success")
              ),
              
              tabPanel("📊 ANOVA Dua Arah",
                h4("Analysis of Variance - Two Way"),
                fluidRow(
                  column(4,
                    selectInput("anova2_dep_var", "Variabel Dependen:", choices = NULL),
                    selectInput("anova2_factor1", "Faktor 1:", choices = NULL),
                    selectInput("anova2_factor2", "Faktor 2:", choices = NULL),
                    checkboxInput("include_interaction", "Sertakan Interaksi", TRUE),
                    actionButton("perform_anova_two", "📊 Lakukan ANOVA 2-Way", class = "btn btn-warning"),
                    br(), br(),
                    h6("📋 Interpretasi ANOVA 2-Way:"),
                    div(id = "anova_two_interpretation",
                      p("ANOVA dua arah menguji efek dua faktor dan interaksinya terhadap variabel dependen.")
                    )
                  ),
                  column(8,
                    h5("📊 Interaction Plot"),
                    withSpinner(plotOutput("anova_two_plot", height = "350px")),
                    br(),
                    h5("📈 Hasil ANOVA 2-Way"),
                    withSpinner(verbatimTextOutput("anova_two_results"))
                  )
                ),
                hr(),
                fluidRow(
                  column(6,
                    h5("📊 Effect Sizes"),
                    withSpinner(verbatimTextOutput("effect_size_two"))
                  ),
                  column(6,
                    h5("📈 Marginal Means"),
                    withSpinner(DT::dataTableOutput("marginal_means"))
                  )
                ),
                br(),
                downloadButton("download_anova_two", "📥 Download Hasil ANOVA 2-Way", class = "btn btn-info")
              ),
              
              tabPanel("🔍 Diagnostik ANOVA",
                h4("Diagnostik dan Validasi Asumsi ANOVA"),
                fluidRow(
                  column(4,
                    h5("Pilih Hasil ANOVA:"),
                    radioButtons("anova_diagnostic_choice", "",
                      choices = list("ANOVA Satu Arah" = "one_way", "ANOVA Dua Arah" = "two_way")
                    ),
                    actionButton("run_diagnostics", "🔍 Jalankan Diagnostik", class = "btn btn-danger"),
                    br(), br(),
                    h6("📋 Asumsi ANOVA:"),
                    div(id = "anova_assumptions",
                      tags$ul(
                        tags$li("Normalitas residual"),
                        tags$li("Homogenitas varians"),
                        tags$li("Independensi observasi")
                      )
                    )
                  ),
                  column(8,
                    h5("📊 Diagnostic Plots"),
                    withSpinner(plotOutput("anova_diagnostics", height = "400px"))
                  )
                ),
                hr(),
                fluidRow(
                  column(6,
                    h5("📈 Uji Normalitas Residual"),
                    withSpinner(verbatimTextOutput("residual_normality"))
                  ),
                  column(6,
                    h5("⚖️ Uji Homogenitas Varians"),
                    withSpinner(verbatimTextOutput("residual_homogeneity"))
                  )
                ),
                br(),
                downloadButton("download_diagnostics", "📥 Download Diagnostik", class = "btn btn-primary")
              )
            )
          )
        )
      ),
      
      # MULTIPLE REGRESSION TAB
      tabItem(tabName = "regression",
        fluidRow(
          box(width = 12, title = "Analisis Regresi Linear Berganda", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("🔧 Model Building",
                h4("Pembangunan Model Regresi"),
                fluidRow(
                  column(4,
                    selectInput("reg_dependent", "Variabel Dependen:", choices = NULL),
                    checkboxGroupInput("reg_predictors", "Variabel Prediktor:", choices = NULL),
                    checkboxInput("include_interaction_reg", "Sertakan Interaksi", FALSE),
                    conditionalPanel(
                      condition = "input.include_interaction_reg == true",
                      selectInput("interaction_vars", "Pilih 2 Variabel untuk Interaksi:", 
                                choices = NULL, multiple = TRUE)
                    ),
                    selectInput("model_selection", "Metode Seleksi Model:",
                      choices = list("Enter" = "enter", "Forward" = "forward", "Backward" = "backward", "Stepwise" = "stepwise")
                    ),
                    actionButton("build_model", "🔧 Bangun Model", class = "btn btn-primary")
                  ),
                  column(8,
                    h5("📊 Ringkasan Model"),
                    withSpinner(verbatimTextOutput("model_summary")),
                    br(),
                    h5("📈 ANOVA Model"),
                    withSpinner(verbatimTextOutput("model_anova"))
                  )
                ),
                br(),
                downloadButton("download_model", "📥 Download Model Results", class = "btn btn-success")
              ),
              
              tabPanel("📊 Diagnostik Model",
                h4("Diagnostik dan Validasi Model Regresi"),
                fluidRow(
                  column(4,
                    h5("Pilihan Diagnostik:"),
                    checkboxGroupInput("diagnostic_plots", "Plot Diagnostik:",
                      choices = list(
                        "Residuals vs Fitted" = "res_fitted",
                        "Normal Q-Q" = "qq",
                        "Scale-Location" = "scale_location",
                        "Cook's Distance" = "cooks"
                      ),
                      selected = c("res_fitted", "qq")
                    ),
                    actionButton("run_reg_diagnostics", "🔍 Jalankan Diagnostik", class = "btn btn-warning"),
                    br(), br(),
                    h6("📋 Asumsi Regresi:"),
                    div(id = "regression_assumptions",
                      tags$ul(
                        tags$li("Linearitas"),
                        tags$li("Independensi"),
                        tags$li("Homoskedastisitas"),
                        tags$li("Normalitas residual"),
                        tags$li("Tidak ada multikolinearitas")
                      )
                    )
                  ),
                  column(8,
                    h5("📊 Diagnostic Plots"),
                    withSpinner(plotOutput("regression_diagnostics", height = "500px"))
                  )
                ),
                hr(),
                fluidRow(
                  column(4,
                    h5("🔍 Uji Multikolinearitas (VIF)"),
                    withSpinner(DT::dataTableOutput("vif_table"))
                  ),
                  column(4,
                    h5("📈 Uji Normalitas Residual"),
                    withSpinner(verbatimTextOutput("reg_normality_test"))
                  ),
                  column(4,
                    h5("⚖️ Uji Homoskedastisitas"),
                    withSpinner(verbatimTextOutput("homoscedasticity_test"))
                  )
                ),
                br(),
                downloadButton("download_reg_diagnostics", "📥 Download Diagnostik Regresi", class = "btn btn-info")
              ),
              
              tabPanel("📈 Prediksi & Validasi",
                h4("Prediksi Model dan Validasi"),
                fluidRow(
                  column(4,
                    h5("Cross-Validation:"),
                    numericInput("cv_folds", "Number of Folds:", value = 5, min = 3, max = 10),
                    actionButton("run_cv", "🎯 Jalankan CV", class = "btn btn-success"),
                    br(), br(),
                    h5("Prediksi Baru:"),
                    p("Masukkan nilai untuk prediksi:"),
                    div(id = "prediction_inputs")
                  ),
                  column(8,
                    h5("📊 Model Performance"),
                    withSpinner(verbatimTextOutput("model_performance")),
                    br(),
                    h5("📈 Predicted vs Actual"),
                    withSpinner(plotOutput("prediction_plot", height = "350px"))
                  )
                ),
                hr(),
                fluidRow(
                  column(6,
                    h5("📊 Residual Analysis"),
                    withSpinner(plotOutput("residual_analysis", height = "300px"))
                  ),
                  column(6,
                    h5("📈 Model Metrics"),
                    withSpinner(verbatimTextOutput("model_metrics"))
                  )
                ),
                br(),
                downloadButton("download_predictions", "📥 Download Prediksi", class = "btn btn-primary")
              ),
              
              tabPanel("📋 Interpretasi Model",
                h4("Interpretasi dan Kesimpulan Model"),
                fluidRow(
                  column(12,
                    h5("📊 Koefisien dan Interpretasi"),
                    withSpinner(DT::dataTableOutput("coefficient_table")),
                    br(),
                    h5("📈 Confidence Intervals"),
                    withSpinner(DT::dataTableOutput("confidence_intervals")),
                    br(),
                    h5("🎯 Interpretasi Model"),
                    div(id = "model_interpretation",
                      p("Interpretasi komprehensif model regresi akan ditampilkan di sini berdasarkan hasil analisis.")
                    ),
                    br(),
                    h5("📋 Kesimpulan dan Rekomendasi"),
                    div(id = "model_conclusions",
                      p("Kesimpulan dan rekomendasi berdasarkan hasil analisis regresi.")
                    )
                  )
                ),
                br(),
                downloadButton("download_interpretation", "📥 Download Interpretasi Lengkap", class = "btn btn-success")
              )
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    original_data = NULL,
    current_data = NULL,
    distance_matrix = NULL,
    categorical_data = NULL,
    model_results = NULL,
    anova_one_results = NULL,
    anova_two_results = NULL
  )
  
  # Load data on startup
  observe({
    data_list <- load_data()
    if (!is.null(data_list)) {
      values$original_data <- data_list$sovi
      values$current_data <- data_list$sovi
      values$distance_matrix <- data_list$distance
      
      # Update choices for all inputs
      numeric_vars <- names(values$current_data)[sapply(values$current_data, is.numeric)]
      all_vars <- names(values$current_data)
      
      updateSelectInput(session, "var_to_categorize", choices = numeric_vars)
      updateSelectInput(session, "desc_var", choices = numeric_vars)
      updateSelectInput(session, "norm_var", choices = numeric_vars)
      updateSelectInput(session, "homo_numeric_var", choices = numeric_vars)
      updateSelectInput(session, "x_var", choices = numeric_vars)
      updateSelectInput(session, "y_var", choices = numeric_vars)
      updateSelectInput(session, "color_var", choices = all_vars)
      updateSelectInput(session, "map_var", choices = numeric_vars)
      updateSelectInput(session, "outlier_var", choices = numeric_vars)
      updateCheckboxGroupInput(session, "pair_vars", choices = numeric_vars[1:min(6, length(numeric_vars))])
      
      # For regression
      updateSelectInput(session, "reg_dependent", choices = numeric_vars)
      updateCheckboxGroupInput(session, "reg_predictors", choices = numeric_vars)
      
      # For ANOVA
      updateSelectInput(session, "anova_dep_var", choices = numeric_vars)
      updateSelectInput(session, "anova2_dep_var", choices = numeric_vars)
      
      # For inferential tests
      updateSelectInput(session, "var_test_var", choices = numeric_vars)
      updateSelectInput(session, "ttest_var", choices = numeric_vars)
    }
  })
  
  # DATA OVERVIEW
  output$data_overview <- DT::renderDataTable({
    req(values$current_data)
    DT::datatable(values$current_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data_summary <- renderPrint({
    req(values$current_data)
    summary(values$current_data)
  })
  
  output$data_structure <- renderPrint({
    req(values$current_data)
    str(values$current_data)
  })
  
  output$missing_plot <- renderPlot({
    req(values$current_data)
    VIM::aggr(values$current_data, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE)
  })
  
  # CATEGORIZATION
  observeEvent(input$create_categorical, {
    req(input$var_to_categorize, input$n_categories, input$category_labels)
    
    labels <- trimws(strsplit(input$category_labels, ",")[[1]])
    if (length(labels) != input$n_categories) {
      showNotification("Jumlah label harus sama dengan jumlah kategori!", type = "error")
      return()
    }
    
    var_data <- values$current_data[[input$var_to_categorize]]
    categorical_var <- create_categorical(var_data, breaks = input$n_categories, labels = labels)
    
    new_var_name <- paste0(input$var_to_categorize, "_cat")
    values$current_data[[new_var_name]] <- categorical_var
    
    # Update categorical choices
    cat_vars <- names(values$current_data)[sapply(values$current_data, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "group_var_desc", choices = cat_vars)
    updateSelectInput(session, "group_var_norm", choices = cat_vars)
    updateSelectInput(session, "homo_group_var", choices = cat_vars)
    updateSelectInput(session, "prop_var", choices = cat_vars)
    updateSelectInput(session, "var_test_group", choices = cat_vars)
    updateSelectInput(session, "ttest_group", choices = cat_vars)
    updateSelectInput(session, "anova_indep_var", choices = cat_vars)
    updateSelectInput(session, "anova2_factor1", choices = cat_vars)
    updateSelectInput(session, "anova2_factor2", choices = cat_vars)
    
    showNotification("Kategorisasi berhasil dibuat!", type = "success")
  })
  
  output$categorization_plot <- renderPlot({
    req(input$var_to_categorize)
    if (!is.null(values$current_data)) {
      var_name <- input$var_to_categorize
      cat_var_name <- paste0(var_name, "_cat")
      
      if (cat_var_name %in% names(values$current_data)) {
        p1 <- ggplot(values$current_data, aes_string(x = var_name)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          labs(title = "Distribusi Asli", x = var_name) +
          theme_minimal()
        
        p2 <- ggplot(values$current_data, aes_string(x = cat_var_name)) +
          geom_bar(fill = "coral", alpha = 0.7) +
          labs(title = "Distribusi Kategorik", x = cat_var_name) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        grid.arrange(p1, p2, ncol = 2)
      }
    }
  })
  
  output$category_table <- DT::renderDataTable({
    req(input$var_to_categorize)
    cat_var_name <- paste0(input$var_to_categorize, "_cat")
    if (cat_var_name %in% names(values$current_data)) {
      freq_table <- table(values$current_data[[cat_var_name]])
      prop_table <- prop.table(freq_table)
      
      result <- data.frame(
        Kategori = names(freq_table),
        Frekuensi = as.numeric(freq_table),
        Proporsi = round(as.numeric(prop_table), 4),
        Persen = round(as.numeric(prop_table) * 100, 2)
      )
      
      DT::datatable(result, options = list(dom = 't'))
    }
  })
  
  # OUTLIER DETECTION
  observeEvent(input$detect_outliers, {
    req(input$outlier_var, input$outlier_method)
    
    x <- values$current_data[[input$outlier_var]]
    x <- x[!is.na(x)]
    
    if (input$outlier_method == "iqr") {
      Q1 <- quantile(x, 0.25)
      Q3 <- quantile(x, 0.75)
      IQR <- Q3 - Q1
      outliers <- which(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))
    } else if (input$outlier_method == "zscore") {
      z_scores <- abs((x - mean(x)) / sd(x))
      outliers <- which(z_scores > 3)
    } else if (input$outlier_method == "modified_z") {
      median_x <- median(x)
      mad_x <- mad(x)
      modified_z <- 0.6745 * (x - median_x) / mad_x
      outliers <- which(abs(modified_z) > 3.5)
    }
    
    values$outliers <- outliers
    showNotification(paste("Terdeteksi", length(outliers), "outliers"), type = "message")
  })
  
  output$outlier_plot <- renderPlot({
    req(input$outlier_var)
    if (!is.null(values$outliers)) {
      data_plot <- values$current_data
      data_plot$is_outlier <- seq_len(nrow(data_plot)) %in% values$outliers
      
      ggplot(data_plot, aes_string(x = "1", y = input$outlier_var, color = "is_outlier")) +
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.6) +
        scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
        labs(title = "Deteksi Outliers", y = input$outlier_var, color = "Outlier") +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
  })
  
  output$outlier_table <- DT::renderDataTable({
    req(input$outlier_var)
    if (!is.null(values$outliers) && length(values$outliers) > 0) {
      outlier_data <- values$current_data[values$outliers, ]
      DT::datatable(outlier_data, options = list(scrollX = TRUE, pageLength = 5))
    }
  })
  
  # DESCRIPTIVE STATISTICS
  output$descriptive_stats <- renderPrint({
    req(input$desc_var)
    if (input$by_group && !is.null(input$group_var_desc)) {
      by(values$current_data[[input$desc_var]], values$current_data[[input$group_var_desc]], summary)
    } else {
      summary(values$current_data[[input$desc_var]])
    }
  })
  
  output$histogram_plot <- renderPlot({
    req(input$desc_var)
    if (input$by_group && !is.null(input$group_var_desc)) {
      ggplot(values$current_data, aes_string(x = input$desc_var, fill = input$group_var_desc)) +
        geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
        facet_wrap(as.formula(paste("~", input$group_var_desc))) +
        labs(title = paste("Histogram", input$desc_var, "by", input$group_var_desc)) +
        theme_minimal()
    } else {
      ggplot(values$current_data, aes_string(x = input$desc_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        geom_density(aes(y = ..density.. * nrow(values$current_data) * diff(range(values$current_data[[input$desc_var]], na.rm = TRUE))/30), 
                    color = "red", size = 1) +
        labs(title = paste("Histogram dan Density Plot -", input$desc_var)) +
        theme_minimal()
    }
  })
  
  output$boxplot_plot <- renderPlot({
    req(input$desc_var)
    if (input$by_group && !is.null(input$group_var_desc)) {
      ggplot(values$current_data, aes_string(x = input$group_var_desc, y = input$desc_var, fill = input$group_var_desc)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste("Box Plot", input$desc_var, "by", input$group_var_desc)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(values$current_data, aes_string(x = "1", y = input$desc_var)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        labs(title = paste("Box Plot -", input$desc_var), x = "") +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
  })
  
  # ADVANCED VISUALIZATION
  observeEvent(input$generate_viz, {
    req(input$viz_type)
    
    output$advanced_plot <- renderPlot({
      if (input$viz_type == "scatter") {
        req(input$x_var, input$y_var)
        p <- ggplot(values$current_data, aes_string(x = input$x_var, y = input$y_var))
        if (!is.null(input$color_var)) {
          p <- p + aes_string(color = input$color_var)
        }
        p + geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = TRUE) +
          labs(title = paste("Scatter Plot:", input$x_var, "vs", input$y_var)) +
          theme_minimal()
      } else if (input$viz_type == "correlation") {
        numeric_data <- values$current_data[sapply(values$current_data, is.numeric)]
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
                tl.cex = 0.8, tl.col = "black", tl.srt = 45)
      } else if (input$viz_type == "pairs") {
        req(input$pair_vars)
        if (length(input$pair_vars) > 1) {
          pairs(values$current_data[input$pair_vars], 
                main = "Pair Plot - Selected Variables")
        }
      } else if (input$viz_type == "density") {
        req(input$desc_var)
        ggplot(values$current_data, aes_string(x = input$desc_var)) +
          geom_density(fill = "skyblue", alpha = 0.7) +
          labs(title = paste("Density Plot -", input$desc_var)) +
          theme_minimal()
      }
    })
  })
  
  # CLUSTERING
  observeEvent(input$perform_clustering, {
    req(input$map_var, input$cluster_method, input$n_clusters)
    
    # Prepare data for clustering
    cluster_data <- values$current_data[complete.cases(values$current_data[input$map_var]), ]
    cluster_vars <- cluster_data[sapply(cluster_data, is.numeric)]
    
    # Standardize data
    cluster_vars_scaled <- scale(cluster_vars)
    
    # Perform clustering
    if (input$cluster_method == "kmeans") {
      cluster_result <- kmeans(cluster_vars_scaled, centers = input$n_clusters, nstart = 25)
      clusters <- cluster_result$cluster
    } else if (input$cluster_method == "hclust") {
      dist_matrix <- dist(cluster_vars_scaled)
      hc <- hclust(dist_matrix)
      clusters <- cutree(hc, k = input$n_clusters)
    } else if (input$cluster_method == "pam") {
      pam_result <- pam(cluster_vars_scaled, k = input$n_clusters)
      clusters <- pam_result$clustering
    }
    
    cluster_data$cluster <- factor(clusters)
    values$cluster_results <- cluster_data
    
    showNotification("Clustering berhasil dilakukan!", type = "success")
  })
  
  output$cluster_plot <- renderPlot({
    if (!is.null(values$cluster_results)) {
      ggplot(values$cluster_results, aes_string(x = input$map_var, y = "POPULATION", color = "cluster")) +
        geom_point(alpha = 0.7, size = 2) +
        labs(title = paste("Clustering Results -", input$cluster_method),
             x = input$map_var, y = "Population", color = "Cluster") +
        theme_minimal()
    }
  })
  
  output$cluster_summary <- DT::renderDataTable({
    if (!is.null(values$cluster_results)) {
      cluster_summary <- values$cluster_results %>%
        group_by(cluster) %>%
        summarise(
          n = n(),
          mean_var = mean(get(input$map_var), na.rm = TRUE),
          mean_pop = mean(POPULATION, na.rm = TRUE),
          mean_poverty = mean(POVERTY, na.rm = TRUE)
        )
      
      DT::datatable(cluster_summary, options = list(dom = 't')) %>%
        DT::formatRound(columns = c("mean_var", "mean_pop", "mean_poverty"), digits = 2)
    }
  })
  
  # NORMALITY TESTING
  observeEvent(input$test_normality, {
    req(input$norm_var)
    
    if (input$by_group_norm && !is.null(input$group_var_norm)) {
      groups <- unique(values$current_data[[input$group_var_norm]])
      norm_results <- list()
      for (group in groups) {
        group_data <- values$current_data[values$current_data[[input$group_var_norm]] == group, ]
        norm_results[[as.character(group)]] <- perform_normality_test(group_data, input$norm_var)
      }
      values$normality_results <- norm_results
    } else {
      values$normality_results <- perform_normality_test(values$current_data, input$norm_var)
    }
  })
  
  output$normality_plot <- renderPlot({
    req(input$norm_var)
    
    if (input$by_group_norm && !is.null(input$group_var_norm)) {
      p1 <- ggplot(values$current_data, aes_string(x = input$norm_var)) +
        geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue") +
        facet_wrap(as.formula(paste("~", input$group_var_norm))) +
        labs(title = "Histogram by Group") +
        theme_minimal()
      
      p2 <- ggplot(values$current_data, aes_string(sample = input$norm_var, color = input$group_var_norm)) +
        geom_qq() + geom_qq_line() +
        facet_wrap(as.formula(paste("~", input$group_var_norm))) +
        labs(title = "Q-Q Plot by Group") +
        theme_minimal()
      
      grid.arrange(p1, p2, ncol = 1)
    } else {
      p1 <- ggplot(values$current_data, aes_string(x = input$norm_var)) +
        geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue") +
        labs(title = "Histogram") +
        theme_minimal()
      
      p2 <- ggplot(values$current_data, aes_string(sample = input$norm_var)) +
        geom_qq() + geom_qq_line() +
        labs(title = "Q-Q Plot") +
        theme_minimal()
      
      grid.arrange(p1, p2, ncol = 2)
    }
  })
  
  output$normality_results <- renderPrint({
    if (!is.null(values$normality_results)) {
      if (is.list(values$normality_results) && "method" %in% names(values$normality_results)) {
        # Single test result
        cat("Uji Normalitas:", values$normality_results$method, "\n")
        cat("Statistik:", round(values$normality_results$statistic, 4), "\n")
        cat("P-value:", formatC(values$normality_results$p.value, format = "e", digits = 4), "\n")
        cat("\nInterpretasi:\n")
        if (values$normality_results$p.value < 0.05) {
          cat("Data TIDAK mengikuti distribusi normal (p < 0.05)")
        } else {
          cat("Data mengikuti distribusi normal (p ≥ 0.05)")
        }
      } else {
        # Multiple group results
        for (group in names(values$normality_results)) {
          result <- values$normality_results[[group]]
          cat("Kelompok:", group, "\n")
          cat("Uji:", result$method, "\n")
          cat("Statistik:", round(result$statistic, 4), "\n")
          cat("P-value:", formatC(result$p.value, format = "e", digits = 4), "\n")
          cat("Interpretasi:", if(result$p.value < 0.05) "Tidak Normal" else "Normal", "\n\n")
        }
      }
    }
  })
  
  # HOMOGENEITY TESTING
  observeEvent(input$test_homogeneity, {
    req(input$homo_numeric_var, input$homo_group_var)
    
    values$homogeneity_results <- perform_homogeneity_test(
      values$current_data, input$homo_numeric_var, input$homo_group_var
    )
  })
  
  output$homogeneity_plot <- renderPlot({
    req(input$homo_numeric_var, input$homo_group_var)
    
    ggplot(values$current_data, aes_string(x = input$homo_group_var, y = input$homo_numeric_var, 
                                          fill = input$homo_group_var)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste("Box Plot:", input$homo_numeric_var, "by", input$homo_group_var)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$homogeneity_results <- renderPrint({
    if (!is.null(values$homogeneity_results)) {
      cat("Uji Homogenitas Varians (Levene Test)\n")
      cat("F-statistic:", round(values$homogeneity_results$statistic, 4), "\n")
      cat("P-value:", formatC(values$homogeneity_results$p.value, format = "e", digits = 4), "\n")
      cat("\nInterpretasi:\n")
      if (values$homogeneity_results$p.value < 0.05) {
        cat("Varians antar kelompok TIDAK homogen (p < 0.05)")
      } else {
        cat("Varians antar kelompok homogen (p ≥ 0.05)")
      }
    }
  })
  
  output$group_desc_stats <- DT::renderDataTable({
    req(input$homo_numeric_var, input$homo_group_var)
    
    group_stats <- values$current_data %>%
      group_by(!!sym(input$homo_group_var)) %>%
      summarise(
        N = n(),
        Mean = mean(!!sym(input$homo_numeric_var), na.rm = TRUE),
        SD = sd(!!sym(input$homo_numeric_var), na.rm = TRUE),
        Variance = var(!!sym(input$homo_numeric_var), na.rm = TRUE),
        Min = min(!!sym(input$homo_numeric_var), na.rm = TRUE),
        Max = max(!!sym(input$homo_numeric_var), na.rm = TRUE)
      )
    
    DT::datatable(group_stats, options = list(dom = 't')) %>%
      DT::formatRound(columns = c("Mean", "SD", "Variance"), digits = 3)
  })
  
  # PROPORTION TEST
  observeEvent(input$prop_var, {
    if (!is.null(input$prop_var)) {
      categories <- unique(values$current_data[[input$prop_var]])
      updateSelectInput(session, "prop_category", choices = categories)
    }
  })
  
  observeEvent(input$test_proportion, {
    req(input$prop_var, input$prop_category, input$prop_null)
    
    data_subset <- values$current_data[[input$prop_var]]
    success_count <- sum(data_subset == input$prop_category, na.rm = TRUE)
    total_count <- sum(!is.na(data_subset))
    
    values$proportion_results <- prop.test(
      x = success_count, 
      n = total_count,
      p = input$prop_null,
      alternative = input$prop_alternative,
      conf.level = input$confidence_level
    )
  })
  
  output$proportion_plot <- renderPlot({
    req(input$prop_var)
    
    ggplot(values$current_data, aes_string(x = input$prop_var)) +
      geom_bar(fill = "coral", alpha = 0.7) +
      labs(title = paste("Distribusi", input$prop_var)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$proportion_results <- renderPrint({
    if (!is.null(values$proportion_results)) {
      cat("Uji Proporsi\n")
      cat("H0: p =", input$prop_null, "\n")
      cat("H1: p", input$prop_alternative, input$prop_null, "\n\n")
      print(values$proportion_results)
      
      cat("\nInterpretasi:\n")
      if (values$proportion_results$p.value < 0.05) {
        cat("Proporsi berbeda signifikan dari nilai yang dihipotesiskan (p < 0.05)")
      } else {
        cat("Proporsi tidak berbeda signifikan dari nilai yang dihipotesiskan (p ≥ 0.05)")
      }
    }
  })
  
  # VARIANCE TESTS
  observeEvent(input$test_variance_one, {
    req(input$var_test_var, input$var_null)
    
    x <- values$current_data[[input$var_test_var]]
    x <- x[!is.na(x)]
    n <- length(x)
    var_sample <- var(x)
    
    # Chi-square test for variance
    chi_stat <- (n - 1) * var_sample / input$var_null
    
    if (input$var_alternative == "two.sided") {
      p_value <- 2 * min(pchisq(chi_stat, df = n-1), 1 - pchisq(chi_stat, df = n-1))
    } else if (input$var_alternative == "greater") {
      p_value <- 1 - pchisq(chi_stat, df = n-1)
    } else {
      p_value <- pchisq(chi_stat, df = n-1)
    }
    
    values$variance_one_results <- list(
      statistic = chi_stat,
      p.value = p_value,
      sample_var = var_sample,
      n = n
    )
  })
  
  output$variance_one_plot <- renderPlot({
    req(input$var_test_var)
    
    ggplot(values$current_data, aes_string(x = input$var_test_var)) +
      geom_histogram(bins = 30, fill = "lightgreen", alpha = 0.7) +
      labs(title = paste("Distribusi", input$var_test_var)) +
      theme_minimal()
  })
  
  output$variance_one_results <- renderPrint({
    if (!is.null(values$variance_one_results)) {
      cat("Uji Varians Satu Sampel (Chi-square test)\n")
      cat("H0: σ² =", input$var_null, "\n")
      cat("H1: σ²", input$var_alternative, input$var_null, "\n\n")
      cat("Sample variance:", round(values$variance_one_results$sample_var, 4), "\n")
      cat("Chi-square statistic:", round(values$variance_one_results$statistic, 4), "\n")
      cat("Degrees of freedom:", values$variance_one_results$n - 1, "\n")
      cat("P-value:", formatC(values$variance_one_results$p.value, format = "e", digits = 4), "\n")
      
      cat("\nInterpretasi:\n")
      if (values$variance_one_results$p.value < 0.05) {
        cat("Varians berbeda signifikan dari nilai yang dihipotesiskan (p < 0.05)")
      } else {
        cat("Varians tidak berbeda signifikan dari nilai yang dihipotesiskan (p ≥ 0.05)")
      }
    }
  })
  
  # F-TEST FOR TWO VARIANCES
  observeEvent(input$test_variance_two, {
    req(input$var_test_numeric, input$var_test_group)
    
    groups <- unique(values$current_data[[input$var_test_group]])
    if (length(groups) == 2) {
      group1_data <- values$current_data[values$current_data[[input$var_test_group]] == groups[1], input$var_test_numeric]
      group2_data <- values$current_data[values$current_data[[input$var_test_group]] == groups[2], input$var_test_numeric]
      
      group1_data <- group1_data[!is.na(group1_data)]
      group2_data <- group2_data[!is.na(group2_data)]
      
      values$variance_two_results <- var.test(group1_data, group2_data, alternative = input$var_test_alternative)
    }
  })
  
  output$variance_two_plot <- renderPlot({
    req(input$var_test_numeric, input$var_test_group)
    
    ggplot(values$current_data, aes_string(x = input$var_test_group, y = input$var_test_numeric, 
                                          fill = input$var_test_group)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste("Perbandingan Varians:", input$var_test_numeric)) +
      theme_minimal()
  })
  
  output$variance_two_results <- renderPrint({
    if (!is.null(values$variance_two_results)) {
      print(values$variance_two_results)
      
      cat("\nInterpretasi:\n")
      if (values$variance_two_results$p.value < 0.05) {
        cat("Varians kedua kelompok berbeda signifikan (p < 0.05)")
      } else {
        cat("Varians kedua kelompok tidak berbeda signifikan (p ≥ 0.05)")
      }
    }
  })
  
  # T-TEST
  observeEvent(input$perform_ttest, {
    req(input$ttest_type, input$ttest_var)
    
    if (input$ttest_type == "one_sample") {
      x <- values$current_data[[input$ttest_var]]
      x <- x[!is.na(x)]
      values$ttest_results <- t.test(x, mu = input$mu_null)
    } else if (input$ttest_type == "two_sample") {
      req(input$ttest_group)
      groups <- unique(values$current_data[[input$ttest_group]])
      if (length(groups) == 2) {
        group1 <- values$current_data[values$current_data[[input$ttest_group]] == groups[1], input$ttest_var]
        group2 <- values$current_data[values$current_data[[input$ttest_group]] == groups[2], input$ttest_var]
        
        group1 <- group1[!is.na(group1)]
        group2 <- group2[!is.na(group2)]
        
        values$ttest_results <- t.test(group1, group2, var.equal = input$var_equal)
      }
    } else if (input$ttest_type == "paired") {
      # For paired t-test, would need paired data structure
      showNotification("Paired t-test requires specific data structure", type = "warning")
    }
  })
  
  output$ttest_plot <- renderPlot({
    req(input$ttest_var)
    
    if (input$ttest_type == "one_sample") {
      ggplot(values$current_data, aes_string(x = input$ttest_var)) +
        geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
        geom_vline(xintercept = input$mu_null, color = "red", linetype = "dashed", size = 1) +
        labs(title = paste("Distribusi", input$ttest_var), subtitle = "Garis merah: nilai null hypothesis") +
        theme_minimal()
    } else if (input$ttest_type == "two_sample") {
      req(input$ttest_group)
      ggplot(values$current_data, aes_string(x = input$ttest_group, y = input$ttest_var, fill = input$ttest_group)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste("Perbandingan", input$ttest_var, "by", input$ttest_group)) +
        theme_minimal()
    }
  })
  
  output$ttest_results <- renderPrint({
    if (!is.null(values$ttest_results)) {
      print(values$ttest_results)
      
      cat("\nInterpretasi:\n")
      if (values$ttest_results$p.value < 0.05) {
        if (input$ttest_type == "one_sample") {
          cat("Rata-rata berbeda signifikan dari", input$mu_null, "(p < 0.05)")
        } else {
          cat("Rata-rata kedua kelompok berbeda signifikan (p < 0.05)")
        }
      } else {
        if (input$ttest_type == "one_sample") {
          cat("Rata-rata tidak berbeda signifikan dari", input$mu_null, "(p ≥ 0.05)")
        } else {
          cat("Rata-rata kedua kelompok tidak berbeda signifikan (p ≥ 0.05)")
        }
      }
    }
  })
  
  # ANOVA ONE-WAY
  observeEvent(input$perform_anova_one, {
    req(input$anova_dep_var, input$anova_indep_var)
    
    formula_str <- paste(input$anova_dep_var, "~", input$anova_indep_var)
    anova_model <- aov(as.formula(formula_str), data = values$current_data)
    values$anova_one_results <- list(
      model = anova_model,
      summary = summary(anova_model),
      formula = formula_str
    )
    
    if (input$perform_posthoc) {
      values$posthoc_results <- TukeyHSD(anova_model)
    }
  })
  
  output$anova_one_plot <- renderPlot({
    req(input$anova_dep_var, input$anova_indep_var)
    
    ggplot(values$current_data, aes_string(x = input$anova_indep_var, y = input$anova_dep_var, 
                                          fill = input$anova_indep_var)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste("ANOVA:", input$anova_dep_var, "by", input$anova_indep_var)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$anova_one_results <- renderPrint({
    if (!is.null(values$anova_one_results)) {
      cat("ANOVA Satu Arah\n")
      cat("Formula:", values$anova_one_results$formula, "\n\n")
      print(values$anova_one_results$summary)
      
      f_stat <- values$anova_one_results$summary[[1]]$`F value`[1]
      p_value <- values$anova_one_results$summary[[1]]$`Pr(>F)`[1]
      
      cat("\nInterpretasi:\n")
      if (p_value < 0.05) {
        cat("Terdapat perbedaan signifikan antar kelompok (p < 0.05)")
      } else {
        cat("Tidak terdapat perbedaan signifikan antar kelompok (p ≥ 0.05)")
      }
    }
  })
  
  output$posthoc_results <- DT::renderDataTable({
    if (!is.null(values$posthoc_results)) {
      tukey_df <- as.data.frame(values$posthoc_results[[1]])
      tukey_df$Comparison <- rownames(tukey_df)
      tukey_df <- tukey_df[, c("Comparison", "diff", "lwr", "upr", "p adj")]
      
      DT::datatable(tukey_df, options = list(pageLength = 10)) %>%
        DT::formatRound(columns = c("diff", "lwr", "upr", "p adj"), digits = 4)
    }
  })
  
  output$effect_size_one <- renderPrint({
    if (!is.null(values$anova_one_results)) {
      # Calculate eta-squared (effect size)
      ss_total <- sum(values$anova_one_results$summary[[1]]$`Sum Sq`)
      ss_between <- values$anova_one_results$summary[[1]]$`Sum Sq`[1]
      eta_squared <- ss_between / ss_total
      
      cat("Effect Size (Eta-squared):", round(eta_squared, 4), "\n")
      cat("Interpretasi:\n")
      if (eta_squared < 0.01) {
        cat("Effect size kecil (< 0.01)")
      } else if (eta_squared < 0.06) {
        cat("Effect size sedang (0.01 - 0.06)")
      } else {
        cat("Effect size besar (> 0.06)")
      }
    }
  })
  
  # ANOVA TWO-WAY
  observeEvent(input$perform_anova_two, {
    req(input$anova2_dep_var, input$anova2_factor1, input$anova2_factor2)
    
    if (input$include_interaction) {
      formula_str <- paste(input$anova2_dep_var, "~", input$anova2_factor1, "*", input$anova2_factor2)
    } else {
      formula_str <- paste(input$anova2_dep_var, "~", input$anova2_factor1, "+", input$anova2_factor2)
    }
    
    anova2_model <- aov(as.formula(formula_str), data = values$current_data)
    values$anova_two_results <- list(
      model = anova2_model,
      summary = summary(anova2_model),
      formula = formula_str
    )
  })
  
  output$anova_two_plot <- renderPlot({
    req(input$anova2_dep_var, input$anova2_factor1, input$anova2_factor2)
    
    ggplot(values$current_data, aes_string(x = input$anova2_factor1, y = input$anova2_dep_var, 
                                          color = input$anova2_factor2)) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun = mean, geom = "line", aes_string(group = input$anova2_factor2)) +
      labs(title = paste("Interaction Plot:", input$anova2_dep_var),
           x = input$anova2_factor1, color = input$anova2_factor2) +
      theme_minimal()
  })
  
  output$anova_two_results <- renderPrint({
    if (!is.null(values$anova_two_results)) {
      cat("ANOVA Dua Arah\n")
      cat("Formula:", values$anova_two_results$formula, "\n\n")
      print(values$anova_two_results$summary)
      
      cat("\nInterpretasi:\n")
      summary_table <- values$anova_two_results$summary[[1]]
      
      for (i in 1:nrow(summary_table)) {
        factor_name <- rownames(summary_table)[i]
        p_value <- summary_table$`Pr(>F)`[i]
        
        if (!is.na(p_value)) {
          if (p_value < 0.05) {
            cat(factor_name, ": Efek signifikan (p < 0.05)\n")
          } else {
            cat(factor_name, ": Efek tidak signifikan (p ≥ 0.05)\n")
          }
        }
      }
    }
  })
  
  output$effect_size_two <- renderPrint({
    if (!is.null(values$anova_two_results)) {
      summary_table <- values$anova_two_results$summary[[1]]
      ss_total <- sum(summary_table$`Sum Sq`, na.rm = TRUE)
      
      cat("Effect Sizes (Eta-squared):\n")
      for (i in 1:(nrow(summary_table)-1)) {
        factor_name <- rownames(summary_table)[i]
        ss_factor <- summary_table$`Sum Sq`[i]
        eta_squared <- ss_factor / ss_total
        cat(factor_name, ":", round(eta_squared, 4), "\n")
      }
    }
  })
  
  output$marginal_means <- DT::renderDataTable({
    req(input$anova2_dep_var, input$anova2_factor1, input$anova2_factor2)
    
    marginal_means <- values$current_data %>%
      group_by(!!sym(input$anova2_factor1), !!sym(input$anova2_factor2)) %>%
      summarise(
        Mean = mean(!!sym(input$anova2_dep_var), na.rm = TRUE),
        SD = sd(!!sym(input$anova2_dep_var), na.rm = TRUE),
        N = n(),
        .groups = 'drop'
      )
    
    DT::datatable(marginal_means, options = list(pageLength = 10)) %>%
      DT::formatRound(columns = c("Mean", "SD"), digits = 3)
  })
  
  # REGRESSION MODEL
  observeEvent(input$build_model, {
    req(input$reg_dependent, input$reg_predictors)
    
    if (input$include_interaction_reg && length(input$interaction_vars) == 2) {
      interaction_term <- paste(input$interaction_vars, collapse = ":")
      predictors <- c(input$reg_predictors, interaction_term)
    } else {
      predictors <- input$reg_predictors
    }
    
    formula_str <- paste(input$reg_dependent, "~", paste(predictors, collapse = " + "))
    
    reg_model <- lm(as.formula(formula_str), data = values$current_data)
    values$model_results <- list(
      model = reg_model,
      formula = formula_str,
      summary = summary(reg_model)
    )
    
    showNotification("Model regresi berhasil dibangun!", type = "success")
  })
  
  output$model_summary <- renderPrint({
    if (!is.null(values$model_results)) {
      cat("Model Regresi Linear Berganda\n")
      cat("Formula:", values$model_results$formula, "\n\n")
      print(values$model_results$summary)
    }
  })
  
  output$model_anova <- renderPrint({
    if (!is.null(values$model_results)) {
      cat("ANOVA untuk Model Regresi:\n")
      print(anova(values$model_results$model))
    }
  })
  
  # REGRESSION DIAGNOSTICS
  observeEvent(input$run_reg_diagnostics, {
    req(values$model_results)
    
    output$regression_diagnostics <- renderPlot({
      model <- values$model_results$model
      
      selected_plots <- input$diagnostic_plots
      n_plots <- length(selected_plots)
      
      if (n_plots > 0) {
        par(mfrow = c(ceiling(n_plots/2), 2))
        
        if ("res_fitted" %in% selected_plots) {
          plot(model, which = 1, main = "Residuals vs Fitted")
        }
        if ("qq" %in% selected_plots) {
          plot(model, which = 2, main = "Normal Q-Q")
        }
        if ("scale_location" %in% selected_plots) {
          plot(model, which = 3, main = "Scale-Location")
        }
        if ("cooks" %in% selected_plots) {
          plot(model, which = 4, main = "Cook's Distance")
        }
      }
    })
  })
  
  output$vif_table <- DT::renderDataTable({
    if (!is.null(values$model_results)) {
      model <- values$model_results$model
      
      # Calculate VIF only if there are multiple predictors
      if (length(coefficients(model)) > 2) {
        vif_values <- car::vif(model)
        vif_df <- data.frame(
          Variable = names(vif_values),
          VIF = as.numeric(vif_values),
          Interpretation = ifelse(as.numeric(vif_values) > 10, "High Multicollinearity", 
                                 ifelse(as.numeric(vif_values) > 5, "Moderate Multicollinearity", "Low Multicollinearity"))
        )
        
        DT::datatable(vif_df, options = list(dom = 't')) %>%
          DT::formatRound(columns = "VIF", digits = 3)
      }
    }
  })
  
  output$reg_normality_test <- renderPrint({
    if (!is.null(values$model_results)) {
      residuals <- residuals(values$model_results$model)
      shapiro_result <- shapiro.test(residuals)
      
      cat("Uji Normalitas Residual (Shapiro-Wilk):\n")
      cat("W-statistic:", round(shapiro_result$statistic, 4), "\n")
      cat("P-value:", formatC(shapiro_result$p.value, format = "e", digits = 4), "\n")
      cat("\nInterpretasi:\n")
      if (shapiro_result$p.value < 0.05) {
        cat("Residual TIDAK normal (p < 0.05)")
      } else {
        cat("Residual normal (p ≥ 0.05)")
      }
    }
  })
  
  output$homoscedasticity_test <- renderPrint({
    if (!is.null(values$model_results)) {
      # Breusch-Pagan test for homoscedasticity
      bp_test <- car::ncvTest(values$model_results$model)
      
      cat("Uji Homoskedastisitas (Breusch-Pagan):\n")
      cat("Chi-square statistic:", round(bp_test$ChiSquare, 4), "\n")
      cat("P-value:", formatC(bp_test$p, format = "e", digits = 4), "\n")
      cat("\nInterpretasi:\n")
      if (bp_test$p < 0.05) {
        cat("Terdapat heteroskedastisitas (p < 0.05)")
      } else {
        cat("Homoskedastisitas terpenuhi (p ≥ 0.05)")
      }
    }
  })
  
  # MODEL PERFORMANCE
  observeEvent(input$run_cv, {
    req(values$model_results)
    
    model <- values$model_results$model
    data_complete <- model.frame(model)
    
    # Simple cross-validation
    set.seed(123)
    folds <- cut(seq(1, nrow(data_complete)), breaks = input$cv_folds, labels = FALSE)
    
    cv_results <- list()
    for (i in 1:input$cv_folds) {
      test_indices <- which(folds == i)
      train_data <- data_complete[-test_indices, ]
      test_data <- data_complete[test_indices, ]
      
      cv_model <- lm(values$model_results$formula, data = train_data)
      predictions <- predict(cv_model, test_data)
      actual <- test_data[, 1]  # First column is dependent variable
      
      rmse <- sqrt(mean((predictions - actual)^2))
      mae <- mean(abs(predictions - actual))
      
      cv_results[[i]] <- list(rmse = rmse, mae = mae)
    }
    
    values$cv_results <- cv_results
  })
  
  output$model_performance <- renderPrint({
    if (!is.null(values$cv_results)) {
      rmse_values <- sapply(values$cv_results, function(x) x$rmse)
      mae_values <- sapply(values$cv_results, function(x) x$mae)
      
      cat("Cross-Validation Results (", input$cv_folds, "-fold):\n")
      cat("Mean RMSE:", round(mean(rmse_values), 4), "± ", round(sd(rmse_values), 4), "\n")
      cat("Mean MAE:", round(mean(mae_values), 4), "± ", round(sd(mae_values), 4), "\n")
      
      # Model R-squared
      if (!is.null(values$model_results)) {
        r_squared <- summary(values$model_results$model)$r.squared
        adj_r_squared <- summary(values$model_results$model)$adj.r.squared
        cat("\nModel Fit:\n")
        cat("R-squared:", round(r_squared, 4), "\n")
        cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")
      }
    }
  })
  
  output$prediction_plot <- renderPlot({
    if (!is.null(values$model_results)) {
      model <- values$model_results$model
      fitted_values <- fitted(model)
      actual_values <- model.frame(model)[, 1]
      
      plot_data <- data.frame(
        Actual = actual_values,
        Predicted = fitted_values
      )
      
      ggplot(plot_data, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.6) +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "Predicted vs Actual Values", 
             x = "Actual Values", y = "Predicted Values") +
        theme_minimal()
    }
  })
  
  output$residual_analysis <- renderPlot({
    if (!is.null(values$model_results)) {
      model <- values$model_results$model
      residuals_val <- residuals(model)
      fitted_val <- fitted(model)
      
      plot_data <- data.frame(
        Fitted = fitted_val,
        Residuals = residuals_val
      )
      
      ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_smooth(se = FALSE, color = "blue") +
        labs(title = "Residual Analysis", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
    }
  })
  
  output$model_metrics <- renderPrint({
    if (!is.null(values$model_results)) {
      model <- values$model_results$model
      summary_model <- summary(model)
      
      cat("Model Performance Metrics:\n")
      cat("Multiple R-squared:", round(summary_model$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(summary_model$adj.r.squared, 4), "\n")
      cat("F-statistic:", round(summary_model$fstatistic[1], 4), "\n")
      cat("P-value:", formatC(pf(summary_model$fstatistic[1], 
                               summary_model$fstatistic[2], 
                               summary_model$fstatistic[3], 
                               lower.tail = FALSE), format = "e", digits = 4), "\n")
      cat("Residual Standard Error:", round(summary_model$sigma, 4), "\n")
      cat("Degrees of Freedom:", summary_model$df[2], "\n")
    }
  })
  
  # COEFFICIENT TABLE
  output$coefficient_table <- DT::renderDataTable({
    if (!is.null(values$model_results)) {
      coeff_summary <- summary(values$model_results$model)$coefficients
      coeff_df <- data.frame(
        Variable = rownames(coeff_summary),
        Estimate = coeff_summary[, "Estimate"],
        Std_Error = coeff_summary[, "Std. Error"],
        t_value = coeff_summary[, "t value"],
        p_value = coeff_summary[, "Pr(>|t|)"],
        Significance = ifelse(coeff_summary[, "Pr(>|t|)"] < 0.001, "***",
                             ifelse(coeff_summary[, "Pr(>|t|)"] < 0.01, "**",
                                   ifelse(coeff_summary[, "Pr(>|t|)"] < 0.05, "*", "")))
      )
      
      DT::datatable(coeff_df, options = list(pageLength = 15)) %>%
        DT::formatRound(columns = c("Estimate", "Std_Error", "t_value", "p_value"), digits = 4)
    }
  })
  
  output$confidence_intervals <- DT::renderDataTable({
    if (!is.null(values$model_results)) {
      ci_95 <- confint(values$model_results$model, level = 0.95)
      ci_df <- data.frame(
        Variable = rownames(ci_95),
        Lower_95 = ci_95[, 1],
        Upper_95 = ci_95[, 2],
        Width = ci_95[, 2] - ci_95[, 1]
      )
      
      DT::datatable(ci_df, options = list(pageLength = 15)) %>%
        DT::formatRound(columns = c("Lower_95", "Upper_95", "Width"), digits = 4)
    }
  })
  
  # DOWNLOAD HANDLERS
  output$download_original <- downloadHandler(
    filename = function() {
      paste("sovi_data_original_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$original_data, file, row.names = FALSE)
    }
  )
  
  output$download_categorized <- downloadHandler(
    filename = function() {
      paste("sovi_data_categorized_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$current_data, file, row.names = FALSE)
    }
  )
  
  output$download_descriptive <- downloadHandler(
    filename = function() {
      paste("descriptive_statistics_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create a simple report
      pdf(file)
      if (!is.null(input$desc_var)) {
        hist(values$current_data[[input$desc_var]], main = paste("Histogram of", input$desc_var))
        boxplot(values$current_data[[input$desc_var]], main = paste("Boxplot of", input$desc_var))
      }
      dev.off()
    }
  )
  
  # Additional download handlers for other analyses would follow similar pattern
  
  # Show notifications for missing functionality
  observe({
    # Update interaction variables choices when predictors change
    if (!is.null(input$reg_predictors)) {
      updateSelectInput(session, "interaction_vars", choices = input$reg_predictors)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)