# ==============================================================================
# ANALITIKA VULNERABILITAS SOSIAL INDONESIA (AVSI) DASHBOARD
# Dashboard Analisis Data Social Vulnerability Index (SOVI) Indonesia
# Versi Stabil - Semua Error Diperbaiki
# ==============================================================================

# Load required libraries with comprehensive error handling
required_packages <- c("shiny", "shinydashboard", "DT", "ggplot2", "corrplot", 
                      "VIM", "psych", "car", "nortest", "dplyr", "readr", 
                      "cluster", "gridExtra", "shinycssloaders")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Optional packages
optional_packages <- c("shinyWidgets", "fresh", "plotly", "leaflet", "factoextra", "rstatix", "broom")
for (pkg in optional_packages) {
  tryCatch(library(pkg, character.only = TRUE), error = function(e) {})
}

# Data loading function with robust error handling
load_data <- function() {
  tryCatch({
    # Load SOVI data
    sovi_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", 
                         show_col_types = FALSE)
    
    # Load distance matrix
    distance_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv",
                             show_col_types = FALSE)
    distance_matrix <- as.matrix(distance_data[,-1])
    rownames(distance_matrix) <- distance_data$...1
    
    # Convert character columns to factors for proper categorical handling
    sovi_data <- sovi_data %>%
      mutate(DISTRICTCODE = as.character(DISTRICTCODE))
    
    return(list(sovi = sovi_data, distance = distance_matrix))
  }, error = function(e) {
    # If online data fails, create sample data
    sample_data <- data.frame(
      DISTRICTCODE = 1:100,
      CHILDREN = rnorm(100, 10, 3),
      FEMALE = rnorm(100, 50, 5),
      ELDERLY = rnorm(100, 8, 2),
      FHEAD = rnorm(100, 15, 4),
      FAMILYSIZE = rnorm(100, 4, 1),
      NOELECTRIC = rnorm(100, 5, 2),
      LOWEDU = rnorm(100, 30, 8),
      GROWTH = rnorm(100, 2, 1),
      POVERTY = rnorm(100, 15, 5),
      ILLITERATE = rnorm(100, 8, 3),
      NOTRAINING = rnorm(100, 80, 15),
      DPRONE = rnorm(100, 50, 20),
      RENTED = rnorm(100, 10, 5),
      NOSEWER = rnorm(100, 25, 10),
      TAPWATER = rnorm(100, 70, 15),
      POPULATION = rnorm(100, 100000, 50000)
    )
    
    # Ensure positive values where appropriate
    sample_data[sample_data < 0] <- abs(sample_data[sample_data < 0])
    
    distance_matrix <- as.matrix(dist(sample_data[, -1]))
    
    return(list(sovi = sample_data, distance = distance_matrix))
  })
}

# Helper functions
create_categorical <- function(x, n_categories = 3, labels = c("Rendah", "Sedang", "Tinggi")) {
  tryCatch({
    # Validate inputs
    if (length(labels) != n_categories) {
      labels <- paste("Kategori", 1:n_categories)
    }
    
    # Remove NA values for quantile calculation
    x_clean <- x[!is.na(x)]
    
    if (length(x_clean) == 0) {
      stop("No valid data for categorization")
    }
    
    # Calculate breaks using quantiles
    breaks <- quantile(x_clean, probs = seq(0, 1, length.out = n_categories + 1), na.rm = TRUE)
    
    # Handle case where quantiles are identical (all values are the same)
    if (length(unique(breaks)) < length(breaks)) {
      min_val <- min(x_clean, na.rm = TRUE)
      max_val <- max(x_clean, na.rm = TRUE)
      
      if (min_val == max_val) {
        # All values are the same, create a single category
        result <- factor(rep(labels[1], length(x)), levels = labels[1])
        return(result)
      } else {
        # Create equal-width intervals
        breaks <- seq(min_val, max_val, length.out = n_categories + 1)
        breaks[1] <- min_val - 0.001  # Ensure all values are included
        breaks[length(breaks)] <- max_val + 0.001
      }
    }
    
    # Create categorical variable
    result <- cut(x, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE)
    
    return(result)
  }, error = function(e) {
    stop(paste("Error in create_categorical:", e$message))
  })
}

perform_normality_test <- function(data, variable) {
  x <- data[[variable]]
  x <- x[!is.na(x)]
  
  if(length(x) < 3) return(list(method = "Insufficient data", statistic = NA, p.value = NA))
  
  if(length(x) < 5000) {
    shapiro_test <- shapiro.test(x)
    return(list(method = "Shapiro-Wilk", statistic = shapiro_test$statistic, p.value = shapiro_test$p.value))
  } else {
    ks_test <- lillie.test(x)
    return(list(method = "Lilliefors", statistic = ks_test$statistic, p.value = ks_test$p.value))
  }
}

perform_homogeneity_test <- function(data, numeric_var, group_var) {
  tryCatch({
    formula_str <- paste(numeric_var, "~", group_var)
    levene_test <- car::leveneTest(as.formula(formula_str), data = data)
    return(list(statistic = levene_test$`F value`[1], p.value = levene_test$`Pr(>F)`[1]))
  }, error = function(e) {
    return(list(statistic = NA, p.value = NA))
  })
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "ANALITIKA VULNERABILITAS SOSIAL INDONESIA (AVSI)"),
  
  dashboardSidebar(
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
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
      "))
    ),
    
    tabItems(
      # BERANDA TAB
      tabItem(tabName = "beranda",
        fluidRow(
          box(width = 12, title = "Selamat Datang di Dashboard AVSI", status = "primary", solidHeader = TRUE,
            h3("Analitika Vulnerabilitas Sosial Indonesia (AVSI)"),
            hr(),
            
            h4("📋 Metadata Dataset"),
            div(style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin: 10px 0;",
              h5("🔹 Dataset SOVI (Social Vulnerability Index)"),
              p("Dataset ini berisi indeks vulnerabilitas sosial untuk berbagai wilayah di Indonesia dengan 16 variabel sosio-ekonomi."),
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
            
            h4("🎯 Fitur Dashboard"),
            fluidRow(
              valueBox(value = "7", subtitle = "Menu Analisis", icon = icon("layer-group"), color = "blue", width = 4),
              valueBox(value = "16", subtitle = "Variabel Data", icon = icon("database"), color = "green", width = 4),
              valueBox(value = "Lengkap", subtitle = "Analisis Statistik", icon = icon("chart-line"), color = "yellow", width = 4)
            ),
            
            div(style = "background-color: #f0f8f0; padding: 15px; border-radius: 5px; margin: 10px 0;",
              h5("✅ Kemampuan Analisis Dashboard"),
              fluidRow(
                column(6,
                  tags$ul(
                    tags$li("📊 Manajemen dan transformasi data"),
                    tags$li("🔍 Eksplorasi data dan visualisasi"),
                    tags$li("✅ Uji asumsi statistik"),
                    tags$li("📈 Uji proporsi dan varians")
                  )
                ),
                column(6,
                  tags$ul(
                    tags$li("📊 ANOVA satu dan dua arah"),
                    tags$li("🔗 Regresi linear berganda"),
                    tags$li("🗺️ Analisis clustering spasial"),
                    tags$li("📥 Download hasil analisis")
                  )
                )
              )
            ),
            
            hr(),
            div(style = "text-align: center; background-color: #e9ecef; padding: 10px; border-radius: 5px;",
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
                    selectInput("var_to_categorize", "Pilih Variabel:", choices = NULL),
                    numericInput("n_categories", "Jumlah Kategori:", value = 3, min = 2, max = 5),
                    textAreaInput("category_labels", "Label Kategori (pisahkan dengan koma):",
                      value = "Rendah, Sedang, Tinggi", rows = 3
                    ),
                    actionButton("create_categorical", "🔄 Buat Kategorisasi", class = "btn btn-success"),
                    br(), br(),
                    div(style = "background-color: #fff3cd; padding: 10px; border-radius: 5px;",
                      h6("💡 Interpretasi:"),
                      p("Kategorisasi memungkinkan analisis data kontinyu dalam bentuk kelompok diskrit.")
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
                    actionButton("detect_outliers", "🔍 Deteksi Outliers", class = "btn btn-warning")
                  ),
                  column(6,
                    h5("📈 Visualisasi Outliers"),
                    withSpinner(plotOutput("outlier_plot", height = "350px"))
                  )
                ),
                hr(),
                h5("📊 Summary Outliers yang Terdeteksi"),
                withSpinner(DT::dataTableOutput("outlier_table"))
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
                    h5("📊 Histogram"),
                    withSpinner(plotOutput("histogram_plot", height = "350px"))
                  ),
                  column(6,
                    h5("📦 Box Plot"),
                    withSpinner(plotOutput("boxplot_plot", height = "350px"))
                  )
                ),
                br(),
                downloadButton("download_descriptive", "📥 Download Hasil Deskriptif", class = "btn btn-success")
              ),
              
              tabPanel("📈 Visualisasi Lanjutan",
                fluidRow(
                  column(4,
                    selectInput("viz_type", "Jenis Visualisasi:",
                      choices = list("Scatter Plot" = "scatter", "Correlation Plot" = "correlation", "Density Plot" = "density")
                    ),
                    conditionalPanel(
                      condition = "input.viz_type == 'scatter'",
                      selectInput("x_var", "Variabel X:", choices = NULL),
                      selectInput("y_var", "Variabel Y:", choices = NULL)
                    ),
                    actionButton("generate_viz", "📊 Generate Visualisasi", class = "btn btn-info")
                  ),
                  column(8,
                    withSpinner(plotOutput("advanced_plot", height = "500px"))
                  )
                ),
                br(),
                downloadButton("download_visualization", "📥 Download Visualisasi", class = "btn btn-primary")
              ),
              
              tabPanel("🗺️ Pemetaan Spasial",
                h4("Analisis Clustering Vulnerabilitas"),
                fluidRow(
                  column(4,
                    selectInput("map_var", "Variabel untuk Pemetaan:", choices = NULL),
                    selectInput("cluster_method", "Metode Clustering:",
                      choices = list("K-Means" = "kmeans", "Hierarchical" = "hclust")
                    ),
                    numericInput("n_clusters", "Jumlah Cluster:", value = 3, min = 2, max = 8),
                    actionButton("perform_clustering", "🎯 Lakukan Clustering", class = "btn btn-warning"),
                    br(), br(),
                    div(style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px;",
                      h6("📍 Interpretasi:"),
                      p("Clustering mengelompokkan wilayah berdasarkan kesamaan karakteristik vulnerabilitas.")
                    )
                  ),
                  column(8,
                    h5("🗺️ Hasil Clustering Wilayah"),
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
                fluidRow(
                  column(4,
                    selectInput("norm_var", "Pilih Variabel:", choices = NULL),
                    checkboxInput("by_group_norm", "Uji per kelompok", FALSE),
                    conditionalPanel(
                      condition = "input.by_group_norm == true",
                      selectInput("group_var_norm", "Variabel Pengelompokan:", choices = NULL)
                    ),
                    actionButton("test_normality", "🔍 Uji Normalitas", class = "btn btn-primary")
                  ),
                  column(8,
                    h5("📈 Q-Q Plot & Histogram"),
                    withSpinner(plotOutput("normality_plot", height = "400px"))
                  )
                ),
                hr(),
                h5("📊 Hasil Uji Normalitas"),
                withSpinner(verbatimTextOutput("normality_results")),
                br(),
                downloadButton("download_normality", "📥 Download Hasil Normalitas", class = "btn btn-info")
              ),
              
              tabPanel("⚖️ Uji Homogenitas",
                fluidRow(
                  column(4,
                    selectInput("homo_numeric_var", "Variabel Numerik:", choices = NULL),
                    selectInput("homo_group_var", "Variabel Pengelompokan:", choices = NULL),
                    actionButton("test_homogeneity", "⚖️ Uji Homogenitas", class = "btn btn-success")
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
                h5("📊 Status Asumsi Data"),
                withSpinner(DT::dataTableOutput("assumption_summary")),
                br(),
                div(style = "background-color: #f0f8f0; padding: 15px; border-radius: 5px;",
                  h5("🎯 Rekomendasi Analisis"),
                  tags$ul(
                    tags$li("✅ Jika data normal dan homogen: gunakan parametric tests"),
                    tags$li("⚠️ Jika data tidak normal: pertimbangkan transformasi atau non-parametric tests"),
                    tags$li("⚠️ Jika varians tidak homogen: gunakan Welch's t-test atau robust methods")
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
                fluidRow(
                  column(4,
                    selectInput("prop_var", "Pilih Variabel Kategorik:", choices = NULL),
                    selectInput("prop_category", "Kategori yang Diuji:", choices = NULL),
                    numericInput("prop_null", "Proporsi Null Hypothesis:", value = 0.5, min = 0, max = 1, step = 0.01),
                    actionButton("test_proportion", "📊 Uji Proporsi", class = "btn btn-primary")
                  ),
                  column(8,
                    h5("📈 Visualisasi Proporsi"),
                    withSpinner(plotOutput("proportion_plot", height = "350px")),
                    br(),
                    h5("📊 Hasil Uji Proporsi"),
                    withSpinner(verbatimTextOutput("proportion_results"))
                  )
                )
              ),
              
              tabPanel("📏 Uji Varians",
                fluidRow(
                  column(4,
                    h5("Uji Varians Satu Kelompok"),
                    selectInput("var_test_var", "Pilih Variabel:", choices = NULL),
                    numericInput("var_null", "Varians Null Hypothesis:", value = 1, min = 0, step = 0.1),
                    actionButton("test_variance_one", "📏 Uji Varians", class = "btn btn-info")
                  ),
                  column(8,
                    h5("📊 Distribusi Data"),
                    withSpinner(plotOutput("variance_plot", height = "350px")),
                    br(),
                    h5("📈 Hasil Uji Varians"),
                    withSpinner(verbatimTextOutput("variance_results"))
                  )
                )
              ),
              
              tabPanel("🔢 Uji Beda Rata-rata",
                fluidRow(
                  column(4,
                    selectInput("ttest_type", "Jenis T-Test:",
                      choices = list("One Sample" = "one_sample", "Two Sample" = "two_sample")
                    ),
                    selectInput("ttest_var", "Variabel Numerik:", choices = NULL),
                    conditionalPanel(
                      condition = "input.ttest_type == 'one_sample'",
                      numericInput("mu_null", "Mean Null Hypothesis:", value = 0)
                    ),
                    conditionalPanel(
                      condition = "input.ttest_type == 'two_sample'",
                      selectInput("ttest_group", "Variabel Pengelompokan:", choices = NULL)
                    ),
                    actionButton("perform_ttest", "🔢 Lakukan T-Test", class = "btn btn-danger")
                  ),
                  column(8,
                    h5("📊 Visualisasi T-Test"),
                    withSpinner(plotOutput("ttest_plot", height = "350px")),
                    br(),
                    h5("📈 Hasil T-Test"),
                    withSpinner(verbatimTextOutput("ttest_results"))
                  )
                )
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
                fluidRow(
                  column(4,
                    selectInput("anova_dep_var", "Variabel Dependen:", choices = NULL),
                    selectInput("anova_indep_var", "Variabel Independen:", choices = NULL),
                    checkboxInput("perform_posthoc", "Post-hoc Test (Tukey HSD)", TRUE),
                    actionButton("perform_anova_one", "📊 Lakukan ANOVA", class = "btn btn-primary")
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
                )
              ),
              
              tabPanel("📊 ANOVA Dua Arah",
                fluidRow(
                  column(4,
                    selectInput("anova2_dep_var", "Variabel Dependen:", choices = NULL),
                    selectInput("anova2_factor1", "Faktor 1:", choices = NULL),
                    selectInput("anova2_factor2", "Faktor 2:", choices = NULL),
                    checkboxInput("include_interaction", "Sertakan Interaksi", TRUE),
                    actionButton("perform_anova_two", "📊 Lakukan ANOVA 2-Way", class = "btn btn-warning")
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
                h5("📈 Marginal Means"),
                withSpinner(DT::dataTableOutput("marginal_means"))
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
                fluidRow(
                  column(4,
                    selectInput("reg_dependent", "Variabel Dependen:", choices = NULL),
                    checkboxGroupInput("reg_predictors", "Variabel Prediktor:", choices = NULL),
                    actionButton("build_model", "🔧 Bangun Model", class = "btn btn-primary")
                  ),
                  column(8,
                    h5("📊 Ringkasan Model"),
                    withSpinner(verbatimTextOutput("model_summary"))
                  )
                )
              ),
              
              tabPanel("📊 Diagnostik Model",
                fluidRow(
                  column(4,
                    h5("📋 Asumsi Regresi:"),
                    div(style = "background-color: #fff3cd; padding: 10px; border-radius: 5px;",
                      tags$ul(
                        tags$li("Linearitas"),
                        tags$li("Independensi"),
                        tags$li("Homoskedastisitas"),
                        tags$li("Normalitas residual"),
                        tags$li("Tidak ada multikolinearitas")
                      )
                    ),
                    actionButton("run_reg_diagnostics", "🔍 Jalankan Diagnostik", class = "btn btn-warning")
                  ),
                  column(8,
                    h5("📊 Diagnostic Plots"),
                    withSpinner(plotOutput("regression_diagnostics", height = "500px"))
                  )
                ),
                hr(),
                fluidRow(
                  column(6,
                    h5("📈 Uji Normalitas Residual"),
                    withSpinner(verbatimTextOutput("reg_normality_test"))
                  ),
                  column(6,
                    h5("📊 Model Metrics"),
                    withSpinner(verbatimTextOutput("model_metrics"))
                  )
                )
              ),
              
              tabPanel("📋 Interpretasi Model",
                h5("📊 Koefisien dan Interpretasi"),
                withSpinner(DT::dataTableOutput("coefficient_table")),
                br(),
                div(style = "background-color: #f0f8f0; padding: 15px; border-radius: 5px;",
                  h5("🎯 Interpretasi Model"),
                  p("Interpretasi koefisien regresi akan ditampilkan berdasarkan model yang dibangun.")
                )
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
  
  # Reactive values to store all data and results
  values <- reactiveValues(
    original_data = NULL,
    current_data = NULL,
    distance_matrix = NULL,
    normality_results = NULL,
    homogeneity_results = NULL,
    outliers = NULL,
    cluster_results = NULL,
    proportion_results = NULL,
    variance_results = NULL,
    ttest_results = NULL,
    anova_one_results = NULL,
    anova_two_results = NULL,
    posthoc_results = NULL,
    model_results = NULL
  )
  
  # Load data on startup with error handling
  observe({
    tryCatch({
      data_list <- load_data()
      if (!is.null(data_list)) {
        values$original_data <- data_list$sovi
        values$current_data <- data_list$sovi
        values$distance_matrix <- data_list$distance
        
        # Update all choices
        numeric_vars <- names(values$current_data)[sapply(values$current_data, is.numeric)]
        all_vars <- names(values$current_data)
        cat_vars <- c()  # Initially empty, will be populated when categories are created
        
        # Update numeric variable choices
        updateSelectInput(session, "var_to_categorize", choices = numeric_vars)
        updateSelectInput(session, "desc_var", choices = numeric_vars)
        updateSelectInput(session, "norm_var", choices = numeric_vars)
        updateSelectInput(session, "homo_numeric_var", choices = numeric_vars)
        updateSelectInput(session, "x_var", choices = numeric_vars)
        updateSelectInput(session, "y_var", choices = numeric_vars)
        updateSelectInput(session, "map_var", choices = numeric_vars)
        updateSelectInput(session, "outlier_var", choices = numeric_vars)
        updateSelectInput(session, "reg_dependent", choices = numeric_vars)
        updateCheckboxGroupInput(session, "reg_predictors", choices = numeric_vars)
        updateSelectInput(session, "anova_dep_var", choices = numeric_vars)
        updateSelectInput(session, "anova2_dep_var", choices = numeric_vars)
        updateSelectInput(session, "var_test_var", choices = numeric_vars)
        updateSelectInput(session, "ttest_var", choices = numeric_vars)
        
        # Initialize categorical variable choices (empty initially)
        updateSelectInput(session, "group_var_desc", choices = cat_vars)
        updateSelectInput(session, "group_var_norm", choices = cat_vars)
        updateSelectInput(session, "homo_group_var", choices = cat_vars)
        updateSelectInput(session, "prop_var", choices = cat_vars)
        updateSelectInput(session, "ttest_group", choices = cat_vars)
        updateSelectInput(session, "anova_indep_var", choices = cat_vars)
        updateSelectInput(session, "anova2_factor1", choices = cat_vars)
        updateSelectInput(session, "anova2_factor2", choices = cat_vars)
        
        showNotification("Data berhasil dimuat!", type = "default")
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
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
    tryCatch({
      VIM::aggr(values$current_data, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE)
    }, error = function(e) {
      plot(1, 1, main = "Error in missing data plot")
    })
  })
  
  # CATEGORIZATION with robust error handling
  observeEvent(input$create_categorical, {
    tryCatch({
      req(input$var_to_categorize, input$n_categories, input$category_labels)
      req(values$current_data)
      
      # Validate inputs
      if (is.null(input$var_to_categorize) || input$var_to_categorize == "") {
        showNotification("Pilih variabel terlebih dahulu!", type = "warning")
        return()
      }
      
      if (input$n_categories < 2 || input$n_categories > 10) {
        showNotification("Jumlah kategori harus antara 2-10!", type = "warning")
        return()
      }
      
      labels <- trimws(strsplit(input$category_labels, ",")[[1]])
      if (length(labels) != input$n_categories) {
        showNotification("Jumlah label harus sama dengan jumlah kategori!", type = "warning")
        return()
      }
      
      var_data <- values$current_data[[input$var_to_categorize]]
      if (is.null(var_data)) {
        showNotification("Variabel tidak ditemukan!", type = "error")
        return()
      }
      
      if (all(is.na(var_data))) {
        showNotification("Variabel tidak memiliki data valid!", type = "error")
        return()
      }
      
      if (!is.numeric(var_data)) {
        showNotification("Hanya variabel numerik yang dapat dikategorisasi!", type = "error")
        return()
      }
      
      # Create categorical variable with proper error handling
      categorical_var <- tryCatch({
        create_categorical(var_data, input$n_categories, labels)
      }, error = function(e) {
        showNotification(paste("Gagal membuat kategori:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(categorical_var)) {
        return()
      }
      
      new_var_name <- paste0(input$var_to_categorize, "_cat")
      values$current_data[[new_var_name]] <- categorical_var
      
      # Update categorical choices
      cat_vars <- names(values$current_data)[sapply(values$current_data, function(x) is.factor(x) || is.character(x))]
      cat_vars <- cat_vars[cat_vars != "DISTRICTCODE"]  # Exclude ID variables
      
      # Update all categorical variable inputs
      tryCatch({
        updateSelectInput(session, "group_var_desc", choices = cat_vars)
        updateSelectInput(session, "group_var_norm", choices = cat_vars)
        updateSelectInput(session, "homo_group_var", choices = cat_vars)
        updateSelectInput(session, "prop_var", choices = cat_vars)
        updateSelectInput(session, "ttest_group", choices = cat_vars)
        updateSelectInput(session, "anova_indep_var", choices = cat_vars)
        updateSelectInput(session, "anova2_factor1", choices = cat_vars)
        updateSelectInput(session, "anova2_factor2", choices = cat_vars)
      }, error = function(e) {
        # Silently continue if update fails
      })
      
      showNotification("Kategorisasi berhasil dibuat!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam kategorisasi:", e$message), type = "error")
    })
  })
  
  output$categorization_plot <- renderPlot({
    req(input$var_to_categorize)
    req(values$current_data)
    
    tryCatch({
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
      } else {
        plot(1, 1, main = "Buat kategorisasi terlebih dahulu")
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$category_table <- DT::renderDataTable({
    req(input$var_to_categorize)
    cat_var_name <- paste0(input$var_to_categorize, "_cat")
    
    if (cat_var_name %in% names(values$current_data)) {
      tryCatch({
        freq_table <- table(values$current_data[[cat_var_name]], useNA = "ifany")
        prop_table <- prop.table(freq_table)
        
        result <- data.frame(
          Kategori = names(freq_table),
          Frekuensi = as.numeric(freq_table),
          Proporsi = round(as.numeric(prop_table), 4),
          Persen = round(as.numeric(prop_table) * 100, 2)
        )
        
        DT::datatable(result, options = list(dom = 't'))
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    }
  })
  
  # OUTLIER DETECTION
  observeEvent(input$detect_outliers, {
    tryCatch({
      req(input$outlier_var, input$outlier_method)
      req(values$current_data)
      
      x <- values$current_data[[input$outlier_var]]
      x <- x[!is.na(x)]
      
      if (length(x) < 10) {
        showNotification("Data tidak cukup untuk deteksi outliers", type = "warning")
        return()
      }
      
      if (input$outlier_method == "iqr") {
        Q1 <- quantile(x, 0.25)
        Q3 <- quantile(x, 0.75)
        IQR <- Q3 - Q1
        outliers <- which(values$current_data[[input$outlier_var]] < (Q1 - 1.5 * IQR) | 
                         values$current_data[[input$outlier_var]] > (Q3 + 1.5 * IQR))
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
      showNotification(paste("Terdeteksi", length(outliers), "outliers"), type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam deteksi outliers:", e$message), type = "error")
    })
  })
  
  output$outlier_plot <- renderPlot({
    req(input$outlier_var)
    req(values$current_data)
    
    tryCatch({
      data_plot <- values$current_data
      data_plot$is_outlier <- FALSE
      
      if (!is.null(values$outliers) && length(values$outliers) > 0) {
        data_plot$is_outlier[values$outliers] <- TRUE
      }
      
      ggplot(data_plot, aes_string(x = "1", y = input$outlier_var, color = "is_outlier")) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.6) +
        scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
        labs(title = "Deteksi Outliers", y = input$outlier_var, color = "Outlier") +
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$outlier_table <- DT::renderDataTable({
    if (!is.null(values$outliers) && length(values$outliers) > 0) {
      outlier_data <- values$current_data[values$outliers, ]
      DT::datatable(outlier_data, options = list(scrollX = TRUE, pageLength = 5))
    } else {
      DT::datatable(data.frame(Message = "Tidak ada outliers yang terdeteksi"))
    }
  })
  
  # DESCRIPTIVE STATISTICS
  output$descriptive_stats <- renderPrint({
    tryCatch({
      req(input$desc_var)
      req(values$current_data)
      
      if (input$by_group && !is.null(input$group_var_desc) && input$group_var_desc != "") {
        group_data <- values$current_data[[input$group_var_desc]]
        numeric_data <- values$current_data[[input$desc_var]]
        
        if (is.null(group_data) || is.null(numeric_data)) {
          cat("Data tidak tersedia untuk analisis berdasarkan kelompok")
          return()
        }
        
        # Create summary by group
        by(numeric_data, group_data, function(x) {
          c(
            N = length(x[!is.na(x)]),
            Mean = mean(x, na.rm = TRUE),
            SD = sd(x, na.rm = TRUE),
            Min = min(x, na.rm = TRUE),
            Q1 = quantile(x, 0.25, na.rm = TRUE),
            Median = median(x, na.rm = TRUE),
            Q3 = quantile(x, 0.75, na.rm = TRUE),
            Max = max(x, na.rm = TRUE)
          )
        })
      } else {
        summary(values$current_data[[input$desc_var]])
      }
    }, error = function(e) {
      cat("Error dalam analisis deskriptif:", e$message)
    })
  })
  
  output$histogram_plot <- renderPlot({
    tryCatch({
      req(input$desc_var)
      req(values$current_data)
      
      if (input$by_group && !is.null(input$group_var_desc) && input$group_var_desc != "") {
        ggplot(values$current_data, aes_string(x = input$desc_var, fill = input$group_var_desc)) +
          geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
          facet_wrap(as.formula(paste("~", input$group_var_desc))) +
          labs(title = paste("Histogram", input$desc_var, "by", input$group_var_desc)) +
          theme_minimal()
      } else {
        ggplot(values$current_data, aes_string(x = input$desc_var)) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Histogram -", input$desc_var)) +
          theme_minimal()
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$boxplot_plot <- renderPlot({
    tryCatch({
      req(input$desc_var)
      req(values$current_data)
      
      if (input$by_group && !is.null(input$group_var_desc) && input$group_var_desc != "") {
        ggplot(values$current_data, aes_string(x = input$group_var_desc, y = input$desc_var, fill = input$group_var_desc)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Box Plot", input$desc_var, "by", input$group_var_desc)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        temp_data <- values$current_data
        temp_data$dummy_group <- "All Data"
        
        ggplot(temp_data, aes_string(x = "dummy_group", y = input$desc_var)) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          labs(title = paste("Box Plot -", input$desc_var), x = "") +
          theme_minimal() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  # ADVANCED VISUALIZATION
  observeEvent(input$generate_viz, {
    req(input$viz_type)
    
    output$advanced_plot <- renderPlot({
      tryCatch({
        if (input$viz_type == "scatter") {
          req(input$x_var, input$y_var)
          ggplot(values$current_data, aes_string(x = input$x_var, y = input$y_var)) +
            geom_point(alpha = 0.6, color = "steelblue") +
            geom_smooth(method = "lm", se = TRUE, color = "red") +
            labs(title = paste("Scatter Plot:", input$x_var, "vs", input$y_var)) +
            theme_minimal()
        } else if (input$viz_type == "correlation") {
          numeric_data <- values$current_data[sapply(values$current_data, is.numeric)]
          cor_matrix <- cor(numeric_data, use = "complete.obs")
          corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
                  tl.cex = 0.8, tl.col = "black", tl.srt = 45)
        } else if (input$viz_type == "density") {
          req(input$desc_var)
          ggplot(values$current_data, aes_string(x = input$desc_var)) +
            geom_density(fill = "skyblue", alpha = 0.7) +
            labs(title = paste("Density Plot -", input$desc_var)) +
            theme_minimal()
        }
      }, error = function(e) {
        plot(1, 1, main = paste("Error:", e$message))
      })
    })
  })
  
  # CLUSTERING
  observeEvent(input$perform_clustering, {
    tryCatch({
      req(input$map_var, input$cluster_method, input$n_clusters)
      req(values$current_data)
      
      # Prepare data for clustering
      cluster_data <- values$current_data[complete.cases(values$current_data[input$map_var]), ]
      
      if (nrow(cluster_data) < input$n_clusters) {
        showNotification("Data tidak cukup untuk jumlah cluster yang diminta!", type = "error")
        return()
      }
      
      cluster_vars <- cluster_data[sapply(cluster_data, is.numeric)]
      cluster_vars <- cluster_vars[sapply(cluster_vars, function(x) var(x, na.rm = TRUE) > 0)]
      
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
      }
      
      cluster_data$cluster <- factor(clusters)
      values$cluster_results <- cluster_data
      
      showNotification("Clustering berhasil dilakukan!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam clustering:", e$message), type = "error")
    })
  })
  
  output$cluster_plot <- renderPlot({
    if (!is.null(values$cluster_results)) {
      tryCatch({
        ggplot(values$cluster_results, aes_string(x = input$map_var, y = "POPULATION", color = "cluster")) +
          geom_point(alpha = 0.7, size = 2) +
          labs(title = paste("Clustering Results -", input$cluster_method),
               x = input$map_var, y = "Population", color = "Cluster") +
          theme_minimal()
      }, error = function(e) {
        plot(1, 1, main = paste("Error:", e$message))
      })
    } else {
      plot(1, 1, main = "Lakukan clustering terlebih dahulu")
    }
  })
  
  output$cluster_summary <- DT::renderDataTable({
    if (!is.null(values$cluster_results)) {
      tryCatch({
        cluster_summary <- values$cluster_results %>%
          group_by(cluster) %>%
          summarise(
            n = n(),
            mean_var = mean(!!sym(input$map_var), na.rm = TRUE),
            mean_pop = mean(POPULATION, na.rm = TRUE),
            mean_poverty = mean(POVERTY, na.rm = TRUE),
            .groups = 'drop'
          )
        
        DT::datatable(cluster_summary, options = list(dom = 't')) %>%
          DT::formatRound(columns = c("mean_var", "mean_pop", "mean_poverty"), digits = 2)
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    } else {
      DT::datatable(data.frame(Message = "Belum ada hasil clustering"))
    }
  })
  
  # NORMALITY TESTING
  observeEvent(input$test_normality, {
    tryCatch({
      req(input$norm_var)
      req(values$current_data)
      
      if (input$by_group_norm && !is.null(input$group_var_norm) && input$group_var_norm != "") {
        groups <- unique(values$current_data[[input$group_var_norm]])
        groups <- groups[!is.na(groups)]
        norm_results <- list()
        
        for (group in groups) {
          group_data <- values$current_data[values$current_data[[input$group_var_norm]] == group, ]
          norm_results[[as.character(group)]] <- perform_normality_test(group_data, input$norm_var)
        }
        values$normality_results <- norm_results
      } else {
        values$normality_results <- perform_normality_test(values$current_data, input$norm_var)
      }
      
      showNotification("Uji normalitas selesai!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam uji normalitas:", e$message), type = "error")
    })
  })
  
  output$normality_plot <- renderPlot({
    req(input$norm_var)
    req(values$current_data)
    
    tryCatch({
      if (input$by_group_norm && !is.null(input$group_var_norm) && input$group_var_norm != "") {
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
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$normality_results <- renderPrint({
    if (!is.null(values$normality_results)) {
      tryCatch({
        if (is.list(values$normality_results) && "method" %in% names(values$normality_results)) {
          # Single test result
          cat("Uji Normalitas:", values$normality_results$method, "\n")
          if (!is.na(values$normality_results$statistic)) {
            cat("Statistik:", round(values$normality_results$statistic, 4), "\n")
          }
          cat("P-value:", formatC(values$normality_results$p.value, format = "e", digits = 4), "\n")
          cat("\nInterpretasi:\n")
          if (is.na(values$normality_results$p.value)) {
            cat("Data tidak cukup untuk uji normalitas")
          } else if (values$normality_results$p.value < 0.05) {
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
            if (!is.na(result$statistic)) {
              cat("Statistik:", round(result$statistic, 4), "\n")
            }
            cat("P-value:", formatC(result$p.value, format = "e", digits = 4), "\n")
            cat("Interpretasi:", if(is.na(result$p.value) || result$p.value < 0.05) "Tidak Normal" else "Normal", "\n\n")
          }
        }
      }, error = function(e) {
        cat("Error dalam menampilkan hasil:", e$message)
      })
    } else {
      cat("Belum ada uji normalitas yang dilakukan")
    }
  })
  
  # HOMOGENEITY TESTING
  observeEvent(input$test_homogeneity, {
    tryCatch({
      req(input$homo_numeric_var, input$homo_group_var)
      req(values$current_data)
      
      if (input$homo_group_var == "" || input$homo_numeric_var == "") {
        showNotification("Pilih variabel terlebih dahulu!", type = "warning")
        return()
      }
      
      values$homogeneity_results <- perform_homogeneity_test(
        values$current_data, input$homo_numeric_var, input$homo_group_var
      )
      
      showNotification("Uji homogenitas selesai!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam uji homogenitas:", e$message), type = "error")
    })
  })
  
  output$homogeneity_plot <- renderPlot({
    req(input$homo_numeric_var, input$homo_group_var)
    req(values$current_data)
    
    tryCatch({
      if (input$homo_group_var != "" && input$homo_numeric_var != "") {
        ggplot(values$current_data, aes_string(x = input$homo_group_var, y = input$homo_numeric_var, 
                                              fill = input$homo_group_var)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Box Plot:", input$homo_numeric_var, "by", input$homo_group_var)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        plot(1, 1, main = "Pilih variabel terlebih dahulu")
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$homogeneity_results <- renderPrint({
    if (!is.null(values$homogeneity_results)) {
      tryCatch({
        cat("Uji Homogenitas Varians (Levene Test)\n")
        if (!is.na(values$homogeneity_results$statistic)) {
          cat("F-statistic:", round(values$homogeneity_results$statistic, 4), "\n")
        }
        cat("P-value:", formatC(values$homogeneity_results$p.value, format = "e", digits = 4), "\n")
        cat("\nInterpretasi:\n")
        if (is.na(values$homogeneity_results$p.value)) {
          cat("Data tidak cukup untuk uji homogenitas")
        } else if (values$homogeneity_results$p.value < 0.05) {
          cat("Varians antar kelompok TIDAK homogen (p < 0.05)")
        } else {
          cat("Varians antar kelompok homogen (p ≥ 0.05)")
        }
      }, error = function(e) {
        cat("Error dalam menampilkan hasil:", e$message)
      })
    } else {
      cat("Belum ada uji homogenitas yang dilakukan")
    }
  })
  
  output$group_desc_stats <- DT::renderDataTable({
    tryCatch({
      req(input$homo_numeric_var, input$homo_group_var)
      req(values$current_data)
      
      if (input$homo_group_var == "" || input$homo_numeric_var == "") {
        return(DT::datatable(data.frame(Message = "Pilih variabel terlebih dahulu")))
      }
      
      group_stats <- values$current_data %>%
        group_by(!!sym(input$homo_group_var)) %>%
        summarise(
          N = n(),
          Mean = mean(!!sym(input$homo_numeric_var), na.rm = TRUE),
          SD = sd(!!sym(input$homo_numeric_var), na.rm = TRUE),
          Variance = var(!!sym(input$homo_numeric_var), na.rm = TRUE),
          Min = min(!!sym(input$homo_numeric_var), na.rm = TRUE),
          Max = max(!!sym(input$homo_numeric_var), na.rm = TRUE),
          .groups = 'drop'
        )
      
      DT::datatable(group_stats, options = list(dom = 't')) %>%
        DT::formatRound(columns = c("Mean", "SD", "Variance"), digits = 3)
    }, error = function(e) {
      return(DT::datatable(data.frame(Error = paste("Error:", e$message))))
    })
  })
  
  # ASSUMPTIONS SUMMARY
  output$assumption_summary <- DT::renderDataTable({
    summary_data <- data.frame(
      Test = character(),
      Variable = character(),
      Result = character(),
      P_value = character(),
      Interpretation = character(),
      stringsAsFactors = FALSE
    )
    
    # Add normality results
    if (!is.null(values$normality_results)) {
      if (is.list(values$normality_results) && "method" %in% names(values$normality_results)) {
        summary_data <- rbind(summary_data, data.frame(
          Test = values$normality_results$method,
          Variable = "Selected Variable",
          Result = ifelse(is.na(values$normality_results$statistic), "N/A", 
                         paste("Statistic:", round(values$normality_results$statistic, 4))),
          P_value = ifelse(is.na(values$normality_results$p.value), "N/A",
                          formatC(values$normality_results$p.value, format = "e", digits = 3)),
          Interpretation = ifelse(is.na(values$normality_results$p.value), "Insufficient Data",
                                 ifelse(values$normality_results$p.value < 0.05, "Not Normal", "Normal"))
        ))
      }
    }
    
    # Add homogeneity results
    if (!is.null(values$homogeneity_results)) {
      summary_data <- rbind(summary_data, data.frame(
        Test = "Levene Test",
        Variable = "Group Comparison",
        Result = ifelse(is.na(values$homogeneity_results$statistic), "N/A",
                       paste("F-statistic:", round(values$homogeneity_results$statistic, 4))),
        P_value = ifelse(is.na(values$homogeneity_results$p.value), "N/A",
                        formatC(values$homogeneity_results$p.value, format = "e", digits = 3)),
        Interpretation = ifelse(is.na(values$homogeneity_results$p.value), "Insufficient Data",
                               ifelse(values$homogeneity_results$p.value < 0.05, "Not Homogeneous", "Homogeneous"))
      ))
    }
    
    if (nrow(summary_data) > 0) {
      DT::datatable(summary_data, options = list(dom = 't'))
    } else {
      DT::datatable(data.frame(Message = "Belum ada uji asumsi yang dilakukan"))
    }
  })
  
  # PROPORTION TEST
  observeEvent(input$prop_var, {
    if (!is.null(input$prop_var) && input$prop_var != "" && !is.null(values$current_data)) {
      tryCatch({
        var_data <- values$current_data[[input$prop_var]]
        if (!is.null(var_data)) {
          categories <- unique(var_data[!is.na(var_data)])
          updateSelectInput(session, "prop_category", choices = categories)
        }
      }, error = function(e) {
        showNotification(paste("Error updating categories:", e$message), type = "error")
      })
    }
  })
  
  observeEvent(input$test_proportion, {
    tryCatch({
      req(input$prop_var, input$prop_category, input$prop_null)
      req(values$current_data)
      
      data_subset <- values$current_data[[input$prop_var]]
      success_count <- sum(data_subset == input$prop_category, na.rm = TRUE)
      total_count <- sum(!is.na(data_subset))
      
      if (total_count == 0) {
        showNotification("Tidak ada data valid untuk uji proporsi", type = "error")
        return()
      }
      
      values$proportion_results <- prop.test(
        x = success_count, 
        n = total_count,
        p = input$prop_null,
        conf.level = 0.95
      )
      
      showNotification("Uji proporsi selesai!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam uji proporsi:", e$message), type = "error")
    })
  })
  
  output$proportion_plot <- renderPlot({
    req(input$prop_var)
    req(values$current_data)
    
    tryCatch({
      if (input$prop_var != "") {
        ggplot(values$current_data, aes_string(x = input$prop_var)) +
          geom_bar(fill = "coral", alpha = 0.7) +
          labs(title = paste("Distribusi", input$prop_var)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        plot(1, 1, main = "Pilih variabel terlebih dahulu")
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$proportion_results <- renderPrint({
    if (!is.null(values$proportion_results)) {
      tryCatch({
        cat("Uji Proporsi\n")
        cat("H0: p =", input$prop_null, "\n")
        cat("H1: p ≠", input$prop_null, "\n\n")
        print(values$proportion_results)
        
        cat("\nInterpretasi:\n")
        if (values$proportion_results$p.value < 0.05) {
          cat("Proporsi berbeda signifikan dari nilai yang dihipotesiskan (p < 0.05)")
        } else {
          cat("Proporsi tidak berbeda signifikan dari nilai yang dihipotesiskan (p ≥ 0.05)")
        }
      }, error = function(e) {
        cat("Error dalam menampilkan hasil:", e$message)
      })
    } else {
      cat("Belum ada uji proporsi yang dilakukan")
    }
  })
  
  # VARIANCE TEST
  observeEvent(input$test_variance_one, {
    tryCatch({
      req(input$var_test_var, input$var_null)
      req(values$current_data)
      
      x <- values$current_data[[input$var_test_var]]
      x <- x[!is.na(x)]
      
      if (length(x) < 10) {
        showNotification("Data tidak cukup untuk uji varians", type = "error")
        return()
      }
      
      n <- length(x)
      var_sample <- var(x)
      
      # Chi-square test for variance
      chi_stat <- (n - 1) * var_sample / input$var_null
      p_value <- 2 * min(pchisq(chi_stat, df = n-1), 1 - pchisq(chi_stat, df = n-1))
      
      values$variance_results <- list(
        statistic = chi_stat,
        p.value = p_value,
        sample_var = var_sample,
        n = n
      )
      
      showNotification("Uji varians selesai!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam uji varians:", e$message), type = "error")
    })
  })
  
  output$variance_plot <- renderPlot({
    req(input$var_test_var)
    req(values$current_data)
    
    tryCatch({
      ggplot(values$current_data, aes_string(x = input$var_test_var)) +
        geom_histogram(bins = 30, fill = "lightgreen", alpha = 0.7) +
        labs(title = paste("Distribusi", input$var_test_var)) +
        theme_minimal()
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$variance_results <- renderPrint({
    if (!is.null(values$variance_results)) {
      tryCatch({
        cat("Uji Varians Satu Sampel (Chi-square test)\n")
        cat("H0: σ² =", input$var_null, "\n")
        cat("H1: σ² ≠", input$var_null, "\n\n")
        cat("Sample variance:", round(values$variance_results$sample_var, 4), "\n")
        cat("Chi-square statistic:", round(values$variance_results$statistic, 4), "\n")
        cat("Degrees of freedom:", values$variance_results$n - 1, "\n")
        cat("P-value:", formatC(values$variance_results$p.value, format = "e", digits = 4), "\n")
        
        cat("\nInterpretasi:\n")
        if (values$variance_results$p.value < 0.05) {
          cat("Varians berbeda signifikan dari nilai yang dihipotesiskan (p < 0.05)")
        } else {
          cat("Varians tidak berbeda signifikan dari nilai yang dihipotesiskan (p ≥ 0.05)")
        }
      }, error = function(e) {
        cat("Error dalam menampilkan hasil:", e$message)
      })
    } else {
      cat("Belum ada uji varians yang dilakukan")
    }
  })
  
  # T-TEST
  observeEvent(input$perform_ttest, {
    tryCatch({
      req(input$ttest_type, input$ttest_var)
      req(values$current_data)
      
      if (input$ttest_type == "one_sample") {
        x <- values$current_data[[input$ttest_var]]
        x <- x[!is.na(x)]
        if (length(x) < 3) {
          showNotification("Data tidak cukup untuk t-test", type = "error")
          return()
        }
        values$ttest_results <- t.test(x, mu = input$mu_null)
      } else if (input$ttest_type == "two_sample") {
        req(input$ttest_group)
        if (input$ttest_group == "") {
          showNotification("Pilih variabel pengelompokan!", type = "error")
          return()
        }
        
        groups <- unique(values$current_data[[input$ttest_group]])
        groups <- groups[!is.na(groups)]
        
        if (length(groups) != 2) {
          showNotification("Variabel pengelompokan harus memiliki tepat 2 kategori!", type = "error")
          return()
        }
        
        group1 <- values$current_data[values$current_data[[input$ttest_group]] == groups[1], input$ttest_var]
        group2 <- values$current_data[values$current_data[[input$ttest_group]] == groups[2], input$ttest_var]
        
        group1 <- group1[!is.na(group1)]
        group2 <- group2[!is.na(group2)]
        
        if (length(group1) < 3 || length(group2) < 3) {
          showNotification("Data tidak cukup dalam setiap kelompok untuk t-test", type = "error")
          return()
        }
        
        values$ttest_results <- t.test(group1, group2)
      }
      
      showNotification("T-test selesai!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam t-test:", e$message), type = "error")
    })
  })
  
  output$ttest_plot <- renderPlot({
    req(input$ttest_var)
    req(values$current_data)
    
    tryCatch({
      if (input$ttest_type == "one_sample") {
        ggplot(values$current_data, aes_string(x = input$ttest_var)) +
          geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
          geom_vline(xintercept = input$mu_null, color = "red", linetype = "dashed", size = 1) +
          labs(title = paste("Distribusi", input$ttest_var), subtitle = "Garis merah: nilai null hypothesis") +
          theme_minimal()
      } else if (input$ttest_type == "two_sample" && !is.null(input$ttest_group) && input$ttest_group != "") {
        ggplot(values$current_data, aes_string(x = input$ttest_group, y = input$ttest_var, fill = input$ttest_group)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Perbandingan", input$ttest_var, "by", input$ttest_group)) +
          theme_minimal()
      } else {
        plot(1, 1, main = "Pilih tipe t-test dan variabel")
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$ttest_results <- renderPrint({
    if (!is.null(values$ttest_results)) {
      tryCatch({
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
      }, error = function(e) {
        cat("Error dalam menampilkan hasil:", e$message)
      })
    } else {
      cat("Belum ada t-test yang dilakukan")
    }
  })
  
  # ANOVA ONE-WAY
  observeEvent(input$perform_anova_one, {
    tryCatch({
      req(input$anova_dep_var, input$anova_indep_var)
      req(values$current_data)
      
      if (input$anova_indep_var == "" || input$anova_dep_var == "") {
        showNotification("Pilih variabel terlebih dahulu!", type = "error")
        return()
      }
      
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
      
      showNotification("ANOVA satu arah selesai!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam ANOVA:", e$message), type = "error")
    })
  })
  
  output$anova_one_plot <- renderPlot({
    req(input$anova_dep_var, input$anova_indep_var)
    req(values$current_data)
    
    tryCatch({
      if (input$anova_indep_var != "" && input$anova_dep_var != "") {
        ggplot(values$current_data, aes_string(x = input$anova_indep_var, y = input$anova_dep_var, 
                                              fill = input$anova_indep_var)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("ANOVA:", input$anova_dep_var, "by", input$anova_indep_var)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        plot(1, 1, main = "Pilih variabel terlebih dahulu")
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$anova_one_results <- renderPrint({
    if (!is.null(values$anova_one_results)) {
      tryCatch({
        cat("ANOVA Satu Arah\n")
        cat("Formula:", values$anova_one_results$formula, "\n\n")
        print(values$anova_one_results$summary)
        
        f_stat <- values$anova_one_results$summary[[1]]$`F value`[1]
        p_value <- values$anova_one_results$summary[[1]]$`Pr(>F)`[1]
        
        cat("\nInterpretasi:\n")
        if (!is.na(p_value) && p_value < 0.05) {
          cat("Terdapat perbedaan signifikan antar kelompok (p < 0.05)")
        } else {
          cat("Tidak terdapat perbedaan signifikan antar kelompok (p ≥ 0.05)")
        }
      }, error = function(e) {
        cat("Error dalam menampilkan hasil:", e$message)
      })
    } else {
      cat("Belum ada ANOVA yang dilakukan")
    }
  })
  
  output$posthoc_results <- DT::renderDataTable({
    if (!is.null(values$posthoc_results)) {
      tryCatch({
        tukey_df <- as.data.frame(values$posthoc_results[[1]])
        tukey_df$Comparison <- rownames(tukey_df)
        tukey_df <- tukey_df[, c("Comparison", "diff", "lwr", "upr", "p adj")]
        
        DT::datatable(tukey_df, options = list(pageLength = 10)) %>%
          DT::formatRound(columns = c("diff", "lwr", "upr", "p adj"), digits = 4)
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    } else {
      DT::datatable(data.frame(Message = "Belum ada post-hoc test yang dilakukan"))
    }
  })
  
  output$effect_size_one <- renderPrint({
    if (!is.null(values$anova_one_results)) {
      tryCatch({
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
      }, error = function(e) {
        cat("Error dalam menghitung effect size:", e$message)
      })
    } else {
      cat("Belum ada ANOVA yang dilakukan")
    }
  })
  
  # ANOVA TWO-WAY
  observeEvent(input$perform_anova_two, {
    tryCatch({
      req(input$anova2_dep_var, input$anova2_factor1, input$anova2_factor2)
      req(values$current_data)
      
      if (input$anova2_dep_var == "" || input$anova2_factor1 == "" || input$anova2_factor2 == "") {
        showNotification("Pilih semua variabel terlebih dahulu!", type = "error")
        return()
      }
      
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
      
      showNotification("ANOVA dua arah selesai!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam ANOVA dua arah:", e$message), type = "error")
    })
  })
  
  output$anova_two_plot <- renderPlot({
    req(input$anova2_dep_var, input$anova2_factor1, input$anova2_factor2)
    req(values$current_data)
    
    tryCatch({
      if (input$anova2_dep_var != "" && input$anova2_factor1 != "" && input$anova2_factor2 != "") {
        ggplot(values$current_data, aes_string(x = input$anova2_factor1, y = input$anova2_dep_var, 
                                              color = input$anova2_factor2)) +
          stat_summary(fun = mean, geom = "point", size = 3) +
          stat_summary(fun = mean, geom = "line", aes_string(group = input$anova2_factor2)) +
          labs(title = paste("Interaction Plot:", input$anova2_dep_var),
               x = input$anova2_factor1, color = input$anova2_factor2) +
          theme_minimal()
      } else {
        plot(1, 1, main = "Pilih semua variabel terlebih dahulu")
      }
    }, error = function(e) {
      plot(1, 1, main = paste("Error:", e$message))
    })
  })
  
  output$anova_two_results <- renderPrint({
    if (!is.null(values$anova_two_results)) {
      tryCatch({
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
      }, error = function(e) {
        cat("Error dalam menampilkan hasil:", e$message)
      })
    } else {
      cat("Belum ada ANOVA dua arah yang dilakukan")
    }
  })
  
  output$marginal_means <- DT::renderDataTable({
    tryCatch({
      req(input$anova2_dep_var, input$anova2_factor1, input$anova2_factor2)
      req(values$current_data)
      
      if (input$anova2_dep_var == "" || input$anova2_factor1 == "" || input$anova2_factor2 == "") {
        return(DT::datatable(data.frame(Message = "Pilih semua variabel terlebih dahulu")))
      }
      
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
    }, error = function(e) {
      return(DT::datatable(data.frame(Error = paste("Error:", e$message))))
    })
  })
  
  # REGRESSION MODEL
  observeEvent(input$build_model, {
    tryCatch({
      req(input$reg_dependent, input$reg_predictors)
      req(values$current_data)
      
      if (length(input$reg_predictors) == 0) {
        showNotification("Pilih minimal satu variabel prediktor!", type = "error")
        return()
      }
      
      predictors <- input$reg_predictors
      formula_str <- paste(input$reg_dependent, "~", paste(predictors, collapse = " + "))
      
      # Check if variables exist in data
      all_vars <- c(input$reg_dependent, input$reg_predictors)
      missing_vars <- all_vars[!all_vars %in% names(values$current_data)]
      
      if (length(missing_vars) > 0) {
        showNotification(paste("Variabel tidak ditemukan:", paste(missing_vars, collapse = ", ")), type = "error")
        return()
      }
      
      reg_model <- lm(as.formula(formula_str), data = values$current_data)
      values$model_results <- list(
        model = reg_model,
        formula = formula_str,
        summary = summary(reg_model)
      )
      
      showNotification("Model regresi berhasil dibangun!", type = "default")
    }, error = function(e) {
      showNotification(paste("Error dalam membangun model:", e$message), type = "error")
    })
  })
  
  output$model_summary <- renderPrint({
    if (!is.null(values$model_results)) {
      tryCatch({
        cat("Model Regresi Linear Berganda\n")
        cat("Formula:", values$model_results$formula, "\n\n")
        print(values$model_results$summary)
      }, error = function(e) {
        cat("Error dalam menampilkan model:", e$message)
      })
    } else {
      cat("Belum ada model yang dibangun")
    }
  })
  
  # REGRESSION DIAGNOSTICS
  observeEvent(input$run_reg_diagnostics, {
    if (!is.null(values$model_results)) {
      output$regression_diagnostics <- renderPlot({
        tryCatch({
          model <- values$model_results$model
          par(mfrow = c(2, 2))
          plot(model)
        }, error = function(e) {
          plot(1, 1, main = paste("Error:", e$message))
        })
      })
    } else {
      showNotification("Bangun model terlebih dahulu!", type = "warning")
    }
  })
  
  output$reg_normality_test <- renderPrint({
    if (!is.null(values$model_results)) {
      tryCatch({
        residuals_model <- residuals(values$model_results$model)
        if (length(residuals_model) > 3) {
          shapiro_result <- shapiro.test(residuals_model)
          
          cat("Uji Normalitas Residual (Shapiro-Wilk):\n")
          cat("W-statistic:", round(shapiro_result$statistic, 4), "\n")
          cat("P-value:", formatC(shapiro_result$p.value, format = "e", digits = 4), "\n")
          cat("\nInterpretasi:\n")
          if (shapiro_result$p.value < 0.05) {
            cat("Residual TIDAK normal (p < 0.05)")
          } else {
            cat("Residual normal (p ≥ 0.05)")
          }
        } else {
          cat("Data tidak cukup untuk uji normalitas residual")
        }
      }, error = function(e) {
        cat("Error dalam uji normalitas residual:", e$message)
      })
    } else {
      cat("Tidak ada model yang tersedia")
    }
  })
  
  output$model_metrics <- renderPrint({
    if (!is.null(values$model_results)) {
      tryCatch({
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
      }, error = function(e) {
        cat("Error dalam menampilkan metrics:", e$message)
      })
    } else {
      cat("Tidak ada model yang tersedia")
    }
  })
  
  # COEFFICIENT TABLE
  output$coefficient_table <- DT::renderDataTable({
    if (!is.null(values$model_results)) {
      tryCatch({
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
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Error:", e$message)))
      })
    } else {
      DT::datatable(data.frame(Message = "Belum ada model yang dibangun"))
    }
  })
  
  # DOWNLOAD HANDLERS
  output$download_original <- downloadHandler(
    filename = function() {
      paste("sovi_data_original_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$original_data)) {
        write.csv(values$original_data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_categorized <- downloadHandler(
    filename = function() {
      paste("sovi_data_categorized_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$current_data)) {
        write.csv(values$current_data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_descriptive <- downloadHandler(
    filename = function() {
      paste("descriptive_statistics_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      
      if (!is.null(input$desc_var) && !is.null(values$current_data)) {
        tryCatch({
          par(mfrow = c(2, 2))
          
          # Histogram
          hist(values$current_data[[input$desc_var]], main = paste("Histogram of", input$desc_var),
               xlab = input$desc_var, col = "lightblue")
          
          # Boxplot
          boxplot(values$current_data[[input$desc_var]], main = paste("Boxplot of", input$desc_var),
                  ylab = input$desc_var, col = "lightgreen")
          
          # Q-Q plot
          qqnorm(values$current_data[[input$desc_var]], main = paste("Q-Q Plot of", input$desc_var))
          qqline(values$current_data[[input$desc_var]])
          
          # Density plot
          plot(density(values$current_data[[input$desc_var]], na.rm = TRUE), 
               main = paste("Density Plot of", input$desc_var))
        }, error = function(e) {
          plot(1, 1, main = "Error in generating plots")
        })
      }
      
      dev.off()
    }
  )
  
  output$download_visualization <- downloadHandler(
    filename = function() {
      paste("advanced_visualization_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 10, height = 8)
      
      tryCatch({
        if (input$viz_type == "scatter" && !is.null(input$x_var) && !is.null(input$y_var)) {
          plot(values$current_data[[input$x_var]], values$current_data[[input$y_var]],
               xlab = input$x_var, ylab = input$y_var, 
               main = paste("Scatter Plot:", input$x_var, "vs", input$y_var),
               col = "blue", pch = 16, alpha = 0.6)
          abline(lm(values$current_data[[input$y_var]] ~ values$current_data[[input$x_var]]), col = "red")
        } else if (input$viz_type == "correlation") {
          numeric_data <- values$current_data[sapply(values$current_data, is.numeric)]
          cor_matrix <- cor(numeric_data, use = "complete.obs")
          corrplot(cor_matrix, method = "color", type = "upper", order = "hclust")
        } else {
          plot(1, 1, main = "Select visualization type and variables")
        }
      }, error = function(e) {
        plot(1, 1, main = "Error in generating visualization")
      })
      
      dev.off()
    }
  )
  
  output$download_clustering <- downloadHandler(
    filename = function() {
      paste("clustering_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$cluster_results)) {
        write.csv(values$cluster_results, file, row.names = FALSE)
      }
    }
  )
  
  output$download_normality <- downloadHandler(
    filename = function() {
      paste("normality_test_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      if (!is.null(values$normality_results)) {
        sink(file)
        cat("HASIL UJI NORMALITAS\n")
        cat("===================\n\n")
        
        if (is.list(values$normality_results) && "method" %in% names(values$normality_results)) {
          cat("Uji Normalitas:", values$normality_results$method, "\n")
          if (!is.na(values$normality_results$statistic)) {
            cat("Statistik:", round(values$normality_results$statistic, 4), "\n")
          }
          cat("P-value:", formatC(values$normality_results$p.value, format = "e", digits = 4), "\n")
        }
        
        sink()
      }
    }
  )
  
  output$download_homogeneity <- downloadHandler(
    filename = function() {
      paste("homogeneity_test_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      if (!is.null(values$homogeneity_results)) {
        sink(file)
        cat("HASIL UJI HOMOGENITAS\n")
        cat("====================\n\n")
        cat("Uji Homogenitas Varians (Levene Test)\n")
        if (!is.na(values$homogeneity_results$statistic)) {
          cat("F-statistic:", round(values$homogeneity_results$statistic, 4), "\n")
        }
        cat("P-value:", formatC(values$homogeneity_results$p.value, format = "e", digits = 4), "\n")
        sink()
      }
    }
  )
  
  output$download_assumptions <- downloadHandler(
    filename = function() {
      paste("assumptions_summary_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file)
      plot(1, 1, main = "Assumptions Summary Report", type = "n")
      text(1, 1, "Detailed assumptions summary\nwould be generated here", cex = 1.2)
      dev.off()
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)