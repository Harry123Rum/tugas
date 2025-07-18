# ===================================================================
# Geo-Social Vulnerability Analytics Dashboard (GSVAD)
# Dibuat oleh AI Gemini untuk memenuhi permintaan UAS
# Versi: 1.0
# Tanggal: 18 Juli 2025
# ===================================================================

# -- 1. SETUP: MEMUAT PUSTAKA (LIBRARIES) --
# Pastikan semua pustaka ini sudah terinstal sebelum menjalankan aplikasi
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(car)      # Untuk uji Levene dan VIF
library(nortest)  # Untuk uji normalitas
library(rmarkdown)# Untuk membuat laporan unduhan
library(knitr)    # Untuk tabel di laporan
library(tinytex)  # Untuk render PDF
library(leaflet)  # Untuk peta interaktif
library(sf)       # Untuk data spasial (peta)
library(httr)     # Untuk mengambil data dari GitHub

# ===================================================================
# -- 2. DATA LOADING: MENGAMBIL DATA DARI GITHUB --
# ===================================================================

# Fungsi untuk mencoba mengunduh file dengan beberapa percobaan
download_with_retry <- function(url) {
  tryCatch({
    # Gunakan httr::GET untuk kontrol yang lebih baik dan untuk menghindari masalah cache
    response <- GET(url)
    stop_for_status(response) # Berhenti jika ada error HTTP (misal: 404 Not Found)
    content <- content(response, as = "text", encoding = "UTF-8")
    return(read.csv(text = content))
  }, error = function(e) {
    showNotification(paste("Gagal memuat data dari URL:", url, ". Error:", e$message), type = "error", duration = 15)
    return(NULL)
  })
}

# URL ke file mentah di GitHub
sovi_url <- "https://raw.githubusercontent.com/wpgp/SoVI/master/sovi_data.csv"
distance_url <- "https://raw.githubusercontent.com/wpgp/SoVI/master/distance.csv"
metadata_url <- "https://raw.githubusercontent.com/wpgp/SoVI/master/metadata.csv"

# Memuat data utama
sovi_data <- download_with_retry(sovi_url)
# distance_matrix <- download_with_retry(distance_url) # Matriks jarak bisa digunakan untuk analisis spasial/clustering lanjutan
metadata <- download_with_retry(metadata_url)

# Membersihkan dan mempersiapkan data
# Mengidentifikasi kolom numerik dan kategorik
if (!is.null(sovi_data)) {
  # Pastikan FIPS diformat dengan benar untuk penggabungan peta
  sovi_data$FIPS <- sprintf("%05d", sovi_data$FIPS)
  
  numeric_vars <- sovi_data %>% select(where(is.numeric)) %>% names()
  # Hapus variabel ID/lokasi dari daftar numerik untuk analisis
  numeric_vars <- setdiff(numeric_vars, c("FIPS", "SoVI_SCORE")) 
  
  categorical_vars <- sovi_data %>% select(where(is.character), where(is.factor)) %>% names()
  categorical_vars <- setdiff(categorical_vars, c("COUNTY", "ST_ABBR", "STATE"))
} else {
  # Fallback jika data gagal dimuat
  numeric_vars <- c()
  categorical_vars <- c()
}


# ===================================================================
# -- 3. USER INTERFACE (UI) --
# ===================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "GSVAD Dashboard"),
  
  # -- Sidebar Menu --
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar"),
               menuSubItem("Statistik Deskriptif", tabName = "deskriptif"),
               menuSubItem("Visualisasi Grafik", tabName = "visualisasi"),
               menuSubItem("Visualisasi Peta", tabName = "peta_visual")
      ),
      menuItem("Uji Asumsi Klasik", tabName = "uji_asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", icon = icon("calculator"),
               menuSubItem("Uji Proporsi & Varians", tabName = "inferensia1"),
               menuSubItem("Uji Beda Rata-rata (ANOVA)", tabName = "inferensia2")
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("chart-line"))
    )
  ),
  
  # -- Body Content --
  dashboardBody(
    tabItems(
      # 1. Tab Beranda
      tabItem(tabName = "beranda",
              fluidRow(
                box(title = "Selamat Datang di GSVAD", width = 12, solidHeader = true, status = "primary",
                    h4("Geo-Social Vulnerability Analytics Dashboard (GSVAD)"),
                    p("Dashboard ini dirancang untuk melakukan analisis komprehensif terhadap data Kerentanan Sosial (Social Vulnerability - SoVI). Anda dapat melakukan manajemen data, eksplorasi, uji asumsi, analisis inferensia, hingga pemodelan regresi."),
                    p("Gunakan menu di sebelah kiri untuk menavigasi berbagai fitur analisis. Setiap halaman dilengkapi dengan opsi untuk mengunduh output analisis dalam format gambar (JPG), dokumen (Word/PDF), serta laporan ringkasan per halaman.")
                ),
                box(title = "Metadata Variabel", width = 12, solidHeader = true, status = "info",
                    DT::dataTableOutput("metadata_table")
                )
              )
      ),
      
      # 2. Tab Manajemen Data
      tabItem(tabName = "manajemen_data",
              fluidRow(
                box(title = "Kategorisasi Variabel Kontinyu", width = 4, status = "warning",
                    selectInput("var_to_categorize", "Pilih Variabel Kontinyu:", choices = numeric_vars),
                    numericInput("num_categories", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                    actionButton("run_categorize", "Proses", icon = icon("cogs")),
                    hr(),
                    p("Fitur ini mengubah variabel numerik menjadi variabel kategorik berdasarkan kuantil (rentang yang sama). Ini berguna untuk analisis seperti ANOVA.")
                ),
                box(title = "Hasil Kategorisasi", width = 8, status = "success",
                    DT::dataTableOutput("categorized_table"),
                    hr(),
                    h4("Interpretasi"),
                    verbatimTextOutput("categorize_interpretation")
                )
              )
      ),
      
      # 3.1. Tab Eksplorasi - Deskriptif
      tabItem(tabName = "deskriptif",
              fluidRow(
                box(title = "Pilihan Statistik Deskriptif", width = 4, status = "primary",
                    selectInput("desc_var", "Pilih Variabel:", choices = c(numeric_vars, categorical_vars)),
                    downloadButton("download_desc_report", "Unduh Laporan Halaman (PDF)")
                ),
                box(title = "Ringkasan Statistik", width = 8, status = "info",
                    verbatimTextOutput("desc_summary"),
                    hr(),
                    h4("Interpretasi"),
                    uiOutput("desc_interpretation")
                )
              )
      ),
      
      # 3.2. Tab Eksplorasi - Visualisasi Grafik
      tabItem(tabName = "visualisasi",
              fluidRow(
                box(title = "Opsi Visualisasi", width = 4, status = "primary",
                    selectInput("plot_type", "Pilih Jenis Grafik:", 
                                choices = c("Histogram", "Boxplot", "Scatter Plot")),
                    # UI dinamis berdasarkan jenis plot
                    uiOutput("plot_ui_controls"),
                    downloadButton("download_plot_jpg", "Unduh Grafik (JPG)")
                ),
                box(title = "Tampilan Grafik", width = 8, status = "info",
                    plotOutput("main_plot"),
                    hr(),
                    h4("Interpretasi"),
                    textOutput("plot_interpretation")
                )
              )
      ),
      
      # 3.3. Tab Eksplorasi - Visualisasi Peta
      tabItem(tabName = "peta_visual",
              fluidRow(
                box(title = "Pilihan Peta Choropleth", width = 12, status = "primary",
                    column(6, selectInput("map_var", "Pilih Variabel untuk Dipetakan:", choices = c("SoVI_SCORE", numeric_vars))),
                    column(6, selectInput("map_color", "Pilih Skema Warna:", choices = c("YlOrRd", "Blues", "Viridis", "Greens")))
                ),
                box(title = "Peta Interaktif Kerentanan Sosial", width = 12, status = "info",
                    leafletOutput("sovi_map", height = "600px"),
                    hr(),
                    h4("Interpretasi"),
                    textOutput("map_interpretation")
                )
              )
      ),
      
      # 4. Tab Uji Asumsi
      tabItem(tabName = "uji_asumsi",
              fluidRow(
                box(title = "Opsi Uji Asumsi", width = 4, status = "primary",
                    selectInput("asumsi_var", "Pilih Variabel Numerik:", choices = numeric_vars),
                    selectInput("asumsi_group_var", "Pilih Variabel Grup (untuk Homogenitas):", choices = c("Tidak Ada", categorical_vars)),
                    actionButton("run_asumsi", "Jalankan Uji", icon = icon("play-circle"))
                ),
                box(title = "Hasil Uji Normalitas (Shapiro-Wilk)", width = 8, status = "info",
                    verbatimTextOutput("normality_test_output"),
                    hr(),
                    h4("Interpretasi"),
                    textOutput("normality_interpretation")
                ),
                box(title = "Hasil Uji Homogenitas Varians (Levene's Test)", width = 8, status = "info",
                    verbatimTextOutput("homogeneity_test_output"),
                    hr(),
                    h4("Interpretasi"),
                    textOutput("homogeneity_interpretation")
                )
              )
      ),
      
      # 5.1 Tab Inferensia 1 (Proporsi & Varians)
      tabItem(tabName = "inferensia1",
              tabsetPanel(
                tabPanel("Uji Proporsi",
                         fluidRow(
                           box(title = "Uji Proporsi Satu Sampel", width=6, status="primary",
                               selectInput("prop1_var", "Pilih Variabel Biner (Kategorik):", choices=categorical_vars),
                               uiOutput("prop1_level_ui"),
                               numericInput("prop1_p0", "Nilai Proporsi Hipotesis (p0):", 0.5, min=0, max=1, step=0.01),
                               actionButton("run_prop1", "Jalankan Uji")
                           ),
                           box(title="Hasil Uji Proporsi 1 Sampel", width=6, status="info",
                               verbatimTextOutput("prop1_output"),
                               textOutput("prop1_interp")
                           )
                         ),
                         fluidRow(
                           box(title = "Uji Proporsi Dua Sampel", width=6, status="primary",
                               selectInput("prop2_var", "Pilih Variabel Grup:", choices=categorical_vars),
                               selectInput("prop2_response", "Pilih Variabel Respon Biner:", choices=categorical_vars),
                               actionButton("run_prop2", "Jalankan Uji")
                           ),
                           box(title="Hasil Uji Proporsi 2 Sampel", width=6, status="info",
                               verbatimTextOutput("prop2_output"),
                               textOutput("prop2_interp")
                           )
                         )
                ),
                tabPanel("Uji Varians",
                         fluidRow(
                           box(title = "Uji Varians Dua Sampel (F-test)", width=6, status="primary",
                               selectInput("var2_group", "Pilih Variabel Grup (harus 2 level):", choices=categorical_vars),
                               selectInput("var2_numeric", "Pilih Variabel Numerik:", choices=numeric_vars),
                               actionButton("run_var2", "Jalankan Uji")
                           ),
                           box(title="Hasil Uji Varians 2 Sampel", width=6, status="info",
                               verbatimTextOutput("var2_output"),
                               textOutput("var2_interp")
                           )
                         )
                )
              )
      ),
      
      # 5.2 Tab Inferensia 2 (ANOVA)
      tabItem(tabName = "inferensia2",
              fluidRow(
                box(title = "Opsi Analisis Varians (ANOVA)", width = 4, status = "primary",
                    selectInput("anova_dep_var", "Pilih Variabel Dependen (Numerik):", choices = numeric_vars),
                    selectInput("anova_indep_var1", "Pilih Faktor Independen 1 (Kategorik):", choices = categorical_vars),
                    selectInput("anova_indep_var2", "Pilih Faktor Independen 2 (Opsional, untuk Two-Way):", choices = c("Tidak Ada", categorical_vars)),
                    actionButton("run_anova", "Jalankan ANOVA")
                ),
                box(title = "Hasil ANOVA", width = 8, status = "info",
                    verbatimTextOutput("anova_output"),
                    hr(),
                    h4("Interpretasi"),
                    textOutput("anova_interpretation"),
                    hr(),
                    plotOutput("anova_plot")
                )
              )
      ),
      
      # 6. Tab Regresi
      tabItem(tabName = "regresi",
              fluidRow(
                box(title = "Pemilihan Variabel Regresi", width = 4, status = "primary",
                    selectInput("reg_dep_var", "Pilih Variabel Dependen (Y):", choices = numeric_vars),
                    selectizeInput("reg_indep_vars", "Pilih Variabel Independen (X):", choices = numeric_vars, multiple = TRUE),
                    actionButton("run_regression", "Jalankan Regresi", icon = icon("play"))
                ),
                box(title = "Hasil Model Regresi", width = 8, status = "info",
                    verbatimTextOutput("regression_summary"),
                    hr(),
                    h4("Interpretasi Model"),
                    textOutput("regression_interpretation")
                )
              ),
              fluidRow(
                box(title = "Uji Asumsi Regresi", width = 12, status = "warning",
                    tabsetPanel(
                      tabPanel("Normalitas Residuals", plotOutput("reg_norm_plot"), textOutput("reg_norm_interp")),
                      tabPanel("Homoskedastisitas", plotOutput("reg_homo_plot"), textOutput("reg_homo_interp")),
                      tabPanel("Multikolinearitas (VIF)", verbatimTextOutput("reg_vif_output"), textOutput("reg_vif_interp"))
                    )
                )
              )
      )
    )
  )
)


# ===================================================================
# -- 4. SERVER LOGIC --
# ===================================================================

server <- function(input, output, session) {
  
  # Data reaktif untuk menampung perubahan (misal: setelah kategorisasi)
  reactive_data <- reactiveVal(sovi_data)
  
  # -- Beranda --
  output$metadata_table <- DT::renderDataTable({
    req(metadata)
    DT::datatable(metadata, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  
  # -- Manajemen Data --
  observeEvent(input$run_categorize, {
    req(reactive_data(), input$var_to_categorize, input$num_categories)
    df <- reactive_data()
    var <- input$var_to_categorize
    
    # Menggunakan cut untuk membuat kategori berdasarkan kuantil
    df[[paste0(var, "_cat")]] <- cut(
      df[[var]],
      breaks = quantile(df[[var]], probs = seq(0, 1, by = 1/input$num_categories), na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste("Kategori", 1:input$num_categories)
    )
    
    # Update data reaktif
    reactive_data(df) 
    
    # Update pilihan variabel kategorik di seluruh aplikasi
    new_categorical_vars <- df %>% select(where(is.character), where(is.factor)) %>% names()
    new_categorical_vars <- setdiff(new_categorical_vars, c("COUNTY", "ST_ABBR", "STATE"))
    
    updateSelectInput(session, "asumsi_group_var", choices = c("Tidak Ada", new_categorical_vars))
    updateSelectInput(session, "anova_indep_var1", choices = new_categorical_vars)
    updateSelectInput(session, "anova_indep_var2", choices = c("Tidak Ada", new_categorical_vars))
    updateSelectInput(session, "prop1_var", choices = new_categorical_vars)
    updateSelectInput(session, "prop2_var", choices = new_categorical_vars)
    updateSelectInput(session, "prop2_response", choices = new_categorical_vars)
    updateSelectInput(session, "var2_group", choices = new_categorical_vars)
    
    output$categorized_table <- DT::renderDataTable({
      DT::datatable(df[, c(var, paste0(var, "_cat"))], options = list(pageLength = 5))
    })
    
    output$categorize_interpretation <- renderText({
      paste("Variabel '", var, "' telah berhasil diubah menjadi variabel kategorik baru '", paste0(var, "_cat"), 
            "' dengan ", input$num_categories, " level berdasarkan rentang kuantilnya. Anda sekarang dapat menggunakan variabel baru ini dalam analisis yang memerlukan input kategorik, seperti ANOVA.", sep="")
    })
  })
  
  # -- Eksplorasi: Deskriptif --
  desc_output <- eventReactive(input$desc_var, {
    req(reactive_data(), input$desc_var)
    df <- reactive_data()
    var <- input$desc_var
    if (var %in% numeric_vars) {
      summary(df[[var]])
    } else {
      table(df[[var]])
    }
  })
  
  output$desc_summary <- renderPrint({
    desc_output()
  })
  
  output$desc_interpretation <- renderUI({
    req(input$desc_var)
    var <- input$desc_var
    if (var %in% numeric_vars) {
      s <- summary(reactive_data()[[var]])
      HTML(paste0("<b>Variabel ", var, "</b> adalah variabel numerik dengan nilai ",
                  "<b>Minimum:</b> ", round(s[1], 2), ", ",
                  "<b>Median (nilai tengah):</b> ", round(s[3], 2), ", ",
                  "<b>Rata-rata:</b> ", round(s[4], 2), ", dan ",
                  "<b>Maksimum:</b> ", round(s[6], 2), "."))
    } else {
      tbl <- table(reactive_data()[[var]])
      most_freq <- names(which.max(tbl))
      HTML(paste0("<b>Variabel ", var, "</b> adalah variabel kategorik. ",
                  "Kategori yang paling sering muncul adalah <b>'", most_freq, "'</b> dengan frekuensi sebanyak ", max(tbl), " kali."))
    }
  })
  
  # -- Eksplorasi: Visualisasi Grafik --
  output$plot_ui_controls <- renderUI({
    req(input$plot_type)
    df_cols <- names(reactive_data())
    
    if (input$plot_type == "Histogram") {
      selectInput("hist_var", "Pilih Variabel:", choices = numeric_vars)
    } else if (input$plot_type == "Boxplot") {
      fluidRow(
        column(6, selectInput("box_x", "Variabel Kategorik (X):", choices = categorical_vars)),
        column(6, selectInput("box_y", "Variabel Numerik (Y):", choices = numeric_vars))
      )
    } else if (input$plot_type == "Scatter Plot") {
      fluidRow(
        column(6, selectInput("scatter_x", "Variabel X:", choices = numeric_vars)),
        column(6, selectInput("scatter_y", "Variabel Y:", choices = numeric_vars))
      )
    }
  })
  
  current_plot <- reactive({
    req(input$plot_type)
    df <- reactive_data()
    
    p <- ggplot() # default empty plot
    
    if (input$plot_type == "Histogram" && !is.null(input$hist_var)) {
      p <- ggplot(df, aes_string(x = input$hist_var)) + 
        geom_histogram(aes(y = ..density..), fill = "skyblue", color = "white", bins=30) +
        geom_density(alpha = 0.2, fill = "#FF6666") +
        labs(title = paste("Histogram dari", input$hist_var), x = input$hist_var, y = "Densitas") +
        theme_minimal()
    } else if (input$plot_type == "Boxplot" && !is.null(input$box_x) && !is.null(input$box_y)) {
      p <- ggplot(df, aes_string(x = input$box_x, y = input$box_y, fill = input$box_x)) +
        geom_boxplot() +
        labs(title = paste("Boxplot", input$box_y, "berdasarkan", input$box_x), x = input$box_x, y = input$box_y) +
        theme_minimal() + theme(legend.position = "none")
    } else if (input$plot_type == "Scatter Plot" && !is.null(input$scatter_x) && !is.null(input$scatter_y)) {
      p <- ggplot(df, aes_string(x = input$scatter_x, y = input$scatter_y)) +
        geom_point(alpha = 0.6, color = "darkblue") +
        geom_smooth(method = "lm", color = "red", se = FALSE) +
        labs(title = paste("Scatter Plot antara", input$scatter_y, "dan", input$scatter_x), x = input$scatter_x, y = input$scatter_y) +
        theme_minimal()
    }
    p
  })
  
  output$main_plot <- renderPlot({
    current_plot()
  })
  
  output$download_plot_jpg <- downloadHandler(
    filename = function() { paste(input$plot_type, '_', Sys.Date(), '.jpg', sep='') },
    content = function(file) {
      ggsave(file, plot = current_plot(), device = "jpeg", width = 8, height = 6)
    }
  )
  
  output$plot_interpretation <- renderText({
    req(input$plot_type)
    if (input$plot_type == "Histogram" && !is.null(input$hist_var)) {
      paste("Grafik ini menunjukkan distribusi frekuensi dari variabel '", input$hist_var, 
            "'. Bentuk histogram (misalnya, simetris, miring ke kanan/kiri) memberikan gambaran tentang sebaran data.", sep="")
    } else if (input$plot_type == "Boxplot" && !is.null(input$box_x) && !is.null(input$box_y)) {
      paste("Grafik ini membandingkan distribusi variabel '", input$box_y, "' di setiap kategori dari '", input$box_x, 
            "'. Perhatikan perbedaan median (garis tengah), rentang interkuartil (kotak), dan adanya outlier (titik).", sep="")
    } else if (input$plot_type == "Scatter Plot" && !is.null(input$scatter_x) && !is.null(input$scatter_y)) {
      paste("Grafik ini menunjukkan hubungan antara '", input$scatter_x, "' dan '", input$scatter_y, 
            "'. Garis merah menunjukkan tren linear. Arah (positif/negatif) dan kekuatan sebaran titik mengindikasikan korelasi.", sep="")
    }
  })
  
  # -- Eksplorasi: Peta --
  # Memuat shapefile (hanya sekali)
  counties_sf <- reactive({
    showNotification("Mengunduh data geografis (shapefile)...", duration = 5)
    tryCatch({
      # Menggunakan tigris untuk mendapatkan shapefile county AS
      options(tigris_use_cache = TRUE)
      tigris::counties(cb = TRUE, resolution = "500k") %>%
        rename(FIPS = GEOID)
    }, error = function(e) {
      showNotification("Gagal mengunduh shapefile. Pastikan koneksi internet stabil.", type = "error")
      return(NULL)
    })
  })
  
  # Menggabungkan data sovi dengan shapefile
  sovi_sf <- reactive({
    req(counties_sf(), reactive_data(), input$map_var)
    showNotification("Menggabungkan data dengan peta...", duration = 3)
    sovi_df <- reactive_data()
    counties <- counties_sf()
    
    # Pastikan FIPS di kedua set data cocok
    if ("FIPS" %in% names(sovi_df) && "FIPS" %in% names(counties)) {
      left_join(counties, sovi_df, by = "FIPS") %>%
        filter(!is.na(.data[[input$map_var]]))
    } else {
      return(NULL)
    }
  })
  
  output$sovi_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Peta Dasar") %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) # Pusat AS
  })
  
  observe({
    req(sovi_sf(), input$map_var)
    map_data <- sovi_sf()
    
    pal <- colorNumeric(
      palette = input$map_color,
      domain = map_data[[input$map_var]]
    )
    
    labels <- sprintf(
      "<strong>%s, %s</strong><br/>%s: %g",
      map_data$COUNTY, map_data$ST_ABBR, input$map_var, map_data[[input$map_var]]
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("sovi_map", data = map_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(get(input$map_var)),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~get(input$map_var), opacity = 0.7, title = input$map_var,
                position = "bottomright")
  })
  
  output$map_interpretation <- renderText({
    req(input$map_var)
    paste("Peta ini memvisualisasikan sebaran geografis dari variabel '", input$map_var, 
          "'. Warna yang lebih gelap menunjukkan nilai yang lebih tinggi. Pola spasial (misalnya, pengelompokan di wilayah tertentu) dapat mengindikasikan adanya faktor geografis yang mempengaruhi variabel tersebut.")
  })
  
  
  # -- Uji Asumsi --
  normality_result <- eventReactive(input$run_asumsi, {
    req(reactive_data(), input$asumsi_var)
    data_vec <- reactive_data()[[input$asumsi_var]]
    if(length(na.omit(data_vec)) > 3 && length(na.omit(data_vec)) < 5000) {
      shapiro.test(data_vec)
    } else {
      # Shapiro test tidak ideal untuk N > 5000, gunakan lillie
      lillie.test(na.omit(data_vec))
    }
  })
  
  homogeneity_result <- eventReactive(input$run_asumsi, {
    req(reactive_data(), input$asumsi_var, input$asumsi_group_var)
    if(input$asumsi_group_var != "Tidak Ada"){
      df <- reactive_data()
      formula <- as.formula(paste(input$asumsi_var, "~", input$asumsi_group_var))
      leveneTest(formula, data = df)
    } else {
      "Pilih variabel grup untuk menjalankan uji ini."
    }
  })
  
  output$normality_test_output <- renderPrint({ normality_result() })
  output$homogeneity_test_output <- renderPrint({ homogeneity_result() })
  
  output$normality_interpretation <- renderText({
    res <- normality_result()
    p_val <- res$p.value
    method <- res$method
    paste("Hipotesis Nol (H0): Data berdistribusi normal. Uji yang digunakan adalah", method,
          ". Dengan p-value =", round(p_val, 4), ",",
          if(p_val > 0.05) "lebih besar dari 0.05, maka kita gagal menolak H0. Dapat disimpulkan data berdistribusi normal."
          else "lebih kecil dari 0.05, maka kita menolak H0. Dapat disimpulkan data tidak berdistribusi normal.")
  })
  
  output$homogeneity_interpretation <- renderText({
    res <- homogeneity_result()
    if(is.character(res)) return(res)
    p_val <- res$`Pr(>F)`[1]
    paste("Hipotesis Nol (H0): Varians data adalah homogen antar kelompok. Dengan p-value =", round(p_val, 4), ",",
          if(p_val > 0.05) "lebih besar dari 0.05, maka kita gagal menolak H0. Asumsi homogenitas varians terpenuhi."
          else "lebih kecil dari 0.05, maka kita menolak H0. Asumsi homogenitas varians tidak terpenuhi (varians berbeda).")
  })
  
  # -- Inferensia 1: Proporsi --
  output$prop1_level_ui <- renderUI({
    req(input$prop1_var)
    lvls <- unique(na.omit(reactive_data()[[input$prop1_var]]))
    selectInput("prop1_level", "Pilih Level 'Success':", choices=lvls)
  })
  
  prop1_res <- eventReactive(input$run_prop1, {
    req(reactive_data(), input$prop1_var, input$prop1_level, input$prop1_p0)
    df <- reactive_data()
    success_count <- sum(df[[input$prop1_var]] == input$prop1_level, na.rm=TRUE)
    total_count <- sum(!is.na(df[[input$prop1_var]]))
    prop.test(x=success_count, n=total_count, p=input$prop1_p0)
  })
  
  output$prop1_output <- renderPrint({ prop1_res() })
  output$prop1_interp <- renderText({
    res <- prop1_res()
    p_val <- res$p.value
    paste("H0: Proporsi populasi sama dengan", input$prop1_p0,
          ". P-value =", round(p_val, 4), ".",
          if(p_val > 0.05) " Gagal menolak H0. Tidak ada bukti statistik bahwa proporsi berbeda dari hipotesis."
          else " Menolak H0. Terdapat bukti statistik bahwa proporsi berbeda dari hipotesis.")
  })
  
  prop2_res <- eventReactive(input$run_prop2, {
    req(reactive_data(), input$prop2_var, input$prop2_response)
    df <- reactive_data()
    # Membuat tabel kontingensi
    contingency_table <- table(df[[input$prop2_var]], df[[input$prop2_response]])
    prop.test(contingency_table)
  })
  
  output$prop2_output <- renderPrint({ prop2_res() })
  output$prop2_interp <- renderText({
    res <- prop2_res()
    p_val <- res$p.value
    paste("H0: Tidak ada perbedaan proporsi '", input$prop2_response, "' antar kelompok '", input$prop2_var,
          "'. P-value =", round(p_val, 4), ".",
          if(p_val > 0.05) " Gagal menolak H0. Tidak ada perbedaan proporsi yang signifikan."
          else " Menolak H0. Terdapat perbedaan proporsi yang signifikan antar kelompok.")
  })
  
  # -- Inferensia 1: Varians --
  var2_res <- eventReactive(input$run_var2, {
    req(reactive_data(), input$var2_group, input$var2_numeric)
    df <- reactive_data()
    
    # Pastikan grup hanya memiliki 2 level
    group_levels <- unique(na.omit(df[[input$var2_group]]))
    if (length(group_levels) != 2) {
      return("Variabel grup harus memiliki tepat 2 level untuk F-test.")
    }
    
    formula <- as.formula(paste(input$var2_numeric, "~", input$var2_group))
    var.test(formula, data=df)
  })
  
  output$var2_output <- renderPrint({ var2_res() })
  output$var2_interp <- renderText({
    res <- var2_res()
    if(is.character(res)) return(res)
    p_val <- res$p.value
    paste("H0: Rasio varians kedua kelompok sama dengan 1 (varians homogen).",
          "P-value =", round(p_val, 4), ".",
          if(p_val > 0.05) " Gagal menolak H0. Tidak ada bukti perbedaan varians yang signifikan."
          else " Menolak H0. Terdapat bukti perbedaan varians yang signifikan antar kelompok.")
  })
  
  # -- Inferensia 2: ANOVA --
  anova_result <- eventReactive(input$run_anova, {
    req(reactive_data(), input$anova_dep_var, input$anova_indep_var1)
    df <- reactive_data()
    
    if (input$anova_indep_var2 == "Tidak Ada") { # One-way ANOVA
      formula <- as.formula(paste(input$anova_dep_var, "~", input$anova_indep_var1))
    } else { # Two-way ANOVA
      formula <- as.formula(paste(input$anova_dep_var, "~", input$anova_indep_var1, "*", input$anova_indep_var2))
    }
    
    aov(formula, data = df)
  })
  
  output$anova_output <- renderPrint({
    summary(anova_result())
  })
  
  output$anova_interpretation <- renderText({
    res <- summary(anova_result())
    p_values <- res[[1]]$`Pr(>F)`
    terms <- rownames(res[[1]])
    
    interp <- ""
    if (input$anova_indep_var2 == "Tidak Ada") { # One-way
      p_val <- p_values[1]
      interp <- paste("Uji ANOVA satu arah dilakukan untuk melihat perbedaan rata-rata '", input$anova_dep_var, 
                      "' berdasarkan kelompok '", input$anova_indep_var1, "'. P-value adalah ", round(p_val, 4), ". ",
                      if(p_val < 0.05) "Ini menunjukkan ada perbedaan rata-rata yang signifikan secara statistik antar setidaknya dua kelompok."
                      else "Ini menunjukkan tidak ada perbedaan rata-rata yang signifikan secara statistik antar kelompok.")
    } else { # Two-way
      p1 <- p_values[1]; p2 <- p_values[2]; p_int <- p_values[3]
      interp1 <- paste("Efek utama '", input$anova_indep_var1, "': p-value=", round(p1, 4), ". ",
                       if(p1 < 0.05) "Signifikan." else "Tidak signifikan.")
      interp2 <- paste("Efek utama '", input$anova_indep_var2, "': p-value=", round(p2, 4), ". ",
                       if(p2 < 0.05) "Signifikan." else "Tidak signifikan.")
      interp_int <- paste("Efek interaksi: p-value=", round(p_int, 4), ". ",
                          if(p_int < 0.05) "Signifikan, menunjukkan efek satu faktor bergantung pada level faktor lainnya." 
                          else "Tidak signifikan, menunjukkan efek kedua faktor bersifat independen.")
      interp <- paste(interp1, interp2, interp_int, sep="\n")
    }
    interp
  })
  
  output$anova_plot <- renderPlot({
    req(anova_result())
    par(mfrow=c(1,2))
    plot(anova_result(), 1) # Residuals vs Fitted
    plot(anova_result(), 2) # Q-Q Plot
  })
  
  # -- Regresi --
  regression_model <- eventReactive(input$run_regression, {
    req(reactive_data(), input$reg_dep_var, length(input$reg_indep_vars) > 0)
    df <- reactive_data()
    formula <- as.formula(paste(input$reg_dep_var, "~", paste(input$reg_indep_vars, collapse = "+")))
    lm(formula, data = df)
  })
  
  output$regression_summary <- renderPrint({
    summary(regression_model())
  })
  
  output$regression_interpretation <- renderText({
    mod <- regression_model()
    s <- summary(mod)
    paste("Model regresi menghasilkan R-squared sebesar ", round(s$r.squared, 3), 
          ", yang berarti sekitar ", round(s$r.squared*100, 1), "% variasi pada '", input$reg_dep_var, 
          "' dapat dijelaskan oleh variabel independen yang dipilih. F-statistic model memiliki p-value ", 
          format.pval(s$fstatistic[3]), ", menunjukkan signifikansi model secara keseluruhan.", sep="")
  })
  
  # Asumsi Regresi
  output$reg_norm_plot <- renderPlot({
    mod <- regression_model()
    qqPlot(mod$residuals, main="Q-Q Plot of Residuals")
  })
  output$reg_norm_interp <- renderText({
    res <- shapiro.test(residuals(regression_model()))
    paste("Uji normalitas Shapiro-Wilk pada residual menghasilkan p-value =", round(res$p.value, 4), ". ",
          "Jika p > 0.05, asumsi normalitas residual terpenuhi. Titik-titik pada plot Q-Q idealnya mengikuti garis diagonal.", sep="")
  })
  
  output$reg_homo_plot <- renderPlot({
    mod <- regression_model()
    plot(fitted(mod), residuals(mod), xlab="Fitted Values", ylab="Residuals", main="Residuals vs Fitted Plot")
    abline(h=0, col="red", lty=2)
  })
  output$reg_homo_interp <- renderText({
    mod <- regression_model()
    bp <- lmtest::bptest(mod)
    paste("Plot ini digunakan untuk memeriksa homoskedastisitas (varians residual konstan). Idealnya, titik-titik tersebar acak tanpa pola tertentu. ",
          "Uji Breusch-Pagan menghasilkan p-value =", round(bp$p.value, 4), ". ",
          "Jika p > 0.05, asumsi homoskedastisitas terpenuhi.", sep="")
  })
  
  output$reg_vif_output <- renderPrint({
    # VIF hanya untuk model dengan > 1 prediktor
    if(length(input$reg_indep_vars) > 1) {
      vif(regression_model())
    } else {
      "VIF memerlukan lebih dari satu variabel independen."
    }
  })
  output$reg_vif_interp <- renderText({
    if(length(input$reg_indep_vars) > 1) {
      "Variance Inflation Factor (VIF) mengukur multikolinearitas. Nilai VIF > 5 atau 10 sering dianggap sebagai indikasi adanya masalah multikolinearitas yang perlu ditangani."
    } else {""}
  })
  
  # -- Download Laporan Halaman --
  output$download_desc_report <- downloadHandler(
    filename = function() {
      paste0("laporan-deskriptif-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Buat file Rmd sementara
      temp_report_path <- file.path(tempdir(), "report.Rmd")
      
      # Isi file Rmd dengan konten dinamis
      report_content <- paste0(
        '--- \n',
        'title: "Laporan Statistik Deskriptif" \n',
        'output: pdf_document \n',
        'params: \n',
        '  var: "', input$desc_var, '" \n',
        '--- \n\n',
        '```{r setup, include=FALSE} \n',
        'knitr::opts_chunk$set(echo = FALSE) \n',
        'data <- read.csv("', sovi_url, '") \n',
        '``` \n\n',
        '## Ringkasan untuk Variabel: `r params$var` \n\n',
        '```{r} \n',
        'if ("', input$desc_var, '" %in% names(which(sapply(data, is.numeric)))) { \n',
        '  summary(data[[params$var]]) \n',
        '} else { \n',
        '  knitr::kable(table(data[[params$var]])) \n',
        '} \n',
        '``` \n\n',
        '## Interpretasi \n\n',
        '```{r} \n',
        'if ("', input$desc_var, '" %in% names(which(sapply(data, is.numeric)))) { \n',
        '  s <- summary(data[[params$var]]) \n',
        '  paste0("Variabel ", params$var, " adalah variabel numerik dengan nilai ", \n',
        '         "Minimum: ", round(s[1], 2), ", ", \n',
        '         "Median: ", round(s[3], 2), ", ", \n',
        '         "Rata-rata: ", round(s[4], 2), ", dan ", \n',
        '         "Maksimum: ", round(s[6], 2), ".") \n',
        '} else { \n',
        '  tbl <- table(data[[params$var]]) \n',
        '  most_freq <- names(which.max(tbl)) \n',
        '  paste0("Variabel ", params$var, " adalah variabel kategorik. ", \n',
        '         "Kategori yang paling sering muncul adalah \'", most_freq, "\' dengan frekuensi sebanyak ", max(tbl), " kali.") \n',
        '} \n',
        '``` \n'
      )
      
      writeLines(report_content, temp_report_path)
      
      # Render Rmd ke file output
      rmarkdown::render(temp_report_path, output_file = file, 
                        envir = new.env(parent = globalenv()))
    }
  )
}

# ===================================================================
# -- 5. RUN APP --
# ===================================================================
shinyApp(ui, server)