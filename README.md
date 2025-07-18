# SoVi Explorer Pro - Dashboard Analisis Kerentanan Sosial Terpadu

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge&logo=rstudio&logoColor=white)](https://shiny.rstudio.com/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)](https://opensource.org/licenses/MIT)

## 📋 Deskripsi

**SoVi Explorer Pro** adalah dashboard interaktif yang dibangun dengan R Shiny untuk analisis komprehensif data Social Vulnerability Index (SoVI). Dashboard ini dikembangkan sebagai tugas akhir mata kuliah Statistika Terapan dengan fitur-fitur analisis statistik yang lengkap.

## 🎯 Fitur Utama

### 🏠 **Beranda**
- Metadata dan informasi dashboard
- Pemuatan data otomatis dari sumber online
- Status dan ringkasan dataset

### 📊 **Manajemen Data**
- Overview dan struktur data
- Analisis missing values dengan visualisasi
- Kategorisasi variabel kontinyu (kuartil, tersil, median)
- Preprocessing data dengan interpretasi

### 🔍 **Eksplorasi Data**
- **Statistik Deskriptif**: Ringkasan lengkap dengan interpretasi
- **Visualisasi Univariat**: Histogram, boxplot, density plot
- **Visualisasi Bivariat**: Scatter plot, correlation matrix
- **Peta Interaktif**: Visualisasi spasial data SOVI

### 📏 **Uji Asumsi**
- **Uji Normalitas**: Shapiro-Wilk test dengan Q-Q plot
- **Uji Homogenitas**: Levene test dengan visualisasi
- Interpretasi lengkap untuk setiap uji

### 📈 **Statistik Inferensia I**
- **Uji Proporsi**: Satu sampel dengan interpretasi
- **Uji Varians**: Satu dan dua kelompok
- Visualisasi dan penjelasan hasil

### 📉 **Statistik Inferensia II (ANOVA)**
- **ANOVA Satu Arah**: Dengan post-hoc Tukey HSD
- **ANOVA Dua Arah**: Dengan uji interaksi
- Diagnostic plots dan interpretasi

### 🎯 **Regresi Linear Berganda**
- Model regresi dengan multiple predictors
- **Uji Asumsi Regresi**: Linearitas, normalitas, homoskedastisitas, independensi
- **Prediksi**: Input nilai baru untuk prediksi
- Diagnostic plots lengkap

### 💾 **Download & Export**
- **PDF Report**: Laporan lengkap hasil analisis
- **Excel Export**: Data dan hasil statistik
- **Image Export**: Semua grafik dan visualisasi
- **Multiple Formats**: Word, CSV, R script

## 🚀 Instalasi dan Penggunaan

### Prerequisites
- R (versi 4.0 atau lebih tinggi)
- RStudio (opsional, tapi direkomendasikan)

### Instalasi Package
Dashboard akan otomatis menginstal package yang diperlukan saat dijalankan pertama kali:

```r
# Package yang akan diinstal otomatis:
- shiny, shinydashboard, DT, plotly, ggplot2
- leaflet, corrplot, car, broom, psych
- VIM, cluster, factoextra, rmarkdown
- knitr, downloadthis, openxlsx, flextable, officer
```

### Menjalankan Dashboard

1. **Clone repository:**
```bash
git clone https://github.com/Harry123Rum/tugas.git
cd tugas
```

2. **Jalankan dashboard:**
```r
# Buka R atau RStudio, lalu:
source("sovi_dashboard.R")
```

3. **Akses dashboard:**
   - Dashboard akan terbuka di browser default
   - Klik tombol "📥 Muat Data" di halaman beranda
   - Jelajahi berbagai menu analisis

## 📊 Sumber Data

Dashboard menggunakan data SOVI (Social Vulnerability Index) dari:

- **Data SOVI**: [sovi_data.csv](https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv)
- **Matriks Jarak**: [distance.csv](https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv)
- **Metadata**: [Research Article](https://www.sciencedirect.com/science/article/pii/S2352340921010180)

## 🔧 Struktur File

```
tugas/
├── sovi_dashboard.R          # File utama dashboard
├── template.R               # Template tambahan
├── README.md               # Dokumentasi ini
├── requirements.txt        # Daftar package R
└── docs/                  # Dokumentasi tambahan
    ├── user_guide.md      # Panduan pengguna
    └── screenshots/       # Screenshot dashboard
```

## 📖 Panduan Penggunaan

### 1. Memulai Analisis
1. Jalankan dashboard dan klik "Muat Data"
2. Tunggu hingga data berhasil dimuat
3. Navigasi menggunakan menu sidebar

### 2. Analisis Bertahap
1. **Manajemen Data**: Eksplorasi dan preprocessing
2. **Eksplorasi Data**: Memahami karakteristik data
3. **Uji Asumsi**: Verifikasi asumsi statistik
4. **Inferensia**: Uji hipotesis sesuai kebutuhan
5. **Regresi**: Analisis hubungan antar variabel
6. **Download**: Ekspor hasil analisis

### 3. Interpretasi Hasil
- Setiap output dilengkapi interpretasi dalam bahasa Indonesia
- Panduan statistical significance dan practical significance
- Visualisasi mendukung interpretasi kuantitatif

## 🎓 Konteks Akademik

Dashboard ini dikembangkan untuk:
- **Mata Kuliah**: Statistika Terapan
- **Tujuan**: Tugas Akhir Semester
- **Fokus**: Analisis praktis data SOVI dengan metode statistik komprehensif
- **Deployment**: Wajib deploy (dapat menggunakan shinyapps.io)

## 📋 Kriteria yang Dipenuhi

✅ Dashboard R Shiny dengan multiple menu  
✅ Beranda dengan metadata dan informasi  
✅ Manajemen data dengan kategorisasi variabel  
✅ Eksplorasi data dengan statistik deskriptif dan visualisasi  
✅ Peta interaktif untuk analisis spasial  
✅ Uji asumsi (normalitas dan homogenitas)  
✅ Statistik inferensia (proporsi, varians, 1-2 kelompok)  
✅ ANOVA (satu arah dan dua arah)  
✅ Regresi linear berganda dengan uji asumsi  
✅ Download dalam multiple format (JPG, PDF, Word)  
✅ Interpretasi lengkap untuk setiap output  
✅ Nama dashboard yang unik  

## 🚀 Deployment

### Option 1: ShinyApps.io (Recommended)
```r
# Install rsconnect
install.packages("rsconnect")
library(rsconnect)

# Setup account (sekali saja)
rsconnect::setAccountInfo(name='your-account', 
                          token='your-token', 
                          secret='your-secret')

# Deploy
rsconnect::deployApp(appFiles = "sovi_dashboard.R")
```

### Option 2: Local Server
```r
# Jalankan secara lokal
shiny::runApp("sovi_dashboard.R", port = 3838)
```

## 🤝 Kontribusi

Project ini dikembangkan untuk tugas akademik. Untuk saran atau perbaikan:

1. Fork repository
2. Buat feature branch (`git checkout -b feature/improvement`)
3. Commit changes (`git commit -am 'Add improvement'`)
4. Push branch (`git push origin feature/improvement`)
5. Buat Pull Request

## 📝 Lisensi

Project ini menggunakan lisensi MIT. Lihat file [LICENSE](LICENSE) untuk detail.

## 👨‍💻 Author

**Harry123Rum**
- GitHub: [@Harry123Rum](https://github.com/Harry123Rum)
- Repository: [tugas](https://github.com/Harry123Rum/tugas)

## 🙏 Acknowledgments

- **Data Source**: SOVI data dari naspaclust repository
- **Framework**: R Shiny ecosystem
- **Inspiration**: Kebutuhan analisis kerentanan sosial yang komprehensif
- **Academic Support**: Mata kuliah Statistika Terapan

---

## 📊 Preview Dashboard

*Dashboard preview akan ditambahkan setelah deployment*

### Beranda
![Beranda Preview](docs/screenshots/beranda.png)

### Eksplorasi Data
![Eksplorasi Preview](docs/screenshots/eksplorasi.png)

### Analisis Regresi
![Regresi Preview](docs/screenshots/regresi.png)

---

**Note**: Dashboard ini siap untuk deployment dan memenuhi semua requirements tugas akhir mata kuliah Statistika Terapan.