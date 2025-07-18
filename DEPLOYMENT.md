# 🚀 Panduan Deployment SoVi Explorer Pro

## 📋 Ringkasan

Dashboard **SoVi Explorer Pro** dapat di-deploy menggunakan beberapa metode. Dokumen ini memberikan panduan lengkap untuk setiap metode deployment.

## 🎯 Metode Deployment

### 1. ShinyApps.io (Recommended) ⭐

ShinyApps.io adalah platform cloud gratis dari RStudio untuk hosting aplikasi Shiny.

#### Langkah-langkah:

1. **Buat Akun ShinyApps.io**
   - Kunjungi [shinyapps.io](https://www.shinyapps.io/)
   - Daftar akun gratis
   - Catat nama akun, token, dan secret

2. **Setup di R/RStudio**
```r
# Install package rsconnect
install.packages("rsconnect")
library(rsconnect)

# Konfigurasi akun (sekali saja)
rsconnect::setAccountInfo(
  name   = 'nama-akun-anda',
  token  = 'token-dari-shinyapps',
  secret = 'secret-dari-shinyapps'
)
```

3. **Deploy Dashboard**
```r
# Deploy aplikasi
rsconnect::deployApp(
  appFiles = c("sovi_dashboard.R", "requirements.txt"),
  appName = "sovi-explorer-pro"
)
```

4. **Akses Dashboard**
   - URL: `https://nama-akun-anda.shinyapps.io/sovi-explorer-pro/`

#### Keuntungan:
- ✅ Gratis (dengan limitasi)
- ✅ Setup mudah
- ✅ URL publik langsung
- ✅ SSL otomatis

#### Limitasi Gratis:
- 25 jam aktif per bulan
- 5 aplikasi maksimal
- Memory terbatas

---

### 2. Shiny Server (Local/VPS) 🖥️

Deploy di server sendiri untuk kontrol penuh.

#### Requirements:
- Ubuntu/CentOS server
- R terinstall
- Shiny Server

#### Setup Ubuntu:

1. **Install R**
```bash
sudo apt update
sudo apt install -y r-base r-base-dev
```

2. **Install Shiny Server**
```bash
# Download dan install
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.20.1002-amd64.deb
sudo dpkg -i shiny-server-1.5.20.1002-amd64.deb
```

3. **Install Package R**
```r
sudo R
install.packages(c("shiny", "shinydashboard", "DT", "plotly", "ggplot2", 
                   "leaflet", "corrplot", "car", "broom", "psych", "VIM", 
                   "cluster", "factoextra", "rmarkdown", "knitr", "downloadthis", 
                   "openxlsx", "flextable", "officer", "reshape2"))
```

4. **Deploy Dashboard**
```bash
# Copy file ke direktori Shiny Server
sudo cp sovi_dashboard.R /srv/shiny-server/sovi-explorer/app.R
sudo systemctl restart shiny-server
```

5. **Akses Dashboard**
   - URL: `http://server-ip:3838/sovi-explorer/`

---

### 3. Docker Deployment 🐳

Containerize dashboard untuk deployment yang konsisten.

#### Dockerfile:
```dockerfile
FROM rocker/shiny-verse:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'plotly', 'ggplot2', 'leaflet', 'corrplot', 'car', 'broom', 'psych', 'VIM', 'cluster', 'factoextra', 'rmarkdown', 'knitr', 'downloadthis', 'openxlsx', 'flextable', 'officer', 'reshape2'), repos='https://cran.rstudio.com/')"

# Copy dashboard
COPY sovi_dashboard.R /srv/shiny-server/app.R

# Expose port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
```

#### Build dan Run:
```bash
# Build image
docker build -t sovi-explorer-pro .

# Run container
docker run -d -p 3838:3838 --name sovi-dashboard sovi-explorer-pro
```

---

### 4. GitHub Pages + ShinyApps.io Integration 🔗

Kombinasi GitHub Pages untuk dokumentasi dan ShinyApps.io untuk aplikasi.

#### Setup:
1. Push code ke GitHub
2. Enable GitHub Pages di repository settings
3. Deploy dashboard ke ShinyApps.io
4. Link dari GitHub Pages ke dashboard

---

## 🔧 Konfigurasi Lanjutan

### Environment Variables
```r
# Untuk konfigurasi berbeda berdasarkan environment
if (Sys.getenv("SHINY_ENV") == "production") {
  # Production settings
  options(shiny.maxRequestSize = 50*1024^2)  # 50MB
} else {
  # Development settings
  options(shiny.maxRequestSize = 10*1024^2)  # 10MB
}
```

### Performance Optimization
```r
# Di sovi_dashboard.R, tambahkan:
options(shiny.usecairo = FALSE)  # Untuk performa grafik
options(repos = "https://cran.rstudio.com/")  # Repo stabil
```

---

## 📊 Monitoring & Maintenance

### Monitoring Dashboard
- **ShinyApps.io**: Built-in analytics
- **Shiny Server**: Log files di `/var/log/shiny-server/`
- **Docker**: `docker logs container-name`

### Update Dashboard
1. Update code lokal
2. Test perubahan
3. Re-deploy:
   - ShinyApps.io: `rsconnect::deployApp()`
   - Shiny Server: Copy file dan restart service
   - Docker: Rebuild image dan restart container

---

## 🚨 Troubleshooting

### Common Issues:

1. **Package Installation Error**
```r
# Solution: Install dari source
install.packages("package-name", type = "source")
```

2. **Memory Limit**
```r
# Tambahkan di awal dashboard
options(shiny.maxRequestSize = 100*1024^2)  # 100MB
```

3. **Timeout Issues**
```r
# Increase timeout
options(shiny.idletimeout = 300)  # 5 minutes
```

4. **Port Already in Use**
```bash
# Kill process menggunakan port 3838
sudo lsof -ti:3838 | xargs kill -9
```

---

## 📋 Checklist Deployment

### Pre-Deployment:
- [ ] Semua package terinstall
- [ ] Dashboard berjalan lokal tanpa error
- [ ] Data source dapat diakses
- [ ] Test semua fitur

### Post-Deployment:
- [ ] Dashboard dapat diakses via URL
- [ ] Semua menu berfungsi
- [ ] Download feature bekerja
- [ ] Performance acceptable
- [ ] Error handling proper

---

## 🎓 Untuk Tugas Akademik

### Requirement Dosen: "Wajib Deploy"

**Rekomendasi untuk tugas:**
1. **ShinyApps.io** - Termudah dan gratis
2. Dokumentasikan URL deployment
3. Screenshot dashboard di README
4. Include deployment guide dalam laporan

### Template Laporan Deployment:
```
Dashboard telah berhasil di-deploy di:
URL: https://username.shinyapps.io/sovi-explorer-pro/

Metode Deployment: ShinyApps.io
Tanggal Deploy: [tanggal]
Status: Aktif dan dapat diakses publik

Fitur yang telah diverifikasi:
✅ Loading data SOVI
✅ Semua menu navigasi
✅ Analisis statistik
✅ Download laporan
✅ Visualisasi interaktif
```

---

## 📞 Support

Jika mengalami masalah deployment:
1. Check log files untuk error details
2. Verifikasi semua dependencies
3. Test ulang di environment lokal
4. Konsultasi dokumentasi platform deployment

**Happy Deploying! 🚀**