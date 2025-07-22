# Dashboard Analisis Kerentanan Sosial Indonesia

## Deskripsi
Dashboard interaktif untuk menganalisis kerentanan sosial di Indonesia menggunakan data SOVI (Social Vulnerability Index). Dashboard ini mengintegrasikan analisis spasial, visualisasi peta, dan matriks korelasi untuk memberikan pemahaman komprehensif tentang kerentanan sosial di berbagai daerah.

## Fitur Utama

### 1. Overview
- **Value Boxes**: Menampilkan statistik utama (total daerah, rata-rata SOVI, daerah kerentanan tinggi)
- **Distribusi Kerentanan**: Grafik batang kategori kerentanan
- **Top 10 Daerah Paling Rentan**: Tabel daerah dengan SOVI tertinggi
- **Boxplot Indikator**: Distribusi statistik untuk setiap indikator kerentanan

### 2. Peta Interaktif
- **Visualisasi Spasial**: Peta Indonesia dengan data kerentanan
- **Multiple Variables**: Pilihan variabel untuk dipetakan (SOVI, kemiskinan, pendidikan, dll)
- **Color Schemes**: Berbagai skema warna (Viridis, Plasma, Inferno, Spectral)
- **Interactive Popups**: Informasi detail untuk setiap daerah
- **Customizable Classes**: Jumlah kelas warna dapat disesuaikan

### 3. Analisis Spasial
- **Moran's I**: Perhitungan autokorelasi spasial
- **Spatial Weights Matrix**: Matriks pembobot berdasarkan jarak
- **Moran Scatterplot**: Visualisasi hubungan spasial dengan kuadran HH, HL, LH, LL
- **Customizable Threshold**: Pengaturan threshold jarak untuk analisis

### 4. Matriks Korelasi
- **Correlation Matrix**: Heatmap korelasi antar indikator
- **Multiple Methods**: Pearson, Spearman, Kendall
- **PCA Analysis**: Principal Component Analysis dengan biplot
- **Variable Contribution**: Kontribusi variabel terhadap komponen utama

### 5. Data Explorer
- **Interactive Table**: Tabel data dengan filter dan pencarian
- **Data Filtering**: Filter berdasarkan kategori kerentanan dan range SOVI
- **Descriptive Statistics**: Statistik deskriptif untuk data yang difilter
- **Data Export**: Download data dalam format CSV

## Indikator Kerentanan

Dashboard menganalisis 14 indikator kerentanan sosial:

1. **CHILDREN**: Persentase populasi di bawah 5 tahun
2. **FEMALE**: Persentase populasi perempuan
3. **ELDERLY**: Persentase populasi 65 tahun ke atas
4. **FHEAD**: Persentase rumah tangga dengan kepala rumah tangga perempuan
5. **FAMILYSIZE**: Rata-rata ukuran keluarga
6. **NOELECTRIC**: Persentase rumah tangga tanpa listrik
7. **LOWEDU**: Persentase populasi 15+ dengan pendidikan rendah
8. **GROWTH**: Persentase pertumbuhan populasi
9. **POVERTY**: Persentase penduduk miskin
10. **ILLITERATE**: Persentase populasi buta huruf
11. **NOTRAINING**: Persentase rumah tangga tanpa pelatihan bencana
12. **DPRONE**: Persentase daerah rawan bencana
13. **RENTED**: Persentase rumah tangga penyewa
14. **NOSEWER**: Persentase rumah tangga tanpa sistem pembuangan

## Metodologi Analisis

### SOVI Index Calculation
1. **Standardisasi**: Semua variabel distandarisasi menggunakan z-score
2. **Agregasi**: SOVI index = sum of standardized scores
3. **Normalisasi**: Dinormalisasi ke skala 0-100
4. **Kategorisasi**: 
   - Rendah: 0-25
   - Sedang: 25-50
   - Tinggi: 50-75
   - Sangat Tinggi: 75-100

### Spatial Analysis
1. **Distance Matrix**: Menggunakan data jarak antar daerah
2. **Spatial Weights**: Inverse distance weighting dengan threshold
3. **Moran's I**: Mengukur autokorelasi spasial
4. **Spatial Lag**: Rata-rata tertimbang nilai tetangga

### Interpretasi Moran's I
- **I > 0.3**: Klasterisasi positif kuat (daerah serupa berkelompok)
- **0.1 < I ≤ 0.3**: Klasterisasi positif sedang
- **-0.1 ≤ I ≤ 0.1**: Distribusi acak
- **I < -0.1**: Klasterisasi negatif (daerah berbeda berdekatan)

## Cara Menjalankan Dashboard

### Prerequisites
Pastikan R dan RStudio terinstall, kemudian install packages yang diperlukan:

```r
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr",
  "leaflet", "sf", "jsonlite", "spdep", "htmltools", "plotly",
  "corrplot", "RColorBrewer", "viridis", "shinyjs", "shinycssloaders"
)

install.packages(required_packages)
```

### Menjalankan Dashboard

1. **Dari RStudio**:
   ```r
   # Load file
   source("vulnera_dashboard_fixed.R")
   
   # Atau jalankan langsung
   shiny::runApp("vulnera_dashboard_fixed.R")
   ```

2. **Dari Command Line**:
   ```bash
   Rscript -e "shiny::runApp('vulnera_dashboard_fixed.R')"
   ```

3. **Deploy ke Shiny Server**:
   Upload file `vulnera_dashboard_fixed.R` ke server Shiny

## Data Sources

1. **SOVI Data**: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv
2. **Distance Matrix**: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv
3. **GeoJSON**: File geometri kabupaten/kota Indonesia

## Troubleshooting

### Masalah Umum:
1. **Package tidak terinstall**: Jalankan `install.packages()` untuk packages yang missing
2. **Data tidak ter-load**: Dashboard akan menggunakan sample data jika koneksi internet bermasalah
3. **Peta tidak muncul**: Pastikan koneksi internet untuk tiles peta
4. **Error spatial analysis**: Periksa dimensi data dan matriks jarak

### Performance Tips:
1. **Large Dataset**: Dashboard dibatasi 100 observasi untuk performa optimal
2. **Memory Usage**: Tutup tab browser yang tidak digunakan
3. **Slow Loading**: Tunggu hingga semua data ter-load sepenuhnya

## Pengembangan Lebih Lanjut

### Fitur yang Bisa Ditambahkan:
1. **Real GeoJSON Integration**: Integrasi dengan file GeoJSON aktual Indonesia
2. **Time Series Analysis**: Analisis perubahan kerentanan dari waktu ke waktu
3. **Advanced Clustering**: Algoritma clustering untuk pengelompokan daerah
4. **Predictive Modeling**: Model prediksi kerentanan
5. **Export Maps**: Export peta sebagai PNG/PDF
6. **API Integration**: Integrasi dengan API data pemerintah

### Customization:
1. **Theme**: Ubah tema dan warna dashboard
2. **Additional Indicators**: Tambahkan indikator kerentanan baru
3. **Regional Focus**: Fokus analisis pada provinsi/region tertentu
4. **Multi-language**: Support bahasa Indonesia dan Inggris

## Referensi

1. Cutter, S.L., Boruff, B.J., Shirley, W.L. (2003). Social vulnerability to environmental hazards
2. Flanagan, B.E., et al. (2011). A Social Vulnerability Index for Disaster Management
3. Tate, E. (2012). Social vulnerability indices: a comparative assessment

## Lisensi

Dashboard ini dibuat untuk tujuan edukasi dan penelitian. Silakan gunakan dan modifikasi sesuai kebutuhan.

## Kontak

Untuk pertanyaan atau saran perbaikan, silakan buat issue atau pull request.
