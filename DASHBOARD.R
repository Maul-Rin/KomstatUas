library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(car)
library(tidyr)
library(shinyWidgets)
library(shinyjs)
library(stats)
library(psych)
library(knitr)
library(rmarkdown)

# Load data
sovi_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
distance_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"

# Ensure data loading is robust
sovi_data <- tryCatch(read_csv(sovi_url), error = function(e) NULL)
distance_matrix <- tryCatch(read_csv(distance_url), error = function(e) NULL)

if (is.null(sovi_data) || is.null(distance_matrix)) {
  stop("Failed to load data. Please check the URLs and your internet connection.")
}

# Add 'Province' column for mapping if not present (example for map visualization)
# Assuming 'NAME' could be a proxy for a geographical identifier or you'd have actual lat/lon in sovi_data
# For a proper map, sovi_data would need latitude and longitude columns or a spatial object.
# Adding a placeholder for now.
if (!"latitude" %in% names(sovi_data)) {
  sovi_data$latitude <- runif(nrow(sovi_data), -10, -5) # Example latitude for Indonesia
}
if (!"longitude" %in% names(sovi_data)) {
  sovi_data$longitude <- runif(nrow(sovi_data), 105, 115) # Example longitude for Indonesia
}


ui <- dashboardPage(
  dashboardHeader(title = "SoViStat Dashboard Analisis", titleWidth = 300), # Unique name 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("cogs")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("flask"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_beda_rata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "uji_prop_var"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .shiny-output-error-validation {
          color: red;
          font-weight: bold;
        }
      "))
    ),
    tabItems(
      # Beranda Tab [cite: 22]
      tabItem(tabName = "beranda",
              h2("Selamat Datang di SoViStat Dashboard Analisis", align = "center"),
              fluidRow(
                box(
                  title = "Tentang Dashboard",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Dashboard ini dikembangkan untuk menganalisis dan memvisualisasikan data Social Vulnerability Index (SOVI). Ini adalah bagian dari Ujian Akhir Semester mata kuliah Komputasi Statistik di Politeknik Statistika STIS."),
                  p("Tujuan utama dashboard ini adalah untuk memfasilitasi eksplorasi data, manajemen, uji asumsi, inferensi statistik, dan analisis regresi, dengan output yang jelas dan mudah diinterpretasikan."),
                  p("Data yang digunakan bersumber dari:"),
                  tags$ul(
                    tags$li(tags$a(href = sovi_url, "Data SoVI (sovi_data.csv)")),
                    tags$li(tags$a(href = distance_url, "Matriks Penimbang Jarak (distance.csv)"))
                  ),
                  p("Untuk informasi lebih lanjut mengenai metadata dan fakta integritas, silakan kunjungi tautan berikut:"),
                  tags$ul(
                    tags$li(tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180", "Metadata Artikel [cite: 9]")),
                    tags$li(tags$a(href = "https://s.stis.ac.id/Fakta_integritas_KOMSTAT", "Fakta Integritas [cite: 9]"))
                  )
                )
              )
      ),
      
      # Manajemen Data Tab 
      tabItem(tabName = "manajemen",
              h2("Manajemen Data", align = "center"),
              fluidRow(
                box(
                  title = "Kategorisasi Data Kontinu",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("var_cat", "Pilih Variabel Kontinu untuk Dikategorikan:", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  sliderInput("breaks", "Jumlah Kategori:", min = 2, max = 10, value = 3, step = 1),
                  actionButton("categorize_btn", "Kategorikan Data")
                ),
                box(
                  title = "Tabel Data Setelah Kategorisasi",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("tabel_kat"),
                  h4("Interpretasi:"),
                  p("Tabel di atas menunjukkan data asli dengan penambahan kolom baru yang merupakan versi kategorik dari variabel kontinu yang Anda pilih. Data dikategorikan berdasarkan rentang nilai yang sama, dengan jumlah kategori yang ditentukan oleh slider. Ini berguna untuk analisis yang memerlukan data kategorik, seperti uji chi-kuadrat atau visualisasi berbasis kategori."),
                  downloadButton("download_manajemen_csv", "Unduh Data Kategorik (CSV)"),
                  downloadButton("download_manajemen_pdf", "Unduh Laporan Manajemen Data (PDF)")
                )
              )
      ),
      
      # Eksplorasi Data Tab 
      tabItem(tabName = "eksplorasi",
              h2("Eksplorasi Data", align = "center"),
              fluidRow(
                box(
                  title = "Statistik Deskriptif",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("var_explorasi_desc", "Pilih Variabel untuk Statistik Deskriptif:", names(sovi_data)),
                  verbatimTextOutput("summary_var"),
                  h4("Interpretasi Statistik Deskriptif:"),
                  p("Bagian ini menampilkan ringkasan statistik dasar (min, max, median, mean, kuartil) untuk variabel yang dipilih. Ini memberikan gambaran cepat tentang distribusi dan nilai-nilai sentral data.")
                ),
                box(
                  title = "Visualisasi Data (Histogram)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 8,
                  selectInput("var_plot_hist", "Pilih Variabel untuk Histogram:", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  plotOutput("histPlot"),
                  h4("Interpretasi Histogram:"),
                  p("Histogram menunjukkan distribusi frekuensi dari variabel yang dipilih. Bentuk histogram dapat mengindikasikan simetri, kemencengan (skewness), atau keberadaan outlier dalam data."),
                  downloadButton("download_hist_png", "Unduh Histogram (PNG)"),
                  downloadButton("download_hist_pdf", "Unduh Laporan Histogram (PDF)")
                )
              ),
              fluidRow(
                box(
                  title = "Visualisasi Data (Boxplot)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("var_plot_box", "Pilih Variabel untuk Boxplot:", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  plotOutput("boxPlot"),
                  h4("Interpretasi Boxplot:"),
                  p("Boxplot menyajikan ringkasan visual dari lima angka statistik (minimum, kuartil pertama, median, kuartil ketiga, dan maksimum) serta menunjukkan potensi outlier. Ini membantu dalam memahami sebaran, pusat, dan simetri data."),
                  downloadButton("download_box_png", "Unduh Boxplot (PNG)"),
                  downloadButton("download_box_pdf", "Unduh Laporan Boxplot (PDF)")
                ),
                box(
                  title = "Visualisasi Data (Peta - Lokasi Data)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  p("Peta ini menunjukkan lokasi-lokasi yang terkait dengan data SOVI (jika data memiliki koordinat geografis)."),
                  leafletOutput("map"),
                  h4("Interpretasi Peta:"),
                  p("Peta ini memvisualisasikan distribusi geografis dari unit observasi dalam dataset. Anda dapat melihat konsentrasi data di area tertentu, yang bisa memberikan wawasan spasial untuk analisis Anda. (Catatan: Visualisasi peta ini memerlukan kolom lintang dan bujur dalam dataset Anda untuk berfungsi optimal)."),
                  downloadButton("download_map_png", "Unduh Peta (PNG)"),
                  downloadButton("download_map_pdf", "Unduh Laporan Peta (PDF)")
                )
              ),
              fluidRow(
                box(
                  title = "Tabel Data Lengkap",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("full_data_table"),
                  h4("Interpretasi Tabel Data:"),
                  p("Tabel ini menampilkan seluruh dataset. Anda dapat menggunakan fitur pencarian dan pengurutan untuk menjelajahi data secara detail."),
                  downloadButton("download_full_data_csv", "Unduh Data Lengkap (CSV)"),
                  downloadButton("download_full_data_pdf", "Unduh Laporan Data Lengkap (PDF)")
                )
              )
      ),
      
      # Uji Asumsi Tab 
      tabItem(tabName = "asumsi",
              h2("Uji Asumsi Data", align = "center"),
              fluidRow(
                box(
                  title = "Uji Normalitas",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("var_norm", "Pilih Variabel untuk Uji Normalitas:", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  plotOutput("qqplot"),
                  verbatimTextOutput("uji_norm"),
                  h4("Interpretasi Uji Normalitas (Shapiro-Wilk dan Q-Q Plot):"),
                  p("Uji Shapiro-Wilk (output di atas) menguji hipotesis nol bahwa data berdistribusi normal. Nilai p-value yang kecil (misalnya < 0.05) menunjukkan bukti untuk menolak hipotesis nol, artinya data tidak berdistribusi normal. Q-Q plot membandingkan kuantil data dengan kuantil dari distribusi normal teoritis. Jika titik-titik mendekati garis diagonal, data dianggap normal."),
                  downloadButton("download_norm_png", "Unduh Q-Q Plot (PNG)"),
                  downloadButton("download_norm_pdf", "Unduh Laporan Uji Normalitas (PDF)")
                ),
                box(
                  title = "Uji Homogenitas Varians",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("var_homogen_val", "Pilih Variabel Respon (Numerik):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  selectInput("var_homogen_group", "Pilih Variabel Grup (Kategorik):", names(sovi_data[!sapply(sovi_data, is.numeric)])),
                  verbatimTextOutput("uji_var"),
                  h4("Interpretasi Uji Homogenitas (Levene's Test):"),
                  p("Uji Levene menguji hipotesis nol bahwa varians antar kelompok adalah homogen. Nilai p-value yang kecil (misalnya < 0.05) menunjukkan bukti untuk menolak hipotesis nol, artinya varians antar kelompok tidak homogen. Homogenitas varians adalah asumsi penting untuk beberapa uji statistik parametrik seperti ANOVA dan t-test independen."),
                  downloadButton("download_homo_pdf", "Unduh Laporan Uji Homogenitas (PDF)")
                )
              )
      ),
      
      # Statistik Inferensia Tab - Uji Beda Rata-rata [cite: 26, 28]
      tabItem(tabName = "uji_beda_rata",
              h2("Statistik Inferensia: Uji Beda Rata-rata", align = "center"),
              fluidRow(
                box(
                  title = "Uji T-Test Satu Sampel",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  numericInput("mu", "Nilai Rata-rata Hipotesis (μ₀):", 0, min = -Inf, max = Inf),
                  selectInput("ttest_var_one", "Pilih Variabel (Numerik):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  actionButton("run_ttest_one", "Jalankan Uji T Satu Sampel"),
                  verbatimTextOutput("hasil_ttest_one"),
                  h4("Interpretasi Uji T-Test Satu Sampel:"),
                  p("Uji t satu sampel digunakan untuk menentukan apakah rata-rata sampel berbeda secara signifikan dari nilai rata-rata populasi yang diketahui atau dihipotesiskan (μ₀). Perhatikan nilai p-value: jika p-value < tingkat signifikansi (misalnya 0.05), kita menolak hipotesis nol bahwa rata-rata sampel sama dengan μ₀. Interval kepercayaan memberikan rentang nilai di mana rata-rata populasi kemungkinan besar berada."),
                  downloadButton("download_ttest_one_pdf", "Unduh Laporan Uji T Satu Sampel (PDF)")
                ),
                box(
                  title = "Uji T-Test Dua Sampel (Independen)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("group_two_samp", "Pilih Variabel Grup (Kategorik dengan 2 Level):", names(sovi_data[!sapply(sovi_data, is.numeric)])),
                  selectInput("nilai_two_samp", "Pilih Variabel Nilai (Numerik):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  checkboxInput("var_equal", "Asumsikan Varians Sama (Uji T Student)", value = TRUE),
                  actionButton("run_ttest_two", "Jalankan Uji T Dua Sampel"),
                  verbatimTextOutput("hasil_ttest_two"),
                  h4("Interpretasi Uji T-Test Dua Sampel:"),
                  p("Uji t dua sampel independen membandingkan rata-rata dua kelompok independen. Hipotesis nolnya adalah tidak ada perbedaan signifikan antara rata-rata kedua kelompok. P-value yang rendah menunjukkan perbedaan yang signifikan. 'var.equal' menentukan apakah uji Welch (varian tidak sama) atau uji Student (varian sama) yang digunakan. Periksa asumsi homogenitas varians sebelum memilih opsi ini."),
                  downloadButton("download_ttest_two_pdf", "Unduh Laporan Uji T Dua Sampel (PDF)")
                )
              )
      ),
      
      # Statistik Inferensia Tab - Uji Proporsi & Varians [cite: 34, 36]
      tabItem(tabName = "uji_prop_var",
              h2("Statistik Inferensia: Uji Proporsi & Uji Varians", align = "center"),
              fluidRow(
                box(
                  title = "Uji Proporsi Satu Sampel",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("prop_var_one", "Pilih Variabel (Binomial/Kategorik):", names(sovi_data)),
                  numericInput("success_level", "Jumlah Sukses (Ketikkan nilai numerik):", value = 0, min = 0),
                  numericInput("total_trials", "Jumlah Total Percobaan (Ketikkan nilai numerik):", value = 0, min = 0),
                  numericInput("p_hyp_one", "Proporsi Hipotesis (p₀):", 0.5, min = 0, max = 1),
                  actionButton("run_prop_one", "Jalankan Uji Proporsi Satu Sampel"),
                  verbatimTextOutput("hasil_prop_one"),
                  h4("Interpretasi Uji Proporsi Satu Sampel:"),
                  p("Uji proporsi satu sampel digunakan untuk menilai apakah proporsi sampel berbeda secara signifikan dari proporsi populasi yang dihipotesiskan (p₀). Input 'Jumlah Sukses' dan 'Jumlah Total Percobaan' harus diisi manual berdasarkan ringkasan variabel kategorik yang Anda pilih. P-value yang rendah menunjukkan bukti menolak hipotesis nol, yaitu proporsi sampel berbeda dari p₀."),
                  downloadButton("download_prop_one_pdf", "Unduh Laporan Uji Proporsi Satu Sampel (PDF)")
                ),
                box(
                  title = "Uji Proporsi Dua Sampel",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("prop_var_two", "Pilih Variabel (Binomial/Kategorik):", names(sovi_data)),
                  selectInput("group_prop_two", "Pilih Variabel Grup (Kategorik dengan 2 Level):", names(sovi_data[!sapply(sovi_data, is.numeric)])),
                  numericInput("success1", "Jumlah Sukses Grup 1:", value = 0, min = 0),
                  numericInput("total1", "Jumlah Total Grup 1:", value = 0, min = 0),
                  numericInput("success2", "Jumlah Sukses Grup 2:", value = 0, min = 0),
                  numericInput("total2", "Jumlah Total Grup 2:", value = 0, min = 0),
                  actionButton("run_prop_two", "Jalankan Uji Proporsi Dua Sampel"),
                  verbatimTextOutput("hasil_prop_two"),
                  h4("Interpretasi Uji Proporsi Dua Sampel:"),
                  p("Uji proporsi dua sampel membandingkan proporsi keberhasilan antara dua kelompok independen. Input 'Jumlah Sukses' dan 'Jumlah Total' untuk setiap grup harus diisi berdasarkan data Anda. P-value yang rendah menunjukkan perbedaan yang signifikan antara proporsi kedua kelompok.")
                )
              ),
              fluidRow(
                box(
                  title = "Uji Varians Satu Sampel (Chi-square test for variance)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("var_var_one", "Pilih Variabel (Numerik):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  numericInput("var_hyp_one", "Varians Hipotesis (σ²₀):", 1, min = 0, step = 0.1),
                  actionButton("run_var_one", "Jalankan Uji Varians Satu Sampel"),
                  verbatimTextOutput("hasil_var_one"),
                  h4("Interpretasi Uji Varians Satu Sampel:"),
                  p("Uji ini menilai apakah varians sampel berbeda secara signifikan dari varians populasi yang dihipotesiskan (σ²₀). Perhatikan p-value untuk menarik kesimpulan. (Catatan: Uji ini mengasumsikan data berasal dari populasi normal.)"),
                  downloadButton("download_var_one_pdf", "Unduh Laporan Uji Varians Satu Sampel (PDF)")
                ),
                box(
                  title = "Uji Varians Dua Sampel (F-test)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("var_var_two_val", "Pilih Variabel Nilai (Numerik):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  selectInput("var_var_two_group", "Pilih Variabel Grup (Kategorik dengan 2 Level):", names(sovi_data[!sapply(sovi_data, is.numeric)])),
                  actionButton("run_var_two", "Jalankan Uji Varians Dua Sampel"),
                  verbatimTextOutput("hasil_var_two"),
                  h4("Interpretasi Uji Varians Dua Sampel:"),
                  p("Uji F dua sampel digunakan untuk membandingkan varians dari dua kelompok independen. Hipotesis nolnya adalah varians kedua kelompok adalah sama. P-value yang rendah menunjukkan perbedaan yang signifikan dalam varians. Ini sering digunakan sebagai uji pendahuluan sebelum t-test dua sampel.")
                )
              )
      ),
      
      # Statistik Inferensia Tab - ANOVA [cite: 41, 43]
      tabItem(tabName = "anova",
              h2("Statistik Inferensia: ANOVA", align = "center"),
              fluidRow(
                box(
                  title = "ANOVA Satu Arah",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("respon_anova_one", "Variabel Respon (Numerik):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  selectInput("faktor_anova_one", "Faktor (Kategorik):", names(sovi_data[!sapply(sovi_data, is.numeric)])),
                  actionButton("run_anova_one", "Jalankan ANOVA Satu Arah"),
                  verbatimTextOutput("hasil_anova_one"),
                  h4("Interpretasi ANOVA Satu Arah:"),
                  p("ANOVA satu arah digunakan untuk menguji apakah ada perbedaan signifikan pada rata-rata tiga atau lebih kelompok independen yang dikelompokkan berdasarkan satu faktor kategorik. Hipotesis nol adalah bahwa semua rata-rata kelompok adalah sama. P-value yang kecil (misalnya < 0.05) menunjukkan bahwa setidaknya ada satu kelompok yang rata-ratanya berbeda secara signifikan. Jika signifikan, Anda mungkin perlu uji post-hoc."),
                  downloadButton("download_anova_one_pdf", "Unduh Laporan ANOVA Satu Arah (PDF)")
                ),
                box(
                  title = "ANOVA Dua Arah",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("respon_anova_two", "Variabel Respon (Numerik):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  selectInput("faktor1_anova_two", "Faktor 1 (Kategorik):", names(sovi_data[!sapply(sovi_data, is.numeric)])),
                  selectInput("faktor2_anova_two", "Faktor 2 (Kategorik):", names(sovi_data[!sapply(sovi_data, is.numeric)])),
                  actionButton("run_anova_two", "Jalankan ANOVA Dua Arah"),
                  verbatimTextOutput("hasil_anova_two"),
                  h4("Interpretasi ANOVA Dua Arah:"),
                  p("ANOVA dua arah digunakan untuk menguji efek dua faktor kategorik independen (dan interaksinya) pada satu variabel respon numerik. Output akan menunjukkan signifikansi untuk masing-masing faktor utama dan efek interaksi mereka. P-value yang rendah menandakan efek signifikan."),
                  downloadButton("download_anova_two_pdf", "Unduh Laporan ANOVA Dua Arah (PDF)")
                )
              )
      ),
      
      # Regresi Tab 
      tabItem(tabName = "regresi",
              h2("Regresi Linear Berganda", align = "center"),
              fluidRow(
                box(
                  title = "Model Regresi",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("y_var", "Variabel Respon (Y):", names(sovi_data[sapply(sovi_data, is.numeric)])),
                  selectInput("x_var", "Variabel Prediktor (X's):", names(sovi_data[sapply(sovi_data, is.numeric)]), multiple = TRUE),
                  actionButton("run_reg", "Jalankan Regresi"),
                  verbatimTextOutput("reg_output"),
                  h4("Interpretasi Model Regresi:"),
                  p("Output regresi menunjukkan koefisien model, standar error, t-value, dan p-value untuk setiap prediktor. P-value yang kecil (< 0.05) menunjukkan bahwa prediktor tersebut signifikan secara statistik dalam memprediksi variabel respon. R-squared menunjukkan proporsi variabilitas variabel respon yang dapat dijelaskan oleh model."),
                  downloadButton("download_reg_summary_pdf", "Unduh Laporan Ringkasan Regresi (PDF)")
                ),
                box(
                  title = "Uji Asumsi Regresi",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  h4("Plot Residuals vs Fitted:"),
                  plotOutput("residual_fitted_plot"),
                  h4("Interpretasi Plot Residuals vs Fitted:"),
                  p("Plot ini membantu memeriksa asumsi linearitas dan homogenitas varians residual. Jika ada pola tertentu (misalnya bentuk kipas atau kurva), asumsi mungkin dilanggar."),
                  h4("Normal Q-Q Plot Residuals:"),
                  plotOutput("residual_qq_plot"),
                  h4("Interpretasi Normal Q-Q Plot Residuals:"),
                  p("Plot ini digunakan untuk memeriksa asumsi normalitas residual. Jika titik-titik mengikuti garis diagonal, residual dianggap berdistribusi normal."),
                  h4("Scale-Location Plot Residuals:"),
                  plotOutput("residual_scale_location_plot"),
                  h4("Interpretasi Scale-Location Plot Residuals:"),
                  p("Plot ini juga memeriksa asumsi homogenitas varians (homoskedastisitas). Jika garis merah horizontal dan titik-titik tersebar secara acak di sekitarnya, asumsi terpenuhi."),
                  h4("Residuals vs Leverage Plot:"),
                  plotOutput("residual_leverage_plot"),
                  h4("Interpretasi Residuals vs Leverage Plot:"),
                  p("Plot ini membantu mengidentifikasi observasi yang berpengaruh (leverage) dan outlier. Titik-titik di luar batas Cook's distance menunjukkan observasi yang sangat berpengaruh."),
                  downloadButton("download_reg_plots_pdf", "Unduh Laporan Plot Asumsi Regresi (PDF)")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data for categorization
  categorized_data_r <- reactiveVal(NULL)
  
  observeEvent(input$categorize_btn, {
    req(input$var_cat, input$breaks)
    data <- sovi_data
    # Ensure the column exists and is numeric before cutting
    if (input$var_cat %in% names(data) && is.numeric(data[[input$var_cat]])) {
      data[[paste0(input$var_cat, "_cat")]] <- cut(data[[input$var_cat]],
                                                   breaks = input$breaks,
                                                   include.lowest = TRUE,
                                                   labels = paste0("Kat_", 1:input$breaks))
      categorized_data_r(data)
    } else {
      showNotification("Variabel yang dipilih bukan numerik atau tidak ditemukan.", type = "error")
    }
  })
  
  output$tabel_kat <- renderDT({
    if (!is.null(categorized_data_r())) {
      datatable(categorized_data_r(), options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(sovi_data, options = list(pageLength = 10, scrollX = TRUE)) # Show original data initially
    }
  })
  
  # Download handlers for Manajemen Data
  output$download_manajemen_csv <- downloadHandler(
    filename = function() { "data_kategorik.csv" },
    content = function(file) {
      if (!is.null(categorized_data_r())) {
        write.csv(categorized_data_r(), file, row.names = FALSE)
      } else {
        showNotification("Belum ada data kategorik untuk diunduh.", type = "warning")
      }
    }
  )
  
  output$download_manajemen_pdf <- downloadHandler(
    filename = function() { "laporan_manajemen_data.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "manajemen_data_report.Rmd")
      file.copy("reports/manajemen_data_report.Rmd", tempReport, overwrite = TRUE)
      
      # Pass parameters to the Rmd document
      params <- list(data = if(!is.null(categorized_data_r())) categorized_data_r() else sovi_data,
                     var_cat = input$var_cat,
                     breaks = input$breaks)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # Eksplorasi Data
  output$summary_var <- renderPrint({
    req(input$var_explorasi_desc)
    summary_output <- capture.output(summary(sovi_data[[input$var_explorasi_desc]]))
    cat(summary_output, sep = "\n")
  })
  
  output$histPlot <- renderPlot({
    req(input$var_plot_hist)
    ggplot(sovi_data, aes(x = .data[[input$var_plot_hist]])) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      ggtitle(paste("Histogram Variabel:", input$var_plot_hist)) +
      theme_minimal()
  })
  
  output$boxPlot <- renderPlot({
    req(input$var_plot_box)
    ggplot(sovi_data, aes(y = .data[[input$var_plot_box]])) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      ggtitle(paste("Boxplot Variabel:", input$var_plot_box)) +
      theme_minimal()
  })
  
  output$map <- renderLeaflet({
    # Check if latitude and longitude columns exist
    if ("latitude" %in% names(sovi_data) && "longitude" %in% names(sovi_data)) {
      leaflet(sovi_data) %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude,
                   popup = ~as.character(paste("Row:", row.names(sovi_data)))) %>%
        setView(lng = mean(sovi_data$longitude), lat = mean(sovi_data$latitude), zoom = 5)
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 106.8, lat = -6.2, zoom = 5) %>%
        addPopups(106.8, -6.2, "Koordinat geografis tidak ditemukan di data asli.")
    }
  })
  
  output$full_data_table <- renderDT({
    datatable(sovi_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Download handlers for Eksplorasi Data
  output$download_hist_png <- downloadHandler(
    filename = function() { paste0("histogram_", input$var_plot_hist, ".png") },
    content = function(file) {
      req(input$var_plot_hist)
      ggsave(file, plot = ggplot(sovi_data, aes(x = .data[[input$var_plot_hist]])) +
               geom_histogram(bins = 30, fill = "skyblue", color = "black") +
               ggtitle(paste("Histogram Variabel:", input$var_plot_hist)) +
               theme_minimal(),
             width = 8, height = 6, units = "in")
    }
  )
  
  output$download_hist_pdf <- downloadHandler(
    filename = function() { "laporan_histogram.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "hist_report.Rmd")
      file.copy("reports/hist_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, var = input$var_plot_hist)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_box_png <- downloadHandler(
    filename = function() { paste0("boxplot_", input$var_plot_box, ".png") },
    content = function(file) {
      req(input$var_plot_box)
      ggsave(file, plot = ggplot(sovi_data, aes(y = .data[[input$var_plot_box]])) +
               geom_boxplot(fill = "lightgreen", color = "darkgreen") +
               ggtitle(paste("Boxplot Variabel:", input$var_plot_box)) +
               theme_minimal(),
             width = 8, height = 6, units = "in")
    }
  )
  
  output$download_box_pdf <- downloadHandler(
    filename = function() { "laporan_boxplot.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "boxplot_report.Rmd")
      file.copy("reports/boxplot_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, var = input$var_plot_box)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_map_png <- downloadHandler(
    filename = function() { "map_data.png" },
    content = function(file) {
      # This is tricky as leaflet output is JS-based. You'd typically use webshot2 or a similar package
      # which requires external dependencies. For a simple download, this might not directly work.
      # A screenshot of the map might be manually taken by the user or a more complex solution is needed.
      # For now, let's just create a placeholder file.
      file.create(file)
      showNotification("Fungsi unduh peta ke PNG memerlukan setup tambahan (misalnya `webshot2`). Silakan lakukan tangkapan layar manual.", type = "warning")
    }
  )
  
  output$download_map_pdf <- downloadHandler(
    filename = function() { "laporan_peta_data.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "map_report.Rmd")
      file.copy("reports/map_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data) # You might need to pass coordinates here if the Rmd generates map
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_full_data_csv <- downloadHandler(
    filename = function() { "full_sovi_data.csv" },
    content = function(file) {
      write.csv(sovi_data, file, row.names = FALSE)
    }
  )
  
  output$download_full_data_pdf <- downloadHandler(
    filename = function() { "laporan_data_lengkap.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "full_data_report.Rmd")
      file.copy("reports/full_data_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  
  # Uji Asumsi
  output$qqplot <- renderPlot({
    req(input$var_norm)
    # Ensure data is numeric
    validate(
      need(is.numeric(sovi_data[[input$var_norm]]), "Variabel yang dipilih untuk uji normalitas harus numerik.")
    )
    qqnorm(sovi_data[[input$var_norm]], main = paste("Normal Q-Q Plot untuk", input$var_norm))
    qqline(sovi_data[[input$var_norm]], col = "red")
  })
  
  output$uji_norm <- renderPrint({
    req(input$var_norm)
    validate(
      need(is.numeric(sovi_data[[input$var_norm]]), "Variabel yang dipilih untuk uji normalitas harus numerik.")
    )
    shapiro_result <- shapiro.test(sovi_data[[input$var_norm]])
    cat("Hasil Uji Shapiro-Wilk:\n")
    print(shapiro_result)
    cat("\nInterpretasi: Jika p-value < 0.05, data kemungkinan tidak berdistribusi normal.\n")
  })
  
  output$uji_var <- renderPrint({
    req(input$var_homogen_val, input$var_homogen_group)
    validate(
      need(is.numeric(sovi_data[[input$var_homogen_val]]), "Variabel respon untuk uji homogenitas harus numerik."),
      need(!is.numeric(sovi_data[[input$var_homogen_group]]) && length(unique(na.omit(sovi_data[[input$var_homogen_group]]))) >= 2, "Variabel grup harus kategorik dengan setidaknya dua level.")
    )
    # Ensure the grouping variable is a factor
    data_for_levene <- sovi_data %>%
      filter(!is.na(.data[[input$var_homogen_val]]) & !is.na(.data[[input$var_homogen_group]]))
    data_for_levene[[input$var_homogen_group]] <- as.factor(data_for_levene[[input$var_homogen_group]])
    
    if (nlevels(data_for_levene[[input$var_homogen_group]]) < 2) {
      return("Variabel grup harus memiliki setidaknya dua level unik untuk uji homogenitas.")
    }
    
    levene_result <- leveneTest(as.formula(paste(input$var_homogen_val, "~", input$var_homogen_group)), data = data_for_levene)
    cat("Hasil Uji Levene (Homogenitas Varians):\n")
    print(levene_result)
    cat("\nInterpretasi: Jika p-value < 0.05, varians antar kelompok tidak homogen.\n")
  })
  
  # Download handlers for Uji Asumsi
  output$download_norm_png <- downloadHandler(
    filename = function() { paste0("qqplot_", input$var_norm, ".png") },
    content = function(file) {
      req(input$var_norm)
      png(file)
      qqnorm(sovi_data[[input$var_norm]], main = paste("Normal Q-Q Plot untuk", input$var_norm))
      qqline(sovi_data[[input$var_norm]], col = "red")
      dev.off()
    }
  )
  
  output$download_norm_pdf <- downloadHandler(
    filename = function() { "laporan_uji_normalitas.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "normalitas_report.Rmd")
      file.copy("reports/normalitas_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, var = input$var_norm)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_homo_pdf <- downloadHandler(
    filename = function() { "laporan_uji_homogenitas.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "homogenitas_report.Rmd")
      file.copy("reports/homogenitas_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, val_var = input$var_homogen_val, group_var = input$var_homogen_group)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  # Statistik Inferensia - Uji Beda Rata-rata
  observeEvent(input$run_ttest_one, {
    output$hasil_ttest_one <- renderPrint({
      req(input$ttest_var_one, input$mu)
      validate(
        need(is.numeric(sovi_data[[input$ttest_var_one]]), "Variabel untuk uji T satu sampel harus numerik.")
      )
      t_test_result <- t.test(sovi_data[[input$ttest_var_one]], mu = input$mu)
      cat("Hasil Uji T-Test Satu Sampel:\n")
      print(t_test_result)
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    })
  })
  
  output$download_ttest_one_pdf <- downloadHandler(
    filename = function() { "laporan_uji_t_satu_sampel.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "ttest_one_report.Rmd")
      file.copy("reports/ttest_one_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, var = input$ttest_var_one, mu = input$mu)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  observeEvent(input$run_ttest_two, {
    output$hasil_ttest_two <- renderPrint({
      req(input$group_two_samp, input$nilai_two_samp)
      validate(
        need(is.numeric(sovi_data[[input$nilai_two_samp]]), "Variabel nilai untuk uji T dua sampel harus numerik."),
        need(!is.numeric(sovi_data[[input$group_two_samp]]) && length(unique(na.omit(sovi_data[[input$group_two_samp]]))) == 2, "Variabel grup harus kategorik dengan tepat dua level.")
      )
      
      data_filtered <- sovi_data %>%
        filter(!is.na(.data[[input$nilai_two_samp]]) & !is.na(.data[[input$group_two_samp]]))
      
      if (length(unique(data_filtered[[input$group_two_samp]])) != 2) {
        stop("Variabel grup harus memiliki tepat dua level unik setelah menghilangkan NA.")
      }
      data_filtered[[input$group_two_samp]] <- as.factor(data_filtered[[input$group_two_samp]])
      
      t_test_result <- t.test(as.formula(paste(input$nilai_two_samp, "~", input$group_two_samp)),
                              data = data_filtered,
                              var.equal = input$var_equal)
      cat("Hasil Uji T-Test Dua Sampel Independen:\n")
      print(t_test_result)
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    })
  })
  
  output$download_ttest_two_pdf <- downloadHandler(
    filename = function() { "laporan_uji_t_dua_sampel.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "ttest_two_report.Rmd")
      file.copy("reports/ttest_two_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, value_var = input$nilai_two_samp, group_var = input$group_two_samp, var_equal = input$var_equal)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  # Statistik Inferensia - Uji Proporsi & Varians
  observeEvent(input$run_prop_one, {
    output$hasil_prop_one <- renderPrint({
      req(input$success_level, input$total_trials, input$p_hyp_one)
      validate(
        need(input$total_trials > 0, "Jumlah Total Percobaan harus lebih dari 0."),
        need(input$success_level >= 0 && input$success_level <= input$total_trials, "Jumlah Sukses harus antara 0 dan Total Percobaan.")
      )
      prop_test_result <- prop.test(x = input$success_level, n = input$total_trials, p = input$p_hyp_one)
      cat("Hasil Uji Proporsi Satu Sampel:\n")
      print(prop_test_result)
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    })
  })
  
  output$download_prop_one_pdf <- downloadHandler(
    filename = function() { "laporan_uji_proporsi_satu_sampel.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "prop_one_report.Rmd")
      file.copy("reports/prop_one_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(success = input$success_level, total = input$total_trials, p_hyp = input$p_hyp_one)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  observeEvent(input$run_prop_two, {
    output$hasil_prop_two <- renderPrint({
      req(input$success1, input$total1, input$success2, input$total2)
      validate(
        need(input$total1 > 0 && input$total2 > 0, "Jumlah Total Percobaan untuk kedua grup harus lebih dari 0."),
        need(input$success1 >= 0 && input$success1 <= input$total1, "Jumlah Sukses Grup 1 tidak valid."),
        need(input$success2 >= 0 && input$success2 <= input$total2, "Jumlah Sukses Grup 2 tidak valid.")
      )
      prop_test_result <- prop.test(x = c(input$success1, input$success2), n = c(input$total1, input$total2))
      cat("Hasil Uji Proporsi Dua Sampel:\n")
      print(prop_test_result)
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    })
  })
  
  observeEvent(input$run_var_one, {
    output$hasil_var_one <- renderPrint({
      req(input$var_var_one, input$var_hyp_one)
      validate(
        need(is.numeric(sovi_data[[input$var_var_one]]), "Variabel untuk uji varians harus numerik."),
        need(input$var_hyp_one > 0, "Varians hipotesis harus positif.")
      )
      # Chi-squared test for variance requires a specific implementation or approximation
      # R's var.test is for two samples. For one sample variance test, we can use chisq.test
      # if we calculate the chi-squared statistic manually.
      # Or, more robustly, use a package or manually calculate based on normal assumption.
      
      # Manual calculation for chi-squared test for variance
      sample_var <- var(sovi_data[[input$var_var_one]], na.rm = TRUE)
      n <- sum(!is.na(sovi_data[[input$var_var_one]]))
      chi_sq_stat <- (n - 1) * sample_var / input$var_hyp_one
      
      p_value <- 2 * min(pchisq(chi_sq_stat, df = n - 1), 1 - pchisq(chi_sq_stat, df = n - 1))
      
      cat("Hasil Uji Varians Satu Sampel (menggunakan pendekatan Chi-Square):\n")
      cat(paste0("Statistik Chi-squared = ", round(chi_sq_stat, 4), "\n"))
      cat(paste0("Derajat Kebebasan = ", n - 1, "\n"))
      cat(paste0("Nilai p = ", format.p(p_value), "\n"))
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
      if (p_value < 0.05) {
        cat("Kesimpulan: Terdapat bukti signifikan untuk menolak hipotesis bahwa varians populasi adalah ", input$var_hyp_one, ".\n")
      } else {
        cat("Kesimpulan: Tidak ada cukup bukti untuk menolak hipotesis bahwa varians populasi adalah ", input$var_hyp_one, ".\n")
      }
    })
  })
  
  output$download_var_one_pdf <- downloadHandler(
    filename = function() { "laporan_uji_varians_satu_sampel.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "var_one_report.Rmd")
      file.copy("reports/var_one_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, var = input$var_var_one, var_hyp = input$var_hyp_one)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  observeEvent(input$run_var_two, {
    output$hasil_var_two <- renderPrint({
      req(input$var_var_two_val, input$var_var_two_group)
      validate(
        need(is.numeric(sovi_data[[input$var_var_two_val]]), "Variabel nilai untuk uji varians dua sampel harus numerik."),
        need(!is.numeric(sovi_data[[input$var_var_two_group]]) && length(unique(na.omit(sovi_data[[input$var_var_two_group]]))) == 2, "Variabel grup harus kategorik dengan tepat dua level.")
      )
      
      data_filtered <- sovi_data %>%
        filter(!is.na(.data[[input$var_var_two_val]]) & !is.na(.data[[input$var_var_two_group]]))
      
      if (length(unique(data_filtered[[input$var_var_two_group]])) != 2) {
        stop("Variabel grup harus memiliki tepat dua level unik setelah menghilangkan NA.")
      }
      data_filtered[[input$var_var_two_group]] <- as.factor(data_filtered[[input$var_var_two_group]])
      
      var_test_result <- var.test(as.formula(paste(input$var_var_two_val, "~", input$var_var_two_group)), data = data_filtered)
      cat("Hasil Uji Varians Dua Sampel (F-Test):\n")
      print(var_test_result)
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    })
  })
  
  # Statistik Inferensia - ANOVA
  observeEvent(input$run_anova_one, {
    output$hasil_anova_one <- renderPrint({
      req(input$respon_anova_one, input$faktor_anova_one)
      validate(
        need(is.numeric(sovi_data[[input$respon_anova_one]]), "Variabel respon untuk ANOVA harus numerik."),
        need(!is.numeric(sovi_data[[input$faktor_anova_one]]), "Variabel faktor untuk ANOVA harus kategorik.")
      )
      data_filtered <- sovi_data %>%
        filter(!is.na(.data[[input$respon_anova_one]]) & !is.na(.data[[input$faktor_anova_one]]))
      data_filtered[[input$faktor_anova_one]] <- as.factor(data_filtered[[input$faktor_anova_one]])
      
      if (nlevels(data_filtered[[input$faktor_anova_one]]) < 2) {
        return("Variabel faktor harus memiliki setidaknya dua level unik untuk ANOVA.")
      }
      
      fit <- aov(as.formula(paste(input$respon_anova_one, "~", input$faktor_anova_one)), data = data_filtered)
      cat("Hasil ANOVA Satu Arah:\n")
      print(summary(fit))
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    })
  })
  
  output$download_anova_one_pdf <- downloadHandler(
    filename = function() { "laporan_anova_satu_arah.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "anova_one_report.Rmd")
      file.copy("reports/anova_one_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, respon = input$respon_anova_one, faktor = input$faktor_anova_one)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  
  observeEvent(input$run_anova_two, {
    output$hasil_anova_two <- renderPrint({
      req(input$respon_anova_two, input$faktor1_anova_two, input$faktor2_anova_two)
      validate(
        need(is.numeric(sovi_data[[input$respon_anova_two]]), "Variabel respon untuk ANOVA harus numerik."),
        need(!is.numeric(sovi_data[[input$faktor1_anova_two]]), "Faktor 1 harus kategorik."),
        need(!is.numeric(sovi_data[[input$faktor2_anova_two]]), "Faktor 2 harus kategorik."),
        need(input$faktor1_anova_two != input$faktor2_anova_two, "Faktor 1 dan Faktor 2 tidak boleh sama.")
      )
      
      data_filtered <- sovi_data %>%
        filter(!is.na(.data[[input$respon_anova_two]]) & !is.na(.data[[input$faktor1_anova_two]]) & !is.na(.data[[input$faktor2_anova_two]]))
      
      data_filtered[[input$faktor1_anova_two]] <- as.factor(data_filtered[[input$faktor1_anova_two]])
      data_filtered[[input$faktor2_anova_two]] <- as.factor(data_filtered[[input$faktor2_anova_two]])
      
      if (nlevels(data_filtered[[input$faktor1_anova_two]]) < 2 || nlevels(data_filtered[[input$faktor2_anova_two]]) < 2) {
        return("Kedua faktor harus memiliki setidaknya dua level unik untuk ANOVA dua arah.")
      }
      
      formula_anova_two <- as.formula(paste(input$respon_anova_two, "~", input$faktor1_anova_two, "*", input$faktor2_anova_two))
      fit <- aov(formula_anova_two, data = data_filtered)
      cat("Hasil ANOVA Dua Arah:\n")
      print(summary(fit))
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    })
  })
  
  output$download_anova_two_pdf <- downloadHandler(
    filename = function() { "laporan_anova_dua_arah.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "anova_two_report.Rmd")
      file.copy("reports/anova_two_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(data = sovi_data, respon = input$respon_anova_two, faktor1 = input$faktor1_anova_two, faktor2 = input$faktor2_anova_two)
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  
  # Regresi Linear Berganda
  reg_model <- eventReactive(input$run_reg, {
    req(input$y_var, input$x_var)
    validate(
      need(length(input$x_var) > 0, "Pilih setidaknya satu variabel prediktor."),
      need(input$y_var != input$x_var, "Variabel respon tidak boleh menjadi variabel prediktor.")
    )
    # Ensure all selected variables are numeric
    numeric_vars <- c(input$y_var, input$x_var)
    if (!all(sapply(sovi_data[numeric_vars], is.numeric))) {
      stop("Semua variabel yang dipilih (respon dan prediktor) harus numerik untuk regresi.")
    }
    
    formula_reg <- as.formula(paste(input$y_var, "~", paste(input$x_var, collapse = "+")))
    lm(formula_reg, data = sovi_data)
  })
  
  output$reg_output <- renderPrint({
    model <- reg_model()
    if (!is.null(model)) {
      cat("Hasil Regresi Linear Berganda:\n")
      print(summary(model))
      cat("\nInterpretasi: (Lihat penjelasan di atas)")
    }
  })
  
  output$residual_fitted_plot <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      plot(model, which = 1) # Residuals vs Fitted
    }
  })
  
  output$residual_qq_plot <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      plot(model, which = 2) # Normal Q-Q
    }
  })
  
  output$residual_scale_location_plot <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      plot(model, which = 3) # Scale-Location
    }
  })
  
  output$residual_leverage_plot <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      plot(model, which = 5) # Residuals vs Leverage
    }
  })
  
  # Download handlers for Regresi
  output$download_reg_summary_pdf <- downloadHandler(
    filename = function() { "laporan_regresi_summary.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "reg_summary_report.Rmd")
      file.copy("reports/reg_summary_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(model_summary = summary(reg_model()))
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_reg_plots_pdf <- downloadHandler(
    filename = function() { "laporan_regresi_plots.pdf" },
    content = function(file) {
      tempReport <- file.path(tempdir(), "reg_plots_report.Rmd")
      file.copy("reports/reg_plots_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(model = reg_model())
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  # Function to create combined PDF report
  output$download_gabungan_pdf <- downloadHandler(
    filename = function() {
      paste0(
        "Kelas_NIM_Nama_UAS",
        ".pdf"
      )
    },
    content = function(file) {
      # This is a placeholder. A real implementation would involve combining multiple Rmd files or their outputs.
      # You would create a master Rmd that knits all the individual Rmd reports.
      # For demonstration, I'll just create a dummy file.
      file.create(file)
      showNotification("Fitur unduh laporan gabungan sedang dalam pengembangan. Silakan unduh laporan per bagian terlebih dahulu.", type = "warning")
    }
  )
}

shinyApp(ui, server)
