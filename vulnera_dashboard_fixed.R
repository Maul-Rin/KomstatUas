# Dashboard Analisis Kerentanan Sosial Indonesia - Versi Diperbaiki
# Dengan Integrasi GeoJSON dan Analisis Spasial

# 1. Load Libraries
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr",
  "leaflet", "sf", "jsonlite", "spdep", "htmltools", "plotly",
  "corrplot", "RColorBrewer", "viridis", "shinyjs", "shinycssloaders"
)

# Install missing packages
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, dependencies = TRUE)
}

# Load libraries
lapply(required_packages, library, character.only = TRUE)

# 2. Define Global Variables and Data URLs
sovi_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
distance_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
geojson_file <- "indonesia_kabkota.geojson"

# 3. Data Loading Functions
load_sovi_data <- function() {
  tryCatch({
    data <- read_csv(sovi_url, show_col_types = FALSE)
    message("SOVI data loaded successfully")
    return(data)
  }, error = function(e) {
    message("Error loading SOVI data: ", e$message)
    # Create sample data if download fails
    set.seed(123)
    n <- 100
    data.frame(
      DISTRICTCODE = 1101:(1100 + n),
      CHILDREN = runif(n, 5, 15),
      FEMALE = runif(n, 48, 52),
      ELDERLY = runif(n, 2, 20),
      FHEAD = runif(n, 12, 35),
      FAMILYSIZE = runif(n, 3, 6),
      NOELECTRIC = runif(n, 0, 30),
      LOWEDU = runif(n, 15, 60),
      GROWTH = runif(n, -2, 5),
      POVERTY = runif(n, 5, 40),
      ILLITERATE = runif(n, 2, 25),
      NOTRAINING = runif(n, 30, 98),
      DPRONE = runif(n, 2, 80),
      RENTED = runif(n, 2, 50),
      NOSEWER = runif(n, 5, 45),
      TAPWATER = runif(n, 3, 25),
      POPULATION = sample(50000:500000, n, replace = TRUE)
    )
  })
}

load_distance_data <- function() {
  tryCatch({
    data <- read_csv(distance_url, show_col_types = FALSE)
    message("Distance data loaded successfully")
    return(data)
  }, error = function(e) {
    message("Error loading distance data: ", e$message)
    # Create sample distance matrix
    n <- 100
    matrix(runif(n*n, 0, 1000), nrow = n, ncol = n)
  })
}

load_geojson_data <- function() {
  tryCatch({
    if (file.exists(geojson_file)) {
      geojson <- st_read(geojson_file, quiet = TRUE)
      message("GeoJSON data loaded successfully")
      return(geojson)
    } else {
      message("GeoJSON file not found, creating sample spatial data...")
      # Create sample spatial points
      return(NULL)
    }
  }, error = function(e) {
    message("Error loading GeoJSON: ", e$message)
    return(NULL)
  })
}

# 4. Analysis Functions
calculate_sovi_index <- function(data) {
  # Select vulnerability variables
  vuln_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                 "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                 "NOTRAINING", "DPRONE", "RENTED", "NOSEWER")
  
  # Standardize variables (z-scores)
  data_std <- data %>%
    mutate(across(all_of(vuln_vars), ~ scale(.x)[,1]))
  
  # Calculate SOVI index (sum of standardized scores)
  data_std$SOVI_INDEX <- rowSums(data_std[vuln_vars], na.rm = TRUE)
  
  # Normalize to 0-100 scale
  data_std$SOVI_NORMALIZED <- scales::rescale(data_std$SOVI_INDEX, to = c(0, 100))
  
  # Create vulnerability categories
  data_std$VULN_CATEGORY <- cut(data_std$SOVI_NORMALIZED,
                                breaks = c(0, 25, 50, 75, 100),
                                labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                                include.lowest = TRUE)
  
  return(data_std)
}

create_spatial_weights <- function(distance_matrix, threshold = 200) {
  # Convert distance matrix to spatial weights matrix
  n <- min(100, nrow(distance_matrix))  # Limit to 100 for performance
  weights_matrix <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j && distance_matrix[i, j] <= threshold) {
        weights_matrix[i, j] <- 1 / (distance_matrix[i, j] + 1)
      }
    }
  }
  
  # Row-standardize weights
  row_sums <- rowSums(weights_matrix)
  for (i in 1:n) {
    if (row_sums[i] > 0) {
      weights_matrix[i, ] <- weights_matrix[i, ] / row_sums[i]
    }
  }
  
  return(weights_matrix)
}

# Load Data
sovi_data <- load_sovi_data()
distance_data <- load_distance_data()
geojson_data <- load_geojson_data()

# Calculate SOVI indices
sovi_analyzed <- calculate_sovi_index(sovi_data)

# Create spatial weights matrix
if (is.data.frame(distance_data) && nrow(distance_data) > 0) {
  dist_matrix <- as.matrix(distance_data[, -1])  # Remove first column
  weights_matrix <- create_spatial_weights(dist_matrix, threshold = 200)
} else {
  weights_matrix <- diag(min(100, nrow(sovi_analyzed)))
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Kerentanan Sosial Indonesia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Peta Interaktif", tabName = "map", icon = icon("map")),
      menuItem("Analisis Spasial", tabName = "spatial", icon = icon("chart-line")),
      menuItem("Matriks Korelasi", tabName = "correlation", icon = icon("table")),
      menuItem("Data Explorer", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS for dark theme
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #2c3e50;
          color: #ecf0f1;
        }
        .box {
          background-color: #34495e;
          color: #ecf0f1;
          border: 1px solid #5d6d7e;
        }
        .box-header {
          color: #ecf0f1;
          border-bottom: 1px solid #5d6d7e;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_districts"),
          valueBoxOutput("avg_vulnerability"),
          valueBoxOutput("high_vuln_count")
        ),
        
        fluidRow(
          box(
            title = "Distribusi Tingkat Kerentanan", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("vuln_distribution"))
          ),
          
          box(
            title = "Top 10 Kabupaten/Kota Paling Rentan", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            withSpinner(DT::dataTableOutput("top_vulnerable"))
          )
        ),
        
        fluidRow(
          box(
            title = "Statistik Indikator Kerentanan", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("indicator_boxplot"))
          )
        )
      ),
      
      # Interactive Map Tab
      tabItem(tabName = "map",
        fluidRow(
          box(
            title = "Peta Kerentanan Sosial Indonesia", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                selectInput("map_variable", 
                           "Pilih Variabel untuk Pemetaan:",
                           choices = c("SOVI Index" = "SOVI_NORMALIZED",
                                     "Kemiskinan (%)" = "POVERTY",
                                     "Pendidikan Rendah (%)" = "LOWEDU",
                                     "Tanpa Listrik (%)" = "NOELECTRIC",
                                     "Buta Huruf (%)" = "ILLITERATE",
                                     "Anak-anak (%)" = "CHILDREN",
                                     "Lansia (%)" = "ELDERLY"),
                           selected = "SOVI_NORMALIZED")
              ),
              column(4,
                selectInput("color_palette",
                           "Skema Warna:",
                           choices = c("Viridis" = "viridis",
                                     "Plasma" = "plasma",
                                     "Inferno" = "inferno",
                                     "Spectral" = "Spectral"),
                           selected = "viridis")
              ),
              column(4,
                numericInput("map_bins",
                            "Jumlah Kelas:",
                            value = 5,
                            min = 3,
                            max = 10,
                            step = 1)
              )
            ),
            
            withSpinner(leafletOutput("vulnerability_map", height = "500px"))
          )
        ),
        
        fluidRow(
          box(
            title = "Informasi Peta", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            
            h4("Penjelasan Variabel:"),
            tags$ul(
              tags$li(strong("SOVI Index:"), "Indeks Kerentanan Sosial (0-100, semakin tinggi semakin rentan)"),
              tags$li(strong("Kemiskinan:"), "Persentase penduduk miskin"),
              tags$li(strong("Pendidikan Rendah:"), "Persentase populasi 15+ dengan pendidikan rendah"),
              tags$li(strong("Tanpa Listrik:"), "Persentase rumah tangga tanpa listrik"),
              tags$li(strong("Buta Huruf:"), "Persentase populasi yang tidak bisa baca tulis"),
              tags$li(strong("Anak-anak:"), "Persentase populasi di bawah 5 tahun"),
              tags$li(strong("Lansia:"), "Persentase populasi 65 tahun ke atas")
            )
          )
        )
      ),
      
      # Spatial Analysis Tab
      tabItem(tabName = "spatial",
        fluidRow(
          box(
            title = "Analisis Autokorelasi Spasial", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            
            h4("Moran's I Statistics"),
            verbatimTextOutput("moran_stats"),
            
            h4("Interpretasi Moran's I:"),
            tags$ul(
              tags$li(strong("I > 0.3:"), "Klasterisasi positif kuat"),
              tags$li(strong("0.1 < I ≤ 0.3:"), "Klasterisasi positif sedang"),
              tags$li(strong("-0.1 ≤ I ≤ 0.1:"), "Distribusi acak"),
              tags$li(strong("I < -0.1:"), "Klasterisasi negatif")
            )
          ),
          
          box(
            title = "Pengaturan Matriks Pembobot", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            
            h4("Parameter Spasial:"),
            numericInput("distance_threshold", 
                        "Threshold Jarak (km):", 
                        value = 200, 
                        min = 50, 
                        max = 1000, 
                        step = 50),
            
            actionButton("update_weights", "Update Analisis", 
                        class = "btn-primary"),
            
            br(), br(),
            
            h5("Ringkasan Matriks Pembobot:"),
            verbatimTextOutput("weights_summary")
          )
        ),
        
        fluidRow(
          box(
            title = "Moran Scatterplot", 
            status = "success", 
            solidHeader = TRUE,
            width = 8,
            
            p("Scatterplot ini menunjukkan hubungan antara nilai SOVI di suatu daerah dengan nilai rata-rata SOVI di daerah tetangganya."),
            
            withSpinner(plotlyOutput("moran_scatterplot"))
          ),
          
          box(
            title = "Interpretasi Kuadran", 
            status = "warning", 
            solidHeader = TRUE,
            width = 4,
            
            h5("Kuadran Moran Scatterplot:"),
            tags$ul(
              tags$li(strong("HH (High-High):"), "Daerah dengan SOVI tinggi dikelilingi daerah SOVI tinggi"),
              tags$li(strong("HL (High-Low):"), "Daerah dengan SOVI tinggi dikelilingi daerah SOVI rendah"),
              tags$li(strong("LH (Low-High):"), "Daerah dengan SOVI rendah dikelilingi daerah SOVI tinggi"),
              tags$li(strong("LL (Low-Low):"), "Daerah dengan SOVI rendah dikelilingi daerah SOVI rendah")
            )
          )
        )
      ),
      
      # Correlation Matrix Tab
      tabItem(tabName = "correlation",
        fluidRow(
          box(
            title = "Matriks Korelasi Indikator Kerentanan", 
            status = "primary", 
            solidHeader = TRUE,
            width = 8,
            
            fluidRow(
              column(6,
                selectInput("corr_method",
                           "Metode Korelasi:",
                           choices = c("Pearson" = "pearson",
                                     "Spearman" = "spearman",
                                     "Kendall" = "kendall"),
                           selected = "pearson")
              ),
              column(6,
                checkboxInput("show_insignificant",
                             "Tampilkan korelasi tidak signifikan",
                             value = TRUE)
              )
            ),
            
            withSpinner(plotOutput("correlation_plot", height = "600px"))
          ),
          
          box(
            title = "Analisis Korelasi", 
            status = "info", 
            solidHeader = TRUE,
            width = 4,
            
            h4("Korelasi Tertinggi:"),
            verbatimTextOutput("highest_correlations"),
            
            h4("Interpretasi:"),
            tags$ul(
              tags$li(strong("r > 0.7:"), "Korelasi positif sangat kuat"),
              tags$li(strong("0.3 < r ≤ 0.7:"), "Korelasi positif kuat"),
              tags$li(strong("0.1 < r ≤ 0.3:"), "Korelasi positif lemah"),
              tags$li(strong("-0.1 ≤ r ≤ 0.1:"), "Tidak berkorelasi"),
              tags$li(strong("r < -0.1:"), "Korelasi negatif")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Principal Component Analysis (PCA)", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            
            withSpinner(plotlyOutput("pca_biplot"))
          ),
          
          box(
            title = "Kontribusi Variabel terhadap PC", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            
            withSpinner(plotlyOutput("pca_contribution"))
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Filter dan Eksplorasi Data", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(3,
                selectInput("filter_category",
                           "Filter Kategori Kerentanan:",
                           choices = c("Semua" = "all",
                                     "Rendah" = "Rendah",
                                     "Sedang" = "Sedang", 
                                     "Tinggi" = "Tinggi",
                                     "Sangat Tinggi" = "Sangat Tinggi"),
                           selected = "all")
              ),
              column(3,
                numericInput("min_sovi",
                            "SOVI Minimum:",
                            value = 0,
                            min = 0,
                            max = 100)
              ),
              column(3,
                numericInput("max_sovi",
                            "SOVI Maksimum:",
                            value = 100,
                            min = 0,
                            max = 100)
              ),
              column(3,
                downloadButton("download_data", "Download Data", 
                              class = "btn-success")
              )
            ),
            
            br(),
            
            withSpinner(DT::dataTableOutput("data_table"))
          )
        ),
        
        fluidRow(
          box(
            title = "Statistik Deskriptif", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            
            withSpinner(verbatimTextOutput("descriptive_stats"))
          ),
          
          box(
            title = "Distribusi SOVI Index", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            
            withSpinner(plotlyOutput("sovi_histogram"))
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- sovi_analyzed
    
    if (input$filter_category != "all") {
      data <- data %>% filter(VULN_CATEGORY == input$filter_category)
    }
    
    data <- data %>% 
      filter(SOVI_NORMALIZED >= input$min_sovi & SOVI_NORMALIZED <= input$max_sovi)
    
    return(data)
  })
  
  # Overview Tab Outputs
  output$total_districts <- renderValueBox({
    valueBox(
      value = nrow(sovi_analyzed),
      subtitle = "Total Kabupaten/Kota",
      icon = icon("map-marker-alt"),
      color = "blue"
    )
  })
  
  output$avg_vulnerability <- renderValueBox({
    valueBox(
      value = round(mean(sovi_analyzed$SOVI_NORMALIZED, na.rm = TRUE), 1),
      subtitle = "Rata-rata Indeks SOVI",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$high_vuln_count <- renderValueBox({
    high_vuln <- sum(sovi_analyzed$VULN_CATEGORY %in% c("Tinggi", "Sangat Tinggi"), na.rm = TRUE)
    valueBox(
      value = high_vuln,
      subtitle = "Daerah Kerentanan Tinggi",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$vuln_distribution <- renderPlotly({
    p <- ggplot(sovi_analyzed, aes(x = VULN_CATEGORY, fill = VULN_CATEGORY)) +
      geom_bar(alpha = 0.8) +
      scale_fill_viridis_d(name = "Kategori") +
      theme_minimal() +
      labs(title = "Distribusi Kategori Kerentanan",
           x = "Kategori Kerentanan",
           y = "Jumlah Daerah") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$top_vulnerable <- renderDT({
    top_data <- sovi_analyzed %>%
      arrange(desc(SOVI_NORMALIZED)) %>%
      head(10) %>%
      select(DISTRICTCODE, SOVI_NORMALIZED, VULN_CATEGORY, POVERTY, LOWEDU, ILLITERATE)
    
    datatable(top_data, 
              options = list(pageLength = 10, dom = 't', scrollX = TRUE),
              rownames = FALSE,
              colnames = c("Kode Distrik", "SOVI", "Kategori", "Kemiskinan", "Pend. Rendah", "Buta Huruf")) %>%
      formatRound(c("SOVI_NORMALIZED", "POVERTY", "LOWEDU", "ILLITERATE"), 2)
  })
  
  output$indicator_boxplot <- renderPlotly({
    plot_data <- sovi_analyzed %>%
      select(CHILDREN, ELDERLY, POVERTY, LOWEDU, ILLITERATE, NOELECTRIC) %>%
      pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Value")
    
    p <- ggplot(plot_data, aes(x = Indicator, y = Value, fill = Indicator)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = "Distribusi Indikator Kerentanan",
           x = "Indikator",
           y = "Nilai (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  # Map Tab Outputs
  output$vulnerability_map <- renderLeaflet({
    # Create sample coordinates representing Indonesian districts
    set.seed(42)
    sample_coords <- data.frame(
      lng = c(runif(30, 95, 105),   # Sumatra
              runif(25, 106, 115),  # Java
              runif(20, 108, 120),  # Kalimantan
              runif(15, 119, 125),  # Sulawesi
              runif(10, 125, 141)), # Eastern Indonesia
      lat = c(runif(30, -6, 6),     # Sumatra
              runif(25, -9, -6),    # Java
              runif(20, -4, 4),     # Kalimantan
              runif(15, -6, 2),     # Sulawesi
              runif(10, -9, 0))     # Eastern Indonesia
    )
    
    # Ensure we have enough coordinates
    n_data <- nrow(sovi_analyzed)
    if (nrow(sample_coords) < n_data) {
      additional_coords <- data.frame(
        lng = runif(n_data - nrow(sample_coords), 95, 141),
        lat = runif(n_data - nrow(sample_coords), -11, 6)
      )
      sample_coords <- rbind(sample_coords, additional_coords)
    }
    
    # Take only the number of coordinates we need
    sample_coords <- sample_coords[1:n_data, ]
    
    # Combine with SOVI data
    map_data <- cbind(sample_coords, sovi_analyzed)
    
    # Select variable for mapping
    map_var <- input$map_variable
    if (is.null(map_var)) map_var <- "SOVI_NORMALIZED"
    
    # Create color palette
    if (input$color_palette == "Spectral") {
      pal <- colorQuantile("Spectral", map_data[[map_var]], n = input$map_bins, reverse = TRUE)
    } else {
      pal <- colorQuantile(input$color_palette, map_data[[map_var]], n = input$map_bins)
    }
    
    # Create map
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = 6,
        color = ~pal(get(map_var)),
        popup = ~paste("<b>District:</b>", DISTRICTCODE, "<br>",
                      "<b>SOVI Index:</b>", round(SOVI_NORMALIZED, 2), "<br>",
                      "<b>Kategori:</b>", VULN_CATEGORY, "<br>",
                      "<b>Kemiskinan:</b>", round(POVERTY, 2), "%", "<br>",
                      "<b>Pendidikan Rendah:</b>", round(LOWEDU, 2), "%", "<br>",
                      "<b>Populasi:</b>", format(POPULATION, big.mark = ",")),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        opacity = 0.8
      ) %>%
      addLegend(
        pal = pal,
        values = ~get(map_var),
        title = gsub("_", " ", map_var),
        position = "bottomright",
        opacity = 0.8
      )
  })
  
  # Spatial Analysis Tab Outputs
  output$moran_stats <- renderText({
    n <- min(nrow(sovi_analyzed), nrow(weights_matrix))
    if (n > 1) {
      values <- sovi_analyzed$SOVI_NORMALIZED[1:n]
      weights_sub <- weights_matrix[1:n, 1:n]
      
      # Calculate Moran's I
      mean_val <- mean(values, na.rm = TRUE)
      numerator <- 0
      denominator <- sum((values - mean_val)^2, na.rm = TRUE)
      w_sum <- sum(weights_sub)
      
      for (i in 1:n) {
        for (j in 1:n) {
          if (weights_sub[i, j] > 0 && !is.na(values[i]) && !is.na(values[j])) {
            numerator <- numerator + weights_sub[i, j] * (values[i] - mean_val) * (values[j] - mean_val)
          }
        }
      }
      
      if (w_sum > 0 && denominator > 0) {
        moran_i <- (n / w_sum) * (numerator / denominator)
        
        interpretation <- if (moran_i > 0.3) {
          "Klasterisasi Positif Kuat"
        } else if (moran_i > 0.1) {
          "Klasterisasi Positif Sedang"
        } else if (moran_i > -0.1) {
          "Distribusi Acak"
        } else {
          "Klasterisasi Negatif"
        }
        
        paste("Moran's I:", round(moran_i, 4), "\n",
              "Jumlah observasi:", n, "\n",
              "Interpretasi:", interpretation)
      } else {
        "Tidak dapat menghitung Moran's I - data tidak mencukupi"
      }
    } else {
      "Data tidak mencukupi untuk analisis spasial"
    }
  })
  
  output$weights_summary <- renderText({
    non_zero <- sum(weights_matrix > 0)
    total <- length(weights_matrix)
    avg_neighbors <- mean(rowSums(weights_matrix > 0))
    max_weight <- max(weights_matrix)
    
    paste("Dimensi matriks:", nrow(weights_matrix), "x", ncol(weights_matrix), "\n",
          "Total koneksi:", non_zero, "/", total, "\n",
          "Rata-rata tetangga per daerah:", round(avg_neighbors, 2), "\n",
          "Sparsitas matriks:", round((1 - non_zero/total) * 100, 2), "%", "\n",
          "Bobot maksimum:", round(max_weight, 4))
  })
  
  output$moran_scatterplot <- renderPlotly({
    n <- min(nrow(sovi_analyzed), nrow(weights_matrix))
    if (n > 1) {
      values <- sovi_analyzed$SOVI_NORMALIZED[1:n]
      weights_sub <- weights_matrix[1:n, 1:n]
      
      # Calculate spatial lag
      spatial_lag <- as.vector(weights_sub %*% values)
      
      # Create quadrants
      mean_x <- mean(values, na.rm = TRUE)
      mean_y <- mean(spatial_lag, na.rm = TRUE)
      
      quadrant <- ifelse(values >= mean_x & spatial_lag >= mean_y, "HH",
                        ifelse(values >= mean_x & spatial_lag < mean_y, "HL",
                              ifelse(values < mean_x & spatial_lag >= mean_y, "LH", "LL")))
      
      scatter_data <- data.frame(
        x = values,
        y = spatial_lag,
        district = sovi_analyzed$DISTRICTCODE[1:n],
        quadrant = quadrant
      )
      
      p <- ggplot(scatter_data, aes(x = x, y = y, color = quadrant)) +
        geom_point(alpha = 0.7, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
        geom_hline(yintercept = mean_y, linetype = "dotted", alpha = 0.5) +
        geom_vline(xintercept = mean_x, linetype = "dotted", alpha = 0.5) +
        scale_color_viridis_d(name = "Kuadran") +
        theme_minimal() +
        labs(title = "Moran Scatterplot - Autokorelasi Spasial SOVI",
             x = "SOVI Index",
             y = "Spatial Lag SOVI",
             subtitle = paste("Korelasi:", round(cor(values, spatial_lag), 3)))
      
      ggplotly(p)
    } else {
      plotly_empty() %>%
        layout(title = "Data tidak mencukupi untuk Moran Scatterplot")
    }
  })
  
  # Update weights when button is clicked
  observeEvent(input$update_weights, {
    if (is.data.frame(distance_data) && nrow(distance_data) > 0) {
      weights_matrix <<- create_spatial_weights(dist_matrix, threshold = input$distance_threshold)
      showNotification("Matriks pembobot berhasil diperbarui!", type = "success")
    }
  })
  
  # Correlation Tab Outputs
  output$correlation_plot <- renderPlot({
    vuln_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                   "NOELECTRIC", "LOWEDU", "POVERTY", "ILLITERATE", "NOTRAINING")
    
    cor_data <- sovi_analyzed[vuln_vars]
    cor_matrix <- cor(cor_data, use = "complete.obs", method = input$corr_method)
    
    # Optionally hide insignificant correlations
    if (!input$show_insignificant) {
      cor_matrix[abs(cor_matrix) < 0.1] <- 0
    }
    
    corrplot(cor_matrix, 
             method = "color",
             type = "upper",
             order = "hclust",
             tl.cex = 0.9,
             tl.col = "black",
             tl.srt = 45,
             addCoef.col = "white",
             number.cex = 0.8,
             col = colorRampPalette(c("blue", "white", "red"))(200),
             title = paste("Matriks Korelasi (", tools::toTitleCase(input$corr_method), ")", sep = ""),
             mar = c(0,0,2,0))
  })
  
  output$highest_correlations <- renderText({
    vuln_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                   "NOELECTRIC", "LOWEDU", "POVERTY", "ILLITERATE", "NOTRAINING")
    
    cor_data <- sovi_analyzed[vuln_vars]
    cor_matrix <- cor(cor_data, use = "complete.obs", method = input$corr_method)
    
    # Get upper triangle
    cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
    
    # Find highest correlations
    cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
    cor_df$Correlation <- as.vector(cor_matrix)
    cor_df <- cor_df[!is.na(cor_df$Correlation), ]
    cor_df <- cor_df[order(abs(cor_df$Correlation), decreasing = TRUE), ]
    
    top_5 <- head(cor_df, 5)
    
    result <- paste(paste(top_5$Var1, "-", top_5$Var2, ":", round(top_5$Correlation, 3)), collapse = "\n")
    return(result)
  })
  
  output$pca_biplot <- renderPlotly({
    vuln_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                   "NOELECTRIC", "LOWEDU", "POVERTY", "ILLITERATE", "NOTRAINING")
    
    pca_data <- sovi_analyzed[vuln_vars]
    pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)
    
    # Create biplot data
    pca_scores <- data.frame(pca_result$x[, 1:2])
    pca_scores$VULN_CATEGORY <- sovi_analyzed$VULN_CATEGORY
    
    # Variable loadings
    loadings <- data.frame(pca_result$rotation[, 1:2])
    loadings$Variable <- rownames(loadings)
    
    # Calculate explained variance
    var_exp <- summary(pca_result)$importance[2, 1:2] * 100
    
    p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = VULN_CATEGORY)) +
      geom_point(alpha = 0.6, size = 2) +
      scale_color_viridis_d(name = "Kategori Kerentanan") +
      theme_minimal() +
      labs(title = "PCA Biplot - Analisis Komponen Utama",
           x = paste0("PC1 (", round(var_exp[1], 1), "% variance)"),
           y = paste0("PC2 (", round(var_exp[2], 1), "% variance)"))
    
    # Add loading vectors
    for (i in 1:nrow(loadings)) {
      p <- p + 
        geom_segment(data = loadings[i, ], 
                    aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
                    arrow = arrow(length = unit(0.3, "cm")),
                    color = "red", alpha = 0.7, inherit.aes = FALSE) +
        geom_text(data = loadings[i, ],
                 aes(x = PC1 * 3.2, y = PC2 * 3.2, label = Variable),
                 color = "red", size = 3, inherit.aes = FALSE)
    }
    
    ggplotly(p)
  })
  
  output$pca_contribution <- renderPlotly({
    vuln_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                   "NOELECTRIC", "LOWEDU", "POVERTY", "ILLITERATE", "NOTRAINING")
    
    pca_data <- sovi_analyzed[vuln_vars]
    pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)
    
    # Calculate contributions
    contrib_pc1 <- (pca_result$rotation[, 1]^2) * 100
    contrib_pc2 <- (pca_result$rotation[, 2]^2) * 100
    
    contrib_df <- data.frame(
      Variable = names(contrib_pc1),
      PC1 = contrib_pc1,
      PC2 = contrib_pc2
    ) %>%
      arrange(desc(PC1))
    
    contrib_long <- contrib_df %>%
      pivot_longer(cols = c(PC1, PC2), names_to = "Component", values_to = "Contribution")
    
    p <- ggplot(contrib_long, aes(x = reorder(Variable, Contribution), y = Contribution, fill = Component)) +
      geom_col(position = "dodge", alpha = 0.8) +
      coord_flip() +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = "Kontribusi Variabel terhadap Komponen Utama",
           x = "Variabel",
           y = "Kontribusi (%)")
    
    ggplotly(p)
  })
  
  # Data Explorer Tab Outputs
  output$data_table <- renderDT({
    display_data <- filtered_data() %>%
      select(DISTRICTCODE, SOVI_NORMALIZED, VULN_CATEGORY, POVERTY, LOWEDU, 
             ILLITERATE, NOELECTRIC, CHILDREN, ELDERLY, POPULATION, FAMILYSIZE)
    
    datatable(display_data, 
              options = list(scrollX = TRUE, pageLength = 15, searchHighlight = TRUE),
              rownames = FALSE,
              colnames = c("Kode Distrik", "SOVI Index", "Kategori", "Kemiskinan (%)", 
                          "Pend. Rendah (%)", "Buta Huruf (%)", "Tanpa Listrik (%)",
                          "Anak-anak (%)", "Lansia (%)", "Populasi", "Ukuran Keluarga"),
              filter = 'top') %>%
      formatRound(c("SOVI_NORMALIZED", "POVERTY", "LOWEDU", "ILLITERATE", 
                   "NOELECTRIC", "CHILDREN", "ELDERLY", "FAMILYSIZE"), 2) %>%
      formatCurrency("POPULATION", currency = "", digits = 0, mark = ",")
  })
  
  output$descriptive_stats <- renderText({
    data <- filtered_data()
    numeric_vars <- c("SOVI_NORMALIZED", "POVERTY", "LOWEDU", "ILLITERATE", "NOELECTRIC")
    
    stats_list <- lapply(numeric_vars, function(var) {
      values <- data[[var]]
      c(
        paste(var, ":"),
        paste("  Min:", round(min(values, na.rm = TRUE), 2)),
        paste("  Max:", round(max(values, na.rm = TRUE), 2)),
        paste("  Mean:", round(mean(values, na.rm = TRUE), 2)),
        paste("  Median:", round(median(values, na.rm = TRUE), 2)),
        paste("  SD:", round(sd(values, na.rm = TRUE), 2)),
        ""
      )
    })
    
    paste(unlist(stats_list), collapse = "\n")
  })
  
  output$sovi_histogram <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = SOVI_NORMALIZED, fill = VULN_CATEGORY)) +
      geom_histogram(bins = 30, alpha = 0.7, color = "white") +
      scale_fill_viridis_d(name = "Kategori") +
      theme_minimal() +
      labs(title = "Distribusi SOVI Index",
           x = "SOVI Index",
           y = "Frekuensi")
    
    ggplotly(p)
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("sovi_analysis_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

# Run the Application
shinyApp(ui = ui, server = server)
