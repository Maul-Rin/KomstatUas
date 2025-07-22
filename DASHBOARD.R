# Nama File: vulnera_dashboard_spatial_complete.R
# Dashboard Analisis Kerentanan Sosial - Tema Gunmetal dengan Analisis Spasial Lengkap

# 1. Load Libraries dengan error handling
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readr", 
  "leaflet", "car", "tidyr", "shinyjs", "stats", "psych", 
  "sf", "jsonlite", "spdep", "htmltools", "RColorBrewer", "viridis"
)

# Install missing packages
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

# Load libraries
lapply(required_packages, library, character.only = TRUE)

# 2. Define Metadata and URLs
metadata_sovi <- data.frame(
  Variabel = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
               "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING"),
  Deskripsi = c(
    "Kode unik untuk setiap distrik/kabupaten.",
    "Persentase populasi di bawah lima tahun.",
    "Persentase populasi perempuan.",
    "Persentase populasi 65 tahun ke atas.",
    "Persentase rumah tangga dengan kepala rumah tangga perempuan.",
    "Rata-rata jumlah anggota rumah tangga di satu distrik.",
    "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan.",
    "Persentase populasi 15 tahun ke atas dengan pendidikan rendah.",
    "Persentase perubahan populasi (pertumbuhan populasi).",
    "Persentase penduduk miskin.",
    "Persentase populasi yang tidak bisa membaca dan menulis.",
    "Persentase rumah tangga yang tidak mendapatkan pelatihan bencana."
  ),
  Tipe = c("Kategorik/ID", rep("Numerik", 11)),
  stringsAsFactors = FALSE
)

# URL data
sovi_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
distance_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
metadata_article_url <- "https://www.sciencedirect.com/science/article/pii/S2352340921010180"

# 3. Create sample data if online data fails
create_sample_data <- function() {
  set.seed(123)
  n <- 100
  data.frame(
    DISTRICTCODE = paste0("D", sprintf("%03d", 1:n)),
    CHILDREN = runif(n, 5, 15),
    FEMALE = runif(n, 48, 52),
    ELDERLY = runif(n, 8, 20),
    FHEAD = runif(n, 15, 35),
    FAMILYSIZE = runif(n, 3, 6),
    NOELECTRIC = runif(n, 0, 30),
    LOWEDU = runif(n, 20, 60),
    GROWTH = runif(n, -2, 5),
    POVERTY = runif(n, 5, 40),
    ILLITERATE = runif(n, 2, 25),
    NOTRAINING = runif(n, 60, 95),
    stringsAsFactors = FALSE
  )
}

# 4. Create sample GeoJSON data if file not available
create_sample_geojson <- function(district_codes) {
  # Create simple polygon geometries for Indonesia-like coordinates
  set.seed(123)
  n <- length(district_codes)
  
  # Create simple square polygons around Indonesia coordinates
  polygons <- lapply(1:n, function(i) {
    center_lon <- runif(1, 106, 108)
    center_lat <- runif(1, -8, -6)
    size <- 0.1
    
    coords <- matrix(c(
      center_lon - size, center_lat - size,
      center_lon + size, center_lat - size,
      center_lon + size, center_lat + size,
      center_lon - size, center_lat + size,
      center_lon - size, center_lat - size
    ), ncol = 2, byrow = TRUE)
    
    st_polygon(list(coords))
  })
  
  # Create sf object
  sf_data <- st_sf(
    DISTRICTCODE = district_codes,
    NAME_2 = paste("Kabupaten", district_codes),
    geometry = st_sfc(polygons, crs = 4326)
  )
  
  return(sf_data)
}

# 5. Read Data with fallback
sovi_data_raw <- tryCatch({
  read_csv(sovi_url, show_col_types = FALSE)
}, error = function(e) {
  warning("Gagal memuat data online, menggunakan data sampel: ", e$message)
  create_sample_data()
})

# Main dataset without coordinates
sovi_data_global <- sovi_data_raw[, !names(sovi_data_raw) %in% c("latitude", "longitude")]

# If no DISTRICTCODE, add it
if (!"DISTRICTCODE" %in% names(sovi_data_global)) {
  sovi_data_global$DISTRICTCODE <- paste0("D", sprintf("%03d", 1:nrow(sovi_data_global)))
}

# 6. Load GeoJSON data with fallback to sample data
geojson_data <- tryCatch({
  # Try to read from multiple possible file locations
  possible_files <- c(
    "indonesia511.geojson",
    "data/indonesia511.geojson",
    "./indonesia511.geojson"
  )
  
  loaded_data <- NULL
  for (file_path in possible_files) {
    if (file.exists(file_path)) {
      loaded_data <- st_read(file_path, quiet = TRUE)
      message("GeoJSON loaded from: ", file_path)
      break
    }
  }
  
  if (is.null(loaded_data)) {
    stop("GeoJSON file not found in any expected location")
  }
  
  loaded_data
}, error = function(e) {
  warning("Gagal memuat GeoJSON: ", e$message, ". Menggunakan data geometri sampel.")
  create_sample_geojson(sovi_data_global$DISTRICTCODE)
})

# 7. Load distance matrix data
distance_matrix_data <- tryCatch({
  dist_data <- read_csv(distance_url, show_col_types = FALSE)
  message("Distance matrix loaded successfully")
  dist_data
}, error = function(e) {
  warning("Gagal memuat distance.csv: ", e$message, ". Membuat matriks jarak sampel.")
  
  # Create sample distance matrix
  districts <- sovi_data_global$DISTRICTCODE
  n <- length(districts)
  
  # Create symmetric distance matrix
  set.seed(123)
  dist_matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in i:n) {
      if (i != j) {
        dist_val <- runif(1, 10, 1000)  # Random distances between 10-1000 km
        dist_matrix[i, j] <- dist_val
        dist_matrix[j, i] <- dist_val  # Make symmetric
      }
    }
  }
  
  # Convert to data frame format
  dist_df <- data.frame(DISTRICTCODE = districts, dist_matrix)
  names(dist_df)[-1] <- districts
  dist_df
})

# 8. User Interface (UI)
ui <- dashboardPage(
  dashboardHeader(
    title = "Vulnera: Dashboard Analisis Kerentanan Sosial", 
    titleWidth = 350,
    tags$li(class = "dropdown",
            tags$button(
              id = "theme-toggle",
              class = "btn btn-default navbar-btn",
              style = "margin-right: 10px;",
              onclick = "toggleTheme()",
              tags$i(class = "fas fa-moon", id = "theme-icon"),
              " Dark Mode"
            )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("cogs")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar"),
               menuSubItem("Statistik Deskriptif", tabName = "statistik_deskriptif"),
               menuSubItem("Histogram", tabName = "histogram"),
               menuSubItem("Boxplot", tabName = "boxplot"),
               menuSubItem("Peta", tabName = "peta"),
               menuSubItem("Analisis Spasial", tabName = "analisis_spasial_ekspl")
      ),
      menuItem("Uji Asumsi Data", tabName = "asumsi", icon = icon("check-circle"),
               menuSubItem("Uji Normalitas", tabName = "uji_normalitas"),
               menuSubItem("Uji Homogenitas", tabName = "uji_homogenitas")
      ),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("flask"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_beda_rata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "uji_prop_var"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("chart-line")),
      menuItem("Analisis Spasial", tabName = "spasial", icon = icon("globe"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        /* CSS Variables for Theme */
        :root {
          --gunmetal-primary: #2a3439;
          --gunmetal-light: #3d4a52;
          --gunmetal-dark: #1e2529;
          --gunmetal-accent: #4a5c66;
          --text-primary: #ffffff;
          --text-secondary: #b8c5d1;
          --bg-primary: #f4f6f8;
          --bg-secondary: #ffffff;
          --border-color: #dee2e6;
          --accent-teal: #17a2b8;
          --accent-orange: #fd7e14;
          --accent-success: #28a745;
          --accent-warning: #ffc107;
          --accent-danger: #dc3545;
        }
        
        /* Light Theme (Default) */
        body {
          background-color: var(--bg-primary);
          color: #333;
          transition: all 0.3s ease;
        }
        
        /* Header Styling */
        .main-header .navbar {
          background-color: var(--gunmetal-primary) !important;
          border-bottom: 3px solid var(--gunmetal-accent);
        }
        
        .main-header .navbar-brand {
          color: var(--text-primary) !important;
          font-weight: 600;
        }
        
        /* Sidebar Styling */
        .main-sidebar {
          background-color: var(--gunmetal-light) !important;
        }
        
        .sidebar-menu > li > a {
          color: var(--text-secondary) !important;
          border-left: 3px solid transparent;
          transition: all 0.3s ease;
        }
        
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background-color: var(--gunmetal-accent) !important;
          color: var(--text-primary) !important;
          border-left: 3px solid var(--accent-teal);
        }
        
        .sidebar-menu .treeview-menu > li > a {
          color: var(--text-secondary) !important;
          padding-left: 35px;
        }
        
        .sidebar-menu .treeview-menu > li > a:hover,
        .sidebar-menu .treeview-menu > li.active > a {
          background-color: var(--gunmetal-dark) !important;
          color: var(--text-primary) !important;
        }
        
        /* Content Area */
        .content-wrapper {
          background-color: var(--bg-primary) !important;
          min-height: 100vh;
        }
        
        /* Box Styling */
        .box {
          background-color: var(--bg-secondary);
          border: 1px solid var(--border-color);
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(42, 52, 57, 0.1);
          margin-bottom: 20px;
          transition: all 0.3s ease;
        }
        
        .box:hover {
          box-shadow: 0 4px 20px rgba(42, 52, 57, 0.15);
          transform: translateY(-2px);
        }
        
        .box-header {
          background-color: var(--gunmetal-primary);
          color: var(--text-primary);
          border-radius: 8px 8px 0 0;
          padding: 15px;
          border-bottom: 2px solid var(--gunmetal-accent);
        }
        
        .box-title {
          font-weight: 600;
          font-size: 16px;
        }
        
        /* Info Boxes */
        .info-box {
          background: linear-gradient(135deg, var(--bg-secondary) 0%, #f8f9fa 100%);
          border: 1px solid var(--border-color);
          border-radius: 10px;
          box-shadow: 0 3px 15px rgba(42, 52, 57, 0.1);
          transition: all 0.3s ease;
        }
        
        .info-box:hover {
          transform: translateY(-3px);
          box-shadow: 0 6px 25px rgba(42, 52, 57, 0.15);
        }
        
        .info-box-icon {
          background-color: var(--gunmetal-primary) !important;
          color: var(--text-primary) !important;
        }
        
        /* Buttons */
        .btn-primary {
          background-color: var(--gunmetal-primary);
          border-color: var(--gunmetal-accent);
          color: var(--text-primary);
          transition: all 0.3s ease;
        }
        
        .btn-primary:hover {
          background-color: var(--gunmetal-accent);
          border-color: var(--gunmetal-light);
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(42, 52, 57, 0.3);
        }
        
        .btn-success {
          background-color: var(--accent-success);
          border-color: #1e7e34;
        }
        
        .btn-success:hover {
          background-color: #218838;
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(40, 167, 69, 0.3);
        }
        
        /* Status Messages */
        .status-success {
          background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%);
          border: 1px solid #c3e6cb;
          color: #155724;
          border-radius: 8px;
          padding: 12px;
        }
        
        .status-error {
          background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%);
          border: 1px solid #f5c6cb;
          color: #721c24;
          border-radius: 8px;
          padding: 12px;
        }
        
        .status-processing {
          background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);
          border: 1px solid #ffeaa7;
          color: #856404;
          border-radius: 8px;
          padding: 12px;
        }
        
        /* Theme Toggle Button */
        #theme-toggle {
          background-color: var(--gunmetal-accent) !important;
          border-color: var(--gunmetal-light) !important;
          color: var(--text-primary) !important;
          border-radius: 20px;
          transition: all 0.3s ease;
        }
        
        #theme-toggle:hover {
          background-color: var(--gunmetal-light) !important;
          transform: scale(1.05);
        }
        
        /* Form Controls */
        .form-control {
          border: 2px solid var(--border-color);
          border-radius: 6px;
          transition: all 0.3s ease;
        }
        
        .form-control:focus {
          border-color: var(--gunmetal-primary);
          box-shadow: 0 0 0 0.2rem rgba(42, 52, 57, 0.25);
        }
        
        /* DataTables */
        .dataTables_wrapper {
          background-color: var(--bg-secondary);
          border-radius: 8px;
          padding: 15px;
        }
        
        table.dataTable thead th {
          background-color: var(--gunmetal-primary);
          color: var(--text-primary);
          border-bottom: 2px solid var(--gunmetal-accent);
        }
        
        table.dataTable tbody tr:hover {
          background-color: rgba(42, 52, 57, 0.05);
        }
        
        /* DARK MODE STYLES */
        body.dark-mode {
          --bg-primary: #1a1a1a;
          --bg-secondary: #2d2d2d;
          --text-primary: #ffffff;
          --text-secondary: #b8c5d1;
          --border-color: #404040;
          background-color: var(--bg-primary);
          color: var(--text-primary);
        }
        
        body.dark-mode .content-wrapper {
          background-color: var(--bg-primary) !important;
        }
        
        body.dark-mode .box {
          background-color: var(--bg-secondary);
          border-color: var(--border-color);
          color: var(--text-primary);
        }
        
        body.dark-mode .box-header {
          background-color: #606060 !important;
          color: #ffffff !important;
          border-bottom: 2px solid #757575 !important;
        }
        
        body.dark-mode .info-box {
          background: linear-gradient(135deg, var(--bg-secondary) 0%, #3a3a3a 100%);
          border-color: var(--border-color);
          color: var(--text-primary);
        }
        
        body.dark-mode .info-box-icon {
          background-color: #007bff !important;
          color: var(--text-primary) !important;
        }
        
        body.dark-mode .form-control {
          background-color: var(--bg-secondary);
          border-color: var(--border-color);
          color: var(--text-primary);
        }
        
        body.dark-mode .form-control:focus {
          background-color: var(--bg-secondary);
          border-color: var(--gunmetal-primary);
          color: var(--text-primary);
        }
        
        body.dark-mode .dataTables_wrapper {
          background-color: var(--bg-secondary);
          color: var(--text-primary);
        }
        
        body.dark-mode table.dataTable {
          color: var(--text-primary);
        }
        
        body.dark-mode table.dataTable tbody tr:hover {
          background-color: rgba(255, 255, 255, 0.1);
        }
        
        body.dark-mode .selectize-input {
          background-color: var(--bg-secondary);
          border-color: var(--border-color);
          color: var(--text-primary);
        }
        
        body.dark-mode .selectize-dropdown {
          background-color: var(--bg-secondary);
          border-color: var(--border-color);
          color: var(--text-primary);
        }
        
        /* Animations */
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .box, .info-box {
          animation: fadeIn 0.6s ease-out;
        }
        
        /* Responsive Design */
        @media (max-width: 768px) {
          .main-header .navbar-brand {
            font-size: 14px;
          }
          
          #theme-toggle {
            font-size: 12px;
            padding: 5px 10px;
          }
        }
      ")),
      tags$script(HTML("
        // Theme Toggle Functionality
        function toggleTheme() {
          const body = document.body;
          const themeIcon = document.getElementById('theme-icon');
          const themeButton = document.getElementById('theme-toggle');
          
          if (body.classList.contains('dark-mode')) {
            // Switch to Light Mode
            body.classList.remove('dark-mode');
            themeIcon.className = 'fas fa-moon';
            themeButton.innerHTML = '<i class=\"fas fa-moon\" id=\"theme-icon\"></i> Dark Mode';
            localStorage.setItem('theme', 'light');
          } else {
            // Switch to Dark Mode
            body.classList.add('dark-mode');
            themeIcon.className = 'fas fa-sun';
            themeButton.innerHTML = '<i class=\"fas fa-sun\" id=\"theme-icon\"></i> Light Mode';
            localStorage.setItem('theme', 'dark');
          }
        }
        
        // Load saved theme on page load
        document.addEventListener('DOMContentLoaded', function() {
          const savedTheme = localStorage.getItem('theme');
          const themeIcon = document.getElementById('theme-icon');
          const themeButton = document.getElementById('theme-toggle');
          
          if (savedTheme === 'dark') {
            document.body.classList.add('dark-mode');
            if (themeIcon) themeIcon.className = 'fas fa-sun';
            if (themeButton) themeButton.innerHTML = '<i class=\"fas fa-sun\" id=\"theme-icon\"></i> Light Mode';
          }
        });
        
        // Smooth transitions for theme switching
        document.body.style.transition = 'background-color 0.3s ease, color 0.3s ease';
      "))
    ),
    tabItems(
      # Beranda Tab
      tabItem(tabName = "beranda",
              fluidRow(
                div(class = "col-md-12",
                    h2("🌟 Vulnera: Dashboard Analisis Kerentanan Sosial", 
                       align = "center", 
                       style = "margin-bottom: 30px; color: var(--gunmetal-primary); font-weight: 700;")
                )
              ),
              fluidRow(
                infoBoxOutput("total_records_box", width = 3),
                infoBoxOutput("districts_box", width = 3),
                infoBoxOutput("variables_box", width = 3),
                infoBoxOutput("data_source_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "ℹ️ Informasi Dashboard",
                  width = 6,
                  div(
                    style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 20px; border-radius: 12px; margin-bottom: 15px;",
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 15px;",
                      div(
                        style = "background: var(--gunmetal-primary); color: white; width: 50px; height: 50px; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin-right: 15px;",
                        tags$i(class = "fas fa-chart-line", style = "font-size: 20px;")
                      ),
                      div(
                        h4("🎯 Dashboard Vulnera", style = "color: var(--gunmetal-primary); margin: 0;"),
                        p("Analisis Kerentanan Sosial Indonesia", style = "color: #6c757d; margin: 0; font-size: 14px;")
                      )
                    ),
                    p("Dashboard komprehensif untuk menganalisis Social Vulnerability Index (SoVI) dengan fitur analisis statistik dan spasial yang lengkap.", 
                      style = "color: #495057; line-height: 1.6; margin-bottom: 15px;"),
                    
                    div(
                      style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                      h5("🚀 Fitur Utama:", style = "color: #0277bd; margin-top: 0; margin-bottom: 8px;"),
                      tags$ul(
                        tags$li("📊 Eksplorasi data interaktif"),
                        tags$li("🧪 Uji statistik lengkap"),
                        tags$li("🗺️ Visualisasi spasial"),
                        tags$li("📈 Analisis regresi"),
                        style = "color: #0288d1; margin-bottom: 0; padding-left: 20px;"
                      )
                    ),
                    
                    div(
                      style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); padding: 12px; border-radius: 8px;",
                      p("💡 ", strong("Tips:"), " Gunakan toggle Dark Mode di header untuk pengalaman visual yang optimal!", 
                        style = "margin: 0; color: #f57c00; font-size: 14px;")
                    )
                  ),
                  
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent-success);",
                    div(
                      style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
                      tags$i(class = "fas fa-external-link-alt", style = "color: #2e7d32;"),
                      tags$strong("📚 Sumber Data:", style = "color: #2e7d32;")
                    ),
                    tags$a(href = metadata_article_url, "ScienceDirect Research Article", target = "_blank", 
                           style = "color: #388e3c; text-decoration: none; font-weight: 600; font-size: 14px;"),
                    p("Data penelitian terverifikasi untuk analisis kerentanan sosial", 
                      style = "color: #43a047; margin: 5px 0 0 0; font-size: 12px;")
                  )
                ),
                box(
                  title = "📊 Metadata Variabel",
                  width = 6,
                  div(
                    style = "background: linear-gradient(135deg, #f3e5f5 0%, #e1bee7 100%); padding: 20px; border-radius: 12px; margin-bottom: 15px;",
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 15px;",
                      div(
                        style = "background: #9c27b0; color: white; width: 50px; height: 50px; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin-right: 15px;",
                        tags$i(class = "fas fa-table", style = "font-size: 20px;")
                      ),
                      div(
                        h4("📋 Struktur Data", style = "color: #7b1fa2; margin: 0;"),
                        p("Variabel dan Deskripsi", style = "color: #8e24aa; margin: 0; font-size: 14px;")
                      )
                    ),
                    
                    div(
                      style = "background: linear-gradient(135deg, #fff8e1 0%, #ffecb3 100%); padding: 12px; border-radius: 8px; margin-bottom: 15px;",
                      h5("📊 Kategori Variabel:", style = "color: #f57c00; margin-top: 0; margin-bottom: 8px;"),
                      fluidRow(
                        column(6,
                               tags$ul(
                                 tags$li("👥 Demografi"),
                                 tags$li("🏠 Sosial Ekonomi"),
                                 style = "color: #ef6c00; margin-bottom: 0; padding-left: 15px;"
                               )
                        ),
                        column(6,
                               tags$ul(
                                 tags$li("🎓 Pendidikan"),
                                 tags$li("🗺️ Geografis"),
                                 style = "color: #ef6c00; margin-bottom: 0; padding-left: 15px;"
                               )
                        )
                      )
                    )
                  ),
                  
                  DTOutput("metadata_table_home"),
                  
                  div(
                    style = "background: linear-gradient(135deg, #fce4ec 0%, #f8bbd9 100%); padding: 12px; border-radius: 8px; margin-top: 15px;",
                    p("📋 ", strong("Total:"), " 12 variabel utama untuk analisis kerentanan sosial komprehensif", 
                      style = "margin: 0; color: #c2185b; font-size: 14px;")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "🚀 Panduan Penggunaan Dashboard",
                  width = 12,
                  div(
                    style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 25px; border-radius: 12px;",
                    fluidRow(
                      column(6,
                             h4("📋 Langkah-langkah Analisis:", style = "color: var(--gunmetal-primary); margin-top: 0;"),
                             div(
                               style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                               tags$ol(
                                 tags$li(div("🏠 ", strong("Beranda:"), " Pahami struktur data"), style = "margin-bottom: 8px; color: #495057;"),
                                 tags$li(div("⚙️ ", strong("Manajemen Data:"), " Kategorisasi variabel"), style = "margin-bottom: 8px; color: #495057;"),
                                 tags$li(div("📊 ", strong("Eksplorasi Data:"), " Analisis deskriptif"), style = "margin-bottom: 8px; color: #495057;"),
                                 tags$li(div("✅ ", strong("Uji Asumsi:"), " Verifikasi normalitas"), style = "margin-bottom: 8px; color: #495057;"),
                                 style = "padding-left: 20px; margin-bottom: 0;"
                               )
                             )
                      ),
                      column(6,
                             h4("🔬 Analisis Lanjutan:", style = "color: var(--gunmetal-primary); margin-top: 0;"),
                             div(
                               style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                               tags$ol(start = 5,
                                 tags$li(div("🧪 ", strong("Statistik Inferensia:"), " Uji hipotesis"), style = "margin-bottom: 8px; color: #495057;"),
                                 tags$li(div("📈 ", strong("Regresi:"), " Hubungan variabel"), style = "margin-bottom: 8px; color: #495057;"),
                                 tags$li(div("🌍 ", strong("Analisis Spasial:"), " Pola geografis"), style = "margin-bottom: 8px; color: #495057;"),
                                 tags$li(div("📋 ", strong("Interpretasi:"), " Kesimpulan hasil"), style = "margin-bottom: 8px; color: #495057;"),
                                 style = "padding-left: 20px; margin-bottom: 0;"
                               )
                             )
                      )
                    ),
                    
                    div(
                      style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); padding: 15px; border-radius: 8px; margin-top: 20px; text-align: center;",
                      h5("🎯 Tujuan Analisis", style = "color: #2e7d32; margin-top: 0; margin-bottom: 10px;"),
                      p("Mengidentifikasi faktor-faktor kerentanan sosial dan pola spasialnya untuk mendukung pengambilan kebijakan yang tepat sasaran", 
                        style = "color: #388e3c; margin-bottom: 0; font-style: italic; line-height: 1.6;")
                    )
                  )
                )
              )
      ),
      
      # Manajemen Data Tab
      tabItem(tabName = "manajemen",
              h2("⚙️ Manajemen Data", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "🔧 Kategorisasi Data",
                  width = 4,
                  selectInput("var_cat", "Pilih Variabel untuk Dikategorikan:", 
                              choices = NULL),
                  sliderInput("breaks", "Jumlah Kategori:", 
                              min = 2, max = 10, value = 3, step = 1),
                  actionButton("categorize_btn", "🚀 Kategorikan Data", 
                               class = "btn-success"),
                  br(), br(),
                  div(
                    style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); padding: 12px; border-radius: 6px; border-left: 4px solid var(--accent-orange);",
                    p("💡 Kolom baru akan ditambahkan dengan nama '[variabel]_cat_[jumlah]'.", style = "margin: 0; color: #e65100;")
                  ),
                  br(),
                  div(id = "status_kategorisasi",
                      style = "border-radius: 8px; margin-top: 10px;",
                      uiOutput("status_message")
                  )
                ),
                box(
                  title = "📊 Hasil Kategorisasi",
                  width = 8,
                  DTOutput("tabel_kat"),
                  br(),
                  uiOutput("interpretasi_kategorisasi"),
                  br(),
                  downloadButton("download_manajemen_csv", "💾 Unduh Data (CSV)", 
                                 class = "btn-primary")
                )
              )
      ),
      
      # Statistik Deskriptif Tab
      tabItem(tabName = "statistik_deskriptif",
              h2("📈 Statistik Deskriptif", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "📋 Ringkasan Statistik",
                  width = 8,
                  selectInput("var_explorasi_desc", "Pilih Variabel:", choices = NULL),
                  verbatimTextOutput("summary_var"),
                  p("📋 Statistik ini menunjukkan distribusi dasar variabel yang dipilih seperti mean, median, kuartil, dan sebaran data.")
                ),
                box(
                  title = "📊 Informasi Variabel",
                  width = 4,
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent-success);",
                    h4("💡 Tips Interpretasi:", style = "color: #2e7d32; margin-top: 0;"),
                    tags$ul(
                      tags$li("Mean: Rata-rata nilai"),
                      tags$li("Median: Nilai tengah"),
                      tags$li("Q1-Q3: Rentang kuartil"),
                      tags$li("Min-Max: Rentang data"),
                      style = "color: #388e3c; margin-bottom: 0;"
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "📋 Tabel Data Lengkap",
                  width = 12,
                  DTOutput("full_data_table"),
                  p("📋 Tabel lengkap menampilkan seluruh data untuk eksplorasi lebih detail.")
                )
              )
      ),
      
      # Histogram Tab
      tabItem(tabName = "histogram",
              h2("📊 Histogram", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "📊 Distribusi Frekuensi",
                  width = 8,
                  selectInput("var_plot_hist", "Pilih Variabel:", choices = NULL),
                  plotOutput("histPlot", height = "400px"),
                  p("📊 Histogram menunjukkan distribusi frekuensi data dan pola sebaran nilai.")
                ),
                box(
                  title = "⚙️ Pengaturan Histogram",
                  width = 4,
                  sliderInput("hist_bins", "Jumlah Bins:", min = 5, max = 50, value = 20),
                  div(
                    style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent-orange);",
                    h4("📈 Interpretasi:", style = "color: #f57c00; margin-top: 0;"),
                    tags$ul(
                      tags$li("Bentuk distribusi (normal, skewed, dll)"),
                      tags$li("Puncak distribusi (mode)"),
                      tags$li("Penyebaran data"),
                      tags$li("Outlier atau nilai ekstrem"),
                      style = "color: #ef6c00; margin-bottom: 0;"
                    )
                  )
                )
              )
      ),
      
      # Boxplot Tab
      tabItem(tabName = "boxplot",
              h2("📦 Boxplot", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "📦 Diagram Kotak-Garis",
                  width = 8,
                  selectInput("var_plot_box", "Pilih Variabel:", choices = NULL),
                  plotOutput("boxPlot", height = "400px"),
                  p("📦 Boxplot menunjukkan median, kuartil, dan outlier dalam data.")
                ),
                box(
                  title = "📊 Komponen Boxplot",
                  width = 4,
                  div(
                    style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent-teal);",
                    h4("🔍 Komponen:", style = "color: #0277bd; margin-top: 0;"),
                    tags$ul(
                      tags$li("Garis tengah: Median (Q2)"),
                      tags$li("Kotak: Q1 hingga Q3 (IQR)"),
                      tags$li("Whiskers: Rentang normal"),
                      tags$li("Titik: Outlier"),
                      style = "color: #0288d1; margin-bottom: 0;"
                    )
                  ),
                  br(),
                  div(
                    style = "background: linear-gradient(135deg, #f3e5f5 0%, #e1bee7 100%); padding: 15px; border-radius: 8px; border-left: 4px solid #9c27b0;",
                    h4("💡 Kegunaan:", style = "color: #7b1fa2; margin-top: 0;"),
                    p("Identifikasi outlier dan perbandingan distribusi antar variabel", style = "color: #8e24aa; margin-bottom: 0;")
                  )
                )
              )
      ),
      
      # Peta Tab
      tabItem(tabName = "peta",
              h2("🗺️ Peta Choropleth", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "🗺️ Peta Distribusi Spasial",
                  width = 8,
                  selectInput("map_indicator", "Pilih Indikator untuk Peta:", choices = NULL),
                  leafletOutput("choropleth_map", height = "500px"),
                  p("🗺️ Peta menunjukkan distribusi spasial berdasarkan indikator yang dipilih dengan gradasi warna.")
                ),
                box(
                  title = "🎨 Pengaturan Peta",
                  width = 4,
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent-success);",
                    h4("🗺️ Interpretasi Peta:", style = "color: #2e7d32; margin-top: 0;"),
                    tags$ul(
                      tags$li("Warna gelap: Nilai tinggi"),
                      tags$li("Warna terang: Nilai rendah"),
                      tags$li("Pola klaster: Daerah serupa"),
                      tags$li("Hover: Detail nilai"),
                      style = "color: #388e3c; margin-bottom: 0;"
                    )
                  ),
                  br(),
                  div(
                    style = "background: linear-gradient(135deg, #fff8e1 0%, #ffecb3 100%); padding: 15px; border-radius: 8px; border-left: 4px solid #ff8f00;",
                    h4("🔍 Tips:", style = "color: #f57c00; margin-top: 0;"),
                    p("Gunakan zoom dan pan untuk eksplorasi detail wilayah tertentu", style = "color: #ef6c00; margin-bottom: 0;")
                  )
                )
              )
      ),
      
      # Analisis Spasial Eksplorasi Tab
      tabItem(tabName = "analisis_spasial_ekspl",
              h2("🌍 Analisis Spasial Eksplorasi", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "📊 Statistik Spasial Dasar",
                  width = 6,
                  selectInput("spatial_var_ekspl", "Pilih Variabel untuk Analisis:", choices = NULL),
                  actionButton("run_spatial_stats", "🧮 Hitung Statistik Spasial", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("spatial_stats_output"),
                  p("📊 Statistik spasial dasar untuk memahami pola distribusi geografis.")
                ),
                box(
                  title = "🗺️ Peta Autokorelasi",
                  width = 6,
                  plotOutput("spatial_autocorr_plot", height = "300px"),
                  p("🗺️ Visualisasi pola autokorelasi spasial untuk identifikasi klaster.")
                )
              ),
              fluidRow(
                box(
                  title = "📈 Interpretasi Analisis Spasial",
                  width = 12,
                  div(
                    style = "background: linear-gradient(135deg, #e8eaf6 0%, #c5cae9 100%); padding: 20px; border-radius: 8px; border-left: 4px solid #3f51b5;",
                    h4("🔍 Panduan Interpretasi:", style = "color: #3949ab; margin-top: 0;"),
                    fluidRow(
                      column(6,
                             h5("🎯 Autokorelasi Positif:", style = "color: #5c6bc0;"),
                             tags$ul(
                               tags$li("Nilai serupa berkelompok"),
                               tags$li("Pola klaster yang jelas"),
                               tags$li("Dependensi spasial kuat"),
                               style = "color: #7986cb;"
                             )
                      ),
                      column(6,
                             h5("🎯 Autokorelasi Negatif:", style = "color: #5c6bc0;"),
                             tags$ul(
                               tags$li("Nilai berbeda berdekatan"),
                               tags$li("Pola checkerboard"),
                               tags$li("Kompetisi spasial"),
                               style = "color: #7986cb;"
                             )
                      )
                    )
                  )
                )
              )
      ),
      
      # Uji Normalitas Tab
      tabItem(tabName = "uji_normalitas",
              h2("📊 Uji Normalitas", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "📈 Q-Q Plot",
                  width = 8,
                  selectInput("var_norm", "Pilih Variabel:", choices = NULL),
                  plotOutput("qqplot", height = "400px"),
                  p("📈 Q-Q plot membandingkan distribusi data dengan distribusi normal teoritis.")
                ),
                box(
                  title = "📊 Hasil Uji Statistik",
                  width = 4,
                  verbatimTextOutput("uji_norm"),
                  br(),
                  div(
                    style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent-success);",
                    h4("💡 Interpretasi:", style = "color: #2e7d32; margin-top: 0;"),
                    tags$ul(
                      tags$li("p-value > 0.05: Normal"),
                      tags$li("p-value ≤ 0.05: Tidak normal"),
                      tags$li("Titik pada garis: Normal"),
                      tags$li("Titik menyebar: Tidak normal"),
                      style = "color: #388e3c; margin-bottom: 0;"
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "📋 Panduan Uji Normalitas",
                  width = 12,
                  div(
                    style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); padding: 20px; border-radius: 8px; border-left: 4px solid var(--accent-teal);",
                    h4("🔍 Shapiro-Wilk Test:", style = "color: #0277bd; margin-top: 0;"),
                    fluidRow(
                      column(6,
                             h5("✅ Asumsi Terpenuhi:", style = "color: #0288d1;"),
                             tags$ul(
                               tags$li("p-value > α (0.05)"),
                               tags$li("Data berdistribusi normal"),
                               tags$li("Dapat menggunakan uji parametrik"),
                               style = "color: #039be5;"
                             )
                      ),
                      column(6,
                             h5("❌ Asumsi Dilanggar:", style = "color: #0288d1;"),
                             tags$ul(
                               tags$li("p-value ≤ α (0.05)"),
                               tags$li("Data tidak berdistribusi normal"),
                               tags$li("Gunakan uji non-parametrik"),
                               style = "color: #039be5;"
                             )
                      )
                    )
                  )
                )
              )
      ),
      
      # Uji Homogenitas Tab
      tabItem(tabName = "uji_homogenitas",
              h2("⚖️ Uji Homogenitas Varians", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "🔧 Pengaturan Uji",
                  width = 4,
                  selectInput("var_homogen_group", "Pilih Variabel Grup:", choices = NULL),
                  actionButton("run_homogen_test", "🚀 Jalankan Uji", class = "btn-primary"),
                  br(), br(),
                  div(
                    style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); padding: 15px; border-radius: 8px; border-left: 4px solid var(--accent-orange);",
                    h4("ℹ️ Informasi:", style = "color: #f57c00; margin-top: 0;"),
                    p("Uji homogenitas akan dilakukan untuk semua variabel numerik terhadap variabel grup yang dipilih.", 
                      style = "color: #ef6c00; margin-bottom: 0;")
                  )
                ),
                box(
                  title = "📊 Hasil Uji Homogenitas",
                  width = 8,
                  verbatimTextOutput("uji_var"),
                  p("⚖️ Uji Levene untuk menguji homogenitas varians antar kelompok.")
                )
              ),
              fluidRow(
                box(
                  title = "📈 Interpretasi Hasil",
                  width = 12,
                  div(
                    style = "background: linear-gradient(135deg, #f3e5f5 0%, #e1bee7 100%); padding: 20px; border-radius: 8px; border-left: 4px solid #9c27b0;",
                    h4("🔍 Levene's Test:", style = "color: #7b1fa2; margin-top: 0;"),
                    fluidRow(
                      column(4,
                             h5("✅ Homogen:", style = "color: #8e24aa;"),
                             tags$ul(
                               tags$li("p-value > 0.05"),
                               tags$li("Varians antar grup sama"),
                               tags$li("Asumsi ANOVA terpenuhi"),
                               style = "color: #9c27b0;"
                             )
                      ),
                      column(4,
                             h5("❌ Tidak Homogen:", style = "color: #8e24aa;"),
                             tags$ul(
                               tags$li("p-value ≤ 0.05"),
                               tags$li("Varians antar grup berbeda"),
                               tags$li("Gunakan Welch's test"),
                               style = "color: #9c27b0;"
                             )
                      ),
                      column(4,
                             h5("🔧 Solusi:", style = "color: #8e24aa;"),
                             tags$ul(
                               tags$li("Transformasi data"),
                               tags$li("Uji non-parametrik"),
                               tags$li("Robust statistics"),
                               style = "color: #9c27b0;"
                             )
                      )
                    )
                  )
                )
              )
      ),
      
      # Uji Beda Rata-rata Tab
      tabItem(tabName = "uji_beda_rata",
              h2("🧪 Uji Beda Rata-rata", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "1️⃣ Uji T Satu Sampel",
                  width = 6,
                  numericInput("mu", "Nilai Hipotesis (μ₀):", 0),
                  selectInput("ttest_var_one", "Pilih Variabel:", choices = NULL),
                  actionButton("run_ttest_one", "🚀 Jalankan Uji", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("hasil_ttest_one")
                ),
                box(
                  title = "2️⃣ Uji T Dua Sampel",
                  width = 6,
                  selectInput("group_two_samp", "Variabel Grup:", choices = NULL),
                  selectInput("nilai_two_samp", "Variabel Nilai:", choices = NULL),
                  checkboxInput("var_equal", "Asumsikan Varians Sama", value = TRUE),
                  actionButton("run_ttest_two", "🚀 Jalankan Uji", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("hasil_ttest_two")
                )
              )
      ),
      
      # Uji Proporsi & Varians Tab
      tabItem(tabName = "uji_prop_var",
              h2("📊 Uji Proporsi & Varians", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "📈 Uji Proporsi Satu Sampel",
                  width = 6,
                  numericInput("success_level", "Jumlah Sukses:", value = 0, min = 0),
                  numericInput("total_trials", "Total Percobaan:", value = 0, min = 0),
                  numericInput("p_hyp_one", "Proporsi Hipotesis:", 0.5, min = 0, max = 1),
                  actionButton("run_prop_one", "🚀 Jalankan Uji", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("hasil_prop_one")
                ),
                box(
                  title = "📊 Uji Varians Dua Sampel",
                  width = 6,
                  selectInput("var_var_two_val", "Variabel Nilai:", choices = NULL),
                  selectInput("var_var_two_group", "Variabel Grup:", choices = NULL),
                  actionButton("run_var_two", "🚀 Jalankan Uji", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("hasil_var_two")
                )
              )
      ),
      
      # ANOVA Tab
      tabItem(tabName = "anova",
              h2("🔬 ANOVA", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "1️⃣ ANOVA Satu Arah",
                  width = 6,
                  selectInput("respon_anova_one", "Variabel Respon:", choices = NULL),
                  selectInput("faktor_anova_one", "Faktor:", choices = NULL),
                  actionButton("run_anova_one", "🚀 Jalankan ANOVA", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("hasil_anova_one")
                ),
                box(
                  title = "2️⃣ ANOVA Dua Arah",
                  width = 6,
                  selectInput("respon_anova_two", "Variabel Respon:", choices = NULL),
                  selectInput("faktor1_anova_two", "Faktor 1:", choices = NULL),
                  selectInput("faktor2_anova_two", "Faktor 2:", choices = NULL),
                  actionButton("run_anova_two", "🚀 Jalankan ANOVA", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("hasil_anova_two")
                )
              )
      ),
      
      # Regresi Tab
      tabItem(tabName = "regresi",
              h2("📈 Regresi Linear Berganda", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "🔧 Model Regresi",
                  width = 6,
                  selectInput("y_var", "Variabel Respon (Y):", choices = NULL),
                  selectizeInput("x_var", "Variabel Prediktor (X):", 
                                 choices = NULL, multiple = TRUE),
                  actionButton("run_reg", "🚀 Jalankan Regresi", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("reg_output")
                ),
                box(
                  title = "🔍 Diagnostik Model",
                  width = 6,
                  h4("📊 Residuals vs Fitted"),
                  plotOutput("residual_fitted_plot", height = "200px"),
                  h4("📈 Normal Q-Q Plot"),
                  plotOutput("residual_qq_plot", height = "200px"),
                  verbatimTextOutput("vif_output")
                )
              )
      ),
      
      # Analisis Spasial Tab
      tabItem(tabName = "spasial",
              h2("🌍 Analisis Spasial", align = "center", style = "color: var(--gunmetal-primary); margin-bottom: 30px;"),
              fluidRow(
                box(
                  title = "📊 Indeks Kerentanan Sosial (SoVI)",
                  width = 6,
                  p("Perhitungan indeks komposit berdasarkan semua indikator kerentanan sosial."),
                  actionButton("calculate_sovi", "🧮 Hitung SoVI", class = "btn-success"),
                  br(), br(),
                  verbatimTextOutput("sovi_calculation"),
                  br(),
                  plotOutput("sovi_histogram", height = "250px")
                ),
                box(
                  title = "🗺️ Peta SoVI",
                  width = 6,
                  leafletOutput("sovi_map"),
                  p("🗺️ Distribusi spasial Indeks Kerentanan Sosial di seluruh wilayah.")
                )
              ),
              fluidRow(
                box(
                  title = "⚖️ Matriks Pembobot Spasial",
                  width = 6,
                  p("Matriks pembobot berdasarkan jarak antar distrik."),
                  DTOutput("distance_matrix_table"),
                  br(),
                  verbatimTextOutput("distance_matrix_summary")
                ),
                box(
                  title = "📈 Uji Autokorelasi Spasial (Moran's I)",
                  width = 6,
                  selectInput("moran_var", "Pilih Variabel untuk Uji Moran's I:", choices = NULL),
                  actionButton("run_moran", "🚀 Jalankan Uji Moran's I", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("moran_output"),
                  br(),
                  plotOutput("moran_plot", height = "250px")
                )
              ),
              fluidRow(
                box(
                  title = "🎯 Hotspot Analysis (Getis-Ord Gi*)",
                  width = 12,
                  selectInput("hotspot_var", "Pilih Variabel untuk Analisis Hotspot:", choices = NULL),
                  actionButton("run_hotspot", "🔥 Jalankan Analisis Hotspot", class = "btn-warning"),
                  br(), br(),
                  div(
                    style = "display: flex; gap: 20px;",
                    div(style = "flex: 1;", verbatimTextOutput("hotspot_output")),
                    div(style = "flex: 1;", leafletOutput("hotspot_map", height = "400px"))
                  )
                )
              )
      )
    )
  )
)

# 9. Server Logic
server <- function(input, output, session) {
  
  # Reactive values
  current_data <- reactiveVal(sovi_data_global)
  categorization_status <- reactiveVal("")
  sovi_calculated <- reactiveVal(NULL)
  
  # Helper function to get variable types
  get_variable_types <- function(data) {
    if (is.null(data)) return(list(numeric = character(0), categorical = character(0)))
    
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    categorical_vars <- names(data)[!sapply(data, is.numeric)]
    
    list(numeric = numeric_vars, categorical = categorical_vars)
  }
  
  # Update choices
  observe({
    data <- current_data()
    if (!is.null(data)) {
      var_types <- get_variable_types(data)
      
      tryCatch({
        updateSelectInput(session, "var_cat", choices = var_types$numeric)
        updateSelectInput(session, "var_explorasi_desc", choices = names(data))
        updateSelectInput(session, "var_plot_hist", choices = var_types$numeric)
        updateSelectInput(session, "var_plot_box", choices = var_types$numeric)
        updateSelectInput(session, "map_indicator", choices = var_types$numeric)
        updateSelectInput(session, "spatial_var_ekspl", choices = var_types$numeric)
        updateSelectInput(session, "var_norm", choices = var_types$numeric)
        updateSelectInput(session, "var_homogen_val", choices = var_types$numeric)
        updateSelectInput(session, "var_homogen_group", choices = var_types$categorical)
        updateSelectInput(session, "ttest_var_one", choices = var_types$numeric)
        updateSelectInput(session, "group_two_samp", choices = var_types$categorical)
        updateSelectInput(session, "nilai_two_samp", choices = var_types$numeric)
        updateSelectInput(session, "var_var_two_val", choices = var_types$numeric)
        updateSelectInput(session, "var_var_two_group", choices = var_types$categorical)
        updateSelectInput(session, "respon_anova_one", choices = var_types$numeric)
        updateSelectInput(session, "faktor_anova_one", choices = var_types$categorical)
        updateSelectInput(session, "respon_anova_two", choices = var_types$numeric)
        updateSelectInput(session, "faktor1_anova_two", choices = var_types$categorical)
        updateSelectInput(session, "faktor2_anova_two", choices = var_types$categorical)
        updateSelectInput(session, "y_var", choices = var_types$numeric)
        updateSelectizeInput(session, "x_var", choices = var_types$numeric)
        updateSelectInput(session, "moran_var", choices = var_types$numeric)
        updateSelectInput(session, "hotspot_var", choices = var_types$numeric)
      }, error = function(e) {
        message("Error updating select inputs: ", e$message)
      })
    }
  })
  
  # Status message
  output$status_message <- renderUI({
    status <- categorization_status()
    if (status == "") {
      div(class = "status-default", p("🔄 Siap untuk kategorisasi data.", style = "color: #666; margin: 0;"))
    } else if (status == "processing") {
      div(class = "status-processing", p("⏳ Sedang memproses kategorisasi...", style = "margin: 0;"))
    } else if (status == "success") {
      div(class = "status-success", p("✅ Kategorisasi berhasil! Variabel baru tersedia di menu lain.", style = "margin: 0;"))
    } else if (status == "error") {
      div(class = "status-error", p("❌ Terjadi kesalahan dalam kategorisasi.", style = "margin: 0;"))
    }
  })
  
  # Kategorisasi data
  observeEvent(input$categorize_btn, {
    categorization_status("processing")
    
    if (is.null(input$var_cat) || input$var_cat == "" || is.null(input$breaks)) {
      categorization_status("error")
      return()
    }
    
    tryCatch({
      data <- current_data()
      selected_var <- input$var_cat
      num_breaks <- input$breaks
      
      if (is.null(data) || !selected_var %in% names(data)) {
        stop("Variabel tidak ditemukan dalam data")
      }
      
      if (!is.numeric(data[[selected_var]])) {
        stop("Variabel yang dipilih bukan numerik")
      }
      
      valid_data <- data[[selected_var]][!is.na(data[[selected_var]])]
      if (length(valid_data) < 2) {
        stop("Data tidak cukup untuk kategorisasi")
      }
      
      new_col_name <- paste0(selected_var, "_cat_", num_breaks)
      
      breaks_seq <- seq(
        min(valid_data, na.rm = TRUE),
        max(valid_data, na.rm = TRUE),
        length.out = num_breaks + 1
      )
      
      new_data <- data
      new_data[[new_col_name]] <- cut(
        new_data[[selected_var]],
        breaks = breaks_seq,
        include.lowest = TRUE,
        labels = paste0("Kategori_", 1:num_breaks),
        right = TRUE
      )
      
      current_data(new_data)
      categorization_status("success")
      
    }, error = function(e) {
      categorization_status("error")
      message("Error during categorization: ", e$message)
    })
  })
  
  # Info boxes
  output$total_records_box <- renderInfoBox({
    data <- current_data()
    infoBox(
      "📊 Total Data",
      value = if(!is.null(data)) nrow(data) else "N/A",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$districts_box <- renderInfoBox({
    data <- current_data()
    infoBox(
      "🗺️ Jumlah Distrik",
      value = if(!is.null(data) && "DISTRICTCODE" %in% names(data)) 
        length(unique(data$DISTRICTCODE)) else "N/A",
      icon = icon("map-marker-alt"),
      color = "green"
    )
  })
  
  output$variables_box <- renderInfoBox({
    data <- current_data()
    infoBox(
      "📋 Jumlah Variabel",
      value = if(!is.null(data)) ncol(data) else "N/A",
      icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$data_source_box <- renderInfoBox({
    infoBox(
      "📚 Sumber Data",
      value = "ScienceDirect",
      icon = icon("book"),
      color = "purple"
    )
  })
  
  # Metadata table
  output$metadata_table_home <- renderDT({
    datatable(metadata_sovi, options = list(
      pageLength = 5,
      dom = 'tp',
      scrollX = TRUE
    ))
  })
  
  # Tabel kategorisasi
  output$tabel_kat <- renderDT({
    data <- current_data()
    if (!is.null(data)) {
      datatable(data, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # Interpretasi kategorisasi
  output$interpretasi_kategorisasi <- renderUI({
    data <- current_data()
    status <- categorization_status()
    
    if (status == "success" && !is.null(data)) {
      cat_cols <- names(data)[grepl("_cat_", names(data))]
      
      if (length(cat_cols) > 0) {
        latest_col <- tail(cat_cols, 1)
        freq_table <- table(data[[latest_col]], useNA = "ifany")
        freq_text <- paste(names(freq_table), " (", freq_table, ")", collapse = ", ")
        
        div(
          h4("🎉 Hasil Kategorisasi:", style = "color: #27ae60;"),
          p("📊 Kolom baru:", strong(latest_col)),
          p("📈 Distribusi:", freq_text),
          p("🔗 Variabel ini sekarang tersedia di menu Uji Asumsi, Statistik Inferensia, dan ANOVA.", 
            style = "color: #2980b9; font-style: italic;")
        )
      } else {
        p("✅ Data berhasil diproses.")
      }
    } else {
      p("🔧 Pilih variabel numerik dan jumlah kategori, lalu klik 'Kategorikan Data'.")
    }
  })
  
  # Download handler
  output$download_manajemen_csv <- downloadHandler(
    filename = function() {
      paste0("data_kategorik_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- current_data()
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
  
  # Statistik deskriptif
  output$summary_var <- renderPrint({
    req(input$var_explorasi_desc)
    data <- current_data()
    if (!is.null(data) && input$var_explorasi_desc %in% names(data)) {
      summary(data[[input$var_explorasi_desc]])
    }
  })
  
  # Histogram with dynamic bins
  output$histPlot <- renderPlot({
    req(input$var_plot_hist)
    data <- current_data()
    bins <- ifelse(is.null(input$hist_bins), 20, input$hist_bins)
    
    if (!is.null(data) && input$var_plot_hist %in% names(data)) {
      ggplot(data, aes(x = .data[[input$var_plot_hist]])) +
        geom_histogram(bins = bins, fill = "#2a3439", color = "#3d4a52", alpha = 0.8) +
        ggtitle(paste("Histogram:", input$var_plot_hist, "(", bins, "bins)")) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#2a3439", size = 14, face = "bold"),
          axis.title = element_text(color = "#2a3439"),
          axis.text = element_text(color = "#3d4a52")
        )
    }
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    req(input$var_plot_box)
    data <- current_data()
    if (!is.null(data) && input$var_plot_box %in% names(data)) {
      ggplot(data, aes(y = .data[[input$var_plot_box]])) +
        geom_boxplot(fill = "#17a2b8", color = "#2a3439", alpha = 0.8) +
        ggtitle(paste("Boxplot:", input$var_plot_box)) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#2a3439", size = 14, face = "bold"),
          axis.title = element_text(color = "#2a3439"),
          axis.text = element_text(color = "#3d4a52")
        )
    }
  })
  
  # Choropleth Map
  output$choropleth_map <- renderLeaflet({
    req(input$map_indicator)
    
    if (is.null(geojson_data)) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "GeoJSON data tidak tersedia. Menggunakan data sampel."))
    }
    
    data_for_map <- current_data()
    selected_indicator <- input$map_indicator
    
    # Find ID column in GeoJSON
    get_geojson_id_col <- function(sf_obj) {
      names_sf <- names(st_drop_geometry(sf_obj))
      possible_ids <- c("DISTRICTCODE", "districtcode", "id", "ID", "ADM2_PCODE", "kode", "KAB_KOTA_KO", "NAME_2")
      for (id_col in possible_ids) {
        if (id_col %in% names_sf) return(id_col)
      }
      return(NULL)
    }
    
    geojson_id_col_name <- get_geojson_id_col(geojson_data)
    if (is.null(geojson_id_col_name)) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "GeoJSON tidak memiliki kolom ID yang dikenali."))
    }
    
    # Prepare data for joining
    geojson_processed <- geojson_data %>%
      mutate(DISTRICTCODE_JOIN = as.character(trimws(!!sym(geojson_id_col_name))))
    
    data_processed <- data_for_map
    if ("DISTRICTCODE" %in% names(data_processed)) {
      data_processed$DISTRICTCODE_JOIN <- as.character(trimws(data_processed$DISTRICTCODE))
    } else {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "Data tidak memiliki kolom DISTRICTCODE."))
    }
    
    # Check if indicator exists
    if (!selected_indicator %in% names(data_processed)) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, paste0("Indikator '", selected_indicator, "' tidak ditemukan.")))
    }
    
    # Join data
    data_subset <- data_processed %>% 
      select(DISTRICTCODE_JOIN, !!sym(selected_indicator))
    
    map_data <- inner_join(geojson_processed, data_subset, by = "DISTRICTCODE_JOIN")
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "Tidak ada data yang cocok antara GeoJSON dan data atribut."))
    }
    
    # Create color palette
    values <- map_data[[selected_indicator]]
    valid_values <- values[!is.na(values)]
    
    if (length(valid_values) < 2) {
      pal <- colorFactor("viridis", domain = unique(valid_values), na.color = "#808080")
    } else {
      bins <- quantile(valid_values, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
      bins <- unique(bins)
      if (length(bins) < 2) {
        bins <- c(min(valid_values, na.rm = TRUE), max(valid_values, na.rm = TRUE))
      }
      pal <- colorBin("viridis", domain = valid_values, bins = bins, na.color = "#808080")
    }
    
    # Create labels
    label_name <- if ("NAME_2" %in% names(map_data)) {
      map_data$NAME_2
    } else if ("Kab_Kota" %in% names(map_data)) {
      map_data$Kab_Kota
    } else {
      map_data$DISTRICTCODE_JOIN
    }
    
    label_text <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      label_name, 
      selected_indicator, 
      round(values, 2)
    )
    labels <- lapply(label_text, htmltools::HTML)
    
    # Create map
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(values),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = values, 
        opacity = 0.7, 
        title = selected_indicator,
        position = "bottomright"
      )
  })
  
  # Full data table
  output$full_data_table <- renderDT({
    data <- current_data()
    if (!is.null(data)) {
      datatable(data, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # Q-Q Plot
  output$qqplot <- renderPlot({
    req(input$var_norm)
    data <- current_data()
    if (!is.null(data) && input$var_norm %in% names(data)) {
      qqnorm(data[[input$var_norm]], main = paste("Q-Q Plot:", input$var_norm),
             col = "#2a3439", pch = 16)
      qqline(data[[input$var_norm]], col = "#17a2b8", lwd = 2)
    }
  })
  
  # Uji normalitas
  output$uji_norm <- renderPrint({
    req(input$var_norm)
    data <- current_data()
    if (!is.null(data) && input$var_norm %in% names(data)) {
      tryCatch({
        result <- shapiro.test(data[[input$var_norm]])
        cat("📊 Hasil Uji Shapiro-Wilk:\n")
        print(result)
        cat("\n💡 Interpretasi: p-value < 0.05 menunjukkan data tidak normal.\n")
      }, error = function(e) {
        cat("❌ Error dalam uji normalitas:", e$message)
      })
    }
  })
  
  # Uji homogenitas
  output$uji_var <- renderPrint({
    req(input$var_homogen_val, input$var_homogen_group)
    data <- current_data()
    if (!is.null(data)) {
      tryCatch({
        formula_str <- paste(input$var_homogen_val, "~", input$var_homogen_group)
        result <- car::leveneTest(as.formula(formula_str), data = data)
        cat("⚖️ Hasil Uji Levene:\n")
        print(result)
        cat("\n💡 Interpretasi: p-value < 0.05 menunjukkan varians tidak homogen.\n")
      }, error = function(e) {
        cat("❌ Error dalam uji homogenitas:", e$message)
      })
    }
  })
  
  # T-test satu sampel
  observeEvent(input$run_ttest_one, {
    output$hasil_ttest_one <- renderPrint({
      req(input$ttest_var_one, input$mu)
      data <- current_data()
      if (!is.null(data)) {
        tryCatch({
          result <- t.test(data[[input$ttest_var_one]], mu = input$mu)
          cat("🧪 Hasil Uji T Satu Sampel:\n")
          print(result)
        }, error = function(e) {
          cat("❌ Error:", e$message)
        })
      }
    })
  })
  
  # T-test dua sampel
  observeEvent(input$run_ttest_two, {
    output$hasil_ttest_two <- renderPrint({
      req(input$group_two_samp, input$nilai_two_samp)
      data <- current_data()
      if (!is.null(data)) {
        tryCatch({
          formula_str <- paste(input$nilai_two_samp, "~", input$group_two_samp)
          result <- t.test(as.formula(formula_str), data = data, var.equal = input$var_equal)
          cat("🧪 Hasil Uji T Dua Sampel:\n")
          print(result)
        }, error = function(e) {
          cat("❌ Error:", e$message)
        })
      }
    })
  })
  
  # Uji proporsi
  observeEvent(input$run_prop_one, {
    output$hasil_prop_one <- renderPrint({
      req(input$success_level, input$total_trials, input$p_hyp_one)
      tryCatch({
        result <- prop.test(x = input$success_level, n = input$total_trials, p = input$p_hyp_one)
        cat("📈 Hasil Uji Proporsi:\n")
        print(result)
      }, error = function(e) {
        cat("❌ Error:", e$message)
      })
    })
  })
  
  # Uji varians dua sampel
  observeEvent(input$run_var_two, {
    output$hasil_var_two <- renderPrint({
      req(input$var_var_two_val, input$var_var_two_group)
      data <- current_data()
      if (!is.null(data)) {
        tryCatch({
          formula_str <- paste(input$var_var_two_val, "~", input$var_var_two_group)
          result <- var.test(as.formula(formula_str), data = data)
          cat("📊 Hasil Uji Varians:\n")
          print(result)
        }, error = function(e) {
          cat("❌ Error:", e$message)
        })
      }
    })
  })
  
  # ANOVA satu arah
  observeEvent(input$run_anova_one, {
    output$hasil_anova_one <- renderPrint({
      req(input$respon_anova_one, input$faktor_anova_one)
      data <- current_data()
      if (!is.null(data)) {
        tryCatch({
          formula_str <- paste(input$respon_anova_one, "~", input$faktor_anova_one)
          fit <- aov(as.formula(formula_str), data = data)
          cat("🔬 Hasil ANOVA Satu Arah:\n")
          print(summary(fit))
        }, error = function(e) {
          cat("❌ Error:", e$message)
        })
      }
    })
  })
  
  # ANOVA dua arah
  observeEvent(input$run_anova_two, {
    output$hasil_anova_two <- renderPrint({
      req(input$respon_anova_two, input$faktor1_anova_two, input$faktor2_anova_two)
      data <- current_data()
      if (!is.null(data)) {
        tryCatch({
          formula_str <- paste(input$respon_anova_two, "~", 
                               input$faktor1_anova_two, "*", input$faktor2_anova_two)
          fit <- aov(as.formula(formula_str), data = data)
          cat("🔬 Hasil ANOVA Dua Arah:\n")
          print(summary(fit))
        }, error = function(e) {
          cat("❌ Error:", e$message)
        })
      }
    })
  })
  
  # Model regresi
  reg_model <- eventReactive(input$run_reg, {
    req(input$y_var, input$x_var)
    data <- current_data()
    if (!is.null(data)) {
      tryCatch({
        formula_str <- paste(input$y_var, "~", paste(input$x_var, collapse = "+"))
        lm(as.formula(formula_str), data = data)
      }, error = function(e) {
        NULL
      })
    }
  })
  
  # Output regresi
  output$reg_output <- renderPrint({
    model <- reg_model()
    if (!is.null(model)) {
      cat("📈 Hasil Regresi Linear:\n")
      print(summary(model))
    } else {
      cat("🔧 Pilih variabel dan jalankan regresi.")
    }
  })
  
  # Plot residual vs fitted
  output$residual_fitted_plot <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      plot(model, which = 1, col = "#2a3439", pch = 16)
    }
  })
  
  # Q-Q plot residual
  output$residual_qq_plot <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      plot(model, which = 2, col = "#2a3439", pch = 16)
    }
  })
  
  # VIF output
  output$vif_output <- renderPrint({
    model <- reg_model()
    if (!is.null(model) && length(coef(model)) > 2) {
      tryCatch({
        vif_result <- car::vif(model)
        cat("📊 Variance Inflation Factor (VIF):\n")
        print(vif_result)
        cat("\n💡 VIF > 5 menunjukkan multikolinearitas.\n")
      }, error = function(e) {
        cat("❌ VIF tidak dapat dihitung:", e$message)
      })
    } else {
      cat("🔧 Diperlukan minimal 2 prediktor untuk VIF.")
    }
  })
  
  # SPATIAL ANALYSIS FUNCTIONS
  
  # Calculate SoVI
  observeEvent(input$calculate_sovi, {
    output$sovi_calculation <- renderPrint({
      data <- current_data()
      if (!is.null(data)) {
        tryCatch({
          # Select vulnerability indicators (exclude DISTRICTCODE)
          vulnerability_vars <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                                  "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING")
          
          available_vars <- vulnerability_vars[vulnerability_vars %in% names(data)]
          
          if (length(available_vars) < 3) {
            stop("Tidak cukup variabel kerentanan untuk menghitung SoVI")
          }
          
          # Standardize variables (z-score)
          sovi_data <- data[available_vars]
          sovi_standardized <- scale(sovi_data)
          
          # Calculate SoVI as sum of standardized scores
          sovi_scores <- rowSums(sovi_standardized, na.rm = TRUE)
          
          # Add to current data
          updated_data <- data
          updated_data$SoVI <- sovi_scores
          current_data(updated_data)
          sovi_calculated(sovi_scores)
          
          cat("🧮 Perhitungan Indeks Kerentanan Sosial (SoVI) Berhasil!\n\n")
          cat("📊 Variabel yang digunakan:", paste(available_vars, collapse = ", "), "\n\n")
          cat("📈 Statistik SoVI:\n")
          print(summary(sovi_scores))
          cat("\n💡 Interpretasi:\n")
          cat("- Nilai SoVI yang lebih tinggi menunjukkan kerentanan sosial yang lebih tinggi\n")
          cat("- Nilai negatif menunjukkan kerentanan di bawah rata-rata\n")
          cat("- Nilai positif menunjukkan kerentanan di atas rata-rata\n")
          
        }, error = function(e) {
          cat("❌ Error dalam perhitungan SoVI:", e$message)
        })
      }
    })
    
    # Update SoVI histogram
    output$sovi_histogram <- renderPlot({
      sovi_scores <- sovi_calculated()
      if (!is.null(sovi_scores)) {
        hist(sovi_scores, 
             main = "Distribusi Indeks Kerentanan Sosial (SoVI)",
             xlab = "Nilai SoVI",
             ylab = "Frekuensi",
             col = "#2a3439",
             border = "#3d4a52",
             breaks = 20)
        abline(v = mean(sovi_scores, na.rm = TRUE), col = "#17a2b8", lwd = 2, lty = 2)
        legend("topright", "Rata-rata", col = "#17a2b8", lwd = 2, lty = 2)
      }
    })
  })
  
  # SoVI Map
  output$sovi_map <- renderLeaflet({
    sovi_scores <- sovi_calculated()
    
    if (is.null(sovi_scores) || is.null(geojson_data)) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "Hitung SoVI terlebih dahulu atau GeoJSON tidak tersedia."))
    }
    
    data_with_sovi <- current_data()
    
    # Find ID column in GeoJSON
    get_geojson_id_col <- function(sf_obj) {
      names_sf <- names(st_drop_geometry(sf_obj))
      possible_ids <- c("DISTRICTCODE", "districtcode", "id", "ID", "ADM2_PCODE", "kode", "KAB_KOTA_KO", "NAME_2")
      for (id_col in possible_ids) {
        if (id_col %in% names_sf) return(id_col)
      }
      return(NULL)
    }
    
    geojson_id_col_name <- get_geojson_id_col(geojson_data)
    if (is.null(geojson_id_col_name)) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "GeoJSON tidak memiliki kolom ID yang dikenali."))
    }
    
    # Prepare data for joining
    geojson_processed <- geojson_data %>%
      mutate(DISTRICTCODE_JOIN = as.character(trimws(!!sym(geojson_id_col_name))))
    
    data_processed <- data_with_sovi
    if ("DISTRICTCODE" %in% names(data_processed)) {
      data_processed$DISTRICTCODE_JOIN <- as.character(trimws(data_processed$DISTRICTCODE))
    } else {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "Data tidak memiliki kolom DISTRICTCODE."))
    }
    
    # Join data
    data_subset <- data_processed %>% 
      select(DISTRICTCODE_JOIN, SoVI)
    
    map_data <- inner_join(geojson_processed, data_subset, by = "DISTRICTCODE_JOIN")
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 107, lat = -7, zoom = 6) %>%
               addPopups(107, -7, "Tidak ada data yang cocok antara GeoJSON dan data SoVI."))
    }
    
    # Create color palette for SoVI
    values <- map_data$SoVI
    valid_values <- values[!is.na(values)]
    
    # Use diverging color palette for SoVI (red = high vulnerability, blue = low vulnerability)
    pal <- colorBin("RdYlBu", domain = valid_values, bins = 7, reverse = TRUE, na.color = "#808080")
    
    # Create labels
    label_name <- if ("NAME_2" %in% names(map_data)) {
      map_data$NAME_2
    } else if ("Kab_Kota" %in% names(map_data)) {
      map_data$Kab_Kota
    } else {
      map_data$DISTRICTCODE_JOIN
    }
    
    label_text <- sprintf(
      "<strong>%s</strong><br/>SoVI: %s<br/>Kategori: %s",
      label_name, 
      round(values, 3),
      ifelse(values > 0, "Tinggi", "Rendah")
    )
    labels <- lapply(label_text, htmltools::HTML)
    
    # Create map
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(values),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = values, 
        opacity = 0.7, 
        title = "Indeks SoVI<br/>(Merah = Tinggi)",
        position = "bottomright"
      )
  })
  
  # Distance matrix table
  output$distance_matrix_table <- renderDT({
    if (!is.null(distance_matrix_data)) {
      datatable(distance_matrix_data[1:min(10, nrow(distance_matrix_data)), 1:min(10, ncol(distance_matrix_data))], 
                options = list(pageLength = 5, scrollX = TRUE, scrollY = "300px"))
    } else {
      datatable(data.frame(Pesan = "Data matriks jarak tidak tersedia."), options = list(dom = 't'))
    }
  })
  
  # Distance matrix summary
  output$distance_matrix_summary <- renderPrint({
    if (!is.null(distance_matrix_data)) {
      cat("📊 Ringkasan Matriks Jarak:\n\n")
      
      dist_mat_numeric <- tryCatch({
        as.matrix(distance_matrix_data[, -1])
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(dist_mat_numeric)) {
        cat("📏 Dimensi Matriks:", nrow(dist_mat_numeric), "x", ncol(dist_mat_numeric), "\n")
        cat("📈 Statistik Jarak:\n")
        
        # Remove diagonal (self-distances) for summary
        upper_tri <- dist_mat_numeric[upper.tri(dist_mat_numeric)]
        print(summary(upper_tri))
        
        cat("\n🔍 Informasi Tambahan:\n")
        cat("- Jarak minimum:", min(upper_tri, na.rm = TRUE), "\n")
        cat("- Jarak maksimum:", max(upper_tri, na.rm = TRUE), "\n")
        cat("- Rata-rata jarak:", round(mean(upper_tri, na.rm = TRUE), 2), "\n")
        
        if (isSymmetric(dist_mat_numeric)) {
          cat("✅ Matriks bersifat simetris\n")
        } else {
          cat("⚠️ Matriks tidak simetris\n")
        }
      } else {
        cat("❌ Tidak dapat memproses matriks jarak\n")
      }
    } else {
      cat("❌ Data matriks jarak tidak tersedia\n")
    }
  })
  
  # Moran's I Test
  observeEvent(input$run_moran, {
    output$moran_output <- renderPrint({
      req(input$moran_var)
      
      data <- current_data()
      selected_var <- input$moran_var
      
      if (is.null(data) || !selected_var %in% names(data)) {
        cat("❌ Variabel tidak ditemukan dalam data.")
        return()
      }
      
      if (is.null(geojson_data)) {
        cat("❌ Data GeoJSON tidak tersedia untuk analisis spasial.")
        return()
      }
      
      if (is.null(distance_matrix_data)) {
        cat("❌ Data matriks jarak tidak tersedia untuk analisis spasial.")
        return()
      }
      
      tryCatch({
        # Prepare spatial weights from distance matrix
        get_geojson_id_col <- function(sf_obj) {
          names_sf <- names(st_drop_geometry(sf_obj))
          possible_ids <- c("DISTRICTCODE", "districtcode", "id", "ID", "ADM2_PCODE", "kode", "KAB_KOTA_KO")
          for (id_col in possible_ids) {
            if (id_col %in% names_sf) return(id_col)
          }
          return(NULL)
        }
        
        geojson_id_col_name <- get_geojson_id_col(geojson_data)
        if (is.null(geojson_id_col_name)) {
          stop("GeoJSON tidak memiliki kolom ID yang dikenali untuk Moran's I.")
        }
        
        # Get common districts
        geojson_ids <- as.character(trimws(geojson_data[[geojson_id_col_name]]))
        data_ids <- as.character(trimws(data$DISTRICTCODE))
        dist_matrix_ids <- as.character(names(distance_matrix_data)[-1])
        
        common_ids <- intersect(intersect(geojson_ids, data_ids), dist_matrix_ids)
        
        if (length(common_ids) < 3) {
          stop("Tidak cukup distrik yang cocok (minimal 3) untuk analisis Moran's I.")
        }
        
        # Filter and align data
        data_filtered <- data %>%
          filter(DISTRICTCODE %in% common_ids) %>%
          arrange(DISTRICTCODE)
        
        # Extract variable values
        x <- data_filtered[[selected_var]]
        
        # Handle NA values
        if (any(is.na(x))) {
          non_na_indices <- !is.na(x)
          x <- x[non_na_indices]
          common_ids <- common_ids[non_na_indices]
          
          if (length(x) < 3) {
            stop("Terlalu banyak nilai NA untuk analisis Moran's I.")
          }
        }
        
        # Create distance matrix subset
        dist_subset <- distance_matrix_data %>%
          filter(DISTRICTCODE %in% common_ids) %>%
          select(DISTRICTCODE, all_of(common_ids)) %>%
          arrange(DISTRICTCODE)
        
        dist_matrix <- as.matrix(dist_subset[, -1])
        
        # Convert to spatial weights (inverse distance)
        diag(dist_matrix) <- 0
        w_matrix <- 1 / (dist_matrix + .Machine$double.eps)
        diag(w_matrix) <- 0
        
        # Row-standardize weights
        row_sums <- rowSums(w_matrix)
        w_matrix <- w_matrix / ifelse(row_sums == 0, 1, row_sums)
        
        # Convert to listw object
        w_list <- mat2listw(w_matrix, style = "W", zero.policy = TRUE)
        
        # Perform Moran's I test
        moran_result <- moran.test(x, w_list, zero.policy = TRUE)
        
        cat("📈 Hasil Uji Moran's I Global untuk '", selected_var, "':\n\n")
        print(moran_result)
        
        cat("\n🔍 Interpretasi:\n")
        if (moran_result$p.value < 0.05) {
          cat("✅ Terdapat autokorelasi spasial yang signifikan (p < 0.05)\n")
          if (moran_result$estimate[1] > 0) {
            cat("📊 Autokorelasi POSITIF: Nilai tinggi cenderung bertetangga dengan nilai tinggi\n")
          } else {
            cat("📊 Autokorelasi NEGATIF: Nilai tinggi cenderung bertetangga dengan nilai rendah\n")
          }
        } else {
          cat("❌ Tidak ada autokorelasi spasial yang signifikan (p >= 0.05)\n")
          cat("📊 Distribusi spasial cenderung acak\n")
        }
        
        cat("\n📏 Statistik:\n")
        cat("- Moran's I:", round(moran_result$estimate[1], 4), "\n")
        cat("- Expected I:", round(moran_result$estimate[2], 4), "\n")
        cat("- Variance:", round(moran_result$estimate[3], 6), "\n")
        cat("- Z-score:", round((moran_result$estimate[1] - moran_result$estimate[2]) / sqrt(moran_result$estimate[3]), 4), "\n")
        
      }, error = function(e) {
        cat("❌ Error dalam Uji Moran's I:", e$message, "\n")
        cat("\n🔧 Pastikan:\n")
        cat("1. DISTRICTCODE konsisten di semua dataset\n")
        cat("2. Variabel tidak memiliki terlalu banyak nilai NA\n")
        cat("3. Matriks jarak valid dan lengkap\n")
      })
    })
    
    # Moran's I plot
    output$moran_plot <- renderPlot({
      req(input$moran_var)
      
      data <- current_data()
      selected_var <- input$moran_var
      
      if (!is.null(data) && selected_var %in% names(data) && !is.null(distance_matrix_data)) {
        tryCatch({
          # Create Moran scatterplot
          x <- data[[selected_var]]
          x_clean <- x[!is.na(x)]
          
          if (length(x_clean) > 3) {
            # Simple spatial lag calculation (using mean of neighbors as approximation)
            n <- length(x_clean)
            spatial_lag <- rep(mean(x_clean, na.rm = TRUE), n)  # Simplified for visualization
            
            plot(x_clean, spatial_lag,
                 main = paste("Moran Scatterplot:", selected_var),
                 xlab = paste(selected_var, "(standardized)"),
                 ylab = "Spatial Lag",
                 col = "#2a3439",
                 pch = 16)
            
            # Add regression line
            abline(lm(spatial_lag ~ x_clean), col = "#17a2b8", lwd = 2)
            
            # Add quadrant lines
            abline(h = mean(spatial_lag), v = mean(x_clean), col = "gray", lty = 2)
          }
        }, error = function(e) {
          plot(1, 1, type = "n", main = "Error creating Moran plot", xlab = "", ylab = "")
          text(1, 1, paste("Error:", e$message), col = "red")
        })
      }
    })
  })
  
  # Hotspot Analysis
  observeEvent(input$run_hotspot, {
    output$hotspot_output <- renderPrint({
      req(input$hotspot_var)
      
      data <- current_data()
      selected_var <- input$hotspot_var
      
      if (is.null(data) || !selected_var %in% names(data)) {
        cat("❌ Variabel tidak ditemukan dalam data.")
        return()
      }
      
      if (is.null(geojson_data) || is.null(distance_matrix_data)) {
        cat("❌ Data GeoJSON atau matriks jarak tidak tersedia.")
        return()
      }
      
      tryCatch({
        # Prepare data similar to Moran's I
        get_geojson_id_col <- function(sf_obj) {
          names_sf <- names(st_drop_geometry(sf_obj))
          possible_ids <- c("DISTRICTCODE", "districtcode", "id", "ID", "ADM2_PCODE", "kode", "KAB_KOTA_KO")
          for (id_col in possible_ids) {
            if (id_col %in% names_sf) return(id_col)
          }
          return(NULL)
        }
        
        geojson_id_col_name <- get_geojson_id_col(geojson_data)
        if (is.null(geojson_id_col_name)) {
          stop("GeoJSON tidak memiliki kolom ID yang dikenali.")
        }
        
        # Get common districts
        geojson_ids <- as.character(trimws(geojson_data[[geojson_id_col_name]]))
        data_ids <- as.character(trimws(data$DISTRICTCODE))
        dist_matrix_ids <- as.character(names(distance_matrix_data)[-1])
        
        common_ids <- intersect(intersect(geojson_ids, data_ids), dist_matrix_ids)
        
        if (length(common_ids) < 3) {
          stop("Tidak cukup distrik yang cocok untuk analisis hotspot.")
        }
        
        # Filter and align data
        data_filtered <- data %>%
          filter(DISTRICTCODE %in% common_ids) %>%
          arrange(DISTRICTCODE)
        
        # Extract variable values
        x <- data_filtered[[selected_var]]
        
        # Handle NA values
        if (any(is.na(x))) {
          non_na_indices <- !is.na(x)
          x <- x[non_na_indices]
          common_ids <- common_ids[non_na_indices]
          data_filtered <- data_filtered[non_na_indices, ]
          
          if (length(x) < 3) {
            stop("Terlalu banyak nilai NA untuk analisis hotspot.")
          }
        }
        
        # Create distance matrix subset
        dist_subset <- distance_matrix_data %>%
          filter(DISTRICTCODE %in% common_ids) %>%
          select(DISTRICTCODE, all_of(common_ids)) %>%
          arrange(DISTRICTCODE)
        
        dist_matrix <- as.matrix(dist_subset[, -1])
        
        # Convert to spatial weights
        diag(dist_matrix) <- 0
        w_matrix <- 1 / (dist_matrix + .Machine$double.eps)
        diag(w_matrix) <- 0
        
        # Row-standardize weights
        row_sums <- rowSums(w_matrix)
        w_matrix <- w_matrix / ifelse(row_sums == 0, 1, row_sums)
        
        # Convert to listw object
        w_list <- mat2listw(w_matrix, style = "W", zero.policy = TRUE)
        
        # Perform Getis-Ord Gi* test
        gi_result <- localG(x, w_list, zero.policy = TRUE)
        
        # Add results to data
        data_filtered$Gi_star <- as.numeric(gi_result)
        data_filtered$Hotspot_Category <- ifelse(
          abs(data_filtered$Gi_star) > 1.96,
          ifelse(data_filtered$Gi_star > 0, "Hot Spot", "Cold Spot"),
          "Not Significant"
        )
        
        cat("🔥 Hasil Analisis Hotspot (Getis-Ord Gi*) untuk '", selected_var, "':\n\n")
        
        # Summary of hotspots
        hotspot_summary <- table(data_filtered$Hotspot_Category)
        cat("📊 Ringkasan Hotspot:\n")
        print(hotspot_summary)
        
        cat("\n🎯 Statistik Gi*:\n")
        cat("- Rata-rata Gi*:", round(mean(data_filtered$Gi_star, na.rm = TRUE), 4), "\n")
        cat("- Std. Deviasi Gi*:", round(sd(data_filtered$Gi_star, na.rm = TRUE), 4), "\n")
        cat("- Min Gi*:", round(min(data_filtered$Gi_star, na.rm = TRUE), 4), "\n")
        cat("- Max Gi*:", round(max(data_filtered$Gi_star, na.rm = TRUE), 4), "\n")
        
        cat("\n🔍 Interpretasi:\n")
        cat("- Gi* > 1.96: Hot Spot (klaster nilai tinggi, signifikan 95%)\n")
        cat("- Gi* < -1.96: Cold Spot (klaster nilai rendah, signifikan 95%)\n")
        cat("- |Gi*| <= 1.96: Tidak signifikan\n")
        
        # Store results for mapping
        hotspot_results <- data_filtered
        
        # Update reactive value for mapping
        current_data(merge(current_data(), 
                           data_filtered[c("DISTRICTCODE", "Gi_star", "Hotspot_Category")], 
                           by = "DISTRICTCODE", all.x = TRUE))
        
      }, error = function(e) {
        cat("❌ Error dalam analisis hotspot:", e$message, "\n")
      })
    })
    
    # Hotspot map
    output$hotspot_map <- renderLeaflet({
      req(input$hotspot_var)
      
      data_with_hotspot <- current_data()
      
      if (is.null(data_with_hotspot) || !"Gi_star" %in% names(data_with_hotspot) || is.null(geojson_data)) {
        return(leaflet() %>% 
                 addTiles() %>% 
                 setView(lng = 107, lat = -7, zoom = 6) %>%
                 addPopups(107, -7, "Jalankan analisis hotspot terlebih dahulu."))
      }
      
      # Find ID column in GeoJSON
      get_geojson_id_col <- function(sf_obj) {
        names_sf <- names(st_drop_geometry(sf_obj))
        possible_ids <- c("DISTRICTCODE", "districtcode", "id", "ID", "ADM2_PCODE", "kode", "KAB_KOTA_KO", "NAME_2")
        for (id_col in possible_ids) {
          if (id_col %in% names_sf) return(id_col)
        }
        return(NULL)
      }
      
      geojson_id_col_name <- get_geojson_id_col(geojson_data)
      if (is.null(geojson_id_col_name)) {
        return(leaflet() %>% 
                 addTiles() %>% 
                 setView(lng = 107, lat = -7, zoom = 6) %>%
                 addPopups(107, -7, "GeoJSON tidak memiliki kolom ID yang dikenali."))
      }
      
      # Prepare data for joining
      geojson_processed <- geojson_data %>%
        mutate(DISTRICTCODE_JOIN = as.character(trimws(!!sym(geojson_id_col_name))))
      
      data_processed <- data_with_hotspot
      if ("DISTRICTCODE" %in% names(data_processed)) {
        data_processed$DISTRICTCODE_JOIN <- as.character(trimws(data_processed$DISTRICTCODE))
      } else {
        return(leaflet() %>% 
                 addTiles() %>% 
                 setView(lng = 107, lat = -7, zoom = 6) %>%
                 addPopups(107, -7, "Data tidak memiliki kolom DISTRICTCODE."))
      }
      
      # Join data
      data_subset <- data_processed %>% 
        select(DISTRICTCODE_JOIN, Gi_star, Hotspot_Category) %>%
        filter(!is.na(Gi_star))
      
      map_data <- inner_join(geojson_processed, data_subset, by = "DISTRICTCODE_JOIN")
      
      if (nrow(map_data) == 0) {
        return(leaflet() %>% 
                 addTiles() %>% 
                 setView(lng = 107, lat = -7, zoom = 6) %>%
                 addPopups(107, -7, "Tidak ada data hotspot yang cocok."))
      }
      
      # Create color palette for hotspots
      colors <- c("Hot Spot" = "#d73027", "Cold Spot" = "#4575b4", "Not Significant" = "#ffffbf")
      pal <- colorFactor(colors, domain = map_data$Hotspot_Category)
      
      # Create labels
      label_name <- if ("NAME_2" %in% names(map_data)) {
        map_data$NAME_2
      } else if ("Kab_Kota" %in% names(map_data)) {
        map_data$Kab_Kota
      } else {
        map_data$DISTRICTCODE_JOIN
      }
      
      label_text <- sprintf(
        "<strong>%s</strong><br/>Gi* Score: %s<br/>Kategori: %s",
        label_name, 
        round(map_data$Gi_star, 3),
        map_data$Hotspot_Category
      )
      labels <- lapply(label_text, htmltools::HTML)
      
      # Create map
      leaflet(map_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(Hotspot_Category),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.8,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal, 
          values = ~Hotspot_Category, 
          opacity = 0.7, 
          title = "Hotspot Analysis",
          position = "bottomright"
        )
    })
  })
  
  # New reactive outputs for the reorganized menu structure
  
  # Homogenitas test button
  observeEvent(input$run_homogen_test, {
    req(input$var_homogen_group)
    
    output$uji_var <- renderText({
      data <- current_data()
      if (!is.null(data) && input$var_homogen_group %in% names(data)) {
        tryCatch({
          # Get all numeric variables
          numeric_vars <- names(data)[sapply(data, is.numeric)]
          results <- character(0)
          
          for (var in numeric_vars) {
            if (var != input$var_homogen_group) {
              # Create formula for Levene test
              formula_str <- paste(var, "~", input$var_homogen_group)
              test_result <- car::leveneTest(as.formula(formula_str), data = data)
              
              result_text <- paste0(
                "=== UJI HOMOGENITAS: ", var, " ===\n",
                "Levene's Test for Homogeneity of Variance\n",
                "F-statistic: ", round(test_result$`F value`[1], 4), "\n",
                "p-value: ", round(test_result$`Pr(>F)`[1], 6), "\n",
                if (test_result$`Pr(>F)`[1] < 0.05) {
                  "Kesimpulan: Varians TIDAK homogen (p < 0.05)"
                } else {
                  "Kesimpulan: Varians homogen (p >= 0.05)"
                },
                "\n\n"
              )
              results <- c(results, result_text)
            }
          }
          paste(results, collapse = "")
        }, error = function(e) {
          paste("Error dalam uji homogenitas:", e$message)
        })
      } else {
        "Pilih variabel grup untuk uji homogenitas"
      }
    })
  })
  
  # Spatial statistics for exploration
  observeEvent(input$run_spatial_stats, {
    req(input$spatial_var_ekspl)
    
    output$spatial_stats_output <- renderText({
      data <- current_data()
      if (!is.null(data) && input$spatial_var_ekspl %in% names(data)) {
        tryCatch({
          selected_var <- data[[input$spatial_var_ekspl]]
          
          # Basic spatial statistics
          result_text <- paste0(
            "=== STATISTIK SPASIAL DASAR ===\n",
            "Variabel: ", input$spatial_var_ekspl, "\n\n",
            "Statistik Deskriptif:\n",
            "Mean: ", round(mean(selected_var, na.rm = TRUE), 4), "\n",
            "Median: ", round(median(selected_var, na.rm = TRUE), 4), "\n",
            "Std Dev: ", round(sd(selected_var, na.rm = TRUE), 4), "\n",
            "Min: ", round(min(selected_var, na.rm = TRUE), 4), "\n",
            "Max: ", round(max(selected_var, na.rm = TRUE), 4), "\n\n",
            "Analisis Spasial:\n",
            "Jumlah observasi: ", length(selected_var[!is.na(selected_var)]), "\n",
            "Missing values: ", sum(is.na(selected_var)), "\n",
            "Variability: ", round(sd(selected_var, na.rm = TRUE) / mean(selected_var, na.rm = TRUE), 4), "\n\n",
            "Catatan: Untuk analisis autokorelasi spasial lengkap,\n",
            "gunakan menu Analisis Spasial utama."
          )
          
          result_text
        }, error = function(e) {
          paste("Error dalam analisis spasial:", e$message)
        })
      } else {
        "Pilih variabel untuk analisis spasial"
      }
    })
    
    # Simple spatial autocorrelation plot
    output$spatial_autocorr_plot <- renderPlot({
      data <- current_data()
      if (!is.null(data) && input$spatial_var_ekspl %in% names(data)) {
        tryCatch({
          selected_var <- data[[input$spatial_var_ekspl]]
          
          # Create a simple lag plot for spatial exploration
          n <- length(selected_var)
          if (n > 1) {
            lag_var <- c(NA, selected_var[-n])  # Simple lag-1
            
            plot_data <- data.frame(
              original = selected_var,
              lagged = lag_var
            )
            
            ggplot(plot_data, aes(x = lagged, y = original)) +
              geom_point(color = "#2a3439", alpha = 0.6) +
              geom_smooth(method = "lm", color = "#4a5c66", se = TRUE) +
              ggtitle(paste("Lag Plot:", input$spatial_var_ekspl)) +
              xlab("Lagged Values") +
              ylab("Original Values") +
              theme_minimal() +
              theme(
                plot.title = element_text(color = "#2a3439", size = 14, face = "bold"),
                axis.title = element_text(color = "#2a3439"),
                axis.text = element_text(color = "#3d4a52")
              )
          }
        }, error = function(e) {
          # Return empty plot on error
          ggplot() + theme_void()
        })
      }
    })
  })
}

# 10. Run the App
shinyApp(ui, server)
