# 🚀 PANDUAN DEPLOYMENT DASHBOARD

## 📋 CHECKLIST SEBELUM DEPLOYMENT

### ✅ **FILE REQUIREMENTS**
- [x] `vulnera_dashboard_fixed.R` - Main dashboard file
- [x] `run_dashboard.R` - Runner script  
- [x] `test_dashboard.R` - Testing script
- [x] `indonesia_kabkota.geojson` - GeoJSON file (optional, akan didownload otomatis)
- [x] Documentation files (README, guides)

### ✅ **R PACKAGES REQUIREMENTS**
```r
required_packages <- c(
  "shiny",           # Web framework
  "shinydashboard",  # Dashboard layout
  "DT",             # Interactive tables
  "ggplot2",        # Plotting
  "dplyr",          # Data manipulation
  "readr",          # Data reading
  "leaflet",        # Interactive maps
  "sf",             # Spatial data
  "jsonlite",       # JSON handling
  "spdep",          # Spatial dependencies
  "htmltools",      # HTML utilities
  "plotly",         # Interactive plots
  "corrplot",       # Correlation plots
  "RColorBrewer",   # Color palettes
  "viridis",        # Color schemes
  "shinyjs",        # JavaScript integration
  "shinycssloaders" # Loading animations
)
```

## 🖥️ DEPLOYMENT OPTIONS

### **OPTION 1: LOCAL DEPLOYMENT** 💻

#### **Step 1: Install R and RStudio**
```bash
# Ubuntu/Debian
sudo apt update
sudo apt install r-base r-base-dev

# Windows: Download from https://cran.r-project.org/
# macOS: Download from https://cran.r-project.org/
```

#### **Step 2: Install Required Packages**
```r
# Method 1: Using runner script (Recommended)
source("run_dashboard.R")

# Method 2: Manual installation
required_packages <- c("shiny", "shinydashboard", "DT", "ggplot2", 
                      "dplyr", "readr", "leaflet", "sf", "plotly", 
                      "corrplot", "viridis", "shinyjs")
install.packages(required_packages, dependencies = TRUE)
```

#### **Step 3: Run Dashboard**
```r
# Option A: Quick run
shiny::runApp("vulnera_dashboard_fixed.R")

# Option B: With host/port specification
shiny::runApp("vulnera_dashboard_fixed.R", host = "0.0.0.0", port = 3838)

# Option C: Using runner script
source("run_dashboard.R")
```

### **OPTION 2: SHINY SERVER DEPLOYMENT** 🌐

#### **Step 1: Install Shiny Server**
```bash
# Ubuntu/Debian
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.18.987-amd64.deb
sudo gdebi shiny-server-1.5.18.987-amd64.deb
```

#### **Step 2: Configure Shiny Server**
```bash
# Edit configuration
sudo nano /etc/shiny-server/shiny-server.conf

# Basic configuration:
server {
  listen 3838;
  location / {
    site_dir /srv/shiny-server;
    log_dir /var/log/shiny-server;
    directory_index on;
  }
}
```

#### **Step 3: Deploy Dashboard**
```bash
# Create app directory
sudo mkdir -p /srv/shiny-server/vulnera-dashboard

# Copy files
sudo cp vulnera_dashboard_fixed.R /srv/shiny-server/vulnera-dashboard/app.R
sudo cp *.geojson /srv/shiny-server/vulnera-dashboard/ 2>/dev/null || true

# Set permissions
sudo chown -R shiny:shiny /srv/shiny-server/vulnera-dashboard
sudo chmod -R 755 /srv/shiny-server/vulnera-dashboard

# Restart service
sudo systemctl restart shiny-server
```

#### **Step 4: Access Dashboard**
```
URL: http://your-server-ip:3838/vulnera-dashboard/
```

### **OPTION 3: SHINYAPPS.IO DEPLOYMENT** ☁️

#### **Step 1: Setup Account**
```r
# Install rsconnect
install.packages("rsconnect")

# Configure account (get token from shinyapps.io)
rsconnect::setAccountInfo(
  name = "your-account",
  token = "your-token",
  secret = "your-secret"
)
```

#### **Step 2: Deploy**
```r
# Deploy dashboard
rsconnect::deployApp(
  appFiles = c("vulnera_dashboard_fixed.R"),
  appName = "vulnera-dashboard",
  account = "your-account"
)
```

### **OPTION 4: DOCKER DEPLOYMENT** 🐳

#### **Step 1: Create Dockerfile**
```dockerfile
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'ggplot2', \
    'dplyr', 'readr', 'leaflet', 'sf', 'plotly', 'corrplot', 'viridis', \
    'shinyjs', 'shinycssloaders'), dependencies = TRUE)"

# Copy app files
COPY vulnera_dashboard_fixed.R /srv/shiny-server/app.R
COPY *.geojson /srv/shiny-server/ 2>/dev/null || true

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port
EXPOSE 3838

# Run app
CMD ["/usr/bin/shiny-server"]
```

#### **Step 2: Build and Run**
```bash
# Build image
docker build -t vulnera-dashboard .

# Run container
docker run -d -p 3838:3838 vulnera-dashboard

# Access at http://localhost:3838
```

## 🔧 CONFIGURATION OPTIONS

### **Performance Tuning**
```r
# In app.R, add these options at the top:
options(shiny.maxRequestSize = 100*1024^2)  # 100MB upload limit
options(shiny.usecairo = FALSE)             # Disable Cairo for better performance
```

### **Security Settings**
```bash
# For production deployment, configure firewall
sudo ufw allow 3838/tcp

# Use HTTPS with SSL certificate
# Configure nginx as reverse proxy for SSL termination
```

### **Monitoring**
```bash
# Check Shiny Server logs
sudo tail -f /var/log/shiny-server.log

# Monitor resource usage
htop
```

## 🚨 TROUBLESHOOTING

### **Common Issues & Solutions**

#### **Package Installation Errors**
```r
# If sf package fails to install
sudo apt-get install libgdal-dev libudunits2-dev

# If curl/httr fails
sudo apt-get install libcurl4-openssl-dev libssl-dev
```

#### **Memory Issues**
```r
# Increase memory limits
options(java.parameters = "-Xmx4g")  # For rJava
```

#### **Port Issues**
```bash
# Check if port is in use
sudo netstat -tlnp | grep :3838

# Kill process using port
sudo kill -9 <PID>
```

#### **Permission Issues**
```bash
# Fix file permissions
sudo chown -R shiny:shiny /srv/shiny-server/
sudo chmod -R 755 /srv/shiny-server/
```

## 📊 MONITORING & MAINTENANCE

### **Health Checks**
```bash
# Check if dashboard is running
curl -f http://localhost:3838/vulnera-dashboard/ || echo "Dashboard down"

# Monitor resource usage
ps aux | grep shiny
```

### **Log Management**
```bash
# Rotate logs
sudo logrotate /etc/logrotate.d/shiny-server

# Archive old logs
sudo find /var/log/shiny-server -name "*.log" -mtime +30 -delete
```

### **Updates**
```r
# Update R packages
update.packages(ask = FALSE)

# Update dashboard code
# Replace files and restart service
sudo systemctl restart shiny-server
```

## 🎯 PRODUCTION CHECKLIST

### **Before Going Live** ✅
- [ ] Test all dashboard features
- [ ] Verify data connections
- [ ] Check error handling
- [ ] Test on different browsers
- [ ] Verify mobile responsiveness
- [ ] Setup monitoring
- [ ] Configure backups
- [ ] Setup SSL certificate
- [ ] Document access URLs
- [ ] Train end users

### **Security Checklist** 🔒
- [ ] Enable firewall
- [ ] Use HTTPS
- [ ] Implement authentication (if needed)
- [ ] Regular security updates
- [ ] Monitor access logs
- [ ] Backup configuration files

## 📞 SUPPORT

### **Getting Help**
- Check logs: `/var/log/shiny-server/`
- Test locally first: `source("test_dashboard.R")`
- Verify packages: `sessionInfo()`
- Check documentation: `README_Dashboard.md`

### **Common URLs**
- Local: `http://localhost:3838`
- Server: `http://your-ip:3838/vulnera-dashboard/`
- Logs: `/var/log/shiny-server/vulnera-dashboard-*.log`

---

**Dashboard siap untuk production deployment! 🚀**

*Pilih deployment option yang sesuai dengan kebutuhan Anda dan ikuti langkah-langkah di atas.*
