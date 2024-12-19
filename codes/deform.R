#install.packages("deform")
#install.packages("gstat")

#library(deform)
library(gstat)
library(sp)
#get data
setwd("Documents/work/sentinel2dimexp/")
a1=nc_open("Downloads/210_Sentinel2_to_XarrayDS_to_Rectangles_1000m/092_Features_100px_NC_Store/Feature_01523gc_0087lc_20170412T093031_Fire_1.10m_100px_nc")
a1 
a1=nc_open("~/Downloads/210_Sentinel2_to_XarrayDS_to_Rectangles_1000m/092_Features_100px_NC_Store/Feature_01935gc_0096lc_20171118T093251_Tillage_3.10m_100px_nc")
a1
B08 <- ncvar_get(a1, "B08") #read a band
B04 <- ncvar_get(a1, "B04") #read a band
NDVI=(B08-B04)/(B08+B04)
x <- ncvar_get(a1, "x")
y <- ncvar_get(a1, "y")
timesenti <- ncvar_get(a1, "time")
r=rast(NDVI, extent=ext(min(x), max(x), min(y), max(y)), crs="+init=epsg:32632")

#names(r)=timesenti
timesenti_readable=as.POSIXlt(timesenti,origin="2017-01-02 09:34:02")
names(r)=timesenti_readable
 
# for some reason the for loop does not work for storing variograms in png or pdf. will use r markdown
# if i write it into a function the pdf does not work.
#write_variogram = function(layernr){
  # Subset the raster layer
  rn <- subset(r, layernr)
  
  # Convert raster to a data frame
  points_df <- as.data.frame(rn, xy = TRUE, na.rm = TRUE)
  
  # Check if there is valid data
  if (nrow(points_df) == 0) {
    cat("Skipping layer", layernr, "due to no data.\n")
    next
  }
  
  # Rename the value column
  names(points_df)[3] <- "timestep"
  
  # Convert to SpatialPointsDataFrame
  coordinates(points_df) <- ~ x + y
  
  # Print diagnostic information
  cat("Processing layer:", layernr, "\n")
  #print(head(points_df))  # Verify extracted data
  
  # Compute the variogram and verify it
  vg <- tryCatch({
    gstat::variogram(timestep ~ 1, points_df)
  }, error = function(e) {
    cat("Error computing variogram for layer", layernr, ":", e$message, "\n")
    NULL
  })
  
  # Skip if variogram computation failed
  if (is.null(vg)) {
    next
  }
  
  # Safely open the PDF device
  
  #pngname = paste0("variogram", layernr, ".png")
  #cat("Saving plot to:", pngname, "\n")
  #png(pngname)
  pdf_file <- paste0("variogram", layernr, ".pdf")
  pdf(file = pdf_file)
  # Use on.exit to ensure dev.off() is always called
  #on.exit(dev.off(), add = TRUE)
  # Plot the variogram
  tryCatch({
    plot(vg, main = paste0("Variogram of timestep ", layernr))
  }, error = function(e) {
    cat("Error plotting variogram for layer", layernr, ":", e$message, "\n")
  })

  dev.off()
  cat("Finished layer:", layernr, "\n")
#} 
#write_variogram(29)
#lapply(1:10, write_variogram)

  sample_points <- spatSample(r, size = 100, method = "random", na.rm = FALSE, xy = TRUE, cells=T)
  
  # Print the sampled points
  print(head(sample_points))
  coords <- sample_points[1, c("x", "y")]
  
  # Extract the time series (values across all layers) at this location
  time_series <- terra::extract(r, coords)
  as.numeric(time_series)
  time_steps <-  timesenti_readable
  
  # Combine extracted time series with time steps
  time_series_df <- data.frame(Date = time_steps, Value = as.numeric(time_series[1, -1]))
  plot(time_series_df, typ="l")
  #options(device = "X11")
 # dev.off()
  
 
  # Test a plot
  plot(1:10, main = "Test Plot")