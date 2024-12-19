# direct reading from terra mixes spectral bands and time. 
#install.packages("terra")
#library(terra)
#a=rast("Downloads/210_Sentinel2_to_XarrayDS_to_Rectangles_1000m/092_Features_100px_NC_Store/Feature_01523gc_0087lc_20170412T093031_Fire_1.10m_100px_nc")

#install.packages("ncdf4")
#library(ncdf4)
library(sf)
setwd("~/Documents/work/sentinel2dimexp/")
fire=nc_open("210_Sentinel2_to_XarrayDS_to_Rectangles_1000m/092_Features_100px_NC_Store/Feature_01523gc_0087lc_20170412T093031_Fire_1.10m_100px_nc")
meta=read_sf("210_Sentinel2_to_XarrayDS_to_Rectangles_1000m/20241216_115810.TPRQ32_MCPC_Snapped_1000m_rectangles_NetCFD__ProcStat_GPD.010_Features.SAMPLE.GPKG") 
  meta[2,]$Notes 
a1=nc_open("210_Sentinel2_to_XarrayDS_to_Rectangles_1000m/092_Features_100px_NC_Store/Feature_01935gc_0096lc_20171118T093251_Tillage_3.10m_100px_nc")

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
?as.POSIXlt
every=4
pdf(file=paste0("tillageb_ndvi_every",every,".pdf"))
plot(subset(r, seq(1+every*0,nlyr(r),by=every)))
plot(subset(r, seq(1+every*16,nlyr(r),by=every)))
plot(subset(r, seq(1+every*32,nlyr(r),by=every)))
plot(subset(r, seq(1+every*64,nlyr(r),by=every)))
dev.off() 
 
