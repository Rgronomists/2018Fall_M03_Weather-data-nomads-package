library(rNOMADS)
library(stringr)

#Getting temperature for Iowa, USA,
#Takes data from latest model run for the ground surface from the GrADS-DODS server.
#Using the GFS 0.5 model
urls.out <- GetDODSDates(abbrev = "gfs_0p50")
#Get most recent model date
model.url <- tail(urls.out$url, 1)
#Get most recent model run
model.runs <- GetDODSModelRuns(model.url)
model.run <- tail(model.runs$model.run, 1)
#Get ground temperature for the 6 hour prediction
variables <- c("tmp2m","prmslmsl") #temp at 2 m, prs
time <- c(2,2) #6 hour prediction
lon.dom <- seq(0, 360, by = 0.5) #domain of longitudes in model
lat.dom <- seq(-90, 90, by = 0.5) #domain of latitudes in model
lon <- which((lon.dom >= 360 - 98) & (lon.dom <= 360 - 88)) - 1 #NOMADS indexes start at 0
lat <- which((lat.dom <= 45) & (lat.dom >= 39)) - 1 #NOMADS indexes start at 0
model.data.surface <- DODSGrab(model.url, model.run, variables, time, c(min(lon), max(lon)),c(min(lat), max(lat)))

#Make results into arrays
model.array.surface <- ModelGrid(model.data.surface, c(0.5, 0.5))

#Make a contour plot of the temperature around Iowa, USA:
contour(x = model.array.surface$x - 360, y = model.array.surface$y,
        (model.array.surface$z[1,1,,] - 273.15) * 9/5 + 32, xlab = "Longitude", ylab = "Latitude",
        main = paste("Iowa Surface Temperatures for",
                     model.array.surface$fcst.date, "UTC in F"))

contour(x = model.array.surface$x - 360, y = model.array.surface$y,
        model.array.surface$z[1,2,,] / 100 / 33.8639, xlab = "Longitude", ylab = "Latitude",
        main = paste("Iowa Surface Pressure for",
                     model.array.surface$fcst.date, "UTC in inHg"))


#An example for the Global Forecast System
# using GRIB
setwd("/Users/Kati/Documents/Togliatti_PhD/R/GFS/09-01-18")

abbrev <- "gfs-avn-hi"
model.date <- "20180901"
model.run <- "00"
filetype <- ".grb2"

#get 1 day of forecast from gfs to use as BC for WRF model runs
for (pred in seq(000,024, by=3)){
  ArchiveGribGrab(abbrev, model.date, model.run, pred, local.dir = "/Users/Kati/Documents/Togliatti_PhD/R/GFS/09-01-18", file.type = "grib2")
}

#however you can use those files to look at the temperature prediction for Ames, IA for the next 24 hours
model.data.read.1 <- ReadGrib("20180901_0000_000.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.2 <- ReadGrib("20180901_0000_003.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.3 <- ReadGrib("20180901_0000_006.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.4 <- ReadGrib("20180901_0000_009.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.5 <- ReadGrib("20180901_0000_012.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.6 <- ReadGrib("20180901_0000_015.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.7 <- ReadGrib("20180901_0000_018.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.8 <- ReadGrib("20180901_0000_021.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")
model.data.read.9 <- ReadGrib("20180901_0000_024.grb2", c("2 m above ground"), c("TMP"), domain =c(-96, -90, 44, 40), domain.type="latlon")

model.array.surface.1 <- ModelGrid(model.data.read.1, c(1.0, 1.0))
model.array.surface.2 <- ModelGrid(model.data.read.2, c(1.0, 1.0))
model.array.surface.3 <- ModelGrid(model.data.read.3, c(1.0, 1.0))
model.array.surface.4 <- ModelGrid(model.data.read.4, c(1.0, 1.0))
model.array.surface.5 <- ModelGrid(model.data.read.5, c(1.0, 1.0))
model.array.surface.6 <- ModelGrid(model.data.read.6, c(1.0, 1.0))
model.array.surface.7 <- ModelGrid(model.data.read.7, c(1.0, 1.0))
model.array.surface.8 <- ModelGrid(model.data.read.8, c(1.0, 1.0))
model.array.surface.9 <- ModelGrid(model.data.read.9, c(1.0, 1.0))

contour(x = model.array.surface.1$x, y = model.array.surface.1$y,
        (model.array.surface.1$z[1,1,,] - 273.15) * 9/5 + 32, xlab = "Longitude", ylab = "Latitude",
        main = paste("Iowa Temperature",
                     model.array.surface$fcst.date, "in F"))







