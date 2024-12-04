## PART A: PROJECT SETUP 

### Step 1: Install Packages 

It is important to note that when working with the R programming language, there is a large swath of built-in functions available that can be used to accomplish tasks. Though, sometimes it is necessary to install external packages that possess specific commands to accomplish specialized tasks. 

The command to install an external package is install.packages( ). All packages required for installation can be placed in the brackets separated by a comma and quoted as seen below. 

Now that we understand how to install a package. Let’s go ahead and install them as so:

```{r}
install.packages(c('tmap', 
                   'spdep',
                   'raster',
                   'sf',
                   'lubridate',
                   'dplyr',
                   'gstat',
                   'ggplot2',
                   'maps',
                   'spgwr',
                   'tidyverse',
                   'bcmaps', 
                   'spgwr', 
                   'ggspatial', 
                   'e1071', 
                   'gridExtra', 
                   'gt', 
                   'spatstat', 
                   'knitr'))
```
### Step 2: Load in Libraries 

After we have installed our external packages into R, we cannot use them quite yet. There is one additional step we have to do. We need to set the packages to our library. Each package installed in Step 1 contains a variety of different commands associated with them. Any attempt to run those commands without loading the installed packages into the library will result in an error message.

To load the packages into the RStudio we use the command library( ). Each package needs to be integrated separately as seen below. 


Now that we understand how to load a package into the library, load all packages installed in Step 1 into your library as so: 

```{r}
library(tmap)
library(spdep)
library(raster)
library(sf)
library(lubridate)
library(dplyr)
library(gstat)
library(ggplot2)
library(maps)
library(spgwr)
library(tidyr)
library(bcmaps)
library(spgwr)
library(ggspatial)
library(e1071)
library(tibble)
library(gridExtra)
library(gt)
library(spatstat)
library(knitr)
```
### Step 3: Set Working Directory 

One of the most important elements in setting up a project using R is file maintenance. R can call upon files saved to your computer. This presents the opportunity for you to call upon, a .csv file for example, and bring it into R Studio to conduct data analysis. In order to do so, we must set a working directory. The working directory is where within your file manager that R Studio will reach to call upon a file. For the duration of this project, all files related to this project should be saved to one particular folder. 

To accomplish this important step, you want to copy the path that leads to your desired folder.

Next, set that file path to a variable. In this tutorial we use the variable ‘dir’. 

Next, use the command setwd( ) with the file path variable in the brackets. This will solidify that folder  as your working directory.

This can be accomplished as so: 

```{r}
dir <- "C:/Users/jstra/OneDrive/Documents/GEOG418_Assignment_4"
setwd(dir)
```
## PART B: Independent Data Collection & Management 

As discussed earlier, we will be performing a regression analysis to determine the degree to which precipitation is able to explain the variability of occurrences in forest fires. Note the two variables at play here. Through the spatial understanding of one variable (precipitation) we are attempting to explain its influence in the occurrence of another variable (forest fires). Therefore, precipitation is our independent variable, and forest fires is our dependent variable. Moving forward, before regression analysis can be accomplished, we need to work towards bringing in and cleaning a precipitation variable dataset followed by bringing in and cleaning a forest fire variable set. Once complete we will then work to merge these two datasets together in order to perform a regression analysis. 

Before doing so, lets first focus on bringing in our independent variable dataset, precipitation. To do so we will be using data acquired from the Pacific Climate Impacts Consortium (PCIC). PCIC manages a variety of different meteorological datasets that are made available from different Ministries apart of the BC government, municipal governments within the province and private organizations. Together, this combination of data sources forms the BC Meteorological Station Data which is then managed by PCIC in order to maintain the Provincial Climate Data Set (https://www.pacificclimate.org/data/bc-station-data). For the purposes of this project, we will be examining the data that has been  provided by the BC Ministry of Forests – Wildfire Service Network.
The BC Ministry of Forest – Wildfire Service Network has 1030 meteorological stations stationed in British Columbia (https://services.pacificclimate.org/met-data-portal-pcds/app/). We will be performing an investigation into precipitation data that spans the dates from November 01, 2021 – March 31, 2022. When controlling for this time frame, 175 stations are made available with this data.  We will be using these 175 stations to understand the behaviour of our independent variable.  

Listed as FLNRO – WMB, the BC Ministry of Forest - Wildfire Service Network, conducts hourly observations on a variety of different meteorological features (https://www.pacificclimate.org/data/bc-station-data). For the purpose of this project, we will be focusing on the ‘Precipitation Amount’ recorded at each station. Data collected for this dataset is incorporated into the PCIC database on a real-time basis. Therefore, data pertaining to each date under observation was collected on that particular date.  

Let’s begin the process of collecting managing our independent data. 

### Step 1: Empty Template Formation 

We are now  beginning the process of understanding how our independent variable performs spatially. In order to do so, we need to understand what is happening where. It is our goal during Part B to create a .csv file that contains two important variables. 

The first variable, explains what is happening at each of the 175 stations we will be conducting our analysis on. That is, it is our responsibility to produce a single value at each station that is representative of how that station behaves during our desired timeframe. The single value we will be generating for our tutorial is the mean daily amount of precipitation, in other words, on average, how much precipitation is recorded at a particular station everyday. This single value will give us an understanding of what is the stations relationship to precipitation. 

The second variable locates where the station is in the province. This will take the form of a coordinate.

The creation of .csv file containing this important information will assist us further in performing spatial analysis on this data.

The first step though, before all of this, will be creating an empty template constructed as adata frame. We use the variable name empty_template_df. 

To complete this first step, we will use the data.frame() command as seen below: 

```{r}
empty_template_df <- data.frame(Native.ID = character(), 
                                PRECIP = numeric(), 
                                Longitude = numeric(), 
                                Latitude = numeric(), 
                                stringsAsFactors = FALSE)
```
### Step 2: Empty Template .CSV Creation 

With the formation of a data frame created in Step 1, it is important that we convert the data frame into a .csv file for proper data management and ease with data extraction. This tutorial uses the  variable empty_template_csv for the .csv file 

This can be done so with the write.csv() command as seen below: 

```{r}
empty_template_csv <- 'empty_template.csv'

write.csv(empty_template_df, 
          file = empty_template_csv, 
          row.names = FALSE)
```
### Step 3: Independent Data Extaction 

Given we are going to be examining the occurrence of forest fires during the fire season (April – October) and we are interested in understanding the role winter precipitation plays in that natural phenomenon, we will need to extract precipitation data that falls exactly opposite of the fire season. For that, we are interested in the acquisition of precipitation data that falls between the dates of November 01, 2021 to March 31, 2022.  

As discussed, the acquisition of precipitation data will be obtained through the extraction of data recorded at 175 meteorological stations in British Columbia. This takes the form of 175 separate .csv files. Each .csv file is associated with one of the 175 stations. The .csv files contains meteorological data and the time of recording. In order to understand what the mean daily amount of precipitation is at each station, we will conduct a for loop. The for loop will have the ability to call upon each separate .csv file, calculate the total amount of precipitation recorded on each date between November 01, 2021 and March 31, 2022 and then calculate the mean daily precipitation at that station. 

It should be noted, after the first execution of a for loop, four stations were revealing an average daily precipitation of 0 mm. This is incredibly unlikely given the geography of the region, so it is believed that the .csv files associated with those stations are in error. For that, it is in our best interest to remove these stations for the duration of our data analysis. The stations removed were stations 119, 267, 321 and 408. With that we are now conducting analysis on 171 stations. 

The output of this for loop will be the creation of .csv file that will hold the station number (listed as Native. ID) and the daily average at that station. This tutorial names the .csv file station_data. Information pertaining to Latitude and Longitude will be missing but will be included in the following step. 

The for loop can be conducted as seen below: 

```{r}
station_file_path <- 'C:/Users/jstra/OneDrive/Documents/GEOG418_Assignment_4/pcds_data/FLNRO-WMB'
station_file_list <- list.files(station_file_path, full.names = TRUE)

station_daily_avg_data <- list()


for (empty_template_csv in station_file_list)
  
{
  
  clima_db <- read.csv(empty_template_csv, skip = 1,  header = TRUE)
  clima_db$precipitation <- as.numeric(clima_db$precipitation)
  clima_db <- clima_db[!is.na(clima_db$precipitation), ]
  clima_db$time <- as.POSIXct(clima_db$time, format = '%Y-%m-%d %H:%M:%S')
  clima_db$date <- as.Date(clima_db$time)
  
  clima_db <- clima_db |> 
              filter(date >= as.Date('2021-11-01') & date <= as.Date('2022-03-31'))
  
  daily_avg_precip_data <- clima_db |>
                           group_by(date) |> 
                           summarize(daily_avg_precip = sum(precipitation, na.rm = TRUE))
  
  station_daily_avg <- round(mean(daily_avg_precip_data$daily_avg_precip, na.rm = TRUE), 4)
  
  station_file <- basename(empty_template_csv)
  station_daily_avg_data[[station_file]] <- station_daily_avg
  
  print(paste('File:', station_file, 'Station Daily Average', station_daily_avg))
  

station_daily_avg_df <- data.frame(File_Name = names(station_daily_avg_data), 
                                   station_daily_avg = unlist(station_daily_avg_data))

station_file <- basename(station_daily_avg_df$File_Name)
station_file_clean <- sub('.csv$', "", station_file)

clean_station_daily_avg_data <-(station_daily_avg_df$station_daily_avg)

new_values<- data.frame(Native.ID = station_file_clean, 
                        PRECIP = clean_station_daily_avg_data, 
                        stringsAsFactors = FALSE)

station_data <- bind_rows(empty_template_df, new_values)

output_file_path <- 'Station_Data.csv'
write.csv(station_data, file = output_file_path, row.names= FALSE)

}
```
### Step 4: Merging Independent Data with Location 

Now that we understand what the daily average precipitation is at each of our stations, it is important to understand where they are located. Unfortunately, the station .csv file does not contain the coordinates of where the station is located. The only identifying information is the file name which is representative of the station number. Though, during the acquisition of our data, the inclusion of metadata is provided which includes the coordinates of each station. Our next task is to tie that coordinate information to the station points, so we understand where the precipitation is being recorded.

First assign the metadata .csv file to a variable. This tutorial names the metadata file ‘metadata’.

Next, assign the .csv file created in Step 3 (station_data) to a variable. This tutorial names the .csv file ‘stationdata’.

Using the merge( ) command we can combine these two data sets and using the write.csv ( ) to create a brand new .csv file which will now hold our station precipitation data and where the station is located. This tutorial names the new .csv file ‘ClimateData.csv’. With this important step accomplished, we can now begin the process of interpreting our results. 

Before doing so, go ahead and merge the two .csv files discussed above as so: 

```{r}
metadata <-read.csv('station-metadata-by-history.csv')
stationdata <- read.csv('Station_Data.csv')

merged_data<-merge(metadata, 
                   stationdata, 
                   by = 'Native.ID')|>
                   select(- Longitude.y, - Latitude.y)|>
                   rename(Latitude = Latitude.x, Longitude = Longitude.x)|>
                   drop_na()
  
write.csv(merged_data,
          file ='ClimateData.csv', 
          row.names= FALSE)
```
### Step 5: Mapping Meteorological Station Points 

Now that the mean daily precipitation for each station is tied to a location, we can map where in British Columbia these stations are located. Using the merged .csv file that was created in the last step we are going to convert that .csv file into an sf. A sf  or simple feature is an object in R that standardizes spatial vector data allowing the values associated with coordinates to be treated as such and are then able to be positioned on a mapping projection (https://cran.r-project.org/web/packages/sf/index.html). To then save this new sf to file, we are going to use the st_write command allowing the creation of a shapefile. This format conversion enables the creation of a map using ggplot and using the package ‘bcmaps’ for a template of the province. 

Mapping the station points can be done as so:

```{r}

climate_data <- read.csv('ClimateData.csv')

climate_sf <- st_as_sf(climate_data, 
                       coords = c('Longitude', 'Latitude'), 
                       crs = 4326)

climate_sf <- st_transform(climate_sf, 3005)

st_write(climate_sf, 
         'ClimateData.shp', 
         append = FALSE)

climate_sf<- st_read('ClimateData.shp')
bc_boundary <- bcmaps::bc_bound()

ggplot() +
  geom_sf(data = bc_boundary, fill = "grey", color = "black") +
  geom_sf(data = climate_sf, color = "red", size = 2) +
  labs(title = "Locations of Meteorological Stations used for
           Independent Data Collection", 
       x = "Longitude", 
       y = "Latitude") +
  annotation_north_arrow(location = 'tr', 
                         which_north = 'true', 
                         width = unit(1, 'cm'), 
                         height = unit(1, 'cm')) +
  theme_minimal()
```
![stationmap](https://github.com/user-attachments/assets/88f53929-b662-4b51-98de-335a4224ec84)

The generation of a map reveals that there is a good spread of stations across the entirety of province with the North sparse in stations compared to the southern portion of the province. It should also be noted that the island of Hadia Gwaii has no station points.  

## PART C: Spatial Interpolation 

We now have a map with the locations of our meteorological stations. We also understand the average daily precipitation at each station. With these two variables we now have the opportunity to interpolate the entirety of the surface in  British Columbia. This prompts the opportunity to interpret the daily mean precipitation at any location in the province. This process is known as spatial interpolation. Spatial interpolation predicts the value of a variable at an unmeasured location based on variables at known locations (Bolstad, 2019). It is commonplace for a variable at an unmeasured location to use the nearest variable at a known location to generate an estimated value (Bolstad, 2019).  In order to accomplish this using or converting data into a raster format enables ease when conducting spatial interpolation (Bernhardsen, 2002). In our case, a cell will have a known mean daily precipitation value, and the surrounding cells will be empty in need of an interpolated values. The empty cells will use the measure of euclidean distance to locate the nearest cell in order to generate a value to populate the empty cells (Bolstad, 2019).

For the purpose of this tutorial, we will be conducting the form of spatial interpolation known as Inverse Distance Weight.

### Step 1: Inverse Distance Weight

One method of spatial interpolation is Inverse Distance Weight (IDW). As discussed, a value is estimated at an unknown point using the value of a known point. The implementation of this form of spatial interpolation dictates that the user-decided weight of a known point has an inverse proportion to the distance (Bolstad, 2019). Meaning that as a sample point increases in distance, the influence that sample point has on the unknown value decreases. The higher the weight of the sample point, the level of influence on farther unknown values diminishes at a quicker rate (Bolstad, 2019). This tells us then, that if we are interested in understanding the occurrences at a local level, we would use a higher weight as we are only interested in the values surrounding the sample. Whereas a study area with a larger plain would rely upon a smaller weight for a reliable estimate as the level of influence on farther unknown values diminishes at a slower rate. An important take away is that the closer the unknown value is to the sample point, the samples point influence increases. 

As done with other spatial analysis techniques, a grid is overlaid on the area of study (O’Sullivan & Unwin, 2000). Then, each cell in the grid is assigned a value based on generating results through use of the IDW formula. The selection of the grid size will influence the output of a map. A fine mesh will provide greater information at a local level, whereas a coarse mesh will generate a smooth surface (O’Sullivan & Unwin, 2000). 

To understand how an unknown value is estimated, let’s take a look at the formula used: 

$$
Z(i) = \frac{\sum_{i=1}^{N} \frac{Z_j}{d_{ij^p}}}{\sum_{i=1}^{N} \frac{1}{d_{ij^p}}}
$$

$${Z_j}$$ is the value of known point. $${d_{ij}}$$ is a variable representative of the distance between the unknown value and the known value. The $${^p}$$ is the weight that was discussed earlier. A lower weight indicates that with greater distance the rate in which the influence a sample point has on the unknown value reduces at a slower rate compared to a higher weight (Bolstad, 2019). In the numerator the summed weights of known values influence the results depending on distance, whereas the denominator normalizes the result as the 1 has no influence on the distance. 

To execute this procedure in R, the firs step is  using the st_bbox( ) command to help R understand where our area of study is being projected.

Next, the formation of the grid as discussed above with the help of the st_make_grid( ) command. It would be beneficial to use a larger cell size of 50 000 m X 50 000 m for this tutorial given we are dealing with a large area of study and we are interested in interpolating the values of precipitation across the whole province, not at a local level. 

Finally using the idw( ) command that utilizes our .csv with information pretaining to our stations (climate_data) and the recent grid we created to assign values of mean daily precipitation to empty cells.  A smaller weight should be used in this instance as we are attempting to understand what the estimated values of precipitation are across the entirety of the province. Because of that, it is important that if a cell is far from a station, that the station has some influence on the estimate of that cell. This would not be possible if the weight was larger, as the influence of a station would diminish at a faster rate. But we do not want a far away station to exert too much of an influence as the geography changes throughout the province, which in exchange influences precipitation levels.  For that, lets use a weight of 0.5.

After turning our data into an sf object, we are able to produce a map using ggplot with our interpolated results.

Let's take a look at how the code looks: 


```{r}
climate_data <- st_read('ClimateData.shp')

bbox <- st_bbox(bc_boundary) 

grid <- st_make_grid(st_as_sfc(bbox), 
                     cellsize = c(50000, 50000))

idw_result <- gstat::idw(PRECIP ~ 1, 
                         locations = climate_data, 
                         newdata = st_as_sf(grid), 
                         idp = 0.5)

idw_sf <- st_as_sf(idw_result)

crs_idw <- st_crs(idw_sf)
crs_polygon <- st_crs(bc_boundary)

idw_clipped <- st_intersection(idw_sf, bc_boundary)

print(st_geometry(idw_clipped))

ggplot(data = idw_clipped) +
  geom_sf(aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c(option = "D") +  
  labs(title = "IDW Interpolation of Average Daily Precipitation",
       fill = "Precipitation (mm)", 
       x = "Longitude", 
       y = "Latitude") +
  annotation_north_arrow(location = 'tr', 
                         which_north = 'true', 
                         width = unit(1, 'cm'), 
                         height = unit(1, 'cm')) +
  theme_minimal() +
  theme(legend.position = "right")
```
![Clipped_IDW_Interpolation_Map](https://github.com/user-attachments/assets/fd171dbc-dcc7-4a15-9fd3-a52064b32353)

Upon examination of the results, we can feel confident in our pixel size and weight decision. When conducting IDW, there is no way to test for the best result (O’Sullivan & Unwin, 2000). It is recommended that an analyst decides if it is the correct version based on whether or not the map looks reasonable (O’Sullivan & Unwin, 2000). Examining our results, we can state that there are some true resemblances to a precipitation map of British Columbia. Some characteristics in our interpolated map that are similar to precipitation patterns in British Columbia, is one being the western coast of Vancouver Island seeing the highest amount of precipitation in the province (McGillivray, 2020). The Lower Mainland showing high concentrations of precipitation compared to the rest of the province (McGillivray, 2020). A visible rain shadow in south Vancouver Island (McGillivray, 2020). Moderately high levels of precipitation along the entire western coast. A reduction in precipitation in the center of the southern portion of the province due to energy being expended when carrying up the coastal mountains because of orographic uplift, causing a rain shadow on the leeward side of the mountains (McGillivray, 2020). As well as the increase in precipitation closer to the Alberta border due to the re-occurrence of orographic lift being carried up the Rocky Mountains (McGillivray, 2020). 

Given these similar characteristics we can be confident in our results. 

## Part D: Data Collection and Management

We now have a clear understanding of how our independent variable performs in our area of study. Moving forward we will now focus on bringing in and cleaning our dependent variable.

As discussed, the dependent data we will be using for this tutorial is the occurrences of forest fires in British Columbia from April 2022 – October 2022. The BC Wildfire Incident Locations – Historical data set was used to collect this information via the BC Data Catalogue. The data is collected and published by BC Wildfire Services. The data is formatted in a .shp file and contains the coordinates of each fire incident recorded since 2009 as well as the forest fires characteristics (https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-incident-locations-historical). The recording of a fire is done so through various sources, but all recordings have been tracked by BC Wildfire Services (https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-incident-locations-historical). Given this is a historical data set, information regarding a previous fire season is updated once a year (https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-incident-locations-historical). As we are dealing with 2022 data, this indicates that this information was updated in April 2023.  

Let’s now begin the process of collecting and managing our independent data.

### Step 1: Filtering Dependent Data

As we are dealing with a historical dataset, we want to filter through the data to obtain dates specific to our research. After reading in the forest fire shapefile we can use the command filter( ) to control for only data pertaining to the fire year 2022 in the FIRE_YEAR column. After editing the IGN_DATE (ignition date) column to a year – month – day format structure using the as.POSIXct( ) command, we can control for particular dates.   

Go ahead and filter the fire point data with the following code: 

```{r}
fire_points <- st_read('H_FIRE_PNT_point.shp')|>
                              st_intersection(bc_boundary)

fire_points_2022 <- fire_points |> 
                    filter(FIRE_YEAR == 2022)

fire_points_2022$IGN_DATE <- as.POSIXct(fire_points_2022$IGN, 
                                       format ='%Y%m%d%H%M%S', 
                                       tz = 'UTC')

fire_points_filtered <- fire_points_2022 |> 
                        filter(IGN_DATE >= as.POSIXct('2022-04-01', tz = 'UTC') 
                               & IGN_DATE <= as.POSIXct('2022-10-31', tz = 'UTC'))
```
### Step 2: Descriptive Statistics 

When conducting data analysis, it is often recommended to conduct descriptive statistics on your data in order to get a further understanding. For this tutorial we will conduct descriptive statistics on the size of forest fires recorded during our research period. 

The following code showcases how to do so as well as how to organize the results in a table: 

```{r}
mean_size <- mean(fire_points_filtered$SIZE_HA, na.rm = TRUE)
median_size <- median(fire_points_filtered$SIZE_HA, na.rm = TRUE)
mode_size <- as.numeric(names(sort(table(fire_points_filtered$SIZE_HA), 
                                   decreasing = TRUE, na.rm = TRUE))[1])

sd_size <- sd(fire_points_filtered$SIZE_HA, na.rm = TRUE)
skew_size <- skewness(fire_points_filtered$SIZE_HA, na.rm = TRUE)[1]
kurt_size <- kurtosis(fire_points_filtered$SIZE_HA, na.rm = TRUE)[1]

CoV_size <- (sd_size/mean_size)*100
PVAL <- shapiro.test(fire_points_filtered$SIZE_HA)$p.value

summary_table <- data.frame(
                             Statistic = c('Mean', 'Median', 'Mode', 
                                           'Standard Deviation', 'Skewness','Kurtosis'), 
                             Value = (round(c(mean_size, median_size, mode_size,
                                         sd_size, skew_size, kurt_size), 3))
                            )

textplot(summary_table, cex = 1.2 )
```
![summary_table](https://github.com/user-attachments/assets/cda4b705-e7ac-4684-ba8d-90a7bcd2fcad)

What we can interpret from these findings is that there has most likely been one or a couple of large forest fires which is causing a vast difference between the mean and median. As we can see, the median is 0.04 hectares, whereas the mean is 73.15 hectares. This suggest that the mean is being pulled by a large value while the median, unaffected, remains relatively small. When we compare the median and mode, we can suggest that the majority of fires were on the smaller size as both the median and mode size are on the lower end with the mode size being 0.009 hectares. 

The high value of the standard deviation is another indicator that there was the occurrence of several large fires. The standard deviation value of 983.58 indicates that fire sizes vary a great deal. This is additionally supported by the highly positive skewed value of 27.46 with results really being pulled to the right. The high level of Kurtosis also shows this, with the value of 887.90 suggesting a strong peak indicating the occurrence of outliers.

This suggest the occurrence of several large fires proving to be outliers compared to the multitude of small fires.

### Step 3: Mapping Occurences of Forest Fires 

Let’s now produce a map that locates where forest fires occurred during the summer of 2022.

First we can apply a raster grid to the entire surface of British Columbia, using the raster( ) command. 

Next, the rasterize( ) command will convert the vector data of our filtered fire points into raster data and place it on top of the grid we just made. Forest Fire points that fall within a grid cell can now be counted. 

After turning the new raster data into a data frame and then a sf object we are then able to develop a density a map showing the occurrence of fires in a 50 km by 50 km cell. 

Go ahead and make a map of this data with the following code:

```{r}
raster_template <- raster(extent(bbox),
                          res = c(50000, 50000),
                          crs = st_crs(bc_boundary))

density_raster <- raster::rasterize(fire_points_filtered,  
                                    raster_template, 
                                    fun = 'count', 
                                    field = 1) 

density_raster[is.na(density_raster)]<- 0


density_df <- as.data.frame(density_raster, xy = TRUE)|> 
              rename(FIRE = layer)


density_sf <- st_as_sf(density_df, 
                       coords = c('x', 'y'), 
                       crs = st_crs(bc_boundary))
                                                
ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = FIRE)) +  
  geom_sf(data = bc_boundary, fill = NA, color = "black") +
  scale_fill_viridis_c(option = "plasma") + 
  theme_minimal() +
  labs(title = "Density Map of Fire Points",
       x = "Longitude",
       y = "Latitude",
       fill = "Density") +
  annotation_north_arrow(location = 'tr', 
                         which_north = 'true', 
                         width = unit(1, 'cm'), 
                         height = unit(1, 'cm'))

st_write(density_sf,
         'density_points.shp', 
          delete_dsn = TRUE) 
```
![Fire_Map](https://github.com/user-attachments/assets/899e862c-df13-411a-bace-c703639b5f95)

## Step E: Point-Pattern Analysis 

Now that we understand the location of where forest fires are occurring in the province, we now have to decide if there is clustering occurring with the fires. If clustering is occurring this could suggest that there is a phenomenon at play that is producing forest fires in greater quantities in one region compared to another region. To accomplish this, we use Point Pattern Analysis. 

Point Pattern Analysis is the spatial analysis of point data tied to a location that is used to describe the pattern of points that are representative of an event (O’Sullivan & Unwin, 2000). What is of most interest in the engagement of point pattern analysis is to determine as to whether or not there is clustering occurring in the event data in particular areas (O’Sullivan & Unwin, 2000). It is also of interest to uncover the opposite and determine that the events under analysis are not in a clustering formation (O’Sullivan & Unwin, 2000). The application of point pattern analysis to our data may prove useful in understanding our final findings when we examine the relationship between winter precipitation and summer forest fires. 

The three forms of point pattern analysis we will be conducting include Nearest Neighbour Analysis, K-Function and Kernel Density Estimation (KDE). 

### Step 1: Nearest Neighbour Analysis 

The first point-pattern analysis test we will conduct is Nearest Neighbour Analysis (NNA). The outcome of this test will decide if the occurrence of forest fires is random, dispersed or clustered (Grekousis, 2020). This is accomplished by comparing what has been observed to a random theoretical observation (Grekousis, 2020). The null hypothesis being tested is; is the pattern random due to complete spatial randomness (Grekousis, 2020). This will be tested against a generated p-value which will confirm if this is the case or not. 

Nearest Neighbour Analysis can be conducted in four steps:

##### Step i: Nearest Neighbour Distance 

The distances between each point to its nearest neighbour is calculated. The sum of the measurements are generated. This value is then divided by total number of points (n). This produces the average nearest neighbour distance (NND).

$$ NND = \frac{\sum{NND}}{n} $$

##### Step ii: Random Pattern 

NND is compared to a random pattern (NNDr). One should use the same density as the point-pattern under analysis. This generate a random point-pattern observation that uses the same number of events but arranged if the points were randomly scattered (Grekousis, 2020).

$$ NND_{\text{random}} = \frac{1}{2 \sqrt{\text{density}}} $$

##### Step iii: Perfect Dispersion

Generates an output that indicates what a perfectly dispersed pattern is.

$$ NND_{\text{dispersed}} = \frac{0.26136}{ \sqrt{\text{n(density)}}} $$

##### Step iv: Z - Test

It must be determined if the point-pattern under analysis is significantly different than a random pattern. This is accomplished through a z-test calculated with a standard error. First, calculate the standard error and then the z score. 

$$ \sigma = \frac{0.26136}{ \sqrt{\text{n(density)}}} $$

$$ Z = \frac{NND-NND_{\text{random}}}{\sigma} $$

This is how the do complete a NNA in R: 

```{r}
st_crs(fire_points_filtered)<- st_crs(bc_boundary)

fire_points_df <- st_coordinates(fire_points_filtered)

fire_points_ppp <- ppp(x = fire_points_df[,1], 
                       y = fire_points_df[,2], 
                       window = as.owin(st_bbox(bc_boundary)))

nn_distances <- nndist(fire_points_ppp)

nn_df <- as.data.frame(as.numeric(nn_distances))
colnames(nn_df)<- 'Distance'

nnd <- sum(nn_df$Distance)/nrow(nn_df)

studyArea<- area(bc_boundary)

pointDensity <- nrow(nn_df) / studyArea
random.nnd <- 1 / (2 * sqrt(pointDensity))
perfect_dispersion.nnd <- 1.07453 / sqrt(pointDensity)
ratio <- nnd / random.nnd

standard_error.nnd  <- .26136 / sqrt(nrow(nn_df) * pointDensity)

z <- (nnd - random.nnd)/standard_error.nnd

nnaResults <- data.frame("Study Area" = paste0(round(studyArea, 2)),
                         "Perfect Dispersion" = round(perfect_dispersion.nnd, 2), 
                         "Random" = round(random.nnd, 2), 
                         "NND" = round(nnd, 2), 
                         Zscore = round(z, 2), 
                         Ratio = round(ratio, 2), 
                         check.names = FALSE)
```
![nna](https://github.com/user-attachments/assets/19006d3f-5565-46cf-8eae-15a4d4300f50)

What to do our results reveal about our forest fire point data? It is important to consider that if the nearest neighbour ratio is less than 1, this indicates there is clustering. In our cases we can see that the nearest neighbour ration is 0.62, indicating that clustering is occurring. We can also see that NND, which is the average distance between points is 7070.87 m, which is less than what would occur in a random situation with the same number of points as that value generates to 11,353.07 m. This is also the case when we compare NND to prefect dispersion, NND comes up less. This suggest clustering is occurring with forest fires. With a z-score less than zero, it indicates that the clustering is statistically significant. 

### Step 2: K-Function

An additional point-pattern analysis we will conduct is a K-Function test. This supplementary test will serve as confirmation that indeed clustering is occurring with forest fires. The difference with this test is that it is based on a distance function (Grekousis, 2020). The outcome generated is the expected number of events within a radius as the distance of the radius increases (Grekousis, 2020). Results are then generated into a plot. At first a random point is selected. A radius around the point is then defined. The number of expected points that fall within the radius is recorded. The radius is then increased. The process is then repeated. This data is represented as observed values (Grekousis, 2020). This point-pattern analysis is completed to determine if the pattern under observation is random, dispersed or clustered at a particular distance or at a range of distances (Grekousis, 2020). 

This is the code to conduct a K- Function test and generate a plot: 

```{r}
options(scipen = 999) 
k.function <- Kest(fire_points_ppp, correction = "Ripley")
k.function_envelope <- envelope(fire_points_ppp, Kest, nsim = 99, correction = "Ripley", verbose = FALSE)
plot(k.function_envelope, main = "K-Function Plot Forest Fires, BC; 2023")
```
![Rplot](https://github.com/user-attachments/assets/2aa79ae8-cfd6-451b-9d57-37d22073335c)

How do we interpret our results? The x-axis is the length of a radius from the selected random point. The y-axis is the number of expected points that fall within that radius (Grekousis, 2020). Observed values were calculated using the following formula:

$$ K_{\text{obs}} = \lambda^{-1}E(N_d) $$

A line representing complete spatial randomness (K(theo)) is included into the plot. The radius for K(theo) was calculated with the following formula:

$$ K_{\text{theo}} = \pi d^2 $$

A simulation of K(theo) was applied 99 times. An envelope was created to display the results. The highest outcome of the simulation is placed at the top of the envelope. The lowest outcome of the simulation is placed at the bottom of the envelope. If K(obs) is above the K(theo) envelope, K(obs) captured more points than K(theo). This suggest clustering. If K(obs) is below the K(theo) envelope, K(theo) captured more points than K(obs). This suggest dispersion. If K(obs) falls within the envelope, this suggest K(theo) is no different than a random occurrence of points suggesting the pattern is random (Grekousis, 2020). 

Examining the plot generated, K(obs) appears over K(theo) suggesting that forest fires are clustered. 

### Step 3: Kernel Density Estimation 

The last point-pattern analysis used is a Kernel Density Estimation (KDE). Results are generated into a raster map, where concentration of points indicates density (Grekousis, 2020). The results shown are the probability of density of an event from a reference point that are calculated based on surrounding points. A kernel function is applied to calculate these results that moves to each location and repeats the calculation of the KDE formula with distinguishing weights based on distance (Grekousis, 2020).  This produces a heat map that has a smooth surface that depicts the density of events, as well as the intensity of density in particular regions with the distinguishing features of hot spots and cold spots (Grekousis, 2020). 

KDE is calculated with the following formula: 

$$ \lambda_p = \frac{\text{no.}[S \in C(p,r)]}{\pi r^2} $$

The code to conduct a KDE is as so: 

```{r}
kde.100 <- density(fire_points_ppp, sigma = 100, at = "pixels", eps = c(500, 500))
kde.SG <- as(kde.100, "SpatialGridDataFrame")

kde.500 <- density(fire_points_ppp, sigma = 500, at = "pixels", eps = c(500, 500))
kde.SG <- cbind(kde.SG, as(kde.500, "SpatialGridDataFrame"))

kde.1k <- density(fire_points_ppp, sigma = 1000, at = "pixels", eps = c(500, 500)) 
kde.SG <- cbind(kde.SG, as(kde.1k, "SpatialGridDataFrame"))

kde.5k <- density(fire_points_ppp, sigma = 5000, at = "pixels", eps = c(500, 500))
kde.SG <- cbind(kde.SG, as(kde.5k, "SpatialGridDataFrame"))

names(kde.SG) <- c("sigma.100m", "sigma.500m", "sigma.1km", "sigma.5km")
spplot(kde.SG)
```
