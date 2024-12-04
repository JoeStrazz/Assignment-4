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
![Map of Meteorological Stations](map.png)

The generation of a map reveals that there is a good spread of stations across the entirety of province with the North sparse in stations compared to the southern portion of the province. It should also be noted that the island of Hadia Gwaii has no station points.  

