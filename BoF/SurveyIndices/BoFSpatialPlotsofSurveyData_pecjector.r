###................................................................................###
###                               Spatial Figures                                  ###
###                           Full BoF and Approaches                              ###
###                         J.Sameoto Nov 2017, Oct 2018                           ###
###                     B.Wilson - Modified   Dec 2020                             ###
###       (on https://github.com/Mar-scal/Inshore/tree/main/SurveyIndices)         ###
###................................................................................###

# Spatial figures of commercial, recruit and pre-recruit scallop sizes of Survey Density, Survey Biomass, Condition, Meat count and Clappers for BoF: 
#Full Bay
#SPA 1A
#SPA1B
#SPA 3
#SPA4 and 5
#and SPA 6

options(stringsAsFactors = FALSE)

# required packages
require (CircStats)
require (TeachingDemos)
require (PBSmapping)
require (akima)
require (gstat)
require (fields)
require (splancs)
require (RColorBrewer)
require (spatstat)
require (RPMG)
require (rmapshaper)
require (lubridate)
require (tidyverse)
require (sf)
require(maptools)
require(forcats)
library(ROracle)
#library(RCurl)



# Define: 
#uid <- un.sameotoj
#pwd <- pw.sameotoj
#uid <- un.raperj
#pwd <- un.raperj
#uid <- keyring::key_list("Oracle")[1,2]
#pwd <- keyring::key_get("Oracle", uid)
uid <- un.englishg
pwd <- pw.englishg

#set year 
survey.year <- 2024 #removed maxyear in script and changed to survey year
assessmentyear <- 2024 #year in which you are providing advice for- determines where to save files to
path.directory <- "Y:/Inshore/BoF/"

#set up directory to save plot
saveplot.dir <- paste0(path.directory,assessmentyear,"/Assessment/Figures/")

#ROracle
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# ----Import Source functions and polygons---------------------------------------------------------------------


#source("Y:/INSHORE SCALLOP/BoF/Assessment_fns/contour.gen.r")

#### Import Mar-scal functions 
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
           "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
           "https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r") 
# Note: uses older contour.gen.r version (working on alternative to contour.gen altogether).
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

#### Import Mar-scal shapefiles

# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles

#Management zones
mgmt.zones <- rbind(st_read(paste0(temp2, "/SPA1A_polygon_NAD83.shp")) %>% mutate(ET_ID = "1A"), st_read(paste0(temp2, "/SPA1B_polygon_NAD83.shp")) %>% mutate(ET_ID = "1B"), st_read(paste0(temp2, "/SPA2_polygon_NAD83.shp"))%>% mutate(ET_ID = "2"), st_read(paste0(temp2, "/SPA3_polygon_NAD83.shp"))%>% mutate(ET_ID = "3"), st_read(paste0(temp2, "/SPA4_polygon_NAD83.shp"))%>% mutate(ET_ID = "4"), st_read(paste0(temp2, "/SPA5_polygon_NAD83.shp"))%>% mutate(ET_ID = "5"), st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A"), st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B"), st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C"),  st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")) %>% 
  st_transform(crs= 4326)

grey.zone <- st_read(paste0(temp2, "/GreyZone_lines_NAD83.shp"))
spa3.poly <- st_read(paste0(temp2, "/SPA3_modelledArea.shp"))
inVMS <- st_read(paste0(temp2, "/SPA6_VMSstrata_OUT_2015.shp"))
outVMS <- st_read(paste0(temp2, "/SPA6_VMSstrata_IN_2015.shp"))
SPA6A <- st_read(paste0(temp2, "/SPA6A_polygon_NAD83.shp")) %>% mutate(ET_ID = "6A")
SPA6B <- st_read(paste0(temp2, "/SPA6B_polygon_NAD83.shp")) %>% mutate(ET_ID = "6B")
#SPA6C <- st_read(paste0(temp2, "/SPA6C_polygon_NAD83.shp")) %>% mutate(ET_ID = "6C")
SPA6D <- st_read(paste0(temp2, "/SPA6D_polygon_NAD83.shp")) %>% mutate(ET_ID = "6D")

SPA6 <- rbind(SPA6A, SPA6B, SPA6D) %>% #SPA6C
  st_transform(crs = 4326)

Hi.Res.Basemap <- st_read("Z:/GISdata/Private/AtlanticUSCdnCoast/MAR_Maine.shp")

# -----------------------------Import SHF data (live and dead)--------------------------------------------

##.. LIVE ..##
## NOTE: For BoF plots keep strata_id call included; for document remove strata_id limits
#         *Query reads in ALL strata and ALL tow types - this is not equivalent to what is used in population models*

#Db Query:
quer2 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scliveres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)


#If ROracle: 
ScallopSurv <- dbGetQuery(chan, quer2)
ScallopSurv  <- ScallopSurv[,1:51]

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commerical, recruit and prerecruit data columns

ScallopSurv <- ScallopSurv %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>% #standardize number per tow to numbers per m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60
  
table(ScallopSurv$year)
summary(ScallopSurv) #check data

#attr(ScallopSurv, "projection") #check default projection of data
#attr(ScallopSurv, "projection") <- "LL" # assign projection for data
#ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon", "lat"), crs = 4326)

##.. DEAD ..##
#NOTE:  *Query reads in ALL strata and ALL tow types - this is not equivalent to what is used in population models*

#Db Query:
quer3 <- paste(
  "SELECT * 			                ",
  "FROM scallsur.scdeadres s			",
  " LEFT JOIN                          ",
  " 	(SELECT tow_date, cruise, tow_no     ",
  "	 FROM SCALLSUR.sctows) t             ",
  "on (s.cruise = t.cruise and s.tow_no = t.tow_no)",
  "where strata_id in (1,  2 , 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30, 31, 32, 35, 37, 38, 39, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56)    ",
  sep=""
)

ScallopSurv.dead <- dbGetQuery(chan, quer3)
ScallopSurv.dead   <- ScallopSurv.dead[,1:51]

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and commerical, recruit and prerecruit data columns

ScallopSurv.dead <- ScallopSurv.dead %>% 
  mutate(year = year(TOW_DATE)) %>%  #Formats TOW_DATE as date - works with SFA29
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD 
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195
  mutate(rec = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75
  mutate(pre = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0))# Pre-recruit scallop - BIN_ID_0:BIN_ID_60

# -------------------------------Calculating proportion of clappers------------------------------------------

live <- ScallopSurv %>% 
  dplyr::select(CRUISE, STRATA_ID, tow, year, lat, lon, tot, ID, com, rec, pre)

dead <- ScallopSurv.dead %>% 
  dplyr::select(CRUISE, STRATA_ID, tow, year, lat, lon, tot, ID, com, rec, pre)

prop.clappers <- merge(live,dead,by="ID") %>% 
  mutate(prop.dead.com = com.y/(com.x + com.y)) %>% #number of commercial sized clappers relative to total number of commercial size live and dead combined
  mutate(prop.dead.rec = rec.y/(rec.x + rec.y)) %>% #number of recruit sized clappers relative to total number of recruit size live and dead combined
  mutate_at(vars(prop.dead.com:prop.dead.rec), ~replace(., is.na(.), 0)) %>% 
  dplyr::select(ID, CRUISE = CRUISE.x, STRATA_ID = STRATA_ID.x, tow = tow.x, year = year.x, lat = lat.x, lon = lon.x, prop.dead.com, prop.dead.rec)

#	write.csv(prop.clappers, file="Y:/Inshore/BoF/dataoutput/PropClappers.csv")  #Export if needed 

# --------------------------------Import Biomass per tow data-----------------------------------------

# this is output from the meat weight/shell height modelling !NB: not all years needed depending on what you want to show
#code for reading in multiple csvs at once and combining into one dataframe from D.Keith (2015)

max.yr <- max(na.omit(ScallopSurv$year))
Year <- seq((max.yr-4),max.yr)
Year <- Year[! Year %in% 2020] #No 2020 data - remove from data query.
num.years <- length(Year)

#SPA1A1B4and5
BFliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/BFliveweight",Year[i],".csv",sep=""), header=T)
  BFliveweight <- rbind(BFliveweight,temp)
}

#SPA3
BIliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/BIliveweight",Year[i],".csv",sep=""), header=T)
  BIliveweight <- rbind(BIliveweight,temp)
}

#SPA6
GMliveweight <- NULL
for(i in 1:num.years)
{
  # Make a list with each years data in it, extract it as needed later
  temp <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/GMliveweight",Year[i],".csv",sep=""), header=T)
  GMliveweight <- rbind(GMliveweight,temp)
}

liveweight <- rbind(BFliveweight, if(exists("BIliveweight")) BIliveweight, if(exists("GMliveweight")) GMliveweight) #Combine SPA condition data together if data is available


#check data
head(liveweight)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), tow, tot (standardized to #/m^2), ID (cruise.tow#), and biomass for commerical, recruit and prerecruit data columns
ScallopSurv.kg <- liveweight %>%
  dplyr::rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>% #Convert to DD
  dplyr::rename(tow = TOW_NO) %>%  #Rename column TOW_NO to tow - required for one of the mapping functions.
  mutate(tot = dplyr::select(., BIN_ID_0:BIN_ID_195) %>% rowSums(na.rm = TRUE)/4267.2) %>%  #add tot for bin 0 to bin 195 and standardize to #/m^2
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) %>%  #Creates ID column with cruise and tow number
  mutate(com.bm = dplyr::select(., BIN_ID_80:BIN_ID_195) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Commercial scallop - BIN_ID_80:BIN_ID_195 /1000
  mutate(rec.bm = dplyr::select(., BIN_ID_65:BIN_ID_75) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) %>% # Recruit scallop - BIN_ID_65:BIN_ID_75 /1000
  mutate(pre.bm = dplyr::select(., BIN_ID_0:BIN_ID_60) %>% rowSums(na.rm = TRUE) %>% round(0)/1000) # Pre-recruit scallop - BIN_ID_0:BIN_ID_60 /1000
  

# For Meat Count plot:
ScallopSurv.mtcnt <- ScallopSurv.kg %>% 
  dplyr::select(ID, STRATA_ID, year, lat, lon, com.bm) %>% 
  merge(ScallopSurv %>% dplyr::select(ID, com), by = "ID") %>% 
  mutate(meat.count = (0.5/(com.bm/com))) %>% 
  filter(!is.na(meat.count))

# --------------------------------Load Condition data for survey year -----------------------------------------

#Condition data is read by looking for files with "ConditionforMap" in the file name from the respective SFAs located within the current assessment year folder 
#(e.g. Y:\INSHORE SCALLOP\BoF\2020\Assessment\Data\SurveyIndices\SPA1A1B4and5\BFConditionforMap2013.csv). This will pull in the data for each year that a condition file was previously generated and then can be used for assessing multiple years.

#SPA1A1B4and5
con.dat.list <- list.files(path = paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5"), pattern = "^BFConditionforMap")
BF.con.dat <- list()
for(i in 1:length(con.dat.list)){
  BF.con.dat[[i]] <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA1A1B4and5/",con.dat.list[[i]]))
}

BF.con.dat <- bind_rows(BF.con.dat) 
BF.con.dat <- BF.con.dat %>% #Combine the condition data from files that are found
  mutate(CRUISE = paste0("BF", BF.con.dat$YEAR))#Add Cruise information

#SPA3
con.dat.list <- list.files(path = paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3"), pattern = "^BIConditionforMap")
BI.con.dat <- list()
for(i in 1:length(con.dat.list)){
  BI.con.dat[[i]] <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA3/",con.dat.list[[i]]))
}

BI.con.dat <- bind_rows(BI.con.dat)
BI.con.dat <- BI.con.dat %>% #Combine the condition data from files that are found
  mutate(CRUISE = paste0("BI", BI.con.dat$YEAR)) #Add Cruise information


#SPA6
con.dat.list <- list.files(path = paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6"), pattern = "^GMConditionforMap")
GM.con.dat <- list()
for(i in 1:length(con.dat.list)){
  GM.con.dat[[i]] <- read.csv(paste0(path.directory,assessmentyear,"/Assessment/Data/SurveyIndices/SPA6/",con.dat.list[[i]]))
}

GM.con.dat <- bind_rows(GM.con.dat) 
GM.con.dat <- GM.con.dat %>% #Combine the condition data from files that are found
  mutate(CRUISE = paste0("GM", GM.con.dat$YEAR)) #Add Cruise information

#Now combine the Cruise dataframes together
con.dat <- rbind(BF.con.dat, if(exists("BI.con.dat")) BI.con.dat, if(exists("GM.con.dat")) GM.con.dat) #Combine SPA condition data together if data is available

#check data structure
head(con.dat)
table(con.dat$CRUISE)

#...SETTING UP DATA...#
#create year, lat (DD), lon (DD), and ID (cruise.tow#) columns
con.dat <- con.dat %>%
  dplyr::rename(year = YEAR) %>% 
  mutate(lat = convert.dd.dddd(START_LAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(START_LONG)) %>%  #Convert to DD
  rename(tow = TOW_NO) %>% 
  unite(ID, c("CRUISE", "tow"), sep = ".", remove = FALSE) #No CRUISE column in con.dat - ID column is required for contour.gen function

# --------------Set plot themes (legend orientation/aesthetics)------------------------------

#Set legend format for plots
plot.theme <-  theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                     axis.title = element_text(size = 12),
                     axis.text = element_text(size = 10),
                     legend.title = element_text(size = 10, face = "bold"), 
                     legend.text = element_text(size = 8),
                     legend.position = c(.87,.32), #legend position
                     legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                     legend.box.margin = margin(6, 8, 6, 8)) #Legend bkg margin (top, right, bottom, left)

plot.theme.1b <-  theme(legend.key.size = unit(5,"mm"),
                        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                        axis.title = element_text(size = 12),
                        axis.text = element_text(size = 10),
                        legend.title = element_text(size = 8, face = "bold"), 
                        legend.text = element_text(size = 7),
                        legend.direction="horizontal",
                        legend.position = c(.70, .09),#legend position
                        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                        legend.box.margin = margin(1, 1, 1, 1))

plot.theme.3 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      legend.position = c(.87,.44), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                      legend.box.margin = margin(6, 8, 6, 8))

plot.theme.4 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      #legend.direction="horizontal",
                      legend.position = c(.91,.22), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and transparency
                      legend.box.margin = margin(1, 1, 1, 1)) #Legend bkg margin (top, right, bottom, left)

plot.theme.6 <- theme(plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      legend.title = element_text(size = 10, face = "bold"), 
                      legend.text = element_text(size = 8),
                      legend.position = c(.10,.6), #legend position
                      legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
                      legend.box.margin = margin(6, 8, 6, 8))


#Need to run to use pecjector - will look into 
sf::sf_use_s2(FALSE)


# ------------------------------COMMERCIAL SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

# ----DENSITY PLOTS -----

#For FULL BAY, SPA1A, SPA1B, SPA4&5

#Create contour and specify plot aesthetics. Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
com.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                                       dplyr::select(ID, lon, lat, com),
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector



#Plot with Pecjector for each area:

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object
#adding transparency slows down plot creating and saving significantly....

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "BoF Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = list(x=c(-66.40,-64.80), y=c(44.37,45.30), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4
  
  
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height =8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
##p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector


p <- pecjector(area =list(x=c(-66.4, -67.5), y=c(44.4, 45.2), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA)) #
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32, 54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



# ----BIOMASS PLOTS -----

#NOTE: TO PLOT OTHER YEARS CHANGE *biomass.year*
biomass.year <- survey.year # change year (e.g. biomass.year <- "2019") to plot other years (dependent on the files that were loaded in the object liveweight)

#For FULL BAY, SPA1A, SPA1B, SPA4&5, SPA6. Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
com.contours<-contour.gen(ScallopSurv.kg %>% filter(year==biomass.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,com.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == biomass.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(biomass.year, "", "BoF Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComBiomass',biomass.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == biomass.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(biomass.year, "", "SPA1A Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComBiomass',biomass.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == biomass.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(biomass.year, "", "SPA1B Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComBiomass',biomass.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == biomass.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(biomass.year, "", "SPA4 Biomass (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComBiomass',biomass.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----
#NOTE: TO PLOT OTHER YEARS CHANGE *biomass.year*
biomass.year <- survey.year # change year (e.g. biomass.year <- "2019") to plot other years (dependent on the files that were loaded in the object biomass.year)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == biomass.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #excludes SFA29
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(biomass.year, "", "SPA3 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComBiomass',biomass.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----
#NOTE: TO PLOT OTHER YEARS CHANGE *biomass.year*
biomass.year <- survey.year # change year (e.g. biomass.year <- "2019") to plot other years (dependent on the files that were loaded in the object biomass.year)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == biomass.year, STRATA_ID %in% c(30,31,32)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(biomass.year, "", "SPA6 Biomass (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComBiomass',biomass.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----CONDITION PLOTS-----

#NOTE: TO PLOT OTHER YEARS CHANGE *cond.year*
cond.year <- survey.year # change year (e.g. cond.year <- "2019") to plot other years (dependent on the files that were loaded in the object con.dat)

#For FULL BAY, SPA1A, SPA1B, SPA4&5, SPA3, SPA6. Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.

com.contours <- contour.gen(con.dat %>% filter(year== cond.year) %>% dplyr::select(ID, lon, lat, Condition),ticks='define',nstrata=7,str.min=0,place=2,id.par=5,interp.method='gstat',key='strata',blank=T,plot=F,res=0.01,blank.dist = 0.1)

#lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
lvls=c(4,6,8,10,12,14,16) #for higher conditions
#lvls=c(4,6,8,10,12,14,16,18,20)

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
#labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
labels <- c("4-6", "6-8", "8-10", "10-12", "12-14", "14-16","16+") #for higher conditions
#labels <- c("4-6", "6-8", "8-10", "10-12", "12-14","14-16", "16-18", "18-20", "20+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) # set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
      filter(year == cond.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                                                                      aes(lon, lat), size = 0.5) +
  labs(title = paste(cond.year, "", "BoF Condition"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .75))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Condition',cond.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == cond.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(cond.year, "", "SPA1A Condition"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .75))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Condition',cond.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == cond.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(cond.year, "", "SPA1B Condition"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .75))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Condition',cond.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == cond.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(cond.year, "", "SPA4 Condition"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .75))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Condition',cond.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

#lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
lvls=c(4,6,8,10,12,14,16) #levels to be color coded
#lvls=c(4,6,8,10,12,14,16,18,20)

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
#labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
labels <- c("4-6", "6-8", "8-10", "10-12", "12-14","14-16", "16+")
#labels <- c("4-6", "6-8", "8-10", "10-12", "12-14","14-16", "16-18", "18-20", "20+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == cond.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(cond.year, "", "SPA3 Condition"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .75))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Condition',cond.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

#lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
lvls=c(4,6,8,10,12,14,16) # for 2024 
#lvls=c(4,6,8,10,12,14,16,18) #for 2023

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
#labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
labels <- c("4-6", "6-8", "8-10", "10-12", "12-14","14-16", "16+")
#labels <- c("4-6", "6-8", "8-10", "10-12", "12-14", "14-16", "16-18", "18+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% 
                       filter(year == cond.year, str_detect(CRUISE, "GM")),
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(cond.year, "", "SPA6 Condition"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .75))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Condition',cond.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----MEAT COUNT (IGNORE (2024))-----
###DOES NOT WORK, IGONRE. FUNCTION BREAKS AND DOES NOT ALLOW CREATION OF PLOTS###

#For FULL BAY, SPA1A, SPA1B, SPA4&5
mc.contours <-contour.gen(ScallopSurv.mtcnt %>% filter(year==survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)) %>% #Excludes SPA3 and SFA29
                           dplyr::select(ID, lon, lat, meat.count), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(lvls),"Spectral"), level = lvls)

#Set aesthetics for plot
n.breaks <- length(unique(totCont.poly.sf$col)) 
col <- rev(brewer.pal(length(lvls),"Spectral")) #set colours
cfd <- scale_fill_manual(values = alpha(col[1:n.breaks], 0.45), name = expression(frac(Meats,"500g"))) #set custom fill arguments for pecjector.

# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BOF Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('br',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year,  !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Meat Count (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

#Create contour and specify plot aesthetics
mc.contours <- contour.gen(ScallopSurv.mtcnt %>% 
                              filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)) %>% 
                              dplyr::select(ID, lon, lat, meat.count, -STRATA_ID, -year), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',
                            blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

range(mc.contours$contour.dat$Z)
lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
col=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>%
  mutate(col = brewer.pal(length(lvls),"Spectral"), level = lvls)

#Set aesthetics for plot
n.breaks <- length(unique(totCont.poly.sf$col)) 
col <- rev(brewer.pal(length(lvls),"Spectral")) #set colours
cfd <- scale_fill_manual(values = alpha(col[1:n.breaks], 0.45), name = expression(frac(Meats,"500g"))) #set custom fill arguments for pecjector.


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5,-1, -1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, STRATA_ID %in% c(22, 23, 24)), #only SPA3 strata IDs
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Meat Count (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

mc.contours<-contour.gen(ScallopSurv.mtcnt %>% filter(year==survey.year, STRATA_ID %in% c(30,31,32,54)) %>% #Only SPA6
                           dplyr::select(ID, lon, lat, meat.count), ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',blank=T,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,res=0.01)

lvls=seq(10,45,5)
div=2

CL <- contourLines(mc.contours$image.dat,levels=lvls)
CP <- convCP(CL)
totCont.poly  <- CP$PolySet
Ncol=length(lvls)+div
cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],border=NA,stringsAsFactors=FALSE)

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(lvls),"Spectral"), level = lvls)

#Set aesthetics for plot
n.breaks <- length(unique(totCont.poly.sf$col)) 
col <- rev(brewer.pal(length(lvls),"Spectral")) #set colours
cfd <- scale_fill_manual(values = alpha(col[1:n.breaks], 0.45), name = expression(frac(Meats,"500g"))) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5, -1,-1)), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.mtcnt %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  #geom_sf(data = outVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  #geom_sf(data = inVMS, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA6 Meat Count (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_Meatcount',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----CLAPPERS -----

#For FULL BAY, SPA1A, SPA1B, SPA4&5, SPA 3, SPA6. Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.

com.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year,!STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
                                add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% 
                                mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% 
                                dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #exclude SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Clapper Density (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_ComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----

#For FULL BAY, SPA1A, SPA1B, SPA4&5, SPA, SPA6. Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
com.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,prop.dead.com),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #Excludes SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1a",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #Excludes SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer

p <- pecjector(area = "spa1b",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) +#Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Proportion (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #exclude SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Clapper Proportion (>= 80mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PropComClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ------------------------------RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------


#For FULL BAY, SPA1A, SPA1B, SPA4&5, SPA3, SPA6
#Create contour and specify plot aesthetics. Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
rec.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                              dplyr::select(ID, lon, lat, rec), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(1,5,10,20,30,50,100,200,300) # for lower densities
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-300", "300+")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

# ----DENSITY PLOTS -----

#Plot with Pecjector for each area:

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "BoF Density (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "SPA1A Density (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "SPA1B Density (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "SPA4 Density (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls <- c(1,5,10,20,30,50,100,200,300) # for lower densities
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-300", "300+")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #excludes SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Density (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls <- c(1,5,10,20,30,50,100,200,300) # for lower densities
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-300", "300+")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Density (65-79 mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----BIOMASS PLOTS -----
#Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
rec.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,rec.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls =c(0.01,0.1,0.3,0.5,1,2,3) #for low biomass
#lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-0.3", "0.3-0.5", "0.5-1", "1-2", "2-3", "3+")
#labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Biomass (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Biomass (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Biomass (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Biomass (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls =c(0.01,0.1,0.3,0.5,1,2,3) #for low biomass
#lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-0.3", "0.3-0.5", "0.5-1", "1-2", "2-3", "3+")
#labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #exclude SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Biomass (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls= c(0.01,0.1,0.3,0.5,1,2,3)
#lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-0.3", "0.3-0.5", "0.5-1", "1-2", "2-3", "3+")
#labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.


p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Biomass (65-79 mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----
#Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
rec.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year,!STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Density (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Density (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Density (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
                                add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% 
                                mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% 
                                  dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Density (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #excludes SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Clapper Density (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Clapper Density (65-79 mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_RecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----PROPORTION OF CLAPPERS -----
#Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
rec.contours<-contour.gen(prop.clappers %>% filter(year==survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,prop.dead.rec),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata', blank=T,plot=F,res=0.01)

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))


#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")),add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #Excludes SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Proportion (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1a",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #Excludes SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Proportion (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1b",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #Excludes SPA3 and SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Proportion (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7, title.position = "top"))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #Excludes SFA29
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Clapper Proportion (65-79 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA3 -----

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #Excludes SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Clapper Proportion (65-79 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA6 -----

lvls=c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.50,1) #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("0.01-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20", "0.20-0.25", "0.25-0.30", "0.30-0.50", "0.50-1", "1")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Proportion", limits = labels) #set custom fill arguments for pecjector.


p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = prop.clappers %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Clapper Proportion (65-79 mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PropRecClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)



# ------------------------------PRE-RECRUIT SCALLOP - SURVEY DISTRIBUTION PLOTS -------------------------------------------

#For FULL BAY, SPA1A, SPA1B, SPA4&5, SPA3, SPA6
#Create contour and specify plot aesthetics. Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
pre.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                              dplyr::select(ID, lon, lat, pre), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(1,5,10,20,30,50,100,200,300) # for lower densities
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-300", "300+")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----DENSITY PLOTS -----

#Plot with Pecjector for each area:

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>%
                                   mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "BoF Density (< 65 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "SPA1A Density (< 65 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

#save
ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "SPA1B Density (< 65 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b


ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "SPA4 Density (< 65 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls <- c(1,5,10,20,30,50,100,200,300) # for lower densities
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-300", "300+")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,32,43,44,45,46)), #exclude SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Density (< 65 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls <- c(1,5,10,20,30,50,100,200,300) # for lower densities
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-300", "300+")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), 
               add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Density (< 65 mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----BIOMASS PLOTS -----
#Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
pre.contours<-contour.gen(ScallopSurv.kg %>% filter(year==survey.year, !STRATA_ID %in% c( 46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,pre.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls =c(0.01,0.1,0.2,0.3,0.4,0.5,1) #for low biomass
#lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-1", "1+")
#labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

# ----FULL BAY -----

p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Biomass (< 65 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA1A -----

p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Biomass (< 65 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year,!STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Biomass (< 65 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)), #excludes SPA3 and SFA29 data
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA4 Biomass (< 65 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls =c(0.01,0.1,0.2,0.3,0.4,0.5,1) #for low biomass
#lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-1", "1+")
#labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #exclude SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Biomass (< 65 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls =c(0.01,0.1,0.2,0.3,0.4,0.5,1) #for low biomass
#lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-1", "1+")
#labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.6), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Biomass (< 65 mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----CLAPPERS -----
#Using this contour for all figures to avoid mapping errors. Contour.gen function doesn't run if all values in response variable column are 0.
pre.contours<-contour.gen(ScallopSurv.dead %>%  filter(year==survey.year,!STRATA_ID %in% c(46, 45, 44, 42, 43, 41)) %>% #Excludes SFA29
                            dplyr::select(ID,lon,lat,pre),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',
                          blank=T,plot=F,res=0.01)

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.


# ----FULL BAY -----

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "BoF Clapper Density (< 65 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.50,-64.30), ylim = c(44.25,45.80), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_BF_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1A -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1A",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1A Clapper Density (< 65 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.40,-64.80), ylim = c(44.37,45.30), expand = FALSE)+
  plot.theme

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1A_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA1B -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa1B",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(22, 23, 24, 46, 45, 44, 42, 43, 41)),
                     aes(lon, lat), size = 0.5) +
  labs(title = paste(survey.year, "", "SPA1B Clapper Density (< 65 mm)"), x = "Longitude",
       y = "Latitude") +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-64.30), ylim = c(44.80,45.70), expand = FALSE)+
  plot.theme.1b

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA1B_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA4 and 5 -----

#Pecjector with custom contour layer
p <- pecjector(area = "spa4",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), 
                                add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% 
                                mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% 
                                dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(title = paste(survey.year, "", "SPA4 Clapper Density (< 65 mm)"), x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-66.20,-65.51), ylim = c(44.43,44.96), expand = FALSE)+
  plot.theme.4

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA4_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----SPA3 -----

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area = "spa3",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap", survey = c("inshore", "outline")), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object

p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, !STRATA_ID %in% c(41,42,43,44,45,46)), #excludes SFA29
                     aes(lon, lat), size = 0.5) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = spa3.poly, size = 0.7, colour = "red", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in red
  labs(title = paste(survey.year, "", "SPA3 Clapper Density (< 65 mm)"), x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-66.82,-65.80), ylim = c(43.62,44.60), expand = FALSE)+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  plot.theme.3

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA3_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----SPA6 -----

lvls=c(1,5,10,15,20,25,30,50,100) #levels to be color coded

CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-50", "50-100", "100+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.5,-66.4), y=c(45.2,44.4), crs=4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(bathy = "ScallopMap"), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))
#p$layers[[2]]$aes_params$alpha <- 0.25 #Changing transparency of bathy contours within pecjector object


p + #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.dead %>% 
                       filter(year == survey.year, STRATA_ID %in% c(30,31,32,54)), #Only SPA6
                     aes(lon, lat), size = 0.5) +
  #geom_sf(data = SPA6, size = 0.4, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  geom_sf(data = outVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  geom_sf(data = inVMS, size = 0.7, colour = "grey50", alpha = 0.7, fill = NA) + # Plots Modeled area boundaries in grey
  labs(title = paste(survey.year, "", "SPA6 Clapper Density (< 65 mm)"), x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.5, -66.4), ylim = c(44.4, 45.2), expand = FALSE)+
  plot.theme.6

ggsave(filename = paste0(saveplot.dir,'ContPlot_SPA6_PreClappers',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# --------------Plots for Industry Supplemental Update Document (Markdown) ----------------------------------

#Markdown document directory - paste0("Y:/INSHORE SCALLOP/BoF/",surveyyear,"/Assessment/Documents/CSAS")

#READ IN Land shapefile from Github *same file used in pecjector - used to crop managment zone boundaries.
# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/other_boundaries.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
# Now read in the strata shapefile. Using updated basemap (Oct 2024)
# land <- st_read(paste0(temp2, "/Atl_region_land.shp")) %>% 
#   dplyr::filter(PROVINCE %in% c("New Brunswick", "Nova Scotia", "Maine"))

mgmt.zones.crop <- rmapshaper::ms_erase(mgmt.zones,land)
#mapview::mapview(mgmt.zones.crop)
#mapview::mapview(mgmt.zones)


# ----Commercial Biomass - full bay -----

#NOTE: TO PLOT OTHER YEARS CHANGE *biomass.year*
biomass.year <- survey.year # change year (e.g. biomass.year <- "2019") to plot other years (dependent on the files that were loaded in the object liveweight)

#For FULL BAY, (SPA1A, SPA1B, SPA3, SPA4, SPA5, SPA6)
com.contours<-contour.gen(ScallopSurv.kg %>% filter(year==biomass.year) %>%
                            dplyr::select(ID,lon,lat,com.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.8), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.28,-64.4), y=c(45.62, 43.60), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',add_layer = list(bathy = c(50,'c')), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p+ #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% filter(year==biomass.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = mgmt.zones.crop, size = 0.3, colour= "black", alpha = 0.4, fill = NA)+
  geom_sf(data = grey.zone, size = 1, colour= "black", fill = NA)+
  #geom_sf(data = outVMS, linetype = "dashed", size = 0.4, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS OUT
  geom_sf(data = inVMS, linetype = "dashed",size = 0.3, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS IN
  geom_sf(data = spa3.poly, linetype = "dashed",size = 0.3, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.28,-64.4), ylim = c(43.60, 45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.87,.32), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_ComBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ----Recruit Biomass - full bay -----

rec.contours<-contour.gen(ScallopSurv.kg %>% filter(year==biomass.year) %>%  dplyr::select(ID,lon,lat,rec.bm),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls =c(0.01,0.1,0.3,0.5,1,2,3) #for low biomass
#lvls=c(0.01,0.1,1,2,4,6,8)  #levels to be color coded

CL <- contourLines(rec.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
labels <- c("0.01-0.1", "0.1-0.3", "0.3-0.5", "0.5-1", "1-2", "2-3", "3+")
#labels <- c("0.01-0.1", "0.1-1", "1-2", "2-4", "4-6", "6-8", "8+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.8), breaks = labels, name = expression(frac(kg,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.28,-64.4), y=c(45.62, 43.60), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',add_layer = list(bathy = c(50,'c')), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p+ #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% filter(year==biomass.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = mgmt.zones.crop, size = 0.3, colour= "black", alpha = 0.4, fill = NA)+
  geom_sf(data = grey.zone, size = 1, colour= "black", fill = NA)+
  #geom_sf(data = outVMS, linetype = "dashed", size = 0.4, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS OUT
  geom_sf(data = inVMS, linetype = "dashed",size = 0.4, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS IN
  geom_sf(data = spa3.poly, linetype = "dashed",size = 0.45, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.28,-64.4), ylim = c(43.60, 45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.87,.32), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_RecBiomass',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

# ----Pre-Recruit numbers per tow - full bay -----

#Create contour and specify plot aesthetics
pre.contours <- contour.gen(ScallopSurv %>% 
                              filter(year == survey.year) %>%
                              dplyr::select(ID, lon, lat, pre), 
                            ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

lvls <- c(1,5,10,20,30,50,100,200,300) # for lower densities
#lvls <- c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(pre.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Colour aesthetics and breaks for contours
labels <- c("1-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-300", "300+")
#labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.8), breaks = labels, name = expression(frac(N,tow)), limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.28,-64.4), y=c(45.62, 43.60), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',add_layer = list(bathy = c(50,'c')), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p+ #Plot survey data and format figure.
  geom_spatial_point(data = ScallopSurv.kg %>% filter(year==biomass.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = mgmt.zones.crop, size = 0.3, colour= "black", alpha = 0.4, fill = NA)+
  geom_sf(data = grey.zone, size = 1, colour= "black", fill = NA)+
  #geom_sf(data = outVMS, linetype = "dashed", size = 0.4, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS OUT
  geom_sf(data = inVMS, linetype = "dashed",size = 0.4, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS IN
  geom_sf(data = spa3.poly, linetype = "dashed",size = 0.45, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.28,-64.4), ylim = c(43.65, 45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.87,.34), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_PrerecDensity',survey.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# ---- Plots for referencing (not for industry supplement) -----

# ---- Condition - full bay -----

#Make sure to read in the mgmt.zone shapefile before plotting (in the beginning of the script section "Plots for Industry Supplemental Update Document (Markdown)")

cond.year <- survey.year # change year (e.g. cond.year <- "2019") to plot other years (dependent on the files that were loaded in the object con.dat)

#For FULL BAY, (SPA1A, SPA1B, SPA3, SPA4, SPA5, SPA6)
com.contours<-contour.gen(con.dat %>% filter(year==cond.year) %>%
                            dplyr::select(ID,lon,lat, Condition),
                          ticks='define', nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

#lvls=c(5,6,7,8,9,10,11,12) #levels to be color coded
lvls=c(4,6,8,10,12,14,16) #for higher conditions
#lvls=c(4,6,8,10,12,14,16,18,20) #for higher conditions

CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

##Convert pbsmapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(level = unique(CP$PolyData$level))

#Set aesthetics for plot
#labels <- c("5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12+")
labels <- c("4-6", "6-8", "8-10", "10-12", "12-14", "14-16","16+") #for higher conditions
#labels <- c("4-6", "6-8", "8-10", "10-12", "12-14","14-16", "16-18", "18-20", "20+")
col <- brewer.pal(length(lvls),"YlOrBr") #set colours
cfd <- scale_fill_manual(values = alpha(col, 0.4), breaks = labels, name = "Condition (g)", limits = labels) #set custom fill arguments for pecjector.

p <- pecjector(area =list(x=c(-67.18,-64.4), y=c(45.62, 43.60), crs=4326), repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',add_layer = list(bathy = c(50,'c')), add_custom = list(obj = totCont.poly.sf %>% arrange(level) %>% mutate(brk = labels[1:length(unique(CP$PolyData$level))]) %>% mutate(brk = fct_reorder(brk, level)) %>% dplyr::select(brk), size = 1, fill = "cfd", color = NA))


p+ #Plot survey data and format figure.
  geom_spatial_point(data = con.dat %>% filter(year==cond.year), aes(lon, lat), size = 0.5) +
  geom_sf(data = mgmt.zones.crop, size = 0.3, colour= "black", alpha = 0.4, fill = NA)+
  geom_sf(data = grey.zone, size = 1, colour= "black", fill = NA)+
  #geom_sf(data = outVMS, linetype = "dashed", size = 0.4, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS OUT
  geom_sf(data = inVMS, linetype = "dashed",size = 0.3, colour = "black", alpha = 0.7, fill = NA) + #SPA6 - VMS IN
  geom_sf(data = spa3.poly, linetype = "dashed",size = 0.3, colour = "black", alpha = 0.7, fill = NA) +
  ###
  #temporary fix for better map and visible scale bar
  geom_sf(data = Hi.Res.Basemap, size = 0.1, colour = "black", alpha = 1, fill = "grey50") +
  annotation_scale(location = "tl", width_hint = 0.5, pad_x = unit( 0.35, "cm"), pad_y = unit(0.35, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                         pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + 
  ###
  labs(x = "Longitude", y = "Latitude")+
  guides(fill = guide_legend(override.aes= list(alpha = .7))) + #Legend transparency
  coord_sf(xlim = c(-67.18,-64.4), ylim = c(43.60, 45.62), expand = FALSE)+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 11),
        legend.key.size = unit(1.1, "cm"),
        legend.key = element_rect(color=alpha("black", 0.3)),
        legend.position = c(.87,.32), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)), #Legend bkg colour and border
        legend.box.margin = margin(6, 10, 6, 8)) #Legend bkg margin (top, right, bottom, left)

ggsave(filename = paste0(saveplot.dir,'ContPlot_BoFAll_Condition',cond.year,'.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)


# --------------STATIC BoF strata plot for appendix by F.Keyser - Modified for pecjector by B.Wilson (2021) ----------------------------------

require(ggplot2)
require(rgdal)
require(sp)
require(raster)
require(plyr)

BILU.poly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/BILUpoly.csv")
VMSpoly<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA3_VMSpoly.csv")
inVMS<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_IN_R_final_MOD.csv")
outvms<-read.csv("Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_OUT_R_final_MOD.csv")
maritimes_sp <- readOGR("Y:/Maps/shp/Maritimes_UTM.shp")
maritimes_sp <- spTransform(maritimes_sp, CRS("+proj=longlat +datum=WGS84"))
maritimes_sp <- crop(maritimes_sp, extent(-67.35, -62.9, 43.35, 46.2))
maritimes <- fortify(maritimes_sp)

scstrata <- read.csv("Y:/INSHORE SCALLOP/Survey/2017/data entry templates and examples/entry check functions/SCSTRATAINFO_August2017.csv")
names(scstrata) <- c("PID", "POS", "X", "Y", "DESCRIPTION", "AREA")
scstrata <- dplyr::arrange(scstrata, PID, POS)

# Remove Offshore banks
scstrata <- scstrata[!scstrata$DESCRIPTION %in% c("NE BANK", "SW BANK"),]

# All folowing code setting positional information for labels
scstratalabs <- aggregate(data=scstrata, cbind(X, Y) ~ DESCRIPTION, function(x) mean(x))
scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("Cape Spencer", "ANNAPOLIS BASIN (BA)", "MidBay North", "SPA 6C", "SPA 1 WEST A",
                                               "SCOTS BAY")] <- scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("Cape Spencer", "ANNAPOLIS BASIN (BA)", "MidBay North", "SPA 6C", "SPA 1 WEST A",
                                                                                                               "SCOTS BAY")] - 0.07
scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("SPA 1 WEST B")] <- scstratalabs$Y[scstratalabs$DESCRIPTION %in% c("SPA 1 WEST B")] + 0.1
scstratalabs$X[scstratalabs$DESCRIPTION %in% c("BRIER ISLAND (BI)", "LURCHER (LU)")] <- scstratalabs$X[scstratalabs$DESCRIPTION %in% c("BRIER ISLAND (BI)", "LURCHER (LU)")] - 0.1

# Removes brackets after description (eg ANNAPOLIS BASIN (BA))
scstratalabs$DESCRIPTION <- gsub(scstratalabs$DESCRIPTION, pattern="\\s*\\([^\\)]+\\)", replacement="")
scstratalabs$DESCRIPTION <- toupper(scstratalabs$DESCRIPTION)

scstrataman <- data.frame(DESCRIPTION=c("2-8 MILES", "8-16 MILES"), X=c(-65.77, -65.85094), Y=c(44.82, 44.9))

#FIRST need to REMOVE object cfd - custom fill discrete environment for the contour plots
rm(cfd)

p <- pecjector(area =list(x=c(-67.25, -64.3), y=c(43.65, 45.6), crs = 4326),repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot', add_layer = list(land = "grey"))

p +
  geom_polygon(data=scstrata, aes(X, Y, group=PID), fill=NA, colour="black") +
  geom_polygon(data=VMSpoly, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  geom_polygon(data=BILU.poly, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  geom_polygon(data=inVMS, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  geom_polygon(data=outvms, aes(X, Y, group=SID), fill=NA,lty="dashed", colour="black") +
  theme_bw() + 
  theme(panel.grid=element_blank()) +
  geom_label(data=scstratalabs[c(11,12,23:47),], aes(X, Y, label=DESCRIPTION), size=3, alpha=0.8) +
  geom_text(data=scstrataman, aes(X, Y, label=DESCRIPTION), alpha=0.8, angle=34, size=5) +
  xlab("LONGITUDE") + ylab("LATITUDE")


ggsave(filename = paste0(saveplot.dir,'BoF_Strata.png'), plot = last_plot(), scale = 2.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)

########################################################################################################################################
  