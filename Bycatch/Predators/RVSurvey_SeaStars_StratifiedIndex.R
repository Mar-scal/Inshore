#stratified estimates for Asterias sp in BoF RV Survey strata - using PEDStrata function - need to account for survey strata change from pre 2014 - which would add a domain estimator - not done yet TBDONE

library(ggspatial)
library(ROracle)
library(tidyverse)
library(sf)
library(Polychrome)
library(units)
library(PEDstrata)

# RV SURVEY EXPLORATORY SEARCH FOR SEA STAR SPECIES CAUGHT IN THE BAY OF FUNDY (STRAT 490:495)
# KAYLA SILVER (Modified by Brittany)
# SEPTEMBER 8TH, 2026

#### Import Mar-scal functions 
funcs <- c(#"https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
  "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  #"https://raw.githubusercontent.com/Mar-scal/Inshore/master/contour.gen.r"
) 

dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


# importing the map
map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", continent = "north america")

ns_coast <- suppressWarnings(suppressMessages(
  sf::st_crop(map_data,
              c(xmin = -56, ymin = 40, xmax = -69, ymax = 48))))

ns_proj <- sf::st_transform(ns_coast, crs = 32620)

#RV survey strata 
strata <- st_read("W:/OffshoreWind/RVSurvey/RVSurveyStrata/current_RV_Strata_2014on/MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83.shp")
st_crs(strata)

strata <- st_transform(strata, crs = 32620) #converted to WGS84 UTMZone 20N 
dim(strata)
strata$area_calc <- st_area(strata)

RVsurvey.strata.table <- readRDS("D:/GIthub/Inshore/Bycatch/Seastars/RVSurveyStrata.rds")
RVsurvey.strata.table

# SEASTARS
#uid <- keyring::key_list("Oracle")[1,2]
#pwd <- keyring::key_get("Oracle", uid)

uid <- un.sameotoj
pwd <- pw.sameotoj



##LOAD DESIRED GROUNDFISH Tables using ROracle  ----
channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=uid, password=pwd, "PTRAN")

#Select only type 1 set = random stratified #NOTE Because selected for type 1 here, don't need to account for this in PEDSTRATA function (could have selected to type 1 in PEDStrata function with subset call)
gsinf <- ROracle::dbGetQuery(channel, paste("select MISSION, SDATE, SETNO, STRAT, SLAT, SLONG, ELAT, ELONG, DIST, DUR, GEAR from groundfish.gsinf where type =1"))

gscat <- ROracle::dbGetQuery(channel, paste ("select MISSION, SETNO, SPEC, TOTWGT, TOTNO from groundfish.gscat"))

gsmissions<-ROracle::dbGetQuery(channel, paste("select MISSION, YEAR, SEASON, VESEL from groundfish.gsmissions"))

#MERGING TO GET YEAR and SEASON
gsinf<- merge(gsinf,gsmissions,by="MISSION")

#Removing the bad tows from the tow length/distance QC
bad_sets <- tibble::tribble(
  ~MISSION,     ~SETNO,
  "NED2005034", 50,
  "NED2013022", 48,
  "NED2013022", 36
)

gsinf <- gsinf %>%
  anti_join(bad_sets, by = c("MISSION", "SETNO"))

# LOAD THIS TABLE FOR SPECIES CODE AND COMMON NAMES
gsspec<-ROracle::dbGetQuery(channel, paste("select COMM, CODE from groundfish.gsspecies"))
gsspec<- gsspec %>% rename("SPEC"="CODE")

all.spec <- merge(gsspec, gscat, by = "SPEC")

#add mission metadata
all.spec.md <- merge(all.spec, gsinf, by = c("MISSION", "SETNO"))

#Look at SPEC 6100, 6109:6111 (ASTEROIDEA Species)

#star.fish %>% filter(SPEC == 6105,
#STRAT %in% 490:495)
aster <- all.spec.md %>% filter(SPEC %in% c(6109:6111),
                                STRAT %in% 490:495)

BoF.RVsurveystrata <- strata %>% filter(StrataID %in% 490:495)

#map of strata areas being used 
ggplot(ns_proj) + geom_sf(data = BoF.RVsurveystrata, aes(fill = StrataID),  colour = NA)


# GET TOTAL WEIGHTS AMD TOTAL NUMBER BY TOW AND STANDARDIZE BY DISTANCE TOWED 
# IF ELSE FUNCTION - IF VESSEL IS CARTIER OR CABOT ("J" OR "B") use 1/Dist) if any other vessel, use 1.75
aster.std <- aster %>%
  group_by(MISSION,SETNO, YEAR, SEASON, SPEC, COMM, STRAT, DIST, DUR, SLAT, SLONG, ELAT, ELONG, GEAR, VESEL) %>%  
  summarise(
    TOTWGT = sum(TOTWGT, na.rm = TRUE),
    TOTNO  = sum(TOTNO, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    BIOMASS_STD = TOTWGT * if_else(VESEL %in% c("J", "B"),
                                   1 / DIST,
                                   1.75 / DIST),
    ABUNDANCE_STD = TOTNO * if_else(VESEL %in% c("J", "B"),
                                    1 / DIST,
                                    1.75 / DIST)
  )
glimpse(aster.std)


#Check years of data to see if need to make expections for time series (i.e. fill in a year with NA)
table(aster.std$YEAR)

#Stratified estimate for subset of strata in BoF for sea stars 
years <- min(aster.std$YEAR):max(aster.std$YEAR) # Runs to the most recent year by default 
years
X <- length(years)
X

seastar.strat.est <- data.frame(Year=years, Mean.nums=rep(NA,X), Pop=rep(NA,X), SE=rep(NA,X), method=rep("PED",X), Area=rep("490:495", X), Age=rep("SeaStars", X))
for(i in 1:length(seastar.strat.est$Year)){
    temp.data <- aster.std[aster.std$YEAR==(min(years)-1)+i,]
    seastar.strat.est[i,2] <- summary(PEDstrata(temp.data, RVsurvey.strata.table, "STRAT", catch=temp.data$BIOMASS_STD, Subset=temp.data$SEASON=="SUMMER"))$yst #going into mean field 
    seastar.strat.est[i,3] <- summary(PEDstrata(temp.data, RVsurvey.strata.table, "STRAT", catch=temp.data$BIOMASS_STD, Subset=temp.data$SEASON=="SUMMER"))$Yst #going into population field 
    seastar.strat.est[i,4] <- summary(PEDstrata(temp.data, RVsurvey.strata.table, "STRAT", catch=temp.data$BIOMASS_STD, Subset=temp.data$SEASON=="SUMMER"))$se.yst
  }
seastar.strat.est

head(seastar.strat.est)

ggplot(data = seastar.strat.est, aes(x=Year , y=Mean.nums)) + 
  geom_point() + 
  geom_line() + 
 # scale_x_continuous(limits = c(2001, (surveyyear+1)), breaks = seq(2001, (surveyyear+1), by = 4)) +
  theme_bw() + ylab("RV Summer survey mean weight (kg) \n per standardize tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank()) + 
  # geom_pointrange(data = out.e, aes(ymin=(yst-se.yst), ymax=(yst - se.yst))) 
  geom_pointrange(aes(ymin=Mean.nums-SE, ymax=Mean.nums+SE)) #+ 
 # scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))
