#stratified estimates for cancer sp in BoF RV Survey strata - using PEDStrata function - need to account for survey strata change from pre 2014 - which would add a domain estimator - not done yet TBDONE

library(ROracle)
library(dplyr)
library(tidyr)
library(pals)
library(sf)
library(ggplot2)
library(ggspatial)
library(Polychrome)
library(PEDstrata)


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

BoF.RVsurveystrata <- strata %>% filter(StrataID %in% 490:495)

#map of strata areas being used 
ggplot(ns_proj) + geom_sf(data = BoF.RVsurveystrata, aes(fill = StrataID),  colour = NA)

#ORACLE Credentials

#uid <- keyring::key_list("Oracle")[1,2]
#pwd <- keyring::key_get("Oracle", uid)

uid <- un.sameotoj
pwd <- pw.sameotoj


##LOAD DESIRED GROUNDFISH Tables using ROracle  ----
channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=uid, password=pwd, "PTRAN")

gsinf <- ROracle::dbGetQuery(channel, paste("select MISSION, DUR, SDATE, SETNO, STRAT, SLAT, SLONG, ELAT, ELONG, DIST, GEAR from groundfish.gsinf where type =1"))

gscat <- ROracle::dbGetQuery(channel, paste ("select MISSION, SETNO, SPEC, TOTWGT, TOTNO from groundfish.gscat"))

gsmissions<-ROracle::dbGetQuery(channel, paste("select MISSION, YEAR, SEASON, VESEL from groundfish.gsmissions"))

#MERGING TO GET YEAR 
gsinf<- merge(gsinf,gsmissions,by="MISSION")


#Removing the bad tows from the tow length/distance QC
bad_sets <- tibble::tribble(
  ~MISSION,     ~SETNO,
  "NED2005034", 50,
  "NED2013022", 48,
  "NED2013022", 36
)

# TAKING OUT THE BAD TOWS AND FILTERING FOR 2005 UP & FOR STRAT IN BoF (490-495)
# IN THE SURVEY HISTORY TECH REPORT (CLARK AND CLARK - IN PRESS) IT MENTIONS THAT PRIOR TO 2005, JONAH CRAB AND ATLANTIC ROCK CRAB MAY HAVE BEEN CONFUSED. 
# BOTH SPECIES WERE ADDED TO THE STANDARD SAMPLING PROTOCOL FOR THE SPRING SURVEY IN 2009 
# JONAH CRAB WAS ADDED TO THE SUMMER SURVEY SAMPLING PROTOCOL IN 1999 
# DUE TO THE POTENTIAL FOR MISIDENTIFICATION, DATA IS PULLED FOR 2005 ONWARDS; IT IS SAID THAT THE DATA IS GENERALLY OF GOOD QUALITY SINCE ABOUT 2005 (CLARK AND CLARK - IN PRESS)
gsinf <- gsinf %>%
  anti_join(bad_sets, by = c("MISSION", "SETNO")) %>%
  filter(YEAR >= 2005, 
         STRAT %in% c(490:495))



# JOIN GSINF AND GSCAT TO INCLUDE TOTWGT AND TOTNO

LF_Data <- gsinf %>%
  left_join(gscat, by = c("MISSION", "SETNO")) %>% 
  select(-ends_with(".cat"))


# GET TOTAL WEIGHTS AMD TOTAL NUMBER BY TOW AND STANDARDIZE BY DISTANCE TOWED 
# IF ELSE FUNCTION - IF VESSEL IS CARTIER OR CABOT ("J" OR "B") use 1/Dist) if any other vessel, use 1.75

Survey_Data <- LF_Data %>%
  group_by(MISSION,SETNO, YEAR, SEASON, SPEC, STRAT, DIST, DUR, SLAT, SLONG, ELAT, ELONG, GEAR, VESEL) %>%  
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

#Separate Cancer sp.
Cancer.sp.sets <- Survey_Data %>%
  filter(SPEC %in% c(2511, 2513), 
         STRAT %in% 490:495) %>%
  group_by(SPEC) %>%
  summarise(number_sets = n_distinct(MISSION, SETNO),
            .groups = "drop") %>% 
  mutate(COMM = case_when(SPEC == 2511 ~ "JONAH CRAB",
                          SPEC == 2513 ~ "ATLANTIC ROCK CRAB"))
Cancer.sp.sets


#Numbers per tow:
#Add common name:
Cancer.sp <- Survey_Data %>% filter(SPEC %in% c(2511,2513),
                                    STRAT %in% 490:495) %>% 
  mutate(COMM = case_when(SPEC == 2511 ~ "JONAH CRAB",
                          SPEC == 2513 ~ "ATLANTIC ROCK CRAB"))

#Check years of data to see if need to make expections for time series (i.e. fill in a year with NA)
table(Cancer.sp$YEAR)

#Stratified estimate for subset of strata in BoF for sea stars 
years <- min(Cancer.sp$YEAR):max(Cancer.sp$YEAR) # Runs to the most recent year by default 
years
X <- length(years)
X

strat.est <- data.frame(Year=years, Mean.nums=rep(NA,X), Pop=rep(NA,X), SE=rep(NA,X), method=rep("PED",X), Area=rep("490:495", X), Age=rep("CancerSp.", X))
for(i in 1:length(strat.est$Year)){
  temp.data <- Cancer.sp[Cancer.sp$YEAR==(min(years)-1)+i,]
  strat.est[i,2] <- summary(PEDstrata(temp.data, RVsurvey.strata.table, "STRAT", catch=temp.data$BIOMASS_STD, Subset=temp.data$SEASON=="SUMMER"))$yst #going into mean field 
  strat.est[i,3] <- summary(PEDstrata(temp.data, RVsurvey.strata.table, "STRAT", catch=temp.data$BIOMASS_STD, Subset=temp.data$SEASON=="SUMMER"))$Yst #going into population field 
  strat.est[i,4] <- summary(PEDstrata(temp.data, RVsurvey.strata.table, "STRAT", catch=temp.data$BIOMASS_STD, Subset=temp.data$SEASON=="SUMMER"))$se.yst
}
strat.est

head(strat.est)

ggplot(data = strat.est, aes(x=Year , y=Mean.nums)) + 
  geom_point() + 
  geom_line() + 
  # scale_x_continuous(limits = c(2001, (surveyyear+1)), breaks = seq(2001, (surveyyear+1), by = 4)) +
  theme_bw() + ylab("RV Summer survey mean weight (kg) \n per standardize tow") + xlab("Year") + 
  theme(legend.position = c(0.1, 0.9),panel.grid.minor = element_blank()) + 
  # geom_pointrange(data = out.e, aes(ymin=(yst-se.yst), ymax=(yst - se.yst))) 
  geom_pointrange(aes(ymin=Mean.nums-SE, ymax=Mean.nums+SE)) #+ 
# scale_x_continuous(limits = c(2000, (surveyyear+1)), breaks = seq(2002, (surveyyear+1), by = 4))

# Summary of sets broken down by season and year 
Cancer.summary <- Survey_Data %>%
  filter(
    SPEC %in% c(2511, 2513), 
    STRAT %in% 490:495) %>%
  group_by(SPEC, YEAR, SEASON) %>%
  summarize(number_sets = n_distinct(MISSION, SETNO),
            .groups = "drop") %>% 
  mutate(COMM = case_when(SPEC == 2511 ~ "JONAH CRAB",
                          SPEC == 2513 ~ "ATLANTIC ROCK CRAB"))
Cancer.summary


# Plots -------------------------------------------------------------------

# PLOT NUMBER OF SETS BY YEAR AND SEASON
#Looks like species were not distinguished until 2007. Pre 2007, sea stars were recoded as the broad classification Asteroidea S.C.

crab.plot.1 <- ggplot(Cancer.summary, aes(x = factor(YEAR), y = number_sets, fill = factor(COMM))) +
  geom_col() +
  scale_fill_manual(values = as.vector(palette36.colors(30))) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Number of Sets",
    fill = "Species") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~SEASON)
crab.plot.1

#ggsave(plot = crab.plot.1, "Y:/Projects/InshoreScallop_Predators/Crab_BoF/Figures/Cancer_sp_in_RVsurvey_sets.png", scale = 5, width = 9, height = 5, dpi = 300, units = "cm", limitsize = TRUE)



#Summer Numbers - faceted by Species:
Cancer.sum.byspec <- Cancer.sp %>% filter(SEASON == "SUMMER") %>% 
  group_by(YEAR, COMM) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD))


crab.plot.2 <- ggplot(Cancer.sum.byspec, aes(x = YEAR, y = AVG_ABUNDANCE_STD)) +
  stat_summary(geom = "line", fun = mean, linewidth = 0.5) +
  stat_summary(geom = "point", fun = mean, size = 1) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  labs(x = "Year", y = "Mean number per tow - Cancer sp.", title = "Summer") +
  facet_wrap(~COMM)
crab.plot.2

#ggsave(plot = crab.plot.2, "Y:/Projects/InshoreScallop_Predators/Crab_BoF/Figures/Cancer_sp_Summer_Abundance.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#Spring Numbers - faceted by Species:
Cancer.spri.byspec <- Cancer.sp %>% filter(SEASON == "SPRING") %>% 
  group_by(YEAR, COMM) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD))


crab.plot.3 <- ggplot(Cancer.spri.byspec, aes(x = YEAR, y = AVG_ABUNDANCE_STD)) +
  stat_summary(geom = "line", fun = mean, size = 0.5) +
  stat_summary(geom = "point", fun = mean, size = 1) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  labs(x = "Year", y = "Mean number per tow - Cancer sp.", title = "Spring") +
  facet_wrap(~COMM)
crab.plot.3

#ggsave(plot = crab.plot.3, "Y:/Projects/InshoreScallop_Predators/Crab_BoF/Figures/Cancer_sp_Spring_Abundance.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#Both Spring and Summer Numbers - faceted by Species:
Cancer.byspec <- Cancer.sp %>% 
  group_by(YEAR, COMM) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD))


crab.plot.4 <- ggplot(Cancer.byspec, aes(x = YEAR, y = AVG_ABUNDANCE_STD)) +
  stat_summary(geom = "line", fun = mean, size = 0.5) +
  stat_summary(geom = "point", fun = mean, size = 1) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  labs(x = "Year", y = "Mean number per tow - Cancer sp.", title = "Spring and Summer") +
  facet_wrap(~COMM)
crab.plot.4

#ggsave(plot = crab.plot.4, "Y:/Projects/InshoreScallop_Predators/Crab_BoF/Figures/Cancer_sp_Spring_and_Summer_comb_Abundance.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#all Asteria species (Forbesi, rubens and asteria sp.)
Cancer.all <- Cancer.sp %>% 
  group_by(YEAR) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD),
            TOT_BIOMASS_STD = sum(BIOMASS_STD),
            AVG_BIOMASS_STD = mean(BIOMASS_STD))

coeff <- 10
options(scipen = 999)
NUM.WGT.plot <- ggplot(data = Cancer.all, aes (x = YEAR)) + 
  geom_line(data = Cancer.all, aes(y = AVG_BIOMASS_STD), colour = "black") +
  geom_line(data = Cancer.all, aes(y = AVG_ABUNDANCE_STD/coeff), colour = "grey52", linetype = "dashed") +
  scale_y_continuous(name = "Mean biomass per year",
                     sec.axis = sec_axis(~.*coeff, name = "Mean abundance per year"))+
  scale_x_continuous(breaks=seq(min(Cancer.all$YEAR),max(Cancer.all$YEAR), 2))+
  theme_bw()+
  theme(axis.title.y.right = element_text(color = "grey52"))+
  labs(title = "All Cancer species - Spring and Summer")+
  xlab("Year")
NUM.WGT.plot
options(scipen = 0)

#ggsave(plot = NUM.WGT.plot, "Y:/Projects/InshoreScallop_Predators/Crab_BoF/Figures/Cancer_sp_avg_wgt_and_num_spring_summer_comb.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)


########## SPATIAL PLOTS ##########
Cancer.sf <- Cancer.sp %>% 
  mutate(lat = convert.dd.dddd(SLAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(SLONG)) %>% #Convert to DD
  mutate(lon = lon*-1) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  mutate(lat = convert.dd.dddd(SLAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(SLONG)) %>% 
  mutate(lon = lon*-1)


#define for plotting ****MAY NEED TO ADJUST INTERVALS****
labels <- c("0", "1-99", "100-299", "300-499", ">500")
lvls <- c(-Inf, 0, 99, 299,499, Inf) #max(Cancer.sf$ABUNDANCE_STD)

Cancer.sf <- Cancer.sf %>% 
  mutate(brk = cut(ABUNDANCE_STD, breaks = lvls, labels = labels, include.lowest = TRUE))

Cancer.sf.plot <- ggplot() + #Plot survey data and format figure.
  #geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
  geom_sf(data = mgmt.zones.detailed, color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid") +
  geom_sf(data = Land, fill = "grey60") +
  geom_spatial_point(data = Cancer.sf, aes(lon, lat, size = brk, shape = brk, colour = SEASON)) +
  scale_shape_manual("Number of Cancer sp. \n per set",values = c(4, 19, 19, 19, 19), labels =labels)+ #Formatting the shape
  scale_size_manual("Number of Cancer sp. \n per set", values = c(2, 2, 4, 6, 8), labels = labels)+
  facet_wrap(~YEAR, ncol = 8)+
  # Formatting the size for each break
  coord_sf(xlim = c(-67.5, -64.5), ylim = c(43.5, 46), expand = TRUE)+
  labs(x = "Longitude",y = "Latitude") +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) + # Custom X and Y axis formatting (easier for french translation)
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  #annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(0.35, "cm"), pad_y = unit(0.35, "cm")) + # Add scale bar with selectable location
  #annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.25, "cm"), width = unit(1, "cm"),
                         #pad_x = unit(0.35, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering) + # Add north arrow with selectable location+
  theme_bw()+
  theme(legend.key.size = unit(6,"mm"),
        plot.title = element_text(size = 14, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 10),
        #legend.position = c(.86,.22), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.8)),
        legend.box.margin = margin(2, 3, 2, 3),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
Cancer.sf.plot

#ggsave(plot = Cancer.sf.plot, "Y:/Projects/InshoreScallop_Predators/Crab_BoF/Figures/Cancer_sp_Spatial_by_year.png", scale = 5, width = 9, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

