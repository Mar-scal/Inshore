
library(ggspatial)
library(ROracle)
library(tidyverse)
library(sf)
library(Polychrome)

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

# Find where tempfiles are stored
temp <- tempfile()
# Download this to the temp directory
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Now read in the shapefiles
mgmt.zones.detailed <- st_read(paste0(temp2, "/Scallop_Strata.shp")) %>% 
  filter(Scal_Area != "SFA29W") %>% 
  st_transform(4326)

bathy_sf <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/bathymetry/bathymetry_15m.shp") %>% 
  st_transform(4326)

Land <- st_read("/vsicurl/https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/Atl_region_land.shp")


# SEASTARS
#uid <- keyring::key_list("Oracle")[1,2]
#pwd <- keyring::key_get("Oracle", uid)

uid <- un.sameotoj
pwd <- pw.sameotoj



##LOAD DESIRED GROUNDFISH Tables using ROracle  ----
channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=uid, password=pwd, "PTRAN")

#Select only type 1 set = random stratified 
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

# Summary of all sets combined 
# Filtered for starfish codes and for the Bay of Fundy (BoF) strata
seastars <- all.spec.md %>%
  filter(SPEC %in% 6000:6310, 
         STRAT %in% 490:495) %>%
  group_by(COMM, SPEC) %>%
  summarise(number_sets = n_distinct(MISSION, SETNO),
            .groups = "drop")

#write.csv(seastars, "RVsurveyStarfishSummaryofSets.csv", row.names = FALSE)
  
# Summary of sets broken down by season and year 
seastar_summary <- all.spec.md %>%
  filter(
    SPEC %in% 6000:6310,
    STRAT %in% 490:495
  ) %>%
  group_by(COMM, SPEC, YEAR, SEASON) %>%
  summarize(number_sets = n_distinct(MISSION, SETNO),
  .groups = "drop")

#write.csv(seastar_summary, "RVsurveyStarfishSummaryofSets_ByYearAndSeason.csv", row.names = FALSE)

# PLOT NUMBER OF SETS BY YEAR AND SEASON
#Looks like species were not distinguished until 2007. Pre 2007, sea stars were recoded as the broad classification Asteroidea S.C.

sstar.plot.1 <- ggplot(seastar_summary, aes(x = factor(YEAR), y = number_sets, fill = factor(COMM))) +
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
sstar.plot.1

#ggsave(plot = sstar.plot.1, "Y:/Projects/InshoreScallop_Predators/SeaStars_BoF/Figures/Seastars_in_RVsurvey_sets.png", scale = 5, width = 9, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#Just ASTEROIDEA Species
aster.plot.1 <- ggplot(seastar_summary %>% filter(SPEC %in% c(6109:6111)), aes(x = factor(YEAR), y = number_sets, fill = factor(COMM))) +
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
aster.plot.1

#ggsave(plot = aster.plot.1, "Y:/Projects/InshoreScallop_Predators/SeaStars_BoF/Figures/Asterias_sp_in_RVsurvey_sets.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)


####################################################################################

#Look at SPEC 6100, 6109:6111 (ASTEROIDEA Species)

#star.fish %>% filter(SPEC == 6105,
                     #STRAT %in% 490:495)
aster <- all.spec.md %>% filter(SPEC %in% c(6109:6111),
                    STRAT %in% 490:495)

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

#Summer Numbers - faceted by Species:
aster.sum.byspec <- aster.std %>% filter(SEASON == "SUMMER") %>% 
  group_by(YEAR, COMM) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD))

aster.plot.2 <- ggplot(aster.sum.byspec, aes(x = YEAR, y = AVG_ABUNDANCE_STD)) +
  stat_summary(geom = "line", fun = mean, linewidth = 0.5) +
  stat_summary(geom = "point", fun = mean, size = 1) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  labs(x = "Year", y = "Mean number per tow - Asteroidea sp.", title = "Summer") +
  facet_wrap(~COMM)
aster.plot.2
#Large spike in Asterias frobesi in 2020, and then none recorded in the BoF for the following years?
#Large spike in Aterias rubens in 2023, and then steep decline the following year.
#Pretty clear pattern of ups and down every year.

#ggsave(plot = aster.plot.2, "Y:/Projects/InshoreScallop_Predators/SeaStars_BoF/Figures/Asterias_sp_Summer_Abundance.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#Spring Numbers - faceted by Species:
aster.spri.byspec <- aster.std %>% filter(SEASON == "SPRING") %>% 
  group_by(YEAR, COMM) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD))


aster.plot.3 <- ggplot(aster.spri.byspec, aes(x = YEAR, y = AVG_ABUNDANCE_STD)) +
  stat_summary(geom = "line", fun = mean, size = 0.5) +
  stat_summary(geom = "point", fun = mean, size = 1) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  labs(x = "Year", y = "Mean number per tow - Asteroidea sp.", title = "Spring") +
  facet_wrap(~COMM)
aster.plot.3

#ggsave(plot = aster.plot.3, "Y:/Projects/InshoreScallop_Predators/SeaStars_BoF/Figures/Asterias_sp_Spring_Abundance.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#Both Spring and Summer Numbers - faceted by Species:
aster.byspec <- aster.std %>% 
  group_by(YEAR, COMM) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD))


aster.plot.4 <-ggplot(aster.byspec, aes(x = YEAR, y = AVG_ABUNDANCE_STD)) +
  stat_summary(geom = "line", fun = mean, size = 0.5) +
  stat_summary(geom = "point", fun = mean, size = 1) +
  #stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
  labs(x = "Year", y = "Mean number per tow - Asteroidea sp.", title = "Spring and Summer") +
  facet_wrap(~COMM)
aster.plot.4

#ggsave(plot = aster.plot.4, "Y:/Projects/InshoreScallop_Predators/SeaStars_BoF/Figures/Asterias_sp_Spring_and_Summer_combined_Abundance.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

#all Asteria species (Forbesi, rubens and asteria sp.)
aster.all <- aster.std %>% 
  group_by(YEAR) %>% 
  summarize(TOT_ABUNDANCE_STD = sum(ABUNDANCE_STD),
            AVG_ABUNDANCE_STD = mean(ABUNDANCE_STD),
            TOT_BIOMASS_STD = sum(BIOMASS_STD),
            AVG_BIOMASS_STD = mean(BIOMASS_STD))

coeff <- 10^2
options(scipen = 999)
NUM.WGT.plot <- ggplot(data = aster.all, aes (x = YEAR)) + 
  geom_line(data = aster.all, aes(y = AVG_BIOMASS_STD), colour = "black") +
  geom_line(data = aster.all, aes(y = AVG_ABUNDANCE_STD/coeff), colour = "grey52", linetype = "dashed") +
  scale_y_continuous(name = "mean biomass per tow",
                     sec.axis = sec_axis(~.*coeff, name = "mean no. per tow"))+
  scale_x_continuous(breaks=seq(min(aster.all$YEAR),max(aster.all$YEAR), 2))+
  theme_bw()+
  theme(axis.title.y.right = element_text(color = "grey52"))+
  labs(title = "All Asteria species - Spring and Summer")+
  xlab("Year")
NUM.WGT.plot
options(scipen = 0)

#ggsave(plot = NUM.WGT.plot, "Y:/Projects/InshoreScallop_Predators/SeaStars_BoF/Figures/Asterias_sp_avg_wgt_and_num_spring_summer_comb.png", scale = 2.5, width = 8, height = 5, dpi = 300, units = "cm", limitsize = TRUE)

########## SPATIAL PLOTS ##########
aster.sf <- aster.std %>% 
  mutate(lat = convert.dd.dddd(SLAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(SLONG)) %>% #Convert to DD
  mutate(lon = lon*-1) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  mutate(lat = convert.dd.dddd(SLAT)) %>% #Convert to DD
  mutate(lon = convert.dd.dddd(SLONG)) %>% 
  mutate(lon = lon*-1)


#define for plotting ****MAY NEED TO ADJUST INTERVALS****
labels <- c("0", "1-99", "100-499", "500-699", "700-899", ">900")
lvls <- c(-Inf, 0, 99, 499, 699, 899, Inf) #

aster.sf <- aster.sf %>% 
  mutate(brk = cut(ABUNDANCE_STD, breaks = lvls, labels = labels, include.lowest = TRUE))

aster.sf.plot <- ggplot() + #Plot survey data and format figure.
  #geom_sf(data = bathy_sf, color = "steelblue", alpha = 0.1, size = 0.5) +
  geom_sf(data = mgmt.zones.detailed, color = "grey40", fill = NA, linewidth = 0.3, linetype = "solid") +
  geom_sf(data = Land, fill = "grey60") +
  geom_spatial_point(data = aster.sf, aes(lon, lat, size = brk, shape = brk, colour = SEASON)) +
  scale_shape_manual("Number of Asteria sp. \n per set",values = c(4, 19, 19, 19, 19, 19), labels =labels)+ #Formatting the shape
  scale_size_manual("Number of Asteria sp. \n per set", values = c(2, 2, 4, 6, 8, 10), labels = labels)+
  facet_wrap(~YEAR)+
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
aster.sf.plot


#ggsave(plot = aster.sf.plot, "Y:/Projects/InshoreScallop_Predators/SeaStars_BoF/Figures/Asterias_sp_Spatial_by_year.png", scale = 3.5, width = 8, height = 8, dpi = 300, units = "cm", limitsize = TRUE)
