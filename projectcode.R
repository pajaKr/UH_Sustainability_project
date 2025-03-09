# Sustainability in Computer and Data Sciences 2025
# Project work
# Pavlína Křenková, Maarja Mustimets, Dominik Répás, Helen Tirkkonen

# Necessary packages
library(dplyr)
library(readxl)
library(terra) # for doing geo-calculations
library(gpkg) # for reading geopackages (.gpkg)
library(tidyterra)
library(reshape2) # for colsplit()

# Read in the data
folder <- "/home/user/Documents/UH Master's in Data Science/2024-3/Project/data/"


#*--- District data ------------------------------------------------------------
# First goal: I want to calculate the center of all the district polygons
?centroids # I need a SpatVector input 
?buffer # also SpatVector or SpatRaster input

# Data:
data_district <- geopackage(paste0(folder, "Helsinki map service_osaalueet_WFS_20250304_160753/osaalueet_WFS.gpkg"), connect = TRUE)
gpkg_list_tables(data_district)
data_district_sub <- gpkg_table(data_district, "osaalueet_WFS", collect = TRUE)
head(data_district_sub)

gpkg_tables(data_district, collect = TRUE) # gpkg_tables lists all the content tables/geometries/relevant stuff it seems
  # class of osaalueet_WFS: SpatVector! but actually this is a data.frame, and cannot be used like a SpatVector:
#centroids(data_district_sub)

# I have to read it in as a vector:
districts_vect <- gpkg_vect(data_district, "osaalueet_WFS")
districts_vect
centroids(districts_vect)

plot(districts_vect, las=1)
plot(districts_vect, border='blue', col='yellow', lwd=2, add=TRUE)
points(centroids(districts_vect), col='red', pch=20, cex=1)
# The big areas are actually sea, probably need to delete those later

# Also, I assume that this district data is "current"? On the map page it says:
# Ajantasaisuuspäivämäärä:	2025-01-02
# Aineiston päivityspäivämäärä: jatkuva ylläpito
# Vakioitu tuloste: Painettu Helsingin Kiinteistökartta (uusin vuodelta 2013)
# So it has been stable since 2013 at least?


#*--- Income data --------------------------------------------------------------
data_income <- read_xlsx(paste0(folder, "Helsingin alueen valtionveronalaisten tulojen mukaan osa-aluettain 2010-.xlsx"))
# comes in a really weird shape, let's fix it
income <- data.frame()
for (i in 2010:2022) {
  columnr_start <- 2 + (i-2009-1)*2 + 1
  print(columnr_start)
  tmp <- data_income[4:378, c(1, columnr_start:(columnr_start+1))] %>% 
    cbind(as.data.frame(rep(i, 378-3)))
  names(tmp) <- c("Area","Population 31.12","Mean taxable income (euros)","Year")
  income <- rbind(income, tmp)
}
# should separate the Area code and name, like in the district data
income <- cbind(income, colsplit(income$Area, " ", (c("kunta","tunnus","name_fi")))) %>%
  filter(kunta==91 & name_fi!="") %>% 
  mutate(tunnus = ifelse(nchar(tunnus)==2, paste0("0",tunnus), tunnus)) # something to help restore the lost zeroes
str(income)
# change the data types for numeric variables
income <- income %>%
  mutate(`Mean taxable income (euros)` = ifelse(`Mean taxable income (euros)`=="..", NA, as.double(`Mean taxable income (euros)`)),
         `Population 31.12` = as.integer(`Population 31.12`)) # sends NA warning, but no NAs are actually made, so its ok
#write.csv(income, paste0(folder,"income data_processed.csv"))

# Are there areas with very few people, so income statistics cannot be published?
income %>% arrange(`Population 31.12`) %>% head() # absolutely
income %>% arrange(`Population 31.12`) %>% View()
  # It seems like almost all records where the population is under 100 people has been not published
  # It is a little inconsistent for some reason, but I consider 100 a good cut-off
# Marking all records with <100 population
income <- income %>% mutate(Lowpop = ifelse(`Population 31.12`>=100, 0, 1))
table(income$Year, income$Lowpop, useNA="i") 
  # as expected, populations have grown with time (so we were left with more records from later years)
income %>% group_by(Year, Lowpop) %>% 
  summarise(`Mean (Mean taxable income)` = mean(`Mean taxable income (euros)`),
            `Total (Population)` = sum(`Population 31.12`))
  # We are missing out on only around 300 people each year


#*--- How does the data on the districts and income line up? -------------------
districts <- gpkg_table(data_district, "osaalueet_WFS", collect = TRUE) %>% select(tunnus) %>% unlist()
table(districts %in% income$tunnus, useNA="i")
  # FALSE  TRUE 
  #     8   140
districts[!(districts %in% income$tunnus)]
data_district_sub[!(data_district_sub$tunnus %in% income$tunnus), c("kunta","tunnus","nimi_fi")] 
  # these are missing from income data ...but they are not residential areas, or have such a low pop that stats cannot be published
# I remove these districts from the list and also the SpatVector object:
districts_vect <- subset(districts_vect, districts_vect$tunnus %in% income$tunnus)
districts_vect
# We are left with 140 districts

plot(districts_vect, las=1)
plot(districts_vect, border='blue', col='yellow', lwd=2, add=TRUE)
points(centroids(districts_vect), col='red', pch=20, cex=1)
# Still some big coastal areas, but they all have income data now

plot(districts_vect, las=1)
plot(districts_vect, border='blue', col='yellow', lwd=2, add=TRUE)
points(centroids(districts_vect), col='red', pch=20, cex=1)
plot(buffer(centroids(districts_vect), width = 1000), border='red', lwd=2, add=TRUE)
# As the districts are very different in size, 
  # the buffer around the centroid may or may not cover the district
# What if I do not use a centroid, just the district polygon?
plot(districts_vect, las=1)
plot(districts_vect, border='blue', col='yellow', lwd=2, add=TRUE)
points(centroids(districts_vect), col='red', pch=20, cex=1)
plot(buffer(districts_vect, width = 500), border='red', lwd=2, add=TRUE)

# I probably have to experiment to find the best solution


#*--- Green area data: zoned green areas ---------------------------------------
# "Register of public areas in the City of Helsinki"
# Aineistoa ylläpidetään toimialan käyttöön eikä se pääsääntöisesti kata muiden Helsingin 
# kaupungin hallintokuntien tai laitosten vastuulla olevien yleisten alueiden omaisuuseriä."
  # So does not cover property managed by district governments or other public services (roughly translated)
# Julkaisuajankohta: 11.8.2014
data_zonedgreen <- geopackage(paste0(folder, "Helsinki map service_Zoned green areas/Ylre_viheralue_asemakaavoitettu.gpkg"), connect = TRUE)
gpkg_list_tables(data_zonedgreen)
gpkg_tables(data_zonedgreen, collect = TRUE)

zonedgreen_vect <- gpkg_vect(data_zonedgreen, "Ylre_viheralue_asemakaavoitettu")
table(is.valid(zonedgreen_vect), useNA="i") # check for validity
names(zonedgreen_vect)
table(zonedgreen_vect$rakentamisen_tila)
  # Seems like some of these might not have "been built" yet at the time of publishing?

plot(zonedgreen_vect, las=1, border='darkgreen', col='darkgreen')
plot(districts_vect, las=1, add=TRUE)

#*--- Green area data: Green finger --------------------------------------------
# "Helsingin tavoitteellinen viher- ja virkistysverkostokartta" / "VISTRA"
# "ne on esitetty vahvistamattoman v. 2016 suunnittelutilanteen mukaisesti"
  # This is green areas as planned, maybe not as they currently exist.
  # HOWEVER, this covers many green areas that are not covered by the register of public areas,
  # Such as the whole Viikki/Lammassaari bay area and many others
  # So I think it is a good supplement
# Aineiston julkaisupäivämäärä:	6.10.2016
data_greenfinger <- geopackage(paste0(folder, "Helsinki map service_Green finger/MAKA_vistra_21_McVihersormi_kp.gpkg"), connect = TRUE)
gpkg_list_tables(data_greenfinger)
gpkg_tables(data_greenfinger, collect = TRUE)

greenfinger_vect <- gpkg_vect(data_greenfinger, "MAKA_vistra_21_McVihersormi_kp")
table(is.valid(greenfinger_vect), useNA="i") # check for validity
greenfinger_vect <- makeValid(greenfinger_vect) # fix
names(greenfinger_vect) # no variable for area
greenfinger_vect$viheralueen_pa <- expanse(greenfinger_vect) # creating it
summary(greenfinger_vect$viheralueen_pa)

plot(greenfinger_vect, las=1, border='darkgreen', col='darkgreen')
plot(districts_vect, las=1, add=TRUE)

#*--- Green area data: combined (both from Helsinki map service) ---------------
plot(districts_vect, las=1)
plot(greenfinger_vect, las=1, border='darkgreen', col='darkgreen', add=TRUE)
plot(zonedgreen_vect, las=1, border='darkgreen', col='darkgreen', add=TRUE)
plot(districts_vect, las=1, add=TRUE)
# Seems like there isn't green finger data for the districts in the north-east
  # might want to drop those for the analysis for consistency
text(districts_vect, "nimi_fi", cex=.8, halo=TRUE)
# So I need to drop Puroniitty, Landbo, Östersundom, Karhusaari, Talosaari and Salmenkallio
  # Just from the income-greenery calculation or also just the greenery data?
  # The parks data is quite far off from other districts, so I would also remove it from the greenery data
  # But the greenery will not be in any buffer zone anyway, so whatever
districts_vect <- terra::subset(districts_vect, !districts_vect$nimi_fi %in% c("Puroniitty","Landbo","Östersundom","Karhusaari","Talosaari","Salmenkallio"))
# 134 districts left

names(greenfinger_vect)
names(zonedgreen_vect)

helsinki_green <- rbind(zonedgreen_vect, greenfinger_vect)
names(helsinki_green)
plot(helsinki_green, las=1, border='darkgreen', col='darkgreen')
plot(districts_vect, las=1, add=TRUE)


#*--- Green area analysis ------------------------------------------------------
?zonal
districts_vect$viheralueen_pa <- zonal(helsinki_green, districts_vect)$viheralueen_pa
summary(districts_vect$viheralueen_pa)
districts_vect$alueen_pa <- expanse(districts_vect)
summary(districts_vect$alueen_pa)

districts_vect$alue_pa_ratio <- districts_vect$viheralueen_pa/districts_vect$alueen_pa
summary(districts_vect$alue_pa_ratio)

grid = rast(districts_vect, nrow=1000, ncol=1000)
districts_rast = rasterize(districts_vect, grid, field="alue_pa_ratio")

plot(districts_rast, lwd=2, ext=c(25490000, 25510000, 6669000, 6687000))
plot(districts_vect, las=1, add=TRUE)


# With a buffer around the district:
districts_vect_greenbuffer <- buffer(districts_vect, width = 500)
districts_vect$viheralueen_pa_greenbuffer <- zonal(helsinki_green, districts_vect_greenbuffer)$viheralueen_pa
districts_vect$alue_pa_ratio_greenbuffer <- districts_vect$viheralueen_pa_greenbuffer/districts_vect$alueen_pa
summary(districts_vect$alue_pa_ratio_greenbuffer)

districts_rast = rasterize(districts_vect, grid, field="alue_pa_ratio_greenbuffer")
plot(districts_rast, lwd=2, ext=c(25490000, 25510000, 6669000, 6687000))
plot(districts_vect, las=1, add=TRUE) # the plot is not super interesting, but the colors are a bit evened out


#*--- Income analysis ----------------------------------------------------------
# What year to analyse for?
# I would say 2016, which is the green finger data publishing time 
  # Also maybe the 2014 "rakentaminen kesken" have been finished? 
  # Then again the green finger is also "tavoitteellinen"
# Income data is for many years and districts seem to have been stable since at least 2013
income_2016 <- income %>% filter(Year==2016 & Lowpop==0) %>%
  group_by(tunnus) %>%
  summarise(`Population 31.12` = sum(`Population 31.12`),
            `Mean taxable income (euros)` = mean(`Mean taxable income (euros)`))
districts_vect2 <- left_join(districts_vect, income_2016, by="tunnus", suffix=c("","_2016"))
names(districts_vect2)

maxincome2016 <- max(income_2016$`Mean taxable income (euros)`)
summary(districts_vect2$`Mean taxable income (euros)`)
districts_vect2 <- districts_vect2 %>%
  mutate(`Mean taxable income (euros)` = ifelse(is.na(`Mean taxable income (euros)`), 0, `Mean taxable income (euros)`),
         viheralueen_pa_perpop = viheralueen_pa/`Population 31.12`,
         viheralueen_pa_greenbuffer_perpop = viheralueen_pa_greenbuffer/`Population 31.12`,
         income_q = `Mean taxable income (euros)`/ maxincome2016,
         viheralueen_pa_incomeq = alue_pa_ratio*income_q, # idk if this means anything, rethink maybe
         viheralueen_pa_greenbuffer_incomeq = alue_pa_ratio_greenbuffer*income_q) # idk if this means anything
summary(districts_vect2$income_q)

districts_rast = rasterize(districts_vect2, grid, field="viheralueen_pa_perpop")
plot(districts_rast, lwd=2, ext=c(25490000, 25510000, 6669000, 6687000))
plot(districts_vect, las=1, add=TRUE) # the plot is not super interesting, but the colors are a bit evened out
