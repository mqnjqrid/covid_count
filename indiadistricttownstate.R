library(ggplot2)
library(sf)
library(lwgeom)
library(stringr)
library(geojsonio)
library(geojson)

url = "https://raw.githubusercontent.com/covid19india/covid19india-react/master/public/maps/india.json"
indiadistrict<- geojson_sf(topo2geo(url))
indiadistrict <- indiadistrict[,!(names(indiadistrict) %in% c("id"))]
colnames(indiadistrict)[1:5] = toupper(colnames(indiadistrict))[1:5]
indiadistrict <- st_transform(indiadistrict, "+proj=longlat +ellps=WGS84 +datum=WGS84")
#indiadistrict$ST_NM <- indiadistrict$ST_NM %>% str_replace_all(c("Orissa" = "Odisha", "Andaman & Nicobar Island" = "Andaman and Nicobar Islands", "Jammu & Kashmir" = "Jammu and Kashmir", " & " = " and ", "Arunanchal" = "Arunachal", "NCT of Delhi" = "Delhi", 
#                                                                 "Daman and Diu" = "Dadra and Nagar Haveli and Daman and Diu"))
#indiadistrict$ST_NM[indiadistrict$ST_NM == "Dadara and Nagar Havelli"] = "Dadra and Nagar Haveli and Daman and Diu"
indiadistrict$DISTRICT <- indiadistrict$DISTRICT %>% str_replace_all(c("Andaman & Nicobar Island" = "Andaman and Nicobar Islands", " & " = " and ", "Purba |Purbi " = "East ", "Paschim |Paschimi |Pashchim |Pashchimi " = "West "))
indiadistrict$DISTRICT[indiadistrict$DISTRICT %in% c("East", "West", "North", "South", "North East", "North West")] <- paste(indiadistrict$DISTRICT, indiadistrict$ST_NM, sep = ' ')[indiadistrict$DISTRICT %in% c("East", "West", "North", "South", "North East", "North West")]
indiadistrict$ST_NM[indiadistrict$DISTRICT %in% c("Adilabad", "Nizamabad", "Karimnagar", "Medak", "Warangal", "Hyderabad", "Rangareddy", "Mahbubnagar", "Nalgonda", "Khammam")] = "Telangana"
indiadistrict$ST_NM[indiadistrict$DISTRICT %in% c("Kargil", "Leh (ladakh)")] = "Ladakh"
coordfile = st_coordinates(indiadistrict)
distcoord = aggregate(cbind(X, Y) ~ L3, "mean" , data = coordfile)
distcoord$area = units::set_units(x = st_area(indiadistrict), value = km^2)
distcoord$area <- str_remove(distcoord$area, " [hectare]")
distcoord$DISTRICT = indiadistrict$DISTRICT
distcoord$ST_NM = indiadistrict$ST_NM
distcoord$area = as.numeric(as.character(distcoord$area))
distcoord = distcoord[,-1]

indiastate <- st_read("C:/Users/manja/Downloads/maps-master/States/Admin2.shp", stringsAsFactors = FALSE, quiet = TRUE)
indiastate <- st_transform(indiastate, "+proj=longlat +ellps=WGS84 +datum=WGS84")
indiastate$ST_NM <- indiastate$ST_NM %>% str_replace_all(c("Orissa" = "Odisha", "Andaman & Nicobar" = "Andaman and Nicobar Islands", "Jammu & Kashmir" = "Jammu and Kashmir"))
coordfile = indiastate %>% st_coordinates()
statecoord = aggregate(cbind(X, Y) ~ L3, "mean" , data = coordfile)
statecoord$area = units::set_units(x = st_area(indiastate), value = km^2)
statecoord$area <- str_remove(statecoord$area, " [hectare]")
statecoord$ST_NM = indiastate$ST_NM
statecoord = statecoord[,-1]

indiatown <- st_read("C:/Users/manja/Downloads/places/places.shp", stringsAsFactors = FALSE, quiet = TRUE)
# indiatown$name <- indiatown$name %>% str_to_title() %>%
#   str_replace_all(c("Kanchipuram" == "Kancheepuram",
#   "Chikkaballapur" == "Chikkaballapura",
#   "Raj Nandgaon" == "Rajnandgaon",
#   "Ahmed Nagar" == "Ahmadnagar",
#   "Alipur Duar" == "Alipurduar",
#   "Jhargaon" == "Jhargram"
#   ))
indiatown$name[indiatown$name == "Kanchipuram"] = "Kancheepuram"
indiatown$name[indiatown$name == "Alipur Duar"] = "Alipurduar"
indiatown$name[indiatown$name == "Chikkaballapur"] = "Chikkaballapura"
indiatown$name[indiatown$name == "Raj Nandgaon"] = "Rajnandgaon"
indiatown$name[indiatown$name == "Ahmed Nagar"] = "Ahmednagar"
indiatown$name[indiatown$name == "Jhargaon"] = "Jhargram"

towncoord = as.data.frame(st_coordinates(indiatown))
towncoord$tname = indiatown$name
towncoord <- towncoord[!is.na(towncoord$tname),]

rm(list = c("coordfile", "url"))
