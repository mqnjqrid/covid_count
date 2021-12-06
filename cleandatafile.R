library(plyr)
library(dplyr)
library(stringr)

rawdata <- data.frame()
for(fnum in 1:32){
  dr <- read.csv(paste0("https://api.covid19india.org/csv/latest/raw_data", fnum, ".csv"))
  print(dim(dr))
  rawdata <- plyr::rbind.fill(rawdata, dr)
}
  
  rawdata$Date.Announced <- as.Date(as.character(rawdata$Date.Announced), format="%d/%m/%Y")
  rawdata$Status.Change.Date <- as.Date(as.character(rawdata$Status.Change.Date), format="%d/%m/%Y")
  
  rawdata$days <- as.numeric(difftime(rawdata$Date.Announced, min(rawdata$Date.Announced, na.rm = TRUE)), units = c("days"))
  
  rawdata <- rawdata[, !(names(rawdata) %in% c("Estimated.Onset.Date", "Backup.Notes"))]
  rawdata$Age.Bracket <- as.numeric(as.character(rawdata$Age.Bracket))
  
  rawdata$Gender <- rawdata$Gender %>% str_remove_all(" |,|emale") %>% as.factor()
  rawdata$Nationality <- rawdata$Nationality %>% str_to_title() %>% as.factor()
  
#  save(rawdata, file = "C:/Users/manja/Downloads/rawdataapi.Rdata")

  source("C:/Users/manja/Downloads/indiadistricttownstate.R")
  
  load("C:/Users/manja/Downloads/rawdataapi.Rdata")
  rawdata$Current.Status <- str_to_title(rawdata$Current.Status)
  rawdata$Current.Status[grep("Migrated", rawdata$Current.Status)] <- "Migrated"
  
  rawdata$Source_1[is.na(rawdata$Source_1) | is.null(rawdata$Source_1)] <- ""
  rawdata$Source_2[is.na(rawdata$Source_2) | is.null(rawdata$Source_1)] <- ""
  rawdata$Source_3[is.na(rawdata$Source_3) | is.null(rawdata$Source_1)] <- ""
  
  #rawdata <- rawdata %>% filter(str_length(Source_1) + str_length(Source_2) + str_length(Source_3) > 0)

  rawdata$Source_1 <- rawdata$Source_1 %>% str_remove_all("http://|https://|mobile\\.") %>% str_replace_all("t\\.co|[^t]witter.com|\ttwitter.com", "twitter.com")
  rawdata$Source_2 <- rawdata$Source_2 %>% str_remove_all("http://|https://|mobile\\.") %>% str_replace_all("t\\.co|[^t]witter.com|\ttwitter.com", "twitter.com")
  rawdata$Source_3 <- rawdata$Source_3 %>% str_remove_all("http://|https://|mobile\\.") %>% str_replace_all("t\\.co|[^t]witter.com|\ttwitter.com|^witter.com", "twitter.com")
  
  source("C:/Users/manja/Downloads/listcreate.R")
  
  rm(list = c("webpage", "results", "extraofficialsources", "presssources", "othersources", "list1", "list2", "list3", "list4", "list5", "list6", "list7"))
  
  rawdata$Detected.City[is.na(rawdata$Detected.City)] <- ''
  rawdata$Detected.District[is.na(rawdata$Detected.District)] <- ''
  rawdata$Detected.State[is.na(rawdata$Detected.State)] <- ''
  rawdata$Detected.District <- trimws(rawdata$Detected.District) %>% str_to_title() %>% str_replace_all(" And ", " and ")
  ########################################################################
  data <- rawdata[, grepl("Source", names(rawdata)) == FALSE]
  data$Type.of.transmission = str_remove_all(data$Type.of.transmission, " ")
  
  data[tolower(data$Detected.District) %in% setdiff(tolower(data$Detected.District), union('', tolower(indiadistrict$DISTRICT))), c("Detected.District", "Detected.State")] %>% group_by_all() %>% count()
  
  data[grep("Delhi", data$Detected.District),]$Detected.District = "Delhi"
  data[grep("Delhi", data$Detected.City),]$Detected.City = "New Delhi"
  # data[data$Detected.District == "Ayodhya", "Detected.City"] = "Ayodhya"
  # data[data$Detected.District == "Ayodhya", "Detected.District"] = "Faizabad"
  
  table(data[str_length(data$Detected.District) == 0,]$Detected.City)
  table(data[str_length(data$Detected.District) == 0,]$Detected.State)
  
  #data[grepl("Delhi", data$Detected.District) | grepl("Delhi", data$Detected.State),]$Detected.District = "Delhi"
  #data[data$Detected.District == "Mumbai" | data$Detected.City == "Mumbai",]$Detected.District = "Mumbai"
  data$Detected.District <- data$Detected.District %>% str_to_title() %>%
    str_replace_all(c(
      "Saitual" = "Aizawl",
      "Tengunoupal" = "Tengnoupal",
      "Teng$" = "Tengnoupal",
      "Chande$" = "Chandel",
      "Tinsukla" = "Tinsukia",
      "Bokara" = "Bokaro",
      "Gaurela Pendra Marwahi" = "Bilaspur",
      "Kamrup Rural" = "Kamrup",
      "Lmora" = "Almora",
      "Saraikela$" = "Saraikela-Kharsawan",
      "Mayiladuthurai" = "Nagapattinam",
      "Italians" = "",
      "Other State" = "",
      "Other Region" = "",
      "Others" = "",
      "Gamla" = "Gumla",
      "Unknown" = "",
      "Unassigned" = "",
      " \\& " = " and ",
      "Purba |Purbi " = "East ",
      "Paschim |Paschimi |Pashchim |Pashchimi " = "West ",
      "Capf Personnel" = "Bishnupur"#,
#      "Railway Quarantine" = "Chennai"
    )) %>% str_replace_all(c(
      " And " = " and ",
      "Y.s.r" = "Y.S.R",
      "S.a.s" = "S.A.S",
      "S.p.s" = "S.P.S"
    ))
 # data$Detected.District[data$Detected.District == "Airport Quarantine" & data$Detected.State == "Tamil Nadu"] = "Chennai"
  data$Detected.City <- data$Detected.City %>% str_to_title() %>% str_replace_all(c(
    "Khargone" = "Khargon",
    "Jogulamba Gadwal" = "Gadwal",
    "Balasore" = "Baleshwar"
  ))
  
  data$Detected.City[data$Detected.District == "State Pool" & data$Detected.State == "Odisha"] = "Bhuwaneshwar"
  data$Detected.District[data$Detected.District == "State Pool" & data$Detected.State == "Odisha"] = "Khordha"
  
  data$Detected.City[data$Detected.District %in% c("Bsf Camp") & data$Detected.State == "Rajasthan"] = "Jaisalmer"
  data$Detected.District[data$Detected.District %in% c("Bsf Camp") & data$Detected.State == "Rajasthan"] = "Jaisalmer"
  
#  data$Detected.City[data$Detected.District == "State Pool" & data$Detected.State == "Odisha"] = "Bhuwaneshwar"
#  data$Detected.District[data$Detected.District == "State Pool" & data$Detected.State == "Odisha"] = "Khordha"
  
  
  data[tolower(data$Detected.District) %in% setdiff(tolower(data$Detected.District), union('', tolower(indiadistrict$DISTRICT))), c("Detected.City", "Detected.District", "Detected.State")] %>% group_by_all() %>% count()

  
  rm(list = c('gp', 'gp0', 'gp1', 'word', 'maxdist', 'newdist', 'uu'))
  #head(data[data$Detected.District %in% setdiff(setdiff(data$Detected.District, union(indiadistrict$NAME_2, "")), indiatown$name), 7:9], 12)
  
  data$Detected.State[data$Detected.State %in% c('', "State Unassigned")] = NA
  data$Detected.City <- as.factor(data$Detected.City)
  data$Detected.District <- as.factor(data$Detected.District)
  data$Detected.State <- as.factor(data$Detected.State)
  data$Current.Status <- as.factor(data$Current.Status)
  data$Type.of.transmission[is.na(data$Type.of.transmission)] = ''
  data$Type.of.transmission[data$Type.of.transmission == ''] = "Unknown"
  data$Type.of.transmission <- data$Type.of.transmission %>% str_replace_all("TBD", "Unknown")
  data$Type.of.transmission <- as.factor(data$Type.of.transmission)
  data$month <- as.factor(format(data$Date.Announced, "%m"))
  
# save(data, file = "C:/Users/manja/Downloads/cleaneddata.Rdata")

data$Detected.City = as.character(data$Detected.City)
data$Detected.District = as.character(data$Detected.District)  
data$Detected.State = as.character(data$Detected.State)

table(data[which(!(data$Detected.District %in% indiadistrict$DISTRICT | data$Detected.District == '')), c("Detected.District", "Detected.State")])
