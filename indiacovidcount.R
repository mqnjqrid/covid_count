library(plyr)
library(dplyr)
library(stringr)
if(FALSE){
rawdata <- data.frame()
for(fnum in 1:26){
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

save(rawdata, file = "C:/Users/manja/Downloads/rawdataapi.Rdata")
}
source("C:/Users/manja/Downloads/indiadistricttownstate.R")

if(FALSE){
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

rm(list = c(coordfile, webpage, results, extraofficialsources, presssources, othersources, list1, list2, list3, list4, list5, list6, list7))

rawdata$Detected.City[is.na(rawdata$Detected.City)] <- ''
rawdata$Detected.District[is.na(rawdata$Detected.District)] <- ''
rawdata$Detected.State[is.na(rawdata$Detected.State)] <- ''
########################################################################
data <- rawdata[, grepl("Source", names(rawdata)) == FALSE]
data$Type.of.transmission = str_remove_all(data$Type.of.transmission, " ")

data[grep("Leh|Ladakh", paste(data$Detected.City, data$Detected.District)),]$Detected.District = "Leh (Ladakh)"
data[grep("Delhi", data$Detected.City),]$Detected.City = "New Delhi"
data[data$Detected.District == "Ayodhya", "Detected.City"] = "Ayodhya"
data[data$Detected.District == "Ayodhya", "Detected.District"] = "Faizabad"

#table(data[str_length(data$Detected.District) == 0,]$Detected.City)
#table(data[str_length(data$Detected.District) == 0,]$Detected.State)

data[grepl("Delhi", data$Detected.District) | grepl("Delhi", data$Detected.State),]$Detected.District = "New Delhi"
data[data$Detected.District == "Mumbai" | data$Detected.City == "Mumbai",]$Detected.District = "Mumbai"
data$Detected.District <- data$Detected.District %>% str_to_title() %>%
  str_replace_all(c(
    "Mysuru" = "Mysore",
    "Ahmedabad" = "Ahmadabad",
    "Howrah" = "Haora",
    "S\\.P\\.S\\. Nellore" = "Sri Potti Sriramulu Nellore",
    "S\\.a\\.s\\. Nagar" = "Sahibzada Ajit Singh Nagar",
    "Warangal Urban" = "Warangal",
    "Chhota Udaipur" = "Udaipur",
    "Bagalkote" = "Bagalkot",
    "Sri Muktsar Sahib" = "Muktsar",
    "Budgam" = "Badgam",
    "Dholpur" = "Dhaulpur",
    "Hooghly" = "Hugli",
    "Ranga Reddy" = "Rangareddy",
    "Lahaul And Spiti" = "Lahul and Spiti",
    "Bengaluru" = "Bangalore",
    "Ahmednagar" = "Admadnagar",
    "Beed" = "Bid",
    "Italians" = "",
    "Other State" = "",
    "Other Region" = "",
    "Others" = "",
    " \\& " = " and ",
    "Purba |Purbi " = "East ",
    "Paschim |Paschimi |Pashchim |Pashchimi " = "West ",
    "Khargone" = "Khargon",
    "Jogulamba Gadwal" = "Gadwal",
    "Balasore" = "Baleshwar",
    "Agar Malwa" = "Shajapur",
    "Capf Personnel" = "Bishnupur",
    "North Garo Hills" = "East Garo Hills",
    "Gir Somnath" = "Junagadh",
    "Devbhumi Dwarka" = "Jamnagar",
    "Aravalli" = "Sabar Kantha",
    "East West Siang" = "East Siang",
    "West West Siang" = "West Siang",
    "Lower West Siang" = "West Siang",
    "Upper West Siang" = "West Siang",
    "Shi Yomi" = "West Siang",
    "Siang" = "West Siang",
    "Railway Quarantine" = "Chennai",
    "Rajanna Sircilla" = "Medak",
    "Komaram Bheem" = "Adilabad",
    "Mehsana" = "Mahesana",
    "Kutch" = "Kachchh",
    "Sangareddy" = "Medak",
    "Jayashankar Bhupalapally" = "Karimnagar",
    "Wanaparthy" = "Mahbubnagar",
    "Mulugu" = "Warangal",
    "Gomati" = "South Tripura",
    "Unokoti" = "North Tripura",
    "Hnahthial" = "Lawngtlai",
    "Khawzawl" = "Aizawl",
    "Kangpokpi" = "Senapati",
    "Pherzawl" = "Churachandpur",
    "Pakke Kessang" = "East Kameng",
    "Kamle" = "Lower Subansiri",
    "Charaideo" = "Sivasagar",
    "Majuli" = "Jorhat",
    "Gaurela Pendra Marwahi" = "Bilaspur",
    "Jhargram" = "West Medinipur"
  ))
data$Detected.City <- data$Detected.City %>% str_to_title() %>% str_replace_all(c(
  "Khargone" = "Khargon",
  "Jogulamba Gadwal" = "Gadwal",
  "Balasore" = "Baleshwar"
))

data$Detected.City[data$Detected.District == "State Pool"] = "Bhuwaneshwar"
data$Detected.District[data$Detected.District == "State Pool"] = "Khordha"


for(word in setdiff(data$Detected.District, union(indiadistrict$DISTRICT, ""))){
  gp0 = which(tolower(indiadistrict$DISTRICT) == tolower(word) & indiadistrict$ST_NM %in% data$Detected.State[data$Detected.District == word])
  if(length(gp0) == 1){
    data$Detected.District[data$Detected.District == word] = indiadistrict[gp0,]$DISTRICT
    next
  }
  gp = grep(str_replace_all(str_remove_all(word, 'and|east|west|north|south|rural|urban'), ' ', '|'), indiadistrict$DISTRICT, ignore.case = TRUE)
  gp1 = which(sapply(indiadistrict$DISTRICT, function(distword){grepl(distword, word, ignore.case = TRUE)}))
  
  if(max(length(gp), length(gp1)) > 1){
    gp <- intersect(gp, which(indiadistrict$ST_NM %in% data$Detected.State[data$Detected.District == word]))
    gp1 <- intersect(gp1, which(indiadistrict$ST_NM %in% data$Detected.State[data$Detected.District == word]))
  }
  if(length(gp) == 1){
    data$Detected.District[data$Detected.District == word] = indiadistrict[gp,]$DISTRICT
  }else if(length(gp1) == 1){
    data$Detected.District[data$Detected.District == word] = indiadistrict[gp1,]$DISTRICT
  }else if(max(length(gp), length(gp1)) > 1){
    print(word)
    print(indiadistrict$DISTRICT[gp])
    print(indiadistrict$DISTRICT[gp1])
    print(indiadistrict$ST_NM[gp])
    print("------------------------------")
  }
}

#table(data$Detected.District[!(tolower(data$Detected.District) %in% union(tolower(indiadistrict$DISTRICT), ""))])

#length(setdiff(data$Detected.District, union(indiadistrict$DISTRICT, "")))
for(word in setdiff(data$Detected.District, union(indiadistrict$DISTRICT, ""))){
  gp = NULL
  maxdist = 0.1
  while(length(gp) == 0){
    gp <- agrep(word, indiadistrict$DISTRICT, max.distance = maxdist, ignore.case = FALSE)
    maxdist = maxdist + 0.03
  }
  if(length(gp) > 1){
    gp <- gp[substr(indiadistrict$DISTRICT[gp], 1, 1) == toupper(substr(word, 1, 1))]
    gp <- intersect(gp, which(indiadistrict$ST_NM %in% data[data$Detected.District == word,]$Detected.State))
  }
  if(length(gp) == 1){
    data$Detected.District[data$Detected.District == word] = indiadistrict[gp,]$DISTRICT
  }else if(length(gp) > 1){
    print(c(word, length(gp), maxdist - 0.03))
    print(indiadistrict$DISTRICT[gp])
   }
}
length(setdiff(data$Detected.District, union(indiadistrict$DISTRICT, "")))
sort(table(data$Detected.District[!(data$Detected.District %in% union(indiadistrict$DISTRICT, ""))]))

data[!(data$Detected.District %in% union(union(indiadistrict$DISTRICT, indiatown$name), "")), c("Detected.District", "Detected.State")] %>% group_by_all() %>% count()
tail(data[!(data$Detected.District %in% union(union(indiadistrict$DISTRICT, indiatown$name), "")), c("Detected.District", "Detected.State")] %>% group_by_all() %>% count(), 12)


uu <- unique(setdiff(data$Detected.District, union(indiadistrict$DISTRICT, "")))
uu = uu[uu %in% indiatown$name]
for(word in uu){
  if(word %in% indiatown$name){
    data$Detected.City[data$Detected.District == word] = word
    newdist = data$Detected.District[data$Detected.City == word & data$Detected.District != word][1]
    data$Detected.District[data$Detected.City == word & data$Detected.District == word] = newdist
    next
  }
  gp = grep(str_replace_all(str_remove_all(word, 'and|east|west|north|south|rural|urban'), ' ', '|'), indiatown$name, ignore.case = TRUE)
  gp1 = NULL#which(sapply(indiatown$name, function(distword){grepl(str_replace_all(distword, ' ', '|'), word, ignore.case = TRUE)}))
  
  #if(max(length(gp), length(gp1)) > 1){
  #  gp <- intersect(gp, which(data$Detected.State[data$Detected.District == word] == indiadistrict$ST_NM))
  #  gp1 <- intersect(gp1, which(indiadistrict$ST_NM == data$Detected.State[data$Detected.District == word]))
  #}
  if(length(gp) == 1){
    data$Detected.City[data$Detected.District == word] = indiatown[gp,]$name
  }else if(length(gp1) == 1){
    data$Detected.City[data$Detected.District == word] = indiadtown[gp1,]$name
  }else if(max(length(gp), length(gp1)) > 1){
    print(word)
    print(indiatown$name[gp])
    print(indiatown$name[gp1])
    print("------------------------------")
  }
}

for(word in setdiff(data$Detected.City, union(indiatown$name, ""))){
  
  if(tolower(word) %in% tolower(indiatown$name)){
    data$Detected.City[data$Detected.city == word] = word
    next
  }
  gp = grep(str_replace_all(str_remove_all(word, 'and|east|west|north|south|rural|urban'), ' ', '|'), indiatown$name, ignore.case = TRUE)
  
  if(length(gp) == 1){
    data$Detected.City[data$Detected.City == word] = indiatown[gp,]$name
  }else if(length(gp1) == 1){
    data$Detected.City[data$Detected.City == word] = indiadtown[gp1,]$name
  }
}

rm(list = c(gp, gp0, gp1, word, maxdist, newdist, uu))
#head(data[data$Detected.District %in% setdiff(setdiff(data$Detected.District, union(indiadistrict$NAME_2, "")), indiatown$name), 7:9], 12)

data = merge(data, towncoord, by.x = "Detected.City", by.y = "tname", suffix = c('', '.city'), all.x = TRUE)
data = merge(data, distcoord[,-1], by.x = "Detected.District", by.y = "DISTRICT", suffix = c('', '.district'), all.x = TRUE)
data = merge(data, statecoord[,-1], by.x = "Detected.State", by.y = "ST_NM", suffix = c('', '.state'), all.x = TRUE)
data$area <- as.numeric(as.character(data$area))
data$area.state <- as.numeric(as.character(data$area.state))
data$X.state <- as.numeric(as.character(data$X.state))
data$Y.state <- as.numeric(as.character(data$Y.state))

(unique(data[is.na(data$X.district) & data$Detected.City %in% towncoord$tname,1:3]))
head(data[is.na(data$X.district) & !(data$Detected.City %in% towncoord$tname),1:3] %>% group_by_all %>% count, 20)
tail(data[is.na(data$X.district) & !(data$Detected.City %in% towncoord$tname),1:3] %>% group_by_all %>% count, 20)

data$xx = data$X
data$xx[is.na(data$xx)] = data$X.district[is.na(data$xx)]
data$xx[is.na(data$xx)] = data$X.state[is.na(data$xx)]
data$yy = data$Y
data$yy[is.na(data$yy)] = data$Y.district[is.na(data$yy)]
data$yy[is.na(data$yy)] = data$Y.state[is.na(data$yy)]
data$areaa = data$area
distcoordavg = aggregate(area ~ ST_NM, data = distcoord, FUN = mean)
for(state in distcoordavg$ST_NM){
  print(state)
  data$areaa[data$Detected.State == state & is.na(data$areaa)] = distcoordavg[distcoordavg$ST_NM == state, "area"]
}
#data$areaa[is.na(data$areaa)] = data$area.state[is.na(data$areaa)]
#data$days <- as.numeric(difftime(data$Date.Announced, min(data$Date.Announced)), units = c("days"))

data$Detected.City <- as.factor(data$Detected.City)
data$Detected.District <- as.factor(data$Detected.District)
data$Detected.State <- as.factor(data$Detected.State)
data$Current.Status <- as.factor(data$Current.Status)
data$Type.of.transmission[is.na(data$Type.of.transmission)] = ''
data$Type.of.transmission <- as.factor(data$Type.of.transmission)
data$month <- as.factor(format(data$Date.Announced, "%m"))

save(data, file = "C:/Users/manja/Downloads/cleaneddata.Rdata")
}
load("C:/Users/manja/Downloads/cleaneddata.Rdata")

data$postelection = data$Date.Announced > "2021-04-12"

data2 <- data[#data$Num.Cases == 1 & 
                !is.na(data$Num.Cases), c("list4", "list2", "list1", "list3", "Age.Bracket", "Gender", "Nationality", "days",# "xx", "yy", "areaa",
                                          "Num.Cases", "Current.Status", "Type.of.transmission", "Detected.State")]
data2$Gender <- data2$Gender %>% str_remove_all(" |,|emale")
data2$Gender[is.na(data2$Gender)] = ""
data2$Gender = as.factor(data2$Gender)

data2$list2 = (data2$list1 + data2$list2 + data2$list3 >= 1)
data2$agena = 1
data2$agena[!is.na(data2$Age.Bracket)] = 0
data2$Age.Bracket[data2$agena == 1] = 0
rm(list = c("data"))
library(drpop)
phat <- popsize(List_matrix = data2[, !(names(data2) %in% c("list1", "list3", "Nationality"))], K = 2, funcname = c("rangerlogit"), nfolds = 6, eps = 0.01, iter = 100, sl.lib = c("SL.gam", "SL.glm", "SL.ranger"), PLUGIN = FALSE, TMLE = FALSE)
save(phat, file = "C:/Users/manja/Downloads/ccdivsl6foldsnew.Rdata")
plotci(phat)

############################################################3
library(scales) #for trans_new
source("C:/Users/manja/Downloads/indiadistricttownstate.R")
options(scipen = 5)
data1 = data %>% filter(!is.na(Detected.District) & str_length(Detected.District) > 0 & !is.na(Num.Cases) & Num.Cases >= -1e20) %>% mutate(list7 = list1 + list2 + list3 + list6>= 1) %>% as.data.frame
colds = c("Detected.State", "Detected.District", "Detected.City", "Current.Status", "Num.Cases", "days", "list4", "list7")
d1 <-  aggregate(cbind(list4 = list4*Num.Cases, list7 = list7*Num.Cases)~Detected.State+Current.Status, data = data1[,colds], FUN = sum)
d2 <- aggregate(cbind(list4 = list4*Num.Cases, list7 = list7*Num.Cases)~Detected.District, data = data1[,colds], FUN = sum)
d3 <-  aggregate(cbind(list4 = list4*Num.Cases, list7 = list7*Num.Cases)~Detected.State, data = data1[,colds], FUN = sum)

#datat = merge(data, indiatown, by.x = "Detected.City", by.y = "name", suffix = c('', '.city'), all.x = TRUE)
datad = merge(indiadistrict, d2,
              by.y = "Detected.District", by.x = "DISTRICT", all.x = TRUE)# %>% filter(!is.na(Current.Status))
datas = do.call("rbind", lapply(unique(d1$Current.Status), function(status) {mdata = merge(indiastate, d1 %>% filter(Current.Status == status),
              by.x = "ST_NM", by.y = "Detected.State", all.x = TRUE);
              mdata$Current.Status = status;mdata}))
datas3 = merge(indiastate, d3,
              by.y = "Detected.State", by.x = "ST_NM", all.x = TRUE)# %>% filter(!is.na(Current.Status))

g1 = ggplot(datad) + geom_sf(aes(fill= list4), lwd = NA) + theme(legend.key.width = unit(3.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + labs(fill = "count")
g2 = ggplot(datad) + geom_sf(aes(fill= list7), lwd = NA) + theme(legend.key.width = unit(3.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
g3 = ggplot(datas %>% filter(Current.Status != '')) + geom_sf(aes(fill= list4), lwd = NA) + theme(legend.key.width = unit(3.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + labs(fill = "count")
g4 = ggplot(datas %>% filter(Current.Status != '')) + geom_sf(aes(fill= list7), lwd = NA) + theme(legend.key.width = unit(3.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
g33 = ggplot(datas3) + geom_sf(aes(fill= list4), lwd = NA) + theme(legend.key.width = unit(3.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + labs(fill = "Documented victims")
g43 = ggplot(datas3) + geom_sf(aes(fill= list7), lwd = NA) + theme(legend.key.width = unit(3.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

transval = 0.08
my_breaks = round(sapply(seq(modulus_trans(transval)$transform(min(c(datad$list4, datad$list7), na.rm = TRUE)),
                       modulus_trans(transval)$transform(max(c(datad$list4, datad$list7), na.rm = TRUE)), length.out = 10),
            modulus_trans(transval)$inverse))
g5 = ggpubr::ggarrange(g1 + ggtitle("Official sources") + scale_fill_continuous(trans = modulus_trans(transval), breaks = my_breaks),
                  g2 + ggtitle("News and other sources") + scale_fill_continuous(trans = modulus_trans(transval)),
                  common.legend = TRUE)
g5

transval = 0.6
my_breaks = c(0,round(sapply(seq(modulus_trans(transval)$transform(min(c(datas$list4, datas$list7), na.rm = TRUE)),
                             modulus_trans(transval)$transform(max(c(datas$list4, datas$list7), na.rm = TRUE)), length.out = 10),
                         modulus_trans(transval)$inverse)))
g6 = ggpubr::ggarrange(g33 + ggtitle("Official sources") + scale_fill_continuous(trans = modulus_trans(transval), breaks = c(0,80000, 250000, 500000, 900000, 1500000, 2100000, 2800000)),#my_breaks),# breaks = c(0, 5, 10, 20, 50, 100, 500, 5000, 25000)),
                       g43 + ggtitle("News and other sources") + scale_fill_continuous(trans = modulus_trans(transval)),
                       common.legend = TRUE)
g6
png("C:/Users/manja/Downloads/indiacovidstate_allcases.png", width = 900*0.75, height = 600*0.75)
g6
dev.off()

g6 = ggpubr::ggarrange(g3 + ggtitle("Official sources") + facet_wrap(~Current.Status, nrow = 1) + scale_fill_continuous(trans = modulus_trans(0), breaks = c(0, 5, 10, 20, 50, 100, 500, 5000, 25000)),
                  g4 + ggtitle("News and other sources") + facet_wrap(~Current.Status, nrow = 1) + scale_fill_continuous(trans = modulus_trans(0)),
                  ncol = 1, common.legend = TRUE)
pdf("C:/Users/manja/Downloads/indiacoviddistrict_allcases.pdf", width = 9, height = 4.5)
g5
dev.off()
pdf("C:/Users/manja/Downloads/indiacovidstate_allcases.pdf", width = 9, height = 6)
g6
dev.off()
