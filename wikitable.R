library(rvest)
webpage <- read_html("https://en.wikipedia.org/wiki/List_of_districts_in_India")
results <- webpage %>% html_nodes("h3") %>% html_text(trim = TRUE)
results <- gsub(' \\(..\\)\\[edit\\]', '', results[1:36])
results <- results %>% str_replace_all(c("Andaman and Nicobar" = "Andaman and Nicobar Islands", "National Capital Territory of Delhi" = "Delhi"))
results2 <- webpage %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table') %>% html_table(fill = TRUE)# %>% html_text(trim = TRUE) %>% str_subset("Headquarters") %>% str_remove_all("\\[..\\]|\\[.\\]|#\n\n") %>% str_replace_all('\n\n', '\n')
results2<- results2[4:39]
for(s in 1:length(results)){
  results2[[s]]$ST_NM = results[s]
  results2[[s]] <- do.call("cbind", lapply(results2[[s]], function(ic){ic = str_remove_all(as.character(ic), '\\[..\\]|,'); ic = stringi::stri_trans_general(ic, "Latin-ASCII")}))
}
alltab <- as.data.frame(do.call("rbind", results2))
colnames(alltab) <- c('#', "Code", "DISTRICT", "Headquarters", "Population", "Area", "Density", "ST_NM")

alltab = alltab[,c("DISTRICT", "Population", "Area", "Density", "ST_NM")]
alltab$Population <- as.numeric(as.character(alltab$Population))
alltab$Area <- as.numeric(as.character(alltab$Area))
alltab$Density <- as.numeric(as.character(alltab$Density))
alltab$DISTRICT = alltab$DISTRICT %>% str_replace_all(c(
  "Sahibzada Ajit Singh Nagar" = "S.A.S. Nagar",
  "Gulbarga" = "Kalaburagi",
  "Faizabad" = "Ayodhya",
  "Allahabad" = "Prayagraj",
  "Gurgaon" = "Gurugram",
  "Bangalore" = "Bengaluru",
  "Kanker" = "Uttar Bastar Kanker",
  "Dantewada" = "Dakshin Bastar Dantewada"
))
alltab$DISTRICT <- alltab$DISTRICT %>% str_replace_all(c("Andaman & Nicobar Island" = "Andaman and Nicobar Islands", " & " = " and ", "Purba |Purbi " = "East ", "Paschim |Paschimi |Pashchim |Pashchimi " = "West "))
alltab$DISTRICT[alltab$DISTRICT %in% c("East", "West", "North", "South", "North East", "North West")] <- paste(alltab$DISTRICT, alltab$ST_NM, sep = ' ')[alltab$DISTRICT %in% c("East", "West", "North", "South", "North East", "North West")]
alltab[alltab$DISTRICT == "Mumbai City", c("Population", "Area")] = alltab[alltab$DISTRICT == "Mumbai City", c("Population", "Area")] + alltab[alltab$DISTRICT == "Mumbai suburban", c("Population", "Area")]
alltab[alltab$DISTRICT == "Mumbai City", c("Density")] = alltab[alltab$DISTRICT == "Mumbai City", c("Population")]/alltab[alltab$DISTRICT == "Mumbai City", c("Area")]
alltab = alltab[alltab$DISTRICT != "Mumbai suburban",]
alltab[alltab$DISTRICT == "Mumbai City",]$DISTRICT = "Mumbai"

delhistat = colSums(alltab[alltab$ST_NM == "Delhi", c("Population", "Area")], na.rm = TRUE)
alltab <- alltab[alltab$ST_NM != "Delhi",]
alltab = alltab %>% add_row(DISTRICT = "Delhi", ST_NM = "Delhi")
alltab[alltab$ST_NM == "Delhi", c("Population", "Area", "Density")] = c(delhistat, delhistat[1]/delhistat[2])

source("C:/Users/manja/Downloads/indiadistricttownstate.R")
for(word in unique(indiadistrict$DISTRICT[!(indiadistrict$DISTRICT %in% alltab$DISTRICT)])){
  gp = grep(str_replace_all(str_remove_all(word, 'and|east|west|north|south|rural|urban'), ' ', '|'), alltab$DISTRICT, ignore.case = TRUE)
  gp <- intersect(gp, which(alltab$ST_NM %in% indiadistrict$ST_NM[indiadistrict$DISTRICT == word]))
  if(length(gp) == 1){
    #print(c(word, "...", alltab$DISTRICT[gp]))
    alltab$DISTRICT[gp] = word
    next
  }
  gp = NULL
  maxdist = 0.1
  while(length(gp) == 0 & maxdist <= 0.32){
    gp <- agrep(word, alltab$DISTRICT, max.distance = maxdist, ignore.case = FALSE)
    if(length(gp)>1){
      gp <- gp[unlist(sapply(alltab$DISTRICT[gp], function(ww){length(agrep(ww, word, max.distance = maxdist)) > 0}))]
    }
    maxdist = maxdist + 0.03
  }
  gp <- gp[substr(alltab$DISTRICT[gp], 1, 1) == toupper(substr(word, 1, 1))]
  gp <- intersect(gp, which(alltab$ST_NM %in% indiadistrict[indiadistrict$DISTRICT == word,]$ST_NM))
  
  #if(length(gp) >0){print(c(word, "...", alltab$DISTRICT[gp]))}
  if(length(gp) == 1){
    alltab$DISTRICT[gp] = word
  }
}

alltab[alltab$DISTRICT %in% c("Kra Daadi", "Lower Siang", "Hojai"), c("Population", "Area", "Density")] =
  matrix(c(22290, 2202, 10, 80597, NA, NA, 931218, 1686, 550), ncol = 3, byrow = TRUE)

distcoord <- merge(distcoord, alltab, by = c("DISTRICT", "ST_NM"), all.x = TRUE)
distcoord$Area[is.na(distcoord$Area)] = distcoord$area[is.na(distcoord$Area)]
distcoord = distcoord[,!(names(distcoord) == "area")]
# aal = distcoord[is.na(distcoord$Population),]
# aaldist = aal$DISTRICT
# data[data$Detected.District %in% aaldist, c("Detected.District", "Detected.State")] %>% group_by_all() %>% count()
# 

rm(list = c("alltab", "aal", "aaldist", "word", "delhistat", "gp"))

results2 <- webpage %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]/tbody/tr/td/table') %>% html_table(fill = TRUE)# %>% html_text(trim = TRUE) %>% str_subset("Headquarters") %>% str_remove_all("\\[..\\]|\\[.\\]|#\n\n") %>% str_replace_all('\n\n', '\n')
results2 <- results2[[1]][,c(2,4,5)]
colnames(results2) <- c("ST_NM", "Tot.Population", "Population")
results2$Tot.Population <- as.numeric(str_remove_all(results2$Tot.Population, ','))
results2$Population <- as.numeric(str_remove_all(results2$Population, ','))

statecoord <- merge(statecoord, results2, by = "ST_NM", all.x = TRUE)
statecoord$area = as.numeric(as.character(statecoord$area))
statecoord$Density <- statecoord$Tot.Population/statecoord$area

rm(list = c("webpage", "results", "results2"))
