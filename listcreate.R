library(rvest)
webpage <- read_html("https://telegra.ph/Covid-19-Sources-03-19")
results <- webpage %>% html_nodes("p") %>% html_text(trim = TRUE) %>% str_subset("http")
results <- str_remove_all(results, ".* - ")
results <- str_remove_all(results, "http://|https://|mobile\\.")
results <- sub("/$", "", results)

extraofficialsources = "dipr|gov\\.in|arogya|dept|department|health|administration|arcgis|nic\\.in|itanagarsmartcity|dddcovid|dashboard|bulletin|nrhmmanipur|dhfw|ipr_|ani on"

rawdata$Source1 = as.numeric(grepl(paste(c(results, extraofficialsources), collapse = '|', sep = ''), rawdata$Source_1, ignore.case = TRUE))
rawdata$Source2 = as.numeric(grepl(paste(c(results, extraofficialsources), collapse = '|', sep = ''), rawdata$Source_2, ignore.case = TRUE))
rawdata$Source3 = as.numeric(grepl(paste(c(results, extraofficialsources), collapse = '|', sep = ''), rawdata$Source_3, ignore.case = TRUE))

#govtsources = "gov|bulletin|bulletion|dashboard|nic|HFW|covid|corona|nrh|health|ipr|nhm|collector|facebook|cmo|arogya|bjp|arcgis"
#list1[grep(govtsources, uu1, ignore.case = TRUE)] <- 1
#list1[grep(govtsources, uu2, ignore.case = TRUE)] <- 1
#list1[grep(govtsources, uu3, ignore.case = TRUE)] <- 1

presssources = "times|ndtv|indiatv|abp|ians|post|toi|zee|bhaskar|news|hindu|pib|herald|livemint|xpress|jagran|indiatoday|manorama|business|mirror|kashmirmonitor|httweets|thchennai|thandhra|thmumbai|thhyderabad|deshgujarat|mathrubhumi|youtube|amarujala|babushahi|IEBengaluru|wire|diaries|now|magazine|yespunjab|varthabharati|syllad|voiceofsikkim|eastmojo|kannadaprabha|naidunia|kalingatv|punjabupdate|arunachal24|outlook|uniindia|patrika|latestly|tribune|theprint|oneindia|republic|easttoday|shillongtoday|livetv"
rawdata$Source1[rawdata$Source1 == 0 & grepl(presssources, rawdata$Source_1, ignore.case = TRUE)] <- 2
rawdata$Source2[rawdata$Source2 == 0 & grepl(presssources, rawdata$Source_2, ignore.case = TRUE)] <- 2
rawdata$Source3[rawdata$Source3 == 0 & grepl(presssources, rawdata$Source_3, ignore.case = TRUE)] <- 2

othersources = "weather|dronamaps|google|fb.watch|facebook.watch"
rawdata$Source1[rawdata$Source1 == 0 & grepl(othersources, rawdata$Source_1, ignore.case = TRUE)] <- 3
rawdata$Source2[rawdata$Source2 == 0 & grepl(othersources, rawdata$Source_2, ignore.case = TRUE)] <- 3
rawdata$Source3[rawdata$Source3 == 0 & grepl(othersources, rawdata$Source_3, ignore.case = TRUE)] <- 3

rawdata$Source1[rawdata$Source1 == 0 & grepl("t\\.me", rawdata$Source_1, ignore.case = TRUE)] <- 4
rawdata$Source2[rawdata$Source2 == 0 & grepl("t\\.me", rawdata$Source_2, ignore.case = TRUE)] <- 4
rawdata$Source3[rawdata$Source3 == 0 & grepl("t\\.me", rawdata$Source_3, ignore.case = TRUE)] <- 4

#list1 = rep(0, nrow(rawdata))
#list2 = list1
#list3 = list1
#list4 = list1
#list5 = list1
list1 = (rawdata$Source_1 != "" & rawdata$Source1 == 0) | (rawdata$Source_2 != "" & rawdata$Source2 == 0) | (rawdata$Source_3 != "" & rawdata$Source3 == 0)
list2 = rawdata$Source1 == 2 | rawdata$Source2 == 2 | rawdata$Source3 == 2
list3 = rawdata$Source1 == 3 | rawdata$Source2 == 3 | rawdata$Source3 == 3
list4 = rawdata$Source1 == 1 | rawdata$Source2 == 1 | rawdata$Source3 == 1
list5 = rawdata$Source1 == 4 | rawdata$Source2 == 4 | rawdata$Source3 == 4
list6 = (str_length(rawdata$Source_1) + str_length(rawdata$Source_2) + str_length(rawdata$Source_3) == 0)
list7 = (grepl("twitter.com/ANI", rawdata$Source_1) |
           grepl("twitter.com/ANI", rawdata$Source_2) |
           grepl("twitter.com/ANI", rawdata$Source_3))

rawdata$list1 = list1
rawdata$list2 = list2 | list5
rawdata$list3 = list3
rawdata$list4 = list4
rawdata$list6 = list6
rawdata$list7 = list7
#summary(cbind(list1, list2, list3, list4, list5, list6, list7, list1*list2, list1*list3, list1*list4, list2*list3, list2*list4, list3*list4))
# 
# unique(sapply(rawdata$Source_1[rawdata$Source1 == 0], function(word){unlist(str_split(word, '/'))[1]}))
# unique(sapply(rawdata$Source_2[rawdata$Source2 == 0], function(word){unlist(str_split(word, '/'))[1]}))
# unique(sapply(rawdata$Source_3[rawdata$Source3 == 0], function(word){unlist(str_split(word, '/'))[1]}))
# 
# presslist = setdiff(unlist(str_split(presssources, '\\|')), '')
# 
# stotal = rbind(sapply(presslist, function(word){sum(rawdata$Source1 == 2 & grepl(word, rawdata$Source_1, ignore.case = TRUE))}),
#       sapply(presslist, function(word){sum(rawdata$Source2 == 2 & grepl(word, rawdata$Source_2, ignore.case = TRUE))}),
#       sapply(presslist, function(word){sum(rawdata$Source3 == 2 & grepl(word, rawdata$Source_3, ignore.case = TRUE))}))
# stotal
# 
# sort(table(sapply(u1[grep("witter|t\\.co", u1)], function(word){unlist(str_split(word, '/'))[2]})))
# 
# u1 = (sapply(rawdata$Source_1[rawdata$Source1 == 0], function(word){word}))
# u2 = (sapply(rawdata$Source_2[rawdata$Source2 == 0], function(word){word}))
# u3 = (sapply(rawdata$Source_3[rawdata$Source3 == 0], function(word){word}))
# 
# unique(sapply(u1[grep("witter|t\\.co", u1)], function(word){unlist(str_split(word, '/'))[2]}))
