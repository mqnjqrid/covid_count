library(plyr)
library(dplyr)
library(stringr)
devtools::install_github("mqnjqrid/drpop")
library(drpop)
library(ggplot2)

load("C:/Users/manja/Downloads/cleaneddata.Rdata")
source("C:/Users/manja/Downloads/wikitable.R")
data$postelection = data$Date.Announced > "2021-04-12"

data = merge(data, towncoord, by.x = "Detected.City", by.y = "tname", suffix = c('', '.city'), all.x = TRUE)
data = merge(data, distcoord, by.x = c("Detected.District", "Detected.State"), by.y = c("DISTRICT", "ST_NM"), suffix = c('', '.district'), all.x = TRUE)
data = merge(data, statecoord, by.x = "Detected.State", by.y = "ST_NM", suffix = c('', '.state'), all.x = TRUE)

#data[is.na(data$Area), c("Detected.District", "Detected.State")] %>% group_by_all() %>% count()

data$X[is.na(data$X)] = data$X.district[is.na(data$X)]
data$X[is.na(data$X)] = data$X.state[is.na(data$X)]
data$Y[is.na(data$Y)] = data$Y.district[is.na(data$Y)]
data$Y[is.na(data$Y)] = data$Y.state[is.na(data$Y)]
#data$areaa = data$area
distcoordavg = aggregate(Area ~ ST_NM, data = distcoord, FUN = mean)
for(state in distcoordavg$ST_NM){
  #print(state)
  data$Area[data$Detected.State == state & is.na(data$Area)] = distcoordavg$Area[distcoordavg$ST_NM == state]
  data$Population[data$Detected.State == state & is.na(data$Population)] = statecoord$Population[statecoord$ST_NM == state]
  data$Density[data$Detected.State == state & is.na(data$Density)] = statecoord$Density[statecoord$ST_NM == state]
}


data2 <- data[data$Num.Cases == 1 & !is.na(data$Num.Cases), c("list4", "list2", "list1", "list3", "list6", "Age.Bracket", "Gender", "Nationality", "days", "X", "Y", "Area", "Population", "Density", "Tot.Population", "area", "Num.Cases", "Current.Status", "Type.of.transmission", "Detected.State")]
data2$Gender <- data2$Gender %>% str_remove_all(" |,|emale")
data2$Gender[is.na(data2$Gender)] = "Non-Binary"
data2$Gender = as.factor(data2$Gender)

data2$list2 = (data2$list1 + data2$list2 + data2$list3 + data2$list6 >= 1)
data2$agena = 1
data2$agena[!is.na(data2$Age.Bracket)] = 0
data2$Age.Bracket[data2$agena == 1] = 0

rm(list = c("data", "distcoordavg"))

data3 = data2[,!(names(data2) %in% c("list1", "list3", "Nationality", "Detected.State", "Num.Cases"))]
data3 = data3 %>% mutate(Area = log(Area), Population = log(Population), Density = log(Density))
datalong = reshape2::melt(reformat(data3, 1:2) %>% select_if(is.numeric), id.vars = c("list2", "list4"))
datalong = datalong %>% mutate(list11 = list2*list4, list10 = list2*(1-list4), list01 = (1-list2)*list4) %>% reshape2::melt(id.vars = c("list2", "list4", "variable", "value"))
colnames(datalong) = c("list2", "list4", "variable", "value", "caphist", "extra")
datalong = datalong[datalong$extra == 1,]
ggplot(datalong, aes(y = value, x = caphist, color = caphist)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")
rm(list = c("datalong", "indiadistrict", "indiastate", "indiatown", "data2", "s", "maxdist"))
phat <- psinhat(List_matrix = data3, K = 2, funcname = c("ranger", "gam"), nfolds = 5, eps = 0.008, iter = 10000, twolist = FALSE, filterrows = FALSE)
phat$psi
#save(phat, file = "C:/Users/manja/Downloads/covid5folds_ANI_in_both_lists.Rdata")
nrow(data3)/phat$psi
plotci(psinhat = phat)$g1
# nrow(data2)/phat$psi
# logit.PI logit.DR logit.TMLE ranger.PI ranger.DR ranger.TMLE
# 1,2  3429042  4803757    4247128   1995564   3066528     1535038

# ANI included in official and also press lists to increase list overlap
# ranger.PI ranger.DR ranger.TMLE  gam.PI  gam.DR gam.TMLE
# 1,2   1878665   2928549     1403685 3084327 4944238  3584974
#
qhhat <- qhateval(List_matrix = data3, K = 2, funcname = c("mlogit"), nfolds = 2, twolist = FALSE, eps = 0.005, iter = 50)


pp = tidyr::separate(reshape2::melt(phat$nuis), col = Var2, into = c("model", "prob"), sep = "\\.")

pq = cbind(pp[pp$model == "logit",2:4], pp[pp$model == "ranger",4])
colnames(pq)[3:4] = c("value.logit", "value.ranger")
ggplot(pq, aes(x = value.logit, y = value.ranger)) + geom_point() + facet_wrap(~prob)
