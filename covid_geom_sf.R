library(scales)
data2 %>% filter(Num.Cases > 1) %>% subset(select = c(Num.Cases)) %>% unlist %>% log10 %>% hist
ggplot(data2 %>% filter(Num.Cases > 1)) +
  geom_bar(aes(x = Detected.State)) +
  theme(axis.text.x = element_text(angle = 90))

data0 = merge(merge(indiadistrict,
              data2 %>% filter(Num.Cases != 1) %>% group_by(Detected.State, Detected.District, signnum = sign(Num.Cases)) %>% summarise(ncases = sum(Num.Cases)),
              by.x = c("DISTRICT", "ST_NM"),
              by.y = c("Detected.District", "Detected.State")),
              data2 %>% filter(Num.Cases != 1) %>% group_by(Detected.State, Detected.District, signnum = sign(Num.Cases)) %>% count(),
              by.x = c("DISTRICT", "ST_NM", "signnum"),
              by.y = c("Detected.District", "Detected.State", "signnum"))
data1 = merge(merge(indiadistrict,
                    data2 %>% group_by(Detected.State, Detected.District) %>% summarise(ncases = sum(Num.Cases)),
                    by.x = c("DISTRICT", "ST_NM"),
                    by.y = c("Detected.District", "Detected.State")),
                    data2 %>% group_by(Detected.State, Detected.District) %>% count(),
                    by.x = c("DISTRICT", "ST_NM"),
                    by.y = c("Detected.District", "Detected.State"))
ggplot(data0 %>% filter(signnum < 0 & ncases < -500)) + geom_sf(aes(fill = ncases), color = NA) + facet_wrap(~signnum) + scale_fill_continuous(trans = modulus_trans(1))
ggplot(data0 %>% filter(signnum != 0 & n > 1)) + geom_sf(aes(fill = n), color = NA) + facet_wrap(~signnum) + scale_fill_continuous(trans = modulus_trans(1))
ggplot(data1) + geom_sf(aes(fill = ncases/n), color = NA) + scale_fill_continuous(trans = modulus_trans(1))

rbind(data2 %>% subset(select = -c(Age.Bracket, agena, Gender, Nationality, Type.of.transmission)) %>% group_by(Detected.State) %>% slice_max(Num.Cases) %>% as.data.frame, data2 %>% subset(select = -c(Age.Bracket, agena, Gender, Nationality, Type.of.transmission)) %>% group_by(Detected.State) %>% slice_min(Num.Cases) %>% as.data.frame) %>% arrange(Detected.State)
