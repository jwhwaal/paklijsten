library(readxl)
library(tidyverse)

paklijsten <- read_excel("paklijsten.xlsx")
master_agrotallan <- read_excel("at.xlsx")

P50921 <- read_excel("P50921.xlsx")
P50922 <- read_excel("P50922.xlsx")

codes <- unique(master_agrotallan$code) # all codes in producer group
length(unique(master_agrotallan$code))
write.csv(codes, "codes.txt")

length(unique(paklijsten$code))
codes_in_packlist <- unique(paklijsten$code)


code_zone <- paklijsten %>% left_join(., master_agrotallan, by = c("code" = "code")) %>%
  arrange(zone)
write_csv(code_zone, file = "code_zone.csv")
length(unique(code_zone$zone))

#overzicht aantal dozen per pallet - is eigenlijk één grote paklijst
toplayer <- code_zone %>% mutate(toplayer = str_detect(pallet, pattern = "TL")) %>%
  filter(toplayer == TRUE) %>%
group_by(zone, code, ctr) %>%
  arrange(zone)
 
length(unique(toplayer$zone))
  
  
# overzicht aantal dozen per zone
code_zone %>% group_by(zone) %>%
  summarize(no_boxes = sum(number)) %>%
  arrange(desc(no_boxes))

length(unique(code_zone$ctr))


# importeer lijst met geanalyseerde codes
codes_analyzed <- read_excel("codes_anal.xlsx")
codes_anal <- codes_analyzed$code
codes_analyzed %>% arrange(code) %>% print.data.frame()
length(unique(codes_anal)) # aantal geanalyseerde codes
mean(codes_analyzed$code_cont, na.rm = TRUE)

# codes nog te analyseren:
codes_tobeanal <- setdiff(codes_in_packlist, codes_anal)
sort(codes_tobeanal)
length(codes_tobeanal) # aantal nog te analyseren codes

length(codes_in_packlist) - length(codes_anal)
setdiff(codes_in_packlist, codes_anal)
length(setdiff(codes_in_packlist, codes_anal))
setdiff(codes_anal, codes_in_packlist)
length(setdiff(codes_anal, codes_in_packlist))

#codes per ordernummer

#50919
paklijsten %>% filter(ordernr==50919 & !(code %in% codes_anal)) %>%
  group_by(pallet) %>%
  summarize(sum = sum(number)) %>%
  arrange(pallet) %>%
  print.data.frame()

#50920
paklijsten %>% filter(ordernr==50920 & !(code %in% codes_anal)) %>%
  group_by(pallet) %>%
  summarize(sum = sum(number)) %>%
  arrange(pallet) %>%
  print.data.frame()

# 50921
paklijsten %>% filter(ordernr==50921 & !(code %in% codes_anal)) %>%
  group_by(pallet) %>%
  summarize(sum = sum(number)) %>%
  arrange(pallet) %>%
  print.data.frame() 

# 50922
paklijsten %>% filter(ordernr==50922 & !(code %in% codes_anal)) %>%
  group_by(pallet) %>%
  summarize(sum = sum(number)) %>%
  arrange(pallet) %>%
  print.data.frame()

#51036
paklijsten %>% filter(ordernr==51036) %>% select(code) %>% unique() #geeft de  unieke codes 
p51035 <- paklijsten %>% filter(ordernr==51153 & !(code %in% codes_anal)) %>%
  arrange( code) %>%
  print.data.frame()
write_excel_csv(p51035, "P51035.txt")


# wel geanalyseerd:
#50919
paklijsten %>% filter(ordernr==50919) %>% group_by(pallet)
paklijsten %>% filter(ordernr==50919 & (code %in% codes_anal)) %>%
  group_by(pallet) %>% print.data.frame()

#50920
paklijsten %>% filter(ordernr==50920 & (code %in% codes_anal)) %>%
  pull(pallet)


#51036
paklijsten %>% filter(ordernr==50153 & !(code %in% codes_anal)) %>%
  left_join(., master_agrotallan, by = c("code" = "code")) %>%
  select(-name, -lot, -area, - organisation, - ctr) %>%
  group_by(zone) %>% 
  print.data.frame()

paklijsten %>% filter(ordernr==51153 & !(code %in% codes_anal)) %>%
   group_by(pallet) %>% summarize(n = sum(number)) %>% 
  print.data.frame()

p1 <- paklijsten %>% filter(ordernr==51036 & !(code %in% codes_anal))
(unique(p$code))

setdiff(codes_anal, codes)

#zijn de codes van P50921/2 aanwezig in de geanalyseerde codes?
P50921c <- 
setdiff(as.vector(P50921$code), codes_anal)
setdiff(as.vector(P50922$code), codes_anal)

#besmette codes selecteren en localiseren
paklijsten %>% right_join(., codes_analyzed) %>%
  filter(code_cont ==1) %>% 
  arrange(ordernr) %>%
  print.data.frame()

#niet-bio codes selecteren en localiseren
paklijsten %>% right_join(., codes_analyzed) %>%
  filter(code_nobio ==1 & code_cont == 0) %>% 
  arrange(ordernr) %>%
  print.data.frame()


#nog te analyseren in de volgende ordernrs
AW <- read_excel("orders.xlsx") %>% sapply(., function(x){as.numeric(x)}) %>%
  as.data.frame()


orders <- AW$ordernr


paklijsten %>% filter(ordernr  %in% orders) %>% group_by(pallet)
p3 <- paklijsten %>% filter(ordernr %in% orders & !(code %in% codes_anal)) %>%
  group_by(pallet) %>% print.data.frame()
write_excel_csv(p3, "nog te analyseren_09-03.txt")

sort(unique(p3$code))
length(unique(p3$code))

# nog te analyseren week 10
paklijsten %>% filter(ordernr %in% orders_wk_10) %>% group_by(pallet)
p4 <- paklijsten %>% filter(ordernr %in% orders_wk_10 & !(code %in% codes_anal)) %>%
  group_by(pallet) %>% print.data.frame()
write_excel_csv(p4, "nog te analyseren_wk_10.txt")

sort(unique(p4$code))
length(unique(p4$code))

# lijst van pallets met codes nog te analyseren, gesorteerd op order, pallet
p5 <- paklijsten %>% filter(ordernr %in% orders & !(code %in% codes_anal)) %>%
  left_join(., master_agrotallan, by = c("code" = "code")) %>%
  select(pallet, code, number, ordernr) %>%
  group_by(ordernr, pallet) 
p5
write_excel_csv(p5, "te_analyseren.txt")

# lijst gesorteerd op code
p6 <- paklijsten %>% filter(ordernr %in% orders & !(code %in% codes_anal)) %>%
  left_join(., master_agrotallan, by = c("code" = "code")) %>%
  select(-name, -lot, -area, - organisation, - ctr) %>%
  group_by(code) 
write_excel_csv(p6, "te_analyseren_codes.txt")



# lijst van initiële gecontamineerde partij met ex-post wel en niet geanalyseerde codes
p50576_ja <- paklijsten %>% filter(ordernr == 50576 & !(code %in% codes_anal)) %>%
  left_join(., master_agrotallan, by = c("code" = "code")) %>%
  select(pallet, code, number, ordernr) %>%
  group_by(code) 
p50576_ja
write_excel_csv(p4, "P50576_ja.txt")

p50576_nee <- paklijsten %>% filter(ordernr == 50576 & !(code %in% codes_anal)) %>%
  left_join(., master_agrotallan, by = c("code" = "code")) %>%
  select(pallet, code, number, ordernr) %>%
  group_by(code) 
p50576_nee
write_excel_csv(p4, "P50576_nee.txt")

p50576 <- paklijsten %>% filter(ordernr == 50576) %>%
  mutate(analyse = (code %in% codes_anal))
         
P50576 <- paklijsten %>% filter(ordernr == 50576) %>%
  left_join(.,codes_analyzed) %>%
  mutate(resultaat=recode(code_cont, "0" = "RAS", 
                       "1" = "contaminé")) %>%
  select(-code_nobio, -code_cont) %>%
  print.data.frame()

write_excel_csv(P50576, "P50576.txt")


paklijsten %>% filter(code %in% c(1618, 4499))

unique(paklijsten$ordernr)

# maak een lijst van nog te analyseren codes per aankomstweek

paklijsten <- paklijsten %>% left_join(.,AW)

wk11 <- paklijsten %>% filter(week == 12 & !(code %in% codes_anal))
write_excel_csv(wk11, "WK12.txt")



# vraag van Geert aantallen dozen gecontamineerd
orders <- c(51232, 51233, 51235, 51238, 51279)
P50576 <- paklijsten %>% filter(ordernr %in% orders ) %>%
  left_join(.,codes_analyzed) %>%
  filter(code_cont  == 1 | code_nobio ==1) %>%
  select(-code_nobio, -code_cont) %>%
  print.data.frame()

