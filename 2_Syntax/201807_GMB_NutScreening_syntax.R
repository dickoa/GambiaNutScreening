library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(forcats)
library(plotly)
library(RColorBrewer)
library(rmarkdown)
library(ggmosaic)

NutGMB_main <- read_excel("C:/Users/william.olander/Desktop/201807_GMB_NutScreening/3_ProcessedData/Nutrition Screening and Registration Data_Cleaned version_WFPGAM_2018_FINAL_for_RBD.xlsx")

#table of district / LGA - Sami and Janjanbureh districts both are in 3 LGAs
table(NutGMB_main$LGA, NutGMB_main$District)
#recode all Sami and Janjanbureh district entries into CRR
NutGMB_main$LGA[NutGMB_main$District=="Sami"] <- "CRR"
#recode all Sami and Janjanbureh district entries into CRR
NutGMB_main$LGA[NutGMB_main$District=="Janjanbureh"] <- "CRR"
#create admin2names and codes so it can match shape files
NutGMB_main$adm2_code <- NutGMB_main$District
NutGMB_main$adm2_name <- NutGMB_main$District
#recode the names and codes  to match shape files
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Central Baddibu"] <- "16423"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Janjanbureh"] <- "16407"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Jarra Central"] <- "16417"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Jarra East"] <- "16418"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Jarra West"] <- "16419"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Jokadu"] <- "16424"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Kantora"] <- "16430"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Kiang Central"] <- "16420"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Kiang East"] <- "16421"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Kiang West"] <- "16422"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Lower Baddibou"] <- "16425"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Lower Niumi"] <- "16426"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Lower Saloum"] <- "16408"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Niamina East"] <- "16410"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Niamina West"] <- "16411"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Niani"] <- "16412"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Nianija"] <- "16413"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Sami"] <- "16414"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Upper Nuimi"] <- "16428"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Upper Saloum"] <- "16415"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Dankunku"] <- "Niamina Dankunku"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Dankunku"] <- "16409"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Illiasa"] <- "Central Baddibu"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Illiasa"] <- "16423"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Basse/Fulladu East"] <- "Fulladu East"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Basse/Fulladu East"] <- "16429"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Lower Fulladu West"] <- "Fulladu West"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Lower Fulladu West"] <- "16406"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Upper Fulladu West"] <- "Fulladu West"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Upper Fulladu West"] <- "16406"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Wuli East"] <- "Wuli"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Wuli East"] <- "16432"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Wuli West"] <- "Wuli"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Wuli West"] <- "16432"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Sandu"] <- "16431"
#According to https://en.wikipedia.org/wiki/Upper_Baddibu Sabach Sanjal is in Upper Baddibu
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Sabach Sanjal"] <- "16427"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Sabach Sanjal"] <- "Upper Baddibu"
#According to https://en.wikipedia.org/wiki/Fulladu_East Tumana and Jimara are in Fulladu East
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Jimara"] <- "16429"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Jimara"] <- "Fulladu East"
NutGMB_main$adm2_code[NutGMB_main$adm2_code=="Tumana"] <- "16429"
NutGMB_main$adm2_name[NutGMB_main$adm2_name=="Tumana"] <- "Fulladu East"
#make numeric
NutGMB_main$adm2_code <- as.numeric(NutGMB_main$adm2_code)

#create SAM dummy
NutGMB_main$SAM <-NutGMB_main$`Nutrition classification`
NutGMB_main$SAM[NutGMB_main$SAM=="SAM"] <- 1
NutGMB_main$SAM[NutGMB_main$SAM=="MAM"] <- 0
NutGMB_main$SAM[NutGMB_main$SAM=="NORMAL"] <- 0
NutGMB_main$SAM <- as.numeric(NutGMB_main$SAM)
#create MAM dummy
NutGMB_main$MAM <-NutGMB_main$`Nutrition classification`
NutGMB_main$MAM[NutGMB_main$MAM=="SAM"] <- 0
NutGMB_main$MAM[NutGMB_main$MAM=="MAM"] <- 1
NutGMB_main$MAM[NutGMB_main$MAM=="NORMAL"] <- 0
NutGMB_main$MAM <- as.numeric(NutGMB_main$MAM)
#create SAMMAM dummy
NutGMB_main$SAMMAM <-NutGMB_main$`Nutrition classification`
NutGMB_main$SAMMAM[NutGMB_main$SAMMAM=="SAM"] <- 1
NutGMB_main$SAMMAM[NutGMB_main$SAMMAM=="MAM"] <- 1
NutGMB_main$SAMMAM[NutGMB_main$SAMMAM=="NORMAL"] <- 0
NutGMB_main$SAMMAM <- as.numeric(NutGMB_main$SAMMAM)
#recode vitamin A
NutGMB_main$vitA <-NutGMB_main$`Vitamin A supplementation by child`
NutGMB_main$vitA[NutGMB_main$vitA=="Mother claims NO but cards not seen"] <- "No"
NutGMB_main$vitA[NutGMB_main$vitA=="Mother claims YES but card not seen"] <- "Yes"
NutGMB_main$vitA[NutGMB_main$vitA=="NO, card seen"] <- "No"
NutGMB_main$vitA[NutGMB_main$vitA=="YES, card seen"] <- "Yes"
#vitA dummy
NutGMB_main$vitAdummy <-NutGMB_main$vitA
NutGMB_main$vitAdummy[NutGMB_main$vitAdummy=="No"] <- 0
NutGMB_main$vitAdummy[NutGMB_main$vitAdummy=="Yes"] <- 1
NutGMB_main$vitAdummy <- as.numeric(NutGMB_main$vitAdummy)
#recode Child Dewormed
NutGMB_main$dworm <-NutGMB_main$`Child Dewormed`
NutGMB_main$dworm[NutGMB_main$dworm=="Mother claims NO but cards not seen"] <- "No"
NutGMB_main$dworm[NutGMB_main$dworm=="Mother claims YES but card not seen"] <- "Yes"
NutGMB_main$dworm[NutGMB_main$dworm=="NO, card seen"] <- "No"
NutGMB_main$dworm[NutGMB_main$dworm=="YES, card seen"] <- "Yes"
#vitA dummy
NutGMB_main$dwormdummy <-NutGMB_main$dworm
NutGMB_main$dwormdummy[NutGMB_main$dwormdummy=="No"] <- 0
NutGMB_main$dwormdummy[NutGMB_main$dwormdummy=="Yes"] <- 1
NutGMB_main$dwormdummy <- as.numeric(NutGMB_main$dwormdummy)
#diahrrea dummy
table(NutGMB_main$`Child has diarrhoea`)
NutGMB_main$diarrdummy <-NutGMB_main$`Child has diarrhoea`
NutGMB_main$diarrdummy[NutGMB_main$diarrdummy=="No"] <- 0
NutGMB_main$diarrdummy[NutGMB_main$diarrdummy=="Yes"] <- 1
NutGMB_main$diarrdummy <- as.numeric(NutGMB_main$diarrdummy)
#malaria dummy
table(NutGMB_main$`Child has malaria`)
NutGMB_main$maldummy <- NutGMB_main$`Child has malaria`
NutGMB_main$maldummy[NutGMB_main$maldummy=="No"] <- 0
NutGMB_main$maldummy[NutGMB_main$maldummy=="Yes"] <- 1
NutGMB_main$maldummy <- as.numeric(NutGMB_main$maldummy)
#create age category under2
NutGMB_main$age_group <-NutGMB_main$`Child age in months`
NutGMB_main$age_group[as.integer(NutGMB_main$age_group)>=24] <- "Over2yrsUnder5yrs"
NutGMB_main$age_group[as.integer(NutGMB_main$age_group)<24] <- "Over6mthsUnder2yrs"
table(NutGMB_main$age_group)
write.csv(NutGMB_main, "C:/Users/william.olander/Desktop/201807_GMB_NutScreening/3_ProcessedData/NutGMB_main.csv")

#create table of SAMMAM % by vitA % and de-woming by admin2
nutbyvitadworm <- NutGMB_main %>% group_by(LGA, adm2_name, adm2_code,age_group) %>% summarize(casesSAMMAM = sum(SAMMAM), casesvita = sum(vitAdummy), casesdworm = sum(dwormdummy), casesdiarr = sum(diarrdummy), casesmal = sum(maldummy), 
                                                                                    percSAMMAM = casesSAMMAM/n()*100,percvitA = casesvita/n()*100, percdworm = casesdworm/n()*100, percdiarr = casesdiarr/n()*100, percmal = casesmal/n()*100)
nutbyvitadwormage2 <- NutGMB_main %>% filter(age_group=="Over6mthsUnder2yrs") %>% group_by(LGA, adm2_name, adm2_code) %>% summarize(casesSAMMAM2 = sum(SAMMAM), casesvita2 = sum(vitAdummy), casesdworm2 = sum(dwormdummy), casesdiarr2 = sum(diarrdummy), casesmal2 = sum(maldummy), 
                                                                                                                                   percSAMMAM2 = casesSAMMAM2/n()*100,percvitA2 = casesvita2/n()*100, percdworm2 = casesdworm2/n()*100, percdiarr2 = casesdiarr2/n()*100, percmal2 = casesmal2/n()*100) 
nutbyvitadwormage5 <- NutGMB_main %>% filter(age_group=="Over2yrsUnder5yrs") %>% group_by(LGA, adm2_name, adm2_code) %>% summarize(casesSAMMAM5 = sum(SAMMAM), casesvita5 = sum(vitAdummy), casesdworm5 = sum(dwormdummy), casesdiarr5 = sum(diarrdummy), casesmal5 = sum(maldummy), 
                                                                                                                                   percSAMMAM5 = casesSAMMAM5/n()*100,percvitA5 = casesvita5/n()*100, percdworm5 = casesdworm5/n()*100, percdiarr5 = casesdiarr5/n()*100, percmal5 = casesmal5/n()*100) 
###making maps
#import adm2 shapefile
gamb_shp <- read_sf("C:/Users/william.olander/Desktop/201807_GMB_NutScreening/9_Miscellaneous/gmb_lga_district_strata_boundary_20180712.shp")
#merge summary table with shapefile
merg_gamb <- left_join(gamb_shp, nutbyvitadworm, by = "adm2_code")
merg_gamb2 <- left_join(gamb_shp, nutbyvitadwormage2, by = "adm2_code")
merg_gamb5 <- left_join(gamb_shp, nutbyvitadwormage5, by = "adm2_code")
anti_join(nutbyvitadworm, gamb_shp, by = "adm2_code")

##Making maps and charts
setwd("C:/Users/william.olander/Desktop/201807_GMB_NutScreening/4_OutputTables")
#create minimally theme
themebill <- theme_minimal() +theme(panel.grid.major = element_line(colour="white"), axis.text.x = element_text(angle = 60, hjust = 1), axis.title.x = element_blank(), axis.title.y = element_blank()) + theme(legend.position="bottom")
#number of cases by age and gender
bar0 <- NutGMB_main %>% ggplot(aes(factor(fct_infreq(adm2_name)), fill = `Gender`))
bar0 <- bar0 +geom_bar(position = "dodge") +themebill +facet_grid(. ~age_group)
bar0 <- bar0 
ggsave("bar0.png",plot = bar0,width = 6, height = 4)

#SAMMAM cases by district
map1_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = casesSAMMAM2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="white",limits = c(0,412)) +labs(fill="# of SAM/MAM cases")
map1_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = casesSAMMAM5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="white",limits = c(0,412)) +labs(fill="# of SAM/MAM cases")
map2_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = percSAMMAM2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="white",limits = c(0,18)) +labs(fill="% SAM/MAM cases")
map2_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = percSAMMAM5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="white",limits = c(0,18)) +labs(fill="% SAM/MAM cases")
#stacked bar chart of number and % of nutrition cases
NutGMB_main <- NutGMB_main %>% arrange(desc(`Nutrition classification`)) %>% mutate(`Nutrition classification` = factor(`Nutrition classification`, levels = c("SAM", "MAM", "NORMAL")))
bar1 <- NutGMB_main %>% filter(SAMMAM=="1") %>% ggplot(aes(factor(fct_infreq(adm2_name)), fill = `Nutrition classification`))
bar1 <- bar1 +geom_bar(position = "dodge")+themebill+facet_grid(. ~age_group)
bar1 <- bar1 +scale_fill_manual(values=c("#C0392B","#F1C40F","#27AE60"))
bar2 <- NutGMB_main %>% ggplot(aes(adm2_name)) + geom_bar(aes(fill=`Nutrition classification`), position = "fill")
bar2 <- bar2 +themebill+scale_y_continuous(labels = percent)+facet_grid(. ~age_group)
bar2 <- bar2 +scale_fill_manual(values=c("#C0392B","#F1C40F","#27AE60"))
ggsave("map1_2.png",plot = map1_2,width = 6, height = 4)
ggsave("map1_5.png",plot = map1_5,width = 6, height = 4)
ggsave("map2_2.png",plot = map2_2,width = 6, height = 4)
ggsave("map2_5.png",plot = map2_5,width = 6, height = 4)
ggsave("bar1.png",plot = bar1,width = 6, height = 4)
ggsave("bar2.png",plot = bar2,width = 6, height = 4)

#vita cases by district
map3_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = casesvita2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("# of cases",palette = "Blues",na.value="white",direction=1,limits = c(47,2718))
map3_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = casesvita5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("# of cases",palette = "Blues",na.value="white",direction=1,limits = c(47,2718))
map4_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = percvitA2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases",palette = "Blues",na.value="white",direction=1,limits = c(27,98)) 
map4_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = percvitA5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases",palette = "Blues",na.value="white",direction=1,limits = c(27,98)) 
#stacked bar chart of number and % of nutrition cases
bar3 <- NutGMB_main %>% ggplot(aes(factor(fct_infreq(adm2_name)), fill = `vitA`)) +geom_bar(position = "dodge")
bar3 <- bar3 +themebill+facet_grid(. ~age_group)+labs(fill="recieved vitamin A")
bar4 <- NutGMB_main %>% ggplot(aes(adm2_name)) + geom_bar(aes(fill=`vitA`), position = "fill")+facet_grid(. ~age_group)
bar4 <- bar4 +themebill+scale_y_continuous(labels = percent) +labs(fill="recieved vitamin A")
ggsave("map3_2.png",plot = map3_2,width = 6, height = 4)
ggsave("map3_5.png",plot = map3_5,width = 6, height = 4)
ggsave("map4_2.png",plot = map4_2,width = 6, height = 4)
ggsave("map4_5.png",plot = map4_5,width = 6, height = 4)
ggsave("bar3.png",plot = bar3,width = 6, height = 4)
ggsave("bar4.png",plot = bar4,width = 6, height = 4)

#dworm cases by district
map5_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = casesdworm2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("# of cases",palette="Greens",direction = 1, na.value="white",limits = c(44,1974))
map5_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = casesdworm5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA)+scale_fill_distiller("# of cases",palette="Greens",direction = 1, na.value="white",limits = c(44,1974))
map6_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = percdworm2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases",palette="Greens",direction = 1,na.value="white",limits = c(26,95))
map6_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = percdworm5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases",palette="Greens",direction = 1,na.value="white",limits = c(26,95))
#stacked bar chart of number and % of dworm cases
bar5 <- NutGMB_main %>% ggplot(aes(factor(fct_infreq(adm2_name)), fill = `dworm`))
bar5 <- bar5 +geom_bar(position = "dodge") +themebill +facet_grid(. ~age_group)+labs(fill="recieved de-worming medicine")
bar6 <- NutGMB_main %>% ggplot(aes(adm2_name)) + geom_bar(aes(fill=`dworm`), position = "fill")
bar6 <- bar6 +themebill+scale_y_continuous(labels = percent)+facet_grid(. ~age_group)+labs(fill="recieved de-worming medicine")
ggsave("map5_2.png",plot = map5_2,width = 6, height = 4)
ggsave("map5_5.png",plot = map5_5,width = 6, height = 4)
ggsave("map6_2.png",plot = map6_2,width = 6, height = 4)
ggsave("map6_5.png",plot = map6_5,width = 6, height = 4)
ggsave("bar5.png",plot = bar5,width = 6, height = 4)
ggsave("bar6.png",plot = bar6,width = 6, height = 4)

#diarrhoea cases by district
map7_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = casesdiarr2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("# of cases", palette = "Purples", na.value="white",direction=1,limits = c(30,1553))
map7_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = casesdiarr5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("# of cases", palette = "Purples", na.value="white",direction=1,limits = c(30,1553))
map8_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = percdiarr2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases", palette = "Purples", na.value="white",direction=1,limits = c(6,40))
map8_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = percdiarr5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases", palette = "Purples", na.value="white",direction=1,limits = c(6,40))
#stacked bar chart of number and % of diarrhoea cases
bar7 <- NutGMB_main %>% ggplot(aes(factor(fct_infreq(adm2_name)), fill = `Child has diarrhoea`))
bar7 <- bar7 +geom_bar(position = "dodge") +themebill+facet_grid(. ~age_group)+labs(fill="child had diarrhoea")
bar8 <- NutGMB_main %>% ggplot(aes(adm2_name)) + geom_bar(aes(fill=`Child has diarrhoea`), position = "fill")
bar8 <- bar8 +themebill+scale_y_continuous(labels = percent)+facet_grid(. ~age_group)+labs(fill="child had diarrhoea")
ggsave("map7_2.png",plot = map7_2,width = 6, height = 4)
ggsave("map7_5.png",plot = map7_5,width = 6, height = 4)
ggsave("map8_2.png",plot = map8_2,width = 6, height = 4)
ggsave("map8_5.png",plot = map8_5,width = 6, height = 4)
ggsave("bar7.png",plot = bar7,width = 6, height = 4)
ggsave("bar8.png",plot = bar8,width = 6, height = 4)

#malaria cases by district
map9_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = casesmal2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("# of cases",palette="Oranges",direction=1,na.value="white",limits = c(0,126))
map9_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = casesmal5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("# of cases",palette="Oranges",direction=1,na.value="white",limits = c(0,126))
map10_2 <- ggplot(merg_gamb2) +geom_sf(aes(fill = percmal2),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases",palette="Oranges",direction=1,na.value="white",limits = c(0,6)) 
map10_5 <- ggplot(merg_gamb5) +geom_sf(aes(fill = percmal5),na.rm = TRUE) +theme_void() +coord_sf(datum=NA) +scale_fill_distiller("% of cases",palette="Oranges",direction=1,na.value="white",limits = c(0,6)) 
#stacked bar chart of number and % of nutrition cases
bar9 <- NutGMB_main %>% ggplot(aes(factor(fct_infreq(adm2_name)), fill = `Child has malaria`))
bar9 <- bar9 +geom_bar(position = "dodge") +themebill +facet_grid(. ~age_group)+labs(fill="child had malaria")
bar10 <- NutGMB_main %>% ggplot(aes(adm2_name)) + geom_bar(aes(fill=`Child has malaria`), position = "fill")
bar10 <- bar10 +themebill+scale_y_continuous(labels = percent)+facet_grid(. ~age_group)+labs(fill="child had malaria")
ggsave("map9_2.png",plot = map9_2,width = 6, height = 4)
ggsave("map9_5.png",plot = map9_5,width = 6, height = 4)
ggsave("map10_2.png",plot = map10_2,width = 6, height = 4)
ggsave("map10_5.png",plot = map10_5,width = 6, height = 4)
ggsave("bar9.png",plot = bar9,width = 6, height = 4)
ggsave("bar10.png",plot = bar10,width = 6, height = 4)

#scatterplot SAM/MAM and diarrhea
nutvitagraph3 <- ggplot(nutbyvitadworm, aes(percdiarr, percSAMMAM)) + geom_point(aes(colour=LGA,size=casesSAMMAM))
nutvitagraph3 <- nutvitagraph3 + geom_smooth(method='lm', se=FALSE) + theme_minimal()+facet_grid(. ~age_group)
nutvitagraph3 <- nutvitagraph3 + labs(title="", y = "% of children with SAM/MAM in District", x="% of children with Diarrhoea in District", color = "LGA", size = "number of SAM/MAM cases")
ggsave("nutvitagraph3.png",plot = nutvitagraph3,width = 6, height = 4)
cor.test(nutbyvitadworm$percSAMMAM, nutbyvitadworm$percdiarr, method = c("pearson"))

#scatterplot SAM/MAM and malaria
nutvitagraph4 <- ggplot(nutbyvitadworm, aes(percmal, percSAMMAM)) + geom_point(aes(colour=LGA,size=casesSAMMAM))
nutvitagraph4 <- nutvitagraph4 + geom_smooth(method='lm', se=FALSE) + theme_minimal()+facet_wrap(~age_group)
nutvitagraph4 <- nutvitagraph4 + labs(title="", y="% of children with SAM/MAM in District", x = "% of children with Malaria in District", color = "LGA", size = "number of SAM/MAM cases")
ggsave("nutvitagraph4.png",plot = nutvitagraph4,width = 6, height = 4)
cor(nutbyvitadworm$percSAMMAM, nutbyvitadworm$percmal, method = c("pearson"))

#scatterplot SAM/MAM and dworm 
nutvitagraph2 <- ggplot(nutbyvitadworm, aes(percdworm, percSAMMAM)) + geom_point(aes(colour=LGA,size=casesSAMMAM))
nutvitagraph2 <- nutvitagraph2 + geom_smooth(method='lm',se=FALSE) + theme_minimal() +facet_wrap(~age_group)
nutvitagraph2 <- nutvitagraph2 + labs(title="", y="% of children with SAM/MAM in District", x = "% of children with De-wormed in District", color = "LGA", size = "number of SAM/MAM cases")
ggsave("nutvitagraph2.png",plot = nutvitagraph2,width = 6, height = 4)
cor.test(nutbyvitadworm$percSAMMAM, nutbyvitadworm$percdworm, method = c("pearson"))

#scatterplot SAM/MAM and vitA 
nutvitagraph <- ggplot(nutbyvitadworm, aes(percvitA, percSAMMAM)) + geom_point(aes(colour=LGA,size=casesSAMMAM))
nutvitagraph <- nutvitagraph + geom_smooth(method='lm',se=FALSE) + theme_minimal() +facet_wrap(~age_group)
nutvitagraph + labs(title="", y="% of children with SAM/MAM in District", x = "% of children with Vit. A in District", color = "LGA", size = "number of SAM/MAM cases")
ggsave("nutvitagraph.png",plot = nutvitagraph,width = 6, height = 4)
cor.test(nutbyvitadworm$percSAMMAM, nutbyvitadworm$percvitA, method = c("pearson"))

##logistic regression - yikes about vitaminA 
NutGMB_main2 <- NutGMB_main %>% filter(age_group == "Over6mthsUnder2yrs") 
mylogit2 <- glm(as.numeric(SAMMAM) ~ Gender +LGA + dworm + vitA + `Child has diarrhoea` + `Child has malaria`, data = NutGMB_main2, family = "binomial")
summary(mylogit2)
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))

NutGMB_main5 <- NutGMB_main %>% filter(age_group == "Over2yrsUnder5yrs") 
mylogit5 <- glm(as.numeric(SAMMAM) ~ Gender +LGA + dworm + vitA + `Child has diarrhoea` + `Child has malaria`, data = NutGMB_main2, family = "binomial")
summary(mylogit5)
exp(cbind(OR = coef(mylogit5), confint(mylogit5)))

NutGMB_main$age_group[as.integer(NutGMB_main$age_group)>=24] <- "Over2yrsUnder5yrs"
NutGMB_main$age_group[as.integer(NutGMB_main$age_group)<24] <- "Over6mthsUnder2yrs"

#tables 
table1 <- table(NutGMB_main$`Nutrition classification`, NutGMB_main$`Child has diarrhoea`)
table1 <- prop.table(table1, 1)*100 
table1 <- round(table1, digits = 1)

#tables 
table2 <- table(NutGMB_main$`Nutrition classification`, NutGMB_main$`Child has malaria`)
table2 <- prop.table(table2, 1)*100 
table2 <- round(table2, digits = 1)

#tables 
table3 <- table(NutGMB_main$`Nutrition classification`, NutGMB_main$`dworm`)
table3 <- prop.table(table3, 1)*100 
table3 <- round(table3, digits = 1)

#tables 
table4 <- table(NutGMB_main$`Nutrition classification`, NutGMB_main$`vitA`)
table4 <- prop.table(table4, 1)*100 
table4 <- round(table4, digits = 1)


