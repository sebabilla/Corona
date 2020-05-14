#libraries
library(dplyr);library(tidyr);library(ggplot2);library(lubridate);library(hms)
library(readr);library(purrr);library(broom);library(stringr);library(tibble)
library(forcats);library(gridExtra);library(caret);library(ggpubr)

#downloaded filed is in Shift_JIS! Convert it in UTF_8 with Libreoffice Calc (or Excel) before putting it in the directory /Images  
patients <- read_csv("Data/patients2.csv") # https://www.harp.lg.jp/opendata/dataset/1369.html
#cleaning and make table for graphs 3, 4 and 5
# 1. removing japanese numbers
patients <- patients %>% mutate_if(is.character, str_replace_all, pattern = "０", replacement = "0") %>%
  mutate_if(is.character, str_replace_all, pattern = "１", replacement = "1") %>%
  mutate_if(is.character, str_replace_all, pattern = "２", replacement = "2") %>%
  mutate_if(is.character, str_replace_all, pattern = "３", replacement = "3") %>%
  mutate_if(is.character, str_replace_all, pattern = "４", replacement = "4") %>%
  mutate_if(is.character, str_replace_all, pattern = "５", replacement = "5") %>%
  mutate_if(is.character, str_replace_all, pattern = "６", replacement = "6") %>%
  mutate_if(is.character, str_replace_all, pattern = "７", replacement = "7") %>%
  mutate_if(is.character, str_replace_all, pattern = "８", replacement = "8") %>%
  mutate_if(is.character, str_replace_all, pattern = "９", replacement = "9") %>%
  mutate_if(is.character, str_replace_all, pattern = "10歳未満", replacement = "00代") %>%
  mutate_if(is.character, str_replace_all, pattern = "代", replacement = "") %>%
  mutate_if(is.character, str_replace_all, pattern = "非公表", replacement = "NA") %>%
  mutate_if(is.character, str_replace_all, pattern = "^－$", replacement = "NA") %>%
  # 2. use standard NA
  mutate_if(is.character, ~replace(., . == "NA", NA)) %>%
  # 3. concert sub-prefecture count to main cities of those prefecture (which removes inconsistanciy but may deform the real repartion)
  mutate(居住地 = ifelse(居住地 == "渡島総合振興局管内", "函館市", 
                         ifelse(居住地 == "後志総合振興局管内", "倶知安町",
                                   ifelse(居住地 == "胆振総合振興局管内", "室蘭市",
                                             ifelse(居住地 == "宗谷総合振興局管内", "稚内市",
                                                       ifelse(居住地 == "空知総合振興局管内", "岩見沢市",
                                                                 ifelse(居住地 == "上川総合振興局管内", "旭川市",
                                                                           ifelse(居住地 == "十勝総合振興局管内", "帯広市",
                                                                                     ifelse(居住地 %in% c("オホーツク総合振興局管内", "オホーツク総合振興局内"), "網走市", 
                                                                                               ifelse(居住地 == "根室振興局管内", "根室市", 
                                                                                                         ifelse(居住地 %in% c("石狩振興局内", "石狩振興局管内"), "石狩市", 
                                                                                                                   ifelse(居住地 %in% c("釧路総合振興局管内", "釧路総合振興局内"), "釧路市", 
                                                                                                                             ifelse(居住地 == "留萌振興局管内", "留萌市", 居住地))))))))))))) %>%
  # 4. Keep only 2 states for people posityves, symptomatic or asymptomatique, big biais as test are not systematique, before March no info = NA, after no  info = asyptomatic as the data seems more consistent
  mutate(備考 = ifelse(is.na(備考) & month(リリース日) >= 3, "無", 備考)) %>% mutate(備考 = ifelse(!is.na(備考), str_sub(備考, end = 1), NA)) %>% mutate(備考 = ifelse(備考 == "無", "無症", ifelse(!is.na(備考), "有症", NA))) %>%
  mutate(リリース日 = ymd(リリース日)) %>%
  select(リリース日, 居住地, 年代, 性別, 属性, 備考)

#file about patients'state, used to count bad news, this file too needs to be converted before import
death <- read_csv("Data/covid19_data2.csv") # https://www.harp.lg.jp/opendata/dataset/1369.html
death <- death %>% select(年, 月, 日, 日死亡数, 死亡累計) %>% 
  unite("date", 年, 月, 日, sep = "-") %>%
  mutate(date = ymd(date))

#file with localisation (from wikipaedia) and population (from http://www.pref.hokkaido.lg.jp/ss/tuk/900brr/index2.htm)) for cities in Hokkaido
#other locations are aritrary set to be on the edge of the map
places <- read_csv("Data/places_coord.csv")

#check that no new towns have been infected (would need to be add in places_coord.csv)
patients %>% group_by(居住地) %>% summarize(n=n()) %>% mutate(place = 居住地) %>% left_join(.,places) %>% filter(is.na(population))