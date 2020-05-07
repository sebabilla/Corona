#libraries
library(dplyr);library(tidyr);library(ggplot2);library(lubridate);library(hms)
library(readr);library(purrr);library(broom);library(stringr);library(tibble)
library(forcats);library(gridExtra);library(caret)

#downloaded filed is in Shift_JIS! Convert it in UTF_8 with Libreoffice Calc (or Excel) before putting it in the directory /Images  
ch <- read_csv("Data/patients2.csv")
#cleaning and make table for graphs 3, 4 and 5 
ch <- ch %>% mutate_if(is.character, str_replace_all, pattern = "０", replacement = "0") %>%
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
  mutate_if(is.character, ~replace(., . == "NA", NA)) %>%
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
                                                                                                                   ifelse(居住地 %in% c("釧路総合振興局管内", "釧路総合振興局内"), "釧路市", 居住地))))))))))))

#file about patients'state, used to count bad news, this file too needs to be converted before import
death <- read_csv("Data/covid19_data2.csv")
death <- death %>% select(年, 月, 日, 日死亡数, 死亡累計) %>% 
  unite("date", 年, 月, 日, sep = "-") %>%
  mutate(date = ymd(date))
