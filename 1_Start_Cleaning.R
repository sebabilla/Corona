#libraries
library(dplyr);library(tidyr);library(ggplot2);library(lubridate);library(hms)
library(readr);library(purrr);library(broom);library(stringr);library(tibble)
library(forcats);library(gridExtra)

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
  mutate_if(is.character, ~replace(., . == "NA", NA))

#file about patients'state, used to count bad news, this file too needs to be converted before import
death <- read_csv("Data/covid19_data2.csv")
death <- death %>% select(年, 月, 日, 日死亡数, 死亡累計) %>% 
  unite("date", 年, 月, 日, sep = "-") %>%
  mutate(date = ymd(date))
