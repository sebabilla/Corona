#variables, subsets that will be used by functions 3 and 4
start_date <- ymd("2020-01-28")
count_days <- 0:as.numeric(ymd(max(ch$リリース日)) - start_date)

# tibbles for graph function graph_sex_job_age
table_sex_job_age <- ch %>% select(リリース日, 性別, 属性, 年代)%>%
  filter(!is.na(性別)) %>% 
  mutate(job = str_sub(属性, end = 2)) %>%
  group_by(job) %>% mutate(n = n()) %>%
  ungroup() %>%
  mutate(年代 = as.character(年代))

jobmax_12 <- ch %>% select(属性) %>% 
  mutate(job = str_sub(属性, end = 2)) %>%
  group_by(job) %>%
  summarize(n = n()) %>% arrange(desc(n)) %>% 
  head(12) %>% select(job)

#tibble for function map_15d
hok_map <- map_data("world2", "Japan") %>% filter(subregion == "Hokkaido")
places_coord <- read_csv("Data/places_coord.csv")
places_coord <- places_coord %>% mutate(place = str_sub(Place, end = 2)) %>%
  select(place, long, lat) %>% add_row(place = NA, long = 145, lat = 45)
ext_hok <- tibble(location = c("中国←", "↓都府県", "NA"), long = c(140, 145, 145), lat = c(45.5, 41.5, 45.5))
table_places <- ch %>% select(リリース日, 性別, 年代, 居住地)%>%
  mutate(place = str_sub(居住地, end = 2)) %>%
  left_join(., places_coord, by = "place") %>%
  mutate(long = ifelse(is.na(性別), long, ifelse(性別 == "女性", long - 0.1, long + 0.1))) %>%
  mutate(lat = ifelse(is.na(年代), lat, ifelse(as.numeric(年代) < 20, lat + 0.3, ifelse(as.numeric(年代) > 60, lat - 0.3, lat))))
  