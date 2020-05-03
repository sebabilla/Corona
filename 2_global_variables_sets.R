#variables, subsets that will be used by functions
start_date <- ymd("2020-01-28")
count_days <- 0:as.numeric(ymd(max(ch$リリース日)) - start_date)

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