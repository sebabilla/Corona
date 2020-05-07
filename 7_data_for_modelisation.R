#real data of infected people
infected <- ch %>% select(リリース日, 性別, 属性, 年代, 居住地,備考) %>%
  filter(!is.na(性別)) %>% filter(!is.na(年代)) %>% filter(!is.na(居住地)) %>% filter(!居住地 %in% c("茨城県", "神奈川県", "東京都", "大阪府", "京都府", "中国武漢市")) %>%
  filter(!属性 %in% c("不明", NA)) %>%
  mutate(occupation = ifelse(属性 %in% c("中学生", "大学生", "学生", "専門学校生", "小学生", "未就学", "未就学児", "高校生"), "student", 
                               ifelse(属性 %in% c("アルバイト", "パート", "パート従業員", "パート職員"), "parttime",
                                        ifelse(属性 %in% c("介護老人保健施設入所者", "無職", "患者"), "nothing", 
                                                 ifelse(属性 %in% "主婦", "housewife",
                                                 ifelse(属性 %in% c("医師", "医療スタッフ", "医療ソーシャルワーカー", "医療従事者", "医療機関職員", "医療関係職員", "検疫官", "歯科医師", "病院職員", "看護助手", "看護師", "薬剤師", "開業医"),"healthcare", "fulltime")))))) %>%
  mutate(年代 = ifelse(年代 %in% c(90, 100), 80, 年代)) %>%
  mutate(age = 年代, sex = ifelse(性別 == "女性", "Female", "Male"), date = ymd(リリース日), place = 居住地, output = 1) %>%
  mutate(date = as.numeric(date - start_date)) %>%
  select(date, sex, occupation, age, place, output, 備考)

#simulate data of non infected people according to demographics 
#generationg odds
occup_hok_sex_age <- read_csv("Data/occup_hok_sex_age.csv") #build with data from 2017 Employment Status Survey Results for Prefectures, Human resources for health country profiles: Japan. Manila, Philippines: World HealthOrganization Regional Office for the Western Pacific; 2017 and Table 15.  Population by Age (Five-Year Groups) and Sex, and Sex Ratio - Prefectures (2010 and 2015)
occup_hok_sex_age <- occup_hok_sex_age %>% gather("age", "count", `00`: `80`) %>% unite("s_o_a", sex:age)
sumohsac <- sum(occup_hok_sex_age$count)
occup_hok_sex_age <- occup_hok_sex_age %>% mutate(odd = count/sumohsac)
vec_soa <- occup_hok_sex_age$s_o_a
vec_soa_odd <- occup_hok_sex_age$odd
places_pop <- read_csv("Data/places_coord.csv")
places_pop <- places_pop %>% filter(!is.na(population))
sumh <- sum(places_pop$population)
places_pop <- places_pop %>% select(place, population) %>%
  mutate(pourcent = population/sumh) %>% filter(!is.na(pourcent))
vec_city <- places_pop$place
vec_city_odd <- places_pop$pourcent
#generating data
  nnot_infected <- nrow(infected)*3
  city <- sample(vec_city, nnot_infected, replace = TRUE, prob = vec_city_odd)
  soa <- sample(vec_soa, nnot_infected, replace = TRUE, prob = vec_soa_odd)
  sdate <- sample(count_days, nnot_infected, replace = TRUE)
  not_infected <- tibble(date = sdate, place = city, soa = soa) %>% 
    separate(soa, c("sex", "occupation", "age"), sep = "_") %>%
    mutate(output = 0)

#notinfected %>% ggplot(aes(age, fill = fct_infreq(occupation))) + 
#  geom_bar(color="black") +
#  facet_wrap( ~ sex, ncol=2)

#table pour la modelisation
tablemodel <- infected %>% add_row(not_infected) %>%
  arrange(date) %>% mutate(age = as.numeric(age), output = as.factor(output))


#modelisation
  test_index <- createDataPartition(tablemodel$output, times = 1, p = 0.1, list = FALSE)
  train_set <- tablemodel %>% slice(-test_index)
  test_set <- tablemodel %>% slice(test_index)
  glm_fit <- train_set %>% 
    glm(output ~ date+sex+occupation+age+place, data=., family = "binomial")
  p_hat_glm <- predict(glm_fit, newdata = test_set, type = "response")
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
  accuracy <- confusionMatrix(y_hat_glm, factor(test_set$output))$overall["Accuracy"]
  sumglm <- summary(glm_fit)
  #anova(sumglm)
  hotpoint <- tibble(category = rownames(sumglm$coefficients), estimate = sumglm$coefficients[,1], p = sumglm$coefficients[,4]) %>%
    filter(category != "(Intercept)", p <= 0.01) %>% arrange(p)
  listof2 <- list("accuracy" = accuracy, coef = hotpoint)
  listof2

  train_knn <- train(output ~ date+sex+occupation+age+place, method = "knn", 
                     data = train_set,
                     tuneGrid = data.frame(k = seq(9, 71, 2)))
  train_knn$bestTune
  confusionMatrix(predict(train_knn, test_set, type = "raw"),
                  test_set$output)
  
  #text during the movie, before putting the results
  text_model <- read_csv("Data/text_model.csv") 
  plswait <- text_model %>% ggplot(aes(x, y, label=text, size = size))+
    geom_text(hjust = 0) + theme_minimal() + 
    ylim(-0.1,0.8) +
    xlim(-1,300) +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 

    
  pop <- read_csv("Data/places_coord.csv")
  tablemodel2 <- tablemodel %>% mutate(occup = ifelse(occupation == "housewife", 1,
                                          ifelse(occupation == "parttime", 2,
                                                 ifelse(occupation == "student", 3,
                                                        ifelse(occupation == "fulltime", 4, 
                                                               ifelse(occupation == "nothing", 5, 6)))))) %>%
    left_join(., pop, by = "place") %>%
    mutate(log10pop = log10(population))

  test_index <- createDataPartition(tablemodel2$output, times = 1, p = 0.1, list = FALSE)
  train_set <- tablemodel2 %>% slice(-test_index)
  test_set <- tablemodel2 %>% slice(test_index)
  glm_fit <- train_set %>% 
    glm(output ~ date+sex+occupation+age+place, data=., family = "binomial")
  p_hat_glm <- predict(glm_fit, newdata = test_set, type = "response")
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
  accuracy <- confusionMatrix(y_hat_glm, factor(test_set$output))$overall["Accuracy"]
  sumglm <- summary(glm_fit)
  #anova(sumglm)
  hotpoint <- tibble(category = rownames(sumglm$coefficients), estimate = sumglm$coefficients[,1], p = sumglm$coefficients[,4]) %>%
    filter(category != "(Intercept)", p <= 0.05) %>% arrange(p)
  listof2 <- list("accuracy" = accuracy, coef = hotpoint)
  listof2 
  
infected2 <- infected %>% mutate(備考 = ifelse(is.na(備考), "無", 備考)) %>% mutate(etat = str_sub(備考, end = 1)) %>%
  group_by(etat) %>% mutate(n=n()) %>% filter(n>2) %>% ungroup() %>%
  mutate(etat = ifelse(etat == "無", 0, 1))
  test_index <- createDataPartition(infected2$etat, times = 1, p = 0.1, list = FALSE)
  train_set <- infected2 %>% slice(-test_index)
  test_set <- infected2 %>% slice(test_index)
    glm_fit <- train_set %>% 
    glm(etat ~ date+sex+occupation+age+place, data=., family = "binomial")
    p_hat_glm <- predict(glm_fit, newdata = test_set, type = "response")
    y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
    accuracy <- confusionMatrix(y_hat_glm, as.factor(test_set$etat))$overall["Accuracy"]
    sumglm <- summary(glm_fit)
  #anova(sumglm)
  hotpoint <- tibble(category = rownames(sumglm$coefficients), estimate = sumglm$coefficients[,1], p = sumglm$coefficients[,4]) %>%
    filter(category != "(Intercept)", p <= 0.01) %>% arrange(p)
  listof2 <- list("accuracy" = accuracy, coef = hotpoint)
  listof2
  train_knn <- train(etat ~ date+sex+occupation+age+place, method = "knn", 
                     data = train_set,
                     tuneGrid = data.frame(k = seq(9, 71, 2)))
  train_knn$bestTune
  confusionMatrix(as.factor(round(predict(train_knn, test_set, type = "raw"))),
                  as.factor(test_set$etat))
  pred <- predict(train_knn, test_set, type = "raw")
  