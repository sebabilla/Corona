#some things I finally didn't use

#confusion matrix, etc enough to see it's bad
anova(sumglm)

#glm give better results for both models
train_knn <- train(etat ~ date+sex+occupation+age+place, method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
train_knn$bestTune
confusionMatrix(as.factor(round(predict(train_knn, test_set, type = "raw"))),
                as.factor(test_set$etat))
pred <- predict(train_knn, test_set, type = "raw")

#it does not improve the efficiency of the model to transform categorical data in numerical data...
pop <- read_csv("Data/places_coord.csv")
tablemodel2 <- tablemodel %>% mutate(occup = ifelse(occupation == "housewife", 1,
                                                    ifelse(occupation == "parttime", 2,
                                                           ifelse(occupation == "student", 3,
                                                                  ifelse(occupation == "fulltime", 4, 
                                                                         ifelse(occupation == "nothing", 5, 6)))))) %>%
  left_join(., pop, by = "place") %>%
  mutate(log10pop = log10(population))


