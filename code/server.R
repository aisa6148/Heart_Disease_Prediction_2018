library(mice)
library(doParallel)
library(caret)
library(dplyr)
library(rpart)
library(rpart.plot)
library(corrplot)
library(tidyr)
library(ggplot2)
library(randomForest)
library(pROC)

server <- function(input, output) {
  observeEvent(input$submit,{
    output$plotx <- renderPlot({
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num>="1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      library(tidyr)
      
      gather(heart, x, y, age:thal) %>%
        ggplot(aes(x = y, color = num, fill = num)) +
        geom_density(alpha = 0.3) +
        facet_wrap( ~ x, scales = "free", ncol = 3)
    })
    output$ploty <- renderPlot({
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num>="1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      set.seed(42)
      index <- createDataPartition(heart$num, p = 0.7, list = FALSE)
      train_data <- heart[index, ]
      test_data  <- heart[-index, ]
      
      library(dplyr)
      
      rbind(data.frame(group = "train", train_data),
            data.frame(group = "test", test_data)) %>%
        gather(x, y, age:thal) %>%
        ggplot(aes(x = y, color = group, fill = group)) +
        geom_density(alpha = 0.3) +
        facet_wrap( ~ x, scales = "free", ncol = 3)
    })
  })
  
  observeEvent(input$submit1,{
    output$text1 <- renderText({
      
      
      age <- as.numeric(input$age)
      sex <- input$sex
      cp <- input$cp
      trestbps <- as.numeric(input$trestbps)
      chol <- as.numeric(input$chol)
      fbs <- as.numeric(input$fbs)
      restecg <- input$restecg
      thalach <- as.numeric(input$thalach)
      exang<- as.numeric(input$exang)
      oldpeak <- as.numeric(input$oldpeak)
      slope <- input$slope
      ca <- as.numeric(input$ca)
      thal <- as.numeric(input$thal)
      num <- as.factor(0)
      
      switch (sex,
              "Male" = {sex<-1},
              "Female" = {sex<-0}
      )
      
      switch (cp,
              "Typical angina" = {cp<-1},
              "Atypical angina" = {cp<-2},
              "Non-anginal pain" = {cp<-3},
              "Asymtomatic" = {cp<-4}
      )
      
      switch (restecg,
              "Normal" = {restecg<-0},
              "Abnormality" = {restecg<-1},
              "Ventricular hypertrosphy"= {restecg<-2}
      )
      
      switch (slope,
              "Upsloping" = {slope<-1},
              "Flat" = {slope<-2},
              "Downsloping"={slope<-3}
      )
      #paste(id, age, sex, cp, trestbps, cholestrol, fastingbloodsugar, restecg, thalach, exang, oldpeak, slope, ca, thal)
      
      #apply kclustering, if score above 50% then apply logisctic regression
      # if the lr score above 50% then apply ga, and finally the ann
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num >= "1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      heart
      
      validation_index <- createDataPartition(heart$num, p=0.70, list=FALSE)
      test <- heart[-validation_index,]
      heart <- heart[validation_index,]
      
      # summarize the class distribution
      percentage <- prop.table(table(heart$num)) * 100
      cbind(freq=table(heart$num), percentage=percentage)
      summary(heart)
      
      a<-data.frame("age"=age, "sex"=sex, "cp"=cp, "trestbps"=trestbps, "chol"=chol, "fbs"=fbs, "restecg"=restecg, "exang"= exang, "thalach"=thalach, "oldpeak"=oldpeak, "slope"=slope, "ca"=ca, "thal"=thal, "num"=num)      
      
      library(randomForest)
      
      set.seed(10)
      RFModel <- randomForest(num ~ .,
                              data=heart,
                              importance=TRUE,
                              ntree=2000)
      RFPrediction <- predict(RFModel, test)
      RFPredictionprob = predict(RFModel,test,type="prob")[, 2]
      
      RFConfMat <- confusionMatrix(RFPrediction, test[,"num"])
      
      Accuracy = list()
      Accuracy$RF <- RFConfMat$overall['Accuracy']  
      Accuracy
      
      if(RFPredictionprob >=1){
        display<-"The patient has a heart disease"
      }else
      {
        display<-"The patient does not have a heart disease"
      }
      paste("RANDOM FOREST: ",display)
    })
    
    output$plot1 <- renderPlot({
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num >= "1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      heart
      
      validation_index <- createDataPartition(heart$num, p=0.70, list=FALSE)
      test <- heart[-validation_index,]
      heart <- heart[validation_index,]
      
      # summarize the class distribution
      percentage <- prop.table(table(heart$num)) * 100
      cbind(freq=table(heart$num), percentage=percentage)
      summary(heart)
      
      set.seed(10)
      RFModel <- randomForest(num ~ .,
                              data=heart,
                              importance=TRUE,
                              ntree=2000)
      varImpPlot(RFModel)
      
    })
    output$conf1 <- renderTable({
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num >= "1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      heart
      
      validation_index <- createDataPartition(heart$num, p=0.70, list=FALSE)
      test <- heart[-validation_index,]
      heart <- heart[validation_index,]
      
      # summarize the class distribution
      percentage <- prop.table(table(heart$num)) * 100
      cbind(freq=table(heart$num), percentage=percentage)
      summary(heart)
      
      library(randomForest)
      
      set.seed(10)
      RFModel <- randomForest(num ~ .,
                              data=heart,
                              importance=TRUE,
                              ntree=2000)
      #varImpPlot(RFModel)
      RFPrediction <- predict(RFModel, test)
      RFPredictionprob = predict(RFModel,test,type="prob")[, 2]
      cm<-confusionMatrix(RFPrediction, test[,"num"])
      tocsv <- data.frame(cbind(t(cm$overall),t(cm$byClass)))
      tocsv
    })
    
  })
  observeEvent(input$submit2,{
    output$text2 <- renderText({
      age <- as.numeric(input$age)
      sex <- input$sex
      cp <- input$cp
      trestbps <- as.numeric(input$trestbps)
      chol <- as.numeric(input$chol)
      fbs <- as.numeric(input$fbs)
      restecg <- input$restecg
      thalach <- as.numeric(input$thalach)
      exang<- as.numeric(input$exang)
      oldpeak <- as.numeric(input$oldpeak)
      slope <- input$slope
      ca <- as.numeric(input$ca)
      thal <- as.numeric(input$thal)
      num <- as.factor(0)
      
      switch (sex,
              "Male" = {sex<-1},
              "Female" = {sex<-0}
      )
      
      switch (cp,
              "Typical angina" = {cp<-1},
              "Atypical angina" = {cp<-2},
              "Non-anginal pain" = {cp<-3},
              "Asymtomatic" = {cp<-4}
      )
      
      switch (restecg,
              "Normal" = {restecg<-0},
              "Abnormality" = {restecg<-1},
              "Ventricular hypertrosphy"= {restecg<-2}
      )
      
      switch (slope,
              "Upsloping" = {slope<-1},
              "Flat" = {slope<-2},
              "Downsloping"={slope<-3}
      )
      #paste(id, age, sex, cp, trestbps, cholestrol, fastingbloodsugar, restecg, thalach, exang, oldpeak, slope, ca, thal)
      
      #apply kclustering, if score above 50% then apply logisctic regression
      # if the lr score above 50% then apply ga, and finally the ann
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num >= "1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      heart
      
      validation_index <- createDataPartition(heart$num, p=0.70, list=FALSE)
      test <- heart[-validation_index,]
      heart <- heart[validation_index,]
      
      # summarize the class distribution
      percentage <- prop.table(table(heart$num)) * 100
      cbind(freq=table(heart$num), percentage=percentage)
      summary(heart)
      
      a<-data.frame("age"=age, "sex"=sex, "cp"=cp, "trestbps"=trestbps, "chol"=chol, "fbs"=fbs, "restecg"=restecg, "exang"= exang, "thalach"=thalach, "oldpeak"=oldpeak, "slope"=slope, "ca"=ca, "thal"=thal, "num"=num)      
      logistic <- glm(num ~ ., data = heart,family='binomial')
      summary(logistic)
      #Predict Output
      predicted= predict(logistic, a)
      summary(predicted)
      paste(predicted)
      if(predicted>0){
        display<-"The patient has a heart disease."
      }else
      {
        display<-"The patient does not have a heart disease."
      }
      paste("LOGISTIC REGRESSION: ",display)
    })
    output$plot2<- renderPlot({
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num >= "1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      
      heart$num <- as.factor(heart$num)
      
      validation_index <- createDataPartition(heart$num, p=0.70, list=FALSE)
      test <- heart[-validation_index,]
      heart <- heart[validation_index,]
      
      # summarize the class distribution
      percentage <- prop.table(table(heart$num)) * 100
      cbind(freq=table(heart$num), percentage=percentage)
      
      logistic <- glm(num ~ ., data = heart,family='binomial')
      predicted<- predict(logistic, test)
      
      data.frame(actual = test$age,
                 predicted = predicted) %>%
        ggplot(aes(x = actual, y = predicted)) +
        geom_jitter() +
        geom_smooth(method = "lm")
    })
    output$conf2<- renderTable({
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num >= "1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      heart
      
      validation_index <- createDataPartition(heart$num, p=0.70, list=FALSE)
      test <- heart[-validation_index,]
      heart <- heart[validation_index,]
      
      # summarize the class distribution
      percentage <- prop.table(table(heart$num)) * 100
      cbind(freq=table(heart$num), percentage=percentage)
      summary(heart)
      set.seed(10)
      logRegModel <- train(num ~ ., data=heart, method = 'glm', family = 'binomial')
      logRegPrediction <- predict(logRegModel, test)
      logRegPredictionprob <- predict(logRegModel, test, type='prob')[2]
      cm2 <- confusionMatrix(logRegPrediction, test[,"num"])
      tocsv <- data.frame(cbind(t(cm2$overall),t(cm2$byClass)))
      tocsv
    })
  })
  observeEvent(input$submit3,{
    output$text3 <- renderText({
      age <- as.numeric(input$age)
      sex <- input$sex
      cp <- input$cp
      trestbps <- as.numeric(input$trestbps)
      chol <- as.numeric(input$chol)
      fbs <- as.numeric(input$fbs)
      restecg <- input$restecg
      thalach <- as.numeric(input$thalach)
      exang<- as.numeric(input$exang)
      oldpeak <- as.numeric(input$oldpeak)
      slope <- input$slope
      ca <- as.numeric(input$ca)
      thal <- as.numeric(input$thal)
      num <- as.factor(0)
      
      switch (sex,
              "Male" = {sex<-1},
              "Female" = {sex<-0}
      )
      
      switch (cp,
              "Typical angina" = {cp<-1},
              "Atypical angina" = {cp<-2},
              "Non-anginal pain" = {cp<-3},
              "Asymtomatic" = {cp<-4}
      )
      
      switch (restecg,
              "Normal" = {restecg<-0},
              "Abnormality" = {restecg<-1},
              "Ventricular hypertrosphy"= {restecg<-2}
      )
      
      switch (slope,
              "Upsloping" = {slope<-1},
              "Flat" = {slope<-2},
              "Downsloping"={slope<-3}
      )
      
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      
      
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))
      
      a<- data.frame("age"=age, "sex"=sex, "cp"=cp, "trestbps"=trestbps, "chol"=chol, "fbs"=fbs, "restecg"=restecg, "thalach"=thalach, "exang"= exang, "oldpeak"=oldpeak, "slope"=slope, "ca"=ca, "thal"=thal, "num"=0)
      
      
      set.seed(3033)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- heart_df[index, ]
      test_data  <- heart_df[-index, ]
      
      train_data[["num"]] = factor(train_data[["num"]])
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3233)
      
      svm_Linear <- train(num ~., data = train_data, method = "svmLinear",
                          trControl=trctrl,
                          preProcess = c("center", "scale"),
                          tuneLength = 10)
      
      test_pred <- predict(svm_Linear, newdata = a)
      if(test_pred==1){
        display<-"The patient has a heart disease"
      }
      else
       display<-"The patient does not have a heart disease"
      paste("SUPPORT VECTOR MACHINE: ", display)
    })
    output$plot3<- renderPlot({
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      
      
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))
      set.seed(3033)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- heart_df[index, ]
      test_data  <- heart_df[-index, ]
      
      train_data[["num"]] = factor(train_data[["num"]])
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3233)
      grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
      set.seed(3233)
      svm_Linear_Grid <- train(num ~., data = train_data, method = "svmLinear",
                               trControl=trctrl,
                               preProcess = c("center", "scale"),
                               tuneGrid = grid,
                               tuneLength = 10)
      plot(svm_Linear_Grid)
    })
    output$conf3<- renderTable({
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      
      
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))
      
      set.seed(3033)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- heart_df[index, ]
      test_data  <- heart_df[-index, ]
      
      train_data[["num"]] = factor(train_data[["num"]])
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

      grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
      set.seed(3233)
      svmModel <- train(num ~., data = train_data, method = "svmLinear",
                               trControl=trctrl,
                               preProcess = c("center", "scale"),
                               tuneGrid = grid,
                               tuneLength = 10)
      svmPrediction <- predict(svmModel, test_data)
      svmPredictionprob <- predict(svmModel, test_data, type='prob')[2]
      cm3 <- confusionMatrix(svmPrediction, test_data[,"num"])
      tocsv <- data.frame(cbind(t(cm3$overall),t(cm3$byClass)))
      tocsv
    })
  })
  observeEvent(input$submit4,{
    output$text4 <- renderText({
      age <- as.numeric(input$age)
      sex <- input$sex
      cp <- input$cp
      trestbps <- as.numeric(input$trestbps)
      chol <- as.numeric(input$chol)
      fbs <- as.numeric(input$fbs)
      restecg <- input$restecg
      thalach <- as.numeric(input$thalach)
      exang<- as.numeric(input$exang)
      oldpeak <- as.numeric(input$oldpeak)
      slope <- input$slope
      ca <- as.numeric(input$ca)
      thal <- as.numeric(input$thal)
      num <- as.factor(0)
      
      switch (sex,
              "Male" = {sex<-1},
              "Female" = {sex<-0}
      )
      
      switch (cp,
              "Typical angina" = {cp<-1},
              "Atypical angina" = {cp<-2},
              "Non-anginal pain" = {cp<-3},
              "Asymtomatic" = {cp<-4}
      )
      
      switch (restecg,
              "Normal" = {restecg<-0},
              "Abnormality" = {restecg<-1},
              "Ventricular hypertrosphy"= {restecg<-2}
      )
      
      switch (slope,
              "Upsloping" = {slope<-1},
              "Flat" = {slope<-2},
              "Downsloping"={slope<-3}
      )
      
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))
      
      a<-data.frame("age"=age, "sex"=sex, "cp"=cp, "trestbps"=trestbps, "chol"=chol, "fbs"=fbs, "restecg"=restecg, "exang"= exang, "thalach"=thalach, "oldpeak"=oldpeak, "slope"=slope, "ca"=ca, "thal"=thal, "num"=num)
      
      set.seed(3033)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- heart_df[index, ]
      test_data  <- heart_df[-index, ]
      
      train_data[["num"]] = factor(train_data[["num"]])
      
      a<- data.frame("age"=age, "sex"=sex, "cp"=cp, "trestbps"=trestbps, "chol"=chol, "fbs"=fbs, "restecg"=restecg, "thalach"=thalach, "exang"= exang, "oldpeak"=oldpeak, "slope"=slope, "ca"=ca, "thal"=thal, "num"=0)
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3333)
      knn_fit <- train(num ~., data = train_data, method = "knn",
                       trControl=trctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
      
      test_pred2 <- predict(knn_fit, newdata = a)
      
      
      if(test_pred2==0)
      {
        display<-"The patient does not have a heart disease"
      }
      else
      {
        display<-"The patient has a heart disease"
      }
      
      paste("K- NEAREST NEIGHBORS",display)
    })
    output$plot4<- renderPlot({
      
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))
      
      set.seed(3033)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- heart_df[index, ]
      test_data  <- heart_df[-index, ]
      
      train_data[["num"]] = factor(train_data[["num"]])
      
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3333)
      knn_fit <- train(num ~., data = train_data, method = "knn",
                       trControl=trctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
      plot(knn_fit)
    })
    output$conf4<- renderTable({
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))
    
      set.seed(3033)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- heart_df[index, ]
      test_data  <- heart_df[-index, ]
      
      train_data[["num"]] = factor(train_data[["num"]])
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      set.seed(3333)
      knn_fit <- train(num ~., data = train_data, method = "knn",
                       trControl=trctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
      test_pred2 <- predict(knn_fit, test_data)
      knnPredictionprob <- predict(knn_fit, test_data, type='prob')[2]
      cm4<-confusionMatrix(test_pred2, test_data[,"num"])
      tocsv <- data.frame(cbind(t(cm4$overall),t(cm4$byClass)))
      tocsv
    })
  })
  observeEvent(input$submit5,{
    output$text5 <- renderText({
      age <- as.numeric(input$age)
      sex <- input$sex
      cp <- input$cp
      trestbps <- as.numeric(input$trestbps)
      chol <- as.numeric(input$chol)
      fbs <- as.numeric(input$fbs)
      restecg <- input$restecg
      thalach <- as.numeric(input$thalach)
      exang<- as.numeric(input$exang)
      oldpeak <- as.numeric(input$oldpeak)
      slope <- input$slope
      ca <- as.numeric(input$ca)
      thal <- as.numeric(input$thal)
      num <- as.factor(0)
      
      switch (sex,
              "Male" = {sex<-1},
              "Female" = {sex<-0}
      )
      
      switch (cp,
              "Typical angina" = {cp<-1},
              "Atypical angina" = {cp<-2},
              "Non-anginal pain" = {cp<-3},
              "Asymtomatic" = {cp<-4}
      )
      
      switch (restecg,
              "Normal" = {restecg<-0},
              "Abnormality" = {restecg<-1},
              "Ventricular hypertrosphy"= {restecg<-2}
      )
      
      switch (slope,
              "Upsloping" = {slope<-1},
              "Flat" = {slope<-2},
              "Downsloping"={slope<-3}
      )
      
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))
      
      set.seed(3033)
      library(e1071)
      library(caret)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- as.data.frame(heart_df[index, ])
      test_data  <- as.data.frame(heart_df[-index, ])
      
      a<- data.frame("age"=age, "sex"=sex, "cp"=cp, "trestbps"=trestbps, "chol"=chol, "fbs"=fbs, "restecg"=restecg, "thalach"=thalach, "exang"= exang, "oldpeak"=oldpeak, "slope"=slope, "ca"=ca, "thal"=thal, "num"=0)   
      tprior<-table(train_data$num)
      tprior
      tprior<-tprior/sum(tprior)
      tprior
      model<-naiveBayes(as.factor(num)~.,data=train_data)
      model
      summary(model)
      library(e1071)
      results<-predict(model,a)

      if(results==0){
        paste("NAIVE BAYES: The patient does not have a heart disease")
      }
      else
        paste("NAIVE BAYES: The patient has a heart disease")
    })
    #output$plot5<- renderPlot({
    
    #})
    output$conf5<- renderTable({
      heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart_df) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                            "exang", "oldpeak","slope", "ca", "thal", "num")
      heart_df$num <- ifelse(heart_df$num >= "1", "1",
                             ifelse(heart_df$num == "0", "0", NA))

      set.seed(3033)
      library(e1071)
      library(caret)
      index <- createDataPartition(heart_df$num, p = 0.7, list = FALSE)
      train_data <- as.data.frame(heart_df[index, ])
      test_data  <- as.data.frame(heart_df[-index, ])
      tprior<-table(train_data$num)
      tprior
      tprior<-tprior/sum(tprior)
      tprior
      model<-naiveBayes(as.factor(num)~.,data=train_data)
      model
      summary(model)
      library(e1071)
      results<-predict(model,a)
      results
      test_data
      plot(results)
      
      svmPrediction <- predict(model, test_data)
      #svmPredictionprob <- predict(model, test_data, type='prob')[2]
      cm5 <- confusionMatrix(svmPrediction, test_data[,"num"])
      cm5
      tocsv <- data.frame(cbind(t(cm5$overall),t(cm5$byClass)))
      tocsv

    })
  })
  observeEvent(input$submit6,{
    output$text6 <- renderText({
      age <- as.numeric(input$age)
      sex <- input$sex
      cp <- input$cp
      trestbps <- as.numeric(input$trestbps)
      chol <- as.numeric(input$chol)
      fbs <- as.numeric(input$fbs)
      restecg <- input$restecg
      thalach <- as.numeric(input$thalach)
      exang<- as.numeric(input$exang)
      oldpeak <- as.numeric(input$oldpeak)
      slope <- input$slope
      ca <- as.numeric(input$ca)
      thal <- as.numeric(input$thal)
      num <- as.factor(0)
      
      switch (sex,
              "Male" = {sex<-1},
              "Female" = {sex<-0}
      )
      
      switch (cp,
              "Typical angina" = {cp<-1},
              "Atypical angina" = {cp<-2},
              "Non-anginal pain" = {cp<-3},
              "Asymtomatic" = {cp<-4}
      )
      
      switch (restecg,
              "Normal" = {restecg<-0},
              "Abnormality" = {restecg<-1},
              "Ventricular hypertrosphy"= {restecg<-2}
      )
      
      switch (slope,
              "Upsloping" = {slope<-1},
              "Flat" = {slope<-2},
              "Downsloping"={slope<-3}
      )
      #paste(id, age, sex, cp, trestbps, cholestrol, fastingbloodsugar, restecg, thalach, exang, oldpeak, slope, ca, thal)
      
      #apply kclustering, if score above 50% then apply logisctic regression
      # if the lr score above 50% then apply ga, and finally the ann
      heart<- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
      names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg", "thalach",
                         "exang", "oldpeak","slope", "ca", "thal", "num")
      heart$num <- ifelse(heart$num >= "1", "1",
                          ifelse(heart$num == "0", "0", NA))
      heart <- na.omit(heart)
      
      heart$num <- as.factor(heart$num)
      summary(heart$num)
      heart
      
      validation_index <- createDataPartition(heart$num, p=0.70, list=FALSE)
      test <- heart[-validation_index,]
      heart <- heart[validation_index,]
      
      # summarize the class distribution
      percentage <- prop.table(table(heart$num)) * 100
      cbind(freq=table(heart$num), percentage=percentage)
      summary(heart)
      
      a<-data.frame("age"=age, "sex"=sex, "cp"=cp, "trestbps"=trestbps, "chol"=chol, "fbs"=fbs, "restecg"=restecg, "exang"= exang, "thalach"=thalach, "oldpeak"=oldpeak, "slope"=slope, "ca"=ca, "thal"=thal, "num"=num)      
      logistic <- glm(num ~ ., data = heart,family='binomial')
      summary(logistic)
      #Predict Output
      predicted= predict(logistic, a)
      summary(predicted)
      paste(predicted)
      if(predicted>0){
        x<-1
      }else
      {
        x<-0
      }
      b<-data.frame(age, sex, cp, trestbps, chol, fbs, restecg, exang, thalach, oldpeak, slope, ca, thal, x)
      write.table( b,  
                   file="./heart_tidy.csv", 
                   append = T, 
                   sep=',', 
                   row.names=F, 
                   col.names=F )
      paste("New data added")
      
    })
  })
  
  
  
}