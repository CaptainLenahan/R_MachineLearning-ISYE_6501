missing <- which(bcancer$V7 == "?")

mod.bcancer <- bcancer[-missing, -c(1,11)]


mod.bcancer$V7 <- as.integer(mod.bcancer$V7)    
#create lm model

V7.model <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10, data = mod.bcancer)
summary(V7.model)
#TEST DEFAULT MODEL
AIC(V7.model) 
BIC(V7.model)
#eliminate insignificant factors based on p-values

train.control <- trainControl(method = "cv", number =10)

step.model <- train(V7~., data = mod.bcancer, method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:8), trControl = train.control)



#r-Squared of 0.618 for 4 vars!
step.model$results
#best # of variables = 4
step.model$bestTune
#best model appears to be with 6 predictors! 
step.final <- step.model$finalModel
#See significant vars used: model chose  V2-V5
summary(step.model) 

#preserve orig dataset
reg.bcancer <- bcancer
#regression impute
impute.list <- round(predict(step.model, newdata = miss.bcancer), 0)


to.impute <- c(4 ,  8  , 1 ,  2  , 1  , 2  , 
               3  , 1  , 2 ,  6 ,  1 ,  3 ,  
               6 ,  1 ,  1  , 1)



V7.impute <- fix.V7
x <- 1

for(i in 1:699){
  
  if(is.na(V7.impute[i]) == TRUE) 
  {
    
    V7.impute[i] = to.impute[x]
    x = x + 1
  }
  
  i = i + 1
  
} 

V7.impute
reg.bcancer$V7 <- V7.impute
reg.bcancer


nrow(bcancer[missing, ]

     