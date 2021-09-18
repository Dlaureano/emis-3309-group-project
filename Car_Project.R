#logistic regression LDA
#How does the age of the car affect the severity of the accident?
#Is there a relationship between the sex of the driver and the severity of the accident?
#How does weather conditions affect accident severity?
#Does point of impact affect accident severity?
#How does right hand/left hand driving affect accident severity?
  

accident <- read.csv("accidentdata.csv")
#remove all -1's
which(accident$weather_conditions == -1)
removevehicletype <- which(accident$vehicle_type == -1)
accident <- accident[-removevehicletype, ]
removejunloc <- which(accident$junction_location == -1)
accident <- accident[-removejunloc, ]
removepointofimpact <- which(accident$X1st_point_of_impact == -1)
accident <- accident[-removepointofimpact, ]
removelefthand <- which(accident$was_vehicle_left_hand_drive. == -1)
accident <- accident[-removelefthand, ]
removesexdriver <- which(accident$sex_of_driver == -1)
accident <- accident[-removesexdriver, ]
removeageofdriver <- which(accident$age_of_driver == -1)
accident <- accident[-removeageofdriver, ]
removeageofveh <- which(accident$age_of_vehicle == -1)
accident <- accident[-removeageofveh, ]
removeroadcondition <- which(accident$road_surface_conditions == -1)
accident <- accident[-removeroadcondition, ]
removepolice <- which(accident$did_police_officer_attend_scene_of_accident == -1)
accident <- accident [-removepolice, ]

#====================================================================
# Explore existing target variable
#====================================================================
str(accident)
# change data type: chr -> factor
accident$accident_index <- as.factor(accident$accident_index)
accident$accident_severity <- as.factor(accident$accident_severity)

table(accident$accident_severity)


which(is.na(accident$accident_severity))
str(accident)




#====================================================================
# Split train and test data sets
#====================================================================
#set seed
set.seed(100)
trainIdx <- sample(182958, 91479) # randomly pick 300 indices from 1-506
train <- accident[trainIdx, ] # save part of myData using above indices
test <- accident[-trainIdx, ] # save everything except for trainIdx

#====================================================================
#use train data to for model
#====================================================================
#LDA
# Linear Discriminant Analysis (LDA)
library(MASS) 
str(accident)
?lda
lda <- lda(accident_severity ~ . -accident_index, # change lda -> qda for QDA
           data = train)
summary(lda)
summary(lda)
lda$prior # P(No), P(Yes)
lda$means # average of each col per class (Yes/No)

# predict
lda_pred <- predict(lda, test)
summary(lda_pred) # class: predicted label, posterior: predicted prob
lda_pred$class[1:10]
test$accident_severity[1:10]

# confusion matrix 
table(lda_pred$class, test$accident_severity)
#lda_error <- (2+2+12530)/nrow(test) # 0.0141
mean(lda_pred$class != test$accident_severity)


     # plot predictions
plot(test$vehicle_type, test$accident_severity,
     pch = 7, # marker type
     cex = 0.5, # marker size
     col = lda_pred$class, # different col for each predicted class
     main = "LDA predictions",
     xlab = "Column Vehicle_type",
     ylab = "Column Accident_Severity")
legend("topleft", 
       legend = levels(lda_pred$class), # label info
       col = 1:2, 
       cex = 0.5, 
       pch = 7)

