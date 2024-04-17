library(readxl)
#Book1 <- read_excel("C:/Users/HP/Desktop/24 mes with.xlsx")
#Book1 <- read_excel("C:/Users/HP/Desktop/24 mes without.xlsx")
#Book1 <- read_excel("C:/Users/HP/Desktop/12 mes without.xlsx")
#Book1 <- read_excel("C:/Users/HP/Desktop/1 mes without.xlsx")
#Book1 <- read_excel("C:/Users/HP/Desktop/12 mes without cepo.xlsx")
#View(Book1)

#set seed
set.seed(123)

# Stratified
submuestra_no_crisis = which(Book1$Crisis_tag=="0")
submuestra_si_crisis = which(Book1$Crisis_tag=="1")
train_no = sample(submuestra_no_crisis, length(submuestra_no_crisis)*0.75)
train_si = sample(submuestra_si_crisis, length(submuestra_si_crisis)*0.75)
train_sample = sort(c(train_no, train_si))
train = Book1[train_sample, ]
test = Book1[-train_sample, ]

crisis_1=which(test$Crisis_tag=="1")
crisis_0=which(test$Crisis_tag=="0")

##############################################################################################
# CART 
##############################################################################################

# Librerias
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

#######################################################################################################
# Cambio de Criterio de impureza: criterio de informacion CV 10 folds
tree_infor=rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=10),parms = list(split= 'information'))
fancyRpartPlot(tree_infor)
plotcp(tree_infor)
printcp(tree_infor)
bestcp <- tree_infor$cptable[which.min(tree_infor$cptable[,"xerror"]),"CP"]
bestcp
pruned_infor <- prune(tree_infor, cp = bestcp)
fancyRpartPlot(pruned_infor)

# Predict
pred.prune = predict(pruned_infor, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(pruned_infor, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value


#######################################################################################################
# Cambio de Criterio de impureza: criterio de informacion cv 5 folds
tree_infor=rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=5),parms = list(split= 'information'))
fancyRpartPlot(tree_infor)
plotcp(tree_infor)
printcp(tree_infor)
bestcp <- tree_infor$cptable[which.min(tree_infor$cptable[,"xerror"]),"CP"]
bestcp
pruned_infor <- prune(tree_infor, cp = bestcp)
fancyRpartPlot(pruned_infor)

# Predict
pred.prune = predict(pruned_infor, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(pruned_infor, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value

#######################################################################################################
# Cambio de Criterio de impureza: criterio entropy cv 10 folds
tit.tree.entropy <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0, xval=10),parms = list(split= 'entropy'))
fancyRpartPlot(tit.tree.entropy)
tit.tree.entropy <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=10),parms = list(split= 'entropy'))
plotcp(tit.tree.entropy)
printcp(tit.tree.entropy)
bestcp <- tit.tree.entropy$cptable[which.min(tit.tree.entropy$cptable[,"xerror"]),"CP"]
bestcp
tit.pruned.entropy <- prune(tit.tree.entropy, cp = bestcp)
fancyRpartPlot(tit.pruned.entropy)

# Predict
pred.prune = predict(tit.pruned.entropy, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(tit.pruned.entropy, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value

#######################################################################################################
# Cambio de Criterio de impureza: criterio entropy cv 5 folds
tit.tree.entropy <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0, xval=5),parms = list(split= 'entropy'))
fancyRpartPlot(tit.tree.entropy)
tit.tree.entropy <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=5),parms = list(split= 'entropy'))
plotcp(tit.tree.entropy)
printcp(tit.tree.entropy)
bestcp <- tit.tree.entropy$cptable[which.min(tit.tree.entropy$cptable[,"xerror"]),"CP"]
bestcp
tit.pruned.entropy <- prune(tit.tree.entropy, cp = bestcp)
fancyRpartPlot(tit.pruned.entropy)

# Predict
pred.prune = predict(tit.pruned.entropy, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(tit.pruned.entropy, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value

#######################################################################################################
# Cambio de Criterio de impureza: criterio de gini cv 10 folds
tit.tree.gini <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0, xval=10),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
tit.tree.gini <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=10),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
plotcp(tit.tree.gini)
printcp(tit.tree.gini)
bestcp <- tit.tree.gini$cptable[which.min(tit.tree.gini$cptable[,"xerror"]),"CP"]
bestcp
tit.pruned.gini <- prune(tit.tree.gini, cp = bestcp)
fancyRpartPlot(tit.pruned.gini)

# Predict
pred.prune = predict(tit.pruned.gini, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)


# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(tit.pruned.gini, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value


#######################################################################################################
# Cambio de Criterio de impureza: criterio de gini cv 5 folds
tit.tree.gini <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0, xval=5),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
tit.tree.gini <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=5),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
plotcp(tit.tree.gini)
printcp(tit.tree.gini)
bestcp <- tit.tree.gini$cptable[which.min(tit.tree.gini$cptable[,"xerror"]),"CP"]
bestcp
tit.pruned.gini <- prune(tit.tree.gini, cp = bestcp)
fancyRpartPlot(tit.pruned.gini)

# Predict
pred.prune = predict(tit.pruned.gini, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(tit.pruned.gini, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value



##############################################################################################
# BA
##############################################################################################

library (randomForest)

# eliminar rows con omisiones
train=na.omit(train)

#Importante que la variable de respuesta sea categorica sino hace bagging pero con regresion.
train$Crisis_tag=as.factor(train$Crisis_tag)

#mtry=88 nro de variables, al poner todas bagging es un caso particular de RF
bag.tit = randomForest(Crisis_tag ~ . , data=train,ntree=10000,mtry=88, importance =TRUE, maxnodes=30)

#Analizamos la salida. Nos indica la tasa de error de las observaciones out of the bag (OOB) y tambien da la matriz de confusion para los mismos datos.
bag.tit

# Grafica las tasas de error globales y para cada categoria a medida que toma mas muestras bootstrap
plot(bag.tit, main = "")
legend("topright", legend = colnames(bag.tit$err.rate), col = 1:ncol(bag.tit$err.rate), lty = 1, cex = 1, bty = "n")

# Muestra la importancia de cada variable, dependiendo del criteio de impureza considerado.
varImpPlot(bag.tit,sort = TRUE)

#saco la etiqueta
xtest=test[,-c(1,1)] 

#Prediccion en un nuevo conjunto de datos
pred.tit.bag.v = predict (bag.tit ,newdata =xtest,type = "vote")#da la proporcion de arboles que eligen cada clase.

#Pongo la etiqueta por el voto mayoritario de los arboles
pred.tit.bag.label=rep(NA,dim(test)[1]) #como si hubiera missing values genero un vector de ceros

aux1=which(pred.tit.bag.v[,2]>0.50) 
aux2=which(pred.tit.bag.v[,2]<=0.50)
pred.tit.bag.label[aux1]=1
pred.tit.bag.label[aux2]=0

#confusion matrix (test data)
conf.matrix <- table(test$Crisis_tag, pred.tit.bag.label)
rownames(conf.matrix) <- paste("Real", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")


#Me fijo como mirar el resultado a traves de las probabilidades a posteriori y armar la curva ROC
pred.tit.bag.prob = predict (bag.tit ,newdata =xtest,type = "prob")#da la proababilidad de elegir cada clase.

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.bag.prob[,1])
rocbag
ggroc(rocbag)


# Load the ROCR package
library(ROCR)
pred_probs <- predict(bag.tit, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value


##############################################################################################
# RF mtry=21
##############################################################################################

library (randomForest)

# eliminar rows con omisiones
train=na.omit(train)

#Importante que la variable de respuesta sea categorica sino hace bagging pero con regresion.
train$Crisis_tag=as.factor(train$Crisis_tag)

#mtry=97 nro de variables, al poner todas bagging es un caso particular de RF
bag.tit = randomForest(Crisis_tag ~ . , data=train,ntree=10000,mtry=55, importance =TRUE, maxnodes=30)

#Analizamos la salida. Nos indica la tasa de error de las observaciones out of the bag (OOB) y tambien da la matriz de confusion para los mismos datos.
bag.tit

# Grafica las tasas de error globales y para cada categoria a medida que toma mas muestras bootstrap
plot(bag.tit, main = "")
legend("topright", legend = colnames(bag.tit$err.rate), col = 1:ncol(bag.tit$err.rate), lty = 1, cex = 1, bty = "n")

# Muestra la importancia de cada variable, dependiendo del criteio de impureza considerado.
varImpPlot(bag.tit,sort = TRUE)

#saco la etiqueta
xtest=test[,-c(1,1)] 

#Prediccion en un nuevo conjunto de datos
pred.tit.bag.v = predict (bag.tit ,newdata =xtest,type = "vote")#da la proporcion de arboles que eligen cada clase.

#Pongo la etiqueta por el voto mayoritario de los arboles
pred.tit.bag.label=rep(NA,dim(test)[1]) #como si hubiera missing values genero un vector de ceros

aux1=which(pred.tit.bag.v[,2]>0.5) 
aux2=which(pred.tit.bag.v[,2]<=0.5)
pred.tit.bag.label[aux1]=1
pred.tit.bag.label[aux2]=0

#confusion matrix (test data)
conf.matrix <- table(test$Crisis_tag, pred.tit.bag.label)
rownames(conf.matrix) <- paste("Real", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")


#Me fijo como mirar el resultado a traves de las probabilidades a posteriori y armar la curva ROC
pred.tit.rf.prob = predict (bag.tit ,newdata =xtest,type = "prob")#da la proababilidad de elegir cada clase.

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.rf.prob[,1])
rocbag
ggroc(rocbag)

##############################################################################################
# RF con mtry cross validado 5 folds
##############################################################################################

library (randomForest)
library(caret)

# eliminar rows con omisiones
train=na.omit(train)

#Importante que la variable de respuesta sea categorica sino hace bagging pero con regresion.
train$Crisis_tag=as.factor(train$Crisis_tag)

# Define the training control
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Define the grid of mtry values to search over
mtry_grid <- expand.grid(mtry = seq(1, 88, by = 1))  # Change the range and step size as needed

# Train the model using cross-validation
rf_model <- train(Crisis_tag ~ ., data = train, method = "rf",
                  trControl = ctrl, tuneGrid = mtry_grid)

# Print the results
print(rf_model)
mtry_cv=rf_model[["bestTune"]][["mtry"]]

#mtry nro de variables, al poner todas bagging es un caso particular de RF
bag.tit = randomForest(Crisis_tag ~ . , data=train,ntree=10000,mtry=mtry_cv, importance =TRUE)

#Analizamos la salida. Nos indica la tasa de error de las observaciones out of the bag (OOB) y tambien da la matriz de confusion para los mismos datos.
bag.tit

# Grafica las tasas de error globales y para cada categoria a medida que toma mas muestras bootstrap
plot(bag.tit, main = "")
legend("topright", legend = colnames(bag.tit$err.rate), col = 1:ncol(bag.tit$err.rate), lty = 1, cex = 1, bty = "n")

# Muestra la importancia de cada variable, dependiendo del criteio de impureza considerado.
varImpPlot(bag.tit,sort = TRUE)

#saco la etiqueta
xtest=test[,-c(1,1)] 

#Prediccion en un nuevo conjunto de datos
pred.tit.bag.v = predict (bag.tit ,newdata =xtest,type = "vote")#da la proporcion de arboles que eligen cada clase.

#Pongo la etiqueta por el voto mayoritario de los arboles
pred.tit.bag.label=rep(NA,dim(test)[1]) #como si hubiera missing values genero un vector de ceros

aux1=which(pred.tit.bag.v[,2]>0.5) 
aux2=which(pred.tit.bag.v[,2]<=0.5)
pred.tit.bag.label[aux1]=1
pred.tit.bag.label[aux2]=0

#confusion matrix (test data)
conf.matrix <- table(test$Crisis_tag, pred.tit.bag.label)
rownames(conf.matrix) <- paste("Real", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")


#Me fijo como mirar el resultado a traves de las probabilidades a posteriori y armar la curva ROC
pred.tit.rf.prob = predict (bag.tit ,newdata =xtest,type = "prob")#da la proababilidad de elegir cada clase.

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.rf.prob[,1])
rocbag
ggroc(rocbag)

# Load the ROCR package
library(ROCR)
pred_probs <- predict(bag.tit, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value


##############################################################################################
# RF con mtry cross validado 10 folds
##############################################################################################

library (randomForest)
library(caret)

# eliminar rows con omisiones
train=na.omit(train)

#Importante que la variable de respuesta sea categorica sino hace bagging pero con regresion.
train$Crisis_tag=as.factor(train$Crisis_tag)

# Define the training control
ctrl <- trainControl(method = "cv", number = 10)  # 5-fold cross-validation

# Define the grid of mtry values to search over
mtry_grid <- expand.grid(mtry = seq(1, 88, by = 1))  # Change the range and step size as needed

# Train the model using cross-validation
rf_model <- train(Crisis_tag ~ ., data = train, method = "rf",
                  trControl = ctrl, tuneGrid = mtry_grid)

# Print the results
print(rf_model)
mtry_cv=rf_model[["bestTune"]][["mtry"]]

#mtry nro de variables, al poner todas bagging es un caso particular de RF
bag.tit10 = randomForest(Crisis_tag ~ . , data=train,ntree=10000,mtry=mtry_cv, importance =TRUE)

#Analizamos la salida. Nos indica la tasa de error de las observaciones out of the bag (OOB) y tambien da la matriz de confusion para los mismos datos.
bag.tit10

# Grafica las tasas de error globales y para cada categoria a medida que toma mas muestras bootstrap
plot(bag.tit10, main = "")
legend("topright", legend = colnames(bag.tit10$err.rate), col = 1:ncol(bag.tit10$err.rate), lty = 1, cex = 1, bty = "n")

# Muestra la importancia de cada variable, dependiendo del criteio de impureza considerado.
varImpPlot(bag.tit10,sort = TRUE)

#saco la etiqueta
xtest=test[,-c(1,1)] 

#Prediccion en un nuevo conjunto de datos
pred.tit.bag.v = predict (bag.tit10 ,newdata =xtest,type = "vote")#da la proporcion de arboles que eligen cada clase.

#Pongo la etiqueta por el voto mayoritario de los arboles
pred.tit.bag.label=rep(NA,dim(test)[1]) #como si hubiera missing values genero un vector de ceros

aux1=which(pred.tit.bag.v[,2]>0.5) 
aux2=which(pred.tit.bag.v[,2]<=0.5)
pred.tit.bag.label[aux1]=1
pred.tit.bag.label[aux2]=0

#confusion matrix (test data)
conf.matrix <- table(test$Crisis_tag, pred.tit.bag.label)
rownames(conf.matrix) <- paste("Real", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")


#Me fijo como mirar el resultado a traves de las probabilidades a posteriori y armar la curva ROC
pred.tit.rf.prob = predict (bag.tit10 ,newdata =xtest,type = "prob")#da la proababilidad de elegir cada clase.

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.rf.prob[,1])
rocbag
ggroc(rocbag)

# Load the ROCR package
library(ROCR)
pred_probs <- predict(bag.tit10, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value

##############################################################################################
# RF con mtry heuristica
##############################################################################################

library (randomForest)
library(caret)

# eliminar rows con omisiones
train=na.omit(train)

#Importante que la variable de respuesta sea categorica sino hace bagging pero con regresion.
train$Crisis_tag=as.factor(train$Crisis_tag)

mtry_cv=round(sqrt(88))
mtry_cv

#mtry nro de variables, al poner todas bagging es un caso particular de RF
bag.tit = randomForest(Crisis_tag ~ . , data=train,ntree=10000,mtry=mtry_cv, importance =TRUE)

#Analizamos la salida. Nos indica la tasa de error de las observaciones out of the bag (OOB) y tambien da la matriz de confusion para los mismos datos.
bag.tit

# Grafica las tasas de error globales y para cada categoria a medida que toma mas muestras bootstrap
plot(bag.tit, main = "")
legend("topright", legend = colnames(bag.tit$err.rate), col = 1:ncol(bag.tit$err.rate), lty = 1, cex = 1, bty = "n")

# Muestra la importancia de cada variable, dependiendo del criteio de impureza considerado.
varImpPlot(bag.tit,sort = TRUE)

#saco la etiqueta
xtest=test[,-c(1,1)] 

#Prediccion en un nuevo conjunto de datos
pred.tit.bag.v = predict (bag.tit ,newdata =xtest,type = "vote")#da la proporcion de arboles que eligen cada clase.

#Pongo la etiqueta por el voto mayoritario de los arboles
pred.tit.bag.label=rep(NA,dim(test)[1]) #como si hubiera missing values genero un vector de ceros

aux1=which(pred.tit.bag.v[,2]>0.5) 
aux2=which(pred.tit.bag.v[,2]<=0.5)
pred.tit.bag.label[aux1]=1
pred.tit.bag.label[aux2]=0

#confusion matrix (test data)
conf.matrix <- table(test$Crisis_tag, pred.tit.bag.label)
rownames(conf.matrix) <- paste("Real", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")


#Me fijo como mirar el resultado a traves de las probabilidades a posteriori y armar la curva ROC
pred.tit.rf.prob = predict (bag.tit ,newdata =xtest,type = "prob")#da la proababilidad de elegir cada clase.

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.rf.prob[,1])
rocbag
ggroc(rocbag)

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.rf.prob[,1])
rocbag
ggroc(rocbag)


# Load the ROCR package
library(ROCR)
pred_probs <- predict(bag.tit10, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value

#######################################################################################################
# Cambio de Criterio de impureza: criterio de gini cv 10 folds
tit.tree.gini <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0, xval=10),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
tit.tree.gini <- rpart(Crisis_tag ~ ., data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=10),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
plotcp(tit.tree.gini)
printcp(tit.tree.gini)
bestcp <- tit.tree.gini$cptable[which.min(tit.tree.gini$cptable[,"xerror"]),"CP"]
bestcp
tit.pruned.gini <- prune(tit.tree.gini, cp = bestcp)
fancyRpartPlot(tit.pruned.gini)

# Predict
pred.prune = predict(tit.pruned.gini, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)


# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(tit.pruned.gini, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value


#######################################################################################################
# Cambio de Criterio de impureza: criterio de gini cv 10 folds (sin variables fiscales)
tit.tree.gini <- rpart(Crisis_tag ~ . - v34 - v35 - v36 - v37 - v38 - v39 -v40 - v54 - v55 -v57 -v59 -v60 -v33, data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0, xval=10),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
tit.tree.gini <- rpart(Crisis_tag ~ . - v34 - v35 - v36 - v37 - v38 - v39 -v40 - v54 - v55 -v57 -v59 -v60 -v33, data=train,method="class",control = rpart.control(minsplit=2, minbucket = 1, maxdepth = 30, cp = 0.0001, xval=10),parms = list(split= 'gini'))
fancyRpartPlot(tit.tree.gini)
plotcp(tit.tree.gini)
printcp(tit.tree.gini)
bestcp <- tit.tree.gini$cptable[which.min(tit.tree.gini$cptable[,"xerror"]),"CP"]
bestcp
tit.pruned.gini <- prune(tit.tree.gini, cp = bestcp)
fancyRpartPlot(tit.pruned.gini)

# Predict
pred.prune = predict(tit.pruned.gini, test, type = "class")

#Matriz de confusion  (test data)
conf.matrix <- table(test$Crisis_tag, pred.prune)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")

# Load the ROCR package
library(ROCR)
pred_probs <- predict(tit.pruned.gini, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value

##############################################################################################
# BA sin variables fiscales
##############################################################################################

library (randomForest)

# eliminar rows con omisiones
train=na.omit(train)

#Importante que la variable de respuesta sea categorica sino hace bagging pero con regresion.
train$Crisis_tag=as.factor(train$Crisis_tag)

#mtry=88 nro de variables, al poner todas bagging es un caso particular de RF
bag.tit = randomForest(Crisis_tag ~ . - v34 - v35 - v36 - v37 - v38 - v39 -v40 - v54 - v55 -v57 -v59 -v60 -v33 -v35, data=train,ntree=10000,mtry=75, importance =TRUE, maxnodes=30)

#Analizamos la salida. Nos indica la tasa de error de las observaciones out of the bag (OOB) y tambien da la matriz de confusion para los mismos datos.
bag.tit

# Grafica las tasas de error globales y para cada categoria a medida que toma mas muestras bootstrap
plot(bag.tit, main = "")
legend("topright", legend = colnames(bag.tit$err.rate), col = 1:ncol(bag.tit$err.rate), lty = 1, cex = 1, bty = "n")

# Muestra la importancia de cada variable, dependiendo del criteio de impureza considerado.
varImpPlot(bag.tit,sort = TRUE)

#saco la etiqueta
xtest=test[,-c(1,1)] 

#Prediccion en un nuevo conjunto de datos
pred.tit.bag.v = predict (bag.tit ,newdata =xtest,type = "vote")#da la proporcion de arboles que eligen cada clase.

#Pongo la etiqueta por el voto mayoritario de los arboles
pred.tit.bag.label=rep(NA,dim(test)[1]) #como si hubiera missing values genero un vector de ceros

aux1=which(pred.tit.bag.v[,2]>0.50) 
aux2=which(pred.tit.bag.v[,2]<=0.50)
pred.tit.bag.label[aux1]=1
pred.tit.bag.label[aux2]=0

#confusion matrix (test data)
conf.matrix <- table(test$Crisis_tag, pred.tit.bag.label)
rownames(conf.matrix) <- paste("Real", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")


#Me fijo como mirar el resultado a traves de las probabilidades a posteriori y armar la curva ROC
pred.tit.bag.prob = predict (bag.tit ,newdata =xtest,type = "prob")#da la proababilidad de elegir cada clase.

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.bag.prob[,1])
rocbag
ggroc(rocbag)


# Load the ROCR package
library(ROCR)
pred_probs <- predict(bag.tit, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value

##############################################################################################
# RF con mtry cross validado 10 folds (sin variables fiscales)
##############################################################################################

library (randomForest)
library(caret)

# eliminar rows con omisiones
train=na.omit(train)

#Importante que la variable de respuesta sea categorica sino hace bagging pero con regresion.
train$Crisis_tag=as.factor(train$Crisis_tag)

# Define the training control
ctrl <- trainControl(method = "cv", number = 10)  # 5-fold cross-validation

# Define the grid of mtry values to search over
mtry_grid <- expand.grid(mtry = seq(50, 75, by = 5))  # Change the range and step size as needed

# Train the model using cross-validation
rf_model <- train(Crisis_tag ~ . - v34 - v35 - v36 - v37 - v38 - v39 -v40 - v54 - v55 -v57 -v59 -v60 -v33, data = train, method = "rf",
                  trControl = ctrl, tuneGrid = mtry_grid)

# Print the results
print(rf_model)
mtry_cv=rf_model[["bestTune"]][["mtry"]]

#mtry nro de variables, al poner todas bagging es un caso particular de RF
bag.tit10 = randomForest(Crisis_tag ~ . - v34 - v35 - v36 - v37 - v38 - v39 -v40 - v54 - v55 -v57 -v59 -v60 -v33, data=train,ntree=10000,mtry=mtry_cv, importance =TRUE)

#Analizamos la salida. Nos indica la tasa de error de las observaciones out of the bag (OOB) y tambien da la matriz de confusion para los mismos datos.
bag.tit10

# Grafica las tasas de error globales y para cada categoria a medida que toma mas muestras bootstrap
plot(bag.tit10, main = "")
legend("topright", legend = colnames(bag.tit10$err.rate), col = 1:ncol(bag.tit10$err.rate), lty = 1, cex = 1, bty = "n")

# Muestra la importancia de cada variable, dependiendo del criteio de impureza considerado.
varImpPlot(bag.tit10,sort = TRUE)

#saco la etiqueta
xtest=test[,-c(1,1)] 

#Prediccion en un nuevo conjunto de datos
pred.tit.bag.v = predict (bag.tit10 ,newdata =xtest,type = "vote")#da la proporcion de arboles que eligen cada clase.

#Pongo la etiqueta por el voto mayoritario de los arboles
pred.tit.bag.label=rep(NA,dim(test)[1]) #como si hubiera missing values genero un vector de ceros

aux1=which(pred.tit.bag.v[,2]>0.5) 
aux2=which(pred.tit.bag.v[,2]<=0.5)
pred.tit.bag.label[aux1]=1
pred.tit.bag.label[aux2]=0

#confusion matrix (test data)
conf.matrix <- table(test$Crisis_tag, pred.tit.bag.label)
rownames(conf.matrix) <- paste("Real", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# True Positive (TP), True Negative (TN), False Positive (FP), False Negative (FN)
TP <- conf.matrix[2, 2]
TN <- conf.matrix[1, 1]
FP <- conf.matrix[1, 2]
FN <- conf.matrix[2, 1]

# Accuracy
accuracy <- (TP + TN) / sum(conf.matrix)
cat("Accuracy:", accuracy, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# Recall (Sensitivity)
recall <- TP / (TP + FN)
cat("Recall (Sensitivity):", recall, "\n")

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")


#Me fijo como mirar el resultado a traves de las probabilidades a posteriori y armar la curva ROC
pred.tit.rf.prob = predict (bag.tit10 ,newdata =xtest,type = "prob")#da la proababilidad de elegir cada clase.

library(pROC)
rocbag = roc(test$Crisis_tag, pred.tit.rf.prob[,1])
rocbag
ggroc(rocbag)

# Load the ROCR package
library(ROCR)
pred_probs <- predict(bag.tit10, test, type = "prob")[, 2]

# Create a prediction object
prediction <- prediction(pred_probs, test$Crisis_tag)

# Create a performance object
performance <- performance(prediction, "tpr", "fpr")

# Plot the ROC curve
plot(performance, main = "", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n")

conf.matrix
accuracy
precision
recall
f1_score
auc_value









