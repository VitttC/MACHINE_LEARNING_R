# KNN
library(class)
#knn(train,test, target, k)
set.seed(206)
student <- read.csv('../CLASSIFICAZIONE/student_port.csv', sep=';')
train_ind <- sample(395, 300)

# Modello con un solo predittore
train <- student[train_ind, 'studytime', drop=F]
target <- student[train_ind, 'sex']
test <- student[-train_ind, 'studytime', drop=F]
k <- 1
knn_st <- knn(train, test, target,k)

# Calcolo Accuracy
obs <- student[-train_ind, 'sex']
cm_knn_st <- table('Pred'=knn_st, 'Oss'=obs)
accuracy_knn_st <- sum(diag(cm_knn_st)) / sum(cm_knn_st)
cm_knn_st
accuracy_knn_st

# Ciclo per k da 1 a 100 

accuracy_knn_st <- c()
for (k in seq(1,100)) {
  pred <- knn(train, test, target, k)
  cm <- table(pred, obs)
  accuracy <- sum(diag(cm)) / sum(cm)
  accuracy_knn_st <- c(accuracy_knn_st, accuracy)
}

plot(accuracy_knn_st, type='o')
max(accuracy_knn_st)
which.max(accuracy_knn_st)

# L'accuracy è uguale per tutto il modello.
# in questi casi bisogna verificare prima il codice, che in questo caso è corretto
# i k vicini sono molto clusterizzati e noi stiamo usando un solo parametro(studytime)

# K-Fold cross validation
library(caret)

train_control <- trainControl(method='repeatedcv', number=10, repeats=10)
model_st <- train(sex~studytime, data=student, trControl=train_control, 
                  method='knn', tuneGrid=expand.grid(k=1:1))
acc_knn_st <- model_st$results[2]
acc_knn_st

model_st #per vedere i dati sul training eseguito

min(model_st$resample[1])
max(model_st$resample[1])

#KNN CON PIÙ PREDITTORI

pred <- c('studytime', 'Dalc', 'freetime', 'G1', 'Mjob', 
          'higher', 'activities', 'absences', 'Walc', 
          'health', 'paid', 'romantic', 'schoolsup')

# Conversione variabili categoriche in variabili numeriche
cols <- c('higher', 'activities', 'paid', 'romantic', 'schoolsup', 'Mjob')
student_num <- student

for (col in cols){
  student_num[,col] <- sapply(student_num[,col], as.numeric)
}

