# carica la libreria
library(MASS)
#carica i dati
student <- read.csv('../CLASSIFICAZIONE/student_port.csv', sep=';')
#impostazione seed
set.seed(2205)
# definizione set di training
train_ind <- sample(395,300)

# modello con 1 solo predittore
#LDA
lda_st <- lda(sex~studytime, data=student, subset=train_ind)
#QDA
qda_st <- qda(sex~studytime, data=student, subset=train_ind)

lda_st
qda_st

#Predizioni
pred_lda_st <- predict(lda_st, student[-train_ind,])$class
pred_qda_st <- predict(qda_st, student[-train_ind,])$class

# Matrice di confusione
conf_mat_lda <- table('predizioni'=pred_lda_st, 'osservazioni'=student$sex[-train_ind])
conf_mat_qda <- table('predizioni'=pred_qda_st, 'osservazioni'=student$sex[-train_ind])
conf_mat_lda
conf_mat_qda
# Accuracy
acc_lda_st <- sum(diag(conf_mat_lda)) / sum(conf_mat_lda)
acc_qda_st <- sum(diag(conf_mat_qda)) / sum(conf_mat_qda)
acc_lda_st
acc_qda_st

# Caso con più predittori

# LDA
lda_mult <- lda(sex~studytime + Dalc + freetime + G1 + Mjob + 
                  higher + activities + absences + Walc + health +
                  paid + romantic + schoolsup, data=student, 
                subset=train_ind)
pred_lda_mult <- predict(lda_mult, student[-train_ind,])$class
conf_mat_lda_mult <- table('Pred LDA'=pred_lda_mult, 'Oss'=student$sex[-train_ind])
acc_lda_mult <- sum(diag(conf_mat_lda_mult)) / sum(conf_mat_lda_mult)

# QDA 
qda_mult <- qda(sex~studytime + Dalc + freetime + G1 + Mjob + 
                  higher + activities + absences + Walc + health +
                  paid + romantic + schoolsup, data=student, 
                subset=train_ind)
pred_qda_mult <- predict(qda_mult, student[-train_ind,])$class
conf_mat_qda_mult <- table('Pred QDA'=pred_qda_mult, 'Oss'=student$sex[-train_ind])
acc_qda_mult <- sum(diag(conf_mat_qda_mult)) / sum(conf_mat_qda_mult)

conf_mat_lda_mult
acc_lda_mult
conf_mat_qda_mult
acc_qda_mult

