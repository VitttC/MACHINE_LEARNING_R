# K-MEANS

# C CREAZIONE DATASET 100 OSS CIASCUNA CON 2 PARAMETRI
# SI USA LA DISTR. GAUSSIANA CON MEDIA =0 E SIGMA=1
# PER ENTRAMBI I PREDITTORI LE OSS SARANNO DISTRIBUITE
# INTORNO AL PUNTO (0,0)

seed <- 1006
set.seed(seed)
obs <- 100
dataset <- matrix(rnorm(obs *2, 0, 1), ncol = 2)
plot(dataset)

# Cambio della media per entrambi i predittori per
# la metà delle osservazioni:
# sottraggo 3 ad X_1 e 4 ad X_2
# la prima metà delle oss dovrebbe distribuirsi intorno a (3,-4)
# mentre la seconda metà dovrebbe restare con (0,0)

g1 <- obs/2
dataset[1:g1, 1] <- dataset[1:g1, 1] -3
dataset[1:g1, 2] <- dataset[1:g1, 2] +4
plot(dataset[1:g1, 1], dataset[1:g1, 2])

# riporto entrambi i gruppi sullo stesso plot con colori diversi
plot(dataset[1:g1,], col='red', pch=20, cex=2,
     xlim = c(min(dataset[,1]), max(dataset[,1])),
     ylim = c(min(dataset[,2]), max(dataset[,2])))
points(dataset[(g1+1):obs,], col='green', pch=20, cex=2)
# pch=tipo di indicatore, cex=dimensione

# Creazione modello K-MEANS con k=2
k <- 2
km_2 <- kmeans(dataset, k, nstart = 20) 
#nstart indica quante volte vanno variate le condizioni iniziali
km_2$cluster

plot(dataset, col=(km_2$cluster + 1), main='K_Means Clustering con k=2',
     xlab='X_1', ylab = 'X_2', pch=20, cex=2)
# coordinate dei centroidi:
points(km_2$centers, pch=13, cex=2, col='black')
km_2$centers
names(km_2)

# k-mean con k=3
k <- 3
km_3 <- kmeans(dataset, k, nstart = 20) 
#nstart indica quante volte vanno variate le condizioni iniziali
km_3$cluster

plot(dataset, col=(km_3$cluster + 1), main='K_Means Clustering con k=3',
     xlab='X_1', ylab = 'X_2', pch=20, cex=2)
# coordinate dei centroidi:
points(km_3$centers, pch=13, cex=2, col='black')
km_3$centers

# modello con una sola condizione iniziale
k <- 3
km_3_n1 <- kmeans(dataset, k, nstart = 1) 
km_3_n1$cluster

plot(dataset, col=(km_3_n1$cluster + 1), main='K_Means Clustering con k=3 \n nstart=1',
     xlab='X_1', ylab = 'X_2', pch=20, cex=2)
# coordinate dei centroidi:
points(km_3_n1$centers, pch=13, cex=2, col='black')
km_3_n1$centers

#Verifica della WSM (Within sum of squares)
# nel caso di nstart=20 e nstart=1
km_3$tot.withinss
km_3_n1$tot.withinss

# Elbow method per valutare il migliore K graficamente
tot_wss <- c()
for (k in seq(2,15)) {
  model <- kmeans(dataset, k, nstart = 20)
  tot_wss <- c(tot_wss, model$tot.withinss)
}
plot(seq(2,15), tot_wss, type = 'o', xlab = 'K')

# Valutiamo modello con k=6
k <- 6
km_6 <- kmeans(dataset, k, nstart = 20) 
km_6$cluster

plot(dataset, col=(km_6$cluster + 1), main='K_Means Clustering con k=6',
     xlab='X_1', ylab = 'X_2', pch=20, cex=2)
# coordinate dei centroidi:
points(km_6$centers, pch=13, cex=2, col='black')
km_6$centers

#nonostante il dataset originale sia stato generato sulla base di due
# soli gruppi il risultato migliore sembra essere k=6 nonostante sia quello errato
# da qui la soggettività dell'analisi con cluster