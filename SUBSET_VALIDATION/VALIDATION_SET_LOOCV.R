### METODI DIRETTI DI VALUTAZIONE ###

adv <- read.csv('../adv.csv', sep = ',')

# dividiamo il dataset in train e test in parti uguali:
set.seed(2804)
train_set <-sample(200,100) #estrae 100 numeri a caso da una seq 1:200
fit_tv_train <- lm(Vendite~TV, data=adv, subset = train_set)
# il subset crea un modello addestrato con i soli dati presenti
# nel dataframe indicato

# Ora si calcola l'errore (MSE= media del RSS) con il test_set
adv$Vendite[-train_set] # vendite osservate
#vendite previste nel test set
predict(fit_tv_train, adv[-train_set,])

# residuo:
(adv$Vendite[-train_set] - predict(fit_tv_train, adv[-train_set,]))**2
# media: più è piccola e meglio è
MSE_TV <- mean((adv$Vendite[-train_set] - predict(fit_tv_train, adv[-train_set,]))**2)

# Modello in funzione di Digital
fit_dg_train <- lm(Vendite~Digital, data=adv, subset = train_set)
MSE_DG <-  mean((adv$Vendite[-train_set] - predict(fit_dg_train, adv[-train_set,]))**2)

# Modello in funzione di Radio
fit_ra_train <- lm(Vendite~Radio, data=adv, subset = train_set)
MSE_RA <-  mean((adv$Vendite[-train_set] - predict(fit_ra_train, adv[-train_set,]))**2)

# Combinazione degli MSE in un dframe
mse <- c(MSE_TV, MSE_DG, MSE_RA)
models <- c('TV', 'Digital', 'Radio')
mse_df <- data.frame(models, mse)

## Ricalcolo con diverso seed
set.seed(2805)
train_set <-sample(200,100) #estrae 100 numeri a caso da una seq 1:200

# Modello TV
fit_tv_train <- lm(Vendite~TV, data=adv, subset = train_set)
MSE_TV <- mean((adv$Vendite[-train_set] - predict(fit_tv_train, adv[-train_set,]))**2)

# Modello in funzione di Digital
fit_dg_train <- lm(Vendite~Digital, data=adv, subset = train_set)
MSE_DG <-  mean((adv$Vendite[-train_set] - predict(fit_dg_train, adv[-train_set,]))**2)

# Modello in funzione di Radio
fit_ra_train <- lm(Vendite~Radio, data=adv, subset = train_set)
MSE_RA <-  mean((adv$Vendite[-train_set] - predict(fit_ra_train, adv[-train_set,]))**2)


# Aggiungere una colonna al dframe
mse_df$mse2 <- c(MSE_TV, MSE_DG, MSE_RA)

### Ricalcolo con diverso seed
set.seed(2806)
train_set <-sample(200,100) #estrae 100 numeri a caso da una seq 1:200

# Modello TV
fit_tv_train <- lm(Vendite~TV, data=adv, subset = train_set)
MSE_TV <- mean((adv$Vendite[-train_set] - predict(fit_tv_train, adv[-train_set,]))**2)

# Modello in funzione di Digital
fit_dg_train <- lm(Vendite~Digital, data=adv, subset = train_set)
MSE_DG <-  mean((adv$Vendite[-train_set] - predict(fit_dg_train, adv[-train_set,]))**2)

# Modello in funzione di Radio
fit_ra_train <- lm(Vendite~Radio, data=adv, subset = train_set)
MSE_RA <-  mean((adv$Vendite[-train_set] - predict(fit_ra_train, adv[-train_set,]))**2)


# Aggiungere una colonna al dframe
mse_df$mse3 <- c(MSE_TV, MSE_DG, MSE_RA)

#### Ricalcolo con diverso seed
set.seed(2807)
train_set <-sample(200,100) #estrae 100 numeri a caso da una seq 1:200

# Modello TV
fit_tv_train <- lm(Vendite~TV, data=adv, subset = train_set)
MSE_TV <- mean((adv$Vendite[-train_set] - predict(fit_tv_train, adv[-train_set,]))**2)

# Modello in funzione di Digital
fit_dg_train <- lm(Vendite~Digital, data=adv, subset = train_set)
MSE_DG <-  mean((adv$Vendite[-train_set] - predict(fit_dg_train, adv[-train_set,]))**2)

# Modello in funzione di Radio
fit_ra_train <- lm(Vendite~Radio, data=adv, subset = train_set)
MSE_RA <-  mean((adv$Vendite[-train_set] - predict(fit_ra_train, adv[-train_set,]))**2)


# Aggiungere una colonna al dframe
mse_df$mse4 <- c(MSE_TV, MSE_DG, MSE_RA)

# Per vlutare i risultati dei diversi modelli
# visualizziamo i risultati
plot(mse_df$mse, xlab = 'Model', ylab = 'MSE', type='l', ylim =c(9,28) )
# Rappresentiamo gli MSE degli altri dataset
lines(mse_df$mse2, col='blue')
lines(mse_df$mse3, col='red')
lines(mse_df$mse4, col='green')

# Questi metodi sono soggetti a due limitazioni principali:
  # 1) Sovrastima della misura dell'errore
  # 2) Elevata variabilità della stima stessa (MSE)


### LEAVE ONE OUT CROSS VALIDATION ###
  # Nasce per risolvere i limiti del Validation Set

# 1) Il dataset è sempre diviso in train e test ma le
#    due parti non sono uguali.
#    Viene lasciata una sola osservazione da parte per
#    il test set. Da qui il nome del metodo
#    Meno sovrastima perché usati tutti i punti meno uno


# 2) Per ridurre la variabilità del MSE, la procedura
#    viene ripetuta n volte.
#    Per ciascuna delle osservazioni si addestra un modello
#    sui restanti n-1 dati e si calcola MSE_i sull'oss. stessa
#    MSE finale sarà la media degli MSE_i
#    Non presenta variabilità perché testati tutte le possibili
#    combinazioni e poi si fa la media

# La leave one out CV fa parte del pacchetto:
library('boot')

# Il fit si basa sulla GLM (TV)
fit_tv <- glm(Vendite~TV, data=adv)
# Il subset non viene indicato perché la LOOCV è standardizzata.

loocv_tv <- cv.glm(adv, fit_tv)$delta[1]
# Il MSE viene conservato nella variabile delta del modello
# !!!! loocv_tv <- loocv_tv$delta[1]

# Modello per Digital
fit_dg <- glm(Vendite~Digital, data=adv)
loocv_dg <- cv.glm(adv, fit_dg)$delta[1]

# Modello per Radio
fit_ra <- glm(Vendite~Radio, data=adv)
loocv_ra <- cv.glm(adv, fit_ra)$delta[1]

# Salviamo i risultati in un dataframe
mse_loocv <- data.frame('models' = c('TV', 'Digital', 'Radio'), 
                        mse = c(loocv_tv, loocv_dg, loocv_ra))

## Valutazione modelli con due termini

# Modello per TV + Digital
fit_tv_dg_loo <- glm(Vendite~TV +Digital, data=adv)
loocv_tv_dg <- cv.glm(adv, fit_tv_dg_loo)$delta[1]

# Modello per Digital + Radio
fit_dg_ra_loo <- glm(Vendite~Digital + Radio, data=adv)
loocv_dg_ra <- cv.glm(adv, fit_dg_ra_loo)$delta[1]

# Modello per TV + Radio
fit_tv_ra_loo <- glm(Vendite~TV + Radio, data=adv)
loocv_tv_ra <- cv.glm(adv, fit_tv_ra_loo)$delta[1]

mse_loocv2 <- data.frame('models'=c('T+D', 'D+R', 'T+R'),
                         mse=c(loocv_tv_dg, loocv_dg_ra,
                               loocv_tv_ra))
## Limiti della LOOCV ##
# Può essere molto costosa computazionalmente per via
# degli n modelli da addestrare

# Tuttavia nel caso dei minimi quadrati per modelli lineari
# o poliomiali esiste una formula analitica che rimuove il problema
# (Applicabile a qualunque algoritmo)
# Il costo comput.le di una procedura è molto importante

### K-FOLD CROSS VALIDATION ###
# Riduce notevolmente le risorse computazionali richieste
# per stimare l'MSE
# Invece di includere una sola oss.ne nel test set ne mette di più
# Inoltre fornisce una stima dell'errore più accurata del
# test error.

# L'intero dataset viene diviso in k-parti e di queste
# una viene usata per il test mentre le k-1 per il train.
# Questa procedura viene ripetuta k volte e l'MSE finale è
# la media dei kMSE.

# La LOOCV è un caso particolare della k-fold CV perché 
# nel caso della loo abbiamo che k=n

# La maggiore accuratezza nel calcolo dell'errore è dovuta al
# meccanismo di bilancio tra bias e varianza che formano l'errore
# complessivo

# La LOO usa n-1 osservazioni e per questo ha un BIAS migliore(errore riducibile
# utilizzando quanti più dati possibile.) 
# Essendo inv. prop. al diminuire del BIAS aumenta la Varianza(erorre intrinseco dei dati)

# La k-fold usa circa n-n/k oss consentendo di controllare l'equilibrio
# tra bias e varianza attraverso il parametro k
# k tende a n -> basso bias, alta varianza
# k tende a 2 -> alto bias, bassa varianza
# Tipicamene k è compreso tra 5 e 10 (empirico)

## Consideriamo prima i modelli lineari
set.seed(305)
k=5
# 1 predittore
mse_kf_tv <- cv.glm(adv, fit_tv, K=k)$delta[1]
mse_kf_dg <- cv.glm(adv, fit_dg, K=k)$delta[1]
mse_kf_ra <- cv.glm(adv, fit_ra, K=k)$delta[1]



# 2 predittori
mse_kf_tvdg <- cv.glm(adv, fit_tv_dg_loo, K=k)$delta[1]
mse_kf_tvra <- cv.glm(adv, fit_tv_ra_loo, K=k)$delta[1]
mse_kf_dgra <- cv.glm(adv, fit_dg_ra_loo, K=k)$delta[1]

# mse_kf_df$mse2 <- c(mse_kf_tvdg, mse_kf_tvra, mse_kf_dgra)

# 3 predittori
fit_kall <- glm(Vendite~TV+Radio+Digital, data = adv)
mse_kf3 <- cv.glm(adv, fit_kall, K=k)$delta[1]

mse_cv5 <- c(mse_kf_tv, mse_kf_dg, mse_kf_ra, mse_kf_tvdg, mse_kf_tvra, mse_kf_dgra, mse_kf3)
models_cv <- c('T','D','R','TD','TR','DR','TDR')
mse_kf_df <- data.frame(models_cv, mse_cv5)

## Possono essere valutati anche i modelli non lineari e 
#  quelli con sinergia

## Modelli polinomiali con poly()

for (n in seq(2,5)) {
  fit <- glm(Vendite~poly(TV,n), data=adv)
  mse_cv <- cv.glm(adv, fit, K=k)$delta[1]
  mse_cv5 <- c(mse_cv5, mse_cv)
  models_cv <- c(models_cv, paste('TV**', n, sep=''))
  }
# Modelli con sinergia TV*Digital
fit_tv_sin_dg <- glm(Vendite~TV*Digital, data=adv)
mse_cv_tv_sin_dg <- cv.glm(adv, fit_tv_sin_dg, K=k)$delta[1]

mse_cv5 <- c(mse_cv5, mse_cv_tv_sin_dg)
models_cv <- c(models_cv, 'T*D')


mse_kf_df <- data.frame(models_cv, mse_cv5)

## RAPPRESENTAZIONE DEI K-FOLD ##

plot(mse_kf_df$models_cv, mse_kf_df$mse_cv5, las=2 ) #las=2 metterà le etichette in verticale
# sarebbe meglio un barplot
barplot(mse_kf_df$mse_cv5, names=mse_kf_df$models_cv, las=2)

# per riordinare il dataframe
mse_ord <- mse_kf_df[order(mse_cv5), ]
barplot(mse_ord$mse_cv5, names=mse_ord$models_cv, las=2, ylab='MSE', main = 'MSE KFCV K=5')