### FORWARD STEPWISE SELECTION ###

# si parte da un modello senza predittori e poi ne calcoliamo l' R^2

M_0 <- lm(Vendite~1, data=adv) # modello senza parametri
summary(M_0)  # l'intercetta è la media di tutte le vendite
mean(adv$Vendite) 

# I seguenti passi vanno ripetuti per k=0, ..., p-1
#  1) Consideriamo tutti i p-k modelli ottenuti da M_k
#     agigungendo un solo predittore in più

#  2) Per ogni modello p-k si calcola R^2
#     M_k+1 sarà il modello con R^2 più grande

# k=0
M_1a <- lm(Vendite~TV, data=adv)
M_1b <- lm(Vendite~Digital, data=adv)
M_1c <- lm(Vendite~Radio, data=adv)

summary(M_1a)$r.squared # 0.61 questo è il maggiore
summary(M_1b)$r.squared # 0.33
summary(M_1c)$r.squared # 0.05

M_1 <- M_1a # modello con risultato migliore in M_1

# k=1
M_2a <- lm(Vendite~TV+Digital, data=adv)
M_2b <- lm(Vendite~TV+Radio, data=adv)

summary(M_2a)$r.squared # 0.89 maggiore
summary(M_2b)$r.squared # 0.64

M_2 <- M_2a

# k=2 (sarebb p-1 cioè 3-1)
M_3 <- lm(Vendite~TV+Radio+Digital, data=adv)

summary(M_3)$r.squared # 0.89

# Confronto dei tre modelli con adjusted R squared
summary(M_1)$adj.r.squared # 0.610
summary(M_2)$adj.r.squared # 0.896
summary(M_3)$adj.r.squared # 0.895

# gli ultimi risultati non sono validi perché calcolati
# sul trainset e non sul testset.
# il metodo migliore risulta essere TV+Digital


### BACKWARD STEPWISE SELECTION ###

# Si parte da un modello con tutti i predittori M_p
# e se ne calcola l' R^2

# k=3
M_3 <- lm(Vendite~TV+Radio+Digital, data=adv)

# Si ripete per i modelli da k=p, ..., 1
  #1) Si considerano tutti i k modelli M_k togliendo
  #   un solo predittore dei presenti

  #2) Per ciascun modello si calcola R^2 e si assegna
  #   a M_k-1 il modello con R^2 più grande

# k=2
M_2a <- lm(Vendite~Radio+Digital, data=adv)
M_2b <- lm(Vendite~TV+Digital, data=adv)
M_2c <- lm(Vendite~TV+Radio, data=adv)

summary(M_2a)$r.squared # 0.332
summary(M_2b)$r.squared # 0.897
summary(M_2c)$r.squared # 0.645

M_2 <- M_2b

# Dal modello M_2 andiamo a creare dei modelli con
# con un modello in meno

M_1a <- lm(Vendite~TV, data=adv)
M_1b <- lm(Vendite~Digital, data=adv)

summary(M_1a)$r.squared # 0.611
summary(M_1b)$r.squared # 0.332

M_1 <- M_1a

# Infine si calcola per un modello senza predittori
M_0 <- lm(Vendite~1, data=adv)
summary(M_0)$r.squared

# Dati i modelli M_0, M_1, ..., M_p il migliore sarà
# quello con il test error più basso (adj.r.squared)

summary(M_0)$adj.r.squared # 0
summary(M_1)$adj.r.squared # 0.6099
summary(M_2)$adj.r.squared # 0.8961
summary(M_3)$adj.r.squared # 0.8956

# Questo metodo di valutazione però non garantisce che
# le variabili scartate nelle fasi iniziali non siano rilevanti.


### HYBRID STEPWISE SELECTION ###

# Questo metodo unisce forward e backward
# Si parte come per la FSWS da un modello con predittore
# costante e si aggiungono uno alla volta.
# Per ogni predittore aggiunto si effettua una BSWS per
# valutare se possibile tralasciare una delle variabili.
# Vengono valutati più modelli aumentando la probabilità di
# trovare un modello migliore.

# Nel dataset abbiamo: 
  # target: Vendite
  # predittori: TV, Digital, Radio
# Per un totale di 2^p modelli. Cioè 2^3=8 modelli lineari

# Con F/B SWS i modelli diventano 1+(p(p+1)/2) = 7


### VALUTAZIONE MODELLI ###

## AIC ##
# utilizzo un modello con tutti i predittori (i.e. M_3)
library("stats")
AIC(M_3) # AIC accetta come input anche una serie di modelli

AIC(M_0, M_1, M_2, M_3) # si possono conservare i risultati in un dataframe
test_error <- data.frame(AIC(M_0, M_1, M_2, M_3))

M_2$call #per visualizzare quali parametri e la formula usati per generare il modello

## BIC (BAYESIAN INFORMATION CRITERION) ##

BIC <- AIC(M_0, M_1, M_2, M_3, k=log(28)) #n= oss dei modelli; n=nobs(<modello>)
BIC
# anche il BIC stima M_2 come modello migliore

# Confronto con modello con sinergia tra parametri
fit_dg_sin_tv <- lm(formula = Vendite ~ Digital + TV + Digital * TV, data = adv)
fit_tv_2 <- lm(formula = Vendite ~ poly(TV, 2), data = adv)

BIC <- AIC(M_0, M_1, M_2, M_3, fit_dg_sin_tv, fit_tv_2, k=log(28))
BIC
# Il modello con sinergia è quello con BIC(test error) migliore

M_1$call # è un modello lineare in funzione di TV
         # fit_tv_2 ha un test error simile a M_1 perché
         # è una variante quadratica dello stesso


## CP DI MALLOWS ##
# per installare leaps assicurarsi di avere il compilatore 'gfortran'
install.packages('leaps') # installabile anche da Tools
library('leaps')
leaps(adv[,2:4], adv[,5], names=c('TV','Digital', 'Radio' ))
# leaps accetta anche il parametro method= 'Cp' oppure 'adjr2'

# Il modello che il CP più piccolo (2.03) che contiene i
# parametri TV e Digital così come per i test precedenti

# install.packages('olsrr')
library('olsrr')

fit_ratv <- lm(Vendite~Radio + TV, data=adv)
fit_all <- lm(Vendite~TV + Radio + Digital, data=adv)

ols_mallows_cp(fit_ratv, fit_all)
fit_tv_sin_ra <- lm(Vendite~TV + Radio + TV*Radio, data=adv)
ols_mallows_cp(fit_tv_sin_ra, fit_all)

## STEPWISE AUTOMATICA ##
# FORWARD
step(M_0, scope = list(lower=M_0, upper=M_3), direction='forward')
# restituisce tutti i passaggi insieme al modello finale
# per restituire solo l'esito trace=FALSE
step(M_0,scope=list(lower=M_0,upper=M_3),direction='forward',trace=FALSE)

# BACKWARD
step(M_3, direction = 'backward', trace = FALSE)

# IBRIDO
# Comando uguale alla forward ma direzione both
step(M_0, scope = list(lower=M_0, upper=M_3), direction='both', trace=FALSE)
