
# Calcoliamo l'MSE con la k-fold Cross Validation
#
set.seed(305)
k=5
# Consideriamo i modelli lineari (i fit sono gia' stati realizzati con glm)
# 1 predittore:
mse_cv_tv <- cv.glm(adv, fit_tv, K=k)$delta[1]
mse_cv_dg <- cv.glm(adv, fit_dg, K=k)$delta[1]
mse_cv_ra <- cv.glm(adv, fit_ra, K=k)$delta[1]
# 2 predittori:
mse_cv_tvdg <- cv.glm(adv, fit_tvdg, K=k)$delta[1]
mse_cv_tvra <- cv.glm(adv, fit_tvra, K=k)$delta[1]
mse_cv_dgra <- cv.glm(adv, fit_dgra, K=k)$delta[1]
# 3 predittori:
mse_cv_tvdgra <- cv.glm(adv, fit_tvdgra, K=k)$delta[1]

# Inseriamo i risultati in un dataframe:
mse_cv5 <- c(mse_cv_tv, mse_cv_dg, mse_cv_ra, mse_cv_tvdg, mse_cv_tvra, mse_cv_dgra, mse_cv_tvdgra)
models <- c('TV', 'Digital', 'Radio', 'TV+Digital', 'TV+Radio', 'Digital+Radio', 'TV+Dig+Rad')

# Consideriamo dei modelli polinomiali in TV, usando la funzione poly()
for (n in seq(2, 5)){
  fit <- glm(Vendite~poly(TV,n), data=adv)
  mse_cv <- cv.glm(adv, fit, K=k)$delta[1]
  mse_cv5 <- c(mse_cv5, mse_cv)
  models <- c(models, paste('TV**',n, sep=''))
}

# Consideriamo dei modelli con interazione, includiamo quello tra TV e Digital:
fit_tv_sin_dg <- glm(Vendite~TV*Digital, data=adv)
mse_cv_tv_sin_dg <- cv.glm(adv, fit_tv_sin_dg, K=k)$delta[1]

mse_cv5 <- c(mse_cv5, mse_cv_tv_sin_dg)
models <- c(models, 'TV*Digital')
mse_cv <- data.frame(models, mse_cv5)

# Visualizziamo tutti i risultati
plot(mse_cv$models, mse_cv$mse_cv5, las=2)
barplot(mse_cv$mse_cv5, names=mse_cv$models, las=2)
# ordiniamo il dataframe e rifiniamo alcuni dettagli del plot:
mse_cv_ord <- mse_cv[order(mse_cv5),]
barplot(mse_cv_ord$mse_cv5, names=mse_cv_ord$models, las=2, ylab='MSE',
        main='MSE per vari modelli con la 5-fold CV')
 