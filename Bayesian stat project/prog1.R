library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)

dataset= read.table("PROGETTO_DATASET3.txt", header = TRUE, dec=",")
head(dataset)
summary(dataset)
g = lm( Nasdaq ~ Apple + Amazon + Microsoft + Starbucks + Google + Netflix + Tesla + PayPal+JD+Peloton, data = dataset)
summary(g)
plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )
plot(g, which=1 ) 
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
shapiro.test( g$res )

qqnorm( rstandard( g ), ylab = "Studentized residuals", pch = 16 )
abline( 0, 1 )
hist( g$res, 10, probability = TRUE, col = 'lavender', main = 'residuals'  )
boxplot( g$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender' )
g1 = lm( Nasdaq ~ Apple + Microsoft + Starbucks + Google + Netflix + Tesla + PayPal, data = dataset )
summary(g1)
shapiro.test( g1$res )

b = boxcox(Nasdaq ~ Apple + Amazon + Microsoft + Starbucks + Google + Netflix + Tesla + PayPal, data = dataset)
names(b)
#y likelihood evaluation
#x lambda evaluated
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda
#LAMBDA CHE MASSIMIZZA LA LIKELIHOOD

#We can see that the best transformation is the one related to the *maximum* of the curve.
#According to this method, the best $\lambda$ is $ 0.2626263$.

#Finally, we test the new model and we investigate the standardized residuals.
mod1 = lm( (Nasdaq ^ best_lambda - 1)/best_lambda ~ Apple + Amazon + Microsoft + Starbucks + Google + Netflix + Tesla + PayPal, data = dataset) 
summary(mod1)
shapiro.test(residuals(mod1))
#METTO AL POSTO DELLA Y LA TRASFORMAZIONE DI BOX-COX CON IL LAMBDA IDENTIFICATO
#OTTENGO SEMPRE UN BEL MODELLO, R QUADRO ALTO E COEFF MOLTO SIGNIFICATIVI
#VALUTIAMO I RESIDUI
mod1_res = mod1$residuals/summary( mod1 )$sigma

plot( mod1$fitted, mod1_res, xlab = 'Fitted values',  ylab = 'Standarzized residuals'  ) 
#NUVOLA ABBASTANZA OMOGENEA

qqnorm( mod1_res )
abline( 0, 1, col = 'red' )
shapiro.test( residuals( mod1 ) ) 
vif( g )
g2 = lm( Nasdaq ~ Apple + Amazon + Microsoft + Starbucks + Netflix + Tesla + PayPal, data = dataset )
summary(g2)
g3 = lm( Nasdaq ~ Amazon + Microsoft + Starbucks + Google + Netflix + Tesla + PayPal, data = dataset )
summary(g3)
vif(g3)
g4 = lm( Nasdaq ~ Amazon + Starbucks + Google + Netflix + Tesla + PayPal, data = dataset )
summary(g4)
vif(g4)
shapiro.test( g4$res )
g5 = lm( Nasdaq ~ Amazon + Starbucks + Google + Netflix + Tesla , data = dataset )
summary(g5)
vif(g5)
shapiro.test( g5$res )



g6=lm(Nasdaq ~ Apple+Amazon+Google,data=dataset)
summary(g6)
shapiro.test( g6$res )
vif( g6 )
mod1 = lm( (Nasdaq ^ best_lambda - 1)/best_lambda ~ Apple + Amazon + Google , data = dataset) 
summary(mod1)
shapiro.test( mod1$res )
qqnorm( g6$res, ylab = "Raw Residuals", pch = 16 )
qqline( g6$res )




#####INIZIO SUPERINFLUENTI
library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(BAS)
library(rgl)
library(corrplot)






##SUPERINFLUENTI
#X = model.matrix( g6 )
#X

#lev = hat( X )
#lev
# or 
#lev = hatvalues( g6 )
#lev

#Alternatively, we can compute H manually and then exctract its diagonal elements:
#manually
#H = X %*% solve( t( X ) %*% X ) %*% t( X ) 
#lev = diag( H )

#sum(lev) # verifica: sum_i hat( x )_i = p


# __Rule of thumb:__ Given a point h_ii diagonal element of H, the i-th observation is a leverage if:
#  h_ii > 2*p/n

#p = g6$rank # p = 5 
#n = dim(dataset)[1] # n = 50


#plot( g6$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",pch = 16, col = 'black' )
#abline( h = 2*p/n, lty = 2, col = 'red' )
#watchout_points_lev = lev[ which( lev > 2*p/n  ) ]
#watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
#points( g$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )
#lev [ lev >  2 * 5 / 50 ]
#sum( lev [ lev >  2 * 5 / 50 ] )
#gl = lm( Nasdaq ~ Apple + Amazon  +  Google , dataset, subset = ( lev < 2*p/n ) )
#summary( gl )
#summary(g6)
#abs( ( g6$coefficients - gl$coefficients ) / g6$coefficients )
##UNICI CAMBIAMENTI SIGNIFICATIVI DEI BETA HAT SONO PELOTON 23% E MICROSOFT 8%


# We can also visualize the position of leverages for each covariate couple.

#colors = rep( 'black', nrow( dataset ) )
#colors[ watchout_ids_lev ] = c('red', 'blue', 'green', 'orange','white','yellow','gray','pink','brown','khaki')
#pairs( dataset[ , c( 'Nasdaq', 'Apple', 'Amazon', 'Microsoft', 'Starbucks', 'Google','Netflix','Tesla','PayPal','JD','Peloton') ], pch = 16, col = colors, cex = 1 + 0.5 * as.numeric( colors != 'black' ))

### b. Standardized Residuals

#Plot the residuals of the complete model.

# Residui non standardizzati (e non studentizzati)

#plot( g$res, ylab = "Residuals", main = "Plot of residuals" )

#sort( g$res )
#sort( g$res ) [ c( 1, 253 ) ]  ## per vedere il primo e l'ultimo residuo

#countries = row.names( savings )

#identify( 1:50, g$res, countries ) 
# click 2 times on the points you want to make a label appear
# it works only by console and plots window

#Plot the __Standardized Residuals__ of the complete model.

#__Rule of thumb__ A point i is influential if |r_i^{std}|>2

# It is easy to see that influetial points according to standardized residuals and to leverages are different.

#gs = summary(g)
#res_std = g$res/gs$sigma
#watchout_ids_rstd = which( abs( res_std ) > 2 )
#watchout_rstd = res_std[ watchout_ids_rstd ]
#watchout_rstd

# Residui standardizzati 

#plot( g$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
#abline( h = c(-2,2), lty = 2, col = 'orange' )
#points( g$fitted.values[watchout_ids_rstd], 
        #res_std[watchout_ids_rstd], col = 'red', pch = 16 )
#points( g$fitted.values[watchout_ids_lev], 
        #res_std[watchout_ids_lev], col = 'orange', pch = 16 )
#legend('topright', col = c('red','orange'), 
      # c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )
##ANCHE NEL NOSTRO MODELLO I PUNTI INFLUENTI PER IL METODO LEVERAGES SONO DIFFERENTI DAGLI INFLUENTI DEL METODO STANDARDIZED RESIDUALS

### c. Studentized Residuals
# #__Rule of thumb__ A point i is influential if |r_i^{stud}|>2

#Compute the Studentized Residuals, highlighting the influential points.

#gs = summary( g )

#gs$sigma

# manually
#stud = g$residuals / ( gs$sigma * sqrt( 1 - lev ) )

# 'rstandard' gives studentized residuals automatically
#stud = rstandard( g )

#watchout_ids_stud = which( abs( stud ) > 2 )
#watchout_stud = stud[ watchout_ids_stud ]
#watchout_stud


#plot( g$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
#points( g$fitted.values[watchout_ids_stud], 
        #stud[watchout_ids_stud], col = 'pink', pch = 16 )
#points( g$fitted.values[watchout_ids_lev], 
        #stud[watchout_ids_lev], col = 'orange', pch = 16 )
#abline( h = c(-2,2), lty = 2, col = 'orange' )
#legend('topright', col = c('pink','orange'), 
       #c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )


X = model.matrix( g6 )   #elementi della matrice H
X   #ESTRAIAMO LA MATRICE DELLE COVARIATE PIU VETTORE DI UNI

#Leverages: Punti Leva
lev = hat( X )   #VETTORE IN CUI OGNI ELEMENTO è IL LEVERAGE
lev
sum(lev) # verifica: sum_i hat( x )_i = p
#LA SOMMA DI TUTTI I PUNTI LEVA DEVE DARCI P OVEVRO R+1   si, viene 4

# Rule of thumb: Given a point h_ii diagonal element of H, the i-th observation is a leverage if:
#  h_ii > 2*p/n

p = g6$rank # p = 4 
n = dim(dataset)[1] # n = 253

#VEDERE SE Hii>2P/N  =0.03162055 valore soglia
plot( g6$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",pch = 16, col = 'black' )
abline( h = 2*p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2*p/n  ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( g6$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

lev [ lev >  2 * p / n ]     #mi dice quali sono i punti leva
sum( lev [ lev >  2 * p / n ] )    #li somma, otteniamo 0.7189151 su 4, quindi sono abbastanza influenti


#proviamo a fittare il modello senza i punti di leva
gl = lm( Nasdaq ~ Apple + Amazon + Google, dataset, subset = ( lev < 2*p/n ) )
summary( gl )
#otteniamo un modello ancora buono con un R2adj leggermente inferiore ma covariate ancora tutte significative
summary(g6)
#GUARDIAMO LE STIME DEI COEFFICIENTI 
#CONFRONTIAMO LE STIME DEI DUE MODELLI
abs( ( g6$coefficients - gl$coefficients ) / g6$coefficients )
##CAMBIAMENTI SIGNIFICATIVI DEI BETA HAT SONO Amazon del 38% E Apple 5%
#Mentre Google non cambia significativamente

#--------------------
#Analisi residui standardizzati
### b. Standardized Residuals

#Plot the residuals of the complete model.

# Residui non standardizzati (e non studentizzati)

plot( g6$res, ylab = "Residuals", main = "Plot of residuals" )


#Plot the Standardized Residuals of the complete model.

#Rule of thumb A point i is influential if |r_i^{std}|>2

# It is easy to see that influetial points according to standardized residuals and to leverages are different.

gs = summary(g6)
res_std = g6$res/gs$sigma   #Residui standardizzati
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd

# Residui standardizzati 

plot( g6$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( g6$fitted.values[watchout_ids_rstd], 
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( g6$fitted.values[watchout_ids_lev], 
        res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'), 
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )
##ANCHE NEL NOSTRO MODELLO I PUNTI INFLUENTI PER IL METODO LEVERAGES SONO DIFFERENTI DAGLI INFLUENTI DEL METODO STANDARDIZED RESIDUALS

### c. Studentized Residuals
# #Rule of thumb A point i is influential if |r_i^{stud}|>2

#Compute the Studentized Residuals, highlighting the influential points.

gs = summary( g6 )

gs$sigma


# 'rstandard' gives studentized residuals automatically
stud = rstandard( g6 )

watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud
#TROVIAMO GLI STESSI DI PRIMA MA DUE RESIDUI IN PIù

plot( g6$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( g6$fitted.values[watchout_ids_stud], 
        stud[watchout_ids_stud], col = 'pink', pch = 16 )
points( g6$fitted.values[watchout_ids_lev], 
        stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink','orange'), 
       c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )




###da qui io
### d. Cook's distance

# __Rule of thumb__ C_i > {4}/{n-p}

Cdist = cooks.distance( g )
watchout_ids_Cdist = which( Cdist > 4/(n-p) ) 
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist
par( mfrow = c( 1, 4 ) )
##TUTTI E 4 I GRAFICI DEI RESIDUI CON I 4 DIFFERENTI METODI
plot( g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', 
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( g$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], 
        col = 'green', pch = 16 )

plot( g$fitted.values, res_std, pch = 16, xlab = 'Fitted values', 
      ylab = 'Standardized Residuals', main = 'Standardized Residuals' )
points( g$fitted.values[ watchout_ids_rstd ], res_std[ watchout_ids_rstd ] , 
        col = 'pink', pch = 16 )

plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', 
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], 
        col = 'pink', pch = 16 )

plot( g$fitted.values, lev, pch = 16, xlab = 'Fitted values', 
      ylab = 'Leverages', main = 'Leverages' )
points( g$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'orange', pch = 16 )
##MODEL FITTED WITHOUT COOKS INFLUENTIAL POINTS
##VEDIAMO COME IL MODELLO CAMBIA SENZA I COOK INFLUENTIAL POINTS
id_to_keep = !( 1:n %in% watchout_ids_Cdist )
gl = lm( Nasdaq ~ Apple + Amazon + Google, dataset[ id_to_keep, ]  ) 
summary(g6)
summary( gl )
##osservo che R^2ADJ migliora leggermente cosi' come il p-value di amazon 
#VALUTO LA VARIAZIONE PERCENTUALE DEI BETA HAT TRA I DUE MODELLI
abs( ( gl$coef - g6$coef )/g6$coef )
#LEGGERISSIMA VARIAZIONE DEI BETA, MENO DELL'1%

# __All together: Influential Plot__
##IL PLOT TOTALE RAPPRESENTA I RESIDUI STUDENTIZZATI VS I LEVERAGES E LA VARIAZIONE DEL RAGGIO DEI CERCHI è PROPORZIONALE ALLA DISTANZA DI COOK

x11()
influencePlot( g, id.method = "identify", main = "influential Plot",
               sub = "Circle size is proportial to Cook's Distance" )
#ALTRA MANIERA DI RAPPRESENTARE GLI INFLUENTIAL POINTS ####VED BENEEEEEE
plot(g6, which = 5)
#__All together: Influential measures__
#`influential.measures` produces a class "infl" object tabular display showing several 
#diagnostics measures (such as h_{ii} and Cook's distance).
#Those cases which are influential with respect to any of these measures are marked with an asterisk.
influence.measures( g6)
#######FINE SUPERINFLUENTI



