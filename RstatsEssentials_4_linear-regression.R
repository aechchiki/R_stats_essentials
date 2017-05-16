### linear regression

#description graphique
plot(y~x)			# plot(x,y)= relation entre y et x
# estimateur de la droite de régression
b<-cov(x,y)/var(x)		# rem: équation d'une droite: y = ax + b
a<-mean(y)-b*mean(x)		# a= intercepte; b=pente
lm(y~x)	 			# droite de régression linaire
plot(x,y);abline(a,b)
pred<-a+b*x
residu<-y-pred			# residu<-residuals(lm(y~x))

par(mfrow=c(1,3))
RSS<-sum(residu^2)
	plot(y,x)
	abline(lm(x~y))
	segments(x,pred,x,y,col="red")
TSS<-sum((y-mean(y)))^2)
	plot(x,y)
	abline(h=mean(y),lty=2)
	segments(x,mean(y),x,y,col="blue")
ESS<-sum((pred-mean(y))^2)
	plot(x,y)
	abline(h=mean(y),lty=2)
	abline(a,b,col="blue")	# ou abline(c.t,col="blue")
	segments(x,mean(y),x,pred,col="red")

ESS<-TSS-RSS
cov(x,y)^2/var(y)/var(x)	#rho-deux 	#ESS/TSS
cor(x, y)^2			#pearson correlation

cor.test(x, y)			# Pearson's product-moment correlation
summary(lm(y~x))		#résumé des donnéeS
anova(lm(y~x))			# analyse de variance


# démarche en 6 points

df<-read.table(file.choose("df.txt"),h=T)
df<-read.csv(file.choose("df.csv"),h=T,sep=";",dec=".")
head(df)

# (1) 	Hypothèses nulle et alternative
		#H0: diamètre n'a pas d'influence sur le volume
		#H1: diamètre a une influence sur le volume
# (2)
VD<-df$VOLUME		# quantitative, dépendente, "réponse"
VI<-df$DIAMETRE		# quantitative, indépendente, "explicative"
plot(VD~VI)
abline(lm(VD~VI))	# also, b<-cov(VI,VD)/var(VI);a<-mean(VD)-b*mean(VI);abline(a,b)
residu<-residuals(lm(VD~VI))
# (3) 	régression linéaire
# (4) 	test d'homogénéité des résidus
par(mfrow=c(2,2));plot(lm(VD~VI))
par(mfrow=c(1,3))
plot(residu~VI);abline(lm(residu~VI))
plot(abs(residu)~VI);abline(lm(abs(residu)~VI))
qqnorm(residu)
##	Exploration de la structure du graph du milieu
cor.test(abs(residu),VI,method="spearman")
# (5)
summary(lm(VD~VI))
	## transformation log(VD) en cas d'hétéroscédasticité dans les données
summary(lm(log(VD)~VI))
# (6)	 Interprétation (réponse biologique)
	## r^2 (variation expliquée) donne qualité de l'indicateur
