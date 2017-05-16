### ancova

# DEMARCHE EN 6 POINTS

VD~VI.c*VI.f

df<-read.table(file.choose("df.txt"),h=T);head(df)
df<-read.csv(file.choose("df.csv"),h=T,sep=",",dec=".")

# (1) hypothèses
##	H0.1:
##	H0.2:
##	H0.3:
##	H1.1, H1.2, H1.3
# (2)
VD<-df$longev
VI.c<-df$thorax			# VI "covariable", quantitative => toujours AVANT la VI.f
VI.f<-as.factor(df$treat)	# VI "facteur", qualitative
plot(VD~VI.c,pch=19,col=palette()[VI.f])
# (3) ANCOVA (une VD quantitative, une VI quantitative + une VI qualitative)
# (4) vérifier les postulats pour: ANOVA et régression linéaire
	#4.1 	Normalité des résidus
residu<-residuals(lm(VD~VI.c*VI.f))
par(mfrow=c(1,2))
qqnorm(residu);qqline(residu)
shapiro.test(residu)	# confirmation si non-significatif
	#4.2 	Homogénéité des variances
plot(fitted.values(lm(VD~VI.c*VI.f)),residu)	# si pas d'homogénéité, alors transformation
m2<-lm(log(VD)~VI.c*VI.f);plot(fitted.values(m2),residuals(m2)) # jamais de test de Bartlett
# (5) ANCOVA
anova(lm(VD~VI.c*VI.f))
anova(lm(VD~VI.c+VI.f))		# si interaction non-significative
anova(lm(log(VD)~VI.c*VI.f))	# si non-homogénéité des variances
anova(lm(log(VD)~VI.c+VI.f))	# si non-homogénéité et interaction non-significative
# (6) interprétation biologique
