### anova

# decomposition générale

# ANOVA à la main
df<-read.table(file.choose("df.txt"),h=T)
	champs<-seq(from=1,to=30)
	cha<-data.frame(champs,df);names(cha)

# Variables
VD<-cha$rendement
VI<-cha$champs
g.1<-VD[cha$engrais=="1"]	# groupe 1
g.2<-VD[cha$engrais=="2"]	# groupe 2, etc.
g.3<-VD[cha$engrais=="3"]

n<-10; a<-3		#n=nb observations par groupe #a=nb groupe
N<-a*n;residu<-residuals(aov(VD~VI))

# Total Sum of Squares
sst<-sum((VD-mean(VD))^2);sst
# Sum of squares between
moy.1<-mean(g.1)
moy.2<-mean(g.2)
moy.3<-mean(g.3)
ssb<-n*(((moy.1-mean(VD))^2)+((moy.2-mean(VD))^2)+((moy.3-mean(VD))^2));ssb
# Total Sum of squares within
ssw<-sum((g.1-moy.1)^2)+sum((g.2-moy.2)^2)+sum((g.3-moy.3)^2);ssw
# Mean sum of squares
mst<-sst/(N-1);mst
msb<-ssb/(a-1);msb
msw<-ssw/(N-a);msw
# Test de fisher
fisher<-msb/msw;fisher
pf(fisher,a-1,N-a,lower.tail=F)
#grahiques
plot(VD~VI)
# sst
abline(h=mean(VD));segments(VI,mean(VD),VI,VD)
# ssw
segments(0,moy.3,10,moy.3);segments(11,moy.1,20,moy.1);segments(21,moy.2,30,moy.2)
# ssb


### one-factor anova

# DEMARCHE EN 6 POINTS

df<-read.table(file.choose("df.txt"),h=T);head(df)
df<-read.csv(file.choose("df.csv"),h=T,sep=";",dec=".")

# (1)	comparaison des moyennes
##	H0: mean.1 = mean.2 = ... = mean.a
##	H1: au moins une moyenne diffère des autres moyennes
# (2)
VD<-				#quantitative
VI<-as.factor()			#qualitative
	> VI<-as.factor(df$engrais);	df$jours
	> VD<-df$rendement;		df$traitement
# (3) ANOVA
# (4) tests des postulats
	#4.1	Normalité des résidus
residu<-residuals(aov(VD~VI))
qqnorm(residu);qqline(residu)
shapiro.test(residu)		# confirmation si non-significatif
skewness(residu)		# activer les fonctions dans "fonctions.R", entre -0.5 et 0.5 -> normalité
kurtosis(residu)
	#4.2 	Homogénéité des variances
plot(VI,residu)
levene(VD,VI)		# H0: normalité, H1: non normalité
bartlett.test(VD~VI)
# (5) mise en place du test
anova(aov(VD~VI))
# (6) interprétation


### hierarchical anova

# DEMARCHE EN 6 POINTS

VD~VI.1+VI.2%in%VI.1	# même chose que: VD~VI.1/VI.2

df<-read.table(file.choose("df.txt"),h=T);head(df)
df<-read.csv(file.choose("df.csv"),h=T,sep=";",dec=".");head(df)

# (1)
##	H0.1: VI.1 n'as pas d'influence sur VD
##	H0.2: VI.2 n'as pas d'influence sur VD
##	H1.1: VI.1 a une influence sur VD
##	H1.2: VI.2 a une influence sur VD
# (2)
VD<-df$masse					# quantitative
VI.1<-as.factor(df$polluant)			# qualitative, fixe ou aléatoire
VI.2<-as.factor(df$aquarium)			# sous-groupe de VI, qualitative, fixe ou aléatoire
# !! définir facteur nominal en facteur pour l'analyse !!
par(mfrow=c(1,2))
plot(VD~VI.1);plot(VD~VI.2)
# (3) ANOVA hiérarchique; modèle mixte (III)
# (4) tests des postulats
	#4.1	Normalité des résidus
residu<-residuals(aov(VD~VI.1+VI.2%in%VI.1))
qqnorm(residu);qqline(residu)
shapiro.test(residu)		# confirmation si non-significatif
skewness(residu)		# activer les fonctions dans "fonctions.R", entre -0.5 et 0.5 -> normalité
kurtosis(residu)
	#4.2 	Homogénéité des variances
plot(as.numeric(VI.2),residu)
bartlett.test(VD~VI.2)	# H0: normalité, H1: non normalité
# (5) mise en place du test
anova(aov(VD~VI.1+VI.2%in%VI.1))
# correction de la valeur F et de P pour l'effet fixe VI.1
F.pol<-anova(aov(VD~VI.1+VI.2%in%VI.1))[1,3]/anova(aov(VD~VI.1+VI.2%in%VI.1))[2,3]
F.pol;pf(F.pol,3,8,lower.tail=F)
# (6) interprétation biologique


### crossed-factors anova

# DEMARCHE EN 6 POINTS

VD~VI.a+VI.b+VI.a:VIb		# ou VD~VI.a*VI.b
# !! R considère chaque facteur comme fixe et non aléatoire
# -> corriger F-ratios à la main !!

df<-read.table(file.choose("df.txt"),h=T);head(df)
df<-read.csv(file.choose("df.csv"),h=T,sep=";",dec=".");head(df)
# (1)
##	H0.1: VI.a n'as pas d'influence sur VD
##	H0.2: VI.b n'as pas d'influence sur VD
##	H0.3: l'interaction de VI.a et VI.b n'a pas d'influence sur VD
##	H1.1: VI.a a une influence sur VD
##	H1.2: VI.b a une influence sur VD
##	H1.3: l'interaction de VI.a et VI.b a une influence sur VD
# (2)
VD<-df$measure			# quantitative
VI.a<-as.factor(df$genotype)	# qualitative, fixe ou aléatoire
a<-2					# nombre de modalité par A
VI.b<-as.factor(df$treatment)	# qualitative, fixe ou aléatoire
b<-2					# nombre de modalité par B
n<-5					# nombre total de donnée / a*b :D

par(mfrow=c(1,2))
plot(VD~VI.a);plot(VD~VI.b)	# ou
par(mfrow=c(1,1));boxplot(VD~VI.a*VI.b)
# (3)	Faire une table d'ANOVA (modèle I, II ou III) pour les Df surtout
# (4) tests des postulats
	#4.1	Normalité des résidus
par(mfrow=c(1,1))
residu<-residuals(aov(VD~VI.a*VI.b))
qqnorm(residu);qqline(residu)
shapiro.test(residu)	# confirmation si non-significatif
	#4.2 	Homogénéité des variances
groupe<-rep(1:(a*b),each=n)	# vecteur permettant de classer les données en groupes
plot(groupe,residu)
bartlett.test(VD~groupe)	# H0: normalité, H1: non normalité
# (5) mise en place du test
anova(aov(VD~VI.a*VI.b))	# si interaction non significative, retester sans l'interaction
anova(aov(VD~VI.a+VI.b))
## !! correction de F-ratio si présence d'effet aléatoire
F.corr<-40.76/13.06		# pour VI.a aléatoire, reprendre MSb/MSab de anova(aov(VD~VI.a*VI.b)), pour VI.b aléatoire, reprendre MSa/MSab
pf(F.corr,b-1,(a-1)*(b-1),lower.tail=F)	# reprendre df1 et df2
TukeyHSD(aov(VD~VI.a))		# les résultats très petits dans p adj sont les bons
TukeyHSD(aov(VD~VI.b))		# comparaison intra-groupe pour savoir d'où vient l'effet !

# (6) interprétation
