### multiple regression

# démarche en 6 points
df<-read.table(file.choose("df.txt"),h=T)
names(df);head(df)

# (1) 	Poser les hypothèses nulles et alternatives
# (2) 	Définir les variables
VD<-df$poids			# quantitative
VI.1<-df$nourriture		# toutes les VI sont quantitatives !
VI.2<-df$tgroupe		# pas de limite au nombre de VI
VI.3<-df$superficie
VI.4<- # etc.			#WAAAAH C'EST LA FÊTE! YOLO!
cor.test(VI.1,VI.2);cor.test(VI.1,VI.3);cor.test(VI.2,VI.3)
plot(df[,-3])			# la corrélation est importante pour la normalité
# (3) 	Plusieurs régression linéaire -> régression multiple
par(mfrow=c(1,3))
plot(VD~VI.1);plot(VD~VI.2);plot(VD~VI.3)
# (4) Postulats:
	#1) linéarité, 2) homoscedasticité, 3) normalité, 4) indépendance
m<-lm(VD~VI.1+VI.2+VI.3)
par(mfrow=c(2,2));plot(m)
# (5) 	Test
summary(m1)
summary(m2)
summary(m3)
# (6) interprétation biologique
## Contrairement aux régressions linéaires simples, la régression linéaire
## multiple met en évidence un effet significatif négatif (dans ce cas!)
plot(VD~VI.1,pch=21,bg=palette()[VI.2])
