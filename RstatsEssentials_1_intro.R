### intro: in case you forgot how to R

#UTF-8 pour Linux/Mac (Unicode, comp ASCII); Windows preload

df<-read.table(file.choose(),h=T)	#vérifie pour chaque colonne aussi sont type
df<-read.csv(file.choose(),h=T,sep=",",dec=".")

require (packagename)		#installe un package extra (CRAN from Switzerland)
library()			#parcourit les fonctions disponibles dans le workspace
?function 			#ouvre la page d'aide pour la fonction désirée
na.omit()			#omet les NA d'un objet lors d'une analyse.le fichier reste inchangé
a[is.na(a)]<-mean(a,na.rm=TRUE)	#remplace les NA par la moyenne du vecteur
names()				#renvoye les nom en header de chaque colonne du fichier
ls() 				#renvoye une liste d'objets dans un workspace
rm(); rm(list=ls())		#élimine un objet ou une suite de le workspace
	na.rm()			#élimine les na d'un objet (! cause décalage)
sample()			#choix aléatoire suivant une binomiale d'un vecteur d"fini
par(mfrow=c(n,m))		#prepare une base n*m pour y visualiser les graphes
length(a) 			#longeur de a (Ou nombre d'éléments de a)
var(a) 				#variance du vecteur
mean(a)				#moyenne du vecteur a
sd(a) 				#écart-type du vecteur a
sort(a) 			#trie le vecteur en ordre croissant
order(a) 			#donne l'ordre des valeurs du vecteur
summary(a) 			#renvoie les "valeurs-clés" du vecteur
quantile(a)			#renvoie uniquement les quantiles
rev(a) 				#inverse l'ordre du vecteur a
head(a,n) 			#donne les n valeurs supérieures de a
tail(a,n) 			#donne les n valeurs inférieures de a
sum(a) 				#somme des éléments du vecteur a
max(a); min(a) 			#renvoie le maximum / le minimum de a
range(a)			#renvoie le maximum ET le minimu de a
rnorm(x,mean=m,sd=d) 		#génère un ensemble selon la loi normale.
a[n] 				#sélectionne l'élément n du vecteur a
a[-n] 				#supprime l'élément n du vecteur a
a[n]<-b 			#assigne à l'élément n du vecteur a la valeur b
a[n]<-a[n]*m			#opérations effectués pour modifier un élément n du vecteur a
a[n]<-a[n]-m
a[n]<-a[n]+m
a[n]<-a[n]/m
a[a>n]				#renvoie la liste des éléments du vecteur en T/F
a[a<n]
a[a==n]
a[a>=n]
a[a<=n]
a[a>n & a<m]			#renvoie la liste des éléments de a compris entre n et m
a[n:m]				#sélectionnne les valeurs n à m du vecteur a (Par exemple, les éléments 3 à 6 du vecteur)
a==b; df1$a==df2$b 		#teste si les deux vecteurs (Ou colones) sont identiques (renvoie true ou false)
n<-c(a:b)			#génère un vecteur n contenant les nombres de a à b
sample(x,size,replace=[F/T],prob=p) #échantillonage
seq(from=a,to=b,by=c) 		#génère une série de nombre de a à b en c intervalles
apply(x,MARGIN,FUN)		#x= objet matriciel; MARGIN=1(→lignes) =2(→colonnes); FUN=function
	apply(x,MARGIN=1,FUN=function(x){t.test(x)$p.value})	# p.value pour chaque ligne
boxplot(); plot(); hist(); curve()
as.numeric(): as.factor(); as.matrix(); as.vector() #transformation d'objets

# couleurs
plot(...,pch=19,col=c("black","red","blue")[...])
plot(...,pch=19,col=palette()[...])
plot(...,pch=21,bg=palette()[...])
points(a[c==n],b[c==n],col="color")#Permet de séparer en colorant les points

#fichier texte trop lourd pour la commande read.table
dum<-readLines(file.choose())
dum<-lapply(dum,strsplit," ")
dat<-matrix(as.numeric(unlist(lapply(dum[-1],fun<-function(x) x[[1]][-c(1:2)]))),nrow=10,byrow=TRUE)
dat<-data.frame(dat)
x<-unlist(strsplit(unlist(dum[1]),"\""))
x[x==""]<-NA
x<-x[!is.na(x)]
names(dat)<-x[-1]

#DEMARCHE EN SIX POINTS
#Au préalable: nettoyer les données, omettre les NAs

	#1) Mettre en place l'hypothèse nulle (Qu'est-ce que on cherche à prouver faux)
	#2) Définir quelles sont les varaibles (variables explicatives (qui expliquent) )
		#et varaiable expliquée (ce que on explique) - définir les données à disposition
	#3) Choisir les tests et brièvemetn expliquer en quoi ils consistent
	#4) Tester si les conditions pour appliquer ce test sont remplies (Par exemple, si les données sont normales)
	#5) effectuer le test
	#6) Interpréter les résultats
