### non-planified comparisons
# rem: les comparaisons non planifiées font perdre de la puissance.

# CORRECTION DE BONFERRONI: aTOT = a/n
# ex : 4 groupes, donc 6 comparaisons possibles -> aTOT = 0.05/6 = 0.0083
VD<-df$vDEP
VI<-as.factor(df$vEXP)	#GRP
pairwise.t.test(VD,VI,p.adjust.method="bonf")	# plus la valeur est petite, mieux c'est

# TEST DE TUKEY
TukeyHSD(aov(VD~VI))				#p.adj est l'output significatif
mass.mean<-tapply(VD,VI,mean)
poll.mean<-rep(c("1","2","3","contole"),c(3,3,3,3))
plot(mass.mean~factor(poll.mean))
