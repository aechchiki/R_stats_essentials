### kluskal-wallis
# rem: essaye d'abord log/arcsin/x² sur une ANOVA classique, keep this test as ultimate weapon~

# (1)	H0: il n'y a pas de différence entre individus des groupes
	#	H1: il y a une différence
VD<-df$VARDEP
VI<-as.factor(df$groupe)
par(mfrow=c(1,2))
hist(VD)
plot(VD~VI)
# (3) Test de Kruskal-Wallis
# (4) DÉTERMINER LES RANGS
# (5) Mise en place du test
kLuskal.test(VD~VI)
# (6) interprétation - Test de permutations
# (5)
f.stat<-vector(length=1000)
fobs<-anova(aov(VD~VI))[1,4]
for(i in 1:999)
	f.stat[i]<-anova(aov(VD~sample(VI)))[1,4]
f.stat[1000]<-fobs
sum(f.stat>=f.stat[1000])/1000
hist(f.stat,freq=FALSE,ylim=c(0,0.9),breaks=seq(0,9,0.5))
abline(v=f.stat[1000],lwd=2,col="red")
curve(df(x,2,57),0,9,add=TRUE,lwd=2,col="blue")
