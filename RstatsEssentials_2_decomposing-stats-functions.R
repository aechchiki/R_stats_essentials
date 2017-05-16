### decomposing stats functions

#médiane
median <- function(x) {
	n <- length(x)
	s <- sort(x)
	if (n%%2==1) {
	me <- s[(n+1)/2]
	}
	else {
	me <- s[(n/2) + ((n/(2+1))/2)]
	}
	return (me)
}

#variance sans biais
variance <- function(x) {
    n <- length(x)
    m <- sum(x)/n
    va <- sum((x-m)^2)/n
    return(va)
}


#variance avec biais
variance.biais <- function(x) {
	n <- length(x)
	m <- sum((x)/(n-1))
	vb <- sum(((x-m)^2)/(n-1))
	return(vb)
}

#écart-type sans biais
ecart.type <- function(x) {
	o2 <- x^2
	m2 <- (mean(x))^2
	va <- (mean(x^2)-m2)
	et <- sqrt(va)
	return(et)
}

#écart-type avec biais
ecart.type.biais <- function(x) {
	n <- length(x)-1
	f <- 1/n
	r <- sum((x-mean(x))^2)
	etb <- f*r
	return(etb)
}

#skewness
skew <- function(x) {
	par <- ((x-mean(x))^3)
	num <- mean(par)
	den <- (sd(x)^3)
	sk <- (num/den)
	return(sk)
}

#kurtosis
kurt <- function(x) {
	par <- ((x-mean(x))^4)
	num <- mean(par)
	den <- (sd(x)^4)
	ku <- ((num/den)-3)
	return(ku)
}

#IC student 95% pour une moyenne
stm95 <- function(x) {
	t <- 1.761
	m <- mean(x)
	s <- sd(x)
	r <- s/(sqrt(length(x)))
	stL <- y-(t*r)
	stR <- y+(t*r)
	return(c(stL, stR))
}

#IC student 95% pour une différence de moyennes
	#! d'abord, calculer le sigma~tilde
sigma.tilde <- function(m,p) {
	lm <- length(m)
	lp <- length(p)
	vm <- var(m)
	vp <- var(p)
	num <- (((lm-1)*vm)+((lp-1)*vp))
	den <- lm+lp-2
	st2 <- num/den
	return(st2)
}
stdm95 <- function(m,p) {
	t <- qt(0.95, df=(length(m)+length(p)-2))
	d <- mean(p)-mean(m)
	s <- sigma.tilde(m,p)
	rn <- length(m)+length(p)
	rd <- length(m)*length(p)
	r <- rn/rd
	q <- sqrt(r)
	stcL <- (d-(t*s*q))
	stcR <- (d+(t*s*q))
	return(c(stcL, stcR))
}

#IC Welch 95% pour une différence de moyennes
	#! d'abord, calculer le df souhaité
df <- function(m,p) {
	n1 <- var(m)/length(m)
	n2 <- var(p)/length(p)
	n <- (n1+n2)^2
	d1 <- ((var(m))^2)/(length(m)^2 * (length(m)-1))
	d2 <- ((var(p))^2)/(length(p)^2 * (length(p)-1))
	d <- d1+d2
	dfW <- n/d
	return(dfW)
}
wedm95 <- function(m,p) {
	t <- qt(0.975, df=(df(m,p)))
	d <- mean(p)-mean(m)
	r1 <- var(m)/length(m)
	r2 <- var(p)/length(p)
	r <- r1 + r2
	q <- sqrt(r)
	weL <- d-(t*q)
	weR <- d+(t*q)
	return(c(weL, weR))
}

#IC Wald 95% pour une différence de prorportions
	#! d'abord, calculer le standard error
	#p1= proportion corréspondante au vecteur de length n1
SEd <- function (p1, p2, n1, n2) {
	num1 <- p1*(1-p1)
	num2 <- p2*(1-p2)
	r <- (num1/n1)+(num2/n2)
	s <- sqrt(r)
	return(s)
}
wadp95 <- c(d+1.96*SEd (p1, p2, n1, n2), d-1.96*SEd (p1, p2, n1, n2))

#IC Wald 95% our une moyenne
wam95 <- function(x) {
	z <- 1.96
	y <- mean(x)
	s <- sd(x)
	n <- sqrt(length(x))
	waL <- y-z*(s/n)
	waR <- y+z*(s/n)
	return(c(waL, waR))
}

# Test de Levene
LeveneTest<-function(data,groupes){
	resid<-residuals(aov(data~groupes))
	z<-abs(resid)
	an<-anova(lm(z~groupes))
	pvalue<-an[["Pr(>F)"]][1]
	return(pvalue)
}

# Test de t
PvalTest<-function(x){
	t.test(x)$p.value
}

# ANOVA à 1 facteur
FunAnova <- function( data, groupe ) {
	n <- length(data)
	g <- length(levels(groupe))
	ng <- n/g
	v1 <- mean( tapply( data, groupe, var) )
 	v2 <- var( tapply( data, groupe, mean)) * ng
 	s1<-v1*(n-g)
 	s2<-v2*(g-1)
 	fstat <- v2/v1
 	p <- pf( fstat, g-1, n-g, lower.tail=F)
 	vest <- (v2 - v1) / ng
 	return( c(g-1, n-g, s2,s1,v2, v1, fstat, p, vest ) )
}
