

##################
# L3 Micro – TD1 #
##################

	setwd("~/Documents/Cours/Toulouse School of Economics/L3 Microeconomics/TDs/Code")
	#library(stargazer)
	
	layout(1)
	par(mar=c(4,4,1,4) + 0.1, mgp=c(3,1,0))		# marges par défaut
	dev.off()



# Exercice 1 : Préférences Cobb-Douglas #
#########################################

# Données du problème

	omega1 <- 40									# ressources totales en bien 1
	omega2 <- 30									# ressources totales en bien 2
	alpha <- 1/4									# importance relative du bien 1 pour A
	beta <- 3/4										# importance relative du bien 2 pour A
	UA <- function(x1, x2) x1^alpha * x2^beta		# fonction d'utilité de A
	UB <- function(x1, x2) x1 * x2					# fonction d'utilité de B
	
	
# Cartes d'indifférence
	
	step <- .01
	x1 <- seq(0,omega1, by=step)
	
	indiffA <- function(U) { (U/x1^alpha)^(1/beta) }
	indiffB <- function(U) { U/x1 }
	
	niveauxA <- c(6, 12, 18, 24)
	niveauxB <- c(10, 100, 250, 500)
	
	dev.new(width=8, heigth=8*omega2/omega1)
	plot(x = x1, y = x1, xlim = c(0, omega1), ylim = c(0, omega2), type = "n", asp=NA, xlab="Bien 1", ylab="Bien 2", xaxs="i", yaxs="i")
	for (U in niveauxA) lines(x1, indiffA(U), col = "dodgerblue", ylim = c(0, omega2))
	for (U in niveauxB) lines(x1, rev(omega2 - indiffB(U)), col = "darkorange", ylim = c(0, omega2))
	
	
# Courbe des optima
	
	optima <- function(x) omega2*beta*x/(alpha*omega1 + (beta - alpha)*x)
	
	allocA1 <- seq(1, 39, len=10)
	
	plot(x = x1, y = x1, xlim = c(0, omega1), ylim = c(0, omega2), type = "n", asp=NA, xlab="Bien 1", ylab="Bien 2", xaxs="i", yaxs="i")
	for (i in 1:length(allocA1)) {
		xA1 <- allocA1[i]
		xA2 <- optima(allocA1[i])
		#points(xA1, xA2)
		#range <- ((xA1-5)/step):((xA1+7)/step)
		lines(x1, indiffA(UA(xA1, xA2)), col = "dodgerblue", ylim = c(0, omega2))
		lines(x1, rev(omega2 - indiffB(UB(omega1 - xA1, omega2 - xA2))), col = "darkorange", ylim = c(0, omega2))
	}
	lines(x1, optima(x1), col = "purple", lwd = 2)
	
	
# Allocations acceptées par A
	
	I <- c(20, 15)
	
	plot(x = x1, y = x1, xlim = c(0, omega1), ylim = c(0, omega2), type = "n", asp=NA, xlab="Bien 1", ylab="Bien 2", xaxs="i", yaxs="i")
	points(I[1], I[2], pch = 3)
	text(I[1]+1, I[2]+1, labels = "I", cex = 1.3, font = 3)
	lines(x1, indiffA(UA(I[1], I[2])), col = "dodgerblue", ylim = c(0, omega2))
	lines(x1, optima(x1), col = "purple", lwd = 1)
	rangeA <- 1265:length(x1)
	lines(x1[rangeA], optima(x1)[rangeA], col = "dodgerblue", lwd = 2, lty = 2)
	
	
# Allocations acceptées par B
	
	lines(x1, rev(omega2 - indiffB(UB(omega1 - I[1], omega2 - I[2]))), col = "darkorange", ylim = c(0, omega2))
	rangeB <- 1:1400
	lines(x1[rangeB], optima(x1)[rangeB], col = "darkorange", lwd = 2, lty = 2)
	
	
# Allocation X
	
	X <- c(10, 20)
	
	points(X[1], X[2], pch = 3)
	text(X[1]+1, X[2]+1, labels = "X", cex = 1.3, font = 3)



# Exercice 2 : Préférences quasi-linéaires #
############################################
		
# Données du problème
	
	omega1 <- 6
	omega2 <- 4
	UA <- function(x1, x2) log(x1) + x2			# fonction d'utilité de A
	UB <- function(x1, x2) 2*log(x1) + x2		# fonction d'utilité de B
		
	
# Cartes d'indifférence
	
	step <- .01
	x1 <- seq(0,omega1, by=step)
	
	indiffA <- function(U) U - log(x1)
	indiffB <- function(U) U - 2*log(x1)
		
	niveauxA <- seq(.5, 4, len = 4)
	niveauxB <- seq(.5, 6, len = 4)
	
	dev.new(width=8, heigth=8*omega2/omega1)
	plot(x = x1, y = x1, xlim = c(0, omega1), ylim = c(0, omega2), type = "n", asp=NA, xlab="Bien 1", ylab="Bien 2", xaxs="i", yaxs="i")
	for (U in niveauxA) lines(x1, indiffA(U), col = "dodgerblue", ylim = c(0, omega2))
	for (U in niveauxB) 	lines(x1, rev(omega2 - indiffB(U)), col = "darkorange", ylim = c(0, omega2))
	
	
# Courbe des optima internes
	
	allocA2 <- seq(0, 1, len=9)*omega2
	
	plot(x = x1, y = x1, xlim = c(0, omega1), ylim = c(0, omega2), type = "n", asp=NA, xlab="Bien 1", ylab="Bien 2", xaxs="i", yaxs="i")
	for (i in 1:length(allocA2)) {
		xA1 <- omega1/3
		xA2 <- allocA2[i]
		#points(xA1, xA2)
		#range <- ((xA1-.2*omega1)/step):((xA1+.2*omega1)/step)
		lines(x1, indiffA(UA(xA1, xA2)), col = "dodgerblue", ylim = c(0, omega2))
		lines(x1, rev(omega2 - indiffB(UB(omega1 - xA1, omega2 - xA2))), col = "darkorange", ylim = c(0, omega2))
	}
	lines(c(omega1/3, omega1/3), c(0, omega2), lwd = 2, col = "purple")
	
	
# Courbes des optima en coin
	
	xA1 <- omega1/5
	xA2 <- 0
	abline(h = 0, lty = 2)
	#points(xA1, xA2)
	#range <- ((xA1-.2*omega1)/step):((xA1+.2*omega1)/step)
	lines(x1, indiffA(UA(xA1, xA2)), col = "dodgerblue", ylim = c(0, omega2))
	lines(x1, rev(omega2 - indiffB(UB(omega1 - xA1, omega2 - xA2))), col = "darkorange", ylim = c(0, omega2))
	lines(c(0, omega1/3), c(0, 0)+.01, lwd = 2, col = "purple")
	
	
	xA1 <- 2*omega1/3
	xA2 <- omega2
	abline(h = omega2, lty = 2)
	#points(xA1, xA2)
	#range <- ((xA1-.2*omega1)/step):((xA1+.2*omega1)/step)
	lines(x1, indiffA(UA(xA1, xA2)), col = "dodgerblue", ylim = c(0, omega2))
	lines(x1, rev(omega2 - indiffB(UB(omega1 - xA1, omega2 - xA2))), col = "darkorange", ylim = c(0, omega2))
	lines(c(omega1/3, omega1), c(omega2, omega2)-.01, lwd = 2, col = "purple")
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
	
	
	
	
	