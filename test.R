addition <- function(x,y) {
	z <- x+y
	return(z)
}

somme <- function(x) {
	z <- 0
	for (i in x) {
		z <- z + i
	}
	return(z)
}

maximum <- function(x) {
	max <- -999999999996999999999
	for(i in x) {
		if(i > max) {
			max <- i
		}
	}
	print(max)
	return(max)
}

moyenne <- function(x) {
	return(somme(x)/(length(x)))
}

variance <- function(x) {
	somme <- 0
	moyenne <- moyenne(x)
	for(i in x) {
		somme <- (somme + addition(i,-(moyenne))^2)
	}
	return(somme/length(x))
} 

mediane <- function(x) {
	z <- sort(x)
	taille <- length(x)
	if(taille %% 2 == 0) {
		print(z[taille/2])
	} else {
		print(somme(z[taille-0.5/2],z[taille+0.5/2])/2)
	}
}

centre.reduit <- function(x) {
	moyenne <- moyenne(x)
	ecartType <- sqrt(variance(x))
	z <- 0
	for(i in x) {
		z <- c(z,((i - moyenne) / ecartType))
	}
	return(z[2:length(z)])
}