require(qdist)
require(qshift)
require(ggplot2)
###################################################
# definire tutti le funzioni per le distribuzioni #
###################################################

# Normale

dtnorm <- dtruncate("norm")
ptnorm <- ptruncate("norm")
qtnorm <- qtruncate("norm")
rtnorm <- rtruncate("norm")

dsnorm <- dshift("norm")
psnorm <- pshift("norm")
qsnorm <- qshift("norm")
rsnorm <- rshift("norm")

############# d ################

#############################
# prima tronco e poi shifto #
#############################
a <- dnorm(x = 1:10, mean = 5, sd = 3)
dtnorm <- dtruncate("norm")
b <- dtnorm(x = 1:10, mean = 5, sd = 3, L = 2, U = 8)

dtsnorm <- dshift("tnorm")

c <- dtsnorm(x = 1:10, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl
#############################
# prima shifto e poi tronco #
#############################

dsnorm <- dshift("norm")
b <- dsnorm(x = 1:10, mean = 5, sd = 3, shift=2)

dtsnorm <- dtruncate("snorm")
ptsnorm <- ptruncate("snorm")
c <- dtsnorm(x = 1:10, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# p ################

#############################
# prima tronco e poi shifto #
#############################
a <- pnorm(q = 1:10, mean = 5, sd = 3)
ptnorm <- ptruncate("norm")
b <- ptnorm(q = 1:10, mean = 5, sd = 3, L = 2, U = 8)

ptsnorm <- pshift("tnorm")

c <- ptsnorm(q = 1:10, mean = 5, sd = 3, shift=2, L = 2, U = 8)
c <- qtsnorm(p = c(0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.1374524, 0.3087584, 0.5000000, 0.6912416, 0.8625476, 1.0000000), mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

psnorm <- pshift("norm")
b <- psnorm(q = 1:10, mean = 5, sd = 3, shift=2)

ptsnorm <- ptruncate("snorm")
c <- ptsnorm(q = 1:10, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# q ################

#############################
# prima tronco e poi shifto #
#############################
a <- qnorm(p = (1:999)/1000, mean = 5, sd = 3)
qtnorm <- qtruncate("norm")
b <- qtnorm(p = (1:999)/1000, mean = 5, sd = 3, L = 2, U = 8)

qtsnorm <- qshift("tnorm")

c <- qtsnorm(p = (1:999)/1000, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

###############################
# prove da p a q e viceversa  #
###############################

# tronco poi shifto
a <- qtsnorm(p = (1:9)/10, mean = 5, sd = 3, shift=2, L = 2, U = 8)
ptsnorm(q = a, mean = 5, sd = 3, shift=2, L = 2, U = 8)
# giusto

a <- ptsnorm(q = 1:10, mean = 5, sd = 3, shift=2, L = 2, U = 8)
qtsnorm(p = a, mean = 5, sd = 3, shift=2, L = 2, U = 8)

# shifto poi tronco
c <- qtsnorm(p = 1:9/10, mean = 5, sd = 3, shift=2, L = 2, U = 8)
ptsnorm(q = c, mean = 5, sd = 3, shift=2, L = 2, U = 8)


c <- ptsnorm(q = 1:10, mean = 5, sd = 3, shift=2, L = 2, U = 8)
qtsnorm(p = c, mean = 5, sd = 3, shift=2, L = 2, U = 8)


# da q a p è sempre giusto, da p a q no

#############################
# prima shifto e poi tronco #
#############################

qsnorm <- qshift("norm")
b <- qsnorm(p = (1:999)/1000, mean = 5, sd = 3, shift=2)

qtsnorm <- qtruncate("snorm")
c <- qtsnorm(p = (1:999)/1000, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# r ################

#############################
# prima tronco e poi shifto #
#############################
a <- rnorm(n = 100, mean = 5, sd = 3)
rtnorm <- rtruncate("norm")
b <- rtnorm(n = 100, mean = 5, sd = 3, L = 2, U = 8)

rtsnorm <- rshift("tnorm")

c <- rtsnorm(n = 100, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

rsnorm <- rshift("norm")
b <- rsnorm(n = 100, mean = 5, sd = 3, shift=2)

rtsnorm <- rtruncate("snorm")
c <- rtsnorm(n = 100, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

###################################################################################

# Uniforme

dtunif <- dtruncate("unif")
ptunif <- ptruncate("unif")
qtunif <- qtruncate("unif")
rtunif <- rtruncate("unif")

dsunif <- dshift("unif")
psunif <- pshift("unif")
qsunif <- qshift("unif")
rsunif <- rshift("unif")

############# d ################

#############################
# prima tronco e poi shifto #
#############################
a <- dunif(x = 1:10, min = 1, max = 9)
dtunif <- dtruncate("unif")
b <- dtunif(x = 1:10, min = 1, max = 9, L = 2, U = 8)

dtsunif <- dshift("tunif")

c <- dtsunif(x = 1:10, min = 1, max = 9, L = 2, U = 8, shift = 3)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl
#############################
# prima shifto e poi tronco #
#############################

dsunif <- dshift("unif")
b <- dsunif(x = 1:10, min = 1, max = 9, shift=2)

dtsunif <- dtruncate("sunif")
c <- dtsunif(x = 1:10, min = 1, max = 9, L = 2, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# p ################

#############################
# prima tronco e poi shifto #
#############################
a <- punif(q = 1:10, min = 1, max = 9)
ptunif <- ptruncate("unif")
b <- ptunif(q = 1:10, min = 1, max = 9, L = 2, U = 8)

ptsunif <- pshift("tunif")

c <- ptsunif(q = 1:10, min = 1, max = 9, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

psunif <- pshift("unif")
b <- psunif(q = 1:10, min = 1, max = 9, shift=2)

ptsunif <- ptruncate("sunif")
c <- ptsunif(q = 1:10, min = 1, max = 9, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# q ################

#############################
# prima tronco e poi shifto #
#############################
a <- qunif(p = (1:999)/1000, min = 1, max = 9)
qtunif <- qtruncate("unif")
b <- qtunif(p = (1:999)/1000, min = 1, max = 9, L = 2, U = 8)

qtsunif <- qshift("tunif")

c <- qtsunif(p = (1:999)/1000, min = 1, max = 9, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

qsunif <- qshift("unif")
b <- qsunif(p = (1:999)/1000, min = 1, max = 9, shift=2)

qtsunif <- qtruncate("sunif")
c <- qtsunif(p = (1:999)/1000, min = 1, max = 9, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# r ################

#############################
# prima tronco e poi shifto #
#############################
a <- runif(n = 100, min = 1, max = 9)
rtunif <- rtruncate("unif")
b <- rtunif(n = 100, min = 1, max = 9, L = 2, U = 8)

rtsunif <- rshift("tunif")

c <- rtsunif(n = 100, min = 1, max = 9, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

rsunif <- rshift("unif")
b <- rsnorm(n = 100, mean = 5, sd = 3, shift=2)

rtsunif <- rtruncate("sunif")
c <- rtsnorm(n = 100, mean = 5, sd = 3, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

###################################################################################


###################################################################################

# Poisson

dtpois <- dtruncate("pois")
ptpois <- ptruncate("pois")
qtpois <- qtruncate("pois")
rtpois <- rtruncate("pois")

dspois <- dshift("pois")
pspois <- pshift("pois")
qspois <- qshift("pois")
rspois <- rshift("pois")

############# d ################

#############################
# prima tronco e poi shifto #
#############################
a <- dpois(x = 1:10, lambda = 2)
dtpois <- dtruncate("pois")
b <- dtpois(x = 1:10, lambda = 2, L = 2, U = 8)

dtspois <- dshift("tpois")

c <- dtspois(x = 1:10, lambda = 2, L = 2, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl
#############################
# prima shifto e poi tronco #
#############################

dspois <- dshift("pois")
b <- dspois(x = 1:10, lambda = 2, shift=2)

dtspois <- dtruncate("spois")
c <- dtspois(x = 1:10, lambda = 2, L = 2, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# p ################

#############################
# prima tronco e poi shifto #
#############################
a <- ppois(q = 1:10, lambda = 2)

ptpois <- ptruncate("pois")
b <- ptpois(q = 1:10, lambda = 2, L = 2, U = 8)

ptspois <- pshift("tpois")
c <- ptspois(q = 1:10, lambda = 2, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

pspois <- pshift("pois")
b <- pspois(q = 1:10, lambda = 2, shift=2)

ptspois <- ptruncate("spois")
c <- ptspois(q = 1:10, lambda = 2, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# q ################

#############################
# prima tronco e poi shifto #
#############################
a <- qpois(p = (1:999)/1000, lambda = 2)
qtpois <- qtruncate("pois")
b <- qtpois(p = (1:999)/1000, lambda = 2, L = 2, U = 8)

qtspois <- qshift("tpois")

c <- qtspois(p = (1:999)/1000, lambda = 2, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

qspois <- qshift("pois")
b <- qspois(p = (1:999)/1000, lambda = 2, shift=2)

qtspois <- qtruncate("spois")
c <- qtspois(p = (1:999)/1000, lambda = 2, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# r ################

#############################
# prima tronco e poi shifto #
#############################
a <- rpois(n = 100, lambda = 2)
rtpois <- rtruncate("pois")
b <- rtpois(n = 100, lambda = 2, L = 2, U = 8)

rtspois <- rshift("tpois")
c <- rtspois(n = 100, lambda = 2, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

rspois <- rshift("pois")
b <- rspois(n = 100, lambda = 2, shift=2)

rtspois <- rtruncate("spois")
c <- rtspois(n = 100, lambda = 2, shift=2, L = 2, U = 4)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

###################################################################################


###################################################################################

# Gamma

dtgamma <- dtruncate("gamma")
ptgamma <- ptruncate("gamma")
qtgamma <- qtruncate("gamma")
rtgamma <- rtruncate("gamma")

dsgamma <- dshift("gamma")
psgamma <- pshift("gamma")
qsgamma <- qshift("gamma")
rsgamma <- rshift("gamma")

############# d ################

#############################
# prima tronco e poi shifto #
#############################
a <- dgamma(x = 1:10, shape = 2, rate = 1.5)
dtgamma <- dtruncate("gamma")
b <- dtgamma(x = 1:10, shape = 2, rate = 1.5, L = 2, U = 8)

dtsgamma <- dshift("tgamma")

c <- dtsgamma(x = 1:10, shape = 2, rate = 1.5, L = 2, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl
#############################
# prima shifto e poi tronco #
#############################

dsgamma <- dshift("gamma")
b <- dsgamma(x = 1:10, shape = 2, rate = 1.5, shift=2)

dtsgamma <- dtruncate("sgamma")
c <- dtsgamma(x = 1:10, shape = 2, rate = 1.5, L = 4, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# p ################

#############################
# prima tronco e poi shifto #
#############################
a <- pgamma(q = 1:10, shape = 2, rate = 1.5)

ptgamma <- ptruncate("gamma")
b <- ptgamma(q = 1:10, shape = 2, rate = 1.5, L = 2, U = 8)

ptsgamma <- pshift("tgamma")
c <- ptsgamma(q = 1:10, shape = 2, rate = 1.5, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

psgamma <- pshift("gamma")
b <- psgamma(q = 1:10, shape = 2, rate = 1.5, shift=2)

ptsgamma <- ptruncate("sgamma")
c <- ptsgamma(q = 1:10, shape = 2, rate = 1.5, shift=2, L = 2, U = 3)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# q ################

#############################
# prima tronco e poi shifto #
#############################
a <- qgamma(p = (1:999)/1000, shape = 2, rate = 1.5)
qtgamma <- qtruncate("gamma")
b <- qtgamma(p = (1:999)/1000, shape = 2, rate = 1.5, L = 2, U = 4)

qtsgamma <- qshift("tgamma")

c <- qtsgamma(p = (1:999)/1000, shape = 2, rate = 1.5, shift=2, L = 2, U = 4)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

qsgamma <- qshift("gamma")
b <- qsgamma(p = (1:999)/1000, shape = 2, rate = 1.5, shift=2)

qtsgamma <- qtruncate("sgamma")
c <- qtsgamma(p = (1:999)/1000, shape = 2, rate = 1.5, shift=2, L = 2, U = 5)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# r ################

#############################
# prima tronco e poi shifto #
#############################
a <- rgamma(n = 100, shape = 2, rate = 1.5)
rtgamma <- rtruncate("gamma")
b <- rtgamma(n = 100, shape = 2, rate = 1.5, L = 2, U = 5)

rtsgamma <- rshift("tgamma")
c <- rtsgamma(n = 100, shape = 2, rate = 1.5, shift=2, L = 2, U = 5)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

rsgamma <- rshift("gamma")
b <- rsgamma(n = 100, shape = 2, rate = 1.5, shift=2)

rtsgamma <- rtruncate("sgamma")
c <- rtsgamma(n = 100, shape = 2, rate = 1.5, shift=2, L = 3, U = 4)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

###################################################################################


###################################################################################

# Chi quadro

dtchisq <- dtruncate("chisq")
ptchisq <- ptruncate("chisq")
qtchisq <- qtruncate("chisq")
rtchisq <- rtruncate("chisq")

dschisq <- dshift("chisq")
pschisq <- pshift("chisq")
qschisq <- qshift("chisq")
rschisq <- rshift("chisq")

############# d ################

#############################
# prima tronco e poi shifto #
#############################
a <- dchisq(x = 1:10, df = 4)
dtchisq <- dtruncate("chisq")
b <- dtchisq(x = 1:10, df = 4, L = 2, U = 8)

dtschisq <- dshift("tchisq")
c <- dtschisq(x = 1:10, df = 4, L = 2, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl
#############################
# prima shifto e poi tronco #
#############################

dschisq <- dshift("chisq")
b <- dschisq(x = 1:10, df = 4, shift=2)

dtschisq <- dtruncate("schisq")
c <- dtschisq(x = 1:10, df = 4, L = 4, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# p ################

#############################
# prima tronco e poi shifto #
#############################
a <- pchisq(q = 1:10, df = 4)

ptchisq <- ptruncate("chisq")
b <- ptchisq(q = 1:10, df = 4, L = 2, U = 8)

ptschisq <- pshift("tchisq")
c <- ptschisq(q = 1:10, df = 4, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

pschisq <- pshift("chisq")
b <- pschisq(q = 1:10, df = 4, shift=2)

ptschisq <- ptruncate("schisq")
c <- ptschisq(q = 1:10, df = 4, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# q ################

#############################
# prima tronco e poi shifto #
#############################
a <- qchisq(p = (1:999)/1000, df = 4)
qtchisq <- qtruncate("chisq")
b <- qtchisq(p = (1:999)/1000, df = 4, L = 2, U = 8)

qtschisq <- qshift("tchisq")

c <- qtschisq(p = (1:999)/1000, df = 4, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

qschisq <- qshift("chisq")
b <- qschisq(p = (1:999)/1000, df = 4, shift=2)

qtschisq <- qtruncate("schisq")
c <- qtschisq(p = (1:999)/1000, df = 4, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# r ################

#############################
# prima tronco e poi shifto #
#############################
a <- rchisq(n = 100, df = 4)
rtchisq <- rtruncate("chisq")
b <- rtchisq(n = 100, df = 4, L = 2, U = 5)

rtschisq <- rshift("tchisq")
c <- rtschisq(n = 100, df = 4, shift=4, L = 2, U = 5)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

rschisq <- rshift("chisq")
b <- rschisq(n = 100, df = 4, shift=4)

rtschisq <- rtruncate("schisq")
c <- rtschisq(n = 100, df = 4, shift=4, L = 5, U = 10)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

###################################################################################


###################################################################################

# Weibull

dtweibull <- dtruncate("weibull")
ptweibull <- ptruncate("weibull")
qtweibull <- qtruncate("weibull")
rtweibull <- rtruncate("weibull")

dsweibull <- dshift("weibull")
psweibull <- pshift("weibull")
qsweibull <- qshift("weibull")
rsweibull <- rshift("weibull")

############# d ################

#############################
# prima tronco e poi shifto #
#############################

a <- dweibull(x = 1:10, shape = 0.5, scale = 1.5)
dtweibull <- dtruncate("weibull")
b <- dtweibull(x = 1:10, shape = 0.5, scale = 1.5, L = 2, U = 8)

dtsweibull <- dshift("tweibull")
c <- dtsweibull(x = 1:10, shape = 0.5, scale = 1.5, L = 2, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl
#############################
# prima shifto e poi tronco #
#############################

dsweibull <- dshift("weibull")
b <- dsweibull(x = 1:10, shape = 0.5, scale = 1.5, shift=2)

dtsweibull <- dtruncate("sweibull")
c <- dtsweibull(x = 1:10, shape = 0.5, scale = 1.5, L = 2, U = 8, shift = 2)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# p ################

#############################
# prima tronco e poi shifto #
#############################
a <- pweibull(q = 1:10, shape = 0.5, scale = 1.5)

ptweibull <- ptruncate("weibull")
b <- ptweibull(q = 1:10, shape = 0.5, scale = 1.5, L = 2, U = 8)

ptsweibull <- pshift("tweibull")
c <- ptsweibull(q = 1:10, shape = 0.5, scale = 1.5, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

psweibull <- pshift("weibull")
b <- psweibull(q = 1:10, shape = 0.5, scale = 1.5, shift=2)

ptsweibull <- ptruncate("sweibull")
c <- ptsweibull(q = 1:10, shape = 0.5, scale = 1.5, shift=2, L = 2, U = 6)

df <- data.frame(a,b,c,d =1:10, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# q ################

#############################
# prima tronco e poi shifto #
#############################
a <- qweibull(p = (1:999)/1000, shape = 0.5, scale = 1.5)
qtweibull <- qtruncate("weibull")
b <- qtweibull(p = (1:999)/1000, shape = 0.5, scale = 1.5, L = 2, U = 8)

qtsweibull <- qshift("tweibull")
c <- qtsweibull(p = (1:999)/1000, shape = 0.5, scale = 1.5, shift=2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

qsweibull <- qshift("weibull")
b <- qsweibull(p = (1:999)/1000, shape = 0.5, scale = 1.5, shift=2)

qtsweibull <- qtruncate("sweibull")
c <- qtsweibull(p = (1:999)/1000, shape = 0.5, scale = 1.5, shift = 2, L = 2, U = 8)

df <- data.frame(a,b,c,d =1:999, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl


############# r ################

#############################
# prima tronco e poi shifto #
#############################
a <- rweibull(n = 100, shape = 0.5, scale = 1.5)
rtweibull <- rtruncate("weibull")
b <- rtweibull(n = 100, shape = 0.5, scale = 1.5, L = 2, U = 5)

rtsweibull <- rshift("tweibull")
c <- rtsweibull(n = 100, shape = 0.5, scale = 1.5, shift=4, L = 2, U = 5)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue")+
  geom_line(data = df, mapping = aes(y=b, x=d), col="red")+
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

#############################
# prima shifto e poi tronco #
#############################

rsweibull <- rshift("weibull")
b <- rsweibull(n = 100, shape = 0.5, scale = 1.5, shift=4)

rtsweibull <- rtruncate("sweibull")
c <- rtsweibull(n = 100, shape = 0.5, scale = 1.5, shift=4, L = 5, U = 10)

df <- data.frame(a,b,c,d =1:100, stringsAsFactors = F)

pl <- ggplot() +
  geom_line(data = df, mapping = aes(y=a, x=d), col="blue") +
  geom_line(data = df, mapping = aes(y=b, x=d), col="red") +
  geom_line(data = df, mapping = aes(y=c, x=d), col="green")

pl

###################################################################################

# prova ottimizzazione  --> ok

### shiftata troncata

dsweibull <- dshift("weibull")
psweibull <- pshift("weibull")
qsweibull <- qshift("weibull")
dtsweibull <- dtruncate("sweibull")

ltweibull <- function(x, L = -Inf, U = Inf){
  # starting values for parameters (scale, shape) and shift
  shift <- min(x) - 0.01
  x1 <- x - shift
  shape <- (sd(x1)/mean(x1))^(-1.086)
  scale <- mean(x1)/gamma(1+1/shape)
  # parameters vector definition
  theta <- c(shape, scale, shift)
  # likelihood function
  ll <- function(theta, x, L = -Inf, U = Inf){
    shape <- theta[1]
    scale <- theta[2]
    shift <- theta[3]
    ld <- dtsweibull(x = x, shape = shape, scale = scale, shift = shift, L = L, U = U, log=T)
    -sum(ld)
  }
  # maximum likelihood estimation
  optim(par = theta, fn = ll, x = x, L = L, U = U, method = "Nelder-Mead")[["par"]]
}


rsweibull <- rshift("weibull")
rtsweibull <- rtruncate("sweibull")
x <- rtsweibull(n = 10000000, shape = 1, scale = 1.5, shift = 1, L = 1, U = 3)

# maximum likelihood estimate for the shifted weibull distribution
ltweibull(x = x, L = 1, U = 3)

## funzionaaaaaaaaaaaa

##############################################################################

### troncata shiftata -> funzionaaaaaaaaaaa

dtweibull <- dtruncate("weibull")
dstweibull <- dshift("tweibull")


ltweibull <- function(x, shift = 0 ,L = -Inf, U = Inf){
  # starting values for parameters (scale, shape) and shift
  shift <- min(x) - 0.01
  x1 <- x - shift
  #x1 <- x
  shape <- (sd(x1)/mean(x1))^(-1.086)
  scale <- mean(x1)/gamma(1+1/shape)
  # parameters vector definition
  theta <- c(shape, scale, shift)
  #theta <- c(shape, scale)
  # likelihood function
  ll <- function(theta, x, L = -Inf, U = Inf){
    shape <- theta[1]
    scale <- theta[2]
    shift <- theta[3]
    ld <- dstweibull(x = x, shape = shape, scale = scale, shift = shift, L = L, U = U, log=TRUE)
    -sum(ld)
  }
  # maximum likelihood estimation
  optim(par = theta, fn = ll, x = x, L = L, U = U, method = "Nelder-Mead")[["par"]]
}


rtweibull <- rtruncate("weibull")
rstweibull <- rshift("tweibull")
x <- rstweibull(n = 100000, shape = 1, scale = 1.5, shift = 1, L = 1, U = 1000)
x <- rstweibull(n = 100000, shape = 1, scale = 5, shift=2, L = 1, U = 100)
# maximum likelihood estimate for the shifted weibull distribution
ltweibull(x = x, L = 1, U = 1000)
ltweibull(x = x, shift=2, L = 1, U = 100)


trnorm <- rtruncate("norm")
x <- trnorm(n = 1000, mean = 5, sd = 2, L = 3, U = 6)

sd(x)

ltnorm <- function(x, L = -Inf, U = Inf) {
  theta <- c(5, 2)
  ll <- function(theta, x, L = -Inf, U = Inf) {
    mean <- theta[1]
    sd <- theta[2]
    ld <- log(tdnorm(x = x, mean = mean, sd = sd, L = L , U = U))
    -sum(ld)
  }
  optim(par = theta, fn = ll, x = x, L = L, U = U, method = "Nelder-Mead")[["par"]]
}

ltnorm( x = x, L = 3, U = 6)
tdnorm <- dtruncate("norm")





