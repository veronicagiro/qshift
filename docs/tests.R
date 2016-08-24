require(qshift)
require(ggplot2)

##########
# dshift #
##########

## distribuzione binomiale
d <- dshift("binom")
zero <- d(x = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 0)
one <- d(x = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 1)
two <- d(x = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## controllo se la versione di d non shiftata coincide con dbinom originale
zero <- d(x = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 0)
std <- dbinom(x = c(1,2,3,4,5), size = 6, prob = 0.5)
# d coincode con dbinom


## distribuzione poisson
d <- dshift("pois")
zero <- d(x = c(1,2,3,4,5), lambda = 1.5, shift = 0)
one <- d(x = c(1,2,3,4,5), lambda = 1.5, shift = 1)
two <- d(x = c(1,2,3,4,5), lambda = 1.5, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## controllo se la versione di d non shiftata coincide con dpois originale
zero <- d(x = c(1,2,3,4,5), lambda = 1.5, shift = 0)
std <- dpois(x = c(1,2,3,4,5), lambda = 1.5)
# d coincode con dpois


## distribuzione normale
d <- dshift("norm")
zero <- d(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 0)
one <- d(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 1)
two <- d(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, std, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## controllo se la versione di d non shiftata coincide con dnorm originale
zero <- d(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 0)
std <- dnorm(x = c(1,2,3,4,5), mean = 2, sd = 1)
# d coincode con dnorm


## distribuzione gamma
d <- dshift("gamma")
zero <- d(x = c(1,2,3,4,5), shape = 2, rate = 3, shift = 0)
one <- d(x = c(1,2,3,4,5), shape = 2, rate = 3, shift = 1)
two <- d(x = c(1,2,3,4,5), shape = 2, rate = 3, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok (problema rate scale (come articolo Andrea))

## controllo se la versione di d non shiftata coincide con dgamma originale
zero <- d(x = c(1,2,3,4,5), shape = 2, rate = 3, shift = 0)
std <- dgamma(x = c(1,2,3,4,5), shape = 2, rate = 3)
# d coincode con dgamma


## distribuzione weibull
d <- dshift("weibull")
zero <- d(x = c(1,2,3,4,5), shape = 1.4, scale = 3, shift = 0)
one <- d(x = c(1,2,3,4,5), shape = 1.4, scale = 3, shift = 1)
two <- d(x = c(1,2,3,4,5), shape = 1.4, scale = 3, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok


## controllo se la versione di d non shiftata coincide con dweibull originale
zero <- d(x = c(1,2,3,4,5), shape = 1.4, scale = 3, shift = 0)
std <- dweibull(x = c(1,2,3,4,5), shape = 1.4, scale = 3)
# d coincode con dweibull


## distribuzione t di Student
d <- dshift("t")
zero <- d(x = c(1,2,3,4,5), df = 10, shift = 0)
one <- d(x = c(1,2,3,4,5), df = 10, shift = 1)
two <- d(x = c(1,2,3,4,5), df = 10, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## controllo se la versione di d non shiftata coincide con dt originale
zero <- d(x = c(1,2,3,4,5), df = 10, shift = 0)
std <- dt(x = c(1,2,3,4,5), df = 10)
# d coincode con dt


## distribuzione Chi Quadro
d <- dshift("chisq")
zero <- d(x = c(1,2,3,4,5), df = 2, shift = 0)
one <- d(x = c(1,2,3,4,5), df = 2, shift = 1)
two <- d(x = c(1,2,3,4,5), df = 2, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## controllo se la versione di d non shiftata coincide con dchisq originale
zero <- d(x = c(1,2,3,4,5), df = 2, shift = 0)
std <- dchisq(x = c(1,2,3,4,5), df = 2)
# d coincode con dt


##############################################################################################

##########
# pshift #
##########

## distribuzione binomiale
p <- pshift("binom")
zero <- p(q = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 0)
one <- p(q = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 1)
two <- p(q = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok

## controllo se la versione di p non shiftata coincide con pbinom originale
zero <- p(q = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 0)
std <- pbinom(q = c(1,2,3,4,5), size = 6, prob = 0.5)
# d coincode con dt


## distribuzione poisson
p <- pshift("pois")
zero <- p(q = c(1,2,3,4,5), lambda = 1.5, shift = 0)
one <- p(q = c(1,2,3,4,5), lambda = 1.5, shift = 1)
two <- p(q = c(1,2,3,4,5), lambda = 1.5, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok


## distribuzione normale
p <- pshift("norm")
zero <- p(q = c(1,2,3,4,5), mean = 2, sd = 1, shift = 0)
one <- p(q = c(1,2,3,4,5), mean = 2, sd = 1, shift = 1)
two <- p(q = c(1,2,3,4,5), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl


p <- pshift("norm")
zero <- p(q = seq(1, 3, length.out = 100), mean = 2, sd = 1, shift = 0)
one <- p(q = seq(1, 3, length.out = 100), mean = 2, sd = 1, shift = 1)
two <- p(q = seq(1, 3, length.out = 100), mean = 2, sd = 1, shift = 2)

# ok


## distribuzione gamma
p <- pshift("gamma")
zero <- p(q = c(1,2,3,4,5), shape = 1, rate = 0.5, shift = 0)
one <- p(q = c(1,2,3,4,5), shape = 1, rate = 0.5, shift = 1)
two <- p(q = c(1,2,3,4,5), shape = 1, rate = 0.5, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok (problema rate scale (come articolo Andrea))


## distribuzione weibull
p <- pshift("weibull")
zero <- p(q = c(1,2,3,4,5), shape = 0.4, scale = 1, shift = 0)
one <- p(q = c(1,2,3,4,5), shape = 0.4, scale = 1, shift = 1)
two <- p(q = c(1,2,3,4,5), shape = 0.4, scale = 1, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok


## distribuzione t di Student
p <- pshift("t")
zero <- p(q = c(1,2,3,4,5), df = 40, shift = 0)
one <- p(q = c(1,2,3,4,5), df = 40, shift = 1)
two <- p(q = c(1,2,3,4,5), df = 40, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok


## distribuzione Chi Quadro
p <- pshift("chisq")
zero <- p(q = c(1,2,3,4,5), df = 2, shift = 0)
one <- p(q = c(1,2,3,4,5), df = 2, shift = 1)
two <- p(q = c(1,2,3,4,5), df = 2, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok


#########################################################################################

##########
# qshift #
##########

## distribuzione binomiale
q <- qshift("binom")
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5), size = 6, prob = 0.5, shift = 0)
one <- q(p = c(0.1,0.2,0.3,0.4,0.5), size = 6, prob = 0.5, shift = 1)
two <- q(p = c(0.1,0.2,0.3,0.4,0.5), size = 6, prob = 0.5, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok

## controllo se la versione di q non shiftata coincide con qbinom originale
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5), size = 6, prob = 0.5, shift = 0)
std <- qbinom(p = c(0.1,0.2,0.3,0.4,0.5), size = 6, prob = 0.5)
# q coincode con qbinom


## distribuzione poisson
q <- qshift("pois")
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), lambda = 1.5, shift = 0)
one <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), lambda = 1.5, shift = 1)
two <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), lambda = 1.5, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok


## distribuzione normale
q <- qshift("norm")
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 0)
one <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 1)
two <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), std, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok

## controllo se la versione di q non shiftata coincide con qbinom originale
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 0)
std <- qnorm(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1)
# q coincode con qbinom


## distribuzione gamma
q <- qshift("gamma")
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), shape = 2, rate = 3, shift = 0)
one <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), shape = 2, rate = 3, shift = 1)
two <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), shape = 2, rate = 3, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok?


## distribuzione weibull
q <- qshift("weibull")
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), shape = 1.4, scale = 3, shift = 0)
one <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), shape = 1.4, scale = 3, shift = 1)
two <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), shape = 1.4, scale = 3, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok


## distribuzione t di Student
q <- qshift("t")
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 10, shift = 0)
one <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 10, shift = 1)
two <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 10, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok


## distribuzione Chi Quadro
q <- qshift("chisq")
zero <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 2, shift = 0)
one <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 2, shift = 1)
two <- q(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 2, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok

##############################################################################################

##########
# rshift #
##########

## distribuzione binomiale
r <- rshift("binom")
zero <- r(n = 10000, size = 6, prob = 0.5, shift = 0)
one <- r(n = 10000, size = 6, prob = 0.5, shift = 1)
two <- r(n = 10000, size = 6, prob = 0.5, shift = 2)

### prova grafica
df <- data.frame(id = 1:10000, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = sort(zero)), col="blue") +
  geom_line(mapping = aes(x = id, y = sort(one)), col="red") +
  geom_line(mapping = aes(x = id, y = sort(two)), col="green")

pl

# ok


## distribuzione poisson
r <- rshift("pois")
zero <- r(n = 10000, lambda = 1.5, shift = 0)
one <- r(n = 10000, lambda = 1.5, shift = 1)
two <- r(n = 10000, lambda = 1.5, shift = 2)

### prova grafica
df <- data.frame(id = 1:10000, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = sort(zero)), col="blue") +
  geom_line(mapping = aes(x = id, y = sort(one)), col="red") +
  geom_line(mapping = aes(x = id, y = sort(two)), col="green")

pl

# ok


## distribuzione normale
r <- rshift("norm")
zero <- r(n = 10000, mean = 2, sd = 1, shift = 0)
one <- r(n = 10000, mean = 2, sd = 1, shift = 1)
two <- r(n = 10000, mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = 1:10000, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

set.seed(1)
id =1:1000
x <- rnorm(n = 1000, mean = 2, sd = 0.1)
x <- dnorm(x)
plot(pippo, type="l")
plot(dnorm(sort(x)))

plot(dnorm(std), id, type="l")
curve(dnorm(x))

zero <- r(n = 100, mean = 2, sd = 1, shift = 0)


plot(dnorm(sort(zero)), type="l")
plot(dnorm(sort(std)), type="l")

# ok


## distribuzione gamma
r <- rshift("gamma")
zero <- r(n = 1000, shape = 2, rate = 3, shift = 0)
one <- r(n = 1000, shape = 2, rate = 3, shift = 1)
two <- r(n = 1000, shape = 2, rate = 3, shift = 2)

### prova grafica
df <- data.frame(id = 1:1000, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok?


## distribuzione weibull
r <- rshift("weibull")
zero <- r(n = 1000, shape = 1.4, scale = 3, shift = 0)
one <- r(n = 1000, shape = 1.4, scale = 3, shift = 1)
two <- r(n = 1000, shape = 1.4, scale = 3, shift = 2)

### prova grafica
df <- data.frame(id = 1:1000, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = sort(zero), y = id), col="blue") +
  geom_line(mapping = aes(x = sort(one), y = id), col="red") +
  geom_line(mapping = aes(x = sort(two), y = id), col="green")

pl

# ok


## distribuzione t di Student
r <- rshift("t")
zero <- r(n = 1000, df = 10, shift = 0)
one <- r(n = 1000, df = 10, shift = 1)
two <- r(n = 1000, df = 10, shift = 2)

### prova grafica
df <- data.frame(id = 1:1000, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok


## distribuzione Chi Quadro
r <- rshift("chisq")
zero <- r(n = 1000, df = 2, shift = 0)
one <- r(n = 1000, df = 2, shift = 1)
two <- r(n = 1000, df = 2, shift = 2)

### prova grafica
df <- data.frame(id = 1:1000, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = zero, y = id), col="blue") +
  geom_line(mapping = aes(x = one, y = id), col="red") +
  geom_line(mapping = aes(x = two, y = id), col="green")

pl

# ok

##############################################################################################






