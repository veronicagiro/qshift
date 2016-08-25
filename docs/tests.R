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


## distribuzione binomiale negativa
d <- dshift("nbinom")
zero <- d(x = c(2,3,4,5), size = 23, mu = 4, shift = 0)
one <- d(x = c(2,3,4,5), size = 23, mu = 4, shift = 1)
two <- d(x = c(2,3,4,5), size = 23, mu = 4, shift = 2)

### prova grafica
df <- data.frame(id = 2:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok


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


## distribuzione geometrica
d <- dshift("geom")
zero <- d(x = c(2,3,4,5), prob = 0.4, shift = 0)
one <- d(x = c(2,3,4,5), prob = 0.4, shift = 1)
two <- d(x = c(2,3,4,5), prob = 0.4, shift = 2)

### prova grafica
df <- data.frame(id = 2:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione ipergeometrica
d <- dshift("hyper")
zero <- d(x = c(2,3,4,5,6,7,8,9,10), m = 10, n = 7, k = 8, shift = 0)
one <- d(x = c(2,3,4,5,6,7,8,9,10), m = 10, n = 7, k = 8, shift = 1)
two <- d(x = c(2,3,4,5,6,7,8,9,10), m = 10, n = 7, k = 8, shift = 2)

### prova grafica
df <- data.frame(id = 2:10, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione uniforme (qualche  dubbio)
d <- dshift("unif")
zero <- d(x = c(1,2,3,4,5), min = 0, max = 4, shift = 0)
one <- d(x = c(1,2,3,4,5), min = 0, max = 4, shift = 1)
two <- d(x = c(1,2,3,4,5), min = 0, max = 4, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok


## distribuzione normale
d <- dshift("norm")
zero <- d(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 0)
one <- d(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 1)
two <- d(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

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


## distribuzione beta
d <- dshift("beta")
zero <- d(x = c(0,1,2,3,4,5), shape1 = 1, shape2 = 1.5, shift = 0)
one <- d(x = c(0,1,2,3,4,5), shape1 = 1, shape2 = 1.5, shift = 1)
two <- d(x = c(0,1,2,3,4,5), shape1 = 1, shape2 = 1.5, shift = 2)

### prova grafica
df <- data.frame(id = 0:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione logis
d <- dshift("logis")
zero <- d(x = c(0,1,2,3,4,5), location = 5, scale = 2, shift = 0)
one <- d(x = c(0,1,2,3,4,5), location = 5, scale = 2, shift = 1)
two <- d(x = c(0,1,2,3,4,5), location = 5, scale = 2, shift = 2)

### prova grafica
df <- data.frame(id = 0:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione logis
d <- dshift("lnorm")
zero <- d(x = c(3,4,5, 6,7,8,9,10), meanlog = 0 ,sdlog = 0.5, shift = 0)
one <- d(x = c(3,4,5, 6,7,8,9,10), meanlog = 0 ,sdlog = 0.5, shift = 1)
two <- d(x = c(3,4,5, 6,7,8,9,10), meanlog = 0 ,sdlog = 0.5, shift = 2)

### prova grafica
df <- data.frame(id = 3:10, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione cauchy
d <- dshift("cauchy")
zero <- d(x = c(0,1,2,3,4,5), location = 0, scale = 1, shift = 0)
one <- d(x = c(0,1,2,3,4,5), location = 0, scale = 1, shift = 1)
two <- d(x = c(0,1,2,3,4,5), location = 0, scale = 1, shift = 2)

### prova grafica
df <- data.frame(id = 0:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione esponenziale
d <- dshift("exp")
zero <- d(x = c(0,1,2,3,4,5), rate = 2, shift = 0)
one <- d(x = c(0,1,2,3,4,5), rate = 2, shift = 1)
two <- d(x = c(0,1,2,3,4,5), rate = 2, shift = 2)

### prova grafica
df <- data.frame(id = 0:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione f
d <- dshift("f")
zero <- d(x = c(1,2,3,4,5), df1 = 3, df2 = 4, shift = 0)
one <- d(x = c(1,2,3,4,5), df1 = 3, df2 = 4, shift = 1)
two <- d(x = c(1,2,3,4,5), df1 = 3, df2 = 4, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

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


## distribuzione wilcox
d <- dshift("wilcox")
zero <- d(x = c(1,2,3,4,5), m = 5, n = 6, shift = 0)
one <- d(x = c(1,2,3,4,5), m = 5, n = 6, shift = 1)
two <- d(x = c(1,2,3,4,5), m = 5, n = 6, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok

## distribuzione signrank
d <- dshift("signrank")
zero <- d(x = c(1,2,3,4,5), n = 6, shift = 0)
one <- d(x = c(1,2,3,4,5), n = 6, shift = 1)
two <- d(x = c(1,2,3,4,5), n = 6, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok


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

## distribuzione binomiale negativa
p <- pshift("nbinom")
zero <- p(q = c(2,3,4,5), size = 23, mu = 4, shift = 0)
one <- p(q = c(2,3,4,5), size = 23, mu = 4, shift = 1)
two <- p(q = c(2,3,4,5), size = 23, mu = 4, shift = 2)

### prova grafica
df <- data.frame(id = 2:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl
# ok


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

# ok

qn <- qshift("norm")
qn(zero,  mean = 2, sd = 1, shift = 0)
qn(one,  mean = 2, sd = 1, shift = 0)
qn(two,  mean = 2, sd = 1, shift = 0)

dnorm(x = c(1,2,3,4,5), mean = 2, sd = 1)


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

## distribuzione wilcox
p <- pshift("wilcox")
zero <- p(q = c(1,2,3,4,5), m = 5, n = 6, shift = 0)
one <- p(q = c(1,2,3,4,5), m = 5, n = 6, shift = 1)
two <- p(q = c(1,2,3,4,5), m = 5, n = 6, shift = 2)

### prova grafica
df <- data.frame(id = 1:5, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# ok

## distribuzione signrank
p <- pshift("signrank")
zero <- p(q = c(1,2,3,4,5), n = 6, shift = 0)
one <- p(q = c(1,2,3,4,5), n = 6, shift = 1)
two <- p(q = c(1,2,3,4,5), n = 6, shift = 2)

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
qn <- qshift("norm")
zero <- qn(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 0)
one <- qn(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 1)
two <- qn(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), zero, one, two, stringsAsFactors = F)

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

## distribuzione normale
p <- pshift("norm")
zero <- p(q = c(-1,0,1,2,3,4,5), mean = 2, sd = 1, shift = 0)
one <- p(q = c(-1,0,1,2,3,4,5), mean = 2, sd = 1, shift = 1)
two <- p(q = c(-1,0,1,2,3,4,5), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = c(-1,0,1,2,3,4,5), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

## distribuzione normale
qn <- qshift("norm")
zero <- qn(p = c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 0)
one <- qn(p = c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 1)
two <- qn(p = c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl


