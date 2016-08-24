require(qshift)
require(ggplot2)

## uso la distribuzione normale ##

# dshift

sample <- seq(from = - 1, to = 8, length.out = 100)

dn <- dshift("norm")
zero <- dn(x = seq(from = - 1, to = 8, length.out = 100), mean = 2, sd = 1, shift = 0)
one <- dn(x = seq(from = - 1, to = 8, length.out = 100), mean = 2, sd = 1, shift = 1)
two <- dn(x = seq(from = - 1, to = 8, length.out = 100), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = sample, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# pshift

sample <- seq(from = - 1, to = 8, length.out = 100)

p <- pshift("norm")
zero <- p(q = seq(from = - 1, to = 8, length.out = 100), mean = 2, sd = 1, shift = 0)
one <- p(q = seq(from = - 1, to = 8, length.out = 100), mean = 2, sd = 1, shift = 1)
two <- p(q = seq(from = - 1, to = 8, length.out = 100), mean = 2, sd = 1, shift = 2)

### prova grafica
df <- data.frame(id = sample, zero, one, two, stringsAsFactors = F)

pl <- ggplot(data=df) +
  geom_line(mapping = aes(x = id, y = zero), col="blue") +
  geom_line(mapping = aes(x = id, y = one), col="red") +
  geom_line(mapping = aes(x = id, y = two), col="green")

pl

# qshift

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

# rshift
set.seed(1)
prova <- rnorm(n = 1000)
df <- data.frame(prova=prova, stringsAsFactors = F)

pl <- ggplot(data = df, mapping = aes(prova)) +
  geom_histogram()

r <- rshift("norm")
set.seed(1)
pippo <- r(n = 1000, mean = 0, sd = 1)
pluto <- r(n = 1000, mean = 0, sd = 1, shift = 1)

df <- data.frame(pippo=pippo, pluto=pluto, stringsAsFactors = F)

pl <- ggplot(data = df) +
  geom_histogram(mapping = aes(pippo), fill="blue") +
  geom_histogram(mapping = aes(pluto), fill="red", alpha=0.3)

