require(qshift)

sdnorm <- dshift("norm")
sdnorm(x = 1:10, mean = 2, sd = 3, shift = 4, log = T)
sdnorm(x = 1:10, mean = 2, sd = 3, log = T)
dnorm(x = 1:10, mean = 2, sd = 3, log = T)


spnorm <- pshift("norm")

spnorm(q = 1:10, mean = 2, sd = 3)
pnorm(q = 1:10, mean = 2, sd = 3)

spnorm(q = 1:10, mean = 2, sd = 3, shift=1)

spnorm(q = 1:10, mean = 2, sd = 3, log.p = T, shift=1)
pnorm(q = 1:10, mean = 2, sd = 3, log.p = T)

spnorm(q = 1:10, mean = 2, sd = 3, lower.tail = F, shift=1)
pnorm(q = 1:10, mean = 2, sd = 3, lower.tail = F)


pnorm(q = 1:10, mean = 2, sd = 3, log.p = T, lower.tail = F)
spnorm(q = 1:10, mean = 2, sd = 3, log.p = T, lower.tail = F, shift = 1)
spnorm(q = 1:10, mean = 2, sd = 3, log.p = T, lower.tail = F, shift = 1)
spnorm(q = 1:10, mean = 2, sd = 3, log.p = F, lower.tail = F, shift = 1)

qsnorm <- qshift("norm")
a <- qsnorm(p = log(1:9/10), mean = 2, sd = 3, shift = 4, log = T, lower.tail = F)
spnorm(q = a, mean = 2, sd = 3, shift=4, log.p = T, lower.tail = F)

a <- qsnorm(p = 1:9/10, mean = 2, sd = 3, shift = 4, log = F, lower.tail = F)
spnorm(q = a, mean = 2, sd = 3, shift=4, log.p = F, lower.tail = F)



qsnorm(p = 1:9/10, mean = 2, sd = 3, log = F, lower.tail = F)
qnorm(p = 1:9/10, mean = 2, sd = 3, log = F, lower.tail = F)

qsnorm(p = 1:9/10, mean = 2, sd = 3, shift = 5, log = F, lower.tail = F)




