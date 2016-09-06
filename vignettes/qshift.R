## ---- echo=TRUE, message=FALSE-------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 7)

## ----dnorm---------------------------------------------------------------
dnorm(x = 6:10, mean = 8, sd = 1)

## ----sdnorm_definition---------------------------------------------------
shift_dnorm <- function(x, mean = 0, sd = 1, shift = 0, ...){
            x_shifted <- x - shift
            density <- dnorm(x = x_shifted, mean = mean, sd = sd, ...)
            return(density)
}

## ----sdnorm_example_1----------------------------------------------------
shift_dnorm(x = 6:10, mean = 8, sd = 1, shift = 1)

## ----qshift, message=FALSE-----------------------------------------------
require(qshift)
sdnorm <- dshift("norm") # density function
spnorm <- pshift("norm") # probability function
sqnorm <- qshift("norm") # quantile function
srnorm <- rshift("norm") # random generation function

## ----args----------------------------------------------------------------
args(sdnorm)
args(dnorm)

## ----sdnorm_example------------------------------------------------------
x <- seq(4, 14, len = 100)
not_shifted <- sdnorm(x = x, mean = 8, sd = 1.5)
positive_shifted <- sdnorm(x = x, mean = 8, sd = 1.5, shift = 1)
negative_shifted <- sdnorm(x = x, mean = 8, sd = 1.5, shift = -1)

## ----sdnorm_plot, message=FALSE------------------------------------------
require(ggplot2)
df <- data.frame(x=x, 
             sdnorm_type = factor(c(rep("not_shifted", times=length(x)),
                rep("positive_shifted", times=length(x)),
                rep("negative_shifted", times=length(x))),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))

pl <- ggplot(data = df, mapping = aes(x=x, y=value, col=sdnorm_type)) +
  geom_line(size=1) +
  scale_color_manual(values = c("not_shifted" = "cyan3", 
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  xlab("Value") + ylab("Density") +
  ggtitle("Density plot for shifted and not-shifted Normal distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)

## ----spnorm_example------------------------------------------------------
not_shifted <- spnorm(q = x, mean = 8, sd = 1.5)
positive_shifted <- spnorm(q = x, mean = 8, sd = 1.5, shift = 1)
negative_shifted <- spnorm(q = x, mean = 8, sd = 1.5, shift = -1)

## ----spnorm_plot---------------------------------------------------------
df <- data.frame(x=x, 
             spnorm_type = factor(c(rep("not_shifted", times=length(x)),
                rep("positive_shifted", times=length(x)),
                rep("negative_shifted", times=length(x))),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))

pl <- ggplot(data = df, mapping = aes(x=x, y=value, col=spnorm_type)) +
  geom_line(size=1) +
  scale_color_manual(values = c("not_shifted" = "cyan3", 
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  xlab("Quantile") + ylab("Probability") +
  ggtitle("Probability plot for shifted and not-shifted Normal distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)

## ----sqnorm_example------------------------------------------------------
x <- (1:999)/1000
not_shifted <- sqnorm(p = x, mean = 8, sd = 1.5)
positive_shifted <- sqnorm(p = x, mean = 8, sd = 1.5, shift = 1)
negative_shifted <- sqnorm(p = x, mean = 8, sd = 1.5, shift = -1)

## ----sqnorm_plot---------------------------------------------------------
df <- data.frame(x=x, 
             sqnorm_type = factor(c(rep("not_shifted", times=length(x)),
                rep("positive_shifted", times=length(x)),
                rep("negative_shifted", times=length(x))),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))

pl <- ggplot(data = df, mapping = aes(x=x, y=value, col=sqnorm_type)) +
  geom_line(size=1) +
  scale_color_manual(values = c("not_shifted" = "cyan3", 
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  ylab("Quantile") + xlab("Probability") +
  ggtitle("Quantile plot for shifted and not-shifted Normal distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)

## ----srnorm_example------------------------------------------------------
x <- 100000
not_shifted <- srnorm(n = x, mean = 8, sd = 1.5, set_seed = T)
positive_shifted <- srnorm(n = x, mean = 8, sd = 1.5, shift = 3, set_seed = T)
negative_shifted <- srnorm(n = x, mean = 8, sd = 1.5, shift = -3, set_seed = T)

## ----srnorm_plot---------------------------------------------------------
df <- data.frame(srnorm_type = factor(c(rep("not_shifted", times=x),
                rep("positive_shifted", times=x),
                rep("negative_shifted", times=x)),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))

pl <- pl <- ggplot(data = df, mapping = aes(x=value, group=srnorm_type, fill=srnorm_type)) +
  geom_density(alpha=0.6) +
scale_fill_manual(values = c("not_shifted" = "cyan3", 
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  ggtitle("Density plot for shifted and not-shifted Normal distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)

## ----qshift_unif---------------------------------------------------------
sdunif <- dshift("unif")
spunif <- pshift("unif")
squnif <- qshift("unif")
srunif <- rshift("unif")

## ----sdunif_example------------------------------------------------------
x <- seq(4, 14, len = 100)
not_shifted <- sdunif(x = x, min = 6, max = 12)
positive_shifted <- sdunif(x = x, min = 6, max = 12, shift = 1)
negative_shifted <- sdunif(x = x, min = 6, max = 12, shift = -1)

## ----sdunif_plot---------------------------------------------------------
df <- data.frame(x=x, 
             sdunif_type = factor(c(rep("not_shifted", times=length(x)),
                rep("positive_shifted", times=length(x)),
                rep("negative_shifted", times=length(x))),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))

pl <- ggplot(data = df, mapping = aes(x=x, y=value, col=sdunif_type)) +
  geom_line(size=1, alpha =0.8) +
  scale_color_manual(values = c("not_shifted" = "cyan3", 
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  xlab("Value") + ylab("Density") +
  ggtitle("Density plot for shifted and not-shifted Uniform distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)

## ----spunif_example------------------------------------------------------
not_shifted <- spunif(q = x, min = 6, max = 12)
positive_shifted <- spunif(q = x, min = 6, max = 12, shift = 1)
negative_shifted <- spunif(q = x, min = 6, max = 12, shift = -1)

## ----spunif_plot---------------------------------------------------------
df <- data.frame(x=x, 
             spunif_type = factor(c(rep("not_shifted", times=length(x)),
                rep("positive_shifted", times=length(x)),
                rep("negative_shifted", times=length(x))),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))

pl <- ggplot(data = df, mapping = aes(x=x, y=value, col=spunif_type)) +
  geom_line(size=1, alpha =0.8) +
  scale_color_manual(values = c("not_shifted" = "cyan3", 
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  xlab("Quantile") + ylab("Probability") +
  ggtitle("Probability plot for shifted and not-shifted Uniform distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)

## ----squnif_example------------------------------------------------------
x <- (1:999)/1000
not_shifted <- squnif(p = x, min = 6, max = 12)
positive_shifted <- squnif(p = x, min = 6, max = 12, shift = 1)
negative_shifted <- squnif(p = x, min = 6, max = 12, shift = -1)

## ----squnif_plot---------------------------------------------------------
df <- data.frame(x=x, 
             squnif_type = factor(c(rep("not_shifted", times=length(x)),
                rep("positive_shifted", times=length(x)),
                rep("negative_shifted", times=length(x))),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))

pl <- ggplot(data = df, mapping = aes(x=x, y=value, col=squnif_type)) +
  geom_line(size=1, alpha =0.8) +
  scale_color_manual(values = c("not_shifted" = "cyan3",  
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  ylab("Quantile") + xlab("Probability") +
  ggtitle("Quantile plot for shifted and not-shifted Uniform distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)

## ----srunif_example------------------------------------------------------
x <- 10000
not_shifted <- srunif(n = x, min = 6, max = 12, set_seed = T)
positive_shifted <- srunif(n = x, min = 6, max = 12, shift = 3, set_seed = T)
negative_shifted <- srunif(n = x, min = 6, max = 12, shift = -3, set_seed = T)

## ----srunif_plot---------------------------------------------------------
df <- data.frame(srunif_type = factor(c(rep("not_shifted", times=x),
                rep("positive_shifted", times=x),
                rep("negative_shifted", times=x)),
                levels = c("not_shifted", "positive_shifted", "negative_shifted")), 
              value = c(not_shifted, positive_shifted, negative_shifted))


pl <- pl <- ggplot(data = df, mapping = aes(x=value, group=srunif_type, fill=srunif_type)) +
  geom_density(alpha=0.6) +
scale_color_manual(values = c("not_shifted" = "cyan3", 
                            "positive_shifted" = "#89FF99", 
                            "negative_shifted" = "#FF89B4")) +
  xlab("Value") + ylab("Density") +
  ggtitle("Density plot for shifted and not-shifted Uniform distributions") +
  theme(legend.title=element_blank(), legend.text= element_text(size=10),
  plot.title = element_text(size=14), axis.title=element_text(size=12))

print(pl)


## ----sdweibull_def-------------------------------------------------------
sdweibull <- dshift("weibull")

## ----max_likelihood_est--------------------------------------------------
ltweibull <- function(x){
  # starting values for parameters (scale, shape) and shift
  shift <- min(x) - 0.01
  x1 <- x - shift
  shape <- (sd(x1)/mean(x1))^(-1.086)
  scale <- mean(x1)/gamma(1+1/shape)
  # parameters vector definition
  theta <- c(shape, scale, shift)
  # likelihood function
  ll <- function(theta, x){
    shape <- theta[1]
    scale <- theta[2]
    shift <- theta[3]
    ld <- sdweibull(x = x, shape = shape, scale = scale, shift = shift, log = TRUE)
    -sum(ld)
  } 
  # maximum likelihood estimation
  optim(par = theta, fn = ll, x = x)[["par"]]
}

## ----max_likelihood_est_example------------------------------------------
# generate a random sample from a shifted weibull distribution
srweibull <- rshift("weibull")
x <- srweibull(n = 100000, shape = 1, scale = 5, shift = 1)

# maximum likelihood estimate for the shifted weibull distribution
ltweibull(x = x)

