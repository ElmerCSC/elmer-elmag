rm(list = ls())

elmer_res <- read.table("INSERT PATH TO data.txt")

# V1: energy functional; V2: energy functional im

nstp <- 151; fmin <- 35.0e9; fmax <- 38.0e9
f <- seq(from=fmin, to=fmax, length.out=nstp)

a <- 0.28*25.4e-3; b <- a/2; omega <- 2*pi*f;
mu0 <- 4*pi*10^-7; eps0 <- 8.854e-12;
c0 <- 1/sqrt(mu0*eps0); k0 <- omega/c0; kc <- pi/a; beta <- sqrt(k0^2-kc^2);
sigma <- (beta*k0^2*a*b)/(mu0*kc^2)

re.rho <- -(elmer_res$V2/sigma-1)
im.rho <- elmer_res$V1/sigma
rc <- 20*log10(sqrt(re.rho^2+im.rho^2))
#tc <- 20*log10(1-(sqrt(re.rho^2+im.rho^2)))

plot(0, type="n", xlim=c(34e9,38e9), ylim=c(-60,0), axes=F, xlab="Frequency in Hz", ylab="S11 in dB")
abline(h=seq(-70, 0, 10), col="lightgrey")
abline(v=seq(34e9, 38e9, 5e8), col="lightgrey")
points(f, rc, type="l", lwd=2, col="darkorange", lty=1)
axis(side=1); axis(side=2)