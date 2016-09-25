# =============================================================================
# File Name:     newtraph.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-09-24
# Last Modified: 2016-09-24 18:49:37
# =============================================================================


"newtraph" <- 
function(ders, dat, x0)
{
	cat("While N-R may be used for either minimization or\nmaximization")
	cat("The checks for progress in this function are written")
	cat("for maximization.  If you want to minimize, chage your")
	cat("derivative calculations (multiply by -1).")
  crit1<-1e-10
  crit2<-1e-06
  crit3<-1e-06
  c1<-0
  c2<-0
  c3<-0
  curnt <- x0
  nump<-length(x0)
	k <- 0
	repeat {
		k <- k + 1
    cat(" ",fill=T)
    cat(" ",fill=T)
    cat("Current estimates beginning iteration ", k, ":", fill = T)
		cat(curnt, fill = T)
    cat(" ",fill=T)
		int <- ders(curnt, dat)
    logL<-int[[1]]
    gi<-int[[2]]
		cat("Log likelihood for these estimates: ", fill = T)
		cat(logL, fill = T)
    cat("Gradient for these estimates: ",fill=T)
    cat(gi,fill=T)
		cat(" ", fill = T)
		Gi <- int[[3]]
		GiI <- solve(Gi)
		step <- GiI %*% gi
		new <- curnt - step
		sc <- 1
		repeat {
      sc <- sc + 1
			check <- ders(new, dat)
			if(check[[1]] < logL) {new <- curnt - (1/sc) * step}
			if(check[[1]] >= logL) {
        newL<-check[[1]]
        newg<-check[[2]]
      }
      if(check[[1]] >= logL) break
			if(sc == 10) {
        cat("Step halving not effective, try new starting values", fill = T)
        stop()
      }                        
    }
    dist1<-abs(newL-logL)
		dist2 <- (sum((new - curnt)^2))^0.5
		if(crit1 > dist1) {
      c1<-1
      cat("Convergence criterion of ",crit1,"met for change in log likelihood",fill=T)
    }
		if(crit2 > dist2) {
      c2<-1
      cat("Convergence criterion of ",crit2," met for change in estimates",fill=T)
    }
    if(sum(crit3 > abs(newg)) == nump) { 
      c3<-1
      cat("Convergence criterion of ",crit3," met for sum of derivatives",fill=T)
    }
    if(c1+c2+c3==3)	break
		curnt <- new
	}
	cat("", fill = T)
  cat(" ",fill=T)
	final <- ders(new, dat)
  flogL<-final[[1]]
  fgrad<-final[[2]]
  fInf<--1*solve(final[[3]])
	cat("Final Estimates Are: ", new, fill = T)
	cat("", fill = T)
	cat("Final Log Likelihood: ", flogL, fill = T)
	cat("", fill = T)
	cat("Value of Gradient at Convergence:", fill = T)
	cat(fgrad, fill = T)
	cat("", fill = T)
	cat("Inverse Observed Information: ", fill = T)
	cat("(i.e., Inverse of Negative Hessian)", fill = T)
	cat("", fill = T)
	print(fInf)
	res<-list(new,flogL,fInf)
  return(res)
}


gprime <- function(x, par){ (x^(par-1)) * (log(x)) * exp(-x) }

g2prime <- function(x, par){ (x^(par-1)) * (log(x)^2) * exp(-x) }

get_dergam <- function(a) {
  ga <- gamma(a)
  gpa <- integrate(gprime, 0, Inf, par = a)$value
  g2pa <- integrate(g2prime, 0, Inf, par = a)$value
  res <- c(ga, gpa, g2pa)
  return(res)
}


derloglike <- function(theta, y) {
  a <- theta[1]
  b <- theta[2]
  n <- length(y)
  s <- sum(y)
  logs <- sum(log(y))
  dergam <- get_dergam(a)
  gradient1 <- n * log(b) - (n * dergam[2] / dergam[1]) + logs
  gradient2 <- n * a / b - s
  hessianDiag1 <- -n * (dergam[1] * dergam[3] - dergam[2]^2) / dergam[1]^2
  hessianDiag2 <- -n * a / b^2
  hessianOffDiag <- n / b
  loglikelihood <- n * a * log(b) - n * log(dergam[1]) - b * s + (a - 1) * logs
  res <- list(loglikelihood = loglikelihood,
              gradient = c(gradient1, gradient2),
              hessian = matrix(c(hessianDiag1, hessianOffDiag,
                                 hessianOffDiag, hessianDiag2), nrow=2, byrow=TRUE))
  return(res)
}
