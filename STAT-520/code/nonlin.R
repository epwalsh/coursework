"gauss.newton"<-
  function(xmat, y, ps, fctn, ders, wts)
  {
    # fctn is a function that computes the current 
    # expectations, ders is a function that computes
    # the current matrix of derivatives of the 
    # expectation function (nXp), wts is a function
    # that computes weights as 1/g^2 where g is
    # the variance model
    # xmat are covariates that can be in any
    # form (vector, matrix, list, etc.) that is
    # expected by fctn, ders, and wts
    cps <- ps
    cnt <- 0
    crit <- 1e-08
    repeat {
      cnt <- cnt + 1
      cps1 <- cps
      ee <- fctn(xmat, cps)
      V <- ders(xmat, cps)
      W <- wts(xmat, cps)
      d <- solve(t(V) %*% W %*% V)
      d <- d %*% t(V) %*% W %*% as.matrix(y - ee)
      cps <- cps1 + d
      cat("New Estimates at Iteration",cnt,":", fill = T)
      cat(cps, fill = T)
      dist <- (sum((cps - cps1)^2))^0.5
      if(dist < crit) break
    }
    cat("Convergence Criterion of ", crit, " met", fill = T)
    cat("Final Estimates: ", cps, fill = T)
    cps
  }

"nonlin"<-
  function(xmat, ys, ps, fctn, ders, wts)
  {
    # external function definitions for fctn,
    # ders and wts are as in gauss.newton
    # output is list containing betahat(bs),
    # sigma squared hat (sshat), the covariance
    # matrix for betahat (cov), fitted values
    # (yh), studentized residuals (bi), 
    # absolute studentized residuals to
    # (2/3) power (abi),
    # and the matrix of derivatives of the
    # expectation function (fb)
    N <- length(ys)
    P <- length(ps)
    bs <- gauss.newton(xmat, ys, ps, fctn, ders, wts)
    yh <- fctn(xmat, bs)
    r <- ys - yh
    w <- wts(xmat, bs)
    g2 <- 1/(diag(w))
    ri <- r/(g2^0.5)
    fb <- ders(xmat, bs)
    G <- matrix(sqrt(g2), N, P, byrow = F)
    xx <- fb/G
    H <- xx %*% (solve(t(xx) %*% xx)) %*% t(xx)
    h <- diag(H)
    sshat <- (1/(N - P)) * sum(ri^2)
    cov <- matrix(0, P, P)
    cnt <- 0
    repeat {
      cnt <- cnt + 1
      tfb <- as.matrix(fb[cnt,  ])
      tfbfb <- tfb %*% t(tfb)
      tel <- tfbfb/g2[cnt]
      cov <- cov + tel
      if(cnt == N)
        break
    }
    cov <- sshat * solve(cov)
    bi <- ri/((sshat * (1 - h))^0.5)
    abi <- (abs(bi))^(2/3)
    result <- list(bs=bs, sshat=sshat, covb=cov, yhat=yh, stdres=bi, absres=abi, fb=fb)
    result
  }

gompfctn<-function(xs,ps){
  #Gompertz response curve
  #ps is (b1,b2,b3)
  b1<-ps[1]; b2<-ps[2]; b3<-ps[3]
  fs<-b1*exp(-exp(b2-b3*xs))
  return(fs)
}

gompVmat<-function(xs,ps){
  #compute matrix of derivatives for Gompertz model for use with nonlin
  #
  n<-length(xs)
  b1<-ps[1]; b2<-ps[2]; b3<-ps[3]
  t1<-exp(b2-b3*xs)
  t2<-exp((-1)*t1)
  db1<-t2
  db2<-(-1)*b1*t2*t1
  db3<-b1*t2*t1*xs
  V<-matrix(c(db1,db2,db3),n,3,byrow=F)
  return(V)
}

gompwts<-function(xs,ps){
  #weights for gompertz model with power of the mean variances
  #power (thet) must be changed within this function
  #
  thet<-0.50
  mus<-gompfctn(xs,ps)
  ws<-1/(mus^(2*thet))
  W<-diag(ws)
  return(W)
}
