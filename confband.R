confband <- function(my.lm, alpha=0.05){
  
  the.data <- my.lm$model
  
  the.names <- as.character(my.lm$call)
  xy.names <- strsplit(the.names[2]," ~ ")[[1]]
  x.name <- xy.names[2]
  y.name <- xy.names[1]

  n <- length(my.lm$res)

  X <- the.data[,x.name]
  Y <- the.data[,y.name]
  
  xlo <- min(X)-1
  xhi <- max(X)+1
  x.h <- seq(xlo, xhi, length.out=100)
  
  new.lm <- lm(Y ~ X)
  
  yhat.h <- predict(new.lm, data.frame(X=x.h))
  
  xbar <- mean(X)
  
  MSE <- sum( (new.lm$res)^2 )/(n-2)
  
  s2.yhat.h <- MSE*(1/n+(x.h-xbar)^2/sum( (X - xbar)^2 ))
  s.yhat.h <- sqrt(s2.yhat.h)
  
  w2 <- 2*qf(1-alpha, 2, n-2)
  w <- sqrt(w2)
  
  upper <- yhat.h + w*s.yhat.h
  lower <- yhat.h - w*s.yhat.h
  
  lines(x.h, upper)
  lines(x.h, lower)
  
}