
# 整理版-完整REMSVM-调用

remsvm <- function(x, y, a, gamma, lambda, kernel.type, kernel.par= 1,
                   criterion = "0-1", x.test = NULL, y.test = NULL, 
                   cv = FALSE, fold = 5, epsilon.C = 1e-5, epsilon.H = 1e-5)
{
  if((criterion != "0-1") && (criterion != "hinge"))
  {
    cat("ERROR: Only 0-1 and hinge can be used as criterion!", "\n")
    return(NULL)
  }
  len.lambda <- length(lambda) 
  
  ERR <- matrix(0, len.lambda, 1) 
  HIN <- matrix(0, len.lambda, 1) 
  
  kernel <- list(type=kernel.type, par=kernel.par) 
  x <- as.matrix(x)
  K <- eval.kernel(x, x, kernel)
  
  if (cv & !is.null(y.test)) 
  { 
    cat('When cv=TRUE, the test data are not used in cross-validation.','\n')  
  } 
  
  if (cv)  # cross-validation
  { 
    ran <- data.split(y, fold)
    for(i.cv in 1:fold )
    { 
      cat("Leaving subset[", i.cv,"] out in",fold,"fold CV:","\n")
      omit <- (ran == i.cv)
      x.train <- x[!omit,] 
      y.train <- y[!omit] 
      
      x.test <- x[omit,] 
      y.test <- y[omit] 
      
      row.index <- 0 
      
      subK <- eval.kernel(x.train, x.train, kernel) 
      subK.test <- eval.kernel(x.test, x.train, kernel) 
      
      cat("lambda of length",len.lambda,"|")
      
      for(i in lambda) 
      { 
        row.index <- row.index + 1          
        exp2.lambda <- 2^i
        model <- remsvm.compact(subK, y.train,a, gamma,  exp2.lambda, epsilon.C, epsilon.H) 
        fit.test <- predict.remsvm.compact(subK.test, model) 
        ERR[row.index] <- (ERR[row.index]+error.rate(y.test, fit.test)/fold) 
        HIN[row.index] <- (HIN[row.index]+rehinge(a, gamma, y.test, fit.test)/fold) 
        cat('*')
      } 
      cat('|\n')
    }
    cat("The minimum of average", fold, "fold cross-validated", criterion,
        "loss:","\n")
  }
  
  else if ( !cv & is.null(y.test) ) # in-sample evaluation
  {
    cat("lambda of length",len.lambda,"|")
    row.index <- 0 
    for(i in lambda) 
    { 
      row.index <- row.index + 1 
      exp2.lambda <- 2^i
      model <- remsvm.compact(K, y, a, gamma, exp2.lambda, epsilon.C, epsilon.H) 
      ERR[row.index] <- error.rate(y, model$fit) 
      HIN[row.index] <- rehinge(a, gamma, y, model$fit) 
      cat('*')
    } 
    cat('|\n')
    cat("The minimum of average in-sample", criterion,"loss:", "\n")
  }
  
  else # use the test data for tuning
  {
    x.test <- as.matrix(x.test)
    K.test <- eval.kernel(x.test, x, kernel) 
    
    cat("lambda of length",len.lambda,"|")
    row.index <- 0 
    for(i in lambda) 
    { 
      row.index <- row.index + 1 
      exp2.lambda <- 2^i
      model <- remsvm.compact(K, y,a, gamma,  exp2.lambda, epsilon.C, epsilon.H) 
      fit.test <- predict.remsvm.compact(K.test, model) 
      ERR[row.index] <- error.rate(y.test, fit.test) 
      HIN[row.index] <- rehinge(a, gamma, y.test, fit.test) 
      cat('*')
    } 
    cat('|\n')
    cat("The minimum of average", criterion,"loss over test cases:", "\n")
  }
  
  # choose the optimal index for lambda
  # if the optimal values are not unique, choose the largest value 
  # assuming that lambda is in increasing order.
  if(criterion == "0-1")
  {
    optIndex <- (len.lambda:1)[which.min(ERR[len.lambda:1])]
    cat(min(ERR),"\n")
  }
  else if(criterion == "hinge")
  {
    optIndex <- (len.lambda:1)[which.min(HIN[len.lambda:1])]
    cat(min(HIN),"\n")
    
  }
  # choose the best model
  opt.lambda <- lambda[optIndex]
  cat("The optimal lambda on log2 scale:", opt.lambda,"\n")
  
  opt.model <- remsvm.compact(K, y, a, gamma, 2^opt.lambda, epsilon.C, epsilon.H)
  list(opt.lambda = opt.lambda, error = ERR, hinge = HIN, model = opt.model) 
}


###############################################
predict.remsvm <-function(x, x.new, kernel, model) 
{
  x <- as.matrix(x)
  
  if (!is.matrix(x.new)) # degenerate case: x.new is a row vector
  { x.new <- t(as.matrix(x.new)) }
  else { x.new <- as.matrix(x.new) }
  
  vmat <- as.matrix(model$vmat)
  bvec <- as.matrix(model$bvec)
  K <- eval.kernel(x.new, x, kernel)
  
  fit <- (matrix(rep(bvec, nrow(x.new)), ncol=ncol(vmat), byrow=T) + (K %*% vmat))
  return(fit)
}

###############################################
predict.remsvm.compact <- function(K, model) 
{
  vmat <- as.matrix(model$vmat)
  bvec <- as.matrix(model$bvec)
  n.data <- nrow(K)
  n.class <- ncol(vmat)
  fit <- (matrix(rep(bvec, n.data), ncol=n.class, byrow=T) + (K %*% vmat))
  return(fit)
}

#########################################################
remsvm.plot <- function(x, y, kernel, model)
{
  n.grid <- 51
  
  r1 <- range(x[,1])
  r2 <- range(x[,2])
  u <- seq(r1[1]-.05*(r1[2]-r1[1]),r1[2]+.05*(r1[2]-r1[1]), length = n.grid)
  v <- seq(r2[1]-.05*(r2[2]-r2[1]),r2[2]+.05*(r2[2]-r2[1]), length = n.grid)
  x.new <- cbind(rep(u, rep(n.grid, n.grid)), rep(v, n.grid))
  
  fit.new <- predict.remsvm(x, x.new, kernel, model) 
  fit.new.class <- apply(fit.new,1,which.max)
  
  plot(x, type="n", xlab=expression(x[1]), ylab=expression(x[2]),frame.plot=T)
  for (j in 1:max(y))
  { points(x[(y == j),], pch=1, col=j+1)}
  
  for (j in 1:max(y))
  { points(x.new[(fit.new.class == j),], pch='.', col=j+1) }
  
  # delineate the boundary
  for (j in 1:max(y))
  {
    fjcontrast <- fit.new[,j] - apply(as.matrix(fit.new[,-j]),1, max)
    fjcontrast <- t(matrix(fjcontrast, n.grid, n.grid))
    contour(u, v, fjcontrast, add=T, levels=0, labex=0, lty=1, 
            drawlabels=FALSE)
  }
  
}

#############################

error.rate <- function(y, fit)
{
  class.pred <- apply(fit,1,which.max)
  return(sum(class.pred != y)/length(class.pred))
}


######################################################################################

# This hinge function is not complete. Need modification if needed.
#####################
rehinge <- function(a, gamma, y, fit)
{ 
  hin=-1000
  return(hin) 
} 

data.split <- function(y, fold, k = max(y), seed = length(y))
{
  # k: the number of classes
  n.data <- length(y)
  class.size <- table(y)
  ran <- rep(0, n.data) 
  if ( (min(class.size) < fold) & (fold != n.data) )
  {
    warning(' The given fold is bigger than the smallest class size. \n Only a fold size smaller than the minimum class size \n or the same as the sample size (LOOCV) is supported.\n')
    return(NULL)
  }
  
  if ( min(class.size)>= fold )
  {
    set.seed(seed) 
    for (j in 1:k)
    {  
      ran[y==j] <- ceiling(sample(class.size[j])/(class.size[j]+1)*fold) 
    }
  }
  else if ( fold == n.data)
  {
    ran <- 1:n.data
  }
  return(ran)
}


###############################################################################
# kernal evaluation: main.kernel , eval.kernel  (from Lee et al.)
#

#kernel evaluation function

main.kernel <- function(x, u, kernel)
{
  x <- as.matrix(x)
  u <- as.matrix(u)
  if (kernel$type == "linear")
    K <- (x %*% t(u))
  if (kernel$type == "poly")
    K <- (1 + x %*% t(u))^kernel$par
  if (kernel$type == "rbf")
  {
    a <- as.matrix(apply(x^2, 1, 'sum'))
    b <- as.matrix(apply(u^2, 1, 'sum'))
    one.a <- matrix(1, ncol=length(b))   
    one.b <- matrix(1, ncol=length(a))
    K1 <- one.a %x% a
    K2 <- x %*% t(u)
    K3 <- t(one.b %x% b)
    K <- exp(-(K1 - 2 * K2 + K3)/(2 * kernel$par^2))
  }
  return(K)
}

eval.kernel <- function(x, u=x, kernel)
{
  # x: m by d data matrix 
  # u: n by d data matrix 
  # compute m by n kernel matrix
  
  if (!is.matrix(x))  # degenerate case: x is a row vector  
  { x <- t(as.matrix(x))}
  else { x <- as.matrix(x)}
  
  u <- as.matrix(u)
  
  if ( any(kernel$type == c('linear','poly','rbf')) )
  {
    K <- main.kernel(x,u, kernel)
  }
  else if (any(kernel$type == c('spline','spline-t')))
  {
    dimx <- ncol(x)
    K <- 0
    for (d in 1:dimx)
    {
      K.temp <- spline.kernel(as.matrix(x[,d]), as.matrix(u[,d]))
      K <- K +  K.temp$K1 + K.temp$K2
    }
  }
  else if (any(kernel$type == c('spline2','spline-t2')))
  { 
    dimx <- ncol(x)
    K <- 0
    anova.kernel <- vector(mode="list", dimx)
    
    # main effects
    for(d in 1:dimx)
    {
      K.temp <- spline.kernel(as.matrix(x[,d]), as.matrix(u[,d]))
      anova.kernel[[d]] <- K.temp$K1 + K.temp$K2
      K <- K +  anova.kernel[[d]]
    }  
    # two-way interactions
    for (i in 1:(dimx-1))
    {
      for (j in (i+1):dimx)
      {
        K <- K + anova.kernel[[i]]*anova.kernel[[j]]
      }
    }
  }
  else {cat('The specified kernel type is not supported.' ,'\n')}
  
  return(K)
}

spline.kernel <- function(x, u)
{
  x <- as.matrix(x)
  u <- as.matrix(u)
  K1x <- (x - 1/2)
  K1u <- (u - 1/2)
  K2x <- (K1x^2 - 1/12)/2
  K2u <- (K1u^2 - 1/12)/2
  ax <- x%x%matrix(1, 1, nrow(u)) 
  au <- u%x%matrix(1, 1, nrow(x))
  b <- abs(ax - t(au))
  K1 <- K1x%x%t(K1u)
  K2 <- K2x%x%t(K2u) - ((b - 1/2)^4 - (b - 1/2)^2/2 + 7/240)/24
  list(K1 = K1, K2 = K2)
}



###################A toy example generation

twod.data <- function(n.data)
{
  x <- runif(n.data)
  p1 <- (0.97 * exp(-3*x))
  p3 <- exp(-2.5 * (x-1.2)^2)
  p2 <- (1- p1 - p3)
  
  # class.prob is a matrix of probabilities.
  # each column contains the probability that y is in each class for fixed x
  class.prob <- cbind(p1, p2, p3)
  
  # generate x and y
  x <- as.matrix(x)
  u <- runif(n.data)
  y <- rep(0, n.data)
  
  y[u <= p1] <- 1
  y[u > p1 & u < (p1 + p2)] <- 2
  y[u >= (p1 + p2)] <- 3
  
  x2 <- runif(n.data)
  x <- cbind(x, x2)
  
  return(list(x=x,y=y,p=class.prob))
}

