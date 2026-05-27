#####################################################################################
# code for reinforced MSVM
# by Yufeng Liu (started on July 2007; last modified on April 2010; 
#some functions are based on the SMSVM by Lee et al.
# at http://www.stat.osu.edu/~yklee/software.html
######################################################################################

library(quadprog)
library(lpSolve)

################################################################################
#reinforced MSVM: remsvm

# This function is to find the REMSVM solution. 

#Default: choose the lambda minimizing the in-sample training loss
#and get REMSVM solution at the value, or get REMSVM solution at a specific 
#value of lambda.

#If 'cv' is TRUE, it finds the lambda minimizing cross-validated loss
#using the training data only and gets REMSVM solution at the value.

#If 'cv' is FALSE and test data are given, then it finds the lambda
#minimizing the testing loss and gets REMSVM solution at the value.

#[Input]
#lambda can be a scalar or a vector.
#Note that lambda is on log2 scale.#################
#x: a numeric data matrix of covariates.
#y: a vector of scalar valued class labels {1,...,k}.

#[Output]
#opt.lambda: the value of optimal lambda minimizing 'criterion'.
#error: error rate with respect to '0-1' loss.
#rehinge: average risk with respect to 'rehinge' loss.
#model: fitted REMSVM for the optimal lambda.

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


#####################################################################################
#reinforced SVM function: remsvm.compact 

# A function to calculate SVM solutions given a value of lambda and parameters a and #gamma. 

#Its input is not x, a raw data matrix but K, a kernel matrix.

#[Input]
#lambda is a scalar on the original scale.
#y: a vector of scalar valued class labels {1,...,k}.

#epsilon.C: parameter to avoid redundant constraints when gamma=0 and 1. If error message
# occurs for a problem using these gamma values, try a bigger value of epsilon.C such as
# 1e-4 or 1e-3.

#epsilon.H: parameter to avoid singularity of the H matrix in QP.

remsvm.compact <-  function(K, y, a, gamma, lambda, epsilon.C = 1e-5, epsilon.H = 1e-5)
{
   # the sample size, the number of classes and dimension of QP problem
   n.class <- max(y) 
   n.data <- length(y) 
   qp.dim <- (n.data * n.class)
  
   # optimize alpha by solve.QP: min(-d^T b + 1/2 b^T D b)
   #                             subject to A^T b ><- b_0
   # following steps (1)-(6)
   # (1) calculate Dmat
   # average matrix
   In <- diag(1, n.data) 
   AH=matrix(0,n.data,qp.dim)
   for(m in 1:n.class)
   { lvecj<-rep(0,n.data)
     lvecj[y!=m]<-1
     evecj<-matrix(0,1,n.class)
     evecj[1,m]<-1
     vmatj<-diag(lvecj)
     umatj<-evecj%x%In
     AH=AH+((2*vmatj-In)%*%umatj/n.class)
    }
   # get D matrix
   Dmat<-matrix(0, qp.dim,qp.dim)
   for(m in 1:n.class)
   { lvecj<-rep(0,n.data)
     lvecj[y!=m]<-1
     evecj<-matrix(0,1,n.class)
     evecj[1,m]<-1
     vmatj<-diag(lvecj)
     umatj<-evecj%x%In
     Hmatj=(In-2*vmatj)%*%umatj+AH
     Dmat<-Dmat+t(Hmatj)%*%K%*%Hmatj
    }

   # add a small number for stability
   diag(Dmat) <- (diag(Dmat) + epsilon.H)

   # (2) compute -d (g)
   g <- rep(-(n.data*lambda),qp.dim)
   for(j in 1:n.class)
	{for(i in 1:n.data)
         {if(y[i]==j)g[(j-1)*n.data+i]<-(-a*n.data*lambda)}
       }
   dvec<-(-g)

   # (3) compute Amat 
   Amat<-matrix(0, (2*qp.dim+n.class), qp.dim)
   # the first k rows are equality constraint matrix
   for(m in 1:n.class)
   { lvecj<-rep(0,n.data)
     lvecj[y!=m]<-1
     vmatj<-diag(lvecj)
     evecj<-matrix(0,1,n.class)
     evecj[1,m]<-1
     umatj<-evecj%x%In
     Hmatj=(In-2*vmatj)%*%umatj+AH
     Amat[m,]<-rep(1,n.data)%*%Hmatj
    }
   # the next k+1 to qp.dim+k specifies beta>=0
   diag(Amat[(n.class+1):(n.class+qp.dim),])<-1
   diag(Amat[(n.class+qp.dim+1):(n.class+2*qp.dim),])<-(-1)
   
   # (4) compute bvec
   bvec <- rep(0, (2*qp.dim+n.class))
   for(j in 1:n.class)
    {for(i in 1:n.data)
       {flag=0
	  if(y[i]==j) flag=1
	  bvec[n.class+qp.dim+(j-1)*n.data+i]<--(gamma*flag+(1-gamma)*(1-flag))
  #correction to avoid redundant constraints when gamma=0 or 1
        if((flag==1 & gamma==0)|(flag==0 & gamma==1)) 
         bvec[n.class+qp.dim+(j-1)*n.data+i]<-
       bvec[n.class+qp.dim+(j-1)*n.data+i]-epsilon.C 
       }
     }
   # remove one redudant constraint
   Amat1<-Amat[c(1:(n.class-1),(n.class+1):(2*qp.dim+n.class)),]
   bvec1<-bvec[c(1:(n.class-1),(n.class+1):(2*qp.dim+n.class))]

   # (5) find solution by solve.QP

   dual <- solve.QP(Dmat, dvec, t(Amat1), bvec1, meq=(n.class-1))

   # place the dual solution into the non-trivial alpha positions
   alpha <- dual$solution 

   # make alpha zero if they are too small
   alpha[alpha < 0] <- 0
   for(j in 1:n.class)
    {for(i in 1:n.data)
       {if(y[i]==j& (alpha[(j-1)*n.data+i]>gamma)) 
		{alpha[(j-1)*n.data+i]<-gamma}
	  if(y[i]!=j& (alpha[(j-1)*n.data+i]>(1-gamma))) 
		{alpha[(j-1)*n.data+i]<-(1-gamma)}
       }
     }

   # calculate vmat from alpha as a n.data by n.class matrix
   vmat<-matrix(0,n.data,n.class)
   for(m in 1:n.class)
   { lvecj<-rep(0,n.data)
     lvecj[y!=m]<-1
     vmatj<-diag(lvecj)
     evecj<-matrix(0,1,n.class)
     evecj[1,m]<-1
     umatj<-evecj%x%In
     Hmatj=(In-2*vmatj)%*%umatj+AH
     vmat[,m]<-Hmatj%*%alpha/(n.data*lambda)
    }

   # find b vector using LP
   Kvmat <- (K %*% vmat) 
     
   # objective function with \xi_ij and (b_j)_+,-(b_j)_, j=1,...,k 

   alp <- rep((1-gamma),(qp.dim+2*n.class))
   for(j in 1:n.class)
	{ for(i in 1:n.data)
        {if(y[i]==j) alp[n.data*(j-1)+i]<-gamma}
       }
   alp[(qp.dim+1):(qp.dim+2*n.class)]<-0

   # constraint matrix and vector
   Alp<-matrix(0,(qp.dim+1),(qp.dim+2*n.class)) 
   blp<-rep(0,(qp.dim+1)) 
   for(j in 1:n.class)
    {Alp[1,(qp.dim+2*j-1)]<-1
     Alp[1,(qp.dim+2*j)]<-(-1) 
    }

   for(j in 1:n.class)
    {for(i in 1:n.data)
      { Alp[(1+n.data*(j-1)+i),n.data*(j-1)+i]<-1
        if(y[i]==j){Alp[(1+n.data*(j-1)+i),(qp.dim+2*(j-1)+1)]<-1
			  Alp[(1+n.data*(j-1)+i),(qp.dim+2*(j-1)+2)]<-(-1)
                    blp[(1+n.data*(j-1)+i)]<-a-Kvmat[i,j]
                    }
        if(y[i]!=j){Alp[(1+n.data*(j-1)+i),(qp.dim+2*(j-1)+1)]<-(-1)
			  Alp[(1+n.data*(j-1)+i),(qp.dim+2*(j-1)+2)]<-1
                    blp[(1+n.data*(j-1)+i)]<-1+Kvmat[i,j]
                    }
       }
     }
 
     # constraint directions
     const.dir <-rep(">=", (qp.dim+1))
     const.dir[1]<-"="
     bposneg <- lp("min", objective.in=alp, const.mat=Alp, const.dir=const.dir,
                  const.rhs=blp)$solution[(qp.dim+1):(qp.dim+2*n.class)]
     bvec<-rep(0,n.class)
     for(j in 1:n.class)
      {bvec[j]<-bposneg[(2*j-1)]-bposneg[(2*j)]
      }

   # compute the fitted values
   fit <- (matrix(rep(bvec, n.data), ncol=n.class, byrow=T) + Kvmat)
   # return the output  
   list(vmat = vmat, bvec = bvec, fit = fit)
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



three.data <- function(n.data, sigma)
{
  
  # generate x and y
  rx<-rnorm(n.data*2, 0, sigma)
  x <- matrix(rx, n.data, 2)
  y <- rep(0, n.data)
  u <- runif(n.data)
  
  for(i in 1:n.data)
  {if(u[i]<1/3) {x[i,]<-x[i,]+c(0,2)
  y[i]<-1}
    if(u[i]>=1/3 & u[i]<2/3) {x[i,]<-x[i,]+c(-1.732, -1)
    y[i]<-2}
    if(u[i]>=2/3){x[i,]<-x[i,]+c(1.732, -1)
    y[i]<-3}
  }
  return(list(x=x,y=y))
  
}



set.seed(12)
n.test<-10^2
sigma<-1.5
data.test<-three.data(n.test, sigma)

#calculate the Bayes rule
#############################

ypred.test<-rep(0, n.test)
for(i in 1:n.test)
{if(data.test$x[i,2]>(data.test$x[i,1]/1.732) & data.test$x[i,2]>(-data.test$x[i,1]/1.732)) ypred.test[i]<-1
if(data.test$x[i,2]<=(data.test$x[i,1]/1.732) & data.test$x[i,1]>0) ypred.test[i]<-3
if(data.test$x[i,1]<0 & data.test$x[i,2]<(-data.test$x[i,1]/1.732)) ypred.test[i]<-2
}
bayes.error<-sum(ypred.test != data.test$y)/n.test
set.seed(123)

ntrain<-50
lam.vec<- seq(-16,15,2)


data1<-three.data(ntrain,sigma)



#tune
data2<-three.data(ntrain,sigma)



remsvm.outn5 <- remsvm(as.matrix(data1$x), as.vector(data1$y), a=2, gamma=0.7, lambda =lam.vec, epsilon.H = 1e-3, epsilon.C=1e-3,kernel.type = 'linear',
                       x.test=as.matrix(data2$x), y.test=as.vector(data2$y))

print(remsvm.outn5$model)

# prediction
kernel.lin <- list(type='linear')

fit.test5 <- predict.remsvm(as.matrix(data1$x), as.matrix(data.test$x), kernel.lin, remsvm.outn5$model)

# test error rate:

error5<-error.rate(data.test$y, fit.test5)

error5
bayes.error
