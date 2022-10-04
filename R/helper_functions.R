
#' Standardize and normalize vector
#'
#' Standardization and normalization of numeric vectors
#'
#' @param x Numeric vector to be standardized or normalized
#' @describeIn standardize_vector Standardizes vector by subtracting the mean and dividing by the standard deviation
#' @return Standardized vector
#' @export
#'
#' @examples x <- c(0,3,2,1,9,5,4,3,2,1)
#' standardize_vector(x)
standardize_vector <- function(x){
  if(sd(x)==0){
    warning('No variation in scores to standardize')
    return(x)
  }
  return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
}

#' @describeIn standardize_vector Normalizes vector by subtracting the minimum value and dividing by the maximum value
normalize_vector <- function(x){
  if(sd(x)==0){
    warning('No variation in scores to normalize')
    return(x)
  }
  return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
}

#' Random draws from half normal distribution
#'
#' @param n Number of observations
#' @param sd standard deviation
#'
#' @return Vector of random draws from half normal distribution
#'
#' @examples rhnorm(10)
rhnorm <- function(n, sd = 1){
  abs(rnorm(n,sd = sd/sqrt(1-2/pi)))
}

rescale_ivs <- function(x,w){

}


knn.reg_formula <- function(formula,train,test=NULL,k,
                            algorithm = c("kd_tree","cover_tree", "brute"),
                            standardize=TRUE,
                            normalize = FALSE){
  algorithm <- match.arg(algorithm,c("kd_tree","cover_tree", "brute"))
  mf_train <- model.frame(formula,train)
  mm_train <- model.matrix(formula,train)
  n_ivs <- ncol(mm_train) - 1
  if(!is.null(test)){
    mf_test <- model.frame(formula,test)
  mm_test <- model.matrix(formula,test)
  }

  if(standardize){
    #mf_train[,2:ncol(mf_train)] <- apply(mf_train[,2:ncol(mf_train)],2,standardize_vector)
    mm_train[,2:ncol(mm_train)] <- apply(mm_train[,2:ncol(mm_train)],2,standardize_vector)

    if(!is.null(test)){
     # mf_test[,2:ncol(mf_test)] <- apply(mf_test[,2:ncol(mf_test)],2,standardize_vector)
      mm_test[,2:ncol(mm_test)] <- apply(mm_test[,2:ncol(mm_test)],2,standardize_vector)

    }
  }

  if(normalize){
    mf_train[,2:ncol(mf_train)] <- apply(mf_train[,2:ncol(mf_train)],2,normalize_vector)
    mm_train[,2:ncol(mm_train)] <- apply(mm_train[,2:ncol(mm_train)],2,normalize_vector)

    if(!is.null(test)){
      mf_test[,2:ncol(mf_test)] <- apply(mf_test[,2:ncol(mf_test)],2,normalize_vector)
      mm_test[,2:ncol(mm_test)] <- apply(mm_test[,2:ncol(mm_test)],2,normalize_vector)

    }
  }
  if(is.null(test)){
  return(FNN::knn.reg(train = mm_train[,-1],
               y = mf_train[,1],
               k=k,
               algorithm = algorithm))
  }else{
    return(FNN::knn.reg(train = mm_train[,-1],
                        test = mm_test[,-1],
                        y = mf_train[,1],
                        k=k,
                        algorithm = algorithm))
  }

}


