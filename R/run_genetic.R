


#' Run genetic knn
#'
#' @param train Training data
#' @param test Test data, optional
#' @param formula Formula with prediction target on the left hand side and predictor features on the right
#' @param k number of nearest neighbors to consider
#' @param gene_dist Distribution for generating genes. Either a numeric vector of possible variable weights, or a character string referencing a probability distribution in R such as 'unif', 'chisq', or 'norm'. Also works with the half-normal distribution 'hnorm'
#' @param metric Metric to evaluate generations on. Allowed values are any string referring to a evaluation function in the MLmetrics package, or any custom function in the form function(y_pred,y_true) to generate a single performance value
#' @param metric_type Does a higher or lower score on the evaluation metric indicate better performance. E.g. A higher AUPR score indicates better performance while a lower MSE indicates better performance
#' @param n_generations Number of generations to run the algorithm
#' @param n_individuals Number of individuals per generation
#' @param n_discard Number of worst performing individuals to discard in each generation
#' @param n_keep Number of best performing individuals to keep in each generation
#' @param eval_gens Should generations be evaluated on the leave-one-out cross-validation on the training data (loocv) or on bootstrapped out-of-bag data (oob). If 'oob' then training will be on bootstrapped data. 'loocv' is faster but prone to first-degree overfitting.
#' @param oob_repetitions Number of bootstrap repetitions for each individual in each generation when calculating the evaluation metric. A higher number yields lower levels of overfitting, but extends training time by oob_repetitions
#' @param standardize Should predictor features be standardized before knn is run? Defaults to TRUE
#' @param normalize Should predictor features be normalized before knn is run? Defaults to FALSE
#' @param normalize_scores Should evaluation scores be normalized when determining individual fitness? Defaults to TRUE
#' @param p_mutation_small Probability of small mutation in genes when mating
#' @param p_mutation_large Probability of large mutation in genes when mating
#' @param evaluate_ensemble Should ensembling be done on the best performing individuals? Defaults to TRUE
#' @param n_ensemble Number of best performing individuals to include in ensemble
#' @param ...
#'
#' @return
#' @export
#'
#' @examples run_genetic_knn(x,y,z)
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%

run_genetic_knn <- function(train,
                            test= NULL,
                            formula,
                            k,
                            gene_dist = 'unif',
                            metric = 'MSE',
                            metric_type = c('lower_is_better','higher_is_better'),
                            n_generations = 10,
                            n_individuals=100,
                            n_discard=10,
                            n_keep=10,
                            eval_gens = c('loocv','oob','cv'),
                            oob_repetitions = 10,
                            standardize=T,
                            normalize=F,
                            normalize_scores = T,
                            p_mutation_small = 0.05,
                            p_mutation_large = 0.01,
                            evaluate_ensemble = T,
                            n_ensemble=10,
                            ...){
  `%dopar%` <- foreach::`%dopar%`
  `%do%` <- foreach::`%do%`
  `%:%` <- foreach::`%:%`
  dots_arg <- list(...)
  if(is.character(metric)){
    eval <- getFromNamespace(metric,'MLmetrics')
  }else if(is.function(metric)){
    eval <- metric
    metric <- as.character(metric)
  }else{
    stop('No valid metric provided')
  }
  metric_type <- match.arg(metric_type,c('lower_is_better','higher_is_better'))
  eval_gens <- match.arg(eval_gens,c('loocv','oob','cv'))


  mf_train <- model.frame(formula,train)
  mm_train <- model.matrix(formula,train)

  n_ivs <- ncol(mm_train) - 1
  if(!is.null(test)){
    mf_test <- model.frame(formula,test)
    mm_test <- model.matrix(formula,test)

  }

  if(standardize){
    mm_train[,2:ncol(mm_train)] <- apply(mm_train[,2:ncol(mm_train)],2,standardize_vector)
    if(!is.null(test)){
      mm_test[,2:ncol(mm_test)] <- apply(mm_test[,2:ncol(mm_test)],2,standardize_vector)
    }
  }

  if(normalize){
    mm_train[,2:ncol(mm_train)] <- apply(mm_train[,2:ncol(mm_train)],2,normalize_vector)
    if(!is.null(test)){
      mm_train[,2:ncol(mm_test)] <- apply(mm_test[,2:ncol(mm_test)],2,normalize_vector)
    }
  }

  if(class(mf_train[,1])=='factor'){
    if(length(levels(mf_train[,1]))>2){
      stop('Currently only supports binary and continuous outcomes')
    }else{
      mf_train[,1] <- as.numeric(mf_train[,1])-1
      if(!is.null(test)){
        mf_test[,1] <- as.numeric(mf_test[,1])-1
      }
    }
  }


  ## Number of individuals allowed to mate
  n_mates <- n_individuals-n_discard
  ## Number of children to generate
  n_children <- n_individuals-n_discard-n_keep
  ## Generate genes
  genes <- purrr::map(1:n_individuals,~gen_wts(n_ivs,dist=gene_dist,dots_arg))


  results <- list()

  for(j in 1:n_generations){
    if(eval_gens == 'oob'){
      scores <- foreach::foreach(oob = 1:oob_repetitions) %do%
        bootstrapping_routine_knn(mf_train,mm_train,genes,eval,k) %>%
        reduce(`+`)/oob_repetitions
    }else if(eval_gens =='loocv'){
      scores <- loocv_routine_knn(mf_train,mm_train, genes,eval,k)
    }
    if(normalize_scores){
      scores_norm <- normalize_vector(scores)
      if(metric_type == 'lower_is_better'){
        scores_fitness <- (scores_norm - 1)*(-1)
        eval2 <- min
      }else{
        eval2 <- max
      }
      ranked_scores <- sort(scores_fitness,decreasing = T)
    }else{
      if(metric_type == 'lower_is_better'){
        ranked_scores <- sort(scores)
        scores_fitness <- (scores-max(scores))*(-1)
        eval2 <- min
      }else{
        ranked_scores <- sort(scores,decreasing=T)
        eval2 <- max
      }
    }




    ## Reorder individuals by their score/fitness
    genes <- genes[order(scores_fitness,decreasing = T)]
    ## Save n_keep individuals to next generation
    safe <- genes[1:n_keep]
    ## Select individuals for mating
    mate_pop <- genes[1:n_mates]
    ## Make random draws of parents for new children based on their ranked score probability
    pairs <- foreach::foreach(i = 1:n_children) %do%
      sample(1:n_mates,size=2,replace = F,prob = ranked_scores[1:n_mates])

    new_inds <- purrr::map(1:n_discard,~gen_wts(n_ivs,dist=gene_dist,dots_arg))

    children <- purrr::map(pairs, ~mate(genes[[.x[1]]],genes[[.x[2]]],ranked_scores[.x[1]],ranked_scores[.x[2]],gene_dist =  gene_dist,p_mutation_small = p_mutation_small,p_mutation_large=p_mutation_large,dots_arg=dots_arg))

    genes <- c(safe,children,new_inds)

    ## Remove duplicates
    n_duplicates <- sum(duplicated(genes))
    ii <- 0
    while(n_duplicates>0 & ii <= 1000){
      pairs_dup <- foreach::foreach(i = 1:n_duplicates) %do%
        sample(1:n_mates,size=2,replace = F,prob = ranked_scores[1:n_mates])
      children_dup <- purrr::map(pairs_dup, ~mate(genes[[.x[1]]],genes[[.x[2]]],ranked_scores[.x[1]],ranked_scores[.x[2]],gene_dist =  gene_dist,p_mutation_small = p_mutation_small,p_mutation_large=p_mutation_large,dots_arg=dots_arg))
      genes <- c(genes[!duplicated(genes)],children_dup)
      if(ii==1000){
        warning('Duplicates could not be fully removed, replacing duplicates with new inds')
        new_inds_dup <- purrr::map(1:n_duplicates,~gen_wts(n_ivs,dist=gene_dist,dots_arg))
        genes <- c(genes[!duplicated(genes)],new_inds_dup)
      }
      n_duplicates <- sum(duplicated(genes))
    }

    if(!is.null(test)){
      knn_test <- FNN::knn.reg(train = scale(mm_train[,-1],center=FALSE,scale=1/genes[[1]]),
                               test = scale(mm_test[,-1],center=FALSE,scale=1/genes[[1]]),
                               y = mf_train[,1],
                               k=k)
      score_test <- eval(knn_test$pred,mf_test[,1])
      if(evaluate_ensemble){
        knn_test_ensemble <- foreach(e = 1:n_ensemble) %do%
          FNN::knn.reg(train = scale(mm_train[,-1],center=FALSE,scale=1/genes[[e]]),
                       test = scale(mm_test[,-1],center=FALSE,scale=1/genes[[e]]),
                       y = mf_train[,1],
                       k=k)
        knn_train_ensemble <- foreach(e = 1:n_ensemble) %do%
          FNN::knn.reg(train = scale(mm_train[,-1],center=FALSE,scale=1/genes[[e]]),
                       y = mf_train[,1],
                       k=k)

        knn_train_ensemble_pred <- purrr::map(knn_train_ensemble,~.x$pred) %>% reduce(`+`)/n_ensemble

        knn_test_ensemble_pred <- purrr::map(knn_test_ensemble,~.x$pred) %>% reduce(`+`)/n_ensemble
        score_ensemble_train <- eval(knn_train_ensemble_pred,mf_train[,1])

        score_ensemble_test <- eval(knn_test_ensemble_pred,mf_test[,1])
        cat('Ensemble (Best) ',metric,' in train: ',score_ensemble_train,' (',eval2(scores),').',' Ensemble (Best) ',metric,' on test: ',score_ensemble_test,' (',score_test,'). Gen nr: ',j,"\n",sep="")

      }else{
        score_ensemble_test <- NULL
        score_ensemble_train <- NULL
        cat('Best ',metric,' in train: ',eval2(scores),'.',' Best ',metric,' on test: ',score_test,'. Gen nr: ',j,"\n")
      }
    }else{
      score_test <- NULL
      score_ensemble_test <- NULL
      score_ensemble_train <- NULL
      cat('Best ',metric,' in train: ',eval2(scores),' Gen nr: ',j,"\n")
    }

    res <- list(score_test = score_test,
                score_ensemble_test = score_ensemble_test,
                score_ensemble_train = score_ensemble_train,
                weights = genes,
                raw_scores = scores,
                normalized_scores = scores_norm)
    results <- c(results,list(res))


  }
  return(results)
}

bootstrapping_routine_knn <- function(mf_train,mm_train,genes,eval,k){
  boot_id <- sample(1:nrow(mm_train),replace=T)
  mf_train_boot <- mf_train[boot_id,]
  mf_oob_boot <- mf_train[-boot_id,]
  data_train <- mm_train[boot_id,]
  data_oob <- mm_train[-boot_id,]

  knns <- foreach::foreach(w = 1:length(genes)) %dopar%
    FNN::knn.reg(train = scale(data_train[,-1],center=FALSE,scale=1/genes[[w]]),
                 test = scale(data_oob[,-1],center=FALSE,scale=1/genes[[w]]),
                 y = mf_train_boot[,1],
                 k=k)
  knns %>% purrr::map(~eval(.x$pred,mf_oob_boot[,1])) %>% reduce(c)
}


loocv_routine_knn <- function(mf_train,mm_train,genes,eval,k){

  knns <- foreach::foreach(w = 1:length(genes)) %dopar%
    FNN::knn.reg(train = scale(mm_train[,-1],center=FALSE,scale=1/genes[[w]]),
                 y = mf_train[,1],
                 k=k)
  knns %>% purrr::map(~eval(.x$pred,mf_train[,1])) %>% reduce(c)
}
