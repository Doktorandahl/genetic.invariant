


#' Run genetic knn
#'
#' @param train
#' @param test
#' @param formula
#' @param k
#' @param gene_dist
#' @param metric
#' @param metric_type
#' @param n_generations
#' @param n_individuals
#' @param n_discard
#' @param n_keep
#' @param standardize
#' @param normalize
#' @param normalize_scores
#' @param timing
#' @param plot_results
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
                            gene_dist,
                            metric = 'MSE',
                            metric_type = c('lower_is_better','higher_is_better'),
                            n_generations = 10,
                            n_individuals=100,
                            n_discard=10,
                            n_keep=10,
                            standardize=T,
                            normalize=F,
                            normalize_scores = T,
                            timing = T,
                            plot_results = T,
                            ...){
  `%dopar%` <- foreach::`%dopar%`
  `%do%` <- foreach::`%do%`
 if(is.character(metric)){
   eval <- getFromNamespace(metric,'MLmetrics')
 }else if(is.function(metric)){
   eval <- metric
   metric <- as.character(metric)
 }else{
   stop('No valid metric provided')
 }
  metric_type <- match.arg(metric_type,c('lower_is_better','higher_is_better'))

 mf_train <- model.frame(formula,train)
 n_ivs <- ncol(mf_train) - 1
 if(!is.null(test)){
   mf_test <- model.frame(formula,test)
 }

  if(standardize){
    mf_train[,2:ncol(mf_train)] <- apply(mf_train[,2:ncol(mf_train)],2,standardize_vector)
    if(!is.null(test)){
      mf_test[,2:ncol(mf_test)] <- apply(mf_test[,2:ncol(mf_test)],2,standardize_vector)
    }
  }

 if(normalize){
   mf_train[,2:ncol(mf_train)] <- apply(mf_train[,2:ncol(mf_train)],2,normalize_vector)
   if(!is.null(test)){
     mf_train[,2:ncol(mf_test)] <- apply(mf_test[,2:ncol(mf_test)],2,normalize_vector)
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
  genes <- purrr::map(1:n_individuals,~gen_wts(n_ivs,dist=gene_dist,...))


  results <- list()

  for(j in 1:n_generations){
    boot_id <- sample(1:nrow(mf_train),replace=T)
    data_train <- mf_train[boot_id,]
    data_oob <- mf_train[-boot_id,]

    knns <- foreach::foreach(w = 1:length(genes)) %dopar%
      FNN::knn.reg(train = scale(data_train[,-1],center=FALSE,scale=1/genes[[w]]),
               test = scale(data_oob[,-1],center=FALSE,scale=1/genes[[w]]),
               y = data_train[,1],
               k=k)

    scores <- knns %>% purrr::map(~eval(.x$pred,data_oob[,1])) %>% reduce(c)

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
    genes <- genes[order(scores)]
    ## Save n_keep individuals to next generation
    safe <- genes[1:n_keep]
    ## Select individuals for mating
    mate_pop <- genes[1:n_mates]
    ## Make random draws of parents for new children based on their ranked score probability
    pairs <- foreach::foreach(i = 1:n_children) %do%
      sample(1:n_mates,size=2,replace = F,prob = ranked_scores[1:n_mates])

    new_inds <- purrr::map(1:n_discard,~gen_wts(n_ivs,dist=gene_dist,...))

    children <- purrr::map(pairs, ~mate(genes[[.x[1]]],genes[[.x[2]]],ranked_scores[.x[1]],ranked_scores[.x[2]],mutated_genes =  gen_wts(n_ivs,dist=gene_dist,...)))

    genes <- c(safe,children,new_inds)

    if(!is.null(test)){
      knn_test <- FNN::knn.reg(train = scale(mf_train[,-1],center=FALSE,scale=1/genes[[1]]),
                          test = scale(mf_test[,-1],center=FALSE,scale=1/genes[[1]]),
                          y = mf_train[,1],
                          k=k)
      score_test <- eval(knn_test$pred,mf_test[,1])
      cat('Best ',metric,' in train: ',eval2(scores),'.',' Best ',metric,' on test: ',score_test,'. Gen nr: ',j,"\n")
    }else{
      score_test <- NULL
      cat('Best ',metric,' in train: ',eval2(scores),' Gen nr: ',j,"\n")
    }

    res <- list(score_test = score_test, weights = genes, raw_scores = scores, normalized_scores = scores_norm)
    results <- c(results,list(res))


  }
  return(results)
}



