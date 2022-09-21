
run_genetic <- function(n_individuals=100,n_discard=10,n_keep=10,
                        n_gens=10,k,ivs,dv,data,init_standardize = T,evaluate_best_on=NULL,use_oob_evaluation=T,metric = 'mse',timing = T,plotres = T){
  if(init_standardize){
    data <- as.matrix(data)
    data[,ivs] <- data[,ivs] %>% apply(2,standardize_vector)
    if(!is.null(evaluate_best_on)){
      test_data <- as.matrix(evaluate_best_on)
      test_data[,ivs] <- test_data[,ivs] %>% apply(2, standardize_vector)
    }
  }



  n_mates <- n_individuals-n_discard
  n_children <- n_individuals-n_discard-n_keep
  weights <- map(1:n_individuals,~gen_wts(length(ivs)))
  results <- list()
  if(plotres){
    plotdata <- matrix(,n_gens,3)
    colnames(plotdata) <- c('gen','oobmse','mse_test')
  }
  for(j in 1:n_gens){
    tictoc::tic()
    boot_id <- sample(1:nrow(data),replace=T)
    data_train <- data[boot_id,]
    data_test <- data[-boot_id,]
    registerDoParallel(cores=detectCores()-1)
    knns <- foreach(w = 1:length(weights)) %dopar%
      knn.reg(t(t(as.matrix(data_train[,ivs])) * weights[[w]]),t(t(as.matrix(data_test[,ivs])) * weights[[w]]),y = as.matrix(data_train[,dv]),k)
    scores <- knns %>% map(~mse_calc(as.matrix(data_test[,dv]),.x$pred)) %>% reduce(c)
    # if(testtest){
    # unwtd_knn <- knn.reg(as.matrix(data_train[,ivs]),as.matrix(data_test[,ivs]),as.matrix(data_train[,dv]),k)
    #   unwtd_mse <- mse_calc(as.matrix(data_test[,dv]),unwtd_knn$pred)
    #unwtd_knn_true <- knn.reg(as.matrix(data[,ivs]),as.matrix(test_data[,ivs]),as.matrix(data[,dv]),k)
    #unwtd_mse_true <- mse_calc(as.matrix(test_data[,dv]),unwtd_knn_true$pred)
    # }
    ranked_scores <- sort(scores)


    weights <- weights[order(scores)]
    safe <- weights[1:n_keep]
    mate_pop <- weights[1:n_mates]
    pairs <- foreach(i = sample(1:n_mates,
                                size=n_children,
                                replace = T,
                                prob = 1/ranked_scores[1:n_mates])) %do%
      c(i,sample(setdiff(1:n_mates,i),
                 size=1,
                 replace = T,
                 prob = 1/ranked_scores[setdiff(1:n_mates,i)]))

    new_inds <- map(1:n_discard,~gen_wts(length(ivs)))
    children <- pairs %>% map(~mate(.x,weights,scores))
    weights <- c(safe,children,new_inds)
    if(!is.null(evaluate_best_on)){
      knn_test <- knn.reg(t(t(as.matrix(data[,ivs])) * weights[[1]]),t(t(as.matrix(test_data[,ivs])) * weights[[1]]),as.matrix(data[,dv]),k)
      mse_test <- mse_calc(test_data[,dv],knn_test$pred)
      cat('Lowest MSE: ',min(scores),' MSE on test: ',mse_test,' Gen nr: ',j,"\n")
    }else{
      cat('Lowest MSE: ',min(scores),' Gen nr: ',j,"\n")
    }
    res <- list(mse_test = mse_test, weights = weights, mse_all = scores)
    results <- c(results,list(res))
    if(plotres){
      plotdata[j,1] <- j
      plotdata[j, 2] <- min(scores)
      plotdata[j, 3] <- mse_test
      p1 <- plotdata %>% na.omit() %>% as_tibble() %>% pivot_longer(2:3) %>%
        ggplot(aes(x=gen,y=value,color=name)) +
        geom_line()+
        geom_point()+
        theme_minimal()+
        ylim(c(0,1))
      print(p1)
    }
    tictoc::toc()

  }
  return(results)
}



