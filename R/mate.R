
#' Title
#'
#' @param parent1 Vector of genes from parent 1
#' @param parent2 Vector of genes from parent 2
#' @param fitness1 Fitness of parent 1. NULL gives equal fitness of both parents, otherwise scale so that a higher value of fitness corresponds to better fitness
#' @param fitness2 Fitness of parent 2. NULL gives equal fitness of both parents, otherwise scale so that a higher value of fitness corresponds to better fitness
#' @param p_mutation Probability of mutation in the genes
#' @param mutated_genes New genes to be inserted for mutations
#'
#' @return A vector of a child with genes from parent 1 and 2
#'
mate <- function(parent1,parent2,fitness1=NULL,fitness2=NULL,p_mutation = 0.05, mutated_genes=NULL){
  n_genes <- length(parent1)

  genes <- sample(1:2,size=n_genes,replace=T,prob=c(fitness1,fitness2))
  child <- rep(NA,n_genes)
  child[genes==1] <- parent1[genes==1]
  child[genes==2] <- parent2[genes==2]

  if(is.null(gen_wts)){
    return(child)
  }else{
    mutate_gene <- runif(n_genes)<p_mutation
    child[mutate_gene] <- mutated_genes[mutate_gene]
    return(child)
  }
}



mate_old = function(pair,weights,score,mutate_big_prob = 0.01,mutate_small_prob=0.05){
  m1 <- weights[[pair[1]]]
  m2 <- weights[[pair[2]]]
  f1 <- score[pair[1]]
  f2 <- score[pair[2]]

  genes <- sample(1:2,size=length(m1),replace=T,prob=c(1/f1,1/f2))
  child <- rep(NA,length(m1))
  child[genes==1] <- m1[genes==1]
  child[genes==2] <- m2[genes==2]
  mutate_big <- runif(length(m1))<mutate_big_prob
  mutate_small <- runif(length(m1))<mutate_small_prob
  mutate_gene_big <- abs(rnorm(length(child),mean=child,sd=0.5))
  mutate_gene_small <- abs(rnorm(length(child),mean=child,sd=0.1))
  child[mutate_big] <- mutate_gene_big[mutate_big]
  child[mutate_small] <- mutate_gene_small[mutate_small]
  return(child)
}
