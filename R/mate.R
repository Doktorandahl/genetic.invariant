
mate = function(pair,weights,score,mutate_big_prob = 0.01,mutate_small_prob=0.05){
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
