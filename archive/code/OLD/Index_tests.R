test_data <- runif(100)

# Entropia con libreria
return_Entropy <- function(data_col){
  ris <- calcGEI(data_col, alpha = 1)
  if(is.null(ris)){
    ris <- 0
  } else {
    ris <- as.numeric(ris[[1]][1])
  }
  return(ris)
}

return_Entropy(test_data)
calcGEI(test_data)

# Entropia manuale

# return_group <- function(data_col, l = 10){
#   ris_vec <- c()
#   ris_vec[1] <- min(data_col) 
#   for(i in 2:(l)){
#     ris_vec[i] <- ris_vec[i-1] + (max(data_col)-min(data_col))/l
#   }
#   ris_vec[l+1] <- max(data_col)
#   return(ris_vec)
# }
# 
# return_group(b)

# unique(round(diff(return_group(b)),digits = 6))

# return_Entropy_2 <- function(data_col, l = 10){
#   ris_vec <- c()
#   ris_vec[1] <- min(data_col) 
#   for(i in 2:(l)){
#     ris_vec[i] <- ris_vec[i-1] + (max(data_col)-min(data_col))/l
#   }
#   ris_vec[l+1] <- max(data_col)
#   
#   p_vec <- numeric(length = l)
#   
#   for(i in 1:l){
#     p_vec[i] <- length(which(data_col>=ris_vec[i] & data_col<ris_vec[i+1]))
#   }
#   p_vec[l] <- p_vec[l]+1
#   p_vec <- p_vec/length(data_col)
#   ind_E <- -sum(p_vec*log2(p_vec))
#   return(ind_E)
# }

return_Entropy_2 <- function(data_col, l = 10){
  ind_E <- (1/length(data_col))*sum((data_col/mean(data_col))/log(data_col/mean(data_col)))
  return(ind_E)
}

return_Entropy_2(test_data)
sum(return_Entropy_2(test_data))
length(return_Entropy_2(test_data))



