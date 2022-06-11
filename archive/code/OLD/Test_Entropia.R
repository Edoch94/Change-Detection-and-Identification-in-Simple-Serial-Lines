library(triangle)
a <- runif(1000000)
a <- rexp(1000000)
a <- rtriangle(1000000)

# return_Entropy_2 <- function(v){
#   res <- (1/length(v))*(sum((v/mean(v))*log(v/mean(v))))
#   return(res)
# }

return_GEI <- function(v, al = 1){
  if(al == 1){
    res <- (1/length(v))*(sum((v/mean(v))*log(v/mean(v))))
  } else if(al == 0) {
    res <- -(1/length(v))*(sum(log(v/mean(v))))
  } else {
    res <- (1/(length(v)*al*(al-1)))*(sum((v/mean(v))^(al)-1))
  }
  return(res)
}

calcGEI(a, alpha = 1)
return_GEI(a, al = 1) # Notare che v non può contenere valori negativi: v non può essere generato da una distribuzione normale


