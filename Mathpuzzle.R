library(dplyr)
library(pracma)
xx1 <- vector()
xx2 <- vector()
xx3 <- vector()
xx4 <- vector()
# yy1 <- vector()

k <- 1
for (i in 10:99) {
  for (j in i:99) {
    if (!isprime(i) | !isprime(j)) {
      xx1[k] <- i + j
      xx2[k] <- i
      xx3[k] <- j
      xx4[k] <- i*j
      # yy1[k] <- paste(xx1[k],xx4[k],sep = "")
      k <- k + 1
    }
  }
}

xx <- data.frame()
# xx <- data.frame(sum = xx1, num1 = xx2, num2 = xx3, prod = xx4, sumprod = yy1)
xx <- data.frame(sum = xx1, num1 = xx2, num2 = xx3, prod = xx4)

GetFreqSum <- function(sval,tbl_sum) {
  ii <- which(tbl_sum[ ,1] == sval)
  freq <- tbl_sum[ii,2]
  return(freq)
}

GetFreqProd <- function(pval,tbl_prod) {
  ii <- which(tbl_prod[ ,1] == pval)
  freq <- tbl_prod[ii,2]
  return(freq)
}


# dt <- as.data.frame(table(xx$prod))

tbl_sum <- as.data.frame(table(xx$sum))
tbl_prod <- as.data.frame(table(xx$prod))

idx_rel <- NULL
for (i in 1:nrow(xx)) {
  xx$FreqSum[i] <- GetFreqSum(xx$sum[i],tbl_sum)
  xx$FreqProd[i] <- GetFreqProd(xx$prod[i],tbl_prod)
  
  # if (GetFreqProd(xx$prod[i],tbl_prod) > 1) {
  #   # if (GetFreqSum(xx$sum[i],tbl_sum) == 1) {
  #     idx_rel <- c(idx_rel,i)
  #   # }
  # }
  
  # if (GetFreqSum(xx$sum[i],tbl_sum) == 1) {
  #   # print(i)
  #   idx_rel <- c(idx_rel,i)
  #   # print(GetFreqSum(xx$sum[i],tbl_sum))
  #   # cat("\n")
  # }
  
  if (xx$FreqSum[i] > 1) {
    if (xx$FreqProd[i] > 1) {
      idx_rel <- c(idx_rel,i)
    }
  }
  
}

xx[idx_rel,]


# dt1 <- dt %>% filter(Freq>1) # all prods with freq > 1
# sum_val  <-  list()
# idx <- list()
# n1 <- list()
# n2 <- list()
# for (i in 1:nrow(dt1)) {
#   idx[[i]] <- which(xx$prod == dt1[i,1])
#   sum_val[[i]] <- xx$sum[idx[[i]]]
#   n1[[i]]  <- xx$num1[idx[[i]]]
#   n2[[i]]  <- xx$num2[idx[[i]]]
# }
# 
# dt2 <- as.data.frame(table(unlist(sum_val)))
# dt2[dt2$Freq==1,]
# dt3 <- list()
# for (i in 1:nrow(dt1)) {
#   # sum_val[[i]] is relevant
#   ff <- NULL
#   for (j in 1:length(idx[[i]])){
#   ii <- which(dt2[,1] ==  xx$sum[idx[[i]]][j])
#    ff <- c(ff,dt2[ii,2]) # gives frequency
#   }
#   dt3[[i]] <- ff
# }
# 
# 
