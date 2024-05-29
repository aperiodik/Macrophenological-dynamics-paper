# perform dimension reduction: isomap 
# approx computation time: 6 hours

# no. of k-nearest neighbours
knn_vec1 <-16

dimR.list <- lapply(1:nowi,
                    function(i){
                      print(i)
                      x_fi <- datawin.list[[i]]
                      print(dim(x_fi)) # print dimension of data at time i
                      # compute Jaccard distance of time window i
                      D_fi = vegdist(x_fi, "jaccard")
                      kNN_vec = c(knn_vec1) # no. of k nearest neighbours
                      k = kNN_vec
                      n_Y = 15 # no. of isomap components
                      DG_fi = d2dgeo(D_fi, k) # distance structure
                      Y_fi  = cmdscale(DG_fi, k = n_Y) # isomap components
                      print(dim(Y_fi))
                      print(dim(DG_fi))
                      # initialise residual variance 
                      res_var_fi = array(NA, dim = c(length(kNN_vec), n_Y))
                      rownames(res_var_fi) = kNN_vec
                      colnames(res_var_fi) = 1:n_Y
                      # compute residual variance
                      for (ii in 1:n_Y) {
                        # compute distance matrix of the embedding
                        D_Y_fi = as.matrix(dist(Y_fi[, 1:ii])) # default = euclidean distance
                        # estimate explained variances/residual variance
                        res_var_fi[, ii] =  1-(cor(as.vector(DG_fi), as.vector(D_Y_fi))^2)
                      }
                      # create output vector: 1st row = residual variance, 
                      #       following rows = corresp. isomap components per column
                      resultIsoResvar <- rbind(res_var_fi,Y_fi[,1:n_Y]) 
                      print(dim(resultIsoResvar)) # print residual variance
                      my.list <- resultIsoResvar 
                      my.list
                    }
)

print("isomap computation complete")

# save data
file_name_fi <- paste("resu/DimRed_FI_wsize", wsize,"_skp", skp,"_kNN", knn_vec1, ".Rdata", sep = "")
save(dimR.list, file = file_name_fi)
