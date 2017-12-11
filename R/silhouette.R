#km_obj result of kmeans()
#returns S index per cluster of km_obj

silhouetting <- function(km_obj,d_matrix){
    sapply(seq(max(km_obj$cluster)),
           function(clust) mean(sapply(which(km_obj$cluster==clust),function(y) s_ind(y,km_obj,d_matrix)))
    )
}

#columns are samples
#distances between samples via generalized Euclidian
distance_matrix_maker <- function(mat){
    sapply(seq(dim(mat)[2]),
    function(ind) sapply(seq(dim(mat)[2]), function(y) sum((mat[,ind]-mat[,y])**2)**0.5)
    )
}

s_ind<-function(ind, km_obj, dist) {
    a <- a_ind(ind, km_obj, dist)
    b <- b_ind(ind, km_obj, dist)
    (b-a)/max(b,a)
}

a_ind <- function(ind, km_obj, dist){
    mean( sapply( which(km_obj$cluster==km_obj$cluster[ind])  , function(x) dist[ind,x] ) )
}

b_ind <- function(ind,km_obj,dist){
    min(
        sapply(unique(km_obj$cluster)[unique(km_obj$cluster)!=km_obj$cluster[ind]], 
               function(other) mean(sapply(which(km_obj$cluster==other), function(y) dist[ind,y]))
        )
    )
}