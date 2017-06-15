
#Function to make a "fireworks" ordination plot where line segments extend from a group centroid to each coordinate in two-dimensions
#The function needs as input a dataframe output from an ordination analysis with columns for each datum's score for a given ordination dimension
#the function can handle any number of dimensions, and it doesn't matter what they are called. 
#The function also needs a column called "colors" which has the color you want for a particular datum
#Finally, the dataframe needs to have a column called "groups" which is the grouping variable (e.g. populations, treatments, etc)
#There can be no other columns in the dataframe

#the first input is the ordination dataframe as described above, the second two inputs are which dimensions you want (the first one is the x-axis)
fireworksPlot = function(ord_out, dim1, dim2){
  
  calc_centroid = function(x){ 
    means_each_dim = NA
    dimension = NA
    for(j in 1:length(x)){
      if(names(x[j]) %in% c("groups","colors")){
        next
      }else{
        means_each_dim[j] = mean(unlist(x[j]))
        dimension[j] = j
      }
    }
    return(means_each_dim)
  }
  
  parsed_ord_out = split(ord_out, ord_out$groups)
  
  centroids = sapply(parsed_ord_out, FUN=calc_centroid)
  
  #make empty plot of correct dimensions. 
  #just use the first two columns, because there will always be k>= 2 dimensions for a 2d ordination
  
  plot(ord_out[,1],ord_out[,2],type="n", cex=1.2, xlab=paste("MDS", dim1, sep=" "), ylab=paste("MDS", dim2, sep=" "), cex.lab=1.3)
  
  #plot line segments from centroid
  for(i in 1:length(parsed_ord_out)){
    for(l in 1:length(parsed_ord_out[[i]][[dim1]])){
      segments(parsed_ord_out[[i]][[dim1]][l], 
               parsed_ord_out[[i]][[dim2]][l], 
               centroids[dim1,i], 
               centroids[dim2,i],
               col=unlist(unique(parsed_ord_out[[i]][which(names(parsed_ord_out[[i]]) == "colors")])),
               lwd=1.5)
    }
  }
}