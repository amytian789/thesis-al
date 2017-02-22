# Load the MNIST dataset
load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('mnist/train-images-idx3-ubyte')
  #test <<- load_image_file('mnist/t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('mnist/train-labels-idx1-ubyte')
  #test$y <<- load_label_file('mnist/t10k-labels-idx1-ubyte')  
}

# Plot a single digit
show_digitsmall <- function(arr196, col=gray(12:1/12), ...) {
  image(matrix(arr196, nrow=14)[,14:1], col=col, ...)
}

# Compress the MNIST dataset from 28x28 to 14x14
compressImg <- function(full){
  compressFour <- function(j){
    pixelvec = rep(NA,4)
    pixelvec[1] = full[2*j-1+floor((j-1)/14)*28];
    pixelvec[2] = full[2*j+floor((j-1)/14)*28];
    pixelvec[3] = full[2*j-1+28+floor((j-1)/14)*28];
    pixelvec[4] = full[2*j+28+floor((j-1)/14)*28];
    return(mean(pixelvec))
  }
  
  compress = unlist(lapply(1:196,compressFour))
  return(compress)
}

# Plot a multitude of digits
plotTable <- function(numRow,numCol,vec.labels,mat.images){
  vec.uniq = unique(vec.labels)
  par(mfrow=c(numRow,numCol),pty="s",mar = c(0.1,0.1,0.1,0.1))
  for(i in 1:length(vec.uniq)){
    tmpidx = which(vec.labels==vec.uniq[i])
    for(j in 1:length(which(vec.labels==vec.uniq[i]))){
      show_digitsmall(mat.images[tmpidx[j],],asp=TRUE)
    }
  }
}