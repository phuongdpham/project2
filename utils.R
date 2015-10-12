
eval <- function(X, L, C)
{
   n <- nrow(X);
   e <- rep(NA, n);

   for(k in 1:n)
   {
       Xk <- X[k, ];
       Ck <- C[L[k], ];
       e[k] <- mean( abs( Xk - Ck ) );
   }

   return( sum(e) );
}


colorAvg <- function(img)
{
  r <- mean(img[,,1]);
  g <- mean(img[,,2]);
  b <- mean(img[,,3]);
  return( c(r, g, b) );
}

colorAvg2 <- function(img)
{
   mr = nrow(img);
   mc = ncol(img);
   mr <- mr / 2;
   mc <- mc / 2;

   a1 <- img[1:mr, 1:mc, ];
   a2 <- img[1:mr + mr, 1:mc, ];
   a3 <- img[1:mr, 1:mc + mc, ];
   a4 <- img[1:mr + mr, 1:mc + mc, ];
  
   v1 <- colorAvg(a1);
   v2 <- colorAvg(a2);
   v3 <- colorAvg(a3);
   v4 <- colorAvg(a4);
   
   m <- rbind(v1, v2, v3, v4);

   return(m);   
}

colorAvg3 <- function(img)
{
   mr = nrow(img);
   mc = ncol(img);
   mr <- mr / 2;
   mc <- mc / 2;

   a1 <- img[1:mr, 1:mc, ];
   a2 <- img[1:mr + mr, 1:mc, ];
   a3 <- img[1:mr, 1:mc + mc, ];
   a4 <- img[1:mr + mr, 1:mc + mc, ];
  
   v1 <- colorAvg2(a1);
   v2 <- colorAvg2(a2);
   v3 <- colorAvg2(a3);
   v4 <- colorAvg2(a4);
   
   m <- rbind(v1, v2, v3, v4);

   return(m);
}

colorAvg123 <- function(imgName)
{
  img <- readJPEG(imgName);
  
  m1 <- colorAvg(img);
  m2 <- colorAvg2(img);
  m3 <- colorAvg3(img);
  
  
  
  m <- rbind(m1, m2, m3);
  result <- as.vector(m);

  return(result); 
}

calHist <- function(img, x, y, u, v, channel) {
  h <- 0 * c(1 : 16);
  for (i in x : u) {
    for (j in y : v) {
      x <- floor(255 * img[i, j, channel] / 16.0) + 1;
      h[x] <- h[x] + 1;
    }
  }
  return(h);
}

colorHist <- function(imgName)
{
  img <- readJPEG(imgName);
  m <- nrow(img[ , , 1]);
  n <- ncol(img[ , , 1]);
  m1 <- floor(m / 2);
  n1 <- floor(n / 2);
  m2 <- floor(m1 / 2)
  n2 <- floor(n1 / 2)
  
  hist_features <- c();
  
  for (chan in 1 : 3) {
    # layer 1
    hist_features <- c(hist_features, calHist(img, 1, 1, m, n, chan));
    
    # layer 2
    hist_features <- c(hist_features, calHist(img, 1, 1, m1, n1, chan));
    hist_features <- c(hist_features, calHist(img, 1, n1 + 1, m1, n, chan));
    hist_features <- c(hist_features, calHist(img, m1 + 1, 1, m, n1, chan));
    hist_features <- c(hist_features, calHist(img, m1 + 1, n1 + 1, m, n, chan));
    
    # layer 3
    hist_features <- c(hist_features, calHist(img, 1, 1, m2, n2, chan));
    hist_features <- c(hist_features, calHist(img, 1, n2 + 1, m2, n1, chan));
    hist_features <- c(hist_features, calHist(img, m2 + 1, 1, m1, n2, chan));
    hist_features <- c(hist_features, calHist(img, m2 + 1, n2 + 1, m1, n1, chan));
    
    hist_features <- c(hist_features, calHist(img, 1, n1 + 1, m2, n1 + n2, chan));
    hist_features <- c(hist_features, calHist(img, 1, n1 + n2 + 1, m2, n, chan));
    hist_features <- c(hist_features, calHist(img, m2 + 1, n1 + 1, m1, n1 + n2, chan));
    hist_features <- c(hist_features, calHist(img, m2 + 1, n1 + n2 + 1, m1, n, chan));
    
    hist_features <- c(hist_features, calHist(img, m1 + 1, 1, m1 + m2, n2, chan));
    hist_features <- c(hist_features, calHist(img, m1 + 1, n2 + 1, m2, n1, chan));
    hist_features <- c(hist_features, calHist(img, m1 + m2 + 1, 1, m, n2, chan));
    hist_features <- c(hist_features, calHist(img, m1 + m2 + 1, n2 + 1, m, n1, chan));
    
    hist_features <- c(hist_features, calHist(img, m1 + 1, n1 + 1, m1 + m2, n1 + n2, chan));
    hist_features <- c(hist_features, calHist(img, m1 + 1, n1 + n2 + 1, m1 + m2, n, chan));
    hist_features <- c(hist_features, calHist(img, m1 + m2 + 1, n1 + 1, m, n2, chan));
    hist_features <- c(hist_features, calHist(img, m1 + m2 + 1, n1 + n2 + 1, m, n, chan));
  }
  
  return(as.vector(hist_features))
}
