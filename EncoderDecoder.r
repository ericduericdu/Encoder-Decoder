#Eric Du
#Joanne Wang
#Jeffrey Tai
#John Nguyen

library(pixmap)
arr <- NULL
cons <- NULL
nrows <- NULL
ncols <- NULL
message <- NULL
nstride <- NULL
occupied <- NULL
total_pixels <- NULL
Gimg <- NULL
Gm <- NULL
secretencoder <- function(imgfilename, msg, startpix, stride, consec) 
{
  occupied<<-NULL
  if(is.null(consec))
    img <- secretencoderNull(imgfilename, msg, startpix, stride, consec)
  else
    img <- secretencoderVal(imgfilename, msg, startpix, stride, consec)
  #Location of where we want to write the file to
    #Location of where we want to write the file to
  currdir <- getwd()
  currdir <- paste(currdir,"/encoded.pgm",sep="")
  #write.pnm(img,currdir)

  write.pnm(img, currdir, type="pgm")
  return(img)
}

secretencoderNull <- function(imgfilename, msg, startpix, stride, consec) {
  message <- strsplit(msg, "")[[1]] # Store the encoded message
  message <- sapply( message, function(message) utf8ToInt(message)/128)
  message<-c(message, 0)
  image <- read.pnm(imgfilename) # Process pgm file to utilize its methods
  arr <- image@grey # Extract pixel array from processed image
  
  size <- matrix(image@size)
  nrows <- size[1,]
  ncols <- size[2,]
  total_pixels <- nrows * ncols

  if(gcd(total_pixels, stride) != 1)
    warning("Stride and total pixels should be relatively prime or risk overwritten characters")
  
  pix_rep <- head(seq(startpix, startpix + (length(message) * stride), stride),-1)
  
  x <- sapply(pix_rep, function(x) x<-as.integer(x%%nrows) ) 
  x <- replace(x,x==0,nrows)
  y<-c(ifelse(pix_rep%%nrows==0,as.integer(pix_rep/nrows), as.integer(pix_rep/nrows+1)))

  y <- ifelse(y > ncols, as.integer(y%%ncols), y)

  
  cords <- cbind(x,y)
  # for (i in 1:length(message)) {
  #   arr[ cords[i,1], cords[i,2] ] <-  message[i]
  # }

  sapply(1:length(message), function(i, arr, cords, message) arr[ cords[i,1], cords[i,2] ] <<-  message[i], arr, cords, message)
  
  image2 <- image
  image2@grey <- arr

  return(image2)
}

secretencoderVal<-function(imgfilename, msg, startpix, stride, consec) 
{
  message <<- strsplit(msg, "")[[1]] # Store the encoded message
  message <<- sapply(message, function(message) utf8ToInt(message)/128) 
  message<<-c(message, 0)
  image <- read.pnm(imgfilename) # Process pgm file to utilize its methods
  arr <<- image@grey # Extract pixel array from processed image
  
  size <- matrix(image@size)
  nrows <<- size[1,]
  ncols <<- size[2,]
  #nrows<<-10
  #ncols<<-10
  cons <<-consec
  nstride<<-stride
  total_pixels <<- nrows * ncols

  if(gcd(total_pixels, nstride) != 1)
    warning("Stride and total pixels should be relatively prime or risk overwritten characters")

  pix_rep<-head(seq(startpix, startpix + (length(message) * stride), stride),-1)
  pix_rep<-ifelse(pix_rep < total_pixels, pix_rep, pix_rep%%total_pixels)
  pix_rep<-replace(pix_rep,pix_rep==0,total_pixels)

  if(anyDuplicated(pix_rep))
    stop("Condition results in overwritten characters")

  mapply(function(pix, i) place(pix, i), pix_rep, seq_along(1:length(message)))

  image2 <- image
  image2@grey <- arr

  return(image2)
}

place<-function(absPix, index)
{
  startingPix <- absPix
  placed <- FALSE
  placedPix <- absPix

  while(!placed)
  {
    if(consecPix((placedPix-1):(placedPix-cons), placedPix, TRUE) +
       consecPix((placedPix+1):(placedPix+cons), placedPix, TRUE) >= cons 
       ||
       consecPix(seq(placedPix-nrows, placedPix-nrows*cons,-nrows), placedPix, FALSE) +
       consecPix(seq(placedPix+nrows, placedPix+nrows*cons, nrows), placedPix, FALSE) >= cons 
       || 
       placedPix %in% occupied)
    {
      placedPix <- ifelse(((placedPix + nstride) %% total_pixels) == 0, placedPix + nstride, (placedPix + nstride) %% total_pixels)

      if(placedPix == startingPix)
        stop("Illegal attempt to override character(s)")
    }
    else
    {
      occupied<<-c(occupied, placedPix)
      x <- c(as.integer(placedPix%%nrows)) 
      x <- replace(x,x==0,nrows)
      y <- c(ifelse(placedPix%%nrows==0,as.integer(placedPix/nrows), as.integer(placedPix/nrows+1)))
      y <- ifelse(y > ncols, as.integer(y%%ncols), y)
      arr[x,y] <<-  message[index] #make global
      placed <- TRUE
    }
  }
}

consecPix<-function(sequence, absPix, belowAbove)
{
  count <- 0

  if(belowAbove == TRUE)
    for(i in sequence)
      ifelse(i %in% occupied, ifelse((sameCol(i, absPix)), count <- count + 1, count<-count+0), break)
  else #leftRight
    for(i in sequence)
      ifelse(i %in% occupied, count<-count+1, break)

  return(count)

  # if(belowAbove == TRUE)
  #   sapply(sequence, function(x) ifelse(x %in% occupied && count != -1, ifelse(sameCol(x, absPix), count <<- count+1, count <<- count+0), count<<--1))
  # else
  #   sapply(sequence, function(x) ifelse(x %in% occupied && count != -1, count<<-count+1, count<<- -1))

  # ifelse(count == -1, return(0), return(count))
}

sameCol<-function(i, absPix)
{
  icol <- c(ifelse(i%%nrows==0, as.integer(i/nrows), as.integer(i/nrows+1)))
  icol <- ifelse(icol > ncols, as.integer(icol%%ncols), icol)
      
  absPixcol <- c(ifelse(absPix%%nrows==0, as.integer(absPix/nrows), as.integer(absPix/nrows+1)))
  absPixcol  <- ifelse(absPixcol > ncols, as.integer(absPixcol%%ncols), absPixcol)

  return(icol==absPixcol)
}

gcd <- function(x,y)
{
  gcd <- 0
  ifelse(x>y, smaller <- y, smaller <- x)
  
  for(i in 1:smaller)
    if((x%% i == 0) && (y%% i == 0))
      gcd<- i

  return(gcd)
}


####Decodes################



generate <- function(nr,nc,len,consec,stride,startpix,img){
  m <- cbind()
  next_st <- startpix
  ud <- seq(1,consec)
  lr <- seq(nc,len,by=nc*consec)
  lr <- lr[1:consec]
  done = FALSE
  while(length(m) < length(Gimg)){ #Need to change msglen to something
    
    for(i in  seq(next_st,length(Gimg),by=stride)){
      if(i == 0) next
      direc <- cbind(i-ud,i+ud,  i-lr ,i+lr)
      cols <- matrix(
        sapply(direc, 
               function(x){
                 ceiling(x/nr) 
               }), nr,nc)
      bool <- matrix(is.element(direc,m),nr ,nc ) 
      bool <- sapply(1:2,function(x) ifelse(all(bool[,x]==TRUE),x,F)) 
      bool <- which(bool != 0)
      col_num <- length(unique(cols[,bool])) 
      if(col_num == 1 | i < 0)    next
      m <- cbind(m,i)
      
      if(any(Gimg[i] == 0)){
        done = TRUE
        break
      }
    }

    if(done) break
    next_st <- m[length(m)] - len
    #if(next_st == 0) next_st <- 1
    
  }
  #Debug
  #Gm <<- m
  return(m)
}

generateNULL <- function(nr,nc,len,str,sp){
  pix_rep <- head(seq(sp, sp + (len), str),-1)
  
  x <- sapply(pix_rep, function(x) x<-as.integer(x%%nr) ) 
  x <- replace(x,x==0,nr)
  y<-c(ifelse(pix_rep%%nr==0,as.integer(pix_rep/nr), as.integer(pix_rep/nr+1)))
  y <- ifelse(y > nc, as.integer(y%%nc), y)
  
  return(cbind(x,y))
}

secretdecoder <- function(imgfilename,startpix,stride,consec=NULL){
  image <- read.pnm(imgfilename) # Process pgm file to utilize its methods
  img <- image@grey # Extract pixel array from processed image
  #Debug

  #img <- imgfilename@grey
  Gimg <<- matrix(img) 
  #End of debug
  
  nr <- nrow(img)
  nc <- ncol(img)
  
  
  
  if(is.null(consec)){
    se <- generateNULL(nr,nc,length(img),stride,startpix) 
    end <- which(img[se]==0) 
    msg <- img[se[1:end]]
  }
  else{
    img <- matrix(img)
    se <- generate(nr,nc,length(img),consec,stride,startpix)
    msg <- img[se,]
    
  }
  msg <- msg*128
  msg <- msg[ (32<= msg & msg <= 47) | (58 <= msg & msg <= 126)]
  msg <- ifelse((58 <= msg & msg <= 126), msg+1, msg) 
  msg <- sapply(msg, function(x) intToUtf8(x))
  
  msg <- paste(msg, collapse = '')
  return(msg)
}

#secretencoder("/users/ericdu/desktop/school/winter 2017/ecs 145/hw3/part1/LL.pgm","a",3,1,2)

#secretdecoder("/users/ericdu/desktop/school/winter 2017/ecs 145/hw3/part1/encoded.pgm",3,1,2)