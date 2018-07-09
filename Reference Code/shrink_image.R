library(EBImage)

getinto = function(filename){
  if(!dir.exists(filename)){
    print("The file is not found...")
    stop("Wrong directory...")
  }else{
    setwd(paste(getwd(),"/",filename,sep = ""))
    # print("The new directory is:")
    # getwd()
  }
}

getback = function(){
  wd = strsplit(getwd(),"/")
  wd = wd[[1]]
  if(length(wd)>1){
    newwd = wd[1:length(wd)-1]
    temwd = ""
    for(i in 1:length(newwd)){
      temwd = paste(temwd,newwd[i],sep = "/")
    }
    temwd = substring(temwd,2,nchar(temwd))
    setwd(temwd)
  }else{
    print("The current directory is already the mother directory...")
    warning("Unable to get back to previous directory...")
  }
  # print("The current directory is:")
  # getwd()
}

getinto("cropped")
#In order to get the picture with 1/(2^n) the pixel count,
#Input times as 2
#Default Value: Times = 2
shrink_pic = function(picture, times = 2){
  imageData(picture) = imageData(picture)[
    -seq(1,dim(imageData(picture))[1], times),
    -seq(1,dim(imageData(picture))[2], times),]
  return(picture)
}

#This function can double the length of the 
#Dimensions of the picture
enlarge_pic = function(picture){
  dimension = dim(imageData(picture))
  tempmat = matrix(-1, nrow = dimension[1] * 2,
                   ncol = dimension[2] * 2)
  for(i in 1:dimension[1]){
    for(j in 1:dimension[2]){
      tempmat[(2*i-1):(2*i), (2*j-1):(2*j)] = imageData(picture)[i,j]
    }
  }
  imageData(picture) = tempmat
  display(picture)
  return(picture)
}

if(length(list.files(pattern = "png")) == 0){
  cat("Please choose the folder with pictures: \n")
  setwd(choose.dir())
}

if(!file.exists("smaller")){
  dir.create("smaller")
}

getinto("cropped")

for(pic_path in list.files(pattern = "jpg")){
  picture = readImage(pic_path)
  picture = shrink_pic(picture, 2)
  getinto("smaller")
  writeImage(picture, paste("smaller", 
                            pic_path, 
                            sep = ""))
  getback()
}
getinto("smaller")
file.rename(from = list.files(pattern = "jpg"),
            to = paste("smaller_",
                       as.character(seq(1,length(list.files()))),
                       ".jpg",
                       sep = ""))
setwd(wd)
