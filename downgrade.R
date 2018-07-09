####################################################
#Downgrade
#Author Wanjun Gu
#July 6 2018
#Make sure the current working directory contains
#images that are cropped
####################################################

library(EBImage)

#Define File operation function
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

#Shrink the picture to half of its original size
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

if(!file.exists("low_res")){
  dir.create("low_res")
}

filelist = list.files(pattern = "png")
for(pic_path in filelist){
  pic = readImage(pic_path)
  pic = shrink_pic(pic)
  getinto("low_res")
  writeImage(pic, file = paste("low_res_", pic_path, sep = ""))
  getback()
}