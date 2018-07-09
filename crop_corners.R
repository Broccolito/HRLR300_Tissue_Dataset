####################################################
#Image Crop
#Author Wanjun Gu
#July 6 2018
#From the original image to all 150*150 pixel images
#The working directory should be the directory
#With the images that you want to crop
####################################################

rm(list = ls())

#Loading packages needed
library(EBImage)
if(length(list.files(pattern = "png")) == 0){
  cat("Please choose the folder with pictures: \n")
  setwd(choose.dir())
}

wd = getwd()

#Rename the files in numbers
# file.rename(from = list.files(pattern = "png"),
#             to = paste(seq(1:length(list.files(pattern = "png"))), ".png",
#                        sep = ""))

#The dimension of each picture is 150*150
dimension = 150

#Create dir
if(!file.exists("cropped")){
  dir.create("cropped")
}

#Define file operation functions
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

#Crop the first corner of the images
for(pic_path in list.files(pattern = "png")){
  pic = readImage(pic_path)
  imageData(pic) = imageData(pic)[1:dimension,
                                  1:dimension,]
  getinto("cropped")
  writeImage(pic,files = paste("first_corner_cropped_",
                               pic_path,
                               sep = ""))
  getback()
  cat(paste(pic_path, " read\n"))
}

#Crop the second corner of the images
for(pic_path in list.files(pattern = "png")){
  pic = readImage(pic_path)
  imageData(pic) = imageData(pic)[(dim(pic)[1]+1-dimension):dim(pic)[1],
                                  1:dimension,]
  getinto("cropped")
  writeImage(pic,files = paste("second_corner_cropped_",
                               pic_path,
                               sep = ""))
  getback()
  cat(paste(pic_path, " read\n"))
}

#Crop the third corner of the images
for(pic_path in list.files(pattern = "png")){
  pic = readImage(pic_path)
  imageData(pic) = imageData(pic)[1:dimension,
                                  (dim(pic)[2]+1-dimension):dim(pic)[2],]
  getinto("cropped")
  writeImage(pic,files = paste("third_corner_cropped_",
                               pic_path,
                               sep = ""))
  getback()
  cat(paste(pic_path, " read\n"))
}

#Crop the fourth corner of the images
for(pic_path in list.files(pattern = "png")){
  pic = readImage(pic_path)
  imageData(pic) = imageData(pic)[(dim(pic)[1]+1-dimension):dim(pic)[1],
                                  (dim(pic)[2]+1-dimension):dim(pic)[2],]
  getinto("cropped")
  writeImage(pic,files = paste("fourth_corner_cropped_",
                               pic_path,
                               sep = ""))
  getback()
  cat(paste(pic_path, " read\n"))
}

#Rename the cropped images with numbers
getinto("cropped")
file.rename(from = list.files(pattern = "png"),
            to = paste(seq(1:length(list.files(pattern = "png"))), ".png",
                       sep = ""))
setwd(wd)
rm(list = ls())