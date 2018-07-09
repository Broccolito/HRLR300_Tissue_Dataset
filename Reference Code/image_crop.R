####################################################
#Image Crop
#From the original image to all 100*100 pixel images
####################################################

library(EBImage)
if(length(list.files(pattern = "png")) == 0){
  cat("Please choose the folder with pictures: \n")
  setwd(choose.dir())
}

wd = getwd()

dimension = 150

if(!file.exists("cropped")){
  dir.create("cropped")
}

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

for(pic_path in list.files(pattern = "png")){
  pic = readImage(pic_path)
  imageData(pic) = imageData(pic)[1:dimension,1:dimension,]
  getinto("cropped")
  writeImage(pic,files = paste("cropped_",
                               pic_path,
                               sep = ""))
  getback()
}
