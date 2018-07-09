####################################################
#Convert CSV
#Author Wanjun Gu
#July 6 2018
#Convert image files (png or jpg) to CSV files
#Please make sure to set to working 
#Directory to the folder with images
####################################################

library(EBImage)

#Determine file name
filename = "large_images.csv"
#filename = "small_images.csv"

#Can take pictures of format png or jpg
filelist = c(list.files(pattern = "png"),
             list.files(pattern = "jpg"))

for(pic_path in filelist){
  pic = imageData(readImage(pic_path))
  red = as.vector(pic[,,1])
  green = as.vector(pic[,,2])
  blue = as.vector(pic[,,3])
  pic = c(red, green, blue)
  if(pic_path == filelist[1]){
    picture_mat = pic
  }else{
    picture_mat = rbind(picture_mat, pic)
  }
  cat(paste(pic_path, " loaded...\n", sep = ""))
}

cat("\n")
cat(paste(dim(picture_mat)[1], "pictures loaded...\n"))

cat("Saving as csv file...\n")
write.csv(picture_mat, file = filename)