#Run this line to rename the files in a fashion of
# 1 ~ # of files

file.rename(from = list.files(pattern = "png"),
            to = paste(seq(1:length(list.files(pattern = "png"))), ".png",
                       sep = ""))