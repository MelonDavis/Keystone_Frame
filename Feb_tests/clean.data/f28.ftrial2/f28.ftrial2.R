#---Initial rename function development-----

#start by generating the list of file names
image.names <- c(list.files(paste(p.raw, "f28.ftrial2", "/" , "E60" , sep = "")))

#create the file names into paths
for(i in 1:length(image.names)) {
  image.names[i] <- c(paste(p.raw, "f28.ftrial2", "/" , "E60", "/", 
                            image.names[i] , sep = ""))
  
}

#create a list of numbers the same as # of files to replace
image.number.list <- c(rep("NA", length(image.names)))

#paste that into a path and file names
for(i in 1:length(image.names)) {
  image.number.list[i] <- paste(p.raw, "f28.ftrial2", "/" , "E60" , "/" , "IR_", i, 
                                ".jpg", sep = "")
}

#now for every existing file name rename it with the simpler title
for(i in 1:length(image.names)) {
    file.rename(image.names[i], image.number.list[i])
}

#see main for function version

#----Reading & storing img data ----

#Code to automatically read, generate and store min max and mean temp values of
  #a series of FLIR jpegs

#brief reminder on the commands
ma.IR4 <- readflirJPG(paste(p.raw, "m3.outest", "/", "IR_4.jpg", sep = 
                               ""), exiftool.p)

heatmap(ma.IR4, Rowv = NA, Colv = NA, col = heat.colors (256), labCol = FALSE, 
        labRow = FALSE)

#first generate list of files names
im.names <- c(list.files(paste(p.raw, "f28.ftrial2", "/", "E60", sep = "")))

#store into column
f28.d <- data.frame(im.names)

#generate time stamps for second column
for (i in 1:length(im.names)) {

f28.d[i,2] <- substr(file.mtime(paste(p.raw, "f28.ftrial2", "/", "E60", "/", 
                                im.names[i], sep = "")), 12, 19)
}

#temp object to store matrices w/i loop
ma.IR <- "NA"

#create loop that generates, reads and stores info on flir jpegs
for(i in 1:length(im.names)) {
  
  ma.IR <- readflirJPG(paste(p.raw, "f28.ftrial2", "/", "E60", "/", im.names[i], 
                             sep = ""), exiftool.p)
  f28.d[i, 3] <- min(ma.IR)
  f28.d[i, 4] <- max(ma.IR)
  f28.d[i, 5] <- mean(ma.IR)
  
}




