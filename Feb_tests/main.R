#Main

#===== [SYSTEM] =======

R.version.string
#"R version 3.5.1 (2018 - 07 - 02)"

#create object of working directory
working.dir <- getwd()

#===== [PACKAGES] ======

install.packages("Thermimage")
#call thermimage library
library(Thermimage)
#set the path for where exiftool exists
exiftool.p <- paste("/usr", "local", "bin/", sep = "/")
#check it works
system2(paste(exiftool.p, "exiftool", sep = "/"))

#generate camera settings
E60_set <- flirsettings(paste(p.raw, "f2.outside", "/", "FLIR1286.jpg", 
                              sep = ""), exiftool.p, camvals = NULL)
DJI_set <- flirsettings(paste(p.raw, "f22.ftrial", "/", "DJI_0010.jpg", 
                              sep = ""), exiftool.p, camvals = NULL)

install.packages("imager")
library(imager)

#===== [FILE MANAGEMENT] =====

#stored names of folders into an object
output.folder.names <- c("raw.data", "clean.data", "analysis")

#for loop combo with if function that asks whether the each position of 
#output.folder.names exists as a file and if it's true that it doesn't exist
#it will create it.
for(i in 1:length(output.folder.names)) {
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])
}

#Make path names for each folder

#First concenate empty object to store paths
p.final <- c(rep(NA, 3)) 

#for loop that generates path for each folder and assigns path to corresponding
#position in paths.proto
for (i in 1:length(output.folder.names)) {
  p.final[i] <- paste(working.dir, "/", output.folder.names[i], "/", 
                      sep = "")
  
} 
#paths.bigdata should now contain each pathway stored in the same order as 
#output.folder.names.

print(p.final)
#1 = raw; 2 = clean; 3 = analysis
p.raw <- p.final[1]
p.cl <- p.final[2]
p.ana <- p.final[3]
#now it can be refered to through direct object or placement in p.final
#easier off the top of my head to insert path name as individual object

#=====[FUNCTIONS] ======

#----Renaming snapshots generated from phone----

f.rename <- function(dir.path, prefix) {
  
  #start by generating the list of file names
  image.names <- c(list.files(dir.path))
  
  #create the file names into paths
  for(i in 1:length(image.names)) {
    image.names[i] <- c(paste(dir.path, "/", 
                              image.names[i] , sep = ""))
    
  }
  
  #create a list of numbers the same as # of files to replace
  image.number.list <- c(rep("NA", length(image.names)))
  
  #paste that into a path and file names
  for(i in 1:length(image.names)) {
    
    image.number.list[i] <- paste(dir.path , "/" , prefix , "_000", i, 
                                  ".jpg", sep = "")
    if(i > 9) 
      (image.number.list[i] <- paste(dir.path , "/" , prefix , "_00", i, 
                                     ".jpg", sep = ""))
    if(i > 99)
      (image.number.list[i] <- paste(dir.path , "/" , prefix , "_0", i, 
                                     ".jpg", sep = ""))
    
    if(i > 999)
      (image.number.list[i] <- paste(dir.path , "/" , prefix , "_", i, 
                                     ".jpg", sep = ""))
  }
  
  
  #now for every existing file name rename it with the simpler title
  for(i in 1:length(image.names)) {
    file.rename(image.names[i], image.number.list[i])
  }
  
}

f.rename(paste(p.raw, "test", sep = ""), "IR")


#input must be f.rename(pathname, "prefix")
#path name = e.x.: paste(p.raw, "f28.ftrial2", "/" , "E60" , sep = "")
#should call folder in which files are placed
#"prefix" = e.x.: "IR", "DJI", will go at the begenning of every image 
#followed by underscore



#----basic stats on series of jpegs----

m3.d <- data.frame("NA")

#[i can't get output.name to become a data.frame so it stores properly]

f.sumflir <- function(dir.path, output.name) {
  
  #first generate list of files names
  im.names <- c(list.files(paste(p.raw, "m3.outest", sep = "")))
  
  #store into column
  m3.d <- data.frame(im.names)
  
  #generate time stamps for second column
  for (i in 1:length(im.names)) {
    
    m3.d[i,2] <- substr(file.mtime(paste(p.raw, "m3.outest", "/", 
                                         im.names[i], sep = "")), 12, 19)
  }
  
  #temp object to store matrices w/i loop
  ma.IR <- ("NA")
  
  #create loop that generates, reads and stores info on flir jpegs
  for(i in 1:10) {
    
    ma.IR <- readflirJPG(paste(p.raw, "m3.outest", "/", im.names[1], sep = ""), exiftool.p)
    
    output.name[i, 3] <- min(ma.IR)
    output.name[i, 4] <- max(ma.IR)
    output.name[i, 5] <- mean(ma.IR)
    
  }
  
}

f.sumflir(paste(p.raw, "m3.outest", sep = ""), m3.d)
