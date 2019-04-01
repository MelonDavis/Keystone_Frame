#------master overview-----

#call master folder
#[i need to sort the master data folder so this function is usable]
master.folder <- paste()

#list subfolders
subfolders <- list.files(master.folder)

#names data that will be extracted
names.overview <- paste("overview.", subfolders, sep = "")

# enter the data.raw folder and loop through different folders in it
for(i in 1:length(subfolders)){
 
  #i <- 1
  #make path to each folder
  p.t <- paste(master.folder , subfolders[i], sep = "" )
  #list files in the folder
  list.files.subf <- list.files(p.t)
  
  # make temp data storage file
  d.t <- data.frame(matrix(ncol = 5, nrow = length(list.files.subf)))
  colnames(d.t) <- c("name","date","mean", "min", "max")
  str(d.t)
  d.t$name <- list.files.subf
  
  # go into subfolder to do analyses and write it to d.t
  for(j in 1:length(list.files.subf)){
    # j <- 5
    ma.IR <- readflirJPG(paste(p.t, "/", list.files.subf[j], sep = ""), 
                         exiftool.p)
    
    d.t$date[j] <- substr( file.mtime(paste(p.t,"/",list.files.subf[j], 
                                            sep = "")), 12, 19)
    d.t$mean[j] <- mean(ma.IR)
    d.t$min[j] <- min(ma.IR)
    d.t$max[j] <- max(ma.IR)
  }
  # assign to the ith file overview name
  assign(names.overview[i], d.t)
  write.csv(d.t, paste(p.raw.data, names.overview.files[i], ".csv", sep = ""), 
            row.names = FALSE)
  # the end 
}

#-----claire rename-----

master.folder <- paste("/Users/melina/Documents/KeystoneFrame/Claire/", 
                       sep = "")

#list subfolders
subfolders <- list.files(master.folder)

#this isn't the cleanest but i just need to rename these folders
#the function used to manually tell me positions of annotated files so i can 
  #rename them appropriately once the visual files have been renamed in batch

f.annpos <- function(subfolder) {


#begin by determining position of annotated images
#determine what positions the originals of vis.ann have in their folders

orig.t <- list.files(paste(master.folder , subfolder, "/visual",
                               sep = ""))
ann.orig <- list.files(paste(master.folder , subfolder, "/vis.ann",
                             sep = ""))


#save those positions
for (i in 1:length(ann.orig)) {
  
  output.name <- grep(ann.orig[i], orig.t)
  print(output.name)
}
}

#now that I know what positions they each have, i can rename all the visual 
  #folders

subfolders[2:7]

# enter the data.raw folder and loop through different folders in it
for(i in 2:7){
  
  #make path to each folder, in this one there are same three subfolders w/i
    #each subfolder; each gets named differently
  
  #thermal
  f.rename(paste(master.folder , subfolders[i], "/thermal", sep = "" ), 
          "E60.therm")
  
  #now that the positions are saved, we can rename visual
  f.rename(paste(master.folder , subfolders[i], "/visual", sep = "" ), 
          "E60.vis")
  
}

#renamed m.13 manually to DJI

#vis. annotation rename function
  #input must be formated like f.vis.ann("subfolder", c([positions]))

f.vis.ann <- function(subfolder, ann.pos) {

orig.t <- list.files(paste(master.folder , subfolder, "/visual",
                           sep = ""))
ann.orig <- list.files(paste(master.folder , subfolder, "/vis.ann",
                             sep = ""))

replacement.pos <- ann.pos

for(i in 1:length(ann.orig)) {
  
  file.rename(paste(master.folder, subfolder, "/vis.ann/", ann.orig[i], 
                    sep = ""), paste(master.folder, subfolder, "/vis.ann/", 
                                     orig.t[replacement.pos[i]], sep = ""))
  
}

}

f.annpos("f2.initial") #1, 3, 13, 14, 27, 34
f.annpos("m1.rfl") #1, 2, 5
f.annpos("m9.est") #4, 5
f.annpos("m12.fsa") #4, 5, 6, 9, 10
f.annpos("m13.est") #5, 7, 9, 17, 18, 21, 30, 31, 32, 38
f.annpos("m16.qst") #1, 2, 4, 5, 6, 9, 10, 12, 13, 14, 16, 17, 20, 23, 26, 27, 
#28, 19

#works! time to farmer code haha
f.vis.ann("f2.initial", c(1, 3, 13, 14, 27, 34))
f.vis.ann("m1.rfl", c(1, 2, 5))
f.vis.ann("m9.est", c(4, 5))
f.vis.ann("m12.fsa", c(4, 5, 6, 9, 10))
f.vis.ann("m13.est", c(5, 7, 9, 17, 18, 21, 30, 31, 32, 38))
f.vis.ann("m16.qst", c(1, 2, 4, 5, 6, 9, 10, 12, 13, 14, 16, 17, 20, 23, 26, 
                       27))


#generate time stamps for each

#call master folder
#[i need to sort the master data folder so this function is usable]
master.folder <- paste("/Users/melina/Documents/KeystoneFrame/Claire/")

#list subfolders
subfolders <- list.files(master.folder)
subfolders[8]

names.overview <- paste("overview.", subfolders, sep = "")

for(i in 4:9){
  
  i <- 8
  #make path to each folder
  p.t <- paste(master.folder , subfolders[i], "/thermal", sep = "" )
  #list files in the folder
  list.files.subf <- list.files(p.t)
  
  # make temp data storage file
  d.t <- data.frame(matrix(ncol = 2, nrow = length(list.files.subf)))
  colnames(d.t) <- c("name", "date")
  str(d.t)
  d.t$name <- list.files.subf
  
  # go into subfolder to do analyses and write it to d.t
  for(j in 1:length(list.files.subf)){
    
    d.t$date[j] <- substr(file.mtime(paste(p.t,"/",list.files.subf[j], 
                                            sep = "")), 12, 19)
    
  }
  # assign to the ith file overview name
  assign(names.overview[i], d.t)
  write.csv(d.t, paste(master.folder, subfolders[i], "/", 
                       names.overview[i], ".csv", sep = ""), 
            row.names = FALSE)
  # the end 
}

#looks like it worked!

#adjusting the time stamps: f2 and m1 are 13 hours behind; m.9 is on time; m12 
  #+ m13 + m16 are one hour behind

#for f2

d.t <- read.csv(paste(master.folder, subfolders[4], "/", names.overview[4], 
                      ".csv", sep = ""), stringsAsFactors = FALSE)

for (j in 1:nrow(d.t)) {
  
  #add one hour and save in column 3
  d.t[j,3] <- paste(as.numeric(substr(d.t[j,2], 1, 2)) + 13, 
                    substr(d.t[j,2], 3, 8), sep = "")
  
}

write.csv(d.t, paste(master.folder, subfolders[4], "/", 
                     names.overview[4] , ".csv", sep = ""), 
          row.names = FALSE)

#for m1

d.t <- read.csv(paste(master.folder, subfolders[5], "/", names.overview[5], 
                      ".csv", sep = ""), stringsAsFactors = FALSE)

for (j in 1:nrow(d.t)) {
  #if over 20 minus 11 to set right time
  if(as.numeric(substr(d.t[j,2], 1, 2) > 20))
  d.t[j,3] <- paste(as.numeric(substr(d.t[j,2], 1, 2)) - 11, 
                    substr(d.t[j,2], 3, 8), sep = "")
  
  
  #if less than 20 add 13
  if(as.numeric(substr(d.t[j,2], 1, 2) < 20))
    d.t[j,3] <- paste(as.numeric(substr(d.t[j,2], 1, 2)) + 13, 
                      substr(d.t[j,2], 3, 8), sep = "")
  
  if(as.numeric(substr(d.t[j,3], 1, 2) > 20))
    d.t[j,3] <- "NA"
  
}

write.csv(d.t, paste(master.folder, subfolders[5], "/", 
                     names.overview[5] , ".csv", sep = ""), 
          row.names = FALSE)

#for the three files that are one hour behind:

subfolders[6:8] #the relevent folders

for(i in 6:8) {
  i <- 8
  
  #call csv to temp save
  d.t <- read.csv(paste(master.folder, subfolders[i], "/", 
               names.overview[i], ".csv", sep = ""), stringsAsFactors = FALSE)
  
  for (j in 1:nrow(d.t)) {
    
      #add one hour and save in column 3
      d.t[j,3] <- paste(as.numeric(substr(d.t[j,2], 1, 2)) + 1, 
                        substr(d.t[j,2], 3, 8), sep = "")
    
  }
  
  #write temp file into a new csv
  write.csv(d.t, paste(master.folder, subfolders[i], "/", 
                       names.overview[i] , ".csv", sep = ""), 
            row.names = FALSE)
  
}

#-----weather org.-----

#now to match relevent weather data and then the directory will be ready to go!

m18weath <- read.csv(paste(p.raw, "weather", "/", "m18.csv", sep = ""), 
                     stringsAsFactors = FALSE, 
                     strip.white = TRUE, na.strings = c("NA",""))

m18weath <- m18weath[,-1]
m18weath <- m18weath[,-8]
head(m18weath)

for (i in 15:nrow(m18weath)) {

  m18weath[i, 2] <- substr(m18weath[i, 1], 9, 14)
  m18weath[i, 1] <- substr(m18weath[i, 1], 1, 8)
  
}

for (i in 1:14) {
  
  m18weath[i, 2] <- substr(m18weath[i, 1], 9, 14)
  m18weath[i, 1] <- substr(m18weath[i, 1], 1, 8)
  
}

write.csv(m18weath, paste(working.dir, "/", "clean.data/march/",
                     "m18weath.csv", sep = ""), 
          row.names = FALSE)

#only have from the 12th onwards, only relevent for m13 & m16
  #want to average from last photo taken to 12 hours previous

#translate F˚ into C˚
m18weath[, 8] <- (m18weath$Temp..F - 32) * 5/9
colnames(m18weath) <- c("Date","Time.GMT", "Solar.Radiation", "Rain.in", 
                        "Temp.F", "Rel.Humidity", "Wind.speed", "Temp.C")

#m13 - ended at 10:07:26; row 13 = m12 @ 22:00, row 25 = m13 # 10:00
m13.weath <- m18weath[13:25,]

summary(m13weath$Temp.C)
mean(m13weathh$Solar.Radiation)     

write.csv(m13.weath, paste(working.dir, "/", "clean.data/march/",
                                    "m13weath.csv", sep = ""), 
                    row.names = FALSE)

#m16 - ended at 15:59:54; row 103 = m16 @ 16:00; 103 - 12 = 91 -> m16 @ 4:00
m16.weath <- m18weath[91:103,]
summary(m16.weath$Temp.C)
mean(m16.weath$Rel.Humidity)
summary(m16.weath$Solar.Radiation)

write.csv(m16.weath, paste(working.dir, "/", "clean.data/march/",
                           "m16weath.csv", sep = ""), 
          row.names = FALSE)



