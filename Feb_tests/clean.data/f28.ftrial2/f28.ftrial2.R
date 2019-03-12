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

#----E60 data ----

f.rename(paste(p.raw, "f28.ftrial2", "/", "E60", sep = ""), "E60")

#Doing the f.thermsum manually

#visualization of file so I know what I'm working with
ma.E_10 <- readflirJPG(paste(p.raw, "f28.ftrial2", "/", "E60", "/", 
                             "E60_0010.jpg", sep = ""), exiftool.p)

heatmap(cen.E_10, Rowv = NA, Colv = NA, col = heat.colors (256), labCol = FALSE, 
        labRow = FALSE)

cen.E_10 <- ma.E_10[((nrow(ma.E_10)/2) + 50):((nrow(ma.E_10)/2) - 50),
                ((ncol(ma.E_10)/2) + 50):((ncol(ma.E_10)/2) - 50)]

#first generate list of files names
im.names <- c(list.files(paste(p.raw, "f28.ftrial2", "/", "E60", sep = "")))

#store into column
f28_60s.d <- data.frame(im.names)

#generate time stamps for second column
for (i in 1:length(im.names)) {

f28_60s.d[i,2] <- substr(file.mtime(paste(p.raw, "f28.ftrial2", "/", "E60", "/", 
                                im.names[i], sep = "")), 12, 19)
}

#temp object to store matrices w/i loop
ma.IR <- "NA"

#create loop that generates, reads and stores info on flir jpegs
for(i in 1:length(im.names)) {
  
  ma.IR <- readflirJPG(paste(p.raw, "f28.ftrial2", "/", "E60", "/", im.names[i], 
                             sep = ""), exiftool.p)
  f28_60s.d[i, 3] <- min(ma.IR)
  f28_60s.d[i, 4] <- max(ma.IR)
  f28_60s.d[i, 5] <- mean(ma.IR)
  
  f28_60s.d[i, 6] <- mean(ma.IR[((nrow(ma.IR)/2) + 1):((nrow(ma.IR)/2) - 1),
                            ((ncol(ma.IR)/2) + 1):((ncol(ma.IR)/2) - 1)])
  
}

colnames(f28_60s) <- c("im_list", "time", "min", "max", "mean", "o_mean")

#[translate into a new frame of the temp signals]

f28_60t.d <- data.frame(im.names)

for (i in 1:length(im.names)) {

  
  #raw2temp(f28_60s.d[i, j], E = 0.95, OD = 5.03,
                    #PR1 = DJI_set$Info$PlanckR1, PB = DJI_set$Info$PlanckB, 
                    #PF = DJI_set$Info$PlanckF, PO = DJI_set$Info$PlanckO, 
                   # PR2 = DJI_set$Info$PlanckR2)
    f28_60t.d[i, 3] <- f28_60s.d[i, 3]
  
}



#----DJI data----

#visualization of file so I know what I'm working with
ma.D_639 <- readflirJPG(paste(p.raw, "f28.ftrial2", "/", "DJI_0639.jpg", 
                                sep = ""), exiftool.p)

heatmap(cen.D_639, Rowv = NA, Colv = NA, col = heat.colors (256), labCol = FALSE, 
        labRow = FALSE)

#played around until object is highlighted <- [how to automate?]
cen.D_639 <- ma.D_639[((nrow(ma.D_639)/2) - 34):((nrow(ma.D_639)/2) - 56),
                    ((ncol(ma.D_639)/2) - 12):((ncol(ma.D_639)/2) - 19)]

ma.D_639[98:68, 173:133]

#object horizontal temp average (middle row of object)
ma.D_639[83, 173:13]

#object vertical temp average (middle column of object)
ma.D_639[98:68, 153]

#in this scene the camera moved a bit & so did object [in future activiting 
  #center spot should help ; or again if some way to automate]
ma.D_999 <- readflirJPG(paste(p.raw, "f28.ftrial2", "/", "DJI_0999.jpg", 
                           sep = ""), exiftool.p)

#[womp womp] it's super off : O
cen.D_999 <- ma.D_999[104:62,166:139]

heatmap(cen.D_999, Rowv = NA, Colv = NA, col = heat.colors (256), labCol = FALSE, 
        labRow = FALSE)

#Doing the f.thermsum manually

#first generate list of files names
im.names <- c(list.files(paste(p.raw, "f28.ftrial2", "/", "E60", sep = "")))

#store into column
f28_60s.d <- data.frame(im.names)

#generate time stamps for second column
for (i in 1:length(im.names)) {
  
  f28_60s.d[i,2] <- substr(file.mtime(paste(p.raw, "f28.ftrial2", "/", "E60", "/", 
                                            im.names[i], sep = "")), 12, 19)
}

#temp object to store matrices w/i loop
ma.IR <- "NA"

#create loop that generates, reads and stores info on flir jpegs
for(i in 1:length(im.names)) {
  
  ma.IR <- readflirJPG(paste(p.raw, "f28.ftrial2", "/", "E60", "/", im.names[i], 
                             sep = ""), exiftool.p)
  f28_60s.d[i, 3] <- min(ma.IR)
  f28_60s.d[i, 4] <- max(ma.IR)
  f28_60s.d[i, 5] <- mean(ma.IR)
  
  f28_60s.d[i, 6] <- mean(ma.IR[((nrow(ma.IR)/2) + 1):((nrow(ma.IR)/2) - 1),
                                ((ncol(ma.IR)/2) + 1):((ncol(ma.IR)/2) - 1)])
  
}

colnames(f28_60s) <- c("im_list", "time", "min", "max", "mean", "o_mean")

