
wkdir <- getwd()

p.raw.data <- paste(wkdir, "/test/data.raw/", sep = "")

data.folders <- list.files(p.raw.data)
# make names for the data extracted from the files on the data folders
names.overview.files <- paste("overview.data.",data.folders, sep = "")


# enter the data.raw folder and loop through different folders in it
for(i in 1:length(data.folders)){
  # make a data frame to save extracted data
  # i <- 1
  #path to folder
  p.t <- paste(p.raw.data,data.folders[i], sep = "" )
  # files in the folder
  list.files.subf <- list.files(p.t)
  
  # make temp data storage file
  d.t <- data.frame(matrix(ncol = 6, nrow = length(list.files.subf)))
  colnames(d.t) <- c("name","date","mean", "min", "max", "centre")
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
  assign(names.overview.files[i], d.t)
  write.csv(d.t, paste(p.raw.data, names.overview.files[i], ".csv", sep = ""), 
            row.names = FALSE)
  # the end 
}


