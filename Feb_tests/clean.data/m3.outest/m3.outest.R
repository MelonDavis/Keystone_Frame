#m3.outest; dawn tests day 1; 

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
          for(i in 1:length(im.names)) {
            
            ma.IR <- readflirJPG(paste(p.raw, "m3.outest", "/", im.names[i], sep = ""), exiftool.p)
            
            m3.d[i, 3] <- min(ma.IR)
            m3.d[i, 4] <- max(ma.IR)
            m3.d[i, 5] <- mean(ma.IR)
            
          }
          
colnames(m3.d) <- c("im.names", "time", "min", "max", "mean")


