#m3.outest; dawn tests day 1; 

f.rename(paste(p.raw, "m3.outest", sep = ""), "E60")

#still havent figured out function but i wanna look at the ata so we're just 
  #doing it

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
            
            ma.IR <- readflirJPG(paste(p.raw, "m3.outest", "/", im.names[i], 
                                       sep = ""), exiftool.p)
            
            m3.d[i, 3] <- min(ma.IR)
            m3.d[i, 4] <- max(ma.IR)
            m3.d[i, 5] <- mean(ma.IR)
            
          }
          
colnames(m3.d) <- c("im.names", "time", "min", "max", "mean")

for (i in 1:620) {
  
  if(is.na(m3.d$mean[i]) == TRUE)
    (m3.d <- m3.d[-i,])
  
}

m3.d_r <- range(m3.d$mean, m3.d$min, m3.d$max)

plot(m3.d$mean, type = "o", pch = NA, axes = FALSE, ylim = m3.d_r, 
     ann = FALSE)
lines(m3.d$min)
lines(m3.d$max)

write.csv(m3.d, file = paste(p.cl, "m3.outest", "/", "m3sum.csv", sep = ""))

#----m4.outest-----

#rename files
f.rename(paste(p.raw, "m4.outest", sep = ""), "E60")

#quick test on whether i could use a different import to get the photos from my 
  #phone; the answer is yes but it messes the time stamp up so I decided to 
  #stick with original method
m4.d <- readflirJPG(paste(p.raw, "m4.outest", "/", "IMG_3487.JPG", sep = ""), 
                     exiftool.p)
ncol(m4.d)
nrow(m4.d)

min(m4.d)
max(m4.d)
mean(m4.d)

#Doing the flirsum function manually
im.names <- c(list.files(paste(p.raw, "m4.outest", sep = "")))

#store into column
m4.d <- data.frame(im.names)

#generate time stamps for second column
for (i in 1:length(im.names)) {
  
  m4.d[i,2] <- substr(file.mtime(paste(p.raw, "m4.outest", "/", 
                                       im.names[i], sep = "")), 12, 19)
}

#temp object to store matrices w/i loop
ma.IR <- ("NA")

#create loop that generates, reads and stores info on flir jpegs
for(i in 1:length(im.names)) {
  
  ma.IR <- readflirJPG(paste(p.raw, "m4.outest", "/", im.names[i], 
                             sep = ""), exiftool.p)
  
  m4.d[i, 3] <- min(ma.IR)
  m4.d[i, 4] <- max(ma.IR)
  m4.d[i, 5] <- mean(ma.IR)
  
}

colnames(m4.d) <- c("im.names", "time", "min", "max", "mean")

?range()
m4.d_r <- range(m4.d$mean, m4.d$min, m4.d$max)

plot(m4.d$mean, type = "o", pch = NA, axes = FALSE, ylim = m4.d_r, 
     ann = FALSE)
lines(m4.d$max)
lines(m4.d$min)
