
#-----data.logger-----

#creating minute column for my datalogger data sheet 
met_min.p <- paste(p.cl, "met_min.csv", sep = "")

met_min <- seq(0, 59, by = 1)
met_min <- rep(met_min, each = 4)
write.csv(met_min, met_min.p)

#running first tests with the data that's w/i 5 seconds of image capture:

#import the thermal temperature data
thermtest.d <- read.csv(paste(p.raw, "f22.ftrial", "/", "thermtest.csv", sep = ""), stringsAsFactors = FALSE, 
         strip.white = TRUE)

#[optional: save plot]
#pdf(log_temp.p, width = 05, height = 03)

#define y range
time_r <- range(35, thermtest.d$T1, thermtest.d$T2)

#plot data
plot(thermtest.d$T1, type = "o", pch = NA, axes = FALSE, ylim = time_r, ann = FALSE)
lines(thermtest.d$T2)

#create x axis
#small tick for every 5 minutes
axis(1, at = 20 * 0:19.2, labels = FALSE, lwd.ticks = 0.5)
#large tick for every 30 min
axis(1, at = 120 * 0:3.2, labels = seq(0, 1.5, by = 0.5), lwd.ticks = 1.5)

#create y axis
axis(2, at = 5 * 0:max(thermtest.d), lwd.ticks = 0.5)

#labels
title(xlab = "Time", ylab = "Temperature (C˚)")

#dev.off()

#save path for the plot if u wanna save
log_temp.p <- paste(p.cl, "f22.ftrial" ,"log_temp.pdf", sep = "")

#-----imgs [initial mess]-----

#visualization of one file
ma.0009 <- readflirJPG(paste(p.raw, "f22.ftrial", "/", "DJI_0009.jpg", sep = 
                              ""), exiftool.p)
            
pdf(paste(p.cl, "misc", "/", "hm.0009.pdf", sep = ""), width = 03, height = 03)
 
heatmap(ma.0009, Rowv = NA, Colv = NA, col = heat.colors (256), labCol = FALSE, 
        labRow = FALSE)

dev.off()

#get a sense of what signal strength of the scene
max(ma.0009) #3930
min(ma.0009) #3435
mean(ma.0009) #3507.962

grep("3435", ma.0009) #7895

#I went to make this easier to deal with; I'll try taking the center and then 
  #making part of the standard procedure loop 
cen.0009 <- ma.0009[((nrow(ma.0009)/2) + 50):((nrow(ma.0009)/2) - 50),
                    ((ncol(ma.0009)/2) + 50):((ncol(ma.0009)/2) - 50)]

print(ma.0009[(nrow(ma.0009)/2), (ncol(ma.0009)/2)])

#+/- 30 is all around contains whole platform, good place to start
#nrow <- +6 / -4 & ncol <- +25 & - 40 captures object itself

nrow(cen.0009)
ncol(cen.0009)

#yay! it takes the center; I messed around and decided 
heatmap(cen.0009, Rowv = NA, Colv = NA, col = heat.colors (256), 
        scale = "column", labCol = FALSE, 
        labRow = FALSE)
#[how do add number label ticks]

#Stats might not be same as above given presence of logger & variance in pixels
max(cen.0009) #3930
min(cen.0009) #3479
mean(cen.0009) #3517.834
grep("3497", cen.0009)

#impression of colors relating to signal strength
head(cen.0009)
print(cen.0009[,1])
print(cen.0009[,11])

cen.0009[684] #max: 3497
ncol(cen.0009)
grep("3497", cen.0009[2,])
cen.0009[2,63] #has the max signal strength [??? ?? ? ?]
#[I don't understand how R plots the heat map]

#so far i want to store: overall mean temp, overall max/min temp, "object" mean 
  #temp, "object" max/min temp (object is in quotes because it'll really be from 
  #from a square tailored to the object)


#-----imgs [summary] -----

#we still doing f.imgsum manually womp womp

#first generate list of files names
im.names <- c(list.files(paste(p.raw, "f22.ftrial", sep = "")))

#sometimes there are other files, this step replaces them with NA
#it does require all file names to be three letters_four numbers
  #[rename IR to E60]

for (i in 1:length(im.names)){
  
  if(substr(im.names[i], 10, 12) != "jpg")
    (im.names[i] <- NA)
    
}

#store into column
f22.d <- data.frame(im.names)

#generate time stamps for second column
for (i in 1:length(im.names)) {
  
  f22.d[i,2] <- substr(file.mtime(paste(p.raw, "f22.ftrial", "/", 
                                       im.names[i], sep = "")), 12, 19)
}

#temp object to store matrices w/i loop
ma.IR <- ("NA")
cen.IR <- ("NA")

#create loop that generates, reads and stores info on flir jpegs
for(i in 1:length(im.names)) {
  
  ma.IR <- readflirJPG(paste(p.raw, "f22.ftrial", "/", im.names[i], 
                             sep = ""), exiftool.p)
  
  #for this test we want to specify a smaller area of the image
  cen.IR <- ma.IR[((nrow(ma.IR)/2) + 50):((nrow(ma.IR)/2) - 50),
                   ((ncol(ma.IR)/2) + 50):((ncol(ma.IR)/2) - 50)]
  
  f22.d[i, 3] <- min(cen.IR)
  f22.d[i, 4] <- max(cen.IR)
  f22.d[i, 5] <- mean(cen.IR)
  
  #I'm pretty sure for this trial, the center pixel is on the model (when that's 
    #not the case i need to find a standardized way to identify those pixels)
  f22.d[i, 6] <- mean(ma.IR[((nrow(ma.IR)/2) + 1):((nrow(ma.IR)/2) - 1),
                            ((ncol(ma.IR)/2) + 1):((ncol(ma.IR)/2) - 1)])
  
  
}

tail(f22.d)

#I want to build in part of the code that will automatically kick any NAs
#not sure if has to be number or if "length(im.names)" would work, nrow doesn't
for (i in 1:620) {

  if(is.na(f22.d$im_names[i]) == TRUE)
    (f22.d <- f22.d[-i,])
  
}

colnames(f22.d) <- c("im_names", "time", "cenmin", "cenmax", "cenmean", "cpxl")

#[really it becomes about defining how many pixels in the view should have a 
  #certain value (i.e: the model = 20 px so if theres 100 hot px something is 
  #going on); or even more generally, in a standard noisy scene there are this 
  #many px of this strength so if there is more we can identify an photo of 
  #interest; my question is next door neighbor ability]

f22.d_r <- range(f22.d$cenmean[5:614], f22.d$cenmax[5:614], f22.d$cpxl[5:614])

plot(f22.d$cenmax, type = "o", pch = NA, axes = FALSE, ylim = f22.d_r, 
     ann = FALSE)
lines(f22.d$cenmean)
lines(f22.d$cpxl)

summary(f22.d$cpxl)


#----checking extracted pixels----

#[I've realized since it's not useful to extract the pixel of the colored 
  #thermal image, however what this code might still be good for extraction of 
  #corresponding visual RGB pixels/ checking within the signal]


c.0010 <- load.image(paste(p.raw, "f22.ftrial", "/", "DJI_0010.jpg", sep = ""))
structure(c.0010)

#translate to one channel matrix 
m.0010 <- grayscale(c.0010) %>% as.matrix

nrow(m.0010)
ncol(m.0010)

#this is the center pixel
m.0010[168, 128] #0.67

#looks like it works!
heatmap(m.0010, Rowv = NA, Colv = NA, col = heat.colors (256), labCol = FALSE, 
        labRow = FALSE)

#and 0.13 is the mean, 0.0... gets into the black i think so 0.67 makes sense as 
  #the object (light grey)
mean(m.0010)

#removed the files that were before start time
f22.d <- f22.d[-1:-4,]
head(f22.d)

#so i want to run it and say maybe, if it > 0.5 warn me so I can check it
#[actually why don't I just print like the first 10-20 rows to see what it'll 
  #give me]

c.temp <- "NA"

for (i in 1:length(im.names)){
  
  c.temp <- load.image(paste(p.raw, "f22.ftrial", "/", im.names[i], 
                             sep = ""))
  
  m.temp <- grayscale(c.temp) %>% as.matrix
  
  f22.d[i,7] <- m.temp[168, 128]
  
}

#i wanna plot it but i need to make the scales comparable so take it 10 to the 4
f22.d[,8] <- f22.d$cpxl_col * 1000

#plotting to compare color value

f22.d_r <- range(f22.d$col2plot[5:614], f22.d$cpxl[5:614])

#good they do mirror each other which is what we want [there is a stats test to
  #confirm this i believe; the test that says how much variance in one data set
  #can be explained by the variance in another
plot(f22.d$col2plot, type = "o", pch = NA, axes = FALSE, ylim = f22.d_r, 
     ann = FALSE)
lines(f22.d$cpxl)

#the weird part in the begenning for the color value is because humans were in 
  #the frame which changed the color value (aka its a trash value to take in 
  #most cases); the good news is I think it is unnessacarry because the same
  #test/check function should run fine on the signal values; in fact it should 
  #run exactly the same or better

#really I should do a summary statistic to find out the quantile of noise in 
  #each graph; that can give a standard 
abline(h = mean(f22.d_r))

summary(f22.d$cpxl_col)

#[so we could say, if a color pixel falls below the 1st quartile lmk so I can 
  #check the photo w/ my eyeballs]

