#m21.inside
#first test of the dummies; this was the juvi stellars jay (alias: bby)

#in future i need to make sure not to rename any csv files with therm data
f.rename(paste(p.raw, "m21.in", sep =""), "E60")

#----cam.data----

#call master folder
master.folder <- paste(working.dir, "/raw.data", sep = "")

#list subfolders
subfolders <- list.files(master.folder)

#names data that will be extracted
names.overview <- paste("overview.", subfolders, sep = "")
subfolders[]

# enter the data.raw folder and loop through different folders in it
for(i in 1:length(subfolders)){
  
  i <- 10
  #make path to each folder
  p.t <- paste(master.folder , "/", subfolders[i], sep = "" )
  #list files in the folder
  list.files.subf <- list.files(p.t)
  
  # make temp data storage file
  d.t <- data.frame(matrix(ncol = 4, nrow = length(list.files.subf)))
  colnames(d.t) <- c("name","time","model_signal", "model_temp")
  str(d.t)
  d.t$name <- list.files.subf
  
  # go into subfolder to do analyses and write it to d.t
  for(j in 25:length(list.files.subf)){
    #j <- 13
    ma.IR <- readflirJPG(paste(p.t, "/", list.files.subf[j], sep = ""), 
                         exiftool.p)
    
    d.t$time[j] <- substr( file.mtime(paste(p.t,"/",list.files.subf[j], 
                                            sep = "")), 12, 19)
    
    d.t$model_temp[j] <- mean(ma.IR[((nrow(ma.IR)/2) - 2):((nrow(ma.IR)/2) + 2),
                                    ((ncol(ma.IR)/2) - 2):((ncol(ma.IR)/2) + 2)])
    
  }
  
  # assign to the ith file overview name
  assign(names.overview[10], d.t)
  write.csv(d.t, paste(p.cl, "march/", names.overview.files[i], ".csv", sep = ""), 
            row.names = FALSE)
  # the end 
}

#i've found out if the file is edited within preview it is no longer 
  #accessible through the readflir JPG function (at least not without some
  #tweaking)

#now converting raw signal to temperature values

for (i in 1:nrow(overview.m21.in)) {
  
  overview.m21.in$model_temp[i] <- 
    raw2temp(overview.m21.in$model_temp[i], E = 0.95, OD = 6.2,
             PR1 = E60_set$Info$PlanckR1, PB = E60_set$Info$PlanckB, 
             PF = E60_set$Info$PlanckF, PO = E60_set$Info$PlanckO, 
             PR2 = E60_set$Info$PlanckR2)
  
  if(is.na(overview.m21.in$model_signal[i]) == TRUE)
    overview.m21.in$model_temp[i] <- NA
}

#create clean dataframe to merge
m21.cam <- data.frame(matrix(ncol = 3, nrow = nrow(overview.m21.in)))
colnames(m21.cam) <- c("name","time","model_temp")

for(i in 1:nrow(overview.m21.in)) {
  
  m21.cam$name[i] <- overview.m21.in$name[i]
  m21.cam$model_temp[i] <- overview.m21.in$model_temp[i]
  
  #now for time so it matches the log
  m21.cam$time[i] <- substr(overview.m21.in$time[i], 1, 5)
  
}

#----thermologger----

m21.logger <- read.csv(paste(p.raw, "thermlogger/m21.in.csv", sep = ""), 
         stringsAsFactors = FALSE)

#make a time column
for(i in 1:nrow(m21.logger)) {
  
  hours <- c(12, 13, 14, 15)
  minutes[i] <- as.numeric(hour.set[i]) - 60
  
  if(i < 9)
    m21.logger$time[i] <- paste(hours[1], ":",
                                as.numeric(m21.logger$time.since.start[i]) + 20, 
                                sep = "")
  
  if(i >= 9)
    m21.logger$time[i] <- paste(hours[2], ":", 
                                as.numeric(m21.logger$time.since.start[i]) - 40, 
                                sep = "")
  
  if(i >= 21)
    m21.logger$time[i] <- paste(hours[3], ":", 
                                as.numeric(m21.logger$time.since.start[i]) - 100, 
                                sep = "")
  
  if(i >= 33)
    m21.logger$time[i] <- paste(hours[4], ":", 
                                as.numeric(m21.logger$time.since.start[i]) - 
                                  160, sep = "")
} 

#changing :0 and :5 to :00 and :05
m21.logger$time[grep(":0", m21.logger$time)] <- 
    paste(substr(m21.logger$time[grep(":0", m21.logger$time)], 1, 2), ":00", 
          sep = "")

#because there are :50 & :55 it doesn't work in this one, doing quick and dirty 
  #work around
temp <- c(10, 22, 34) #relevent rows

for(i in 1:3) { 
  
m21.logger$time[temp[i]] <- 
  paste(substr(m21.logger$time[temp[i]], 1, 2), ":05", 
        sep = "")

}

#----combining data----

head(m21.logger)
head(m21.cam)
?merge()  

#merge by time while keeping all x values of the logger data
m21.d <- merge(m21.logger, m21.cam, by = "time", all.x = TRUE)

write.csv(m21.d, paste(p.cl, "march/m21.d.csv", sep = ""))

#remove the NAs -> [i can't have them be removed i need to change them into
  #something that can be processed; otherwise the temporal scale is off]
for (i in 1:nrow(m21.d)) {
  
  if(is.na(m21.d$model_temp[i]) == TRUE)
    m21.d <- m21.d[- i,]
  
}

#visual representation

#define range
m21.d_yr <- range(0, m21.d$internal.temp, m21.d$outer.temp, m21.d$model_temp)
m21.d_xr <- range(0, nrow(m21.d))

plot(m21.d$internal.temp, type = "o", pch = NA, axes = FALSE, xlim = m21.d_xr, 
     ylim = m21.d_yr, ann = FALSE) 
lines(m21.d$outer.temp)
lines(m21.d$model_temp)

grep("20", m21.d$time)

axis(2, at = 10 * 0:6)
axis(1, at = 1 * 0:40, lwd.ticks = 0.2, labels = FALSE)
axis(1, at = c(0, 12, 20, 30), c(0, 1, 2, 3))
