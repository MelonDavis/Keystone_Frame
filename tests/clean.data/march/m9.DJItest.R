#m9.DJItest

#tried to use the staplr package but it's pdfs only

m9.vis <- data.frame(list.files(paste(p.raw, "m9.DJItest1", "/", "visual", 
                                      sep = "")))

m9.therm <- list.files(paste(p.raw, "m9.DJItest1", "/", "100MEDIA", sep = ""))

#[they have different amount of photos and i don't know why womp wompp]
length(m9.therm)
nrow(m9.vis)

m9.vis[,2] <- m9.therm[-1]
