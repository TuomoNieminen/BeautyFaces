# Create the Miss Finland Challenge data by merging face and measure data sets
# Tuomo NIeminen 2016

rm(list=ls())

# helper function to format names in miss_faces
format_name <- function(name) {
  name <- gsub("*.png","",name)
  if(as.numeric(substr(name,1,4))<2016) {
    name <- gsub("^.*?_","",name)
  }
  name <- gsub("^.*?_","",name)
  name <- gsub("_"," ",name)
  return(tolower(name))
}


# face data
miss_faces <- get(load("missikuvat.Rda"))
miss_faces <- as.data.frame(miss_faces)
filenames <- rownames(miss_faces)
Encoding(filenames) <- "UTF-8"

miss_faces$year <- as.numeric(substr(filenames,1,4))
miss_faces$name <- sapply(filenames,format_name)

# sort by year and name
miss_faces <- miss_faces[with(miss_faces,order(year,name)),]

# save(file="miss_faces.Rda",miss_faces)
# face data
# miss_faces <- get(load("miss_faces.Rda"))

# other data
miss_measures<- read.table("missidata.txt",header=T, sep="\t",row.names=NULL, stringsAsFactors = F)
#miss_measures <- get(load("miss_measures.Rda"))
miss_measures <- subset(miss_measures, Vuosi >2006)
miss_measures$name <- tolower(miss_measures$Nimi)
miss_measures$year <- miss_measures$Vuosi

# sort by year and name
miss_measures <- miss_measures[with(miss_measures,order(year,name)),]

# remove unneccessary features
miss_measures <- miss_measures[,3:ncol(miss_measures)]
names(miss_measures)
miss_measures <- miss_measures[,c(1:8,10:12,18:ncol(miss_measures))]
names(miss_measures)

# save(file="miss_measures.Rda",miss_measures)
# miss_measures <- get(load("miss_measures.Rda"))


# check if names match
miss_faces$name[!miss_faces$name %in% miss_measures$name]
miss_measures$name[!miss_measures$name %in% miss_faces$name]

miss_faces$name[miss_faces$name=="essi poysti"] <- "essi pöysti"

miss_data <- merge(miss_measures, miss_faces, by=c("name","year"))

# sort by year and name
miss_data <- miss_data[with(miss_data,order(year,name)),]

dim(miss_data)
sum(is.na(miss_data))

save(file="challenge_data.Rda", miss_data)
write.csv(file="challenge_data.csv", 
          row.names=F, fileEncoding="UTF-8",
          miss_data)

