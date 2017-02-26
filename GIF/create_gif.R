source("../missR.R")
missdata <- read.csv("../challenge_data.csv", 
                     encoding="UTF-8", 
                     stringsAsFactors = F)


NC <- ncol(missdata); NR <- nrow(missdata)
# visualize a random face
faces <- missdata[,19:NC]
I <- sample(1:NR, replace=FALSE)

jpeg("miss%02d.jpg", height = 300, width=300)
for (i in I) {
  face <- faces[i,]
  DrawFace(face,newdev=F)
}
dev.off()

# use command line and imageMagick:
# cd dropbox/missit/gif
# convert -delay 12 miss*.jpg missit.gif

#cleanup
unlink("miss*.jpg")
