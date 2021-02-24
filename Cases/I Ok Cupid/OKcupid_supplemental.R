#' Author: Al Sellaro
#' Date: 2-22-2011
#' Purpose: OKCupid Case Supplemental
#' 

# Libs
install.packages('okcupiddata')
library(okcupiddata)
library(dplyr)
library(maps)
library(ggplot2)
library(RColorBrewer)

# Set WD
setwd("~/SynologyDrive/Harvard/DataMiningBusiness/repo/Harvard_DataMining_Business_Student/Cases/I Ok Cupid")

# See all files in wd, leave this blank inside the parentheses
dir()

# Get the okcupid data as `profiles`
data('profiles')
latlon<- read.csv('LatLon.csv')

##### I would do some basic EDA and plotting of individual vars then move to more complex interactions
table(profiles$orientation)

# Age histogram
hist(profiles$age, main = "Age histogram")

# Orientation/Sex EDA
g_o_bar <- barplot(table(profiles$sex, profiles$orientation), 
        main="Orientation and sex distribution",
        col=c("lightblue","goldenrod2"), 
        legend = rownames(table(profiles$sex, profiles$orientation)))

##### Example 2 way EDA
table(profiles$age,profiles$orientation)

#### Missing in income & quick mean imputation example; you can still use vtreat instead to clean all this data but we are only exploring not modeling so maybe dont do it for this case.
sum(is.na(profiles$income))
profiles$income[is.na(profiles$income)] <- mean(profiles$income, na.rm=TRUE)

##### Feature Engineer relationship status & education if you thought there was a connection
profiles$statEDU <- paste(profiles$status, profiles$education, sep = '_')
statEduTbl <- table(profiles$statEDU)
statEduTbl <- sort(statEduTbl, decreasing = TRUE)
coul <- brewer.pal(5, "Set2") 
par(mar=c(20, 5, 3, 1))
statEdu <- barplot(statEduTbl[1:5], 
                   main="Status and education distribution", 
                   col=coul, las = 2)
text(statEdu, 0, round(statEduTbl[1:5], 1),cex=1,pos=3) 


##### Enrich with one of the new data sets, you may want to do this with the other csv
moreData <- left_join(profiles, latlon, by ='location')
head(moreData)
summary(profiles)

#### You can use complete.cases() to identify records without NA if that is the route you want to explore.  Of course you can use a function covered in class to visualize the variables with the hightest % of NA so you could drop those instead of all rows with an NA.
completeMoreData <- moreData[complete.cases(moreData),]

par(mar=c(5, 3, 3, 1))
g_o_bar <- barplot(table(completeMoreData$sex, completeMoreData$orientation),
                   main="Orientation and gender distribution",
                   col=c("lightblue","goldenrod2"), 
                   legend = rownames(table(completeMoreData$sex, completeMoreData$orientation)))


s_d <- table(completeMoreData$sex, completeMoreData$diet)
par(mar=c(8, 5, 3, 1))
s_d_bar <- barplot(s_d, 
                   main="Diet and gender",
                   col=c("lightblue","goldenrod2"), 
                   legend = rownames(s_d),
                   las = 2)

### Mapping
MainStates <- map_data("world")
gg <- ggplot() + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
              color="black", fill="lightblue") +
  geom_point( data=completeMoreData, 
              aes(x=lon, y=lat), color="red", size = 2, alpha=0.5)
gg
# End
