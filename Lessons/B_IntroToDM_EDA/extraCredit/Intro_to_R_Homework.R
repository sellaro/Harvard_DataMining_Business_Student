#' Author: Ted Kwartler
#' Data: 9-2-2020
#' Student: Al Sellaro <sellaro@sellaro.co>
#' Assignment: EDA, Functions, visuals & mapping
#' Instructions: Complete the scaffolded code for Canvas.
 
## Set the working directory (HINT: it should be your wk2_homework folder)
setwd("~/SynologyDrive/Harvard/DataMiningBusiness/repo/Harvard_DataMining_Business_Student/Lessons/B_IntroToDM_EDA/extraCredit")

## Load the libraries, maps ggplot2, ggthemes
library(maps)
library(ggplot2)
library(ggthemes)
library(maps)
library(mapproj)

## Exercises
# 1. Read in diamonds.csv data and call it 'df'
df <-read.csv("diamonds.csv")

# 2. Examine the first 10 rows of data
head(df, 10)

# 3. What is the first value for the 'color' column when looking at head()? 
# Answer: E 

# 4. Create a new data frame called 'diamonds' by sorting the 'df' object by price and decreasing is FALSE
diamonds <- df[order(df$price, decreasing=F),]

# 5. Examine the last 6 rows by calling 'tail()' on the 'diamonds' data frame.  What is the most expensive diamond in the set?
tail(diamonds, 6)
# Answer: Diamond at line 27750, price is 18823

# 6. Copy and paste the results of the 'summary()' stats for the 'caret' attribute below.  You can use either $ or the index to get the vector
# > summary(diamonds$carat)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2000  0.4000  0.7000  0.7979  1.0400  5.0100 

# Introducing additional functions to try out, don't worry these are straight forward:
?max
?min
?range

# 7. What is the maximum value for the "depth" attribute?
# Answer: 79

# 8. What is the minimum value for the "table" attribute?
# Answer: 43

# 9. What is the range of values for the "price" column?
# Answer: 326 18823

# 10. Find the 347th diamond in the data set using row indexing.  Copy paste the single row below.
#> diamonds[347,]
#     carat   cut color clarity depth table price    x    y    z
#6707   0.3 Ideal     J     VS1  62.2    57   411 4.27 4.28 2.66

# 11. Create a barplot of the diamonds' cut and name it barplot.jpg
# ...creating the summary table
cutTally <- as.matrix(table(diamonds$cut))
# ...inspecting before reordering it
head(cutTally)
# ...reordering the table
cutTally <- cutTally[order(cutTally, decreasing=T),]
# ...re-inspecting it
head(cutTally)
barplot(cutTally, main="Diamond Cuts")

# 12. Create a ggplot scatterplot of points with the following aesthetics:
# color = clarity
# x axis = carat
# y axis = price
# point size size = 0.25
# theme = theme_economist_white()
# legend = none
ggplot(diamonds, 
       aes(colour=clarity)) + # data frame then aesthetics
  geom_point(aes(x = carat, y = price), size=0.25) + #add layer of segments & declare x/y 
  theme_economist_white() + #add a default "theme"
  theme(legend.position="none") # turn off the need for a legend



# 13. Examine the price distribution by creating a ggplot geom_histogram() instead of a scatter plot layer.  Use the code scaffold below with the following parameters:
# data = diamonds
# type = geom_histogram
# x = price (we are only examining a single attribute here)
# bin width = 100

ggplot(data=diamonds) + geom_histogram(aes(x=price), binwidth=100)


#14. What is the class() of the carat vector?  HINT: apply class() as a function to the carat column using $ or index number
# Answer: Numeric
class(diamonds$carat)

#15. What is the class of the color vector?
# Answer: character
class(diamonds$color)

#16. Read in the WesternCellTowers.csv cell towers as westTowers
westTowers <- read.csv("WesternCellTowers.csv")

#17. Using map() create a state map of 'oregon', 'utah', 'nevada', 'california' & add points() with westtowers$lon,westtowers$lat, col='blue'
westTowers$state <- trimws(westTowers$state, which='both')
westTowers[westTowers$state %in% c("OR","UT", "NV", "CA"), ]
map('state', region = c('oregon', 'utah', 'nevada', 'california'))
points(westTowers$lon,westTowers$lat, col='blue')

#18. Load the county map data called counties (HINT: with map_data)
counties <- map_data('county')

#19. Load the state data called state 
allStates <- map_data('state')

#20. Subset counties and allStates into the objects below; add the last states, california & nevada to the search index
westernCounties <- counties[counties$region %in% c("oregon","utah", "nevada", "california") ,]
westernStates   <- allStates[allStates$region %in% c("oregon","utah", "nevada", "california") ,]


#21. Create a ggplot map of the cell phone towers in the 4 western states.  Refer to the lesson's example code.
westernStates <- fortify(westernStates, region = 'region')
westernStates <-ggplot() + geom_map(data  =  westernStates, map = westernStates,
                         aes(x = long, y = lat, map_id = region, group = group),
                         fill = 'white', color = 'black', size = 0.25) +
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()
westernStates
westernStates + 
  geom_point(data=westTowers, aes(x=lon, y=lat), color='red', alpha=0.15)

# End 