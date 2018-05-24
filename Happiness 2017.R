
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(caTools)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(data.table)
library(tidyr)
library(corrgram)       
library(corrplot)
library(formattable)
library(cowplot)
library(ggpubr)


# World happiness report 2017
Happiness <- read.csv("2017.csv")

str(Happiness)

# Changing the name of columns
colnames (Happiness) <- c("Country", "Happiness.Rank", "Happiness.Score",
                          "Whisker.High", "Whisker.Low", "Economy", "Family",
                          "Life.Expectancy", "Freedom", "Generosity",
                          "Trust", "Dystopia.Residual")


# Country: Name of countries
# Happiness.Rank: Rank of the country based on the Happiness Score
# Happiness.Score: Happiness measurement on a scale of 0 to 10
# Whisker.High: Upper confidence interval of happiness score
# Whisker.Low: Lower confidence interval of happiness score
# Economy: The value of all final goods and services produced within a nation in a given year
# Per capita GDP is a measure of the total output of a country that takes 
# the gross domestic product (GDP) and divides it by the number of people in 
#that country. The per capita GDP is especially useful when comparing one 
#country to another, because it shows the relative performance of the countries.
# Family: Importance of having a family
# Life.Expectancy: Importance of health and amount of time prople expect to live
# Freedom: Importance of freedom in each country
# Generosity: The quality of being kind and generous
# Trust: Perception of corruption in a government
# Dystopia.Residual: Plays as a reference

# Deleting unnecessary columns (Whisker.high and Whisker.low)

Happiness <- Happiness[, -c(4,5)]

# Creating a new column for continents

Happiness$Continent <- NA

Happiness$Continent[which(Happiness$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                   "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                   "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                   "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                   "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                   "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                   "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
Happiness$Continent[which(Happiness$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                   "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                   "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                   "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                   "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                   "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                   "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                   "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                   "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
Happiness$Continent[which(Happiness$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                   "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                   "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                   "Haiti"))] <- "North America"
Happiness$Continent[which(Happiness$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                   "Colombia", "Ecuador", "Bolivia", "Peru",
                                   "Paraguay", "Venezuela"))] <- "South America"
Happiness$Continent[which(Happiness$Country %in% c("New Zealand", "Australia"))] <- "Australia"
Happiness$Continent[which(is.na(Happiness$Continent))] <- "Africa"


# Moving the continent column's position in the dataset to the second column

Happiness <- Happiness %>% select(Country,Continent, everything())

# Changing Continent column to factor

Happiness$Continent <- as.factor(Happiness$Continent)

str(Happiness)
########## Correlation between variables

# Finding the correlation between numerical columns
Num.cols <- sapply(Happiness, is.numeric)
Cor.data <- cor(Happiness[, Num.cols])

corrplot(Cor.data, method = 'color')      

corrgram(Happiness, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)      

### Obviously there is an inverse correlation between "Happiness Rank"  and all the other
# numerical variables. In other words, the lower the happiness rank, the higher the happiness score,
# and the higher the other seven factors that contribute to happiness.

# So let's remove Happiness rank, and see the correlation again.

###########
# Plot matrix of all variables
newdata = Happiness[,c(4:11)]
plot(newdata, pch=16, col="blue", main="Matrix Scatterplot")

# Plot a correlation graph
newdatacor = cor(Happiness[c(4:11)])
corrplot(newdatacor, method = "number")

######
Rounded_correlation <- round(cor(Happiness[, 4:11], use="pair"),2)

corrgram(Happiness[, 4:11], order=TRUE,
         main="Happiness correlation matrix",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(Happiness[, 4:11], order=TRUE,
         main="Happiness correlation ellipses",
         panel=panel.ellipse,
         text.panel=panel.txt, diag.panel=panel.minmax)

corrgram(Happiness[, 4:11],
         main="Happiness with example panel functions",
         lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)

corrgram(Happiness[, 4:11], order=TRUE,
         main="Happiness data",
         lower.panel=corrgram::panel.ellipse,
         upper.panel=panel.bar, diag.panel=panel.minmax,
         col.regions=colorRampPalette(c("darkgoldenrod4", "burlywood1",
                                        "darkkhaki", "darkgreen")))


corrgram(Happiness[, 4:11], order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix")

## According to the corrplots, these three factors play a significant role in contributing to
# happpiness: Economy, life expectancy, family
# Economy > Life.Expectancy > Family > Freedom > Dystopia.Residual > Trust > Generosity


########## Let's calculate the average happiness score and 
# the average of the other seven variables for each continent

Happiness.Continent <- Happiness %>%
                          select(-3) %>%
                          group_by(Continent) %>%
                          summarise_at(vars(-Country), funs(mean(., na.rm=TRUE)))

# Or we can use aggregate
# aggregate(Happiness[, 4:11], list(Happiness$Continent), mean)

# Melting the "Happiness.Continent" dataset
Happiness.Continent.melt <- melt(Happiness.Continent)


# Faceting
ggplot(Happiness.Continent.melt, aes(y=value, x=Continent, color=Continent, fill=Continent)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of happiness variables for different continents", 
       y = "Average value") 

# We can see that Australia has the highest average in all fields except dystopia residual, after that 
# Europe, North America, and South America are roughly the same regarding 
# happiness score and the other seven factors. Finally, Asia and Africa have the
# lowest scores in all fields.

##### Let's see the correlation between variables for each continent

Var <- c("Continent", "Trust","Economy","Family","Life.Expectancy","Freedom","Generosity",
          "Happiness.Score","Dystopia.Residual")

corrgram(Happiness[,Var] %>% filter(Continent == "Africa"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Africa")
# Correlation between "Happiness Score" and the other variables in Africa:
# Economy > Family > Life.Expectancy > Dystopia.Residual > Freedom
# There is no correlation between happiness score and trust.
# There is an inverse correlation between happiness score and generosity.

corrgram(Happiness %>% select(-3) %>% filter(Continent == "Asia"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Asia")
# Correlation between "Happiness Score" and the other variables in Asia:
# Economy > Family > Life.Expectancy > Freedom > Trust > Dystopia.Residual
# There is no correlation between happiness score and generosity.

corrgram(Happiness %>% select(-3) %>% filter(Continent == "Europe"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Europe")
# Correlation between "Happiness Score" and the other variables in Europe:
# Freedom > Trust > Economy > Family > Dystopia.Residual > Life.Expectancy > Generosity

corrgram(Happiness %>% select(-3) %>% filter(Continent == "North America"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for North America")
# Correlation between "Happiness Score" and the other variables in North America:
# Life.Expectancy > Economy > Freedom > Family > Dystopia.Residual > Trust
# There is an inverse correlation between happiness score and generosity.

corrgram(Happiness %>% select(-3) %>% filter(Continent == "South America"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for South America")
# Correlation between "Happiness Score" and the other variables in South America:
# Dystopia.Residual > Economy > Life.Expectancy > Freedom > Generosity > Trust > Family




####### Happiness ranking for each continent
gg1 <- ggplot(Happiness,
              aes(x=Continent,
                  y=Happiness.Score,
                  color=Continent))+
  geom_point() + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8))) +
  labs(title = "Scatter plot")


gg2 <- ggplot(Happiness , aes(x = Continent, y = Happiness.Score)) +
  geom_boxplot(aes(fill=Continent)) + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8))) +
  labs(title = "Box plot")

gg3 <- ggplot(Happiness,aes(x=Continent,y=Happiness.Score))+
  geom_violin(aes(fill=Continent),alpha=0.7)+ theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8))) +
  labs(title = "Violin plot")

# Compute descriptive statistics by groups
stable <- desc_statby(Happiness, measure.var = "Happiness.Score",
                      grps = "Continent")
stable <- stable[, c("Continent","mean","median")]
names(stable) <- c("Continent", "Mean of happiness score","Median of happiness score")
# Summary table plot
stable.p <- ggtexttable(stable,rows = NULL, 
                         theme = ttheme("classic"))

ggarrange(gg1, gg2, ncol = 1, nrow = 2)
ggarrange(gg3, stable.p, ncol = 1, nrow = 2)

                    


#####################
################# Scatter plot with regression line
### The correlation between happiness score and the other seven factors
# in the happiness dataset for different continents


ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Life.Expectancy, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(Happiness, aes(x = Economy, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(Happiness, aes(x = Freedom, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(Happiness, aes(x = Trust, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(Happiness, aes(x = Generosity, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(Happiness, aes(x = Family, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(Happiness, aes(x = Dystopia.Residual, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

#####################

# Scatter plot colored by groups ("Continent")
#::::::::::::::::::::::::::::Generosity::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Generosity", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Generosity, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Generosity); xmax <- max(Happiness$Generosity)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
#::::::::::::::::::::::::::::Family::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Family", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Family, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Family); xmax <- max(Happiness$Family)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
#::::::::::::::::::::::::::::Life.Expectancy::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Life.Expectancy", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Life.Expectancy, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Life.Expectancy); xmax <- max(Happiness$Life.Expectancy)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
#::::::::::::::::::::::::::::Freedom::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Freedom", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Freedom, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Freedom); xmax <- max(Happiness$Freedom)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
#::::::::::::::::::::::::::::Economy::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Economy", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Economy, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Economy); xmax <- max(Happiness$Economy)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
#::::::::::::::::::::::::::::Trust::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Trust", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Trust, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Trust); xmax <- max(Happiness$Trust)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
#::::::::::::::::::::::::::::Dystopia.Residual::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Dystopia.Residual", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Dystopia.Residual, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Dystopia.Residual); xmax <- max(Happiness$Dystopia.Residual)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)


##################################
######## 3D Plot
library(plot3D)

scatter3D(Happiness$Freedom, Happiness$Life.Expectancy, Happiness$Happiness.Score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness data", xlab = "Freedom",
          ylab ="Life.Expectancy", zlab = "Happiness.Score")

scatter3D(Happiness$Generosity, Happiness$Economy, Happiness$Happiness.Score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness data", xlab = "Generosity",
          ylab ="Economy", zlab = "Happiness.Score")

scatter3D(Happiness$Trust, Happiness$Freedom, Happiness$Happiness.Score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness data", xlab = "Trust",
          ylab ="Freedom", zlab = "Happiness.Score")

scatter3D(Happiness$Family, Happiness$Economy, Happiness$Happiness.Score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness data", xlab = "Trust",
          ylab ="Economy", zlab = "Happiness.Score")


######################
################################ Machine Learning

####

# Splitting the dataset into the Training set and Test set

set.seed(123)
dataset <- Happiness[4:11]
split = sample.split(dataset$Happiness.Score, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


############# Multiple Linear Regression

# Fitting Multiple Linear Regression to the Training set
regressor_lm = lm(formula = Happiness.Score ~ .,
               data = training_set)
summary(regressor_lm)
####### Predicting the Test set results
y_pred_lm = predict(regressor_lm, newdata = test_set)

Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Happiness.Score))

gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
gg.lm

MSE.lm <- sum((test_set$Happiness.Score - y_pred_lm)^2)/nrow(test_set)


####################

############################### SVR

# Fitting SVR to the dataset
library(e1071)
regressor_svr = svm(formula = Happiness.Score ~ .,
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')



# Predicting a new result
y_pred_svr = predict(regressor_svr,  newdata = test_set)

Pred_Actual_svr <- as.data.frame(cbind(Prediction = y_pred_svr, Actual = test_set$Happiness.Score))


Pred_Actual_lm.versus.svr <- cbind(Prediction.lm = y_pred_lm, Prediction.svr = y_pred_svr, Actual = test_set$Happiness.Score)


gg.svr <- ggplot(Pred_Actual_svr, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "SVR", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

gg.svr
################################## Decision Tree Regression

library(rpart)
regressor_dt = rpart(formula = Happiness.Score ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 10))

# Predicting a new result with Decision Tree Regression
y_pred_dt = predict(regressor_dt, newdata = test_set)

Pred_Actual_dt <- as.data.frame(cbind(Prediction = y_pred_dt, Actual = test_set$Happiness.Score))


gg.dt <- ggplot(Pred_Actual_dt, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Decision Tree Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))


# Plotting the tree
library(rpart.plot)
prp(regressor_dt)

gg.dt
########################################## Random Forest Regression

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor_rf = randomForest(x = dataset[-1],
                         y = dataset$Happiness.Score,
                         ntree = 500)

# Predicting a new result with Random Forest Regression
y_pred_rf = predict(regressor_rf, newdata = test_set)

Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$Happiness.Score))


gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

gg.rf

# Randorm forest did a better job than decision tree.

################################################## Neural Net

library(neuralnet)

nn <- neuralnet(Happiness.Score ~ Economy + Family + Life.Expectancy + Freedom + Generosity + Trust + Dystopia.Residual,
                data=training_set,hidden=10,linear.output=TRUE)


plot(nn)

predicted.nn.values <- compute(nn,test_set[,2:8])

Pred_Actual_nn <- as.data.frame(cbind(Prediction = predicted.nn.values$net.result, Actual = test_set$Happiness.Score))

gg.nn <- ggplot(Pred_Actual_nn, aes(Actual, V1 )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Neural Net", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
      axis.title = element_text(family = "Helvetica", size = (10)))
    
gg.nn
MSE.nn <- sum((test_set$Happiness.Score - predicted.nn.values$net.result)^2)/nrow(test_set)


print(paste(MSE.lm,MSE.nn))

print(paste("Mean Squared Error (Multiple Linear Regression):", MSE.lm))
print(paste("Mean Squared Error (Neural Net):", MSE.nn))
######## Real versus predicted for different machine learning algorithms

ggarrange(gg.lm, gg.svr, gg.dt, gg.rf, gg.nn, ncol = 2, nrow = 3)

# Multiple Linear regression is the best predictor. After that, Neural Net do the best job.
# SVR and Random Forest predicted pretty the same.
# Finally Decision Tree is the worst predictor for this specific dataset.




