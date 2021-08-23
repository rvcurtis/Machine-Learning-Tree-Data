---
  title: "Project Group 9 Burlington Tree Analysis"
output: 
  html_document:
  toc: true
df_print:  paged 
---
  
  ```{r setup, include=FALSE, warning = FALSE}
knitr::opts_chunk$set(echo = TRUE)
# All import statements
library(tidyverse)
library(scales)
library(rpart)
library(rpart.plot)
```

## Introduction

This data set is a collection of statistics on trees found in the Burlington ares. This data set is available online at: https://data.burlingtonvt.gov/explore/dataset/planting-sites/information/. This data was gathered by observational study, as no experiments were performed using this data. There are roughly ~12,800 entries in this data set. We are exploring what variable is most important in deciding the appraisal value of a tree here in Burlington. The motivation for this stems from the high prices for some of the trees in the data set (some over $40,000). We do not believe that there is any bias in this data set, but we do believe the missing entries for some rows will introduce bias in out conclusion. We find this data interesting because it tells us a lot of new information about the city we live in. Not much cleaning was required, but we will have to deal with the missing values in the data set. 

```{r Tree Exploration, warning  = FALSE}
# Reading in and exploring the trees data set. 
trees <- read.csv("newTrees.csv",stringsAsFactors = TRUE)
tibble(trees)
summary(trees)
# Exploring the appraise variables further 
summary(trees$appraise)
summary(trees$landuse)
```

## Basic Plots

These plots look at some basic initial hypothesis that our group had. 

```{r cleaning some data, warning=FALSE, message = FALSE}
# removing all entries with an appraisal value of zero (Represents unappraised trees)
trees <- trees %>% filter(appraise > 0)
trees <- trees %>% filter(zone_id != "")
trees <- trees %>% filter(landuse != "unknown" & landuse != "")
```


```{r Tree hist, warning = FALSE, message = FALSE}
# displaying a histogram of appraisal value. This will allow for us to easily appraise.  
ggplot(data = trees, mapping = aes(x = appraise)) +
  geom_histogram( fill =  'lightblue', col = 'black') +
  labs(x = 'Appraisal value in dollars',
       y = 'Count',
       title = 'Histogram of Tree Appraisals in BVT') +
  theme_bw() +
  scale_x_sqrt()

```

The first graph explores the type of land the tree is on, and the resulting price of the tree. Looking at this graph no clear pattern is visible. The only clear observation that can be gained from this graph is that on average, trees found in cemetery land are appraised at higher values, but there are many outliers in this set. One explanation as to why cemeteries have more expensive trees could be due to most cemeteries being established many years ago, resulting in older trees on their land. 

```{r Plot1, warning = FALSE, message = FALSE}
# Basic plot of land use by tree appraisal value.
ggplot(trees, mapping = aes(x = reorder(landuse,appraise), y = appraise, color = landuse)) + 
  geom_boxplot() + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Appraisal Value by Landuse",x = "Landuse", y = "Appraisal Value") + 
  scale_y_log10(labels = scales::dollar_format())
```

The second graph looks at the diameter of the tree and the corresponding appraisal value. This graph luckily gives us more insight than the first, as we see a clear positive relationship of tree diameter and tree price. However, this graph still leaves us with questions. As there do exist smaller diameter trees that are appraised for more than larger diameter trees, which tells us there are exogenous factors we need to consider. To explore this further we created a graph with some restrictions on diameter, to further investigate the explanatory power diameter has on appraisal value. 

```{r Plot2, warning = FALSE, message = FALSE}
# Basic plot of species by tree appraisal value
trees <- trees %>% filter(trees$landuse != "industrial" & trees$landuse != "street" & trees$landuse != "vacant lot" & trees$landuse != "church")
# Plot showing appraisal value  by unrestricted diameter
ggplot(trees, mapping = aes(x = diameter, y = appraise, color = zone_id)) + 
  geom_point(size = .1) + 
  theme_classic() + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  facet_wrap(~landuse) + 
  theme(legend.text = element_text(size = 8), legend.key.size = unit(2,"line"))
```

This third graph still looks at diameter vs appraisal value, but with the diameter domain restricted to trees having a diameter over 25. Analyzing this new graph we see that diameter definitely is not the most important, or at the very least only, variable that concludes how much a tree should be appraised for. As there exists trees that are have smaller diameters but are being appraised for a higher value. 

```{r Plot3}
# Restrict diameter to get a better look
newDiameter <- trees %>% filter(diameter > 25)
# Plot showing appraisal value by restricted diameter 
ggplot(newDiameter, mapping = aes(x = diameter, y = appraise)) + 
  geom_point() + 
  theme_classic() + 
  scale_y_continuous(labels = scales::dollar_format()) + 
  labs(title = "Appraisal Value by Tree Diameter",
       x = 'Diameter',
       y = 'Appraisal Value') +
  geom_smooth(method = 'loess', se = FALSE)
```

The fourth graph explores the relationship between height and appraisal value, this graph again shows us that this a complex problem, as the intuitive thought that bigger trees are more expensive, does not hold up. This graph also shows us that there are many similarly sized trees throughout Burlington, which leads to the conclusion that those trees were planted roughly at the same time. How much does Age effect this situation? 
  
  ```{r Plot4}
# Filter out trees that are less than 25 feet tall
newHeight <- trees %>% filter(height > 25)
# Plot showing appraisal value by Height
ggplot(newHeight, mapping = aes(x = height, y = appraise)) + 
  geom_point() + 
  labs(title = "Appraisal Value by Tree Height", y = "Appraisal Value", x = "Tree Height in Feet") + 
  theme_classic() + 
  scale_y_continuous(labels = scales::dollar_format())
```

This is the question we attempted answer with our fifth graph. This age graph shows us that unfortunately, many of the trees in Burlington do not have their ages strictly defined. This could be for many reasons, they could be planted by individuals who never noted the date. Or it could also be  so this problems exploration is severely limited in this context. We do however see a rough pattern of old trees being more expensive. 

```{r Plot5}
# Filter out the entries where planted = 0
Planted <- trees %>% filter(planted > 0)
ggplot(Planted, mapping = aes(x = planted, y = appraise)) + 
  geom_point() + 
  labs(title = "Appraisal Value by Year Planted", x = "Year Planted", y = "Appraisal Value")+
  theme_classic() + 
  scale_y_continuous(labels = scales::dollar_format())
```


## Advanced Plots

For a more advanced plot, we look at the distribution of trees throughout Burlington. We have created an interactive map using the leaflet package to visually represent the distribution. Though this graph doesn't directly connect to our main goal of this project, we thought it would be insightful to see how the trees are distributed throughout Burlington.

```{r Advanced Plots, warning = FALSE, message = FALSE}
# Using a leaflet map, this plot shows the distribution of trees in the Burlington area
trees <- trees %>% drop_na()
library(leaflet)
library(rgdal)
#leaflet map
treesMap <- leaflet() %>% 
  addTiles() %>% 
  setView(lat = 44.475, lng = -73.212, zoom = 12)
pallet <- colorBin("Greens", trees$ward_id)
treesMap %>% addCircles(lat = trees$lat,
                 lng = trees$long,radius = .1, color = pallet(trees$site_id), stroke = TRUE)
```

### Limitations & Recomendations
The main limitations with this project so far has been the features of the data set. We can only explore the relationships of appraisal and the different columns available. With more features of tree in the data set we could explore even more relationships and how those new features affect the appraisal value. I would recommend adding some categorical variables to this data set, such as: Tree type, Tree health, etc. Adding these variables would us to explore this problem deeper.


### Regression model
We created a regression model to predict the appraise value of a tree. We supplied diameter, height, spread, trunks, and condition to the model to help predict this value. 

Looking at the summary of our model, we see an adjusted R^2 score of .859
``` {r Regression model, warning = FALSE}
# Creating a regression model for our model
model <- lm(appraise ~ diameter + height + spread + trunks + conditn, data = trees)
summary(model)
trees$pred <- predict(model, trees)
cor(trees$pred, trees$appraise)
# plot it
plot1 <- plot(trees$pred, trees$appraise) + abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)
```

### Regression Tree

Here we implemented a regression tree model, which as we can see from the visualization, unfortunately did not provide a lot of explanatory power behind the model's prediction of appraisal value aside from the diameter of the tree. We can see that at most of the decision making stages, some measure of diameter is used to make it's decision. 
```{r regression tree, warning = FALSE}
# regression tree analysis
# create a vector of the columns we would like to drop for the analysis
names <- c("Geo.Point","site_id","modified","lat","long")
# we need to drop the lat and long values for this analysis
trees <- subset(trees,select = -c(Geo.Point,site_id,modified,lat,long,species,park,pred))
tibble(trees)
# need to split the data into training and testing
dt = sort(sample(nrow(trees), nrow(trees)*.8))
tree_train<-trees[dt,]
tree_test<-trees[-dt,]
# now the data is split into 80 and 20 for training and testing
m.rpart <- rpart(appraise ~ ., data = tree_train)
m.rpart
reg_tree_plot <- rpart.plot(m.rpart, digits = 4,
           fallen.leaves = TRUE,
           type = 5, extra = 101,
           box.palette = 'BlGnYl',
           shadow.col = 'gray',
           cex = .5)
reg_tree_plot
p.rpart <- predict(m.rpart, tree_test)
# compare the distribution of predicted values vs. 
#  actual values
summary(p.rpart)
summary(trees$appraise)
# correlation for the two:
cor(p.rpart,tree_test$appraise)
# Regression Tree Analysis
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}
# calculate the MAE
MAE3 <- MAE(p.rpart,tree_test$appraise)
MAE3
# print a summary of the score values to show the distribution
summary(tree_train$appraise)
mean(tree_train$appraise)
# now let's use this average to calculate the MAE using this
MAE4 <- MAE(3326.775,tree_test$appraise)
MAE3 - MAE4
```
### Conclusion

From working with this tree data, it is apparent that there does seem to be some factors that go into the appraisal of trees in Burlington. Our original goal for this project was to determine which variable had the greatest influence on appraisal value, and by looking at the results from our graphs/models, it appears that the information we have gained makes our work inconclusive, although we can draw from the graphs and modeling, that the physical characteristics of the tree(height, diameter, trunks) did play a role in the determination of the tree's appraisal value. Geographic characteristics such as the zone id on the other hand, provided less explanatory power than those physical characteristics. All of the variables that we looked at in our study all had similar distributions when plotted against appraisal value. We believe that if our data set had categorical data such as the tree type/health, it would have made our results a little more conclusive when implementing models such as the multiple regression model and regression trees.
