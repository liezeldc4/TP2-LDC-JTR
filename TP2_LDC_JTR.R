library(tidyverse) 
dataset=read.csv('covid_worldwide.csv')


# Explore the data
view(dataset)
head(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

# Convert columns from character to numeric
dataset$Total.Cases = as.numeric(gsub(",", "", dataset$Total.Cases))                         # Print updated example vector
glimpse(dataset)
ggplot(data=dataset,aes(Total.Cases))+
  geom_histogram()

dataset$Total.Deaths = as.numeric(gsub(",", "", dataset$Total.Deaths))                       # Print updated example vector
glimpse(dataset)
ggplot(data=datset,aes(Total.Deaths))+
  geom_histogram()

dataset$Population = as.numeric(gsub(",", "", dataset$Population))                           # Print updated example vector
glimpse(dataset)
ggplot(data=dataset,aes(Population))+
  geom_histogram()

# Impute Total Cases, Total Deaths, and Population with Median
totalcases_median = median(dataset$Total.Cases, na.rm = TRUE)
dataset$Total.Cases = ifelse(is.na(dataset$Total.Cases), totalcases_median, dataset$Total.Cases)
colSums(is.na(dataset))

totalcases_median = median(dataset$Total.Cases, na.rm = TRUE)
dataset$Total.Cases = ifelse(is.na(dataset$Total.Cases), totalcases_median, dataset$Total.Cases)
colSums(is.na(dataset))

totaldeaths_median = median(dataset$Total.Deaths, na.rm = TRUE)
dataset$Total.Deaths = ifelse(is.na(dataset$Total.Deaths), totaldeaths_median, dataset$Total.Deaths)
colSums(is.na(dataset))

population_median = median(dataset$Population, na.rm = TRUE)
dataset$Population = ifelse(is.na(dataset$Population), population_median, dataset$Population)
colSums(is.na(dataset))

# Bar Plot of Country vs Total Cases
x = dataset$Country
y = dataset$Total.Cases
barplot(y,names.arg=x,xlab="Country",ylab="Total Cases",col="blue",
        main="Country vs Total Cases",border="red")

# Scatter Plot of Total Cases vs Total Deaths
dataset%>% 
  ggplot(aes(Total.Cases,Total.Deaths))+
  geom_point(size=2)+
  labs(title="Total Cases vs Total Deaths")+
  abline(lm(Total.Deaths ~ Total.Cases), col = "blue", lwd = 2)

# Box Plot of Total Cases
ggplot(data=dataset,
       aes(y=Total.Cases))+ # Or you could write aes(height)
  geom_boxplot(fill='red')+
  labs(title="Boxplot of Total Cases",
       x="Total Cases of each Country")

################# Total Cases vs Total Recovered ###################
dataset%>% 
  ggplot(aes(Total.Cases,Total.Recovered))+
  geom_point(size=2)+
  labs(title="Total Cases vs Total Recovered")

########## Total Recovered vs Total Deaths ################
dataset%>% 
  ggplot(aes(Total.Recovered,Total.Deaths))+
  geom_point(size=2)+
  labs(title="Total Recovered vs Total Deaths")

# Population vs Total Cases
dataset%>% 
  ggplot(aes(Population,Total.Cases))+
  geom_point(size=2)+
  labs(title="Population vs Total Cases")

# Population vs Total Deaths
dataset%>% 
  ggplot(aes(Population,Total.Deaths))+
  geom_point(size=2)+
  labs(title="Population vs Total Deaths")

######## Total Cases vs Active Cases ##########
dataset%>% 
  ggplot(aes(Total.Cases,Active.Cases))+
  geom_point(size=2)+
  labs(title="Total Cases vs Active Cases")

######## Total Recovered vs Total Test #########
dataset%>% 
  ggplot(aes(Total.Recovered,Total.Test))+
  geom_point(size=2)+
  labs(title="Total Recovered vs Total Test")

# Heatmap
