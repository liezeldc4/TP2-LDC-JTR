library(tidyverse) 
dataset=read.csv('covid_worldwide.csv')


# Explore the data
view(dataset) # View the entire dataset
head(dataset) # View the first 6 entries of the dataset
glimpse(dataset) # Views every column of the dataframe in a transposed manner
length(dataset) # Length of dataset is 8
names(dataset) # Column Names
summary(dataset) # Shows us a brief summary of each column including the data type and length

# Convert columns from character to numeric
dataset$Total.Cases = as.numeric(gsub(",", "", dataset$Total.Cases)) # Total Cases has been converted to a numerical column
glimpse(dataset) # We can verify this by seeing all commas have been removed
ggplot(data=dataset,aes(Total.Cases))+
  geom_histogram() # The graph shows the values in this column are right skewed

dataset$Total.Deaths = as.numeric(gsub(",", "", dataset$Total.Deaths)) # Total Deaths has been converted to a numerical column
glimpse(dataset) # We can verify this by seeing all commas have been removed
ggplot(data=dataset,aes(Total.Deaths))+
  geom_histogram()  # The graph shows the values in this column are right skewed

dataset$Population = as.numeric(gsub(",", "", dataset$Population)) # Population has been converted to a numeric column
glimpse(dataset) # We can verify this by seeing all commas have been removed
ggplot(data=dataset,aes(Population))+
  geom_histogram()  # The graph shows the values in this column are right skewed

# Impute Total Cases, Total Deaths, and Population with Median since each of the values are right skewed
totalcases_median = median(dataset$Total.Cases, na.rm = TRUE)
dataset$Total.Cases = ifelse(is.na(dataset$Total.Cases), totalcases_median, dataset$Total.Cases) # Replacing all N/A values with the median of the corresponding values to each column
colSums(is.na(dataset)) # We can verify there are no null values in this column

totaldeaths_median = median(dataset$Total.Deaths, na.rm = TRUE)
dataset$Total.Deaths = ifelse(is.na(dataset$Total.Deaths), totaldeaths_median, dataset$Total.Deaths) # Replacing all N/A values with the median of the corresponding values to each column
colSums(is.na(dataset)) # We can verify there are no null values in this column

population_median = median(dataset$Population, na.rm = TRUE)
dataset$Population = ifelse(is.na(dataset$Population), population_median, dataset$Population) # Replacing all N/A values with the median of the corresponding values to each column
colSums(is.na(dataset)) # We can verify there are no null values in this column

# Bar Plot of Country vs Total Cases
x = dataset$Country
y = dataset$Total.Cases
barplot(y,names.arg=x,xlab="Country",ylab="Total Cases",col="blue",
        main="Country vs Total Cases",border="red")

# Scatter Plot of Total Cases vs Total Deaths
dataset%>% 
    ggplot(aes(Total.Cases,Total.Deaths))+
    geom_point(size=2)+
    geom_smooth(method="lm", se=FALSE)+
    labs(title="Total Cases vs Total Deaths")

# Box Plot of Total Cases
dataset %>% 
  filter(Total.Cases<140000) %>% 
  ggplot(aes(y=Total.Cases))+ # Or you could write aes(height)
  geom_boxplot(fill='red')+
  labs(title="Boxplot of Total Cases",
       x="Total Cases of each Country")

# Population vs Total Cases
dataset%>% 
  ggplot(aes(Population,Total.Cases))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=FALSE)+
  labs(title="Population vs Total Cases")

# Population vs Total Deaths
dataset%>% 
  ggplot(aes(Population,Total.Deaths))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=FALSE)+
  labs(title="Population vs Total Deaths")

# Heatmap of population, total deaths and total cases
mydata=dataset[,c(3,4,8)]
corr=round(cor(mydata),2)
head(corr)
install.packages("reshape2")
#Plot
library(reshape2)
melted_corr=melt(corr)
head(melted_corr)
ggplot(data=melted_corr,
       aes(Var1, Var2, fill=value))+
  geom_tile()

# Overall, we ran into some difficulties with this dataset - it needed more data cleaning than expected.
# Due to this, we were unable to convert some columns from a character data type to a numeric data type.
# As such, we could not plot some graphs because the data was not classified as numerical.
# Therefore making it harder to establish a relationship/association between the different variables.

# However, with the columns that were able to be converted (Total Cases, Total Deaths, Population),
# we plotted a variety of graphs so that we may analyze them.

# In the first graph, we can see that the number of cases varies among all countries but its distribution
# is right skewed. There is one peak that occurs with the USA.

# In the second graph, it shows a positive correlation between total cases and total deaths.

# The third graph shows the distribution of total cases in each country using a box plot, and any outliers.
# 25% of data points lie under 24001, the median is 206,592, and 75% of data points lie under 1,296,146.

# The fourth graph is a scatterplot for population against total cases. It shows a positive correlation
# between the two variables. Similarly, we obtain the same observations for the fifth plot of
# population vs total deaths

# The final plot is a heatmap which displays the correlation coefficients between the three numeric variables:
# Total Cases, Total Deaths, and Population.
head(corr)
# Here, we can see there is a positive and closer to 1 correlation coefficient for Total Cases and Total Deaths,
# as opposed to Population vs Total Cases, and Population vs Total Deaths.

