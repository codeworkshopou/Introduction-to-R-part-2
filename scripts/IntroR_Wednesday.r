#################################################
###### Computer Science in Biology Workshop #####
######## Introduction to R II: Day 1 ############
#################################################

# We will provide a short summary of the previous course 
# and go into loading and modifying datasets, using linear 
# models to analyze data, and plotting data with "ggplot2" 

#Instructors: Christina Kolbmann & Meelyn Mayank Pandit 

#### Setting working directory ####

# Place the folder with all your materials from google drive to a 
# single directory/folder (e.g., in Documents)
# Now set your working directory using 'setwd()' function
# This function brings R to the folder your files are in

getwd()
setwd("C:/Users/Kolbm/Documents/R workshop/Introduction to R P2 CK/")
# This is my working directory, you will have to change this to yours

# If you set it correctly, you should see the documents under the Files tab
getwd()
# You can also see the current filepath you are in below the Console tab

#### Organization ####

# A filepath tells you the folders you are in so you can find files
# Its helpful to create folders that specify what type of files are in them
# For Ex: 

# My folder for this workshop is called "Introduction to R P2"
#  If I were to store all my files in just the Intro folder, I would
# have to remember the file's exact name to find it. Whereas, 
# if I create folder like "data", "results", "data_clean", and "scripts",
# I can go into any of those folders and find all the files of that
# purpose there. 


#### Reading a CSV file ##### 

# csv stands for Comma Separated Values and is just like an excel 
# file but you save it as *.csv instead of *.xls
# You can also use this function to load in other files separated
# by tabs, or even ~ using...
# read.csv(file, sep = "\t")
# We will read a file using 'read.csv' function

#saves csv as a dataframe
frogs <- read.csv("data/nucdiv_het.csv", header = FALSE) 
frogs # view file as dataframe

#The 'head' function shows us the first 5ish rows of our file
head(frogs) 
# The 'tail' function shows us the last 5ish rows of our file
tail(frogs)

#What does the 'header = TRUE' do? Let's find out
frogs <- read.csv("data/nucdiv_het.csv", header = TRUE)  
head(frogs) # you can now call columns in frogs by the header/column names

#### Cleaning your data ####

# Let's modify our file so we can use it more effectively
# We will change column names, delete a column, and delete a row
View(frogs)

# Capitalization, using _ , or delayed capitalization are some ways
# people organize names in their files. In frogs, all the column
# names are capital. So we can type faster, let's change the column
# names to all lower case.
names(frogs) <- c("population", "heterozygosity","genediversity")
head(frogs)

# To me, the far right column doesn't look uniform with the others
# Let's rename only that column using delayed capitalization
names(frogs)[3]<-"geneDiversity"
head(frogs)

# Nope still doesn't look right to me. Let's use an _ instead
names(frogs)[3]<-"gene_diversity"
head(frogs)

# The ways to name objects are up to you, but keep in mind how
# you type, and if you may ever need to send your code to others

# As a fresher, these are some ways you can select and remove 
# parts of you dataframe: dataframe[row,col]
frogs[,-1] # all but first column
frogs[1:3,] # choose only first 3 rows of all columns
frogs[,1:2] # choose only first 2 columns of all rows

#### How to Google ####

# Use terms like dataframe, element, value, row, column, vector, string
# Stackoverflow is a great resource
# Ex: "Remove value from integer in column R" 
# This is asking how you would remove extra 0's in numbers in 
# 1 column in the program R

# Look up functions or packages for details in R
?print()

#### Manipulating data ####

# One of the most common steps before working with your data, 
# is figuring out what parts of your files you need.

# tidyverse is a great package for manipulating your data
# Tidyverse is an umbrella package that contains other packages like
# tidyr, dplyr, tibble, ggplot2.

install.packages("tidyverse")
library(tidyverse) # Click YES to all

# Some of the functions I use often from this package are 
# summarize(), mutate(), select(), filter(), count(), 
# arrange(), and the pipe operator %>% 

# Shortcuts for the pipe are: Ctrl+Shift+M or Cmd+Shift+M

# If you've used these packages before you know how useful they are!
# These two in paricular can be specified further using these functions
?select() # starts_with(), matches(), num_range(), where()
?filter() # ==, >, <, &, |, !, is.na(), between(), near()

# To change data with Tidyverse we have to load our data using read_csv
frogs <- read_csv("data/nucdiv_het.csv")
names(frogs) <- c("population", "heterozygosity","gene_diversity")

# You can subset your data for only the columns you need
popHet <- frogs %>% 
  select(population, heterozygosity)
popHet # This is the same as: frogs[,1:2] 

# We can also use these functions to select and view data of interest
small <- frogs %>% 
  filter(heterozygosity > 0.2 & gene_diversity > 0.25) %>% 
  arrange(desc(heterozygosity))
small # Now you have the 5 frogs with high heterozygosity, high 
      # genetic diversity, and the highest heterozygosity first.

# People typically prefer using Tidyverse functions if they remember
# names more than numbers but just remember, they are many ways
# to get the same result. Use whichever works for you!


#### Analyzing data ####

# Now that we know how to choose and manipulate our data, 
# we can analyze it. We will now explore whether the second 
# column (heterozygosity) is correlated to the third column (gene diversity)

# First, we will make a basic plot, are they directly or indirectly correlated?
plot(heterozygosity ~ gene_diversity, data=frogs) # Yes, direct/positive relationship

# We can also test whether the correlation is statistically significant
# through a statistical test called the Pearson Correlation

cor.test(frogs$heterozygosity, frogs$gene_diversity, method="pearson")
# p-value is smaller than 0.05, meaning it is significant
# Cor Value is 0.957 which is very close to 1, meaning it is a positive relationship


#### Analyzing data - On your own ####

# Now, you will look at correlation in a different dataset, 
# you will make a plot and run Pearson Correlation just like 
# in the example above. First I will provide you with a script
# but there will be mistakes. You have to correct them. 
# Answers are on the left of the same line to check if you are right.

hoppers <- read.csv("grasshopper.csv", header = TRUE)               #ANSWER: file name is grasshoppers.csv in the data/ folder
view(hoppers)

plot(Range ~ Emergence, data=frogs)                                 #ANSWER: wrong data frame in the data field
cor.test(hoppers$Range, hoppers$Emergence, method="pearson")

plot(Range ~ Months_Present, data=hoppers)
corr.test(hoppers$Range, hoppers$Months_present, method="pearson")  #ANSWER the function should be cor.test not corr.test

plot(Range ~ Wing_Color, data=hoppers)                              #ANSWER: the variable Wing_Color should be Wing_color, R is case sensitive
cor.test(hoppers&Range, hoppers&Wing_color, method="pearson")       #ANSWER: The ampersand symbols should be replaced with the $ symbol

plot(Range ~ Size, data=hoppers)
cor.test(hoppers$Range, hoppers$Wing_Size, method="pearson")        #ANSWER: the variable Wing_Size should be Size

# If you didn't catch everything that's ok! It takes practice to
# first learn a language, and then know it well enough to find
# mistakes in code; especially if you didn't write it.


#### Displaying data ####

# We can use a different package to make more appealing plots
# ggplot2 is a plotting package that allows you to create complex
# graphs and change more aspects of the graph itself
# This package can do some many things but we are only going over a few.
# Check out the cheatsheet on their website for more abilities! 
# https://ggplot2.tidyverse.org/

# ggplot2 is one of the packages in the Tidyverse umbrella package
# but in the future, if you you don't want to load all of Tidyverse,
# here is the code for just ggplot2

# install.packages("ggplot2")
# library(ggplot2)

# ggplot2 can create graphs like:
# scatter plots - geom_point()
# box plots - geom_boxplot()
# line plots - geom_line()
# more! - Type "geom" then hit Tab

ggplot(frogs) # Just calling ggplot is not the same as plot()

# We need to say where the data should go on our graph
ggplot(frogs, aes(x=gene_diversity, y=heterozygosity))

# Now which type of graph should we plot? Let's do a scatter plot
ggplot(frogs, aes(x=gene_diversity, y=heterozygosity)) +
  geom_point()  # Whenever you want to add an element, you need a +
                # at the end of that line

# Let's change the color of the points according to the population
ggplot(frogs, aes(x=gene_diversity, y=heterozygosity, color=population)) +
  geom_point()

# Add titles
ggplot(frogs, aes(x=gene_diversity, y=heterozygosity, color=population)) +
  geom_point()+
  labs(x="Genetic Diversity", 
       title = "Genetic Variation in Frog Populations")

# Add trendline
ggplot(frogs, aes(x=gene_diversity, y=heterozygosity, color=population)) +
  geom_point()+
  labs(x="Genetic Diversity", 
       title = "Genetic Variation in Frog Populations")+
  geom_abline(intercept = 0.01751061, slope = 0.70460838) # Found these using coef(lm(Heterozygosity ~ Gene_Diversity, data = frogs))

# Save last graph in results folder
ggsave("results/GeneVarinFrogs.png")

# To make sure you saved the right graph, check under the Files tab
# in your results folder for GeneVarinFrogs.png

#### Displaying data - On your own ####

# Use the grasshoppers.csv file to create a plot you like
# Look at the code above and the cheat sheet on this website
# for more ideas! https://ggplot2.tidyverse.org/
hoppers <- read.csv("data/grasshoppers.csv", header = TRUE)
view(hoppers)

####################### Day 2 #########################
#### Understanding & Running code you didn't write ####
#######################################################

