
# R Dataset App

This app gives the user the option to plot a histogram from (almost) any dataset in base R. There are 42 datasets to chose from. Within each dataset, the user can choose any numeric variable to plot as well as any character variable to group by (if any). Additionally, the user has the option to filter the data by value range. The app displays a histogram and a table of the data based on the user input. Lastly, there is an option to download the filtered data to use for analysis.

Link to this app: 

The original version of this app uses only the *Orange* dataset. This is the reason why the name of the repository is `oranges` despite it not being about oranges. Don't want to cause any confusion with grading for assignment 3. The link for that app is below as well.

Link to the old app: https://clayton-allard.shinyapps.io/orange/

# New Features

1. Option to choose any dataset from base R
- Only chose R datasets that are of class `data.frame`, and have at least one numeric variable
2. Option to group by any based character variable
- Only if there are less than 10 unique values. Otherwise, the histogram gets overwhelmed with groups
3. Option to download filtered dataset

# Old Features

1. Option to choose which variable to plot by
2. Option to select a value range to display on the histogram
3. Option to select the number of bins for the histogram to use
4. Option to toggle the option to group by the `Tree` variable or to just count totals
5. An interactive table that can be sorted by any variable, and can filter on character search
