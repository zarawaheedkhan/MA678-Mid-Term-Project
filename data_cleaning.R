# Data cleaning and processing


## Load relevant packages

library(tidyverse)
library(reshape2)
library(stringr)
library(dplyr)

## Read the data

imdb_movies <- read.csv("/Users/zarawaheed/Documents/BostonUniversity/MA678/Mid-Term-Project/IMDb movies.csv", header = TRUE)
imdb_ratings <- read.csv("/Users/zarawaheed/Documents/BostonUniversity/MA678/Mid-Term-Project/IMDb ratings.csv", header = TRUE)

imdb<- full_join(imdb_ratings, imdb_movies)




########### Interesting points to consider ###########

## If comedy or drama

imdb$comedy <- ifelse(grepl("Comedy", imdb$genre), "yes", "no")
imdb$drama <- ifelse(grepl("Drama", imdb$genre), "yes", "no")

## Popular or not

### mean votes per movie (to see popularity)
average_votes <- median(imdb_movies$votes)

imdb$popular <- imdb$votes - average_votes

## Proportion of young voters

imdb$young <- (imdb$males_0age_votes + imdb$males_18age_votes) / imdb$males_allages_votes

## Proportion of male votes

imdb$male_vote_prop <- imdb$males_allages_votes / imdb$total_votes





# Dependent variable options: 

### Binary - If male votes are greater than female votes
imdb$male_gt_female <- ifelse(imdb$males_allages_avg_vote > imdb$females_allages_avg_vote, 1, 0)

### Continuous - Difference between male and female rating
imdb$male_vs_female <- imdb$males_allages_avg_vote - imdb$females_allages_avg_vote

### Continuous - Male rating 
imdb$male_rating <- imdb$males_allages_avg_vote

############  Filtering and cleaning ##############

# Make categorical variable (genre)

imdb <- imdb %>% 
  separate(genre, into = c("A", "B", "C"), sep = ",")

imdb$genre <- imdb$A

imdb1 <- imdb %>% filter(genre == "Romance")
imdb2 <- imdb %>% filter(genre == "Comedy" | genre == "Sci-Fi" | genre == "Drama" | genre == "Action" | genre == "Crime" | genre == "Thriller" | genre == "Family" | genre == "Fantasy" | genre == "Horror")

imdb2$comedy = "no"
imdb2$drama = "no"

imdb <- full_join(imdb2, imdb1)
imdb$genre <- as.factor(imdb$genre)

## Keep only US data

imdb <- imdb %>% filter(country == "USA" & year >= "1990")


## Clean budget

regexp <- "[[:digit:]]+" # prepare regular expression

imdb$budget <- str_extract(imdb$budget, regexp)


# Test uptil now:

#imdb_almost <- imdb %>% select("title", "genre", "comedy", "drama", "popular", "budget", "young", "total_votes", "duration", "year")



## Remove NAs and blank cells

imdb_almost <- imdb[!is.na(imdb$young), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$popular), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$comedy), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$drama), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$male_gt_female), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$male_vs_female), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$male_vote_prop ), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$budget ), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$reviews_from_critics ), ]
imdb_almost <- imdb_almost[!is.na(imdb_almost$reviews_from_users ), ]

imdb_almost$budget <- as.numeric(imdb_almost$budget)
imdb_almost$critics <- as.numeric(imdb_almost$reviews_from_critics)
imdb_almost$reviews <- as.numeric(imdb_almost$reviews_from_users)
imdb_almost$male_votes <- as.numeric(imdb_almost$males_allages_votes)
imdb_almost$total_votes <- as.numeric(imdb_almost$total_votes)
imdb_almost$year <- as.numeric(imdb_almost$year)


## Subset data down to only the relevant columns

keep <- c("title", "male_vs_female", "male_rating", "genre", "popular", "budget", "young", "critics", "reviews", "male_vote_prop", "male_votes", "duration", "year", "male_gt_female", "total_votes", "comedy", "drama")

#imdb_clean <- imdb_almost %>% select("title", "male_vs_female", "genre", "popular", "budget", "young", "critics", "reviews", "male_vote_prop", "male_votes", "duration", "year", "male_gt_female", "total_votes", "comedy", "drama")
imdb_clean <- imdb_almost[, keep]

write.csv(imdb_clean,"imdb.csv", row.names = TRUE)








