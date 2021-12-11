# EDA 

library(lattice)
library(gridExtra)
library(bayesplot)
library("ggpubr")
library(ggplot2)
library(tidyverse)


imdb_clean <- read.csv("imdb.csv", header = TRUE)
imdb_movies <- read.csv("IMDb movies.csv", header = TRUE)
imdb_ratings <- read.csv("IMDb ratings.csv", header = TRUE)

imdb<- full_join(imdb_ratings, imdb_movies)

## Male or female like romance more?

imdb$romance <- ifelse(grepl("Romance", imdb$genre), "yes", "no")
imdb <- imdb %>% filter(romance == "yes")
imdb$male_gt_female <- ifelse(imdb$males_allages_avg_vote > imdb$females_allages_avg_vote, 1, 0)
imdb$male_vote <- imdb$males_allages_avg_vote
imdb$female_vote <- imdb$females_allages_avg_vote

male_gt <- nrow(imdb %>% filter(male_gt_female == 1))
female_gt <- nrow(imdb) - male_gt
  
EDA1 <- data.frame(gender = c("male", "female"), count = c(male_gt, female_gt))

plot1 <- ggplot(EDA1, aes(gender, count)) +
  geom_bar(stat = "identity", width=0.5, fill = "#336666") +
  labs(title = "Movies with higher rating than other gender", x = "Gender", y = "Number of Movies")



# Potential Variable Testing Plots:

# budget - does not seem to be a large effect
a <- ggscatter(imdb, x = "budget", y = 'males_allages_avg_vote', 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "variable", ylab = "budget")


# reviews from critics - seems I should take the log
b <- ggscatter(imdb, x = "reviews_from_critics", y = 'males_allages_avg_vote', 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "variable", ylab = "critic reviews")


# votes - popular might be worth considering
c <- ggscatter(imdb, x = "votes", y = 'males_allages_avg_vote', 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "variable", ylab = "votes")



youngplot <- ggplot(data = imdb_clean)+
  aes(young, male_rating) +
  geom_point(aes(color = genre), alpha = 0.3)+
  labs(title="young voter proportion's impact on rating ",x="proportion of young voters",y="rating")+
  geom_smooth(aes(color = genre),method = "lm",se=F)+
  facet_wrap(~genre)

votesplot <- ggplot(data = imdb_clean)+
  aes(total_votes, male_rating) +
  geom_point(aes(color = genre), alpha = 0.3)+
  labs(title="total number of votes impact on male rating ",x="total votes",y="male rating")+
  geom_smooth(aes(color = genre),method = "lm",se=F)+
  facet_wrap(~genre) 

budgetplot <- ggplot(data = imdb_clean)+
  aes(budget, male_rating) +
  geom_point(aes(color = genre), alpha = 0.3)+
  labs(title="ymovie budget's impact on rating ",x="movie budget",y="male rating")+
  geom_smooth(aes(color = genre),method = "lm",se=F)+
  facet_wrap(~genre)
