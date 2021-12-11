# Model fitting and checking

## Load required packages

library(lmerTest)
library("ggpubr")
library(ggplot2)
library(readr)
library(rstanarm)
library(knitr)
library(kableExtra)
library(tidytext)
library(car) 
library(gvlma)
library(lme4)
library(arm)
library(bayesplot)



imdb_clean <- read.csv("imdb.csv", header = TRUE)



## Model 1: simple linear model

#fit1a <- lm(male_vote_prop ~ comedy + drama + popular, data = imdb_clean)
#fit1b <- lm(male_gt_female ~ comedy + drama + popular, data = imdb_clean)

# summary(fit1b)


## Model 2: Adding predictors

#fit2a <- lm(male_vs_female ~ budget + male_vote_prop + total_votes + reviews + young + duration + year, data = imdb_clean)
#fit2b <- lm(male_rating ~ budget + total_votes + critics + duration + year, data = imdb_clean)

#summary(fit2b)



## Model 3: Multilevel with genres, recreating dependent variable

#fit3 <- lmer(male_vs_female ~ popular + budget + young + male_votes + critics + duration + year + (1 |genre), data = imdb_clean, refresh=0)


## Model 4: dropping male_votes & popular, adding male_vote_prop & reviews, changing genre

#fit4a <- lmer(male_vs_female ~ budget + young + total_votes + (1 + total_votes|genre), data = imdb_clean)
#fit4b <- lmer(male_rating ~ budget + young + total_votes + male_vote_prop + critics + duration + year + (1 + total_votes|genre), data = imdb_clean)
fit4c <- lmer(male_rating ~ budget + total_votes + critics + duration + year + (1|genre), data = imdb_clean)

#summary(fit4c)

## Model 5: using z-scores for all continuous variables

# Rescaling:

imdb_clean$budget_z <- (imdb_clean$budget - mean(imdb_clean$budget))/sd(imdb_clean$budget)
imdb_clean$total_votes_z <- (imdb_clean$total_votes - mean(imdb_clean$total_votes))/sd(imdb_clean$total_votes)
imdb_clean$male_votes_z <- (imdb_clean$male_votes - mean(imdb_clean$male_votes))/sd(imdb_clean$male_votes)
imdb_clean$critics_z <- (imdb_clean$critics - mean(imdb_clean$critics))/sd(imdb_clean$critics)
imdb_clean$duration_z <- (imdb_clean$duration - mean(imdb_clean$duration))/sd(imdb_clean$duration)
imdb_clean$year_z <- (imdb_clean$year - mean(imdb_clean$year))/sd(imdb_clean$year)
imdb_clean$reviews_z <- (imdb_clean$reviews - mean(imdb_clean$reviews))/sd(imdb_clean$reviews)

mean(imdb_clean$budget)
#fit5a <- lmer(male_rating ~ budget_z + total_votes_z + critics + duration + year + (1|genre), data = imdb_clean)
fit5b <- lmer(male_rating ~ budget_z + total_votes_z + critics_z + duration_z + year_z + (1|genre), data = imdb_clean)
#fit5c <- lmer(male_rating ~ budget_z + total_votes_z + critics_z + duration_z + year_z + (1 + budget_z|genre) + (1 + total_votes_z|genre) + (1 + critics_z|genre), data = imdb_clean)

#summary(fit5c)
#coef(fit5c)


# Model 6: final changes

fit <- lmer(male_rating ~ budget_z + total_votes_z + critics_z + duration_z + year_z + reviews_z + young + (1 + budget_z|genre) + (1 + total_votes_z|genre) + (1 + critics_z|genre), data = imdb_clean)

summary(fit)
coef(fit)


#binnedplot(fitted(fit4),resid(fit4))
fitted <- plot(fit)
#qq_plot <- qqmath(fit)


Leverage <- ggplot(data.frame(lev=hatvalues(fit), pearson=residuals(fit,type="pearson")), 
                   aes(x=lev,y=pearson)) + geom_point()

coef(fit)$genre
ranef(fit)$genre
#Random_Effects <- round(ranef(fit)$genre,3)
#print(fit, correlation=TRUE) 
#summ(fit)
#ranova(fit)
