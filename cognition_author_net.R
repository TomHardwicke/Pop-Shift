library(tidyverse)
library(viridis)
library(knitr)

set.seed(123) # set random seed

d <- read_csv("AuthorList_Cognition.csv")

authors <- d %>%
  group_by(author) %>%
  count

d %>% 
  group_by(year) %>%
  summarise(n_papers = length(unique(article_id)), 
            n_authors = length(author),
            n_authors_per_paper = n_authors / n_papers,
            n_unique_authors = length(unique(author))) %>%
  kable(digits = 2)


unique_authors <- d %>% 
  select(-article_id) %>%
  distinct
  
  

overlaps <- d %>%
  split(.$year) %>%
  map_df(function(x) {
    this_year_authors = unique(x$author)
    overlap <- unique_authors %>% 
      group_by(year) %>% 
      summarise(overlap = sum(author %in% this_year_authors), 
                dice = (2 * overlap) / (length(author) + length(this_year_authors))) %>%
      mutate(source_year = x$year[1])
  }) %>%
  mutate(overlap = ifelse(year == source_year, NA, overlap),
         dice = ifelse(year == source_year, NA, dice))

ggplot(filter(overlaps, year > 2008 & year < 2018, 
              source_year > 2008 & source_year < 2018), 
       aes(x = year, y = source_year, fill = dice)) + 
  geom_tile() + 
  xlim(2009,2018) + 
  scale_fill_viridis() + 
  theme_bw()


### TOM CODE

# calculate dice coefficient 
dice <- function(vector1, vector2){
  v1 <- unique(vector1)
  v2 <- unique(vector2)
  
  overlap <- intersect(v1, v2) # find elements that are present in both vectors
  overlapN <- length(overlap) # find number of overlapping elements
  
  v1N <- length(v1) # get length of vector 1
  v2N <- length(v2) # get length of vector 2
  
  diceCoeff <- (2*overlapN) / (v2N + v1N) # calculate dice coefficient
  
  return(diceCoeff)
}

bootDice <- function(v1, v2) {
  v1_new <- sample(v1, size = length(v1), replace = T)
  v2_new <- sample(v2, size = length(v2), replace = T)
  return(dice(v1_new, v2_new))
}

# calculate dice coefficient between authors publishing target year and previous 3 years
rollingDice <- function(targetYear){
  
  previousYears <- as.character(c(targetYear-1, targetYear-2, targetYear-3))
  targetYear <- as.character(targetYear)
  
  targetYear <- d %>% filter(year == targetYear) %>% pull(author) # select authors in this year
  previousYears <- d %>% filter(year %in% previousYears) %>% pull(author) # select authors in these years
  
  diceOut<- dice(targetYear, previousYears)

  myDice <- replicate(9999, bootDice(targetYear, previousYears))
  hist(myDice)
  diceCI <- quantile(myDice, c(.025, .975))
  print(mean(myDice))
  print(diceCI)
  return(diceOut)
}

yearDice <- data.frame(targetYear = seq(2008,2017, 0001)) %>%
  rowwise() %>%
  mutate(rollingDice = rollingDice(targetYear)$dice, 
         proportion = rollingDice(targetYear)$proportion)

ggplot(data = yearDice) +
  geom_line(aes(x = targetYear, y = rollingDice, group = 1), colour = "indianred1", size = 1.1) +
  geom_vline(xintercept=2015.12) +
  scale_x_continuous(breaks = seq(2008, 2017, 0001), labels = seq(2008, 2017, 0001)) +
  ylab("Author overlap with previous three years (Dice coeffcient)") +
  xlab("Publication year") +
  ggtitle("Cognition") +
  papaja::theme_apa()

ggplot(data = yearDice) +
  geom_line(aes(x = targetYear, y = proportion, group = 1), colour = "indianred1", size = 1.1) +
  geom_vline(xintercept=2015.12) +
  scale_x_continuous(breaks = seq(2008, 2017, 0001), labels = seq(2008, 2017, 0001)) +
  ylab("Author overlap with previous three years (proportion)") +
  xlab("Publication year") +
  ggtitle("Cognition") +
  papaja::theme_apa()


x <- array(c(vector1, vector2))
b <- boot(x, statistic = dice, R = 999)
boot.ci(b, type = c("norm", "basic", "perc", "bca"))



# calculate percentile bootstrap



bootDice <- function() {
  v1 <- sample(x = vector1, size = length(vector1), replace = T)
  v2 <- sample(x = vector2, size = length(vector2), replace = T)
  return(dice(v1, v2))
}

myDice <- replicate(999, bootDice())
hist(myDice)
quantile(myDice, c(.025, .975))