library(tidyverse)
library(viridis)
library(knitr)

d <- read_csv("AuthorList.csv")

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

# calculate dice coefficient between authors publishing target year and previous 3 years
rollingDice <- function(targetYear){
  
  previousYears <- as.character(c(targetYear-1, targetYear-2, targetYear-3))
  targetYear <- as.character(targetYear)
  
  targetYear <- d %>% filter(year == targetYear) %>% pull(author) %>% unique() # select only unique authors in this year
  previousYears <- d %>% filter(year %in% previousYears) %>% pull(author) %>% unique() # select only unique authors in these years
  
  overlap <- intersect(targetYear, previousYears) # get vector of authors which appear in target year and previous years
  overlapN <- length(overlap) # get number of authors overlapping
  
  targetN <- length(targetYear) # get number of authors in target year
  previousN <- length(previousYears) # get number of authors in previous years
  
  dice <- (2*overlapN) / (previousN + targetN) # calculate dice coefficient
  proportion <- overlapN/targetN
  
  return(data.frame(dice, proportion))
}

yearDice <- data.frame(targetYear = seq(2011,2017, 0001)) %>%
  rowwise() %>%
  mutate(rollingDice = rollingDice(targetYear)$dice, 
         proportion = rollingDice(targetYear)$proportion)

ggplot(data = yearDice) +
  geom_line(aes(x = targetYear, y = rollingDice, group = 1), colour = "indianred1", size = 1.1) +
  geom_vline(xintercept=2014.05) +
  scale_x_continuous(breaks = seq(2011, 2017, 0001), labels = seq(2011, 2017, 0001)) +
  ylab("Author overlap with previous three years (Dice coeffcient)") +
  xlab("Publication year") +
  ggtitle("Psychological Science") +
  papaja::theme_apa()

ggplot(data = yearDice) +
  geom_line(aes(x = targetYear, y = proportion, group = 1), colour = "indianred1", size = 1.1) +
  geom_vline(xintercept=2014.05) +
  scale_x_continuous(breaks = seq(2011, 2017, 0001), labels = seq(2011, 2017, 0001)) +
  ylab("Author overlap with previous three years (proportion)") +
  xlab("Publication year") +
  ggtitle("Psychological Science") +
  papaja::theme_apa()



