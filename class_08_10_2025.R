# tidyverse - package which has all essentials libraries for data science for R language.

library(tidyverse)

mtcars
mtcars %>%
select(mpg:hp) %>%
arrange(desc(mpg)) %>%
mutate(
    mpg_2 = mpg * 2,
    hp_2 = hp / 2
) %>%
groupby(cyl) %>%
summarise(mean_hp = mean(hp))


# Task 1: finding average hp to wt ratio for each cylinder type.
# using tibble() is same as data.frame() but it additionally prints the data in a better format and its shape is also visible.
mtcars %>%
tibble() %>%
mutate(hp_wt_ratio = hp / wt) %>%
group_by(cyl) %>% 
summarise(avg_hp_wt_ratio = mean(hp_wt_ratio))

# Task 2: # fiding the top 3 cars with highest mpg to hp ratio.
mtcars %>%
mutate(mpg_hp_ratio = mpg / hp) %>%
arrange(desc(mpg_hp_ratio)) %>%
select(mpg_hp_ratio) %>%
head(3)

# task 3: 
# converting titanic dataset into tibble format.
Titanic <- as_tibble(Titanic)
# Titanic

# finding the survival rate of each class.
Titanic %>% 
# as_tibble() %>%
group_by(Class, Survived) %>%
summarise(count = sum(n), .groups = "drop") %>% # n represents all the counts in the dataset. it comes with library(tidyverse)
# either use .groups = "drop" or ungroup() to remove the grouping after summarise.
# ungroup() %>% 
group_by(Class) %>%
mutate(total = sum(count), survival_rate = (count / total) * 100) %>%
filter(Survived == "Yes") %>%
select(Class, survival_rate)