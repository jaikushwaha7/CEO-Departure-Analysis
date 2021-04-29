setwd("D:/Study/R/Ceo Departure Analysis")


# screencast walks through how to use bootstrap resampling, with this week's #TidyTuesday dataset on CEO departures. ????

# Explore data
library(tidyverse)
departures_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv")

head(departures_raw)

departures_raw %>%
  filter(departure_code<9) %>%
  mutate(involuntary = if_else(departure_code %in% 3:4, 'involuntary','other')) %>%
  filter(fyear>1995, fyear<2019) %>%
  count(fyear, involuntary) %>%
  ggplot(aes(fyear, n, color=involuntary))+
  geom_line(size=1.2, alpha=.5) +
  geom_point(size=2) +
  geom_smooth(method = 'lm',lty=2) +
  scale_y_continuous(limits = c(0,NA)) +
  labs(x=NULL, y="Number of CEO departures",color=NULL)

departures<- departures_raw %>%
  filter(departure_code <9) %>%
  mutate(involuntary= if_else(departure_code %in% 3:4, "involuntary", 'other')) %>%
  filter(fyear> 1995, fyear<2019)

str(departures)
departures

# Bootstrapping a model

library(broom)

df<-departures %>%
 count(fyear, involuntary) %>%
 pivot_wider(names_from = involuntary, values_from=n)

df
mod <- glm(cbind(involuntary,other)~ fyear, data=df,family='binomial')
summary(mod)

tidy(mod, exponentiate =TRUE)

# When we use exponentiate =True we get the model coefficients on the linear scale instead of the 
# logistic scale

# We want to do is fit a model like this a whole bunch of times, instead of just once.
# Lets create bootstrape resamples

library(rsample)

set.seed(123)
ceo_folds<- bootstraps(departures, times= 1e3)
ceo_folds

# Now we need to makes a function to count up the departures by year and type. Fit out model 
# and return coefficients we want

fit_binom <- function(split){
  df<- analysis(split) %>%
    count(fyear, involuntary) %>%
    pivot_wider(names_from=involuntary, values_from=n)
  
  mod<- glm(cbind(involuntary,other)~fyear, data =df,family="binomial")
  tidy(mod, exponentiate = T)
}

# We can apply that function to all our bootstrap resampled with purrr::map()
boot_models<- ceo_folds %>% mutate(coef_info = map(splits, fit_binom))
boot_models

# Explore results
# What we want to find? We can compute bootstrap confidence intervals with int_pctl()

percentile_interval<- int_pctl(boot_models, coef_info)
percentile_interval

# We can visalize the results as well
boot_models %>%
  unnest(coef_info) %>%
  filter(term=="fyear") %>%
  ggplot(aes(estimate)) +
  geom_vline(xintercept = 1, lty=2, color='purple', size=2) +
  geom_histogram(color='blue') +
  labs(
    x="Annual increase in involuntary CEO departures",
    title="Over this time period CEO departures are increasingly involuntary",
    subtitle="Each passing year corresponds to a departure being 1-2 % more likely to be involuntary"
  )
  