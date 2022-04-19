# load libraries
library(tidyverse)

# read data in
intense_data <- read_tsv("https://raw.githubusercontent.com/picoral/PREDICAR-2022/main/intensifiers_tweets.tsv")
intense_data <- read_tsv("data/intensifiers_tweets.tsv")

# que contas de twitter?
# pipe, cano atalho: %>% crtl + shift + m 
intense_data %>%
  count(account, sort = TRUE)

# porcentagem de intensificação no corpus
intense_data %>% # and then, e então
  summarize(mean(intensifier))

# descriptive statistics 
intense_data %>%
  group_by(account) %>%
  summarize(n = n(),
            percent_intensified = mean(intensifier),
            sd_intensified = sd(intensifier),
            intensified = n*percent_intensified) %>%
  arrange(percent_intensified)

# collocations
# quais adjetivos mais frequentes que são intensificados
library(tidytext)
intense_data %>%
  filter(intensifier == 1) %>%
  group_by(account, intensifier_lemma) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  ggplot(aes(y = reorder_within(intensifier_lemma, n, account),
             x = n)) +
  geom_col() +
  facet_wrap(~account, scales = "free_y") +
  scale_y_reordered() +
  theme_linedraw() +
  labs(y = "intensificador",
       x = "frequência",
       title = "Distribuição de intensificador por conta de twitter",
       caption = "dados adquiridos pela Twitter API")



