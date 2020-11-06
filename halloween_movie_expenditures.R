library(tidyverse)
library(extrafont)

hmovies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
hmovies_cl <- hmovies %>% 
  mutate(month = month(dmy(release_date), label = T), 
         budget = as.numeric(gsub("[[:punct:]]", "", budget)), 
         run_time = parse_number(movie_run_time)) %>% 
  filter(!is.na(month)) %>% 
  filter(!is.na(budget))

rating_colors <- c(E = "")


hmovies_cl %>% 
  group_by(month, movie_rating) %>% 
  summarize(expenditures = sum(budget)) %>% 
  ggplot(aes(x=month, y=expenditures)) + 
  geom_col(aes(fill = movie_rating)) +
  scale_y_continuous(labels=dollar_format()) +
  theme_dark() +
  theme(
    panel.background = element_blank(), 
    plot.background = element_rect(fill="grey0"), 
    axis.text = element_text(color = "white"), 
    plot.title = element_text(color = "red", hjust = 0.5, size = 22), 
    plot.subtitle = element_text(color = "white", hjust = 0.5), 
    legend.background = element_rect(fill = "grey0"), 
    legend.text = element_text(color = "red"), 
    legend.title = element_text(color = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"), 
    axis.line.y.left = element_line(colour = "red"),
    axis.line.x.bottom = element_line(colour = "red"),
    plot.caption = element_text(color = "white", hjust = 1), 
    axis.ticks = element_line(color = "red"), 
    text = element_text(family = "Showcard Gothic")
  ) +
  labs(title = paste("Expenditures on Horror Movies", emo::ji("witch")), subtitle = "Which months had the highest expenditures?", 
       fill = "Movie Ratings", caption = "By: Ente Kang", xlab = "Month", ylab = "Expenditures") +
  scale_fill_manual(values = harrypotter(n = 9, house = "Gryffindor")) +
  annotate(
    geom = "curve", x = 8, y = 600000000, xend = 9.2, yend =500000000, curvature = -.1, color = "white", 
    arrow = arrow(length = unit(3, "mm"))
  ) +
  annotate(
    geom = "text", x = 6, y = 600000000, label = "Very large expenditures on \nhorror films in October", color = "white", 
    family = "Vladimir Script", size = 7
  )
