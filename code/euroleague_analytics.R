library(googlesheets4)
library(tidyverse)
library(ggpubr)       # post Pearson results


gs4_auth()


url <-"https://docs.google.com/spreadsheets/d/1Wh-vbWIRvdapRjFF28FsRbU9wNXJGj9XaiHavWHY6YE/edit?pli=1#gid=135021531"
data <- range_read(ss = url, range = "week1_2324")

head(data)

top_10_players <- data %>%
    arrange(desc(FPT)) %>%
    head(10) %>%
    pull(Player)
  
data$label <- ifelse(data$Player %in% top_10_players, as.character(data$Player), NA)

library(ggrepel)

data %>% 
  ggplot(label=label) +
  geom_jitter(aes(x=CR,y=FPT)) +
  stat_smooth(aes(x=CR,y=FPT),
              method = "lm",
              formula = y ~ x-1,
              geom = "smooth", color="#333333", linetype="dashed") +
  stat_cor(aes(x=CR,y=FPT),method = "pearson") +
  geom_label_repel(aes(x=CR,y=FPT),label=data$label) +
theme_bw()


  # Scatter plot with labels for top 10 players based on FPT
  ggplot(data, aes(x = PTS, y = REB, label = label)) +
    geom_point() +
    geom_text(vjust = -1, hjust = 1) +  # adjust label position to avoid overlapping with points
    theme_minimal()
  Here, we've plotted PTS vs REB, but you can adjust the x and y aesthetics based on what you want to visualize. The key players with the top 10 FPT values will have their names labeled on the plot. Adjust the vjust and hjust arguments in geom_text() to position the labels as needed.






