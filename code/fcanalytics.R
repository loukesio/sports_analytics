#__________________________
# ┬  ┬┌┐ ┬─┐┌─┐┬─┐┬┌─┐┌─┐
# │  │├┴┐├┬┘├─┤├┬┘│├┤ └─┐
# ┴─┘┴└─┘┴└─┴ ┴┴└─┴└─┘└─┘
#_________________________

dev.off()

library(ggplot2)
library(ggforce)
library(ggimage)


setwd("/Users/theodosiou/Library/Mobile Documents/com~apple~CloudDocs/")


#_________________________________________________________________
# ┌─┐┬─┐┌─┐┌─┐┌┬┐┌─┐  ┌─┐┬┌┬┐┌─┐┬ ┬  ┌─┐┌┐┌┌┬┐  ┬  ┌─┐┌┐┌┌─┐┌─┐
# │  ├┬┘├┤ ├─┤ │ ├┤   ├─┘│ │ │  ├─┤  ├─┤│││ ││  │  ├─┤│││├┤ └─┐
# └─┘┴└─└─┘┴ ┴ ┴ └─┘  ┴  ┴ ┴ └─┘┴ ┴  ┴ ┴┘└┘─┴┘  ┴─┘┴ ┴┘└┘└─┘└─┘
#_________________________________________________________________

#buffer area 

buffer_length <- 110
buffer_width <- 73

### football pitch dimensions 
pitch_length <- 105
pitch_width <- 68

# Number of lanes
n_lanes <- 10

# Width of each lane
lane_width <- pitch_length / n_lanes

# Create alternating lane colors
lanes <- data.frame(
  xmin = seq(0, pitch_length - lane_width, by = lane_width),
  xmax = seq(lane_width, pitch_length, by = lane_width),
  ymin = rep(0, n_lanes),
  ymax = rep(pitch_width, n_lanes),
  fill = rep(c("green3", "green4"), length.out=10)
)

#___________________________________________________
# ┌─┐┌┬┐┌┬┐  ┌┬┐┌─┐┌─┐┌┬┐  ┌─┐┌─┐┌─┐┬┌┬┐┬┌─┐┌┐┌┌─┐
# ├─┤ ││ ││   │ ├┤ ├─┤│││  ├─┘│ │└─┐│ │ ││ ││││└─┐
# ┴ ┴─┴┘─┴┘   ┴ └─┘┴ ┴┴ ┴  ┴  └─┘└─┘┴ ┴ ┴└─┘┘└┘└─┘
#___________________________________________________

generate_positions <- function(system) {
  if (sum(system) != 11) {
    stop("The total number of players must be 11!")
  }
  
  goalkeeper_x <- 2.5
  goalkeeper_y <- pitch_width / 2
  
  players_x <- c(goalkeeper_x)
  players_y <- c(goalkeeper_y)
  
  lines <- c(2.5, 10, 25, 40)  # positions for the different lines on the pitch
  
  # Remove the goalkeeper from the system
  system <- system[-1]
  
  for (i in 1:length(system)) {
    line_x <- lines[i+1]  # +1 because we removed goalkeeper
    num_players <- system[i]
    
    # Space players evenly
    space_y <- seq(pitch_width / (num_players + 1), pitch_width - pitch_width / (num_players + 1), length.out = num_players)
    
    players_x <- c(players_x, rep(line_x, num_players))
    players_y <- c(players_y, space_y)
  }
  
  df <- data.frame(x = players_x, y = players_y, number = 1:11)
  return(df)
}

# Example:
system <- c(1, 3, 4, 3)  # 1 goalkeeper, 3 defenders, 4 midfielders, 3 forwards
teamA_positions <- generate_positions(system)
teamB_positions <- teamA_positions
teamB_positions$x <- pitch_length - teamB_positions$x  # Reflect for Team B
teamA_positions

#_________________________________________
# ╔═╗┬  ┌─┐┌┬┐  ┌┬┐┬ ┬┌─┐  ╔═╗┬┌┬┐┌─┐┬ ┬
# ╠═╝│  │ │ │    │ ├─┤├┤   ╠═╝│ │ │  ├─┤
# ╩  ┴─┘└─┘ ┴    ┴ ┴ ┴└─┘  ╩  ┴ ┴ └─┘┴ ┴
#__________________________________________

# plot <-

ggplot() + 
  geom_rect(aes(xmin = 0, xmax = buffer_length, ymin = 0, ymax = buffer_width),
            fill = "#BDCB9B", colour = NA) +
  coord_fixed(ratio = 1, xlim = c(0, 140), ylim = c(0, 80)) +
  theme_void()

coord_fixed(ylim = c(-60, 70))





ggplot() + 
  geom_rect(aes(xmin = -5, xmax = buffer_length, ymin = -5, ymax = buffer_width),
            fill = "#BDCB9B", colour = NA) +
  # Bottom boundary line
  geom_segment(aes(x = 0, y = 0, xend = pitch_length, yend = 0), color = "white", size = 1) +
  # Top boundary line
  geom_segment(aes(x = 0, y = pitch_width, xend = pitch_length, yend = pitch_width), color = "white", size = 1) +
  # Left boundary line
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = pitch_width), color = "white", size = 1) +
  # Right boundary line
  geom_segment(aes(x = pitch_length, y = 0, xend = pitch_length, yend = pitch_width), color = "white", size = 1) +
  geom_rect(data = lanes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
            color = NA) +
  scale_fill_manual(values = lanes$fill) +
  theme_void() +
  theme(legend.position = "none") +
  # Goals
  geom_rect(aes(xmin = 0, xmax = 4, ymin = 29, ymax = 39), color = "white", fill = NA) +
  geom_rect(aes(xmin = pitch_length, xmax = pitch_length-4, ymin = 29, ymax = 39), color = "white", fill = NA) +
  # Penalty areas
  geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 9.15, ymax = 58.85), color = "white", fill = NA) +
  geom_rect(aes(xmin = pitch_length-16.5, xmax = pitch_length, ymin = 9.15, ymax = 58.85), color = "white", fill = NA) +
  # Centre circle and line
  geom_circle(aes(x0 = pitch_length/2, y0 = pitch_width/2, r = 9.15), color = "white", fill = NA) +
  geom_segment(aes(x = pitch_length/2, y = 0, xend = pitch_length/2, yend = pitch_width), color = "white") +
  coord_fixed(ratio = 1) +
  geom_point(data = teamA_positions, aes(x = x, y = y), color = "#D2495B", size = 10, alpha=0.95) +
  geom_point(data = teamB_positions, aes(x = x, y = y), color = "#2E4157", size = 10, alpha=0.95) +
  geom_text(data = teamA_positions, aes(x = x, y = y, label = number), color = "white", size = 6)+
  geom_text(data = teamB_positions, aes(x = x, y = y, label = number), color = "white", size = 6) +
  ggtitle("Arsenal vs Tottenham") +
  theme(plot.title = element_text(hjust = 0.5, size=18),
        plot.background = element_rect(fill = "#E5E5E5", colour = "#E5E5E5"),
        panel.background = element_rect(fill ="#E5E5E5", colour = "#E5E5E5")) +  # Center the title
  coord_fixed(xlim = c(-5, 110), ylim = c(-60, 80))

print(plot)



#________________________________________________________
# ┌─┐┬─┐┌─┐┌─┐┌─┐┬─┐┌─┐  ┌┬┐┬ ┬┌─┐  ┬  ┌─┐┌─┐┌─┐┌┐┌┌┬┐
# ├─┘├┬┘├┤ ├─┘├─┤├┬┘├┤    │ ├─┤├┤   │  ├┤ │ ┬├┤ │││ ││
# ┴  ┴└─└─┘┴  ┴ ┴┴└─└─┘   ┴ ┴ ┴└─┘  ┴─┘└─┘└─┘└─┘┘└┘─┴┘
#________________________________________________________

# Sample player names (replace with your own list of names)
teamA_names <- paste("Player A", 1:11)
teamB_names <- paste("Player B", 1:11)

# Adjusting the dataframe for players' legend positioning with increased distance between dots
spacing = 5  # Increase this value for greater spacing

teamA_legend_positions_numbers <- data.frame(
  x = rep(1, 11),                                # x positions for Team A numbers (in the middle of circles)
  y = seq(-8, -8 - 10 * spacing, length.out = 11),  # y positions for numbers
  label = 1:11
)

teamA_legend_positions_names <- data.frame(
  x = rep(4, 11),                                # x positions for Team A names (right of circles)
  y = seq(-8, -8 - 10 * spacing, length.out = 11),  # y positions for names
  label = teamA_names
)


teamB_legend_positions_numbers <- data.frame(
  x = rep(104, 11),                                # x positions for Team B numbers (in the middle of circles)
  y = seq(-8, -8 - 10 * spacing, length.out = 11),  # y positions for numbers
  label = 1:11
)

teamB_legend_positions_names <- data.frame(
  x = rep(101, 11),                                # x positions for Team B names (left of circles)
  y = seq(-8, -8 - 10 * spacing, length.out = 11),  # y positions for names
  label = teamB_names
)

#____________________________________________________
# ┌─┐┌┐┌┌┐┌┌─┐┌┬┐┌─┐┌┬┐┌─┐  ┌┬┐┬ ┬┌─┐  ┌─┐┬  ┌─┐┌┬┐
# ├─┤│││││││ │ │ ├─┤ │ ├┤    │ ├─┤├┤   ├─┘│  │ │ │ 
# ┴ ┴┘└┘┘└┘└─┘ ┴ ┴ ┴ ┴ └─┘   ┴ ┴ ┴└─┘  ┴  ┴─┘└─┘ ┴ 
#____________________________________________________

plot2 <-
plot +
  # Adding circles for player numbers in the legend
  geom_point(data = teamA_legend_positions_numbers, aes(x = x, y = y), color = "#EF0107", size = 4) +
  
  # Adding player numbers inside those circles for Team A
  geom_text(data = teamA_legend_positions_numbers, aes(x = x, y = y, label = label), color="white", size = 2.75) +

  # Adding player names next to the circles for Team A
  geom_text(data = teamA_legend_positions_names, aes(x = x, y = y, label = label),  hjust = 0, size = 2.75) +
  
  # Adding circles for player numbers in the Team B legend
  geom_point(data = teamB_legend_positions_numbers, aes(x = x, y = y), color = "#132257", size = 4) +
  
  # Adding player numbers inside those circles for Team B
  geom_text(data = teamB_legend_positions_numbers, aes(x = x, y = y, label = label),color="white", size =2.75) +
  
  # Adding player names to the left of the circles for Team B
  geom_text(data = teamB_legend_positions_names, aes(x = x, y = y, label = label), hjust = 1, size = 2.75) +
  geom_segment(aes(x = pitch_length/2, xend = pitch_length/2, y = -1, yend = -61), linetype = "dotted", color = "#333333", size = 1) +
  theme(text=element_text(family = "Roboto"),
        plot.title = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
coord_fixed(ylim = c(-60, 70))   # Expanding the y-axis limit to accommodate the legend

plot2

add_event_to_plot <- function(plot, minute, team, image_path) {
  
  # Compute the y-coordinate based on the given minute for the image
  y_image_minute <- -1 - minute/1.5
  y_text_minute <- y_image_minute # for the text, it's at the same y-position as the image
  
  # Adjust x for image and text based on the team
  if (team == "teamA") {
    x_image_minute <- pitch_length/2 - 4
    x_text_minute <- x_image_minute - (2.5 * (pitch_length/pitch_width)) # Adjusted based on x:y ratio
  } else if (team == "teamB") {
    x_image_minute <- pitch_length/2 + 4
    x_text_minute <- x_image_minute + (2.5 * (pitch_length/pitch_width)) # Adjusted based on x:y ratio
  }
  
  goal_data <- data.frame(x = x_image_minute, y = y_image_minute, image = image_path)
  minute_data <- data.frame(x = x_text_minute, y = y_text_minute, label = paste0(minute, "'"))
  
  plot + 
    geom_image(data = goal_data, aes(x = x, y = y, image = image), size = 0.02) +
    geom_text(data = minute_data, aes(x = x, y = y, label = label), color = "#333333", size = 3.5, fontface = "bold")
}

# Sample usage
plot <- add_event_to_plot(plot2, 46, "teamA", "1000px-Soccerball.svg.png")
plot
plot <- add_event_to_plot(plot2, 4, "teamA", "red_card.png")
plot

plot <- add_event_to_plot(plot, 50, "teamB", "1000px-Soccerball.svg.png")
plot



library(purrr)

add_multiple_events_to_plot <- function(plot, event_data) {
  
  walk(1:nrow(event_data), function(i) {
    plot <<- add_event_to_plot(plot, event_data$minute[i], event_data$team[i], event_data$image_path[i])
  })
  
  return(plot)
}

# Sample usage:

event_data <- data.frame(
  minute = c(46, 50, 60),
  team = c("teamA", "teamB", "teamA"),
  image_path = c("red_card.png", "yellow_card.png", "red_card.png")
)

plot <- add_multiple_events_to_plot(plot, event_data)
plot


team_logos_data <- data.frame(
  x = c(pitch_length/4, 3*pitch_length/4),  # Adjust these values to place the logos as per your requirement
  y = rep(75, 2),                           # Y-coordinate just below the title
  image = c("arsenal_logo.png", "tottenham.png")
)

plot + 
  geom_image(data = team_logos_data, aes(x = x, y = y, image = image), size = 0.07) +
  theme(text=element_text(family = "Roboto"),
        plot.title = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), size=20)) +
  coord_fixed(ylim = c(-60, 70))









