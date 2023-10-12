rm(list = ls())

library(ggplot2)
library(ggforce)
library(ggtext)
library(ggimage)


buffer_length <- 73
buffer_width <- 110

pitch_length <- 68
pitch_width <- 105

# Number of lanes
n_lanes <- 10

# Width of each lane
lane_width <- pitch_width / n_lanes

lanes <- data.frame(
  ymin = seq(0, pitch_width - lane_width, by = lane_width),
  ymax = seq(lane_width, pitch_width, by = lane_width),
  xmin = rep(0, n_lanes),
  xmax = rep(pitch_length, n_lanes),
  fill = rep(c("#BDCB9B", "#CDD8B6"), length.out=10)
)

generate_positions <- function(system) {
  if (sum(system) != 11) {
    stop("The total number of players must be 11!")
  }
  
  goalkeeper_y <- 3  # Goalkeeper's main position will be along y-axis
  goalkeeper_x <- pitch_length / 2  # Their x-coordinate is in the middle of the width
  
  players_y <- c(goalkeeper_y)
  players_x <- c(goalkeeper_x)
  
  lines <- c(5, 15, 30, 45)  # positions for the different lines on the pitch
  
  # Remove the goalkeeper from the system
  system <- system[-1]
  
  for (i in 1:length(system)) {
    line_y <- lines[i+1]  # +1 because we removed goalkeeper
    num_players <- system[i]
    
    # Compute half the total width that players on this line would occupy
    half_width <- (num_players - 1) * (pitch_length / (num_players + 1)) / 2
    
    # Space players evenly, centered around the midpoint of the pitch
    space_x <- seq(pitch_length / 2 - half_width, pitch_length / 2 + half_width, length.out = num_players)
    
    players_y <- c(players_y, rep(line_y, num_players))
    players_x <- c(players_x, space_x)
  }
  
  df <- data.frame(x = players_x, y = players_y, number = 1:11)
  return(df)
}


player_names_A <- c("Vicario", "Porro", "Romero", "van de Ven", "Udogie", 
                  "Maddison", "Bissouma", "Sarr", "Kulusevski", "Son (c)", "Johnson")

player_names_B <- c("Raya", "White", "Saliba", "Gabriel", "Zinchenko", 
                    "Rice", "Vieira", "Odegaard", "Saka", "Jesus", "Nketiah")


# Assuming the system is: 1 goalkeeper, 4 defenders, 3 midfielders, and 3 forwards
system <- c(1, 4, 3, 3)  

teamA_positions <- generate_positions(system)
teamA_positions$name <- player_names_A
teamA_positions

teamB_positions <- teamA_positions
teamB_positions$y <- pitch_width - teamA_positions$y  # Reflect for Team B along the y-axis
teamB_positions$name <- player_names_B
teamB_positions

# # Example:
# system <- c(1, 3, 4, 3)  # 1 goalkeeper, 3 defenders, 4 midfielders, 3 forwards
# teamA_positions <- generate_positions(system)
# teamB_positions <- teamA_positions

library(ggtext)

main_plot <- 
ggplot() + 
  geom_rect(aes(ymin = -5, ymax = buffer_width, xmin = -5, xmax = buffer_length),
            fill = "#BDCB9B", colour = NA) +
  
  # Boundary lines
  geom_segment(aes(y = 0, x = 0, yend = pitch_width, xend = 0), color = "white", size = 1) +
  geom_segment(aes(y = pitch_width, x = 0, yend = pitch_width, xend = pitch_length), color = "white", size = 1) +
  geom_segment(aes(y = 0, x = 0, yend = 0, xend = pitch_length), color = "white", size = 1) +
  geom_segment(aes(y = 0, x = pitch_length, yend = pitch_width, xend = pitch_length), color = "white", size = 1) +
  
  # Add lanes
  geom_rect(data = lanes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE) +
  scale_fill_manual(values = lanes$fill) +
  
  # Goals
  geom_rect(aes(ymin = 0, ymax = 4, xmin = 29, xmax = 39), color = "white", fill = NA) +
  geom_rect(aes(ymin = pitch_width, ymax = pitch_width-4, xmin = 29, xmax = 39), color = "white", fill = NA) +
  
  # Penalty areas
  geom_rect(aes(ymin = 0, ymax = 16.5, xmin = 9.15, xmax = 58.85), color = "white", fill = NA) +
  geom_rect(aes(ymin = pitch_width-16.5, ymax = pitch_width, xmin = 9.15, xmax = 58.85), color = "white", fill = NA) +
  
  # Centre circle and line
  geom_circle(aes(y0 = pitch_width/2, x0 = pitch_length/2, r = 9.15), color = "white", fill = NA) +
  geom_segment(aes(y = pitch_width/2, x = 0, yend = pitch_width/2, xend = pitch_length), color = "white") +
  coord_fixed(ratio = 1) +
  theme_void() +
  theme(legend.position = "none") +
  geom_point(data = teamA_positions, aes(x = x, y = y), color = "#D2495B", size = 7, alpha=0.95) +
  geom_text(data = teamA_positions, aes(x = x, y = y, label = name), vjust = 3, size = 5) +
  
  geom_point(data = teamB_positions, aes(x = x, y = y), color = "#2E4157", size = 7, alpha=0.95) +
  geom_text(data = teamB_positions, aes(x = x, y = y, label = name), vjust = -2, size = 5) +
  
  labs(title = "<span style='color:#D2495B'><b>Arsenal</b></span> Vs <span style='color:#2E4157'><b>Tottenham</b></span>") +
theme(text=element_text(family = "Roboto"),
        plot.title = element_markdown(margin = margin(t = 20, r = 0, b = 0, l = 0), hjust = 0.5, size=15),
        plot.background = element_rect(fill = "#E5E5E5", colour = "#E5E5E5"),
        panel.background = element_rect(fill ="#E5E5E5", colour = "#E5E5E5")) +
  coord_fixed(xlim = c(-20, 93), ylim = c(-20,110))

main_plot

library(here)
setwd(here("graphic_elements/"))

emblems <- data.frame(
  x = c(-15,  88), # Adjust the X position based on where you want the images
  y = c(pitch_width * 0.75, pitch_width * 0.25), # Positions for Arsenal and Tottenham emblems respectively
  image = c("tottenham.png","arsenal.png"))


transparent <- function(img) {
  magick::image_fx(img, expression = "0.8*a", channel = "alpha")
}

main_plot +
  geom_image(data = emblems, aes(x = x, y = y, image = image),
             image_fun = transparent,
             size = 0.09) # Adjust the size as needed

coach_names <- data.frame(
  x = c(-15, -15), # Assuming you're using the same X positions as the emblems
  y = c(pitch_width * 0.75 - 19 , pitch_width * 0.25 - 12), # Adjust the Y position to place it below the emblems
  label = c("Ange\nPostecoglou","M. Arteta"))

main_plot + 
  geom_image(data = emblems, aes(x = x, y = y, image = image), size = 0.09) + # Adjust the size as needed
  geom_text(data = coach_names, aes(x = x, y = y, label = label), size = 2.75, family="Roboto", hjust = 0.5, color = "#333333")


  plot1 <- 
main_plot +
  geom_segment(aes(x = -15, xend = pitch_length +15, y = -17.5, yend = -17.5), color = "#293241", size = 0.5) 



main_plot +
  geom_segment(aes(x = 85, xend = 85, y = -4, yend = buffer_width), color = "#293241", size = 1)

  






