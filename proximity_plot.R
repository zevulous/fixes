library(ggplot2)
library(ggrepel)

# ================================================================
# USER INPUT: Define your points here
# Format: c("Name", Option1%, Option2%, Option3%)
# Percentages should sum to 100
# ================================================================

data_points <- list(
  c("Point A", 15, 35, 50),
  c("Point B", 5, 30, 65),
  c("Point C", 75, 20, 5),
  c("Point D", 33, 33, 34),
  c("Point E", 90, 5, 5),
  c("Point F", 10, 80, 10),
  c("Point G", 5, 10, 85)
)

# Option labels (customize these)
option_labels <- c("Option 1", "Option 2", "Option 3")

# ================================================================
# SETTINGS (optional tweaks)
# ================================================================
show_labels    <- TRUE    # Show point names
label_size     <- 3       # Size of point labels
point_size     <- 5       # Size of data points
color_power    <- 1.5     # Color blending intensity (higher = more saturated near corners)
gamma          <- 0.85    # Gamma correction for colors

# Base colors for each option
color_opt1 <- "#3182bd"   # Blue
color_opt2 <- "#31a354"   # Green
color_opt3 <- "#e6550d"   # Orange

# ================================================================
# GEOMETRY (no need to modify)
# ================================================================
top    <- c(0.5, sqrt(3) / 2)
left   <- c(0, 0)
right  <- c(1, 0)
center <- c(0.5, sqrt(3) / 6)

triangle <- data.frame(
  x = c(left[1], right[1], top[1]),
  y = c(left[2], right[2], top[2])
)

# ================================================================
# PROCESS DATA
# ================================================================
# Parse input into data frame
points <- do.call(rbind, lapply(data_points, function(p) {
  data.frame(
    name = p[1],
    w1   = as.numeric(p[2]) / 100,
    w2   = as.numeric(p[3]) / 100,
    w3   = as.numeric(p[4]) / 100,
    stringsAsFactors = FALSE
  )
}))

# Calculate position from weights (barycentric placement)
points$x <- points$w1 * top[1] + points$w2 * left[1] + points$w3 * right[1]
points$y <- points$w1 * top[2] + points$w2 * left[2] + points$w3 * right[2]

# Calculate Euclidean distances to corners (for color)
dist_fn <- function(px, py, vx, vy) sqrt((px - vx)^2 + (py - vy)^2)

points$d1 <- dist_fn(points$x, points$y, top[1], top[2])
points$d2 <- dist_fn(points$x, points$y, left[1], left[2])
points$d3 <- dist_fn(points$x, points$y, right[1], right[2])

# Distance → color weights
eps <- 1e-6
inv_d <- 1 / (points[, c("d1", "d2", "d3")]^color_power + eps)
color_weights <- inv_d / rowSums(inv_d)

# Parse base colors
col1 <- col2rgb(color_opt1) / 255
col2 <- col2rgb(color_opt2) / 255
col3 <- col2rgb(color_opt3) / 255

# Blend colors
points$color <- rgb(
  (color_weights[,1] * col1[1] + color_weights[,2] * col2[1] + color_weights[,3] * col3[1])^gamma,
  (color_weights[,1] * col1[2] + color_weights[,2] * col2[2] + color_weights[,3] * col3[2])^gamma,
  (color_weights[,1] * col1[3] + color_weights[,2] * col2[3] + color_weights[,3] * col3[3])^gamma
)

# Corner markers
corners <- data.frame(
  x     = c(top[1], left[1], right[1]),
  y     = c(top[2], left[2], right[2]),
  color = c(color_opt1, color_opt2, color_opt3),
  label = option_labels
)

# ================================================================
# PLOT
# ================================================================
p <- ggplot() +
  
  # Triangle fill
  geom_polygon(
    data = triangle,
    aes(x, y),
    fill = "#fafafa",
    color = NA
  ) +
  
  # Triangle outline
  geom_polygon(
    data = triangle,
    aes(x, y),
    fill = NA,
    color = "#404040",
    linewidth = 1.2
  ) +
  
  # Center marker
  geom_point(
    aes(x = center[1], y = center[2]),
    shape = 21,
    size = 3,
    fill = "white",
    color = "#999999",
    stroke = 0.8
  ) +
  
  # Data points
  geom_point(
    data = points,
    aes(x, y),
    fill = points$color,
    color = "white",
    shape = 21,
    size = point_size,
    stroke = 1
  ) +
  
  # Corner markers
  geom_point(
    data = corners,
    aes(x, y),
    fill = corners$color,
    color = "white",
    shape = 21,
    size = 4,
    stroke = 1
  ) +
  
  # Corner labels
  annotate("text", x = top[1], y = top[2] + 0.08,
           label = option_labels[1], fontface = "bold",
           size = 5, color = color_opt1) +
  annotate("text", x = left[1], y = left[2] - 0.07,
           label = option_labels[2], fontface = "bold",
           size = 5, hjust = 0.5, color = color_opt2) +
  annotate("text", x = right[1], y = right[2] - 0.07,
           label = option_labels[3], fontface = "bold",
           size = 5, hjust = 0.5, color = color_opt3) +
  
  coord_equal(
    xlim = c(-0.15, 1.15),
    ylim = c(-0.15, 1.05)
  ) +
  
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20)) +
  
  # Title and subtitle
  labs(
    title    = "Proximity-Based Decision Space",
    subtitle = "Point color reflects Euclidean distance to each option"
  ) +
  theme(
    plot.title = element_text(
      hjust   = 0.5,
      size    = 18,
      face    = "bold",
      color   = "#333333",
      margin  = margin(b = 5)
    ),
    plot.subtitle = element_text(
      hjust   = 0.5,
      size    = 11,
      color   = "#777777",
      margin  = margin(b = 15)
    )
  )

# Add point labels if enabled
if (show_labels) {
  p <- p + geom_label_repel(
    data = points,
    aes(x = x, y = y, label = name),
    size = label_size,
    color = "#333333",
    fill = "white",
    label.size = 0.2,
    label.padding = unit(0.2, "lines"),
    box.padding = unit(0.4, "lines"),
    point.padding = unit(0.3, "lines"),
    segment.color = "#999999",
    segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 42
  )
}

# Display
print(p)