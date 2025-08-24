#===Library=====

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(maps)
library(grid)  
library(paletteer)

# Data 
data <- read.csv("/Users/racha711/Library/CloudStorage/OneDrive-KyotoUniversity/Racha/Dissertation/GCAP2/data/SurfaceConcDiff/output/Relative_change_Conc.csv")

#Label
species_label_list <- list(
  "PM25"        = quote(PM[2.5]),
  "O3"          = quote(O[3]),
  "AerMassSO4"  = quote(SO[4]),
  "AerMassNIT"  = quote(NIT),
  "AerMassNH4"  = quote(NH[4])
)

species_list <- unique(data$name)
plot_list <- list()


#Main loop 

for (sp in species_list) {
  
  subdata <- data %>%
    filter(name == sp) %>%
    mutate(
      period = factor(period, levels = c("The mid-century", "The end-of-century")),
      scenario = factor(scenario, levels = c("SSP126", "SSP245", "SSP585"))
    )
  
  legend_label <- if (tolower(sp) == "o3") {
    expression(Delta*" Concentration [ppbv]")
  } else {
    expression(Delta*" Concentration ["*mu*"g/m"^3*"]")
  }
  
  main_title <- substitute("Changes in "*x, list(x = species_label_list[[sp]]))

  max_val <- max(abs(subdata$value), na.rm = TRUE)
 
  if (sp == "PM25") {
    clip_val <- 7.5
    limits_val <- c(-7.5, 7.5)
    subdata <- subdata %>%
      mutate(value = pmax(pmin(value, clip_val), -clip_val))
    
  } else if (sp == "O3") {
    clip_val <- 3.5
    limits_val <- c(-3.5, 3.5)
    subdata <- subdata %>%
      mutate(value = pmax(pmin(value, clip_val), -clip_val))
    
  } else {
    clip_val <- 0.5
    limits_val <- c(-0.5, 0.5)
    subdata <- subdata %>%
      mutate(value = pmax(pmin(value, clip_val), -clip_val))
  }
  
  
  p <- ggplot(subdata, aes(x = lon, y = lat, fill = value)) +
    geom_tile() +
    borders("world", colour = "gray30", size = 0.3) +
    coord_fixed(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
    
    scale_fill_gradientn(
      colours = rev(as.vector(paletteer_c("ggthemes::Red-Blue-White Diverging", n = 15))),
      limits = limits_val,
      name = legend_label
    )+
    scale_x_continuous(
      breaks = seq(-150, 150, by = 30),
      labels = c("150W", "120W", "90W", "60W", "30W", "0", "30E", "60E", "90E", "120E", "150E")
    ) +
    scale_y_continuous(
      breaks = c(-60, -30, 0, 30, 60),
      labels = c("60S", "30S", "0", "30N", "60N")
    ) +
    
    facet_grid(rows = vars(scenario), cols = vars(period),switch = "y") +
    
    labs(
      title = NULL,
      x = NULL, y = NULL
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # centered title
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),
      strip.text = element_text(size = 14, face = "bold"),
      strip.placement = "outside",
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm"),
      legend.key.height  = unit(0.5, "cm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      axis.ticks.x = element_line(color = "grey40", linewidth = 0.4),
      axis.ticks.y = element_line(color = "grey40", linewidth = 0.4),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  plot_list[[sp]] <- p  
  
  #==== Output path ====
  output_path <- "/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/RESULT/Conc/"
  file_name <- paste0(output_path, "SpatialChange_", sp, ".tiff")
  
  ggsave(
    filename = file_name,
    plot = p,
    device = "tiff",
    width = 6,
    height = 5.5,
    dpi = 300,
    bg = "white"
  )
  
}
