#Library
library(ggplot2)
library(dplyr)
library(glue)
library(patchwork)
library(readr)

#==============================
#. Load Data and Setup
#==============================

data <- read.csv("/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/GCAP2/data/SurfaceConcDiff/output/Relative_change_Region2.csv")

# Custom unit labels
all_units <- setNames(
  ifelse(unique(data$name) == "O3", "ppbv", "µg m-3"),
  unique(data$name)
)

# Order settings
unit_order <- names(all_units)

region_order <- c("Global", "China", "India", "Southeast Asia","Other Asia","Japan", "Africa", "Middle East",
                  "Europe","Former Soviet Union", "North America", "South America")

plot_data <- data %>%
  mutate(
    region = factor(region, levels = region_order),
    period = factor(period, levels = c("The mid-century", "The end-of-century")),
    x_num = if_else(period == "The mid-century", 1, 2)
  )

#==============================
#. Plotting
#==============================
poll_order <- c("PM25", "O3", "AerMassSO4", "AerMassNIT", "AerMassNH4")
full_plot_list <- list()

for (i in seq_along(poll_order)) {
  poll <- poll_order[i]
  letter_tag <- letters[i]
  plots <- list()
  
  for (reg in region_order) {
    sub <- plot_data %>% filter(region == reg, name == poll)
    
    ylab_text <- glue("Concentration Change\n({all_units[poll]})")
    
    p <- ggplot(sub, aes(x = x_num, y = value_avg, color = scenario)) +
      geom_point(size = 2.3, shape = 18) +
      geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50", linewidth = 0.4) +
      geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.1) +
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("The mid-century", "The end-of-century"),
        limits = c(0.76, 2.24),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      scale_color_manual(
        name = "Scenario", 
        values = c("SSP126" = "#43C6DB", "SSP245" = "#AAF200", "SSP585" = "#FF4162")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
      expand_limits(y = 0) +
      coord_cartesian(clip = "off") +
      labs(
        title = reg,
        x = "Period",
        y = ylab_text
      ) +
      theme_minimal(base_size = 8) +
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 9),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 8, face = "bold"),
        axis.text.x  = element_text(size = 5.5, angle = 0, hjust = 0.5),
        axis.text.y  = element_text(size = 6.5),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.border = element_rect(color = "grey60", fill = NA),
        panel.grid = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        axis.ticks.x = element_line(color = "grey42", linewidth = 0.4),
        axis.ticks.y = element_line(color = "grey42", linewidth = 0.4),
        panel.spacing = unit(0.5, "lines")
      )
    
    plots[[reg]] <- p
  }
  
  # Create pretty title
  poll_label <- if (poll == "PM25") {
    bquote("PM"[2.5])
  } else if (poll == "O3") {
    "Ozone"
  } else if (poll == "AerMassSO4") {
    bquote("SO"[4])
  } else if (poll == "AerMassNIT") {
    "NIT"
  } else if (poll == "AerMassNH4") {
    bquote("NH"[4])
  } else {
    poll
  }
  
  full_plot <- wrap_plots(plots, ncol = 3, guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold")
    )
  
  full_plot <- full_plot +
    plot_annotation(
      title = bquote("(" * .(letter_tag) * ") Changes in " * .(poll_label)),
      theme = theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
    )
  
  ggsave(
    filename = glue("/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/RESULT/Conc/ConcChanges_{poll}.tiff"),
    plot = full_plot,
    width = 6.5, height = 6,
    dpi = 600,
    compression = "lzw",
    bg = "white"
  )
  
  full_plot_list[[poll]] <- full_plot
}
