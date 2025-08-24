library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(glue)
library(patchwork)
library(ggpubr)

data <- read.csv("/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/SpatialHealth/output/csv/Mortality_changes.csv")

data2 <- data %>% mutate(region = case_when(
  REMF == "World" ~ "Global",
  REMF == "CHN" ~ "China",
  REMF == "IND" ~ "India",
  REMF == "XSE" ~ "Southeast Asia",
  REMF %in% c("XOC","XSA") ~ "Other Asia",
  REMF == "JPN" ~ "Japan",
  REMF %in% c("XAF", "XNF") ~ "Africa",
  REMF %in% c("BRA", "XLM") ~ "South America",
  REMF %in% c("CAN", "USA") ~ "North America",
  REMF == "CIS" ~ "Former Soviet Union",
  REMF %in% c("XE25", "XER", "TUR") ~ "Europe",
  REMF == "XME" ~ "Middle East",
  TRUE ~ "Other"
))

data3 <- data2 %>%
  group_by(region,set_plt,scenario,period,sex,ep,quant,age_range) %>%
  summarise(value = round(sum(value_changed),0)) %>%
  mutate(
    x_num = if_else(period == "The mid-century", 1, 2),
    Med = if_else(region == "Global", value/1e6, value/1e3)
  )

write.csv(
  data3,
  "/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/SpatialHealth/output/csv/Finalized_Mortality_change.csv",
  row.names = FALSE
)

plot_data <- data3 %>%
  filter(age_range == "25+", ep == "total", sex == "Both", quant %in% c("med"))

region_units <- c(
  "Global"               = "(Millions of Deaths)",
  "China"                = "(1000s of Deaths)",
  "India"                = "(1000s of Deaths)",
  "Southeast Asia"       = "(1000s of Deaths)",
  "Other Asia"           = "(1000s of Deaths)",
  "Japan"                = "(1000s of Deaths)",
  "Africa"               = "(1000s of Deaths)",
  "Middle East"          = "(1000s of Deaths)",
  "Europe"               = "(1000s of Deaths)",
  "Former Soviet Union"  = "(1000s of Deaths)",
  "North America"        = "(1000s of Deaths)",
  "South America"        = "(1000s of Deaths)"
)

region_order <- names(region_units)
full_plot_list <- list()

for (poll in c("PM2.5", "O3")) {
  plots <- list()
  
  for (reg in region_order) {
    sub <- plot_data %>% filter(region == reg, set_plt == poll)
    ylab_text <- glue("Mortality Change\n{region_units[reg]}")
    
    p <- ggplot(sub, aes(x = x_num, y = Med, colour = scenario)) +
      geom_point(size = 2, shape = 18) +
      geom_vline(xintercept = 1.5, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
      geom_hline(yintercept = 0, linetype = "solid", colour = "grey50", linewidth = 0.1) +
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("The mid-century", "The end-of-century"),
        limits = c(0.76, 2.24),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      scale_colour_manual(
        name = "Scenario",
        values = c("SSP126" = "#43C6DB", "SSP245" = "#AAF200", "SSP585" = "#FF4162")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
      expand_limits(y = 0) +
      coord_cartesian(clip = "off") +
      labs(title = reg, x = "Period", y = ylab_text) +
      theme_minimal(base_size = 8) +
      theme(
        plot.title  = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.text   = element_text(size = 9),
        axis.title.y= element_text(size = 7),
        axis.title.x= element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 5.5),
        axis.text.y = element_text(size = 6.5),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.border     = element_rect(colour = "grey60", fill = NA),
        panel.grid       = element_blank(),
        axis.ticks.x     = element_line(colour = "grey42", linewidth = 0.4),
        axis.ticks.y     = element_line(colour = "grey42", linewidth = 0.4),
        panel.spacing    = unit(0.5, "lines")
      )
    plots[[reg]] <- p
  }
  
  full_plot <- wrap_plots(plots, ncol = 3, guides = "collect") &
    theme(legend.position = "bottom",
          legend.text   = element_text(size = 10),
          legend.title  = element_text(size = 12, face = "bold"))
  
  ggsave(
    filename = glue("/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/RESULT/SpatialHealth/New-MortalityChange_{poll}.tiff"),
    plot = full_plot,
    width = 6.5, height = 6,
    dpi = 300,
    compression = "lzw",
    bg = "white"
  )
  
  full_plot_list[[poll]] <- full_plot
}

title_plot_a <- ggplot() +
  annotate("text", x = 0, y = -50, label = expression(bold("(a) Mortality Changes due to PM"[2.5])), size = 5, fontface = "bold", hjust = 0) +
  xlim(0, 1) + theme_void() 

title_plot_b <- ggplot() +
  annotate("text", x = 0, y = 50, label = "(b) Mortality Changes due to Ozone", size = 5, fontface = "bold", hjust = 0) +
  xlim(0, 1) + theme_void() 

pm25_panel <- wrap_elements(title_plot_a) / full_plot_list[["PM2.5"]] + plot_layout(heights = c(0.08, 1))
o3_panel   <- wrap_elements(title_plot_b) / full_plot_list[["O3"]] + plot_layout(heights = c(0.08, 1))

final_combined <- ggarrange(pm25_panel, o3_panel, nrow = 1, ncol = 2)

ggsave(
  filename = "/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/RESULT/SpatialHealth/Figure_5.tiff",
  plot = final_combined,
  width = 10, height = 6.5,
  dpi = 300, compression = "lzw", bg = "white"
)
