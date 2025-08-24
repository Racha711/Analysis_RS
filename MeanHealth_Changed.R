library(tidyverse)
library(patchwork)
library(glue)

data <- read.csv("/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/SpatialHealth/output/csv/Mortality_changes.csv")

data2 <- data %>% filter(
  ep == "total",
  sex == "Both",
  quant == "med",
  fun %in% c("IER" ,"LogLinear"),
  age_range == "25+",
  #REMF == "World"
  !REMF %in% c("R5OECD90+EU","R5MAF","R5LAM","R5Asia","R5REF","XOC")
) %>%
  mutate(region = case_when(
    REMF == "World" ~ "Global",
    # High-population countries
    REMF == "CHN" ~ "China",
    REMF == "IND" ~ "India",
    # Separated Asia regions
    REMF == "JPN" ~ "Japan",
    REMF == "XSE" ~ "Southeast Asia",
    REMF %in% c("XSA","XOC") ~ "Other Asia & Oceania",
    # Africa
    REMF %in% c("XAF", "XNF") ~ "Africa",
    # Americas
    REMF %in% c("BRA", "XLM") ~ "South America",
    REMF %in% c("CAN", "USA") ~ "North America",
    # Europe
    REMF %in% c("XE25", "XER", "CIS", "TUR") ~ "Europe",
    # Middle East
    REMF == "XME" ~ "Middle East",
    # Fallback group
    TRUE ~ "Other"
  ))

# Custom unit labels
region_units <- c(
  "Global" = "(Millions of Deaths)",
  "China" = "(1000s of Deaths)",
  "India" = "(1000s of Deaths)",
  "Middle East" = "(1000s of Deaths)",
  "Japan" = "(1000s of Deaths)",
  "Southeast Asia" = "(1000s of Deaths)",
  "Other Asia & Oceania" = "(1000s of Deaths)",
  "Africa" = "(1000s of Deaths)",
  "Europe" = "(1000s of Deaths)",
  "North America" = "(1000s of Deaths)",
  "South America" = "(1000s of Deaths)"
)

region_order <- names(region_units)

# Prepare plot_data like before
plot_data <- data2 %>% group_by(set_plt,scenario,ep,period,region) %>%
  summarise(value_changed = sum(value_changed)) %>%
  mutate(
    region = factor(region, levels = region_order),
    MpY = if_else(region == "Global", value_changed / 1e6, value_changed / 1e3),
    period = factor(period, levels = c("The mid-century", "The end-of-century")),
    x_num = if_else(period == "The mid-century", 1, 2)
  )


#===== Prepare plots =====

full_plot_list <- list()

for (poll in c("PM2.5", "O3")) {
  plots <- list()
  
  for (reg in region_order) {
    sub <- plot_data %>% filter(region == reg, set_plt == poll)
    
    ylab_text <- glue("Mortality Change\n{region_units[reg]}")
    
    p <- ggplot(sub, aes(x = x_num, y = MpY, color = scenario)) +
      geom_point(size = 2.3, shape = 18) +
      geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50", linewidth = 0.4) +
      geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.1) +
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("The mid-century", "The end-of-century"),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      scale_color_manual(
        name = "Scenario", 
        values = c("SSP126" = "#43C6DB", "SSP245" = "#AAF200", "SSP585" = "#FF4162")) +
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
  
  