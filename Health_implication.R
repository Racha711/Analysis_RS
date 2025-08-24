#Library
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(patchwork)


#Data
data <- read_csv("/Your_file_path/")

data2 <- data %>%
  filter(ep == "total", quant == "med", sex == "Both", fun %in% c("IER", "LogLinear"), age_range == "25+", !REMF %in% c("R5OECD90+EU","R5MAF","R5LAM","R5Asia","R5REF")) %>%
  mutate(region = case_when(
    REMF == "World" ~ "Global",
    REMF == "CHN" ~ "China",
    REMF == "IND" ~ "India",
    REMF == "XSE" ~ "Southeast Asia",
    REMF %in% c("XOC","XSA") ~ "Other Asia",
    REMF == "JPN" ~ "Japan",
    REMF %in% c("XAF","XNF") ~ "Africa",
    REMF %in% c("BRA","XLM") ~ "South America",
    REMF %in% c("CAN","USA") ~ "North America",
    REMF == "CIS" ~ "Former Soviet Union",
    REMF %in% c("XE25","XER","TUR") ~ "Europe",
    REMF == "XME" ~ "Middle East",
    TRUE ~ "Other"
  ))

color_custom <- c(
  "BaU" = "#000000",
  "BASELINE (HIST)" = "#7F7F7F",
  "SSP126" = "#43C6DB",
  "SSP245" = "#AAF200",
  "SSP585" = "#FF4162"
)

plot_data <- data2 %>%
  group_by(set_plt, Syr, scenario, region) %>%
  summarise(MtMbReg17 = sum(MtMbReg17), .groups = "drop") %>%
  filter(region == "Global") %>%
  mutate(
    scenario = case_when(
      scenario == "HIST" ~ "BASELINE (HIST)",
      scenario == "BAU" ~ "BaU",
      TRUE ~ scenario
    ),
    scenario = factor(scenario, levels = names(color_custom))
  )

make_panel <- function(pollutant, title_label) {
  subdata <- plot_data %>% filter(set_plt == pollutant) %>% mutate(MpY = MtMbReg17 / 1e6)
  label_data <- subdata %>%
    filter(scenario != "BaU") %>%
    left_join(subdata %>% filter(scenario == "BaU") %>% select(Syr, MpY) %>% rename(MpY_BaU = MpY), by = "Syr") %>%
    mutate(
      diff_pct = 100 * (MpY - MpY_BaU) / MpY_BaU,
      label = sprintf("%+0.1f%%", diff_pct),
      vjust_adj = case_when(
        scenario == "SSP585" ~ -2.5,
        scenario == "SSP245" ~ -1.5,
        scenario == "SSP126" ~ -0.5,
        scenario == "BASELINE (HIST)" ~ 1.2,
        TRUE ~ 0
      ),
      hjust_adj = -0.5
    )
  ylab_text <- bquote(atop("Number of Mortality", "(millions y"^{-1}*")"))
  p <- ggplot(subdata, aes(Syr, MpY, group = scenario, color = scenario)) +
    geom_line(data = subset(subdata, scenario == "BaU"), linewidth = 0.25, linetype = "dotdash") +
    geom_point(data = subset(subdata, scenario == "BaU"), size = 1.8, shape = 16) +
    geom_point(data = subset(subdata, scenario != "BaU"), size = 2.5, shape = 18) +
    geom_text(data = label_data, aes(label = label), fontface = "bold", size = 3, show.legend = FALSE,
              vjust = label_data$vjust_adj, hjust = label_data$hjust_adj) +
    scale_color_manual(values = color_custom, name = "Scenario") +
    scale_x_continuous(breaks = c(2015, 2030, 2050, 2100), expand = expansion(mult = c(0.09, 0.3))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = function(y) sprintf("%.1f", y)) +
    labs(x = "Year", y = ylab_text) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, hjust = 0.5, face = "bold"),
      axis.title.y = element_text(size = 14, hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white", color = "white"),
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_rect(linewidth = 1),
      panel.border = element_rect(color = "grey", fill = NA, linewidth = 1),
      axis.ticks.x = element_line(color = "grey42", linewidth = 0.4),
      axis.ticks.y = element_line(color = "grey42", linewidth = 0.4),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.25, linetype = "dashed")
    )
  title_plot <- ggplot() + annotate("text", x = 0, y = 0, label = title_label, size = 5, fontface = "bold", hjust = 0) + xlim(0, 1) + theme_void()
  wrap_elements(title_plot) / p + plot_layout(heights = c(0.08, 1))
}

pm25_panel <- make_panel("PM2.5", expression(bold("(a) Global PM"[2.5] * "-mortality")))
o3_panel   <- make_panel("O3", "(b) Global Ozone-mortality")

final_plot <- (pm25_panel | plot_spacer() | o3_panel) +
  plot_layout(widths = c(1, 0.05, 1), guides = "collect") &
  theme(legend.position = "bottom")


#Output
ggsave(
  filename = "/Users/racha711/OneDrive - Kyoto University/Racha/Dissertation/RESULT/SpatialHealth/Figure_6.tiff",
  plot = final_plot, width = 8.5, height = 4.5, dpi = 300, compression = "lzw", bg = "white"
)
