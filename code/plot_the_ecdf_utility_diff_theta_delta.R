
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

MODEL_1 = "(0.05,1)"
MODEL_2 = "(0.05,0.5)"

default_intervention_size = 10
cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
# load data
# st <- read.csv(
#   paste(cwd,"/data/banerjee-combined-data/output/banerjee_combined_edgelist_spreading_data_dump.csv",sep=""),
#   stringsAsFactors = FALSE
# )

banerjee_combined_data_0.3 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(0.3).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_0.3 <- cbind ( banerjee_combined_data_0.3, DELTA="0.3")
banerjee_combined_data_0.3_2 <- banerjee_combined_data_0.3 %>%
  filter(theta_distribution == '[1, 0, 0, 0]')
combined_summary_null_0.3_2 <- banerjee_combined_data_0.3_2 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_0.3_2 <- banerjee_combined_data_0.3_2 %>%
  inner_join(combined_summary_null_0.3)
banerjee_combined_data_0.3_2$utility_to_spread <- banerjee_combined_data_0.3_2$utility_to_spread/banerjee_combined_data_0.3_2$utility_to_spread_null_mean
banerjee_combined_data_0.3_4 <- banerjee_combined_data_0.3 %>%
  filter(theta_distribution == '[0, 0, 1, 0]')
combined_summary_null_0.3_4 <- banerjee_combined_data_0.3_4 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_0.3_4 <- banerjee_combined_data_0.3_4 %>%
  inner_join(combined_summary_null_0.3)
banerjee_combined_data_0.3_4$utility_to_spread <- banerjee_combined_data_0.3_4$utility_to_spread/banerjee_combined_data_0.3_4$utility_to_spread_null_mean


banerjee_combined_data_0.5 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(0.5).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_0.5 <- cbind ( banerjee_combined_data_0.5, DELTA="0.5")
banerjee_combined_data_0.5_2 <- banerjee_combined_data_0.5 %>%
  filter(theta_distribution == '[1, 0, 0, 0]')
combined_summary_null_0.5_2 <- banerjee_combined_data_0.5_2 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_0.5_2 <- banerjee_combined_data_0.5_2 %>%
  inner_join(combined_summary_null_0.5)
banerjee_combined_data_0.5_2$utility_to_spread <- banerjee_combined_data_0.5_2$utility_to_spread/banerjee_combined_data_0.5_2$utility_to_spread_null_mean
banerjee_combined_data_0.5_4 <- banerjee_combined_data_0.5 %>%
  filter(theta_distribution == '[0, 0, 1, 0]')
combined_summary_null_0.5_4 <- banerjee_combined_data_0.5_4 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_0.5_4 <- banerjee_combined_data_0.5_4 %>%
  inner_join(combined_summary_null_0.5)
banerjee_combined_data_0.5_4$utility_to_spread <- banerjee_combined_data_0.5_4$utility_to_spread/banerjee_combined_data_0.5_4$utility_to_spread_null_mean

banerjee_combined_data_0.8 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(0.8).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_0.8 <- cbind ( banerjee_combined_data_0.8, DELTA="0.8")
banerjee_combined_data_0.8_2 <- banerjee_combined_data_0.8 %>%
  filter(theta_distribution == '[1, 0, 0, 0]')
combined_summary_null_0.8_2 <- banerjee_combined_data_0.8_2 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_0.8_2 <- banerjee_combined_data_0.8_2 %>%
  inner_join(combined_summary_null_0.8)
banerjee_combined_data_0.8_2$utility_to_spread <- banerjee_combined_data_0.8_2$utility_to_spread/banerjee_combined_data_0.8_2$utility_to_spread_null_mean
banerjee_combined_data_0.8_4 <- banerjee_combined_data_0.8 %>%
  filter(theta_distribution == '[0, 0, 1, 0]')
combined_summary_null_0.8_4 <- banerjee_combined_data_0.8_4 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_0.8_4 <- banerjee_combined_data_0.8_4 %>%
  inner_join(combined_summary_null_0.8)
banerjee_combined_data_0.8_4$utility_to_spread <- banerjee_combined_data_0.8_4$utility_to_spread/banerjee_combined_data_0.8_4$utility_to_spread_null_mean


banerjee_combined_data_1 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(1).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_1 <- cbind ( banerjee_combined_data_1, DELTA="1")
banerjee_combined_data_1_2 <- banerjee_combined_data_1 %>%
  filter(theta_distribution == '[1, 0, 0, 0]')
combined_summary_null_1_2 <- banerjee_combined_data_1_2 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_1_2 <- banerjee_combined_data_1_2 %>%
  inner_join(combined_summary_null_1)
banerjee_combined_data_1_2$utility_to_spread <- banerjee_combined_data_1_2$utility_to_spread/banerjee_combined_data_1_2$utility_to_spread_null_mean

banerjee_combined_data_1_4 <- banerjee_combined_data_1 %>%
  filter(theta_distribution == '[0, 0, 1, 0]')
combined_summary_null_1_4 <- banerjee_combined_data_1_4 %>%
  filter(network_size > 10) %>%
  group_by(model, network_group, network_id) %>%
  summarise(
    utility_to_spread_null_median = median(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_mean = mean(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_null_max = max(utility_to_spread[intervention_type == "none"]),
    utility_to_spread_median = median(utility_to_spread),
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_max = max(utility_to_spread)
  )
banerjee_combined_data_1_4 <- banerjee_combined_data_1_4 %>%
  inner_join(combined_summary_null_1)
banerjee_combined_data_1_4$utility_to_spread <- banerjee_combined_data_1_4$utility_to_spread/banerjee_combined_data_1_4$utility_to_spread_null_mean

st = rbind(
  banerjee_combined_data_0.3_2,
  banerjee_combined_data_0.5_2,
  banerjee_combined_data_0.8_2,
  banerjee_combined_data_1_2,
  banerjee_combined_data_0.3_4,
  banerjee_combined_data_0.5_4,
  banerjee_combined_data_0.8_4,
  banerjee_combined_data_1_4
) %>% 
  mutate(DELTA = as.factor(DELTA))%>%
  mutate(DELTA = factor(DELTA, levels(DELTA)[c(1,2,3,4)]))

st$theta_distribution[st$theta_distribution == '[1, 0, 0, 0]'] <- '2'
st$theta_distribution[st$theta_distribution == '[0, 0, 1, 0]'] <- '4'

table(table(st$network_id))

# plotting settings
theme_set(theme_bw())
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)
intervention_colors <- c(
  "none" = "black",
  "random_addition" = brewer.pal(8, "Set1")[1],
  "triad_addition" = brewer.pal(8, "Set1")[2],
  "rewired" = brewer.pal(8, "Set1")[5]
)
intervention_shapes <- c(
  "none" = 16,
  "random_addition" = 16,
  "triad_addition" = 17,
  "rewired" = 17
)


many_ecdf_plot = ggplot(
  aes(x = utility_to_spread,
      color = intervention_type,
      group = paste(intervention_type, network_id)      
  ),
  data = st %>% filter(
    intervention_type != "none"
  )
) +
  scale_x_log10(breaks = c(.1, .5, 1, 2, 10), limits = c(.09, 10.2)) +
  scale_color_manual(values = intervention_colors) +
  stat_ecdf(
    alpha = .5, lwd = .2,
    data = st_theta_2 %>% filter(
      intervention_type == "none"
    ) %>%
      select(-intervention_size)
  ) +
  stat_ecdf(alpha = .35, lwd = .2) +
  facet_grid(model ~ DELTA) +
  ylab("ECDF") +
  xlab("relative utility to spread(theta=2)") +
  theme(legend.position = "bottom") +
  annotation_logticks(
    sides = "b", size = .3,
    short = unit(0.05, "cm"), mid = unit(0.1, "cm"), long = unit(0.2, "cm")
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = .7)))
many_ecdf_plot_facet_by_size_2

st_theta_4<- st %>%
  filter(theta_distribution == 4)
many_ecdf_plot_facet_by_size_4 = ggplot(
  aes(x = utility_to_spread,
      color = intervention_type,
      group = paste(intervention_type, network_id)      
  ),
  data = st_theta_4 %>% filter(
    intervention_type != "none"
  )
) +
  scale_x_log10(breaks = c(.1, .5, 1, 2, 10), limits = c(.09, 10.2)) +
  scale_color_manual(values = intervention_colors) +
  stat_ecdf(
    alpha = .5, lwd = .2,
    data = st_theta_4 %>% filter(
      intervention_type == "none"
    ) %>%
      select(-intervention_size)
  ) +
  stat_ecdf(alpha = .35, lwd = .2) +
  facet_grid(model ~ DELTA) +
  ylab("ECDF") +
  xlab("relative utility to spread(theta=4)") +
  theme(legend.position = "bottom") +
  annotation_logticks(
    sides = "b", size = .3,
    short = unit(0.05, "cm"), mid = unit(0.1, "cm"), long = unit(0.2, "cm")
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = .7)))
many_ecdf_plot_facet_by_size_4
