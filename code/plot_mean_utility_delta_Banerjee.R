library(latex2exp)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(plotrix)

MODEL_1 = "(0.05,1)"

default_intervention_size = 10


cwd <- dirname(rstudioapi::getSourceEditorContext()$path)

intervention_name_map <- c(
  "none" = "original",
  "random_addition" = "random addition",
  "triad_addition" = "triadic addition",
  "rewired" = "rewired"
)

DELTA_map <- c(
  "0.3" = "Banerjee et al.\n(DELTA=0.3)",
  "0.5" = "Banerjee et al.\n(DELTA=0.5)",
  "0.8" = "Banerjee et al.\n(DELTA=0.8)",
  "1" = "Banerjee et al.\n(DELTA=1)"
)



banerjee_combined_data_0.3 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(0.3).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_0.3 <- cbind ( banerjee_combined_data_0.3, DELTA="0.3")
banerjee_combined_filtered_data_0.3 <- banerjee_combined_data_0.3 %>%
  filter(network_size > 10) %>%
  filter(model == MODEL_1) %>%
  filter(theta_distribution == '[1, 0, 0, 0]') %>%
  filter(intervention_size %in% c(0, default_intervention_size)) %>% 
  select(DELTA, intervention_type,utility_to_spread,network_id)%>%
  mutate(
    intervention = intervention_name_map[intervention_type]
  )%>%
  mutate(
    DELTA = DELTA_map[DELTA]
  ) %>%  
  mutate(intervention = as.factor(intervention))%>%
  mutate(intervention = factor(intervention,levels(intervention)[c(1,3,4,2)]))

banerjee_combined_data_0.5 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(0.5).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_0.5 <- cbind ( banerjee_combined_data_0.5, DELTA="0.5")
banerjee_combined_filtered_data_0.5 <- banerjee_combined_data_0.5 %>%
  filter(network_size > 10) %>%
  filter(model == MODEL_1) %>%
  filter(theta_distribution == '[1, 0, 0, 0]') %>%
  filter(intervention_size %in% c(0, default_intervention_size)) %>% 
  select(DELTA, intervention_type,utility_to_spread,network_id)%>%
  mutate(
    intervention = intervention_name_map[intervention_type]
  )%>%
  mutate(
    DELTA = DELTA_map[DELTA]
  ) %>%  
  mutate(intervention = as.factor(intervention))%>%
  mutate(intervention = factor(intervention,levels(intervention)[c(1,3,4,2)]))

banerjee_combined_data_0.8 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(0.8).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_0.8 <- cbind ( banerjee_combined_data_0.8, DELTA="0.8")
banerjee_combined_filtered_data_0.8 <- banerjee_combined_data_0.8 %>%
  filter(network_size > 10) %>%
  filter(model == MODEL_1) %>%
  filter(theta_distribution == '[1, 0, 0, 0]') %>%
  filter(intervention_size %in% c(0, default_intervention_size)) %>% 
  select(DELTA, intervention_type,utility_to_spread,network_id)%>%
  mutate(
    intervention = intervention_name_map[intervention_type]
  )%>%
  mutate(
    DELTA = DELTA_map[DELTA]
  ) %>%  
  mutate(intervention = as.factor(intervention))%>%
  mutate(intervention = factor(intervention,levels(intervention)[c(1,3,4,2)]))

banerjee_combined_data_1 <- read.csv(
  paste(cwd,"/data/banerjee-combined-data/banerjee_combined_edgelist_spreading_data_dump(1).csv",sep=""),
  stringsAsFactors = FALSE
)
banerjee_combined_data_1 <- cbind ( banerjee_combined_data_1, DELTA="1")
banerjee_combined_filtered_data_1 <- banerjee_combined_data_1 %>%
  filter(network_size > 10) %>%
  filter(model == MODEL_1) %>%
  filter(theta_distribution == '[1, 0, 0, 0]') %>%
  filter(intervention_size %in% c(0, default_intervention_size)) %>% 
  select(DELTA, intervention_type,utility_to_spread,network_id)%>%
  mutate(
    intervention = intervention_name_map[intervention_type]
  )%>%
  mutate(
    DELTA = DELTA_map[DELTA]
  ) %>%  
  mutate(intervention = as.factor(intervention))%>%
  mutate(intervention = factor(intervention,levels(intervention)[c(1,3,4,2)]))


theme_set(theme_bw())
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.y=element_blank()
)

intervention_colors <- c(
  "original" = "black",
  "random addition" = brewer.pal(8, "Set1")[1],
  "triadic addition" = brewer.pal(8, "Set1")[2],
  "rewired" = brewer.pal(8, "Set1")[5]
)

intervention_shapes <- c(
  "original" = 21,
  "random addition" = 22,
  "triadic addition" = 23,
  "rewired" = 24
)

all_filtered_data = rbind(
  banerjee_combined_filtered_data_0.3,
  banerjee_combined_filtered_data_0.5,
  banerjee_combined_filtered_data_0.8,
  banerjee_combined_filtered_data_1
) %>% 
  mutate(DELTA = as.factor(DELTA))%>%
  mutate(DELTA = factor(DELTA, levels(DELTA)[c(2,1,4,3)]))

q <- qnorm(1 - .05/2)
all_summaries <- all_filtered_data %>%
  group_by(DELTA, network_id) %>%
  mutate(
    utility_to_spread_diff = utility_to_spread - utility_to_spread[intervention == "original"]
  ) %>%
  group_by(DELTA, intervention) %>%
  summarise(
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_mean_diff = mean(utility_to_spread_diff),
    utility_to_spread_se = std.error(utility_to_spread),
    utility_to_spread_diff_se = std.error(utility_to_spread_diff),
    utility_to_spread_ub = utility_to_spread_mean + q * utility_to_spread_se,
    utility_to_spread_lb = utility_to_spread_mean - q * utility_to_spread_se,
    utility_to_spread_ub_diff = utility_to_spread_mean + q * utility_to_spread_diff_se,
    utility_to_spread_lb_diff = utility_to_spread_mean - q * utility_to_spread_diff_se
  )

all_summaries_group_by_id <- all_filtered_data %>%
  group_by(DELTA, network_id, intervention) %>%
  summarise(
    utility_to_spread = mean(utility_to_spread)
  ) %>%
  group_by(DELTA, network_id) %>%
  mutate(
    utility_to_spread_diff = utility_to_spread - utility_to_spread[intervention == "original"]
  ) %>%
  group_by(DELTA, intervention) %>%
  summarise(
    utility_to_spread_mean = mean(utility_to_spread),
    utility_to_spread_mean_diff = mean(utility_to_spread_diff),
    utility_to_spread_se = std.error(utility_to_spread),
    utility_to_spread_diff_se = std.error(utility_to_spread_diff),
    utility_to_spread_ub = utility_to_spread_mean + q * utility_to_spread_se,
    utility_to_spread_lb = utility_to_spread_mean - q * utility_to_spread_se,
    utility_to_spread_ub_diff = utility_to_spread_mean + q * utility_to_spread_diff_se,
    utility_to_spread_lb_diff = utility_to_spread_mean - q * utility_to_spread_diff_se
  )


write.csv(all_summaries,
          paste(cwd,"/data/all-spreading-time-summaries/all_summaries.csv",sep=""))


write.csv(all_summaries_group_by_id,
          paste(cwd,"/data/all-spreading-time-summaries/all_summaries_group_by_id.csv",sep=""))

write.csv(all_filtered_data,
          paste(cwd,"/data/all-spreading-time-summaries/all_filtered_data.csv",sep=""))


all_summaries_plot_diff_ci <- all_summaries_group_by_id %>%
  ungroup() %>%
  mutate(
    DELTA = factor(DELTA, levels = sort(levels(DELTA), T)),
  ) %>%
  ggplot(
    aes(
      x = DELTA,
      y = utility_to_spread_mean,
      ymin = utility_to_spread_lb_diff,
      ymax = utility_to_spread_ub_diff,
      color = intervention, shape = intervention, fill = intervention
    )
  ) +
  ylab("utility to spread") +
  scale_color_manual(values = intervention_colors) + 
  scale_fill_manual(values = intervention_colors) +
  scale_shape_manual(values = intervention_shapes) +
  geom_point(
    position=position_dodge2(width=0.7, reverse = TRUE),
    size = 2
  ) +
  geom_linerange(
    position=position_dodge2(width=0.7, reverse = TRUE),
    show.legend = F
  ) +
  coord_cartesian(xlim = c(1,20)) + 
  theme(
    legend.justification=c(1, 1),
    legend.position=c(0.95, 0.8),
    legend.title = element_blank(),
    legend.key = element_rect(size = 1),
    legend.key.size = unit(.9, 'lines')
  ) + 
  scale_y_log10(breaks = 2^(5:10)) +
  coord_flip()
all_summaries_plot_diff_ci

