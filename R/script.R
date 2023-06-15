### Quantile Regression Tutorial ###
####################################
library(quantreg)
library(tidyverse)
library(dlookr)
library(broom)
library(DataExplorer)
library(here)

data <- read_csv(here("Dados/nces330_20.csv"), col_types = "iccccd") %>%
  rename(cost = "Value") %>%
  rename_with(.fn = str_to_lower)

data %>%
  plot_histogram()

data %>%
  skimr::skim()

data %>%
  select(where(is.character)) %>%
  map_dbl(n_distinct)

summarize_data <- function(data){
  data %>%
    summarize(mean_cost = median(cost),
              n         = n(),
              std_error = sd(cost) / sqrt(n()),
              .groups   = "drop")
}

data %>%
  ggplot(aes(cost, type, fill = type)) +
  ggridges::geom_density_ridges(height = 1, alpha = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c("#192D45", "#748CDB", "#163B88", "#C1DFF9"))

data %>%
  group_by(year, expense) %>%
  summarize_data() %>%
  ggplot(aes(year, cost, color = expense)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#9F4576", "#607B8B")) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = 2013:2021)+
  ggthemes::theme_hc()

data %>%
  group_by(year, length) %>%
  summarize_data() %>%
  ggplot(aes(year, cost, fill = length)) +
  geom_area(linewidth = 1.2, alpha = 0.8, color = "#f8e2f1") +
  scale_fill_manual(values = c("#9F4576", "#607B8B")) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = 2013:2021) +
  ggthemes::theme_hc()

selected_states <- data %>%
  summarize_data(year, state) %>%
  arrange(state, year) %>%
  group_by(state) %>%
  summarize(diff_perc = ((cost / lag(cost, 2021-2013)) - 1) * 100) %>%
  #mutate(diff_perc = ((cost / lag(cost, 2021-2013)) - 1) * 100) %>%
  drop_na() %>%
  arrange(desc(diff_perc)) %>%
  head(15) %>%
  pull(state)

# data %>%
#   filter(state %in% selected_states) %>%
#   summarize_data(year, state) %>%
#   ggplot(aes(year, cost, color = state)) +
#   geom_line(linewidth = 0.8, show.legend = FALSE) +
#   scale_color_viridis_d(option = "E") +
#   scale_y_continuous(labels = scales::dollar) +
#   scale_x_continuous(breaks = 2013:2021) +
#   facet_wrap(vars(state), scales = "free") +
#   ggthemes::theme_hc()

data %>%
  group_by(year, state) %>%
  summarize_data() %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(diff_perc = ((cost / lag(cost, 2021-2013)) - 1) * 100) %>%
  drop_na() %>%
  ungroup() %>%
  slice_max(diff_perc, n = 20) %>%
  mutate(state = fct_reorder(state, diff_perc),
         perc = glue::glue("{round(diff_perc,2)}%")) %>%
  ggplot(aes(state, diff_perc)) +
  geom_col(fill = "#9F4576") +
  geom_text(aes(label = perc),
            hjust = 1.1, size = 5, color = "white", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = ~ scales::percent(.x, scale = 1)) +
  theme_void() +
  labs(x='', y='') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold"))

data %>%
  filter(state %in% selected_states) %>%
  mutate(state = fct_reorder(state, cost)) %>%
  ggplot(aes(state, cost)) +
  geom_boxplot(alpha = 0.25, outlier.color = NA, width = 0.45) +
  geom_jitter(alpha = 0.25, color = "#9F4576", width = 0.25) +
  geom_hline(yintercept = median(data$cost), lty = 2, size = 1) +
  scale_y_continuous(labels = scales::dollar) +
  #geom_segment(aes(y = median(data$cost), yend = cost, xend = cost)) +
  stat_summary(geom = "point",
               fun = "mean",
               size = 5, pch = 21, fill = "#607B8B") +
  coord_flip() +
  ggthemes::theme_hc() +
  labs(x='', y='')

### Quantile regression
lm_mod <- lm(cost ~ length + expense,
             data = data)
summary(lm_mod)

qr <- rq(cost ~ length + expense,
         data = data,
         tau = seq(0.1, 0.9, 0.1))

tidy(qr) %>%
  filter(!str_detect(term, "Inter")) %>%
  ggplot(aes(tau, estimate)) +
  geom_point(size = 1.8, color = "#9F4576") +
  geom_line(size = 1, color = "#9F4576") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = "#607B8B") +
  facet_wrap(vars(term), scales = "free", ncol = 1)










