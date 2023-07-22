### Quantile Regression Tutorial ###
####################################
library(quantreg)
library(tidyverse)
library(dlookr)
library(broom)
library(DataExplorer)
library(here)
library(gt)
library(gtExtras)

custom_palette <- c("#192D45", "#748CDB", "#163B88", "#C1DFF9")

data <- read_csv(here("Dados/nces330_20.csv"), col_types = "iccccd") %>%
  rename(cost = "Value") %>%
  rename_with(.fn = str_to_lower)

summarize_data <- function(data){
  data %>%
    summarize(mean_cost  = mean(cost),
              total_cost = sum(cost),
              n          = n(),
              std_error  = sd(cost) / sqrt(n()),
              .groups    = "drop")
}


data %>%
  ggplot(aes(cost, length, fill = length)) +
  #ggridges::geom_density_ridges(height = 1, alpha = 0.75, color = NA) +
  ggdist::stat_halfeye(aes(fill = length),
                       adjust = 0.75,
                       point_color = NA, justification = -0.1) +
  geom_boxplot(width = 0.12, color = "black",
               outlier.color = NA, size = 0.8) +
  stat_summary(fun = "sd", geom = "text",
               aes(label = after_stat(str_c("SD = ", round(x)))),
               hjust = 0, vjust = -2.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("#192D45", "#748CDB", "#163B88", "#C1DFF9"))


data %>%
  group_by(year, length) %>%
  summarize_data() %>%
  ggplot(aes(year, mean_cost, color = length)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = mean_cost - std_error,
                  ymax = mean_cost + std_error,
                  fill = length),
              alpha = 0.2) +
  scale_x_continuous(breaks = 2013:2021) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = custom_palette) +
  labs(x='', y='') +
  ggthemes::theme_hc()

data %>%
  ggplot(aes(cost, type, fill = type)) +
  ggridges::geom_density_ridges(height = 1, alpha = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c("#192D45", "#748CDB", "#163B88", "#C1DFF9"))

data %>%
  group_by(year, expense) %>%
  summarize_data() %>%
  ggplot(aes(year, mean_cost, color = expense)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#9F4576", "#607B8B")) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = 2013:2021) +
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
  group_by(year, state) %>%
  summarize_data() %>%
  arrange(state, year) %>%
  group_by(state) %>%
  summarize(diff_perc = ((total_cost / lag(total_cost, 2021-2013)) - 1) * 100) %>%
  #mutate(diff_perc = ((cost / lag(cost, 2021-2013)) - 1) * 100) %>%
  drop_na() %>%
  arrange(diff_perc) %>%
  head(10) %>%
  pull(state)

data %>%
  group_by(year, state) %>%
  #summarize_data() %>%
  summarize(total_cost = sum(cost),
            .groups = "drop") %>%
  arrange(state, year) %>%
  filter(year %in% c(2013, 2021)) %>%
  pivot_wider(id_cols = c(state),
              names_from = year,
              values_from = total_cost) %>%
  mutate(diff_perc = ((`2021` / `2013`) - 1) * 100) %>%
  bind_cols(
    data %>%
      group_by(year, state) %>%
      summarize(total_cost = sum(cost),
                cost_data = list(cost),
                .groups = "drop") %>%
      filter(year %in% c(2013,2021)) %>%
      select(cost_data)
  ) %>%
  slice_max(diff_perc, n = 15) %>%
  gt() %>%
  gtExtras::gt_plt_sparkline(cost_data) %>%
  cols_label(state = "State",
             diff_perc = "% Change") %>%
  fmt_currency(columns = c(`2013`, `2021`),
               use_subunits = FALSE) %>%
  fmt_percent(columns = "diff_perc",
              scale_values = FALSE)


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
  mutate(diff_perc = ((total_cost / lag(total_cost, 2021-2013)) - 1) * 100) %>%
  drop_na() %>%
  ungroup() %>%
  slice_min(diff_perc, n = 10) %>%
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
  mutate(state = fct_reorder(state, cost, mean)) %>%
  ggplot(aes(state, cost, color = state, fill = state)) +
  geom_jitter(alpha = 0.25, width = 0.25) +
  geom_hline(yintercept = mean(data$cost),
             size = 1, color = "gray70") +
  scale_y_continuous(labels = scales::dollar) +
  geom_segment(aes(x = state, xend = state,
                   y = cost, yend = mean(data$cost))) +
  stat_summary(geom = "point",
               fun = "mean",
               size = 5) +
  ggsci::scale_color_simpsons() +
  #coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x='', y='')

plots <- data %>%
  nest(data = -(state)) %>%
  mutate(data = map(data, \(x) {
    x %>%
      group_by(year) %>%
      summarize(cost = sum(cost)) %>%
      pull(cost)
  }
  ))
  # mutate(plot = map2(state, data, function(var, df){
  #   df %>%
  #     group_by(year) %>%
  #     summarize(cost = sum(cost)) %>%
  #     ggplot(aes(year, cost)) +
  #     geom_line(linewidth = 1) +
  #     theme_void()
  # }))

### Quantile regression
lm_mod <- lm(cost ~ length + expense + type,
             data = data)
lm_coefs <- tidy(lm_mod, conf.int = TRUE) %>%
  slice(-1) %>%
  select(term, estimate, conf.low, conf.high)

summary(lm_mod)

qr <- rq(cost ~ length + expense + type,
         data = data,
         tau = seq(0.1, 0.9, 0.1))

models <- tidy(qr) %>%
  mutate(model = "qr") %>%
  filter(!str_detect(term, "Inter")) %>%
  bind_rows(lm_coefs %>%
              select(term, estimate, conf.low, conf.high) %>%
              mutate(model = "lm")) %>%
  mutate(term = str_detect("^(type|expense|length)"))
models %>%
  ggplot(aes(tau, estimate)) +
  facet_wrap(vars(term), scales = "free", ncol = 2) +
  geom_point(size = 1.8, color = custom_palette[3]) +
  geom_line(linewidth = 1, color = custom_palette[3]) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.1, color = custom_palette[2]) +
  geom_hline(data = models %>% filter(model == "lm", str_detect(term, "length")),
             aes(yintercept = estimate), lty = 2, size = 1, color = "gray60") +
  geom_hline(data = models %>% filter(model == "lm", str_detect(term, "expense")),
             aes(yintercept = estimate), lty = 2, size = 1, color = "gray60") +
  geom_hline(data = models %>% filter(model == "lm", str_detect(term, "In-State")),
             aes(yintercept = estimate), lty = 2, size = 1, color = "gray60") +
  geom_hline(data = models %>% filter(model == "lm", str_detect(term, "Out-of-State")),
             aes(yintercept = estimate), lty = 2, size = 1, color = "gray60") +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  ggthemes::theme_hc() +
  theme(strip.background = element_rect(fill = custom_palette[4], linewidth = 1, color = "white"))
  # annotate(geom = "rect", xmin = -Inf, xmax = Inf,
  #          ymin = filter(models, model == "lm", str_detect(term, "expense"))[["conf.low"]],
  #          ymax = filter(models, model == "lm", str_detect(term, "expense"))[["conf.high"]],
  #          alpha = 0.25, fill = custom_palette[3])










