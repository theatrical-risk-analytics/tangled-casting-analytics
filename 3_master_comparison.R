library(tidyverse)
library(lubridate)

# =====================================================================
# STEP 1: ANONYMOUS DIRECTORY PATH HANDLING
# =====================================================================
# Enforces relative data paths based on the local repository structure
teagan_file   <- "data_pipeline/teagan_ai_sentiment.csv"
avantika_file <- "data_pipeline/avantika_longterm_sentiment.csv"
halle_file    <- "data_pipeline/halle_ai_final.csv"
woc_file      <- "data_pipeline/woc_model_sentiment.csv"

# Verify all files are safely present before initializing the pipeline
if (!all(file.exists(c(teagan_file, avantika_file, halle_file, woc_file)))) {
  stop("Data Ingestion Error: Ensure all 4 scored CSV files are uploaded inside your local 'data_pipeline/' folder.")
}

# =====================================================================
# STEP 2: PARSE METRICS ACROSS CHRONOLOGICAL TIMELINE CHUNKS
# =====================================================================
teagan_raw   <- read_csv(teagan_file, show_col_types = FALSE)
avantika_raw <- read_csv(avantika_file, show_col_types = FALSE)
halle_raw    <- read_csv(halle_file, show_col_types = FALSE)
woc_raw      <- read_csv(woc_file, show_col_types = FALSE)

# Process Teagan Matrix (The Brand Sanctuary Strategy)
teagan_m <- teagan_raw %>%
  select(label, created_at) %>%
  mutate(date = as.Date(strptime(created_at, format = "%a %b %d %T %z %Y")), month = floor_date(date, "month")) %>%
  filter(!is.na(month)) %>% group_by(month, label) %>% tally() %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% ungroup() %>%
  { if(! "positive" %in% names(.)) mutate(., positive = 0) else . } %>%
  { if(! "negative" %in% names(.)) mutate(., negative = 0) else . } %>%
  mutate(net_ai_sentiment = positive - negative, Strategy = "Teagan Croft (Brand Sanctuary)") %>%
  select(month, net_ai_sentiment, Strategy)

# Process Avantika Matrix (The Organic Catalyst Strategy)
avantika_m <- avantika_raw %>%
  select(label, created_at) %>%
  mutate(date = as.Date(strptime(created_at, format = "%a %b %d %T %z %Y")), month = floor_date(date, "month")) %>%
  filter(!is.na(month)) %>% group_by(month, label) %>% tally() %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% ungroup() %>%
  { if(! "positive" %in% names(.)) mutate(., positive = 0) else . } %>%
  { if(! "negative" %in% names(.)) mutate(., negative = 0) else . } %>%
  mutate(net_ai_sentiment = positive - negative, Strategy = "Avantika Vandanapu (Organic Catalyst)") %>%
  select(month, net_ai_sentiment, Strategy)

# Process Halle Bailey Matrix (The Historical Blueprint Strategy)
halle_m <- halle_raw %>%
  select(label, created_at) %>%
  mutate(date = as.Date(strptime(created_at, format = "%a %b %d %T %z %Y")), month = floor_date(date, "month")) %>%
  filter(!is.na(month)) %>% group_by(month, label) %>% tally() %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% ungroup() %>%
  { if(! "positive" %in% names(.)) mutate(., positive = 0) else . } %>%
  { if(! "negative" %in% names(.)) mutate(., negative = 0) else . } %>%
  mutate(net_ai_sentiment = positive - negative, Strategy = "Halle Bailey (Historical Blueprint)") %>%
  select(month, net_ai_sentiment, Strategy)

# Process Isolated WOC Rapunzel Buzz Matrix (The Polarized Launchpad Model)
woc_m <- woc_raw %>%
  select(label, created_at) %>%
  mutate(
    date = as.Date(parse_date_time(created_at, orders = c("a b d H:M:S z Y", "ymd HMS", "ymd"))),
    month = floor_date(date, "month")
  ) %>%
  filter(!is.na(month)) %>% group_by(month, label) %>% tally() %>%
  pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% ungroup() %>%
  { if(! "positive" %in% names(.)) mutate(., positive = 0) else . } %>%
  { if(! "negative" %in% names(.)) mutate(., negative = 0) else . } %>%
  mutate(net_ai_sentiment = positive - negative, Strategy = "WOC Rapunzel Buzz (The Polarized Launchpad)") %>%
  select(month, net_ai_sentiment, Strategy)

# =====================================================================
# STEP 3: UNIFY CHANNELS AND PLOT THE EXECUTIVE DASHBOARD
# =====================================================================
master_dashboard_df <- bind_rows(teagan_m, avantika_m, halle_m, woc_m)

# Plot the Complete 4-Panel Studio Intelligence Suite
ggplot(master_dashboard_df, aes(x = month, y = net_ai_sentiment, color = Strategy, group = Strategy)) +
  geom_line(linewidth = 1.3) + 
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_color_manual(values = c("#E74C3C", "#2ECC71", "#9B59B6", "#1DA1F2")) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  facet_wrap(~Strategy, ncol = 1, scales = "free") + 
  labs(
    title = "Studio Media Intelligence: 4-Way Casting Lifecycle Analytics Dashboard",
    subtitle = "Deep learning context comparison across alternative talent profile trajectories",
    x = "Timeline Macro View (Relative Calendar Scales 2019 - 2026)",
    y = "Net AI Sentiment Score (Monthly Aggregates)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1.2, "lines")
  )
