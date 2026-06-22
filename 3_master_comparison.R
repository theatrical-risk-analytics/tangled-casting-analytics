library(tidyverse)
library(lubridate)

# =====================================================================
# STEP 1: ANONYMOUS DIRECTORY PATH HANDLING
# =====================================================================
teagan_file   <- "data_pipeline/teagan_ai_sentiment.csv"
avantika_file <- "data_pipeline/avantika_longterm_sentiment.csv"
halle_file    <- "data_pipeline/halle_ai_final.csv"
woc_file      <- "data_pipeline/woc_model_sentiment.csv"

if (!all(file.exists(c(teagan_file, avantika_file, halle_file, woc_file)))) {
  stop("Data Ingestion Error: Ensure all 4 scored CSV files are uploaded inside your local 'data_pipeline/' folder.")
}

if (!dir.exists("images")) {
  dir.create("images")
}

# =====================================================================
# STEP 2: GENERATE CHART 1 (4-PANEL LIFECYCLE) & CHART 2 (AD-BOOST P&L)
# =====================================================================
teagan_raw   <- read_csv(teagan_file, show_col_types = FALSE)
avantika_raw <- read_csv(avantika_file, show_col_types = FALSE)
halle_raw    <- read_csv(halle_file, show_col_types = FALSE)
woc_raw      <- read_csv(woc_file, show_col_types = FALSE)

process_sentiment <- function(df, strategy_name) {
  df %>%
    select(label, created_at) %>%
    mutate(
      date = as.Date(parse_date_time(created_at, orders = c("a b d H:M:S z Y", "ymd HMS", "ymd"))),
      month = floor_date(date, "month")
    ) %>%
    filter(!is.na(month)) %>% 
    group_by(month, label) %>% tally() %>%
    pivot_wider(names_from = label, values_from = n, values_fill = 0) %>% ungroup() %>%
    { if(! "positive" %in% names(.)) mutate(., positive = 0) else . } %>%
    { if(! "negative" %in% names(.)) mutate(., negative = 0) else . } %>%
    mutate(net_ai_sentiment = positive - negative, Strategy = strategy_name) %>%
    select(month, net_ai_sentiment, Strategy)
}

master_dashboard_df <- bind_rows(
  process_sentiment(teagan_raw, "Teagan Croft (Brand Sanctuary)"),
  process_sentiment(avantika_raw, "Avantika Vandanapu (Organic Catalyst)"),
  process_sentiment(halle_raw, "Halle Bailey (Historical Blueprint)"),
  process_sentiment(woc_raw, "WOC Rapunzel Buzz (The Polarized Launchpad)")
)

ggplot(master_dashboard_df, aes(x = month, y = net_ai_sentiment, color = Strategy, group = Strategy)) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_color_manual(values = c("#2ECC71", "#9B59B6", "#E74C3C", "#1DA1F2")) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  facet_wrap(~Strategy, ncol = 1, scales = "free") + 
  labs(title = "Studio Media Intelligence: 4-Way Casting Lifecycle Analytics Dashboard", x = "Timeline Macro View", y = "Net AI Sentiment Score") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
ggsave("images/chart5_master_comparison.png", width = 11, height = 8.5, dpi = 300, bg = "white")

p_and_l_data <- tibble(
  Strategy = c("Teagan Croft", "Avantika Track", "Alternative WOC", "Halle Bailey Actual"),
  Marketing_Spend = c(340, 150, 110, 140),
  Net_Profit_Loss = c(-211.2, 85.4, 165.0, 224.8)
) %>% pivot_longer(cols = c(Marketing_Spend, Net_Profit_Loss), names_to = "Metric", values_to = "Amount")

ggplot(p_and_l_data, aes(x = Strategy, y = Amount, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = c("Marketing_Spend" = "#E74C3C", "Net_Profit_Loss" = "#2ECC71"), labels = c("Marketing Spend ($M)", "Net Profit / Loss ($M)")) +
  labs(title = "The Paid Marketing Trap: Ad Boosting vs. Bottom-Line Studio ROI", x = "Casting Strategy Vector", y = "Financial Scale (USD Millions)", fill = "Financial Matrix Layer") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("images/ad_boosting_pl.png", width = 8, height = 5, dpi = 300, bg = "white")

# =====================================================================
# STEP 3: ADVANCED STRENGTHENING MODELS (NEW FIGURES)
# =====================================================================

# Model A: Hype-Decay Velocity Curve
decay_data <- tibble(
  Month = 0:24,
  Negative_Backlash = 100 * exp(-0.25 * Month),
  Organic_Fan_Defense = 5 + (95 / (1 + exp(-0.2 * (Month - 8))))
) %>% pivot_longer(cols = -Month, names_to = "Trend", values_to = "Volume")

ggplot(decay_data, aes(x = Month, y = Volume, color = Trend, linetype = Trend)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Negative_Backlash" = "#E74C3C", "Organic_Fan_Defense" = "#2ECC71"), labels = c("Negative Controversy Spike", "Compounding Fan-Defense Loop")) +
  labs(title = "Figure 2.2: Structural Asymmetry of Announcement Hype-Decay", subtitle = "Controversy spikes quickly and burns out; organic fan networks build compounding velocity over time.", x = "Months Since Announcement", y = "Relative Social Volume Index (0 - 100)") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("images/hype_decay_velocity.png", width = 8, height = 4.5, dpi = 300, bg = "white")

# Model B: Econometric Break-Even Matrix
breakeven_grid <- seq(0, 600, by = 50)
be_data <- tibble(
  Box_Office = rep(breakeven_grid, 2),
  Strategy = c(rep("Teagan Croft (Paid Marketing Trap)", length(breakeven_grid)), rep("Alternative WOC (Organic Leverage)", length(breakeven_grid))),
  Total_Costs = c(200 + 340 + (breakeven_grid * 0.1), 200 + 110 + (breakeven_grid * 0.1)), # Production + Marketing + Theater Distribution Split
  Net_Return = (Box_Office * 0.5) - Total_Costs # Studio takes ~50% of theatrical gross
)

ggplot(be_data, aes(x = Box_Office, y = Net_Return, color = Strategy)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.6) +
  labs(title = "Figure 3.2: Studio Risk Break-Even Threshold Optimization Matrix", subtitle = "Lowering customer acquisition costs shifting the net-profit threshold to the left.", x = "Global Theatrical Box Office Gross ($M)", y = "Net Corporate Return to Studio ($M)") +
  scale_color_manual(values = c("#2ECC71", "#E74C3C")) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("images/breakeven_matrix.png", width = 8, height = 4.5, dpi = 300, bg = "white")

# Model C: Weekend Theatrical Box Office Drop-Off Simulation
weeks <- 1:6
attrition_data <- tibble(
  Week = rep(weeks, 2),
  Strategy = c(rep("High-Vocal Profile (Organic Tailwind)", 6), rep("Low-Volume Profile (Paid Marketing Trap)", 6)),
  Gross = c(120 * (0.60)^(weeks - 1), 45 * (0.32)^(weeks - 1)) # Standard word of mouth drop vs rapid collapse
)

ggplot(attrition_data, aes(x = Week, y = Gross, fill = Strategy)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("#2ECC71", "#E74C3C")) +
  labs(title = "Figure 4.2: Weekly Box Office Drop-Off Optimization Vectors", subtitle = "Simulating ticket-sales erosion once upfront promotional spend steps down.", x = "Theatrical Weekend Cycle", y = "Weekend Gross (USD Millions)") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("images/audience_attrition.png", width = 8, height = 4.5, dpi = 300, bg = "white")

print("All 5 strategic visualization matrices successfully compiled and saved to disk!")

