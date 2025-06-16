# ─────────────────────────────────────────────────────────────────────────────
# 0.  Load packages ------------------------------------------------------------
# ─────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(plm)

# ─────────────────────────────────────────────────────────────────────────────
# 1.  Read raw data ------------------------------------------------------------
# ─────────────────────────────────────────────────────────────────────────────
rm(list=ls())
df <- read.csv("~/Library/CloudStorage/OneDrive-TheUniversityofNottingham/Study 4 - Emotions dataset/study4.csv")

# 3.  Define positive & negative emotion suffixes -----------------------------
pos_suffixes <- c("Ease","Calm","Content","Relieved",
                  "Happy","Proud","Fulfulled","Interested","Curious")
neg_suffixes <- c("Anxious","Embarrassed","Fearful","Nervous",
                  "Overwhelmed","Scared","SelfConsc","Stressed")

# 4.  Helper to build Refl/Anti composites ------------------------------------
make_emotion_composite <- function(data, prefix, pos, neg) {
  pattern <- paste0("^T[1-4]_", prefix, "(", paste(c(pos,neg), collapse="|"), ")$")
  data %>%
    select(ID, matches(pattern)) %>%
    pivot_longer(
      cols         = -ID,
      names_to     = c("wave","item"),
      names_pattern= paste0("T([1-4])_", prefix, "(.+)"),
      values_to    = "value"
    ) %>%
    mutate(
      wave    = as.integer(wave),
      valence = if_else(item %in% pos, "benefit", "cost")
    ) %>%
    group_by(ID, wave, valence) %>%
    summarise(
      composite = mean(value, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    pivot_wider(
      names_from  = valence,
      values_from = composite
    ) %>%
    rename_with(~ paste0(prefix, "_", .), c("benefit","cost"))
}

# 5.  Build reflective & anticipated composites -------------------------------
refl_df <- make_emotion_composite(df, "Refl", pos_suffixes, neg_suffixes)
anti_df <- make_emotion_composite(df, "Anti", pos_suffixes, neg_suffixes)

# 6.  Extract intention (T?_Int3_1) and booking (T?_booked) -------------------
int_df <- df %>%
  select(ID, matches("^T[1-4]_Int3_1$")) %>%
  pivot_longer(
    cols         = -ID,
    names_to     = "wave",
    names_pattern= "T([1-4])_Int3_1",
    values_to    = "intention"
  ) %>%
  mutate(wave = as.integer(wave))

book_df <- df %>%
  select(ID, matches("^T[1-4]_booked$")) %>%
  pivot_longer(
    cols         = -ID,
    names_to     = "wave",
    names_pattern= "T([1-4])_booked",
    values_to    = "booked_raw"
  ) %>%
  mutate(
    wave   = as.integer(wave),
    booked = if_else(booked_raw %in% c(1,2), 1, 0)
  ) %>%
  select(ID, wave, booked)

# 7.  Merge everything into one long‐form data frame ---------------------------
long_df <- reduce(
  list(refl_df, anti_df, int_df, book_df),
  left_join,
  by = c("ID","wave")
)

# 8.  Collapse any duplicate ID–wave rows by averaging -------------------------
long_df <- long_df %>%
  group_by(ID, wave) %>%
  summarise(
    Refl_benefit = mean(Refl_benefit, na.rm=TRUE),
    Refl_cost    = mean(Refl_cost,    na.rm=TRUE),
    Anti_benefit = mean(Anti_benefit, na.rm=TRUE),
    Anti_cost    = mean(Anti_cost,    na.rm=TRUE),
    intention    = mean(intention,    na.rm=TRUE),
    booked       = mean(booked,       na.rm=TRUE),
    .groups      = "drop"
  )

# 9.  Declare panel structure -------------------------------------------------
panel_df <- pdata.frame(long_df, index = c("ID","wave"))

# 10. Run Fixed‐Effects models ------------------------------------------------

# (a) Intention ~ Refl/Anti benefit & cost
fe_int <- plm(
  intention ~ Refl_benefit + Refl_cost + Anti_benefit + Anti_cost,
  data  = panel_df,
  model = "within"
)
cat("===== FE Model: Intention =====\n")
print(summary(fe_int))

# (b) Rebooking ~ Refl/Anti benefit & cost (LPM)
fe_book <- plm(
  booked ~ Refl_benefit + Refl_cost + Anti_benefit + Anti_cost,
  data  = panel_df,
  model = "within"
)
cat("\n===== FE Model: Rebooking =====\n")
print(summary(fe_book))
