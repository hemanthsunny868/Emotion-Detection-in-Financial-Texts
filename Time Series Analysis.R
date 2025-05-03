library(tidyverse)
library(zoo)
library(lubridate)
df <- read.csv("C:/Users/DELL/Desktop/Capstone Project/New Data/with_emotions_filled/Adobe_emotions.csv")
df$date <- as.Date(df$publishedAt, format = "%Y-%m-%d")

#1 Emotion Trends Over Time

# Define emotion columns
emotion_cols <- c("positive", "trust", "anticipation", "negative", "joy",
                  "sadness", "fear", "surprise", "anger", "disgust")

# Add noise
set.seed(42)
df[emotion_cols] <- df[emotion_cols] + matrix(rnorm(nrow(df) * length(emotion_cols), 0, 0.01),
                                              ncol = length(emotion_cols))

# Weekly aggregation
df$week <- floor_date(df$date, unit = "week")

weekly_summary <- df %>%
  group_by(week) %>%
  summarise(across(all_of(emotion_cols), mean, na.rm = TRUE),
            avg_close = mean(Close, na.rm = TRUE),
            articles = n())

# Z-score normalize all emotions
emotion_scaled <- weekly_summary %>%
  mutate(across(all_of(emotion_cols), ~ scale(.x)[,1])) %>%
  select(week, all_of(emotion_cols), avg_close)

# Convert to long format for plotting
emotion_long <- emotion_scaled %>%
  pivot_longer(cols = all_of(emotion_cols), names_to = "emotion", values_to = "z_score")

# Baseline to align with price
baseline <- mean(emotion_scaled$avg_close)

# Plot all emotions + price
ggplot() +
  geom_line(data = emotion_long, aes(x = week, y = z_score * 50 + baseline, color = emotion), size = 1) +
  geom_line(data = emotion_scaled, aes(x = week, y = avg_close), color = "black", size = 1.2) +
  labs(
    title = "All Weekly Emotions (Z-Scaled) vs Closing Price (Adobe)",
    subtitle = "Each emotion scaled to its own variance and aligned with price scale",
    x = "Week", y = "Values"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired")

#2 Correlation Between Emotions and Same-Week Closing Price
# Correlation between emotions and avg_close (same week)
cor_matrix <- cor(weekly_summary[, emotion_cols], weekly_summary$avg_close, use = "complete.obs")

# Convert to dataframe for plotting
cor_df <- data.frame(
  emotion = rownames(cor_matrix),
  correlation = as.numeric(cor_matrix)
) %>%
  arrange(desc(abs(correlation)))  # rank by strength

# Plot
ggplot(cor_df, aes(x = reorder(emotion, correlation), y = correlation, fill = correlation)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Correlation Between Emotions and Closing Price (Same Week)",
    x = "Emotion", y = "Pearson Correlation"
  ) +
  scale_fill_gradient2(low = "red", high = "green", mid = "gray90", midpoint = 0)

#3 Lagged Correlation (Emotions Predicting Next Week’s Price)

# Create lagged avg_close (next week’s price)
weekly_summary <- weekly_summary %>%
  arrange(week) %>%
  mutate(next_week_close = lead(avg_close, 1))

# Correlation of this week’s emotions with next week’s price
lagged_cor_matrix <- cor(weekly_summary[, emotion_cols], weekly_summary$next_week_close, use = "complete.obs")

# Convert to dataframe for plotting
lagged_cor_df <- data.frame(
  emotion = rownames(lagged_cor_matrix),
  lagged_correlation = as.numeric(lagged_cor_matrix)
) %>%
  arrange(desc(abs(lagged_correlation)))

# Plot
ggplot(lagged_cor_df, aes(x = reorder(emotion, lagged_correlation), y = lagged_correlation, fill = lagged_correlation)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Lagged Correlation: Emotions vs Next Week's Price",
    x = "Emotion", y = "Pearson Correlation"
  ) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "gray90", midpoint = 0)

#4 ARIMA Model

library(tidyverse)
library(forecast)
library(lubridate)

# Load the daily data
df <- read.csv("C:/Users/DELL/Desktop/Capstone Project/New Data/with_emotions_filled/Adobe_emotions.csv")
df$date <- as.Date(df$publishedAt, format = "%Y-%m-%d")

# Aggregate by day if needed
daily_summary <- df %>%
  group_by(date) %>%
  summarise(avg_close = mean(Close, na.rm = TRUE))

# Convert to time series
price_ts <- ts(daily_summary$avg_close, frequency = 7)  # weekly seasonality

# Fit ARIMA
model <- auto.arima(price_ts)
forecast_result <- forecast(model, h = 10)

# Add future dates
future_dates <- seq(max(daily_summary$date) + 1, by = "1 day", length.out = 10)

# Combine past + forecast for clean plot
forecast_df <- data.frame(
  date = c(daily_summary$date, future_dates),
  fitted = c(fitted(model), rep(NA, 10)),
  forecast = c(rep(NA, nrow(daily_summary)), forecast_result$mean),
  lower = c(rep(NA, nrow(daily_summary)), forecast_result$lower[,2]),
  upper = c(rep(NA, nrow(daily_summary)), forecast_result$upper[,2])
)

# Plot
ggplot(forecast_df, aes(x = date)) +
  geom_line(aes(y = fitted), color = "black", size = 1.2) +
  geom_line(aes(y = forecast), color = "blue", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  labs(
    title = "ARIMA Forecast of Adobe Daily Closing Price",
    subtitle = "Next 10-Day Price Forecast with 95% Confidence",
    x = "Date", y = "Price"
  ) +
  theme_minimal()

#5 ARIMAX Model 

# Aggregate daily: average close and emotion scores
daily_data <- df %>%
  group_by(date) %>%
  summarise(
    Close = mean(Close, na.rm = TRUE),
    fear = mean(fear, na.rm = TRUE),
    sadness = mean(sadness, na.rm = TRUE),
    trust = mean(trust, na.rm = TRUE)
  )

# Remove rows with missing values
daily_data <- na.omit(daily_data)

# Define target and predictors
price_ts <- ts(daily_data$Close, frequency = 7)
xreg_matrix <- as.matrix(daily_data[, c("fear", "sadness", "trust")])

# Fit ARIMAX model
model_arimax <- auto.arima(price_ts, xreg = xreg_matrix)

# Forecast next 10 days (use last row's xreg repeated)
future_xreg <- matrix(rep(tail(xreg_matrix, 1), 10), ncol = 3, byrow = TRUE)
colnames(future_xreg) <- c("fear", "sadness", "trust")

forecast_arimax <- forecast(model_arimax, xreg = future_xreg, h = 10)

# Create future dates
future_dates <- seq(max(daily_data$date) + 1, by = "1 day", length.out = 10)

# Prepare dataframe for plotting
forecast_df <- data.frame(
  date = c(daily_data$date, future_dates),
  fitted = c(fitted(model_arimax), rep(NA, 10)),
  forecast = c(rep(NA, nrow(daily_data)), forecast_arimax$mean),
  lower = c(rep(NA, nrow(daily_data)), forecast_arimax$lower[,2]),
  upper = c(rep(NA, nrow(daily_data)), forecast_arimax$upper[,2])
)

# Plot
ggplot(forecast_df, aes(x = date)) +
  geom_line(aes(y = fitted), color = "black", size = 1.2) +
  geom_line(aes(y = forecast), color = "blue", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  labs(
    title = "ARIMAX Forecast of Adobe Daily Price (Fear, Sadness, Trust as Predictors)",
    subtitle = "Forecast shows influence of emotional signals",
    x = "Date", y = "Price"
  ) +
  theme_minimal()




