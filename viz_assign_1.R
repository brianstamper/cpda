# Viz assign 1
library(ggplot2)
library(data.table)
library(readxl)

wow_data <- read_excel('data/WOW visualization data.xlsx')
wow_data[is.na(wow_data)] <- 0
names(wow_data)[[1]] <- 'time'

wow_data$time <- (wow_data$time - 1) * 12

signal_values <- function(x) match(x, c(0:9, LETTERS)) - 1
wow_data[, -1] <- lapply(wow_data[, -1], function(x) signal_values(x))

wow_data <- melt(wow_data, 'time')

time_formatter <- function(x) {
  m <- floor(x / 60)
  s <- floor(x %% 60)
  sprintf('%d:%02d', m, s)
}

p <- ggplot(wow_data, aes(x = time, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 4, linetype = 'dashed') +
  annotate('text', x = 1, y = 4, hjust = 0, vjust = -.5, label = 'sigma \u2265 4 may be\nconsidered significant', fontface = 'italic', size = 3) +
  xlab('Time (mm:ss)') +
  scale_x_continuous(labels = time_formatter) +
  ylab('Standard deviations above normal (sigma)') +
  scale_y_continuous(breaks = 1:10 * 5) +
  labs(color = 'Channel', 
       title = 'Ohio State - Ohio Wesleyan radio observatory "Wow" signal',
       subtitle = 'showing signal strength across all channels at the time of the incident')
p
ggsave('wow_signal.png', width = 6, height = 5)
