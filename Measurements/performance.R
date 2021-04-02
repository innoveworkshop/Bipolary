#' performance.R
#' Plots the performance of the amplifier based on the measured values.
#' 
#' @author Nathan Campos <nathan@innoveworkshop.com>

# Load Libraries ----
library(readr)
library(tibble)
library(dplyr)
library(reshape2)
library(scales)
library(ggplot2)

# Load Data ----
freq_response <- readr::read_csv("./Measurements/Frequency Response.csv") %>% clean_names()

# Function Definitions ----
# Clean up data frame column names. (https://www.r-bloggers.com/2019/07/clean-consistent-column-names/)
clean_names <- function (.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data
  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  
  n <- gsub("(^_+|_+$)", "", n)
  n <- gsub("_+", "_", n)
  n <- gsub("^([0-9])", "n\\1", n)
  
  if (unique) n <- make.unique(n, sep = "_")
  
  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}

# Converts a number (non-power) to decibels using a reference.
to_decibel <- function (value, ref = 1) {
  return(20 * log10(value / ref))
}

# Plots the frequency response of the amplifier.
plot_frequency_response <- function (start_freq = 1, stop_freq = 100000) {
  # Only get measurements between the range the user wants to plot.
  dt <- dplyr::filter(freq_response,
                      dplyr::between(frequency, start_freq, stop_freq))
  
  # Convert values to decibels.
  refs <- apply(freq_response, MARGIN = 2, function (x) max(x, na.rm = TRUE))
  dt <- transform(dt, unloaded = to_decibel(unloaded, refs[["unloaded"]]))
  dt <- transform(dt, n30_ohms = to_decibel(n30_ohms, refs[["n30_ohms"]]))
  dt <- transform(dt, n150_ohms = to_decibel(n150_ohms, refs[["n150_ohms"]]))
  
  # Make the dataset friendlier to ggplot2.
  dt <- reshape2::melt(dt, id.vars = "frequency")

  # Setup the plot.
  g <- ggplot(dt, aes(x = frequency, y = value, color = variable)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 12), se = FALSE, size = 1) +
    labs(x = "Frequency (Hz)", y = "Magnitude (dB)") +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = "", labels = c("Unloaded", "30\u03a9", "150\u03a9")) +
    scale_x_log10(labels = scales::label_number_si()) + annotation_logticks(sides = "b")
  
  return(g)
}

# Plots the frequency response of the amplifier.
plot_frequency_response <- function (start_freq = 1, stop_freq = 100000) {
  # Only get measurements between the range the user wants to plot.
  dt <- dplyr::filter(freq_response,
                      dplyr::between(frequency, start_freq, stop_freq))
  
  # Convert values to decibels.
  refs <- apply(freq_response, MARGIN = 2, function (x) max(x, na.rm = TRUE))
  dt <- transform(dt, unloaded = to_decibel(unloaded, refs[["unloaded"]]))
  dt <- transform(dt, n30_ohms = to_decibel(n30_ohms, refs[["n30_ohms"]]))
  dt <- transform(dt, n150_ohms = to_decibel(n150_ohms, refs[["n150_ohms"]]))
  
  # Make the dataset friendlier to ggplot2.
  dt <- reshape2::melt(dt, id.vars = "frequency")
  
  # Setup the plot.
  g <- ggplot(dt, aes(x = frequency, y = value, color = variable)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 12), se = FALSE, size = 1) +
    labs(x = "Frequency (Hz)", y = "Magnitude (dB)") +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = "", labels = c("Unloaded", "30\u03a9", "150\u03a9")) +
    scale_x_log10(labels = scales::label_number_si()) + annotation_logticks(sides = "b")
  
  return(g)
}

# Plot Data ----
print(plot_frequency_response(10, 100000))