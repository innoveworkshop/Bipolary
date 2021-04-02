#' performance.R
#' Plots the performance of the amplifier based on the measured values.
#' 
#' @author Nathan Campos <nathan@innoveworkshop.com>

# Load Libraries ----
library(readr)
library(tibble)
library(ggplot2)

# Load Data ----
freq_response <- readr::read_csv("./Measurements/Frequency Response.csv")
output_levels <- readr::read_csv("./Measurements/Output Levels.csv")