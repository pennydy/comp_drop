library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggsignif)
library(tidytext)
library(RColorBrewer)
library(stringr)

theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")


# 1. Data ----
sentence_files <- list.files(path="../data", pattern="sentences_with_comp*",
                             full.names=TRUE, recursive=FALSE)

sentences.df <- lapply(sentence_files,
                   FUN = read.csv,
                   header = TRUE) %>% 
  bind_rows()


frequency_files <- list.files(path="../data", pattern="verb_freq*",
                              full.names=TRUE, recursive=FALSE)

# TODO: combine the frequency files based on the verb column
frequency.df <- lapply(frequency_files,
                       FUN = read.csv,
                       header = TRUE)

