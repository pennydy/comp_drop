library(lme4)
library(lmerTest)
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggsignif)
library(tidytext)
library(RColorBrewer)
library(stringr)
library(brms)
library(corrplot)

theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")


# 1. Data ----
# sentence datafram with surprisal
surprisal_files <- list.files(path="../../data", pattern="sentences_surprisal_alt*",
                              full.names=TRUE, recursive=FALSE)

surprisal5.df <- lapply(surprisal_files,
                       FUN = read.csv,
                       header = TRUE) %>%
  bind_rows() %>%
  select(-c("embedded_clause", "embedded_clause_n")) %>%
  drop_na() %>%
  rename(local_verb = "verb_sum",
         local_embedded_n = "embedded_n_sum")

that_surprisal5.df <- surprisal5.df %>%
  select(-c(sentence, pseudo_that)) %>%
  filter(comp_type %in% c("that", "none")) %>%
  filter(matrix_verb_to_cc >= 0) %>% # all <0 ones are other comp types (e.g., object rc)
  mutate(comp_type = if_else(comp_type == "that", 1, 0),
         item = row_number()) %>%
  relocate(item)

# calculate the entropy
entropy5_summary <- surprisal5.df %>%
  group_by(verb) %>%
  summarize(expected_verb = mean(local_verb, na.rm=TRUE), # verb entropy
            expected_embedded_n = mean(local_embedded_n, na.rm=TRUE)) # first n words of embedded entropy

# alternative first 7 words instead of 5
surprisal7.df <- read.csv("../../data/sentences_surprisal_more_words.csv", header=TRUE)

surprisal7.df <- surprisal7.df %>% 
  select(-c("embedded_clause", "embedded_clause_n")) %>% 
  drop_na() %>% 
  rename(local_verb = "verb_sum",
         local_embedded_n = "embedded_n_sum")

that_surprisal7.df <- surprisal7.df %>% 
  select(-c(sentence, pseudo_that)) %>% 
  filter(comp_type %in% c("that", "none")) %>% 
  filter(matrix_verb_to_cc >= 0) %>% # all <0 ones are other comp types (e.g., object rc)
  mutate(comp_type = if_else(comp_type == "that", 1, 0),
         item = row_number()) %>% 
  relocate(item)

all_counts <- length(that_surprisal7.df$comp_type)
that_count <- sum(that_surprisal7.df$comp_type)
that_proportion <- that_count / all_counts

# obtain the frequency count from the dataframe
frequency.df_alt <- surprisal7.df %>%
  group_by(verb) %>%
  summarize(cc = n()) # combination of that and "none"

frequency_files <- list.files(path="../../data", pattern="verb_freq_test*",
                              full.names=TRUE, recursive=FALSE)

frequency.df <- lapply(frequency_files,
                       FUN = read.csv,
                       header = TRUE) %>% 
  bind_rows()


overal_frequency <- frequency.df %>% 
  group_by(verb) %>% 
  summarize(overall=sum(overall),
            that = sum(that),
            none = sum(none),
            other = sum(other))

frequency_summary_alt <- left_join(frequency.df_alt, overal_frequency, by="verb")
all_verbs_counts = sum(frequency_summary_alt$overall)
all_verbs_counts

frequency_summary_alt <- frequency_summary_alt %>% 
  group_by(verb) %>% 
  summarize(verb_prob=overall/all_verbs_counts,
            frequency_verb=-log(verb_prob,base=2), # verb frequency
            cc_prob = cc/overall,
            frequency_cc = -log(cc_prob,base=2)) %>%  # first n words of embedded frequency
  ungroup()


# calculate the entropy
entropy7_summary <- surprisal7.df %>%
  group_by(verb) %>%
  summarize(expected_verb = mean(local_verb, na.rm=TRUE), # verb entropy
            expected_embedded_n = mean(local_embedded_n, na.rm=TRUE)) # first n words of embedded entropy

# # semantic types
# semantic_types <- read.csv("../../data/semantic_types.csv", header=TRUE)

# combine frequency and contextual informativity to dataframe
that_sentence_freq <- left_join(that_surprisal7.df, frequency_summary_alt, by="verb") %>% 
  left_join(., entropy7_summary, by="verb") # %>% 
  # left_join(., semantic_types, by="verb")

that_sentence_freq <- that_sentence_freq %>%
  mutate(frequency_verb = as.numeric(scale(frequency_verb, center=TRUE, scale=TRUE)),
         frequency_cc = as.numeric(scale(frequency_cc, center=TRUE, scale=TRUE)),
         local_embedded_n = as.numeric(scale(local_embedded_n, center=TRUE, scale=TRUE)),
         local_verb = as.numeric(scale(local_verb, center=TRUE, scale=TRUE)),
         expected_verb = as.numeric(scale(expected_verb, center=TRUE, scale=TRUE)),
         expected_embedded_n = as.numeric(scale(expected_embedded_n, center=TRUE, scale=TRUE)))
# write.csv(that_sentence_freq,"~/Downloads/that_sentences_freq_scaled.csv", row.names = TRUE)


# 2. Analysis ----
that_sentence_freq <- that_sentence_freq %>%
  filter(matrix_verb_to_cc == 0)

write.csv(that_sentence_freq,"~/Downloads/that_sentences_freq_n7.csv", row.names = TRUE)

sum(that_sentence_freq$comp_type)
sum(that_sentence_freq$comp_type)/length(that_sentence_freq$comp_type)


# 1. plot embedded_n_sum by comp_type
ggplot(that_sentence_freq, aes(x = as.factor(comp_type), y = local_embedded_n)) +
  geom_boxplot() +
  labs(x = "Complementizer Presence (comp_type)", 
       y = "Embedded n Local Informativity (local_embedded_n)", 
       title = "Embedded n Sum by Complementizer Presence")

# 2. plot verb_sum by comp_type
ggplot(that_sentence_freq, aes(x = as.factor(comp_type), y = local_verb)) +
  geom_boxplot() +
  labs(x = "Complementizer Presence (comp_type)", 
       y = "Verb Local Informativity (verb_sum)", 
       title = "Verb Sum by Complementizer Presence")

# 3. Correlation Matrix
# leave "matrix_verb_to_cc" out 
cols <- c("local_embedded_n", "local_verb", "frequency_verb", "frequency_cc","expected_embedded_n", "expected_verb")
corr_matrix <- cor(that_sentence_freq[, cols], use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)

# 4. Logistic Regression
# that_sentence_freq$semantic_type <- as.factor(that_sentence_freq$semantic_type)
model_random <- glmer(comp_type ~ local_embedded_n + local_verb + frequency_verb + frequency_cc + expected_embedded_n + expected_verb + (1|verb), 
             data = that_sentence_freq, family = binomial())
summary(model_random)


model <- glm(comp_type ~ local_embedded_n + local_verb + frequency_verb + frequency_cc + expected_embedded_n + expected_verb, 
                      data = that_sentence_freq, family = binomial())
summary(model)

anova(model, model_random, test="LRT")

model_bayesian <- brm(comp_type ~ local_embedded_n + local_verb + frequency_verb + frequency_cc + expected_embedded_n + expected_verb, 
             data = that_sentence_freq, 
             family="bernoulli",
             iter=8000,
             warmup = 4000,
             chains=4,
             cores=4,
             control=list(max_treedepth = 15, adapt_delta = 0.99),
             prior = c(set_prior("normal(0,3)", class = "b")),
             file="../cache/model_bayesian",
             seed=1024)
summary(model_bayesian)

# Extract the coefficients matrix from the model summary
coef_matrix <- summary(model_random)$coefficients

# Convert the matrix to a data frame and add a column for the term names
coef_df <- as.data.frame(coef_matrix)
coef_df$term <- rownames(coef_df)
names(coef_df)[names(coef_df) == "Std. Error"] <- "std_err"
names(coef_df)[names(coef_df) == "z value"] <- "z_value"

rownames(coef_df) <- NULL

# Calculate 95% confidence intervals
coef_df$lower <- coef_df$Estimate - 1.96 * coef_df$std_err
coef_df$upper <- coef_df$Estimate + 1.96 * coef_df$std_err

# Plot the coefficients with error bars using ggplot2
coefficient_plot <- ggplot(coef_df, aes(x = term, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +  # flip the coordinates for better readability
  labs(title = "Logistic Regression Coefficients",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal()
coefficient_plot
ggsave(coefficient_plot, file="../graphs/coefficient_random_plot.pdf", width=6, height=4)         


# frequency_verb
frequency_verb_model <- glmer(comp_type ~ local_embedded_n + local_verb  + frequency_cc + expected_embedded_n + expected_verb + (1|verb), 
data = that_sentence_freq, family = binomial())
summary(frequency_verb_model)
anova(frequency_verb_model, model_random,  test="LRT")

# frequency_cc
frequency_cc_model <- glmer(comp_type ~ local_embedded_n + local_verb  + frequency_verb + expected_embedded_n + expected_verb + (1|verb), 
                            data = that_sentence_freq, family = binomial())
summary(frequency_cc_model)
anova(frequency_cc_model, model_random,  test="LRT")

# local_verb
local_verb_model <- glmer(comp_type ~ local_embedded_n + frequency_verb + frequency_cc + expected_embedded_n + expected_verb + (1|verb), 
                          data = that_sentence_freq, family = binomial())
summary(local_verb_model)
anova(local_verb_model, model_random,  test="LRT")

# local_embedded_n
local_embedded_n_model <- glmer(comp_type ~ local_verb + frequency_verb + frequency_cc + expected_embedded_n + expected_verb + (1|verb), 
                        data = that_sentence_freq, family = binomial())
summary(local_embedded_n_model)
anova(local_embedded_n_model, model_random,  test="LRT")

# expected_verb
expected_verb_model <- glmer(comp_type ~ local_embedded_n + local_verb + frequency_verb + frequency_cc + expected_embedded_n + (1|verb), 
                              data = that_sentence_freq, family = binomial())
summary(expected_verb_model)
anova(expected_verb_model, model_random,  test="LRT")

# expected_embedded_n
expected_embedded_n_model <- glmer(comp_type ~ local_embedded_n + local_verb + frequency_verb + frequency_cc + expected_verb + (1|verb), 
                           data = that_sentence_freq, family = binomial())
summary(expected_embedded_n_model)
anova(expected_embedded_n_model, model_random,  test="LRT")

