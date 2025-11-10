library(lme4)
library(lmerTest)
library(brms)
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
# load dolma
dolma_files <- list.files(path="../../data", pattern="dolma_v1_6-sample_surprisal_verb_*", full.names=TRUE, recursive=FALSE)

df.dolma <- lapply(dolma_files,
                     FUN = read.csv,
                     header = TRUE) %>%
  bind_rows()

# df.dolma <- read.csv("../../data/dolma_v1_6-sample_test_processed.csv",header=TRUE)
# write.csv(head(df.dolma, n = 300), "../../data/TEST_dolma_v1_6-sample_surprisal_all_verb.csv")

# word count
total_words = 7778838
word_count <- read.csv("../../data/word_count.csv", header=TRUE) %>% 
  filter(word != " ") %>% 
  mutate(embedded_n_percentage = count/total_words,
         frequency_embedded_n = -log(embedded_n_percentage, base=2)) %>% 
  rename(embedded_first_word = "word")

df.dolma_freq <- df.dolma %>% 
  mutate(complement_type=if_else(complementizer %in% c("none", "other"), "other", complement_type)) %>% 
  left_join(., word_count, by="embedded_first_word")

# remove if the verb is before the matrix subject
df.dolma_freq <- df.dolma_freq %>% 
  filter(is.na(matrix_predicate_position) | matrix_predicate_position>=0)
  
# remove verbs with ccomp < 5
df.dolma_ccomp_summary <- df.dolma_freq %>% 
  group_by(matrix_predicate_lemma) %>% 
  summarize(ccomp = sum(complement_type == "ccomp"),
            nonCP = sum(complement_type == "other")) %>% 
  ungroup()
verb_list <- subset(df.dolma_ccomp_summary, ccomp>5)$matrix_predicate_lemma
df.dolma_freq <- df.dolma_freq %>% 
  filter(matrix_predicate_lemma %in% verb_list)

# get counts
total_verb <- length(df.dolma$matrix_predicate_lemma)
total_verb # 332353

df.dolma_summary <- df.dolma_freq %>% 
  group_by(matrix_predicate_lemma) %>% 
  summarize(verb_count = n(),
            verb_percentage = verb_count/total_verb,
            frequency_verb = -log(verb_percentage, base=2),
            ccomp = sum(complement_type == "ccomp"),
            other = sum(complement_type == "other"), #  this includes either the embedded-wh or nonCP
            ccomp_percentage = ccomp/verb_count,
            frequency_cc = -log(ccomp_percentage, base=2),
            that = sum(complementizer=="that"),
            omitted = sum(complementizer=="omitted"),
            that_percentage = that/verb_count) %>% 
  ungroup()

# plot 20 most frequent verbs in terms of their overall freq
dolma_verb_20 <- df.dolma_summary %>% 
  arrange(desc(verb_count)) %>% 
  head(n=20) %>% 
  pivot_longer(cols=c("that","omitted","other"),
               names_to="type",
               values_to="count") %>% 
  mutate(type = fct_relevel(type, "other", "omitted"))

ggplot(dolma_verb_20,
       aes(x=reorder(matrix_predicate_lemma, verb_count),
           y=count,
           fill=type))+
  geom_col()+
  # scale_y_log10()+ # log-scale
  coord_flip()+
  labs(x = "Count", 
       y = "Verb")

# plot 20 most frequent verbs with ccomp in terms of their overal freq
dolma_ccomp_verb_20 <- df.dolma_summary %>% 
  arrange(desc(ccomp)) %>% 
  head(n=20) %>% 
  pivot_longer(cols=c("that","omitted"),
               names_to="type",
               values_to="count")

ggplot(dolma_ccomp_verb_20,
       aes(x=reorder(matrix_predicate_lemma, ccomp),
           y=count,
           fill=type))+
  geom_col()+
  # scale_y_log10()+ # log-scale
  coord_flip()+
  labs(x = "Verb", 
       y = "Count")

# plot 20 least frequent verbs with ccomp in terms of their uses
dolma_ccomp_verb_least_20 <- df.dolma_summary %>% 
  arrange(ccomp) %>% 
  head(n=20) %>% 
  pivot_longer(cols=c("that","omitted"),
               names_to="type",
               values_to="count")

ggplot(dolma_ccomp_verb_least_20,
       aes(x=reorder(matrix_predicate_lemma, ccomp),
           y=count,
           fill=type))+
  geom_col()+
  # scale_y_log10()+ # log-scale
  coord_flip()+
  labs(x = "Verb", 
       y = "Count")

# clean up the data for analysis
df.dolma_entropy <- df.dolma_freq %>% 
  filter(!is.na(verb_sum) & !is.na(cc_with_that_sum)) %>% 
  # mutate(local_embedded_n = cc_no_that_sum - cc_with_that_sum) %>% 
  rename(local_verb = "verb_sum",
         local_embedded_n = "cc_no_that_sum",
         entropy_verb = "verb_entropy",
         entropy_embedded_n = "cc_n_entropy")
  # mutate(local_onset = local_onset / cc_onset) %>% 
  # na.omit(local_onset)

# calculate the expected informativity
df.dolma_expected <- df.dolma_entropy %>%
  group_by(matrix_predicate_lemma) %>%
  summarize(expected_verb = mean(local_verb, na.rm=TRUE), # expected info of the verb
            expected_embedded_n = mean(local_embedded_n, na.rm=TRUE)) %>% 
  ungroup()
            # expected_onset = mean(local_onset, na.rm=TRUE)) # expected info of first n words of the embedded clause

df.dolma_ccomp_full <- left_join(df.dolma_entropy, df.dolma_summary, by="matrix_predicate_lemma") %>%
  left_join(., df.dolma_expected, by="matrix_predicate_lemma")

df.dolma_ccomp_full <- df.dolma_ccomp_full %>% 
  filter(matrix_predicate_to_cc >= 0) %>% 
  mutate(embedded_subject_type = case_when(tolower(embedded_subject_head) == "i" ~ "I",
                                           tolower(embedded_subject_head) == "it" ~ "it",
                                           tolower(embedded_subject_head) %in% c("he", "she", "they", "you") ~ "pronoun", 
                                           TRUE ~ "NP"),
         embedded_length = embedded_clause_minus_one + 1,
         matrix_length = sapply(strsplit(matrix_span_verb, " "), length) + matrix_predicate_to_cc)

df.dolma_ccomp_that <- df.dolma_ccomp_full %>% 
  select(c("complementizer", "matrix_predicate_lemma", "matrix_predicate_id", "matrix_predicate_position", "matrix_predicate_to_cc", "matrix_subject_type", "cc_onset", "cc_remainder", "frequency_verb", "frequency_cc", "frequency_embedded_n", "local_verb",  "local_embedded_n", "expected_verb",  "expected_embedded_n", "doc_id", "entropy_verb","entropy_embedded_n", "embedded_subject_type", "embedded_length", "matrix_length", "verb_count"))

df.dolma_ccomp_that <- df.dolma_ccomp_that %>% 
  filter(!is.na(matrix_subject_type) & matrix_subject_type != "") %>%
  mutate(frequency_verb = as.numeric(scale(frequency_verb, center=TRUE, scale=TRUE)),
         frequency_cc = as.numeric(scale(frequency_cc, center=TRUE, scale=TRUE)),
         frequency_embedded_n = as.numeric(scale(frequency_embedded_n, center=TRUE, scale=TRUE)),
         local_verb = as.numeric(scale(local_verb, center=TRUE, scale=TRUE)),
         local_embedded_n = as.numeric(scale(local_embedded_n, center=TRUE, scale=TRUE)),
         # local_onset = as.numeric(scale(local_onset, center=TRUE, scale=TRUE)),
         expected_verb = as.numeric(scale(expected_verb, center=TRUE, scale=TRUE)),
         expected_embedded_n = as.numeric(scale(expected_embedded_n, center=TRUE, scale=TRUE)),
         # expected_onset = as.numeric(scale(expected_onset, center=TRUE, scale=TRUE)),
         matrix_subject_type = as.factor(matrix_subject_type),
         matrix_predicate_id = as.numeric(scale(matrix_predicate_id, center=TRUE, scale=TRUE)),
         matrix_predicate_position = as.numeric(scale(matrix_predicate_position, center=TRUE, scale=TRUE)),
         matrix_predicate_to_cc_scale = as.numeric(scale(matrix_predicate_to_cc, center=TRUE, scale=TRUE)),
         cc_onset = as.numeric(scale(cc_onset, center=TRUE, scale=TRUE)),
         cc_remainder = as.numeric(scale(cc_remainder, center=TRUE, scale=TRUE)),
         doc_id = as.factor(doc_id),
         embedded_length = as.numeric(scale(embedded_length, center=TRUE, scale=TRUE)),
         embedded_subject_type = as.factor(embedded_subject_type),
         matrix_length = as.numeric(scale(matrix_length, center=TRUE, scale=TRUE)),
         verb_count = as.numeric(scale(verb_count, center=TRUE, scale=TRUE)),
         entropy_verb = as.numeric(scale(entropy_verb, center=TRUE, scale=TRUE)),
         entropy_embedded_n = as.numeric(scale(entropy_embedded_n, center=TRUE, scale=TRUE)),
         complementizer = fct_relevel(as.factor(complementizer),"that"))

df.dolma_ccomp_that <- df.dolma_ccomp_that %>%
  mutate(complementizer_num = if_else(complementizer == "that", 1, 0))

all_counts <- length(df.dolma_ccomp_that$complementizer) 
all_counts # 69573
that_count <- sum(df.dolma_ccomp_that$complementizer_num) 
that_count # 24938
that_count / all_counts # 0.358

# to do: preferences of individual verb

length(unique(df.dolma_ccomp_that$matrix_predicate_lemma)) # 127

# 2. Analysis ----
# 1. plot embedded n word surprisal by comp_type
ggplot(df.dolma_ccomp_that, 
       aes(x = as.factor(complementizer), 
           y = local_embedded_n)) +
  geom_boxplot() +
  labs(x = "Complementizer Presence", 
       y = "Embedded Onset Local Informativity (local_embedded_onset)", 
       title = "Embedded Onset Local Informativity by Complementizer Presence")

# 2. plot verb expected surprisal and onset surprisal
ggplot(df.dolma_ccomp_that,
       aes(x = as.factor(complementizer),
           y = expected_verb)) +
  geom_boxplot() +
  labs(x = "Complementizer Presence ", 
       y = "Verb Local Informativity (local_verb)", 
       title = "Verb Local Informativity by Complementizer Presence")

ggplot(df.dolma_ccomp_that,
       aes(x = as.factor(complementizer),
           y = local_embedded_n)) +
  geom_boxplot() +
  labs(x = "Complementizer Presence ", 
       y = "Onset surprisal (local_embedded_n)", 
       title = "Onset Local Informativity by Complementizer Presence")

# 3. Correlation Matrix
# control variables
cols <- c("cc_remainder","cc_onset", "matrix_predicate_position", "matrix_predicate_to_cc_scale")
corr_matrix <- cor(df.dolma_ccomp_that[, cols], use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)

# main variables
cols <- c("frequency_verb", "frequency_cc", "frequency_embedded_n", "local_verb", "local_embedded_n", "expected_verb", "expected_embedded_n")
corr_matrix <- cor(df.dolma_ccomp_that[, cols], use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)

# control and main variables
cols <- c("cc_remainder","cc_onset", "matrix_predicate_position", "matrix_predicate_to_cc_scale", "frequency_verb", "frequency_cc", "frequency_embedded_n", "local_verb", "local_embedded_n", "expected_verb", "expected_embedded_n")
corr_matrix <- cor(df.dolma_ccomp_that[, cols], use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)

# correlation between CC onset surprisal and CC onset entropy (should match with the values in the corr matrix)
cor(df.dolma_ccomp_that$expected_embedded_n, df.dolma_ccomp_that$local_embedded_n, method = "pearson")
cor(df.dolma_ccomp_that$frequency_verb, df.dolma_ccomp_that$local_verb, method = "pearson")
# cor(df.dolma_ccomp_that$verb_count, df.dolma_ccomp_that$local_verb, method = "pearson")
cor(df.dolma_ccomp_that$local_embedded_n, df.dolma_ccomp_that$entropy_embedded_n, method = "pearson") # r = -0.02 in rabinovich
# cor(df.dolma_ccomp_that$local_onset, df.dolma_ccomp_that$cc_onset, method = "pearson")

# 4. Residualize highly correlated variables
# residualize highly correlated items
resid_model_verb_frequency_expected <- lm(expected_verb ~ frequency_verb, data = df.dolma_ccomp_that)
summary(resid_model_verb_frequency_expected)
df.dolma_ccomp_that$expected_verb_resid <- resid(resid_model_verb_frequency_expected)

resid_model_entropy_verb_embedded <- lm(entropy_embedded_n ~ entropy_verb, data = df.dolma_ccomp_that)
summary(resid_model_entropy_verb_embedded)
df.dolma_ccomp_that$entropy_verb_resid <- resid(resid_model_entropy_verb_embedded)

resid_model_local_embedded_n <- lm(local_embedded_n ~ matrix_predicate_to_cc_scale, data = df.dolma_ccomp_that)
summary(resid_model_local_embedded_n)
df.dolma_ccomp_that$local_embedded_n_resid <- resid(resid_model_local_embedded_n)

# control and main variables after resid
cols <- c("cc_remainder","cc_onset", "matrix_predicate_position", "matrix_predicate_to_cc_scale", "frequency_verb", "frequency_cc", "frequency_embedded_n", "local_verb", "local_embedded_n_resid", "expected_verb_resid", "expected_embedded_n")
corr_matrix <- cor(df.dolma_ccomp_that[, cols], use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)

# 5. Logistic Regression
df.dolma_ccomp_that$item <- 1:nrow(df.dolma_ccomp_that)
df.dolma_ccomp_that$item <- as.factor(df.dolma_ccomp_that$item)
# predict the likelihood of dropping "that"
levels(df.dolma_ccomp_that$complementizer)

# grammaticalization
model_grammatical <- glm(complementizer ~ cc_onset + cc_remainder + matrix_predicate_position + frequency_verb + local_embedded_n + expected_verb_resid + frequency_cc,
                         data = df.dolma_ccomp_that,
                         family = binomial())
summary(model_grammatical)

model_grammatical_random <- glmer(complementizer ~ cc_remainder + cc_onset + matrix_predicate_position + frequency_verb + local_embedded_n + expected_verb_resid + frequency_cc + (1|matrix_predicate_lemma),
                         data = df.dolma_ccomp_that,
                         family = binomial())
summary(model_grammatical_random)

# analysis from rabinovich 
model_rabinovich <- glm(complementizer ~ cc_remainder + cc_onset + matrix_predicate_position + frequency_verb + local_embedded_n + entropy_embedded_n,
                   data = df.dolma_ccomp_that %>% 
                     filter(matrix_predicate_to_cc == 0),
                   family = binomial())
summary(model_rabinovich)

# rabinovich + random verb effect
model_rabinovich_random <- glmer(complementizer ~ cc_remainder + cc_onset + matrix_length + frequency_verb + local_embedded_n + entropy_embedded_n + (1|matrix_predicate_lemma),
                   data = df.dolma_ccomp_that %>% 
                     filter(matrix_predicate_to_cc == 0),
                   family = binomial(),
                   control = glmerControl(optimizer = "bobyqa"))
summary(model_rabinovich_random)

# rabinovich + additional predictors (frequency_verb instead of verb_count, entropy_verb_resid instead of entropy_verb)
model_rabinovich_full <- glm(complementizer ~ matrix_subject_type + cc_onset + cc_remainder + matrix_length + frequency_verb + local_embedded_n + entropy_embedded_n + cc_onset + frequency_cc + local_verb + entropy_verb_resid + matrix_predicate_to_cc_scale,
                                 data = df.dolma_ccomp_that,
                                 family = binomial())
summary(model_rabinovich_full)

# rabinovich + additional predictors + random verb effect (frequency_verb instead of verb_count)
model_rabinovich_full_random <- glmer(complementizer ~ matrix_subject_type + matrix_length + cc_onset + cc_remainder + frequency_verb + local_embedded_n + entropy_embedded_n + matrix_predicate_to_cc_scale + frequency_cc + local_verb + entropy_verb_resid + (1|matrix_predicate_lemma),
                        data = df.dolma_ccomp_that,
                        family = binomial(),
                        control = glmerControl(optimizer = "bobyqa"))
summary(model_rabinovich_full_random)

# expected surp + additional predictors (frequency_verb instead of verb_count, matrix_predicate_id instead of matrix length)
# make sure to SCALE (matrix_predicate_to_cc_scale)
model_expected_full <- glm(complementizer ~ matrix_subject_type + cc_remainder + cc_onset + matrix_predicate_position + matrix_predicate_to_cc_scale + frequency_verb + frequency_cc + frequency_embedded_n + local_verb + local_embedded_n +  expected_verb_resid + expected_embedded_n,
                                    data = df.dolma_ccomp_that,
                                    family = binomial())
summary(model_expected_full)

# expected surp + additional predictors + random verb effect (frequency verb instead of verb count, matrix_predicate_id instead of matrix length) 
# make sure to SCALE (matrix_predicate_to_cc_scale)
model_expected_full_random <- glmer(complementizer ~ matrix_subject_type + cc_remainder + cc_onset + matrix_predicate_position + matrix_predicate_to_cc_scale + verb_count + frequency_verb + frequency_cc + frequency_embedded_n + local_verb + local_embedded_n_resid +  expected_verb_resid + expected_embedded_n + (1|matrix_predicate_lemma),
                                 data = df.dolma_ccomp_that,
                                 family = binomial(),
                                 control = glmerControl(optimizer = "bobyqa"))
summary(model_expected_full_random)

model_expected_full_no_subj_random <- glmer(complementizer ~ cc_remainder + cc_onset + matrix_predicate_position + matrix_predicate_to_cc_scale + verb_count + frequency_verb + frequency_cc + frequency_embedded_n + local_verb + local_embedded_n + expected_verb_resid + expected_embedded_n + (1|matrix_predicate_lemma),
                                    data = df.dolma_ccomp_that,
                                    family = binomial(),
                                    control = glmerControl(optimizer = "bobyqa"))
summary(model_expected_full_no_subj_random)

priors <- c(
  prior(normal(0, 1), class = "b"),                 # fixed effects
  prior(student_t(3, 0, 2.5), class = "Intercept"), # intercept
  prior(exponential(1), class = "sd")               # random effect SDs
)
brms_expected_full <- brm(complementizer ~ matrix_subject_type + cc_remainder + cc_onset + matrix_predicate_id + matrix_predicate_to_cc_scale + frequency_verb + frequency_cc + frequency_embedded_n + local_verb + local_embedded_n +  expected_verb_resid + expected_embedded_n + (1|matrix_predicate_lemma),
                          data   = df.dolma_ccomp_that,
                          family = bernoulli(link = "logit"),
                          prior  = priors,
                          chains = 4, cores = 4,
                          iter   = 6000, warmup = 3000, seed = 123,
                          file = "brms_expected_full_random_no_that.rds",
                          control = list(adapt_delta = 0.99, max_treedepth = 15)
)
summary(brms_expected_full)
# saveRDS(brms_expected_full, file = "brms_expected_full_random_no_that.rds")
# brms_expected_full <- readRDS(file = "brms_expected_full_random_no_that.rds")

brms_expected_no_subj_full <- brm(complementizer ~ cc_remainder + cc_onset + matrix_predicate_id + matrix_predicate_to_cc_scale + frequency_verb + frequency_cc + frequency_embedded_n + local_verb + local_embedded_n +  expected_verb_resid + expected_embedded_n + (1|matrix_predicate_lemma),
                          data   = df.dolma_ccomp_that,
                          family = bernoulli(link = "logit"),
                          prior  = priors,
                          chains = 4, cores = 4,
                          iter   = 6000, warmup = 3000, seed = 123,
                          file = "brms_expected_full_random_no_subj.rds",
                          control = list(adapt_delta = 0.99, max_treedepth = 15)
)
summary(brms_expected_no_subj_full)

fixef_matrix <- fixef(brms_expected_full, robust = TRUE, probs = c(.025, .975))
fixef_df <- as.data.frame(fixef_matrix) %>% 
  mutate(term = rownames(fixef_matrix)) %>% 
  filter(term %in% c("frequency_verb", "frequency_cc", "frequency_embedded_n", "local_verb", "local_embedded_n", "expected_verb_resid", "expected_embedded_n")) %>% 
  mutate(term = case_when(term == "frequency_verb" ~ "noncontext_verb",
                          term == "frequency_cc" ~ "noncontext_cc",
                          term == "frequency_embedded_n" ~ "noncontext_w1",
                          term == "local_embedded_n" ~ "local_w1",
                          term == "expected_verb_resid" ~ "expected_verb",
                          term == "expected_embedded_n" ~ "expected_w1",
                          TRUE ~ term)) %>% 
  rename(lower = "Q2.5",
         upper = "Q97.5")
  
# Plot the coefficients with error bars using ggplot2
coefficient_blank_plot <- ggplot(fixef_df, aes(x = term, y = Estimate)) +
  geom_point(size = 0, color="lightgrey") +
  scale_x_discrete(drop = FALSE) +
  ylim(-0.55, 0.3) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  coord_flip() +  # flip the coordinates for better readability
  labs(title = "Bayesian Logistic Regression Coefficients",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal()
coefficient_blank_plot
ggsave(coefficient_blank_plot, file="../graphs/blank_coefficient_plot.pdf", width=6, height=4)  

coefficient_noncontext_plot <- ggplot(fixef_df, aes(x = term, y = Estimate)) +
  geom_blank(data = fixef_df, aes(x = term, y = 0)) +
  geom_point(data=fixef_df %>% 
               filter(term %in% c("noncontext_verb", "noncontext_w1", "noncontext_cc")), 
             size = 3) +
  geom_errorbar(data=fixef_df %>% 
                  filter(term %in% c("noncontext_verb", "noncontext_w1", "noncontext_cc")),
                aes(ymin = lower, ymax = upper), width = 0.2) +
  ylim(-0.55, 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  coord_flip() +  # flip the coordinates for better readability
  labs(title = "Bayesian Logistic Regression Coefficients",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal()
coefficient_noncontext_plot
ggsave(coefficient_noncontext_plot, file="../graphs/noncontext_coefficient_plot.pdf", width=6, height=4) 

coefficient_local_plot <- ggplot(fixef_df, aes(x = term, y = Estimate)) +
  geom_blank(data = fixef_df, aes(x = term, y = 0)) +
  geom_point(data=fixef_df %>% 
               filter(term %in% c("noncontext_verb", "noncontext_w1", "noncontext_cc", "local_verb", "local_w1")), 
             size = 3) +
  geom_errorbar(data=fixef_df %>% 
                  filter(term %in% c("noncontext_verb", "noncontext_w1", "noncontext_cc","local_verb", "local_w1")),
                aes(ymin = lower, ymax = upper), width = 0.2) +
  ylim(-0.55, 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  coord_flip() +  # flip the coordinates for better readability
  labs(title = "Bayesian Logistic Regression Coefficients",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal()
coefficient_local_plot
ggsave(coefficient_local_plot, file="../graphs/noncontext_local_plot.pdf", width=6, height=4)

coefficient_plot <- ggplot(fixef_df, aes(x = term, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  ylim(-0.55, 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  coord_flip() +  # flip the coordinates for better readability
  labs(title = "Bayesian Logistic Regression Coefficients",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal()
coefficient_plot
ggsave(coefficient_plot, file="../graphs/brms_expected_full_random_coefficient_plot.pdf", width=6, height=4)  

# info only measures (frequency verb instead of verb count)
model_info <- glm(complementizer ~ frequency_verb + frequency_cc + frequency_embedded_n + local_verb + local_embedded_n + expected_embedded_n + expected_verb_resid,
                           data = df.dolma_ccomp_that,
                           family = binomial())
summary(model_info)

# info only measures + random verb effect (frequency verb instead of verb count)
model_info_random <- glmer(complementizer ~ frequency_verb + frequency_cc + frequency_embedded_n + local_verb + local_embedded_n + expected_embedded_n + expected_verb_resid + (1|matrix_predicate_lemma),
                                    data = df.dolma_ccomp_that,
                                    family = binomial(),
                                    control = glmerControl(optimizer = "bobyqa"))
summary(model_info_random)

# individual verbs
# top-20 words
model_think <- glm(complementizer ~ matrix_subject_type + embedded_length + matrix_predicate_id + local_embedded_n + cc_onset + local_verb, 
                      data = df.dolma_ccomp_that %>% 
                        filter(matrix_predicate_lemma == "think"),
                      family = binomial())
summary(model_think)

model_believe <- glm(complementizer ~ matrix_subject_type + embedded_length + matrix_predicate_id + local_embedded_n + cc_onset + local_verb, 
                         data = df.dolma_ccomp_that %>% 
                           filter(matrix_predicate_lemma == "believe"),
                         family = binomial())
summary(model_believe)

model_say <- glm(complementizer ~ matrix_subject_type + embedded_length + matrix_predicate_id + local_embedded_n + cc_onset + local_verb, 
                 data = df.dolma_ccomp_that %>% 
                   filter(matrix_predicate_lemma == "say"),
                 family = binomial())
summary(model_say)

# bottom-20 words
model_caution <- glm(complementizer ~ matrix_subject_type + embedded_length + matrix_predicate_id + local_embedded_n + cc_onset + local_verb, 
                     data = df.dolma_ccomp_that %>% 
                       filter(matrix_predicate_lemma == "caution"),
                     family = binomial())
summary(model_caution)

model_dream <- glm(complementizer ~ matrix_subject_type + embedded_length + matrix_predicate_id + local_embedded_n + cc_onset + local_verb, 
                         data = df.dolma_ccomp_that %>% 
                           filter(matrix_predicate_lemma == "dream"),
                         family = binomial())
summary(model_dream)

model_boast <- glm(complementizer ~ matrix_subject_type + embedded_length + matrix_predicate_id + local_embedded_n + cc_onset + local_verb, 
                     data = df.dolma_ccomp_that %>% 
                       filter(matrix_predicate_lemma == "boast"),
                     family = binomial())
summary(model_boast)

model_bayesian <- brm(complementizer ~ local_embedded_onset + local_verb + frequency_verb + frequency_cc + expected_embedded_onset + expected_verb, 
             data = df.dolma_ccomp_that, 
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
coef_matrix <- summary(model_expected_full_random)$coefficients

# Convert the matrix to a data frame and add a column for the term names
coef_df <- as.data.frame(coef_matrix)
coef_df$term <- rownames(coef_df)
names(coef_df)[names(coef_df) == "Std. Error"] <- "std_err"
names(coef_df)[names(coef_df) == "z value"] <- "z_value"

rownames(coef_df) <- NULL

# Calculate 95% confidence intervals
coef_df$lower <- coef_df$Estimate - 1.96 * coef_df$std_err
coef_df$upper <- coef_df$Estimate + 1.96 * coef_df$std_err

coef_df_info <- coef_df %>% 
  filter(term %in% c("frequency_verb", "frequency_cc", "local_embedded_n", "local_verb", "expected_embedded_n", "expected_verb_resid")) %>% 
  mutate(term = case_when(term == "frequency_verb" ~ "noncontext_verb",
                          term == "frequency_cc" ~ "noncontext_cc",
                          term == "expected_verb_resid" ~ "expected_verb",
                          TRUE ~ term))

# Plot the coefficients with error bars using ggplot2
coefficient_plot <- ggplot(coef_df_info, aes(x = term, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color="grey") +
  coord_flip() +  # flip the coordinates for better readability
  labs(title = "Logistic Regression Coefficients",
       x = "Predictor",
       y = "Coefficient Estimate") +
  theme_minimal()
coefficient_plot
ggsave(coefficient_plot, file="../graphs/full_random_coefficient_plot.jpeg", width=6, height=4)         

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

