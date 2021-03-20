# setting up libraries
library(dplyr) 
library(magrittr) 
library(tidyr) 
library(ggplot2) 
theme_set(theme_bw()) 
library(extrafont)
font_import(pattern = "Times New Roman", prompt = F)
loadfonts()
library(readxl)
library(gdata) 
library(stringr) 
library(brms) 
library(bayesplot) 
library(tidybayes)
bayesplot_theme_set(new = theme_bw()) 
library(modelr)
library(MASS) 


# read the second data frame, drop NA values
df2 <- read_excel("data.exp2.xlsx" ) %>% drop_na()


# encode vector types
df2$Subject %<>% as.integer()
df2$Language %<>% as.factor() %>% reorder.factor(new.order = c("Turkish", "English"))
df2$Question %<>% as.factor()
df2$Proficiency %<>% as.factor()
df2$Emotion1_GALC %<>% as.factor()
df2$Emotion_dyads %<>% as.factor()
df2$Main_emotion %<>% as.factor()
df2$Dominance_ANEW %<>% as.factor() %>% reorder.factor(new.order = c("Low", "High", "Mixed or no emotion"))
df2$arousal%<>% as.factor()  %>% reorder.factor(new.order = c("Low", "High", "Mixed or no emotion"))
df2$mixed %<>% as.factor() 

# create nomix df 
df2_forp1 <- df2 %>%
  subset(Emotion_dyads != "Mixed or no emotion") %>% 
  subset(Emotion1_GALC %in% c("Happiness", "Admiration/Awe", "Anger", "Disappointment", "Anxiety", "Shame", "Feelinglove", "Guilt", "Pride", "Sadness")) %>% 
  subset(Main_emotion != "Mixed or no emotion")


df2_nomix <- df2 %>% subset(arousal != "Mixed or no emotion") %>% subset(Main_emotion != "Mixed or no emotion") %>% 
    subset(Emotion1_GALC != "Mixed or no emotion")

# confirm the deletion of level "mixed" for df2_nomix
df2_nomix$Dominance_ANEW <- factor(df2_nomix$Dominance_ANEW)
df2_nomix$arousal <- factor(df2_nomix$arousal)

# checking contrasts
contrasts(df2_nomix$arousal)
contrasts(df2_nomix$Language)
contrasts(df2_nomix$Proficiency)
contrasts(df2_nomix$Dominance_ANEW)

sort(table(df2_nomix$Emotion1_GALC),decreasing=TRUE)[1:30]

# summaries
df2 %>%
  group_by(Language, Proficiency) %>% 
  summarize(prop = mean(arousal == "High"))

df2_nomix %>%
  group_by(Language, Proficiency) %>% 
  summarize(prop = mean(arousal == "High"))

df2_nomix %>%
  group_by(Language) %>% 
  summarize(prop = mean(arousal == "High"))

df2_nomix %>%
  group_by(Proficiency) %>% 
  summarize(prop = mean(arousal == "High"))

# plot things
p1 <- df2_forp1 %>% 
  ggplot(aes(Emotion1_GALC, group = Proficiency)) +
  geom_bar(aes(y = ..prop.., fill = Proficiency), stat="count", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual("Proficiency", values = c("Intermediate" = "#9ecae1", "Proficient" = "#084c94")) +
  facet_wrap(~ Language) + 
  xlab("Emotion Type") + ylab("Percentage") +
  theme(text=element_text(size=15)) 
ggsave("p1.png", plot = p1, width = 10, height = 8)
  
p2 <- df2_nomix %>% 
    ggplot(aes(arousal, group = Proficiency)) +
    geom_bar(aes(y = ..prop.., fill = Proficiency), stat="count", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    scale_fill_manual("Proficiency", values = c("Intermediate" = "#9ecae1", "Proficient" = "#084c94")) +
    facet_wrap(~ Language) + 
    xlab("Arousal") + ylab("Percentage") +
  theme(text=element_text(size=15)) 
ggsave("p2.png", plot = p2, width = 7, height = 8)

 p3 <- df2_forp1  %>% subset(Language == "English") %>% 
    ggplot(aes(arousal, group = Proficiency)) +
    geom_bar(aes(y = ..prop.., fill = Proficiency), stat="count", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    scale_fill_manual("Proficiency", values = c("Intermediate" = "#9ecae1", "Proficient" = "#084c94")) +
    xlab("Arousal") + ylab("Percentage") +
  theme(text=element_text(size=15)) 
  ggsave("p3.png", plot = p3, width = 7, height = 8)
  
# model for arousal nomix 
arousal.model.nomix <- brm(arousal ~ Language + Proficiency, 
                  family = bernoulli(link = "logit"), data = df2_nomix,
                  chains = 4, cores = 4, file = "m1")

summary(arousal.model.nomix)

#model for incorrect responses
mixed.model <- brm(mixed ~ Language + Proficiency,
                         family = bernoulli(link = "logit"), data = df2,
                         chains = 4, cores = 4, file = "m2")
summary(mixed.model)
 

#plots for model1

# creating a df for the model results with the mcmc_intervals_data function. 
# I included only the predictor estimate.
df_m1 <- mcmc_intervals_data(arousal.model.nomix, pars = vars(starts_with("b_"))) %>% 
  subset(parameter != "b_Intercept")

# recoding the predictor estimate.
df_m1$parameter %<>% dplyr::recode(b_LanguageEnglish = "English",
                                   b_ProficiencyProficient  = "Proficient",
                                   b_Dominance_ANEWHigh = "High Dominance",
                                   'b_LanguageEnglish:ProficiencyProficient' = "English*Proficient")

df_m1$parameter %<>% reorder.factor(new.order = c("English*Proficient", "High Dominance", "Proficient", "English"))
                              
# plotting the model plot with credible intervals, and a dashed vertical line at the intercept (0).
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 2) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, colour = "black") +
  theme_minimal() +
  scale_x_continuous(limits = c(-1, 3.70)) +
  xlab("Estimate (log)") + ylab("Coefficient") +
  theme(text=element_text(size=15))

ggsave("p4.png", plot = p4, width = 6, height = 6)


#plots for model2

# creating a df for the model results with the mcmc_intervals_data function. 
# I included only the predictor estimate.
df_m2 <- mcmc_intervals_data(mixed.model, pars = vars(starts_with("b_"))) %>% subset(parameter != "b_Intercept")

# recoding the predictor estimate. SPECIAL
df_m2$parameter %<>% dplyr::recode(b_LanguageEnglish = "English",
                                   b_ProficiencyProficient  = "Proficient",
                                   'b_LanguageEnglish:ProficiencyProficient' = "English*Proficient")
df_m2$parameter %<>% reorder.factor(new.order = c("English*Proficient","Proficient", "English"))
                               
# plotting the model plot with credible intervals, and a dashed vertical line at the intercept (0). 
p5 <- df_m2 %>% ggplot(aes(m, parameter)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 2) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, colour = "black") +
  theme_minimal() +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  xlab("Estimate (log)") + ylab("Coefficient") +
  theme(text=element_text(size=15))

ggsave("p5.png", plot = p5, width = 6, height = 6)

