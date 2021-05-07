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

#read the first data frame
df1 <- read_excel("culture_df.xlsx") %>% drop_na()

df1$Culture %<>% dplyr::recode('1' = "German", '2' = "Turkish", '3' = "Japanese")

# encode vector types
df1$Subject %<>% as.integer()
df1$Culture %<>%  as.factor() %>%  reorder.factor(new.order = c("Turkish","German", "Japanese"))
df1$Question %<>% as.factor()
df1$Dyad %<>% as.factor()
df1$Emotion1_GALC %<>% as.factor()
df1$Arousal_ANEW %<>%  as.factor() %>% reorder.factor(new.order = c("Low","High"))
df1$Dominance_ANEW %<>% as.factor() %>% reorder.factor(new.order = c("Low","High"))

df_model <- df1 %>% dplyr::select(Subject, Culture, Question, Arousal_ANEW, Dominance_ANEW)

df_nomix <- df1 %>% subset(Dyad != "Conflict") %>% subset(Emotion1_GALC %in% c("Happiness", "Anger", "Disappointment", "Anxiety", "Shame", "Feelinglove", "Guilt", "Pride", "Sadness", "Gratitude", "Positive", "Contentment"))

#summary

df_nomix %>%
  group_by(Culture) %>% 
  dplyr::summarize(prop = mean(Arousal_ANEW == "High"))


df_nomix %>%
  group_by(Culture) %>% 
  dplyr::summarize(prop = mean(Dominance_ANEW == "High"))

#Plots
sort(table(df_nomix$Emotion1_GALC),decreasing=TRUE)[1:12]

p1 <- df_nomix %>% 
  ggplot(aes(Emotion1_GALC, group = Culture)) +
  geom_bar(aes(y = ..prop.., fill = Culture), stat="count", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual("Culture", values = c("German" = "#9ecae1", "Japanese" = "#084c94", "Turkish" = "#4682B4")) +
  xlab("Emotion Type") + ylab("Percentage") +
  theme(text=element_text(size=15)) 
ggsave("p1_ills.png", plot = p1, width = 10, height = 8)

p2 <- df_nomix %>% 
  ggplot(aes(Arousal_ANEW, group = Culture)) +
  geom_bar(aes(y = ..prop.., fill = Culture), stat="count", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual("Culture", values = c("German" = "#9ecae1", "Japanese" = "#084c94", "Turkish" = "#4682B4")) +
  xlab("Arousal") + ylab("Percentage") +
  theme(text=element_text(size=15)) 
ggsave("p2_ills.png", plot = p2, width = 7, height = 8)

p3 <- df_nomix %>% 
  ggplot(aes(Dominance_ANEW, group = Culture)) +
  geom_bar(aes(y = ..prop.., fill = Culture), stat="count", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_manual("Culture", values = c("German" = "#9ecae1", "Japanese" = "#084c94", "Turkish" = "#4682B4")) +
  xlab("Dominance") + ylab("Percentage") +
  theme(text=element_text(size=15)) 
ggsave("p3_ills.png", plot = p3, width = 7, height = 8)

#plot for idv

dfidv <- data.frame(country = c("Germany", "Japan", "Turkey"), idv = c(67, 46, 37))

p6 <- ggplot(dfidv) +
  aes(x = country, y = idv, label = idv) +
  geom_bar(fill = "#ca4b2b", stat = "identity") +
  theme_minimal() +
  xlab("Country") + ylab("IDV Score") +
  geom_text(size = 5, position = position_nudge(y = 2)) +
  theme(text=element_text(size=15))
ggsave("p6_ills.png", plot = p6, width = 7, height = 8)

df_nomix %<>% mutate(idv = case_when(
  Culture == "German" ~ 67,
  Culture == "Japanese" ~ 46,
  Culture == "Turkish" ~ 37))

df_nomix %<>% mutate(arousal = case_when(
  Arousal_ANEW == "High" ~ 1,
  Arousal_ANEW == "Low" ~ 0))


# set contrasts for the model#
contrasts(df_nomix$Culture) <- contr.sum(3)
contrasts(df_nomix$Dominance_ANEW) <- contr.sum(2)
contrasts(df_nomix$Arousal_ANEW) <- contr.sum(2)


# model for Arousal 

culturem1 <- brm(Dominance_ANEW  ~ Culture,
                 family = bernoulli(link = "logit"), data = df_nomix,
                 chains = 4, cores = 4)

culturem2 <- brm(Arousal_ANEW ~ Culture + Dominance_ANEW,
                 family = bernoulli(link = "logit"), data = df_nomix,
                 chains = 4, cores = 4)

#plots for model1

# creating a df for the model results with the mcmc_intervals_data function. I included only the predictor estimate. SPECIAL
dfculture_m1 <- mcmc_intervals_data(culturem2, pars = vars(starts_with("b_"))) %>% subset(parameter != "b_Intercept")

# recoding the predictor estimate. SPECIAL
dfculture_m1$parameter %<>% dplyr::recode(b_CultureGerman = "German vs. Turkish",
                                          b_CultureJapanese  = "Japanese vs. Turkish",
                                          b_Dominance_ANEWHigh = "High Dominance",)

dfculture_m1$parameter %<>% reorder.factor(new.order = c("High Dominance", "Japanese vs. Turkish", "German vs. Turkish"))

# plotting the model plot with credible intervals, and a dashed vertical line at the intercept (0). SPECIAL. Modele plot yapmak zorundna de??ilsin
p4 <- dfculture_m1 %>% ggplot(aes(m, parameter)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 2) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, colour = "black") +
  theme_minimal() +
  scale_x_continuous(limits = c(-1, 3.90)) +
  xlab("Estimate (log)") + ylab("Coefficient") +
  theme(text=element_text(size=15))

ggsave("p4_ills.png", plot = p4, width = 6, height = 6)

#plot for m2

# creating a df for the model results with the mcmc_intervals_data function. I included only the predictor estimate. SPECIAL
dfculture_m2 <- mcmc_intervals_data(culturem1, pars = vars(starts_with("b_"))) %>% subset(parameter != "b_Intercept")

# recoding the predictor estimate. SPECIAL
dfculture_m2$parameter %<>% dplyr::recode(b_CultureGerman = "German vs. Turkish",
                                          b_CultureJapanese  = "Japanese vs. Turkish")

dfculture_m2$parameter %<>% reorder.factor(new.order = c("Japanese vs. Turkish", "German vs. Turkish"))

# plotting the model plot with credible intervals, and a dashed vertical line at the intercept (0). SPECIAL. Modele plot yapmak zorundna de??ilsin
p5 <- dfculture_m2 %>% ggplot(aes(m, parameter)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 2) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, colour = "black") +
  theme_minimal() +
  scale_x_continuous(limits = c(-1, 1)) +
  xlab("Estimate (log)") + ylab("Coefficient") +
  theme(text=element_text(size=15))
ggsave("p5_ills.png", plot = p5, width = 6, height = 6)






# read the second data frame, drop NA values
df2 <- read_excel("proficiency_df.xlsx" ) %>% drop_na()


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

