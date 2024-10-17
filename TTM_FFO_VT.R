#### TTM FFO VT ###
# Motivations and Barriers for the Participation of Family Forest Owners in 
# Habitat Conservation Assistance Programs in Vermont: An Application of the Transtheoretical Model

# TTM Stages:
# - Pre-contemplation (unfamiliar)
# - Pre-contemplation (familiar)
# - Contemplation 
# - Preparation
# - Action
# - Relapse/Opt out

# Three models:
# - Unfamiliar to familiar
# - TTM stages (Pre-contemplation > Contemplation > Preparation > Action)
# - Relapse/Opt out

#### Rsources ####
# Ordinal logistic regression
# - https://www.bookdown.org/rwnahhas/RMPH/blr-ordinal.html
# - https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

#### General Set up ####
rm(list = ls())

# library(dplyr)
library(tidyverse)
#library(MASS) # for ordered logistic regression, but do not load use MASS::polr to avoid conflicts with dplyr
# library(generalhoslem) # Do not load, use generalhoslem::function to avoid conflicts with dplyr

#### Load Data ####
dat <- as_tibble(readRDS("DATA/VT.FFO_06.16.2021.rds")) %>%
  mutate(LN_AC_WOOD = log(AC_WOOD),
         PC_TTM3 = if_else(PC_FAM %in% c("heard term not familiar", "never heard of term"), "Unfamiliar", PC_TTM3),
         CS_TTM3 = if_else(CS_TTM2 %in% c("Supposed to skip"), "Unfamiliar", CS_TTM3),
         across(c(EX_TTM3, PC_TTM3, CS_TTM3), as.character),
         across(c(EX_TTM3, PC_TTM3, CS_TTM3), 
                ~ case_match(.x,
                                 "Unfamiliar" ~ "Pre-contemplation (unfamiliar)", 
                                 "Unaware" ~ "Pre-contemplation (familiar)", 
                                 "Resist" ~ "Pre-contemplation (familiar)",
                                 .default = .x)),
         across(c(EX_TTM3, PC_TTM3, CS_TTM3), 
                ~ factor(.x, levels = c("Pre-contemplation (unfamiliar)", "Pre-contemplation (familiar)", 
                                        "Contemplation", "Preparation", "Action"))),
         across(c(WVO_HSMF, WVO_TNOH, WVO_FAWA, WVO_WSTF_HF, WVO_PWWT,
                  WVO_WSSF_SBS, WVO_IVAL, WVO_ASHR, WVO_WALM, WVO_ICAA, WVO_IFAS, WVO_IVTS), 
                ~ case_match(.x,
                             "Strongly disagree" ~ 1,
                             "Slightly disagree" ~ 2,
                             "Neither agree nor disagree" ~ 3,
                             "Slightly agree" ~ 4,
                             "Strongly agree" ~ 5)),
         across(c(WVO_HICA, WVO_HDNR), 
                ~ case_match(.x,
                             "Strongly disagree" ~ 5,
                             "Slightly disagree" ~ 4,
                             "Neither agree nor disagree" ~ 3,
                             "Slightly agree" ~ 2,
                             "Strongly agree" ~ 1)),
         WVO_APP_USE = (WVO_HSMF + WVO_TNOH + WVO_FAWA) / 3,
         WVO_HUNT = (WVO_WSTF_HF + WVO_HICA + WVO_HDNR + WVO_PWWT) / 4,
         WVO_DOMINIONISTIC = (WVO_APP_USE + WVO_HUNT) / 2,
         WVO_SOC_AFF = (WVO_WSSF_SBS + WVO_IVAL + WVO_ASHR + WVO_WALM) / 4,
         WVO_CARING = (WVO_ICAA + WVO_IFAS + WVO_IVTS) / 3,
         WVO_MUTUALISTIC = (WVO_CARING + WVO_SOC_AFF) / 2,
         WVO_OREINTATION = 
           factor(case_when(WVO_MUTUALISTIC <= 3 & WVO_DOMINIONISTIC > 3 ~ "Traditionalist",
                            WVO_MUTUALISTIC <= 3 & WVO_DOMINIONISTIC <= 3 ~ "Distanced",
                            WVO_MUTUALISTIC > 3 & WVO_DOMINIONISTIC > 3 ~ "Pluralist",
                            WVO_MUTUALISTIC > 3 & WVO_DOMINIONISTIC <= 3 ~ "Mutualist")))

#### Basic Summaries ####
ttm.summary <- bind_rows(dat %>% count(EX_TTM3) %>% rename(Stage = EX_TTM3) %>%
                           bind_rows(tibble(Stage = "Pre-contemplation (unfamiliar)", n = 0)) %>%
                           mutate(Activity = "EX"),
                         dat %>% count(PC_TTM3) %>% rename(Stage = PC_TTM3) %>%
                           mutate(Activity = "PC"),
                         dat %>% count(CS_TTM3) %>% rename(Stage = CS_TTM3)%>%
                           mutate(Activity = "CS")) %>%
  select(Activity, Stage, n) %>%
  filter(!is.na(Stage)) %>%
  mutate(#Stage = if_else(is.na(Stage), "Missing data", Stage),
         Stage = factor(Stage, 
                        levels = c("Pre-contemplation (unfamiliar)", "Pre-contemplation (familiar)", 
                                   "Contemplation", "Preparation", "Action"),
                        labels = c("Pre-contemplation\n(unfamiliar)", "Pre-contemplation\n(familiar)", 
                                   "Contemplation", "Preparation", "Action"))) %>%
  group_by(Activity) %>%
  mutate(p = n / sum(n),
         se = sqrt((p * (1- p)) / (sum(n) - 1))) %>%
  ungroup() %>%
  mutate(lower =  (p - (1.96 * se)) * 100,
         lower = if_else(lower < 0, 0, lower),
         upper = (p + (1.96 * se)) * 100) %>%
  arrange(Activity, Stage)
ttm.summary

# Figure 1
ggplot(ttm.summary %>% filter(Activity == "EX"), aes(x = Stage, y = p * 100)) +
  geom_bar(stat = "identity", fill = "#155C7B") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.5) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
  labs(y = "Percent of Respondents", x = element_blank()) +
  coord_flip() +
  theme_linedraw() +
  theme(plot.margin = unit(c(5, 7.5, 5, 5), "pt"),
        panel.grid.major.y = element_blank()) +
  ggtitle("A. Expert visit")
ggsave("FIGURES/TTM_EX.png", width = 5, height = 2.75)
ggplot(ttm.summary %>% filter(Activity == "PC"), aes(x = Stage, y = p * 100)) +
  geom_bar(stat = "identity", fill = "#155C7B") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.5) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
  labs(y = "Percent of Respondents", x = element_blank()) +
  coord_flip() +
  theme_linedraw() +
  theme(plot.margin = unit(c(5, 7.5, 5, 5), "pt"),
        panel.grid.major.y = element_blank()) +
  ggtitle("B. Patch cut")
ggsave("FIGURES/TTM_PC.png", width = 5, height = 2.75)
ggplot(ttm.summary %>% filter(Activity == "CS"), aes(x = Stage, y = p * 100)) +
  geom_bar(stat = "identity", fill = "#155C7B") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.5) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,100)) +
  labs(y = "Percent of Respondents", x = element_blank()) +
  coord_flip() +
  theme_linedraw() +
  theme(plot.margin = unit(c(5, 7.5, 5, 5), "pt"),
        panel.grid.major.y = element_blank()) +
  ggtitle("C. Cost-share")
ggsave("FIGURES/TTM_CS.png", width = 5, height = 2.75)

#### Expert visits ####
dat.ex <- dat %>%
  mutate(EX_STAGE = factor(case_match(EX_TTM3,
                                      "Pre-contemplation (familiar)" ~ "1", # Precontemplation
                                      c("Contemplation", "Preparation") ~ "2",
                                      "Action" ~ "3")),
         EX_STAGE_1 = factor(case_match(EX_STAGE,
                                        c("1") ~ "0",
                                        c("2", "3") ~ "1")),
         EX_STAGE_2 = factor(case_match(EX_STAGE,
                                        c("1", "2") ~ "0",
                                        c("3") ~ "1"))) %>%
  mutate(across(c(EX_COST:EX_NEED),
                ~ factor(case_match(.x,
                                    c("Neutral","Disagree") ~ "0",
                                    c("Agree") ~ "1"))),
         EX_INFO_KNOW = factor(case_when(EX_INFO == "1" | EX_KNOW == "1" ~ "1",
                                         EX_INFO == "0" | EX_KNOW == "0" ~ "0")),
         EX_COST_EFF = factor(case_when(EX_COST == "1" | EX_EFF == "1" ~ "1",
                                         EX_COST == "0" | EX_EFF == "0" ~ "0"))) %>%
  select(EX_STAGE, EX_STAGE_1, EX_STAGE_2, 
         EX_COST_EFF, EX_LEARN, EX_PERS, EX_REAS, EX_INFO_KNOW, EX_NEED, 
         LN_AC_WOOD, WVO_MUTUALISTIC, WVO_DOMINIONISTIC) %>%
  na.omit
summary(dat.ex)

# Estimate model
ex.polr <- MASS::polr(EX_STAGE ~ EX_COST_EFF + EX_LEARN + EX_PERS + EX_REAS + EX_INFO_KNOW + EX_NEED + 
                        LN_AC_WOOD + WVO_MUTUALISTIC + WVO_DOMINIONISTIC, 
                      data = dat.ex, Hess = T)
summary(ex.polr)
ex.polr.coeff <- tibble(VARIABLE = row.names(coef(summary(ex.polr))),
                        as_tibble(coef(summary(ex.polr)))) %>%
  rename("SE" = "Std. Error", "t_value" = "t value") %>%
  mutate(p_value = pnorm(abs(t_value), lower.tail = FALSE) * 2,
         p_value_2 = round(p_value, 2)) %>%
  left_join(tibble(VARIABLE = names(coef(ex.polr)),
                   OR = exp(coef(ex.polr)),
                   OR_LOWER = confint(ex.polr)[,1],
                   OR_UPPER = confint(ex.polr)[,2]), by = join_by(VARIABLE))
ex.polr.coeff
write_csv(ex.polr.coeff, "DATA/TTM_EX_MODEL.csv")

# Test for multicollinearity
car::vif(ex.polr)

# Goodness of fit
generalhoslem::lipsitz.test(ex.polr)
generalhoslem::logitgof(dat.ex$EX_STAGE, fitted(ex.polr), ord = T)
generalhoslem::pulkrob.chisq(ex.polr, c("EX_COST_EFF", "EX_LEARN", "EX_PERS", "EX_REAS", "EX_INFO_KNOW", "EX_NEED"))
generalhoslem::pulkrob.deviance(ex.polr, c("EX_COST_EFF", "EX_LEARN", "EX_PERS", "EX_REAS", "EX_INFO_KNOW", "EX_NEED"))

# Test proportional odds assumption
ex.glm.1 <- glm(EX_STAGE_1 ~ EX_COST_EFF + EX_LEARN + EX_PERS + EX_REAS + EX_INFO_KNOW + EX_NEED + 
                  LN_AC_WOOD + WVO_MUTUALISTIC + WVO_DOMINIONISTIC,
                family = binomial, data = dat.ex)
ex.glm.2 <- glm(EX_STAGE_2 ~ EX_COST_EFF + EX_LEARN + EX_PERS + EX_REAS + EX_INFO_KNOW + EX_NEED + 
                  LN_AC_WOOD + WVO_MUTUALISTIC + WVO_DOMINIONISTIC,
                family = binomial, data = dat.ex)
exp(cbind("ordinal"  = ex.polr$coefficients,
          "binary 1" = ex.glm.1$coefficients[-1],
          "binary 2" = ex.glm.2$coefficients[-1]))

#### Patch cuts ####
dat.pc <- dat %>%
  mutate(PC_STAGE = factor(case_match(PC_TTM3,
                                      "Pre-contemplation (familiar)" ~ "1", # Precontemplation
                                      c("Contemplation", "Preparation") ~ "2",
                                      "Action" ~ "3")),
         PC_STAGE_1 = factor(case_match(PC_STAGE,
                                        c("1") ~ "0",
                                        c("2", "3") ~ "1")),
         PC_STAGE_2 = factor(case_match(PC_STAGE,
                                        c("1", "2") ~ "0",
                                        c("3") ~ "1"))) %>%
  mutate(across(c(PC_YOUNG:PC_UNW),
                ~ factor(case_match(.x,
                                    c("Neutral","Disagree") ~ "0",
                                    c("Agree") ~ "1"))),
         PC_INC_EFF = factor(case_when(PC_INC == "1" | PC_EFF == "1" ~ "1",
                                       PC_INC == "0" | PC_EFF == "0" ~ "0"))) %>%
  select(PC_STAGE, PC_STAGE_1, PC_STAGE_2, 
         PC_YOUNG, PC_HAB, PC_HUNT, PC_HEALTH, PC_UGLY, PC_REC, PC_HARM, PC_INC_EFF, PC_UNW, 
         LN_AC_WOOD, WVO_MUTUALISTIC, WVO_DOMINIONISTIC) %>%
  na.omit
summary(dat.pc)

# Estimate model
pc.polr <- MASS::polr(PC_STAGE ~ PC_YOUNG + PC_HAB + PC_HUNT+ PC_HEALTH + PC_UGLY + PC_REC + PC_HARM + PC_INC_EFF + 
                        PC_UNW + LN_AC_WOOD + WVO_MUTUALISTIC + WVO_DOMINIONISTIC, 
                      data = dat.pc, Hess = T)
summary(pc.polr)
pc.polr.coeff <- tibble(VARIABLE = row.names(coef(summary(pc.polr))),
                        as_tibble(coef(summary(pc.polr)))) %>%
  dplyr::rename("SE" = "Std. Error", "t_value" = "t value") %>%
  mutate(p_value = pnorm(abs(t_value), lower.tail = FALSE) * 2,
         p_value_2 = round(p_value, 2)) %>%
  left_join(tibble(VARIABLE = names(coef(pc.polr)),
                   OR = exp(coef(pc.polr)),
                   OR_LOWER = confint(pc.polr)[,1],
                   OR_UPPER = confint(pc.polr)[,2]), by = join_by(VARIABLE))
pc.polr.coeff
write_csv(pc.polr.coeff, "DATA/TTM_PC_MODEL.csv")

# Test of multicollinearity
car::vif(pc.polr)

# Goodness of fit
generalhoslem::lipsitz.test(pc.polr)
generalhoslem::logitgof(dat.pc$PC_STAGE, fitted(pc.polr), ord = T)
generalhoslem::pulkrob.chisq(pc.polr, c("PC_YOUNG", "PC_HAB", "PC_HUNT", "PC_HEALTH", "PC_UGLY", "PC_REC", "PC_HARM", 
                                        "PC_INC_EFF", "PC_UNW"))
generalhoslem::pulkrob.deviance(pc.polr, c("PC_YOUNG", "PC_HAB", "PC_HUNT", "PC_HEALTH", "PC_UGLY", "PC_REC", "PC_HARM", 
                                           "PC_INC_EFF", "PC_UNW"))

# Test proportional odds assumption
pc.glm.1 <- glm(PC_STAGE_1 ~ PC_YOUNG + PC_HAB + PC_HUNT+ PC_HEALTH + PC_UGLY + PC_REC + PC_HARM + PC_INC_EFF + 
                  PC_UNW + LN_AC_WOOD + WVO_MUTUALISTIC + WVO_DOMINIONISTIC, 
                family = binomial, data = dat.pc)
pc.glm.2 <- glm(PC_STAGE_2 ~ PC_YOUNG + PC_HAB + PC_HUNT+ PC_HEALTH + PC_UGLY + PC_REC + PC_HARM + PC_INC_EFF + 
                  PC_UNW + LN_AC_WOOD + WVO_MUTUALISTIC + WVO_DOMINIONISTIC, 
                family = binomial, data = dat.pc)
exp(cbind("ordinal"  = pc.polr$coefficients,
          "binary 1" = pc.glm.1$coefficients[-1],
          "binary 2" = pc.glm.2$coefficients[-1]))

#### Cost-share ####
dat.cs <- dat %>%
  mutate(CS_STAGE = factor(case_match(CS_TTM3,
                                      "Pre-contemplation (familiar)" ~ "1", # Precontemplation
                                      c("Contemplation", "Preparation") ~ "2",
                                      "Action" ~ "3")),
         CS_STAGE_1 = factor(case_match(CS_STAGE,
                                        c("1") ~ "0",
                                        c("2", "3") ~ "1")),
         CS_STAGE_2 = factor(case_match(CS_STAGE,
                                        c("1", "2") ~ "0",
                                        c("3") ~ "1"))) %>%
  mutate(across(c(CS_INT:CS_DNK),
                ~ factor(case_match(.x,
                                    c("Neutral","Disagree") ~ "0",
                                    c("Agree") ~ "1"))),
         CS_NGO_GOV = factor(case_when(CS_NGO == "1" | CS_GOV == "1" ~ "1",
                                       CS_NGO == "0" | CS_GOV == "0" ~ "0")),
         CS_FUND_EFF = factor(case_when(CS_FUND == "1" | CS_EFF == "1" ~ "1",
                                        CS_FUND == "0" | CS_EFF == "0" ~ "0"))) %>%
  select(CS_STAGE, CS_STAGE_1, CS_STAGE_2,
         CS_INT, CS_NGO_GOV, CS_FUND_EFF, CS_FIN, CS_IMP, CS_REA, CS_INFO, CS_REC, CS_DNK,
         LN_AC_WOOD, WVO_MUTUALISTIC, WVO_DOMINIONISTIC) %>%
  na.omit
summary(dat.cs)

# Estimate model
cs.polr <- MASS::polr(CS_STAGE ~ CS_INT + CS_NGO_GOV + CS_FUND_EFF + CS_FIN + CS_IMP + CS_REA + CS_INFO + CS_REC + CS_DNK + 
                        LN_AC_WOOD + WVO_MUTUALISTIC + WVO_DOMINIONISTIC,
                      data = dat.cs, Hess = T)
summary(cs.polr)
cs.polr.coeff <- tibble(VARIABLE = row.names(coef(summary(cs.polr))),
                        as_tibble(coef(summary(cs.polr)))) %>%
  rename("SE" = "Std. Error", "t_value" = "t value") %>%
  mutate(p_value = pnorm(abs(t_value), lower.tail = FALSE) * 2,
         p_value_2 = round(p_value, 2)) %>%
  left_join(tibble(VARIABLE = names(coef(cs.polr)),
                   OR = exp(coef(cs.polr)),
                   OR_LOWER = confint(cs.polr)[,1],
                   OR_UPPER = confint(cs.polr)[,2]), by = join_by(VARIABLE))
cs.polr.coeff
write_csv(cs.polr.coeff, "DATA/TTM_CS_MODEL.csv")

# Test for multicollinearity
car::vif(cs.polr)

# Goodness of fit
generalhoslem::lipsitz.test(cs.polr)
generalhoslem::logitgof(dat.cs$CS_STAGE, fitted(cs.polr), ord = T)
generalhoslem::pulkrob.chisq(cs.polr, c("CS_INT", "CS_NGO_GOV", "CS_FUND_EFF", "CS_FIN", "CS_IMP", "CS_REA", 
                                        "CS_INFO", "CS_REC", "CS_DNK"))
generalhoslem::pulkrob.deviance(cs.polr, c("CS_INT", "CS_NGO_GOV", "CS_FUND_EFF", "CS_FIN", "CS_IMP", "CS_REA", 
                                           "CS_INFO", "CS_REC", "CS_DNK"))

# Test proportional odds assumption
cs.glm.1 <- glm(CS_STAGE_1 ~ CS_INT + CS_NGO_GOV + CS_FUND_EFF + CS_FIN + CS_IMP + CS_REA + CS_INFO + CS_DNK + LN_AC_WOOD,
                family = binomial, data = dat.cs)
cs.glm.2 <- glm(CS_STAGE_2 ~ CS_INT + CS_NGO_GOV + CS_FUND_EFF + CS_FIN + CS_IMP + CS_REA + CS_INFO + CS_DNK + LN_AC_WOOD,
                family = binomial, data = dat.cs)
exp(cbind("ordinal"  = cs.polr$coefficients,
          "binary 1" = cs.glm.1$coefficients[-1],
          "binary 2" = cs.glm.2$coefficients[-1]))
