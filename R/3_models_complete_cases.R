set.seed(1)

library(tidyverse)
library(sandwich)
library(lmtest)
library(texreg)
library(stats)
library(multcomp)
library(ggthemes)
library(data.table)
library(pscl)
library(broom)
library(pracma)
library(RColorBrewer)
library(mirt)
library(car)
library(gvlma)
library(ggpubr)

# read in data
modelData <- read_csv('data/preprocessed_data_complete_cases.csv')
modelData

# global plotting values
global_font_size = 14
axis_font_size = 13
# color blind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# make sure variables are of right class
modelData$age <- as.numeric(modelData$age)
modelData$gender <- factor(modelData$gender)
modelData$education <- factor(modelData$education)
modelData$party <- factor(modelData$party)
modelData$income <- factor(modelData$income, levels=c("Under10K",
                                                      "10-20K",
                                                      "20-30K",
                                                      "30-40K",
                                                      "40-50K",
                                                      "50-60K",
                                                      "60-70K",
                                                      "70-80K",
                                                      "80-90K",
                                                      "90-100K",
                                                      "Over100K"),
                           ordered=TRUE)
modelData$too_many_imm <- factor(modelData$too_many_imm)
modelData$condition <- factor(modelData$condition)
modelData$eu_vote <- factor(modelData$eu_vote)
modelData$employment <- factor(modelData$employment)
modelData$student <- factor(modelData$student)
modelData$take_jobs <- as.numeric(modelData$take_jobs)
modelData$drive_down_wages <- as.numeric(modelData$drive_down_wages)
modelData$create_jobs <- as.numeric(modelData$create_jobs)
modelData$brexit_to_cut <- as.numeric(modelData$brexit_to_cut)
modelData$increase_imm <- as.numeric(modelData$increase_imm)
modelData$sm_or_control <- as.numeric(modelData$sm_or_control)
modelData

# rename EU vote variable
levels(modelData$eu_vote)[match("No vote",levels(modelData$eu_vote))] <- "No_vote"
table(modelData$eu_vote)

# 'Too Many Immigrants' Model
par(mfrow=c(1,1))

# fit 'Too Many Immigrants' model
m <- glm(too_many_imm ~
           condition +
           eu_vote +
           employment +
           student +
           age +
           gender +
           education +
           party +
           income,
         modelData, family = "binomial")
summary(m)

# robust SEs
m_vcov <- vcovHC(m, type="HC1")
(m_coeftest <- coeftest(m, vcov = m_vcov))
(m_coefci <- coefci(m, vcov = m_vcov))

# multiple comparisons for 'Too Many Immigrants' model
m_mult <- glht(m, mcp(condition="Tukey"), vcov = sandwich) # robust SEs
summary(m_mult,test = adjusted("holm"))
par(mar=c(4,8,4,2)) 
plot(confint(m_mult))
cf_toomany <- data.frame(confint(m_mult)$confint)
setDT(cf_toomany, keep.rownames = TRUE)[]
colnames(cf_toomany)[colnames(cf_toomany)=="rn"] <- "Comparison"
cf_toomany$Comparison <- factor(cf_toomany$Comparison, levels = c("vis - video",
                                                                  "vis - text",
                                                                  "video - text",
                                                                  "video - control",
                                                                  "vis - control",
                                                                  "text - control"))

# treatments compared to the control
cf_toomany1 <- cf_toomany[1:3,]
cf_toomany1$Comparison <- c("Text","Video","Visual")
too_many_imm_graph <- ggplot(cf_toomany1, aes(x=Comparison, y=Estimate)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", fill=c("green","green","green"), shape=21) +
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference\n(Logged Odds)") +
  ggtitle("Too Many Immigrants") +
  labs(subtitle = "Estimated Differences Compared to Control") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  coord_flip()
too_many_imm_graph

# treatments compared with one another
cf_toomany2 <- cf_toomany[4:6,]
cf_toomany2$Comparison <- c("Video - Text","Visual - Text","Visual - Video")
too_many_imm_graph_comp <- ggplot(cf_toomany2, aes(x=Comparison, y=Estimate)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", fill="white", shape=21) +
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Comparison") +
  ylab("Difference\n(Logged Odds)") +
  ggtitle("Too Many Immigrants") +
  labs(subtitle = "Estimated Differences Between Treatments") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  coord_flip()
too_many_imm_graph_comp

# 'Too Many Immigrants' model - with interactions
m_segment <- glm(too_many_imm ~
                   condition +
                   eu_vote +
                   condition:eu_vote +
                   employment +
                   student +
                   age +
                   gender +
                   education +
                   party +
                   income,
                 modelData, family = "binomial")
summary(m_segment)

# robust SEs
m_segment_vcov <- vcovHC(m_segment, type="HC1")
(m_segment_coeftest <- coeftest(m_segment, vcov = m_segment_vcov))
(m_segment_coefci <- coefci(m_segment, vcov = m_segment_vcov))

too_many_imm_glht <- data.frame("Condition" = c(rep("Visualisation",3), rep("Video",3), rep("Text",3)),
                                "Referendum vote" = rep(c("Voted leave","Voted remain","Did not vote"),3),
                                "Est." = c(m_segment$coefficients[[4]],
                                           summary(glht(m_segment, linfct = c("conditionvis + conditionvis:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                           summary(glht(m_segment, linfct = c("conditionvis + conditionvis:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]],
                                           m_segment$coefficients[[3]],
                                           summary(glht(m_segment, linfct = c("conditionvideo + conditionvideo:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                           summary(glht(m_segment, linfct = c("conditionvideo + conditionvideo:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]],
                                           m_segment$coefficients[[2]],
                                           summary(glht(m_segment, linfct = c("conditiontext + conditiontext:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                           summary(glht(m_segment, linfct = c("conditiontext + conditiontext:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]]),
                                "p-value" = c(coef(summary(m_segment))[4,4],
                                              summary(glht(m_segment, linfct = c("conditionvis + conditionvis:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                              summary(glht(m_segment, linfct = c("conditionvis + conditionvis:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]],
                                              coef(summary(m_segment))[3,4],
                                              summary(glht(m_segment, linfct = c("conditionvideo + conditionvideo:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                              summary(glht(m_segment, linfct = c("conditionvideo + conditionvideo:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]],
                                              coef(summary(m_segment))[2,4],
                                              summary(glht(m_segment, linfct = c("conditiontext + conditiontext:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                              summary(glht(m_segment, linfct = c("conditiontext + conditiontext:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]]))

too_many_imm_glht

# Economic Perceptions: IRT Model
econ_items <- data.frame(modelData$take_jobs,
                         modelData$drive_down_wages,
                         modelData$create_jobs)
psych::alpha(econ_items)
# Good internal consistency

IRT_econ_items <- mirt(data=econ_items,
                       model=1,
                       itemtype = "gpcm",
                       verbose=FALSE)
summary(IRT_econ_items)
# Items are loading well onto the factor (F1)
plot(IRT_econ_items, type="trace")
# Categories are nicely ordered

plot(IRT_econ_items, type="info")
# The scale provides most information slightly below the mean
coef(IRT_econ_items, IRTpars=T)

econ <- fscores(IRT_econ_items) # each person's expected score
modelData$econ <- econ

# Unidimensionality evaluated through scree plot
par(mfrow=c(1, 1))
psych::fa.parallel(econ_items, cor="poly")
# Suggests one factor (unidimensional)

# Q3 for local independence
Q3resid <- data.frame(residuals(IRT_econ_items, type="Q3"))

itemfit(IRT_econ_items, empirical.plot = 1)
itemfit(IRT_econ_items, empirical.plot = 2)
itemfit(IRT_econ_items, empirical.plot = 3)

plot(modelData$econ ~ modelData$condition, ylab="Economic Perceptions",xlab="Condition")

c(min = min(modelData$econ),
  mean = mean(modelData$econ),
  sd = sd(modelData$econ),
  max = max(modelData$econ))
# mean of 0 and SD of 1

# Economic Perceptions Model
m_econ <- lm(econ ~
               condition +
               eu_vote +
               employment +
               student +
               age +
               gender +
               education +
               party +
               income,
             modelData)
summary(m_econ)

# robust SEs
m_econ_vcov <- vcovHC(m_econ, type="HC1")
(m_econ_coeftest <- coeftest(m_econ, vcov = m_econ_vcov))
(m_econ_coefci <- coefci(m_econ, vcov = m_econ_vcov))

# Multiple Comparisons for Economic Perceptions Model
Anova(m_econ, type = "III")
m_econ_mult <- glht(m_econ, mcp(condition="Tukey"), vcov = sandwich)
summary(m_econ_mult,test = adjusted("holm"))
par(mar=c(4,8,4,2)) 
plot(confint(m_econ_mult))
cf_econ <- data.frame(confint(m_econ_mult)$confint)
setDT(cf_econ, keep.rownames = TRUE)[]
colnames(cf_econ)[colnames(cf_econ)=="rn"] <- "Comparison"
cf_econ$Comparison <- factor(cf_econ$Comparison, levels = c("vis - video",
                                                            "vis - text",
                                                            "video - text",
                                                            "video - control",
                                                            "vis - control",
                                                            "text - control"))

# Treatments compared to control
cf_econ1 <- cf_econ[1:3,]
cf_econ1$Comparison <- c("Text","Video","Visual")
econ_graph <- ggplot(cf_econ1, aes(x=Comparison, y=Estimate)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", fill="green", shape=21) +
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference\n(Economic Perceptions Scale)") +
  ggtitle("Economic Perceptions") +
  labs(subtitle = "Estimated Differences Compared to Control") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  coord_flip()
econ_graph

# Treatments compared with one another
cf_econ2 <- cf_econ[4:6,]
cf_econ2$Comparison <- c("Video - Text","Visual - Text","Visual - Video")
econ_graph_comp <- ggplot(cf_econ2, aes(x=Comparison, y=Estimate)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", fill=c("white","white","white"), shape=21) +
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Comparison") +
  ylab("Difference\n(Economic Perceptions Scale)") +
  ggtitle("Economic Perceptions") +
  labs(subtitle = "Estimated Differences Between Treatments") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  coord_flip()
econ_graph_comp

# Economic Perceptions Model - with interactions
m_econ_segm <- lm(econ ~
                    condition +
                    eu_vote +
                    condition:eu_vote +
                    employment +
                    student +
                    age +
                    gender +
                    education +
                    party +
                    income,
                  modelData)
summary(m_econ_segm)

# robust SEs
m_econ_segm_vcov <- vcovHC(m_econ_segm, type="HC1")
(m_econ_segm_coeftest <- coeftest(m_econ_segm, vcov = m_econ_segm_vcov))
(m_econ_segm_coefci <- coefci(m_econ_segm, vcov = m_econ_segm_vcov))

econ_glht <- data.frame("Condition" = c(rep("Visualisation",3), rep("Video",3), rep("Text",3)),
                        "Referendum vote" = rep(c("Voted leave","Voted remain","Did not vote"),3),
                        "Est." = c(m_econ_segm$coefficients[[4]],
                                   summary(glht(m_econ_segm, linfct = c("conditionvis + conditionvis:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                   summary(glht(m_econ_segm, linfct = c("conditionvis + conditionvis:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]],
                                   m_econ_segm$coefficients[[3]],
                                   summary(glht(m_econ_segm, linfct = c("conditionvideo + conditionvideo:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                   summary(glht(m_econ_segm, linfct = c("conditionvideo + conditionvideo:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]],
                                   m_econ_segm$coefficients[[2]],
                                   summary(glht(m_econ_segm, linfct = c("conditiontext + conditiontext:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                   summary(glht(m_econ_segm, linfct = c("conditiontext + conditiontext:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]]),
                        "p-value" = c(coef(summary(m_econ_segm))[4,4],
                                      summary(glht(m_econ_segm, linfct = c("conditionvis + conditionvis:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                      summary(glht(m_econ_segm, linfct = c("conditionvis + conditionvis:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]],
                                      coef(summary(m_econ_segm))[3,4],
                                      summary(glht(m_econ_segm, linfct = c("conditionvideo + conditionvideo:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                      summary(glht(m_econ_segm, linfct = c("conditionvideo + conditionvideo:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]],
                                      coef(summary(m_econ_segm))[2,4],
                                      summary(glht(m_econ_segm, linfct = c("conditiontext + conditiontext:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                      summary(glht(m_econ_segm, linfct = c("conditiontext + conditiontext:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]]))
econ_glht

# Policy Preferences: IRT Model
policy_items <- data.frame(modelData$brexit_to_cut,
                           modelData$increase_imm,
                           modelData$sm_or_control)
psych::alpha(policy_items)

IRT_policy_items <- mirt(data=policy_items,
                         model=1,
                         itemtype = "gpcm",
                         verbose = FALSE)
summary(IRT_policy_items)
# Two of the three items are not loading well. Let's see what's going on.
plot(IRT_policy_items, type="trace")

# Recode in light of item trace lines
barplot(table(modelData$sm_or_control))
modelData$sm_or_control[modelData$sm_or_control < 3 & !is.na(modelData$sm_or_control)] <- 1
modelData$sm_or_control[modelData$sm_or_control > 2 & modelData$sm_or_control < 8 & !is.na(modelData$sm_or_control)] <- 2
modelData$sm_or_control[modelData$sm_or_control > 7 & !is.na(modelData$sm_or_control)] <- 3
barplot(table(modelData$sm_or_control))

barplot(table(modelData$increase_imm))
modelData$increase_imm[modelData$increase_imm < 3 & !is.na(modelData$increase_imm)] <- 1
modelData$increase_imm[modelData$increase_imm > 2 & modelData$increase_imm < 8 & !is.na(modelData$increase_imm)] <- 2
modelData$increase_imm[modelData$increase_imm > 7 & !is.na(modelData$increase_imm)] <- 3
barplot(table(modelData$increase_imm))

barplot(table(modelData$brexit_to_cut))
modelData$brexit_to_cut[modelData$brexit_to_cut == 0 & !is.na(modelData$brexit_to_cut)] <- 1
modelData$brexit_to_cut[modelData$brexit_to_cut == 1 & !is.na(modelData$brexit_to_cut)] <- 1
modelData$brexit_to_cut[modelData$brexit_to_cut == 2 & !is.na(modelData$brexit_to_cut)] <- 2
modelData$brexit_to_cut[modelData$brexit_to_cut == 3 & !is.na(modelData$brexit_to_cut)] <- 3
modelData$brexit_to_cut[modelData$brexit_to_cut == 4 & !is.na(modelData$brexit_to_cut)] <- 4
modelData$brexit_to_cut[modelData$brexit_to_cut == 5 & !is.na(modelData$brexit_to_cut)] <- 4
modelData$brexit_to_cut[modelData$brexit_to_cut == 6 & !is.na(modelData$brexit_to_cut)] <- 4
modelData$brexit_to_cut[modelData$brexit_to_cut == 7 & !is.na(modelData$brexit_to_cut)] <- 5
modelData$brexit_to_cut[modelData$brexit_to_cut == 8 & !is.na(modelData$brexit_to_cut)] <- 5
modelData$brexit_to_cut[modelData$brexit_to_cut == 9 & !is.na(modelData$brexit_to_cut)] <- 6
modelData$brexit_to_cut[modelData$brexit_to_cut == 10 & !is.na(modelData$brexit_to_cut)] <- 7
barplot(table(modelData$brexit_to_cut))

# New IRT model
policy_items <- data.frame(modelData$brexit_to_cut,
                           modelData$increase_imm,
                           modelData$sm_or_control)
psych::alpha(policy_items)

IRT_policy_items <- mirt(data=policy_items,
                         model=1,
                         itemtype = "gpcm",
                         verbose = FALSE)
summary(IRT_policy_items)
# All items load well now
plot(IRT_policy_items, type="trace")
# That's better
plot(IRT_policy_items, type="info")
coef(IRT_policy_items, IRTpars=T)
modelData$policy <- fscores(IRT_policy_items) # each person's expected score

# Unidimensionality evaluated through scree plot
par(mfrow=c(1,1))
psych::fa.parallel(policy_items, cor="poly")

# Q3 for local independence
Q3resid <- data.frame(residuals(IRT_policy_items, type="Q3"))

itemfit(IRT_policy_items, empirical.plot = 1)
itemfit(IRT_policy_items, empirical.plot = 2)
itemfit(IRT_policy_items, empirical.plot = 3)

c(min = min(modelData$policy),
  mean = mean(modelData$policy),
  sd = sd(modelData$policy),
  max = max(modelData$policy))
# mean of 0 and SD of 1

par(mfrow=c(1,1))
plot(modelData$policy ~ modelData$condition,ylab="Condition",xlab="Policy Preferences")

m_policy <- lm(policy ~
                 condition +
                 eu_vote +
                 employment +
                 student +
                 age +
                 gender +
                 education +
                 party +
                 income,
               modelData)
summary(m_policy)

# robust SEs
m_policy_vcov <- vcovHC(m_policy, type="HC1")
(m_policy_coeftest <- coeftest(m_policy, vcov = m_policy_vcov))
(m_policy_coefci <- coefci(m_policy, vcov = m_policy_vcov))

# Multiple Comparisons for Policy Preferences Model
Anova(m_policy, type = "III")
policy_mult <- glht(m_policy, linfct = mcp(condition = "Tukey"), vcov = sandwich)
summary(policy_mult, test = adjusted("holm"))
par(mar=c(4,8,4,2)) 
plot(confint(policy_mult))

cf_policy <- data.frame(confint(policy_mult)$confint)
setDT(cf_policy, keep.rownames = TRUE)[]
colnames(cf_policy)[colnames(cf_policy)=="rn"] <- "Comparison"
cf_policy$Comparison <- factor(cf_policy$Comparison, levels = c("vis - video",
                                                                "vis - text",
                                                                "video - text",
                                                                "video - control",
                                                                "vis - control",
                                                                "text - control"))
# Treatments compared to control
cf_policy1 <- cf_policy[1:3,]
cf_policy1$Comparison <- c("Text","Video","Visual")
policy_graph <- ggplot(cf_policy1, aes(x=Comparison, y=Estimate)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", fill=c("green","green","green"), shape=21) +
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Condition") +
  ylab("Difference\n(Policy Scale)") +
  ggtitle("Policy Preferences") +
  labs(subtitle = "Estimated Differences Compared to Control") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  coord_flip()
policy_graph

# Treatments compared with one another
cf_policy2 <- cf_policy[4:6,]
cf_policy2$Comparison <- c("Video - Text","Visual - Text","Visual - Video")
policy_graph_comp <- ggplot(cf_policy2, aes(x=Comparison, y=Estimate)) + 
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", fill=c("white","white","white"), shape=21) +
  geom_hline(yintercept=0, linetype="11", color = "black") +
  xlab("Comparison") +
  ylab("Difference\n(Policy Scale)") +
  ggtitle("Policy Preferences") +
  labs(subtitle = "Estimated Differences Between Treatments") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  coord_flip()
policy_graph_comp

# Policy Preferences Model - with Interactions
m_policy_segm <- lm(policy ~
                      condition +
                      eu_vote +
                      condition:eu_vote +
                      employment +
                      student +
                      age +
                      gender +
                      education +
                      party +
                      income,
                    data = modelData)
summary(m_policy_segm)

# robust SEs
m_policy_segm_vcov <- vcovHC(m_policy_segm, type="HC1")
(m_policy_segm_coeftest <- coeftest(m_policy_segm, vcov = m_policy_segm_vcov))
(m_policy_segm_coefci <- coefci(m_policy_segm, vcov = m_policy_segm_vcov))

policy_glht <- data.frame("Condition" = c(rep("Visualisation",3), rep("Video",3), rep("Text",3)),
                          "Referendum vote" = rep(c("Voted leave","Voted remain","Did not vote"),3),
                          "Est." = c(m_policy_segm$coefficients[[4]],
                                     summary(glht(m_policy_segm, linfct = c("conditionvis + conditionvis:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                     summary(glht(m_policy_segm, linfct = c("conditionvis + conditionvis:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]],
                                     m_policy_segm$coefficients[[3]],
                                     summary(glht(m_policy_segm, linfct = c("conditionvideo + conditionvideo:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                     summary(glht(m_policy_segm, linfct = c("conditionvideo + conditionvideo:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]],
                                     m_policy_segm$coefficients[[2]],
                                     summary(glht(m_policy_segm, linfct = c("conditiontext + conditiontext:eu_voteRemain = 0"), vcov = sandwich))$test$coefficients[[1]],
                                     summary(glht(m_policy_segm, linfct = c("conditiontext + conditiontext:eu_voteNo_vote = 0"), vcov = sandwich))$test$coefficients[[1]]),
                          "p-value" = c(coef(summary(m_policy_segm))[4,4],
                                        summary(glht(m_policy_segm, linfct = c("conditionvis + conditionvis:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                        summary(glht(m_policy_segm, linfct = c("conditionvis + conditionvis:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]],
                                        coef(summary(m_policy_segm))[3,4],
                                        summary(glht(m_policy_segm, linfct = c("conditionvideo + conditionvideo:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                        summary(glht(m_policy_segm, linfct = c("conditionvideo + conditionvideo:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]],
                                        coef(summary(m_policy_segm))[2,4],
                                        summary(glht(m_policy_segm, linfct = c("conditiontext + conditiontext:eu_voteRemain = 0"), vcov = sandwich))$test$pvalues[[1]],
                                        summary(glht(m_policy_segm, linfct = c("conditiontext + conditiontext:eu_voteNo_vote = 0"), vcov = sandwich))$test$pvalues[[1]]))

policy_glht

# Printing models
htmlreg(list(m,m_segment,m_econ,m_policy,m_econ_segm,m_policy_segm), 
        file = "tables/complete_cases_models.doc",
        single.row = TRUE, 
        caption = "Models fitted with only complete cases (no imputation) and robust SEs",
        digits=4,
        custom.model.names=c("Too Many Immigrants (main effect)",
                             "Too Many Immigrants (interacted)",
                             "Economic Perceptions (main effect)",
                             "Policy Preferences (main effect)",
                             "Economic Perceptions (interacted)",
                             "Policy Preferences (interacted)"),
        override.se = list(m_coeftest[,2],
                           m_segment_coeftest[,2],
                           m_econ_coeftest[,2],
                           m_policy_coeftest[,2],
                           m_econ_segm_coeftest[,2],
                           m_policy_segm_coeftest[,2]),
        override.pvalues = list(m_coeftest[,4],
                                m_segment_coeftest[,4],
                                m_econ_coeftest[,4],
                                m_policy_coeftest[,4],
                                m_econ_segm_coeftest[,4],
                                m_policy_segm_coeftest[,4]),
        results = "asis")
