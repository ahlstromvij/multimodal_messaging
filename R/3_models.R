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

# global plotting values
global_font_size = 14
axis_font_size = 13
# color blind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in data
modelData <- read_csv('data/preprocessed_data.csv')
modelData

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

# cross-tabulation with condition
modelData %>% 
  group_by(condition, too_many_imm) %>% 
  tally() %>% 
  spread(condition, n)

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

# intercept only model
m0 <- glm(too_many_imm ~
            condition,
          modelData, family = "binomial")
summary(m0)

# robust SEs
m0_vcov <- vcovHC(m0, type="HC1")
(m0_coeftest <- coeftest(m0, vcov = m0_vcov))
(m0_coefci <- coefci(m0, vcov = m0_vcov))

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

# Calculate pseudo R2
# McFadden of 0.2-0.4 represents excellent fit: https://stats.stackexchange.com/questions/82105/
# mcfaddens-pseudo-r2-interpretation/99615#99615
pR2(m)

# Assess colinearity. The smallest possible value of VIF is one (absence of multicollinearity). 
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of 
# collinearity (James et al. 2014).
car::vif(m)

# Influential values (look for Cook's > 1)
# Code from http://www.sthda.com/english/articles/36-classification-methods-essentials/148-
#logistic-regression-assumptions-and-diagnostics-in-r/#influential-values
par(mfrow=c(1,1))
plot(m, which = 4, id.n = 3)
model.data <- augment(m) %>% 
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd) # Shows top 3 observations in terms of Cook's.
# Data points with an absolute standardized residuals above 3 represent possible outliers 
# and may deserve closer attention.
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = too_many_imm), alpha = .5) +
  theme_bw()
# Filter potential influential data points with abs(.std.res) > 3:
model.data %>% 
  filter(abs(.std.resid) > 3)
# No such observations

# Check for linearity with age, the only numeric predictor
mydata <- model.data[6]
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
probabilities <- predict(m, type = "response")
mydata <- na.omit(mydata[1]) %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()
# Looks fairly linear.

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

# 'Too Many Immigrants' - predicted probabilities
# Find typical Leave, Remain, and No_vote
temp_df <- modelData
temp_df %>% 
  ggplot() +
  aes(x = age) +
  geom_histogram(binwidth = 1, fill = 'salmon', color = 'white')

barplot(table(temp_df$age))
min(temp_df$age)
max(temp_df$age)

xs=c(18,30,45,64,86)
temp_df <- temp_df %>% mutate(age_cat=cut(age, breaks=xs, labels=c("18-30","31-45","46-64","65_over")))
table(temp_df$age_cat)
barplot(table(temp_df$age_cat))

# Mean age for each category
mean(na.omit(temp_df$age[temp_df$age_cat=="18-30"])) # mean = 24
mean(na.omit(temp_df$age[temp_df$age_cat=="31-45"])) # mean = 37
mean(na.omit(temp_df$age[temp_df$age_cat=="46-64"])) # mean = 53
mean(na.omit(temp_df$age[temp_df$age_cat=="65_over"])) # mean = 69

# Typical by referendum vote
temp_df_rem <- temp_df[temp_df$eu_vote=="Remain",]
x <- data.frame(table(temp_df_rem[c(2,3,4,5,13,14,15,17)]))
x[which.max(x$Freq), ]
temp_df_leave <- temp_df[temp_df$eu_vote=="Leave",]
x <- data.frame(table(temp_df_leave[c(2,3,4,5,13,14,15,17)]))
x[which.max(x$Freq), ]
temp_df_novote <- temp_df[temp_df$eu_vote=="No_vote",]
x <- data.frame(table(temp_df_novote[c(2,3,4,5,13,14,15,17)]))
x[which.max(x$Freq), ]

# From here: https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
fam <- family(m_segment)
fam
str(fam)

ilink <- fam$linkinv
ilink

newdata1 <- with(m_segment, data.frame(condition = c("control","text","vis","video"),
                                       eu_vote = "Remain",
                                       gender = "Male",
                                       education = "BA",
                                       party = "Lab",
                                       income = "30-40K",
                                       employment = "Full-Time",
                                       student = "No",
                                       age = 37))

ndata1 <- bind_cols(newdata1, setNames(as_tibble(predict(m_segment, newdata1, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))

# Create the interval and backtransform
conf_level <- 0.95
crit_value <- qnorm((1-conf_level)/2, lower.tail = FALSE)

ndata1 <- mutate(ndata1,
                 fit_resp  = ilink(fit_link),
                 upr = ilink(fit_link + (crit_value * se_link)),
                 lwr = ilink(fit_link - (crit_value * se_link)))

newdata2 <- with(m_segment, data.frame(condition = c("control","text","vis","video"),
                                       eu_vote = "Leave",
                                       gender = "Male",
                                       education = "BA",
                                       party = "Con",
                                       income = "40-50K",
                                       employment = "Full-Time",
                                       student = "No",
                                       age = 37))

ndata2 <- bind_cols(newdata2, setNames(as_tibble(predict(m_segment, newdata2, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))

# Create the interval and backtransform
ndata2 <- mutate(ndata2,
                 fit_resp  = ilink(fit_link),
                 upr = ilink(fit_link + (crit_value * se_link)),
                 lwr = ilink(fit_link - (crit_value * se_link)))

newdata3 <- with(m_segment, data.frame(condition = c("control","text","vis","video"),
                                       eu_vote = "No_vote",
                                       gender = "Female",
                                       education = "A-lev",
                                       party = "Lab",
                                       income = "Under10K",
                                       employment = "Unemployed",
                                       student = "Yes",
                                       age = 24))

ndata3 <- bind_cols(newdata3, setNames(as_tibble(predict(m_segment, newdata3, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))

# Create the interval and backtransform
ndata3 <- mutate(ndata3,
                 fit_resp  = ilink(fit_link),
                 upr = ilink(fit_link + (crit_value * se_link)),
                 lwr = ilink(fit_link - (crit_value * se_link)))


ndata1 <- ndata1[c(1,12,13,14)]
ndata1$Participant <- "Typical Remain"
ndata2 <- ndata2[c(1,12,13,14)]
ndata2$Participant <- "Typical Leave"
ndata3 <- ndata3[c(1,12,13,14)]
ndata3$Participant <- "Typical Non-voter"

combined_df <- rbind(ndata1,ndata2,ndata3)

too_many_imm_graph_segm <- combined_df %>% 
  mutate(condition = dplyr::recode(condition,
                                   'control' = 'Control',
                                   'vis' = 'Visual',
                                   'video' = 'Video',
                                   'text' = 'Text')) %>% 
  ggplot() +
  aes(x=condition, y=fit_resp, group=Participant, color=Participant) +
  geom_line(aes(color=Participant)) +
  geom_point(aes(color=Participant, shape=Participant), size = 3) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2,
                position=position_dodge(0.05)) +
  scale_color_manual(values = brewer.pal(n = 3, name = "Set1")) +
  xlab("Condition") +
  ylab("Probability of Affirmitive") +
  ggtitle("Too Many Immigrants") +
  labs(subtitle = "Estimated probability by respondent type (95% CI)") +
  theme(plot.title = element_text(face = "bold")) +
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  scale_color_brewer(palette="Dark2") +
  theme(legend.title=element_blank())
too_many_imm_graph_segm

# Calculate pseudo R2. 
# McFadden of 0.2-0.4 represents excellent fit: https://stats.stackexchange.com/questions/82105/
# mcfaddens-pseudo-r2-interpretation/99615#99615
pR2(m_segment)

# Assess colinearity. The smallest possible value of VIF is one (absence of multicollinearity). 
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of 
# collinearity (James et al. 2014).
car::vif(m_segment)

# Influential values (look for Cook's > 1)
# Code from http://www.sthda.com/english/articles/36-classification-methods-essentials/148-
#logistic-regression-assumptions-and-diagnostics-in-r/#influential-values
par(mfrow=c(1,1))
plot(m_segment, which = 4, id.n = 3)
model.data <- augment(m_segment) %>% 
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
# Data points with an absolute standardized residuals above 3 represent possible outliers 
# and may deserve closer attention.
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = too_many_imm), alpha = .5) +
  theme_bw()
# Filter potential influential data points with abs(.std.res) > 3:
model.data %>% 
  filter(abs(.std.resid) > 3)

# Check for linearity with age, the only numeric predictor
mydata <- model.data[6]
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
probabilities <- predict(m_segment, type = "response")
mydata <- na.omit(mydata[1]) %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

# Mean untransformed responses on the economic perceptions items that will go into IRT
econ_items_tbl <- modelData %>% 
  group_by(condition) %>% 
  summarise(take_jobs_mean = mean(take_jobs),
            take_jobs_sd = sd(take_jobs),
            take_jobs_n = length(take_jobs),
            drive_down_wages_mean = mean(drive_down_wages),
            drive_down_wages_sd = sd(drive_down_wages),
            drive_down_wages_n = length(drive_down_wages),
            create_jobs_mean = mean(create_jobs),
            create_jobs_sd = sd(create_jobs),
            create_jobs_n = length(create_jobs)) %>% 
  pivot_longer(cols = -condition,
               names_to = c('item','type'),
               names_pattern = '(take_jobs|drive_down_wages|create_jobs)_(mean|sd|n)',
               values_to = 'value')
econ_items_tbl

econ_items_tbl %>% 
  filter(type == "mean") %>% 
  pivot_wider(names_from = condition,
              values_from = value) %>% 
  mutate(text_diff = text - control,
         video_diff = video - control,
         vis_diff = vis - control) %>% 
  summarise(text = mean(text_diff),
            video = mean(video_diff),
            vis = mean(vis_diff),
            overall = mean(c(text, video, vis))) # 0.3396667 (6.8%)

econ_items_plot <- econ_items_tbl %>% 
  pivot_wider(names_from = type,
              values_from = value) %>% 
  mutate(upr = mean + qt(0.975, df=n-1)*sd/sqrt(n),
         lwr = mean - qt(0.975, df=n-1)*sd/sqrt(n)) %>% 
  mutate(item = dplyr::recode(item,
                              'create_jobs' = 'Create jobs',
                              'drive_down_wages' = 'Drive down wages',
                              'take_jobs' = 'Take jobs'),
         condition = dplyr::recode(condition,
                                   'vis' = 'Visual',
                                   'video' = 'Video',
                                   'text' = 'Text',
                                   'control' = 'Control')) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ggplot() +
  aes(x = condition, y = mean, fill = item, label = mean) +
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", shape=21) +
  geom_text(vjust = -1.5) +
  xlab("Condition") +
  ylab("Mean response (on a scale of 1-5)") +
  ggtitle("Mean Untransformed Values for Economic Perceptions Items") +
  theme(plot.title = element_text(face = "bold")) +
  labs(subtitle = "Higher values correspond to more positive attitudes (95% CIs)") +
  coord_flip() +
  theme(legend.position="none") +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  facet_wrap(~item)
econ_items_plot

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

m_econ0 <- lm(econ ~
                condition,
              modelData)
summary(m_econ0)

# robust SEs
m_econ0_vcov <- vcovHC(m_econ0, type="HC1")
(m_econ0_coeftest <- coeftest(m_econ0, vcov = m_econ0_vcov))
(m_econ0_coefci <- coefci(m_econ0, vcov = m_econ0_vcov))

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

# Diagnostics for Economic Perceptions Model
# Linearity assumption (want straight red line)
plot(m_econ, 1)

# Homogeneity of variance, or homoscedasticity (want straight red line)
plot(m_econ, 3)

# Normality of residuals (want the points to track the line)
plot(m_econ, 2)

# Outliers and high leverage points
plot(m_econ, 5)
# When cases are outside of the Cook’s distance (meaning they have high Cook’s 
# distance scores), the cases are influential to the regression results.
# From: https://data.library.virginia.edu/diagnostic-plots/

# multicolinearity
car::vif(m_econ)

# Let's also validate the outcome of that visual inspection by fitting a GVLMA 
# (Global Validation of Linear Models Assumptions) model:
gvmodel_econ <- gvlma(m_econ) 
summary(gvmodel_econ)

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

# Diagnostics for Economic Perceptions Model - with Interactions
# linearity assumption
plot(m_econ_segm, 1)

# homogeneity of variance
plot(m_econ_segm, 3)

# normality of residuals
plot(m_econ_segm, 2)

# outliers and high leverage points
plot(m_econ_segm, 5)

# multicolinearity
car::vif(m_econ_segm)

gvmodel_econ_segm <- gvlma(m_econ_segm) 
summary(gvmodel_econ_segm)

# Predicted Location of Typical Respondents on Economic Perceptions Scale
newdata1 <- with(m_econ_segm, data.frame(condition = c("control","text","vis","video"),
                                         eu_vote = "Remain",
                                         gender = "Male",
                                         education = "BA",
                                         party = "Lab",
                                         income = "30-40K",
                                         employment = "Full-Time",
                                         student = "No",
                                         age = 37))
fit1 <- predict(m_econ_segm, newdata=newdata1,interval = "confidence", level = 0.95)
newdata1$fit <- fit1[,1]
newdata1$lwr <- fit1[,2]
newdata1$upr <- fit1[,3]

newdata2 <- with(m_econ_segm, data.frame(condition = c("control","text","vis","video"),
                                         eu_vote = "Leave",
                                         gender = "Male",
                                         education = "BA",
                                         party = "Con",
                                         income = "40-50K",
                                         employment = "Full-Time",
                                         student = "No",
                                         age = 37))
fit2 <- predict(m_econ_segm, newdata=newdata2,interval = "confidence",level = 0.95)
newdata2$fit <- fit2[,1]
newdata2$lwr <- fit2[,2]
newdata2$upr <- fit2[,3]

newdata3 <- with(m_econ_segm, data.frame(condition = c("control","text","vis","video"),
                                         eu_vote = "No_vote",
                                         gender = "Female",
                                         education = "A-lev",
                                         party = "Lab",
                                         income = "Under10K",
                                         employment = "Unemployed",
                                         student = "Yes",
                                         age = 24))
fit3 <- predict(m_econ_segm, newdata=newdata3,interval = "confidence", level = 0.95)
newdata3$fit <- fit3[,1]
newdata3$lwr <- fit3[,2]
newdata3$upr <- fit3[,3]

newdata1 <- newdata1[c(1,10,11,12)]
newdata1$Participant <- "Typical Remain"
newdata2 <- newdata2[c(1,10,11,12)]
newdata2$Participant <- "Typical Leave"
newdata3 <- newdata3[c(1,10,11,12)]
newdata3$Participant <- "Typical Non-voter"

combined_df <- rbind(newdata1,newdata2,newdata3)

econ_graph_segm <- combined_df %>% 
  mutate(condition = dplyr::recode(condition,
                                   'control' = 'Control',
                                   'vis' = 'Visual',
                                   'video' = 'Video',
                                   'text' = 'Text')) %>% 
  ggplot() +
  aes(x=condition, y=fit, group=Participant,color=Participant) +
  geom_line(aes(color=Participant)) +
  geom_point(aes(color=Participant, shape=Participant), size = 3) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2,
                position=position_dodge(0.05)) +
  scale_color_manual(values = brewer.pal(n = 3, name = "Set1")) +
  xlab("Condition") +
  ylab("Economic Perceptions Scale") +
  ggtitle("Economic Perceptions") +
  labs(subtitle = "Estimated location on scale by respondent type (95% CI)") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  scale_color_brewer(palette="Dark2") +
  theme(legend.title=element_blank())
econ_graph_segm

# Mean untransformed responses on the policy preferences items that will go into IRT
policy_items_tbl <- modelData %>% 
  group_by(condition) %>% 
  summarise(brexit_to_cut_mean = mean(brexit_to_cut),
            brexit_to_cut_sd = sd(brexit_to_cut),
            brexit_to_cut_n = length(brexit_to_cut),
            increase_imm_mean = mean(increase_imm),
            increase_imm_sd = sd(increase_imm),
            increase_imm_n = length(increase_imm),
            sm_or_control_mean = mean(sm_or_control),
            sm_or_control_sd = sd(sm_or_control),
            sm_or_control_n = length(sm_or_control)) %>% 
  pivot_longer(cols = -condition,
               names_to = c('item','type'),
               names_pattern = '(brexit_to_cut|increase_imm|sm_or_control)_(mean|sd|n)',
               values_to = 'value')
policy_items_tbl

policy_items_tbl %>% 
  filter(type == "mean") %>% 
  pivot_wider(names_from = condition,
              values_from = value) %>% 
  mutate(text_diff = text - control,
         video_diff = video - control,
         vis_diff = vis - control) %>% 
  summarise(text = mean(text_diff),
            video = mean(video_diff),
            vis = mean(vis_diff),
            overall = mean(c(text, video, vis))) # 0.431 (4.3%)

policy_pref_plot <- policy_items_tbl %>% 
  pivot_wider(names_from = type,
              values_from = value) %>% 
  mutate(upr = mean + qt(0.975, df=n-1)*sd/sqrt(n),
         lwr = mean - qt(0.975, df=n-1)*sd/sqrt(n)) %>% 
  mutate(item = dplyr::recode(item,
                              'brexit_to_cut' = 'Brexit an opportunity to cut immigration',
                              'increase_imm' = 'Increase immigration',
                              'sm_or_control' = 'Prioritise single market over control'),
         condition = dplyr::recode(condition,
                                   'vis' = 'Visual',
                                   'video' = 'Video',
                                   'text' = 'Text',
                                   'control' = 'Control')) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  ggplot() +
  aes(x = condition, y = mean, fill = item, label = mean) +
  geom_pointrange(aes(ymin=lwr, ymax=upr),color="black", shape=21) +
  geom_text(vjust = -1.5) +
  xlab("Condition") +
  ylab("Mean response (on a scale of 0-10)") +
  ggtitle("Mean Untransformed Values for Policy Preferences Items") +
  theme(plot.title = element_text(face = "bold")) +
  labs(subtitle = "Higher values correspond to more positive attitudes (95% CIs)") +
  coord_flip() +
  theme(legend.position="none") +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  facet_wrap(~item)
policy_pref_plot

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

m_policy0 <- lm(policy ~
                  condition,
                modelData)
summary(m_policy0)

# robust SEs
m_policy0_vcov <- vcovHC(m_policy0, type="HC1")
(m_policy0_coeftest <- coeftest(m_policy0, vcov = m_policy0_vcov))
(m_policy0_coefci <- coefci(m_policy0, vcov = m_policy0_vcov))

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

# Diagnostics for Policy Preferences Model
# linearity assumption
plot(m_policy, 1)

# homogeneity of variance
plot(m_policy, 3)

# normality of residuals
plot(m_policy, 2)

# outliers and high leverage points
plot(m_policy, 5)

# multicolinearity
car::vif(m_policy)

gvmodel_policy <- gvlma(m_policy) 
summary(gvmodel_policy)
# global stat and skewness not satisfied

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

# Predicted Location of Typical Respondents on Policy Preferences Scale
newdata1 <- with(m_policy_segm, data.frame(condition = c("control","text","vis","video"),
                                           eu_vote = "Remain",
                                           gender = "Male",
                                           education = "BA",
                                           party = "Lab",
                                           income = "30-40K",
                                           employment = "Full-Time",
                                           student = "No",
                                           age = 37))
fit1 <- predict(m_policy_segm, newdata=newdata1,interval = "confidence", level = 0.95)
newdata1$fit <- fit1[,1]
newdata1$lwr <- fit1[,2]
newdata1$upr <- fit1[,3]

newdata2 <- with(m_policy_segm, data.frame(condition = c("control","text","vis","video"),
                                           eu_vote = "Leave",
                                           gender = "Male",
                                           education = "BA",
                                           party = "Con",
                                           income = "40-50K",
                                           employment = "Full-Time",
                                           student = "No",
                                           age = 37))
fit2 <- predict(m_policy_segm, newdata=newdata2,interval = "confidence", level = 0.95)
newdata2$fit <- fit2[,1]
newdata2$lwr <- fit2[,2]
newdata2$upr <- fit2[,3]

newdata3 <- with(m_policy_segm, data.frame(condition = c("control","text","vis","video"),
                                           eu_vote = "No_vote",
                                           gender = "Female",
                                           education = "A-lev",
                                           party = "Lab",
                                           income = "Under10K",
                                           employment = "Unemployed",
                                           student = "Yes",
                                           age = 24))
fit3 <- predict(m_policy_segm, newdata=newdata3,interval = "confidence", level = 0.95)
newdata3$fit <- fit3[,1]
newdata3$lwr <- fit3[,2]
newdata3$upr <- fit3[,3]

newdata1 <- newdata1[c(1,10,11,12)]
newdata1$Participant <- "Typical Remain"
newdata2 <- newdata2[c(1,10,11,12)]
newdata2$Participant <- "Typical Leave"
newdata3 <- newdata3[c(1,10,11,12)]
newdata3$Participant <- "Typical Non-voter"

combined_df <- rbind(newdata1,newdata2,newdata3)

policy_graph_segm <- combined_df %>% 
  mutate(condition = dplyr::recode(condition,
                                   'control' = 'Control',
                                   'vis' = 'Visual',
                                   'video' = 'Video',
                                   'text' = 'Text')) %>% 
  ggplot() +
  aes(x=condition, y=fit, group=Participant,color=Participant) +
  geom_line(aes(color=Participant)) +
  geom_point(aes(color=Participant, shape=Participant), size = 3) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2,
                position=position_dodge(0.05)) +
  scale_color_manual(values = brewer.pal(n = 3, name = "Set1")) +
  xlab("Condition") +
  ylab("Policy Preferences Scale") +
  ggtitle("Policy Preferences") +
  labs(subtitle = "Estimated location on scale by respondent type (95% CI)") +
  theme(plot.title = element_text(face = "bold")) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size)) +
  scale_color_brewer(palette="Dark2") +
  theme(legend.title=element_blank())
policy_graph_segm

# Diagnostics for Policy Preferences Model, with Interactions
# linearity assumption
plot(m_policy_segm, 1)

# homogeneity of variance
plot(m_policy_segm, 3)

# normality of residuals
plot(m_policy_segm, 2)

# outliers and high leverage points
plot(m_policy_segm, 5)

# multicolinearity
car::vif(m_policy_segm)

gvmodel_policy_segm <- gvlma(m_policy_segm) 
summary(gvmodel_policy_segm)
# skewness not satisfied

# Leave vs. Remain in control group
barplot(prop.table(table(modelData$too_many_imm[modelData$condition=='control'],
                         modelData$eu_vote[modelData$condition=='control']), margin=2),
        legend=TRUE)

prop.table(table(modelData$too_many_imm[modelData$condition=='control'],
                 modelData$eu_vote[modelData$condition=='control']), margin=2)

too_many_imm_control <- modelData %>%
  mutate(eu_vote = dplyr::recode(eu_vote, 'No_vote'='Did not vote')) %>% 
  mutate(too_many_imm = dplyr::recode(too_many_imm, `1`='Yes', `0`='No')) %>% 
  filter(condition == 'control') %>% 
  group_by(eu_vote, too_many_imm) %>% 
  tally() %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot() +
  aes(x = eu_vote, y = prop, fill = too_many_imm) +
  geom_bar(position='fill', stat='identity') +
  ylab('Proportion') +
  xlab('EU Referendum Vote') +
  ggtitle("Too Many Immigrants") +
  labs(subtitle = "Proportions by referendum vote in control condition") +
  guides(fill=guide_legend(title='Response')) +
  labs(subtitle = "Proportions by referendum vote in control condition") +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_manual(values=cbPalette) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size))
too_many_imm_control

econ_perceptions_control <- modelData %>%
  mutate(eu_vote = dplyr::recode(eu_vote, 'No_vote' = 'Did not vote')) %>% 
  filter(condition == 'control') %>% 
  ggplot() +
  aes(x=eu_vote, y=econ, fill=eu_vote) + 
  xlab("EU Referendum Vote") +
  ylab("Economic Perceptions Scale") +
  ggtitle("Economic Perceptions") +
  labs(subtitle = "Perceptions by referendum vote in control condition") +
  theme(plot.title = element_text(face = "bold")) +
  geom_boxplot() +
  theme(legend.position="none") +
  scale_fill_manual(values=cbPalette) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size))
econ_perceptions_control

policy_perceptions_control <- modelData %>% 
  mutate(eu_vote = dplyr::recode(eu_vote, 'No_vote' = 'Did not vote')) %>% 
  filter(condition == 'control') %>% 
  ggplot() +
  aes(x=eu_vote, y=policy, fill=eu_vote) + 
  xlab("EU Referendum Vote") +
  ylab("Policy Preferences Scale") +
  ggtitle("Policy Preferences") +
  labs(subtitle = "Preferences by referendum vote in control condition") +
  theme(plot.title = element_text(face = "bold")) +
  geom_boxplot() +
  theme(legend.position="none") +
  scale_fill_manual(values=cbPalette) +
  theme(text = element_text(size=global_font_size)) +
  theme(axis.text.x=element_text(size=axis_font_size)) +
  theme(axis.text.y=element_text(size=axis_font_size))
policy_perceptions_control

jpeg(file="plots/control_plot.jpeg", width=1350, height=500)
figure_control <- ggarrange(too_many_imm_control, econ_perceptions_control, policy_perceptions_control,
                            ncol = 3, nrow = 1,
                            common.legend = FALSE, legend = NULL)
figure_control
dev.off()

jpeg(file="plots/treatment_plot.jpeg", width=1350, height=500)
treatment_effects <- ggarrange(too_many_imm_graph, econ_graph, policy_graph,
                               ncol = 3, nrow = 1,
                               common.legend = FALSE, legend = NULL)
treatment_effects
dev.off()

jpeg(file="plots/treatment_multcomp.jpeg", width=1350, height=500)
treatment_effects_comp <- ggarrange(too_many_imm_graph_comp, econ_graph_comp, policy_graph_comp,
                                    ncol = 3, nrow = 1,
                                    common.legend = FALSE, legend = NULL)
treatment_effects_comp
dev.off()

jpeg(file="plots/pred_probs.jpeg", width=1350, height=500)
treatment_effects_interact <- ggarrange(too_many_imm_graph_segm, econ_graph_segm, policy_graph_segm,
                                        ncol = 3, nrow = 1,
                                        common.legend = TRUE, legend = "right")
treatment_effects_interact
dev.off()

jpeg(file="plots/scale_items_plots.jpeg", width=1400, height=500)
scale_items_plots <- ggarrange(econ_items_plot, policy_pref_plot,
                                        ncol = 2, nrow = 1,
                                        common.legend = FALSE, legend = NULL)
scale_items_plots
dev.off()

# Printing linear combinations table
all_glht <- cbind(too_many_imm_glht,econ_glht[,3:4],policy_glht[,3:4])
names(all_glht) <- c("Condition","Referendum vote","tmi_est","tmi_p","econ_est","econ_p","policy_est","policy_p")
write_csv(all_glht,"tables/all_glht.csv")

# Printing models
htmlreg(list(m,m_segment), 
        file = "tables/logistic_models.doc",
        single.row = TRUE, 
        caption = "Too Many Immigrants (logistic models with robut standard errors)",
        digits=4,
        custom.model.names=c("Main effects model",
                             "Interacted model"),
        override.se = list(m_coeftest[,2],
                           m_segment_coeftest[,2]),
        override.pvalues = list(m_coeftest[,4],
                                m_segment_coeftest[,4]),
        results = "asis")

htmlreg(list(m_econ, m_policy, m_econ_segm, m_policy_segm), 
        file = "tables/linear_models.doc",
        single.row = TRUE, 
        caption = "Economic Perceptions and Policy Preferences (OLS with robust standard errors)",
        digits=4,
        custom.model.names=c("Economic Perceptions (main effect)",
                             "Policy Preferences (main effect)",
                             "Economic Perceptions (interaction)",
                             "Policy Preferences (interaction)"),
        override.se = list(m_econ_coeftest[,2],
                           m_policy_coeftest[,2],
                           m_econ_segm_coeftest[,2],
                           m_policy_segm_coeftest[,2]),
        override.pvalues = list(m_econ_coeftest[,4],
                                m_policy_coeftest[,4],
                                m_econ_segm_coeftest[,4],
                                m_policy_segm_coeftest[,4]),
        results = "asis")

htmlreg(list(m0, m_policy0, m_econ0), 
        file = "tables/simple_models.doc",
        single.row = TRUE, 
        caption = "Models without covariates (robust standard errors)",
        digits=4,
        custom.model.names=c("Too Many Immigrants",
                             "Economic Perceptions",
                             "Policy Preferences"),
        override.se = list(m0_coeftest[,2],
                           m_policy0_coeftest[,2],
                           m_econ0_coeftest[,2]),
        override.pvalues = list(m0_coeftest[,4],
                                m_policy0_coeftest[,4],
                                m_econ0_coeftest[,4]),
        results = "asis")

