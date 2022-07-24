set.seed(1)
library(tidyverse)

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

# sample targets from Ipsos:
ipsos <- data.frame('Leave' = c(0.184,0.206,0.375),
                    'No_vote' = c(0.142,0.136,0.278), 
                    'Remain' = c(0.177,0.156,0.347))
row.names(ipsos) <- c('Female','Male','Total')
ipsos

# achieved sample proportions:
achieved <- prop.table(table(modelData$gender,modelData$eu_vote))
achieved <- rbind(achieved,c(sum(achieved[1:2]),sum(achieved[3:4]),sum(achieved[5:6])))
row.names(achieved) <- c('Female','Male','Total')
round(achieved,3)

# sample sizes by condition
table(modelData$condition)

# gender by condition
modelData %>% 
  group_by(condition, gender) %>% 
  tally() %>% 
  spread(condition, n)

modelData %>% 
  group_by(condition, gender) %>% 
  tally %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot() +
  aes(x = condition, y = prop, fill = gender) +
  geom_bar(stat = 'identity')

# referendum vote by condition
modelData %>% 
  group_by(condition, eu_vote) %>% 
  tally() %>% 
  spread(condition, n)

modelData %>% 
  group_by(condition, eu_vote) %>% 
  tally() %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot() +
  aes(x = condition, y = prop, fill = eu_vote) +
  geom_bar(stat = 'identity')

# party by condition
modelData %>% 
  group_by(condition, party) %>% 
  tally() %>% 
  spread(condition, n)

modelData %>% 
  group_by(condition, party) %>% 
  tally() %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot() +
  aes(x = condition, y = prop, fill = party) +
  geom_bar(stat = 'identity')

# descriptive statistics table for sample
modelData$edu_sorted <- factor(modelData$education, 
                               levels = c("GCSEorLess","A-lev","Post2",
                                          "BA","PG"))
modelData$empl_sorted <- factor(modelData$employment, 
                                levels = c("Full-Time", "Part-Time",
                                           "Not in paid work","Unemployed","Other"))
modelData$euvote_sorted <- factor(modelData$eu_vote, 
                                  levels = c("Remain", "Leave","No_vote"))
modelData$student_sorted <- factor(modelData$student, 
                                   levels = c("Yes", "No"))
modelData$party_sorted <- factor(modelData$party, 
                                 levels = c("Con", "Lab", "Lib", "SNP", "UKIP", "Plaid",
                                            "Grn","None","Rather_not_say"))

age <- round(tapply(modelData$age, modelData$condition, mean),digits=1)
gender <- round(prop.table(table(modelData$gender,modelData$condition),2)*100,digits=1)
eu_vote <- round(prop.table(table(modelData$euvote_sorted,modelData$condition),2)*100,digits=1)
party <- round(prop.table(table(modelData$party_sorted,modelData$condition),2)*100,digits=1)
education <- round(prop.table(table(modelData$edu_sorted,modelData$condition),2)*100,digits=1)
income <- round(prop.table(table(modelData$income,modelData$condition),2)*100,digits=1)
empl <- round(prop.table(table(modelData$empl_sorted,modelData$condition),2)*100,digits=1)
student <- round(prop.table(table(modelData$student_sorted,modelData$condition),2)*100,digits=1)

summary_df <- data.frame(rbind(gender,income,education,empl,student,eu_vote,party))
summary_df$sd <- NA
for(i in 1:length(summary_df$control)){
  summary_df$sd[i] <- round(sd(summary_df[i,1:4]),digits=2)
}
summary_df[1:4] <- lapply(summary_df[1:4], function(x) paste(x,"%",sep=""))
cond <- table(modelData$condition)
summary_df <- rbind(age,summary_df,cond)

summary_df$sd[1] <- round(sd(summary_df[1,1:4]),digits=2)
summary_df$sd[39] <- round(sd(summary_df[39,1:4]),digits=2)

row.names(summary_df) <- c("Age (mean)","Female","Male",
                           "Income: Under 10k",
                           "Income: 10-20k",
                           "Income: 20-30k",
                           "Income: 30-40k",
                           "Income: 40-50k",
                           "Income: 50-60k",
                           "Income: 60-70k",
                           "Income: 70-80k",
                           "Income: 80-90k",
                           "Income: 90-100k",
                           "Income: Over 100k",
                           "Educ: GCSE or less",
                           "Educ: A-level",
                           "Educ: Post-sec",
                           "Educ: BA",
                           "Educ: PG",
                           "Empl: Full-time",
                           "Empl: Part-time",
                           "Empl: Not in paid",
                           "Empl: Unemployed",
                           "Empl: Other",
                           "Student: Yes",
                           "Student: No",
                           "EU vote: Remain",
                           "EU vote: Leave",
                           "EU vote: Didn't vote",
                           "Party: Cons",
                           "Party: Labour",
                           "Party: LibDem",
                           "Party: SNP",
                           "Party: UKIP",
                           "Party: Plaid",
                           "Party: Green",
                           "Party: None",
                           "Party: Rather not say",
                           "N")
write_csv(summary_df, "tables_and_plots/sample_summary_table.csv")
summary_df
