set.seed(1)
library(tidyverse)

allData <- read_csv('data/raw_data.csv') %>% 
  select(-c(1:13, 19, 24:27, 31))
allData

# sample size
dim(allData)[1]

# recode variables
allData <- allData %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  mutate(too_many_imm = recode(too_many_imm, 
                               "Yes, too many" = 1,
                               "No, not too many" = 0,
                               .default=NA_real_),
         brexit_to_cut = 10 - (as.numeric(brexit_to_cut)), # reversed
         increase_imm = as.numeric(increase_imm),
         sm_or_control = 10 - (as.numeric(sm_or_control)), # reversed
         take_jobs = recode(take_jobs,
                            "Strongly agree" = 1,
                            "Agree" = 2,
                            "Neither agree, nor disagree" = 3,
                            "Disagree" = 4,
                            "Strongly disagree" = 5,
                            .default=NA_real_),
         drive_down_wages = recode(drive_down_wages,
                                   "Strongly agree" = 1,
                                   "Agree" = 2,
                                   "Neither agree, nor disagree" = 3,
                                   "Disagree" = 4,
                                   "Strongly disagree" = 5,
                                   .default=NA_real_),
         create_jobs = recode(create_jobs, # reversed
                              "Strongly agree" = 5,
                              "Agree" = 4,
                              "Neither agree, nor disagree" = 3,
                              "Disagree" = 2,
                              "Strongly disagree" = 1,
                              .default=NA_real_),
         age = as.numeric(age),
         gender = as.factor(gender),
         student = as.factor(student),
         employment = as.factor(employment),
         employment = recode(employment,
                             "Due to start work" = "Other"),
         education = recode(education,
                            "A-level or equivalent" = "A-lev",
                            "Bachelors or equivalent degree level qualification (BA, BSc etc.)" = "BA",
                            "GCSE or equivalent" = "GCSEorLess",
                            "No educational qualification" = "GCSEorLess",
                            "Post-secondary below-degree level qualification" = "Post2",
                            "Postgraduate degree or equivalent qualification (MA, MSc, PGCE, PhD etc.)" = "PG",
                            "Primary School" = "GCSEorLess",
                            "Would rather not say" = NA_character_),
         education = as.factor(education),
         party = recode(party,
                        "Conservatives" = "Con",
                        "Labour" = "Lab",
                        "Green Party" = "Grn",
                        "Liberal Democrats" = "Lib",
                        "None of the above" = "None",
                        "Plaid Cymru" = "Plaid",
                        "Scottish National Party" = "SNP",
                        "Would rather not say" = "Rather_not_say"),
         party = as.factor(party),
         eu_vote = as.factor(eu_vote),
         income = recode(income,
                         "Under £10,000" = "Under10K",
                         "£10,000-19,999" = "10-20K",
                         "£10,000-19,999" = "10-20K",
                         "£20,000-29,999" = "20-30K",
                         "£30,000-39,999" = "30-40K",
                         "£40,000-49,999" = "40-50K",
                         "£50,000-59,999" = "50-60K",
                         "£60,000-69,999" = "60-70K",
                         "£70,000-79,999" = "70-80K",
                         "£80,000-89,999" = "80-90K",
                         "£90,000-99,999" = "90-100K",
                         "Over £100,000" = "Over100K"),
         condition = as.factor(condition))

allData$income <- factor(allData$income, levels=c("Under10K",
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

# missing data
library(naniar)
vis_miss(allData)
# 2.4% missing
allData %>% 
  complete.cases() %>% 
  sum()/dim(allData)[1]
# 74% complete cases

# see if proportion of missing roughly equal by condition
library(sjmisc)
missing_df <- allData %>%
  select(condition, education, income, eu_vote, employment, student, too_many_imm, brexit_to_cut, increase_imm, sm_or_control) %>% 
  group_by(condition) %>%       
  summarise_all(funs(sum(is.na(.))/n()*100)) %>% 
  mutate(across(2:10, round, 2)) %>% 
  rotate_df() %>% 
  rownames_to_column()

write_csv(missing_df, 'tables/missing_values_by_cond.csv')
 
# impute missing data
library(Hmisc)
names(allData)
impute_arg <- aregImpute(~ age +
                           gender +
                           education +
                           party +
                           income +
                           too_many_imm +
                           take_jobs +
                           drive_down_wages +
                           create_jobs +
                           brexit_to_cut +
                           increase_imm +
                           sm_or_control +
                           eu_vote +
                           employment +
                           student,
                         data = allData, n.impute = 10, tlinear = FALSE)
impute_arg
imp_data <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=allData, list.out=TRUE,pr=FALSE, check=FALSE)) 
head(imp_data, 20)

modelData <- tibble(cbind(imp_data, allData$condition))
names(modelData)[16] <- "condition"
modelData

write_csv(modelData, 'data/preprocessed_data.csv')
