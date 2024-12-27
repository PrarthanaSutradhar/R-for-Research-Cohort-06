# load packages
library(tidyverse)
library(readxl)
library(gtsummary)
library(gt)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(likert)
library(ISLR)
library(easystats)
library(ggthemes)

# load data
Data <- read_excel("E:/CHIRAL/R for Reseaech C06/Assignment/AMR_KAP_Data_Clean data.xlsx")

# explore data
head(Data)
glimpse(Data)

# checking missing values
is.na(Data)
sum(is.na(Data))

# removing missing values
Data <- na.omit(Data)
sum(is.na(Data))

# convert data type
# Demographic data
Data$`Parent’s sex` <- as.factor(Data$`Parent’s sex`)
Data$`Parent’s age (years)` <- as.factor(Data$`Parent’s age (years)`)
Data$`Parent’s education level` <- as.factor(Data$`Parent’s education level`)
Data$`Employment status` <- as.factor(Data$`Employment status`)
Data$`Family type` <- as.factor(Data$`Family type`)
Data$`Your average household income per month (BDT)` <- as.factor(Data$`Your average household income per month (BDT)`)
Data$`Child’s sex` <- as.factor(Data$`Child’s sex`)
Data$`Child’s age (years)` <- as.factor(Data$`Child’s age (years)`)
Data$`Number of children`<- as.factor(Data$`Number of children`)
Data$`Who is the leading child caregiver at home?` <- as.factor(Data$`Who is the leading child caregiver at home?`)
Data$`Are grandparents at home involved in treatment decisions when your child is ill?` <- as.factor(Data$`Are grandparents at home involved in treatment decisions when your child is ill?`)

# Attitude AMR
Data$`A child with cold is given antibiotics(Disagree)` <- as.factor(Data$`A child with cold is given antibiotics(Disagree)`)
Data$`I stop antibiotics when my child condition improves(Disagree)` <- as.factor(Data$`I stop antibiotics when my child condition improves(Disagree)`)
Data$`I reusing the same antibiotics for similar symptoms(Disagree)` <- as.factor(Data$`I reusing the same antibiotics for similar symptoms(Disagree)`)
Data$`Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)` <- as.factor(Data$`Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)`)
Data$`Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)` <- as.factor(Data$`Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)`)
Data$` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)` <- as.factor(Data$` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)`)
Data$` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)` <- as.factor(Data$` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)`)
Data$` Antibiotics are safe and hence can be used commonly(Disagree)` <- as.factor(Data$` Antibiotics are safe and hence can be used commonly(Disagree)`)
Data$` Sick child is given antibiotics, even there is no indication(Disagree)` <- as.factor(Data$` Sick child is given antibiotics, even there is no indication(Disagree)`)
Data$` Antibiotics can improve fever in children(Disagree)` <- as.factor(Data$` Antibiotics can improve fever in children(Disagree)`)


# Source of information
Data$`Information provided by pharmaceutical companies leaflet` <- as.factor(Data$`Information provided by pharmaceutical companies leaflet`)
Data$`Information from prescribers` <- as.factor(Data$`Information from prescribers`)
Data$`Information from dispensers` <- as.factor(Data$`Information from dispensers`)
Data$`Information from nurses` <- as.factor(Data$`Information from nurses`)
Data$`Information from University courses` <- as.factor(Data$`Information from University courses`)
Data$`Information given by a colleague` <- as.factor(Data$`Information given by a colleague`)
Data$`Information provided by pharmaceutical companies leaflet` <- as.factor(Data$`Information provided by pharmaceutical companies leaflet`)
Data$Internet <- as.factor(Data$Internet)
Data$`Social media` <- as.factor(Data$`Social media`)
Data$Others <- as.factor(Data$Others)

# Knowledge AMR
Data$`Antibiotic kills the bacteria(Yes)` <- as.factor(Data$`Antibiotic kills the bacteria(Yes)`)
Data$` Amoxicillin is an antibiotic (Yes)` <- as.factor(Data$` Amoxicillin is an antibiotic (Yes)`)
Data$` Azithromycin is an antibiotic(Yes)` <- as.factor(Data$` Azithromycin is an antibiotic(Yes)`)
Data$` Paracetamol is an antibiotic(No)` <- as.factor(Data$` Paracetamol is an antibiotic(No)`)
Data$` Antibiotic kills the virus(No)` <- as.factor(Data$` Antibiotic kills the virus(No)`)
Data$` Antibiotics used to treat diarrhoea(Yes)` <- as.factor(Data$` Antibiotics used to treat diarrhoea(Yes)`)
Data$` Antibiotics are useful for flu and cough(No)` <- as.factor(Data$` Antibiotics are useful for flu and cough(No)`)
Data$` Antibiotic resistant bacteria are difficult to treat(Yes)` <- as.factor(Data$` Antibiotic resistant bacteria are difficult to treat(Yes)`)
Data$` Misuse of antibiotics can lead to antibiotic resistant bacteria(Yes)` <- as.factor(Data$`Parent’s sex`)
Data$` Antibiotics can cause allergic reactions(Yes)` <- as.factor(Data$` Antibiotics can cause allergic reactions(Yes)`)
Data$Infectious disease are becoming difficult to treat with antibiotics(Yes) <- as.factor(Data$Infectious disease are becoming difficult to treat with antibiotics(Yes))
                                                                                              
# Practice AMR
Data$` I give my children antibiotics(No)` <- as.factor(Data$` I give my children antibiotics(No)`)
Data$` I check expiring date of antibiotic before giving to children(Yes)` <- as.factor(Data$` I check expiring date of antibiotic before giving to children(Yes)`)
Data$` I seek medical advice before giving antibiotic to my children(Yes)` <- as.factor(Data$` I seek medical advice before giving antibiotic to my children(Yes)`)
Data$` I give my children antibiotics when they get cough(No)` <- as.factor(Data$` I give my children antibiotics when they get cough(No)`)
Data$` I like to take antibiotic from pharmacy instead of taking from doctor(No)` <- as.factor(Data$` I like to take antibiotic from pharmacy instead of taking from doctor(No)`)
Data$` My child should complete a given dose, even he improve after 2 dose(Yes)` <- as.factor(Data$` My child should complete a given dose, even he improve after 2 dose(Yes)`)

# change column name
colnames(Data) <- paste0("Q", 1:48)

# Demographic data - Table 1: Demographic characteristics of study participants (N = 704).
Demo_data <- Data |> select(Q1:Q11) |> 
  tbl_summary() |> 
   as_gt() |> 
   gtsave("Tab-Fig/Table1.docx")

# 'Knowledge of AMR' data
unique(knowledge_data$Q20)

knowledge_data <- Data |> select(Q12:Q23) |>
  select(Q12:Q23) |>
  mutate(across(Q12:Q14, ~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0,
    . == "Don't Know" ~ 0,
    TRUE ~ NA_real_
  ))) |>
  mutate(across(Q15:Q16, ~case_when(
    . == "No" ~ 1,
    . == "Yes" ~ 0,
    . == "Don't Know" ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  mutate(Q17 = case_when(
    Q17 == "Yes" ~ 1,
    Q17 == "No" ~ 0,
    Q17 == "Don't Know" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q18 = case_when(
    Q18 == "No" ~ 1,
    Q18 == "Yes" ~ 0,
    Q18 == "Don't Know" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(across(Q19:Q23, ~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0,
    . == "Don't Know" ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(Knowledge_score = sum(c_across(Q12:Q23), na.rm = TRUE)) |> 
  mutate(Knowledge_level = case_when(
    Knowledge_score < 6 ~ "Poor",
    Knowledge_score >= 6 ~ "Moderate"   
  ))

knowledge_data |> select(14) |> tbl_summary()

# 'Attitude towards AMR' data
attitude_data <- Data |> select(Q24:Q33) |>
  select(Q24:Q33) |> 
  mutate(across(Q24:Q33, ~case_when(
    . == "Disagree" ~ 1,
    . == "Agree" ~ 0,
    . == "Neutral" ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |> 
  mutate(Attitude_score = sum(c_across(Q24:Q33), na.rm = TRUE)) |> 
  mutate(Attitude_level = case_when(
    Attitude_score < 5 ~ "Negative",
    Attitude_score >= 8 ~ "Positive"
    ))

attitude_data |> select(12) |> tbl_summary()

# 'Practice regarding AMR' data
practice_data <- Data |> select(Q34:Q39) |>
  select(Q34:Q39) |> 
  mutate(Q34 = case_when(
    Q34 == "No" ~ 1,
    Q34 == "Yes" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q35 = case_when(
    Q35 == "Yes" ~ 1,
    Q35 == "No" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q36 = case_when(
    Q36 == "Yes" ~ 1,
    Q36 == "No" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q37 = case_when(
    Q37 == "No" ~ 1,
    Q37 == "Yes" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q38 = case_when(
    Q38 == "No" ~ 1,
    Q38 == "Yes" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q39 = case_when(
    Q39 == "Yes" ~ 1,
    Q39 == "No" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  rowwise() |> 
  mutate(Practice_score = sum(c_across(Q34:Q39), na.rm = TRUE)) |> 
  mutate(Practice_level = case_when(
    Practice_score < 5 ~ "Misuse",
    Practice_score >=5 ~ "Good"
  ))
  
  
practice_data |> select(8) |> tbl_summary()  

# Factors associated with levels of Prctice
# combining data
New_data_P <- cbind(Demo_data,practice_data)

#  recode Practice level
New_data_P <- New_data_P |> 
  mutate(Practice_Code = case_when(
    Practice_level == "Misuse" ~ 0,
    Practice_level == "Good" ~ 1
  ))

# logistic regression 

New_data_P |> 
  select(1:9, Practice_Code) |> 
  tbl_uvregression(
    method = glm,
    method.args = list(family = binomial),
    y = Practice_Code,
    expotentiate = TRUE
  ) |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("Tab-Fig/Table6.docx")

# Factors associated with level of Attitude
# combining data
New_data_A <- cbind(Demo_data,attitude_data)

#  recode Practice level
New_data_A<- New_data_A |> 
  mutate(Attitude_Code = case_when(
    Attitude_level == "Negative" ~ 0,
    Attitude_level == "Positive" ~ 1
  ))

# logistic regression 

New_data_A |> 
  select(1:9, Attitude_Code) |> 
  tbl_uvregression(
    method = glm,
    method.args = list(family = binomial),
    y = Attitude_Code,
    expotentiate = TRUE
  ) |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("Tab-Fig/Table5.docx")


# Factors associated with level of knowledge
# combining data
New_data_K <- cbind(Demo_data, knowledge_data)

#  recode Practice level
New_data_K<- New_data_K |> 
  mutate(Knowledge_Code = case_when(
    Knowledge_level == "Poor" ~ 0,
    Knowledge_level == "Moderate" ~ 1
  ))

# logistic regression 

New_data_K |> 
  select(1:9, Knowledge_Code) |> 
  tbl_uvregression(
    method = glm,
    method.args = list(family = binomial),
    y = Knowledge_Code,
    expotentiate = TRUE
  ) |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("Tab-Fig/Table4.docx")

# 'Sources of information' data
Soi_data <- Data |> select(40:48) |>
  tbl_summary() |>
  as_gt() |>
  gtsave("Tab-Fig/Table2.docx")

# Figure 3. Practices among parents of school-going children regarding antibiotic resistance (N = 704).
# create a stacked barplot
# 1st create dataset
practice <- c(rep("I give my children antibiotics(No)",2), 
             rep("I check expiring date of antibiotic before giving to children(Yes)",2),
             rep("I seek medical advice before giving antibiotic to my children(Yes)",2),
             rep("I give my children antibiotics when they get cough(No)",2),
             rep("I like to take antibiotic from pharmacy instead of taking from doctor(No)",2),
             rep("My child should complete a given dose, even he improve after 2 dose(Yes)",2))

Response <- rep(c("No", "Yes"), 6)
Percentage <- c(31,69,18,82,64,36,42,58,49,51,24,76)
data_bar <- data.frame(practice, Response, Percentage)

# staked barplot
ggplot(data_bar, aes(fill = Response, y = practice,
                     x = Percentage)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_pubr(base_size = 6, base_family = "Arial") 
ggsave("Figure3.png", dpi = 300)



# Figure 1. Distribution of knowledge of antibiotic resistance among parents of school-going children (N = 704).
# create a stacked barplot
# 1st create dataset
knowledge <- c(rep("Antibiotic kills the bacteria(Yes)",3), 
              rep("Amoxicillin is an antibiotic (Yes) ",3),
              rep("Azithromycin is an antibiotic(Yes)",3),
              rep("Paracetamol is an antibiotic(No)",3),
              rep("Antibiotic kills the virus(No)",3),
              rep("Antibiotics used to treat diarrhoea(Yes)",3),
              rep("Antibiotics are useful for flu and cough(No)",3),
              rep("Antibiotic resistant bacteria are difficult to treat(Yes)",3),
              rep("Misuse of antibiotics can lead to antibiotic resistant bacteria(Yes)",3),
              rep("Antibiotics can cause allergic reactions(Yes)",3),
              rep("Antibiotics can kill normal flora(Yes)",3),
              rep("Infectious disease are becoming difficult to treat with antibiotics(Yes)",3))

Response <- rep(c("Don't know", "No", "Yes"), 12)
Percentage <- c(38,6,56,63,11,26,56,8,36,12,79,9,44,12,44,14,52,34,5,39,57,42,11,47,15,9,75,20,38,42,45,13,41,39,27,34)
knowledge_bar <- data.frame(knowledge, Response, Percentage)

# staked barplot
ggplot(knowledge_bar, aes(fill = Response, y = knowledge,
                     x = Percentage)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_pubr(base_size = 6, base_family = "Arial") 
ggsave("Figure1.png", dpi = 300)


# Figure 2. Attitude towards antibiotic resistance and the misuse of antibiotics among parents of school-going children
# create a stacked barplot
# 1st create dataset
Attitude <- c(rep("I will see another doctor if the first one has not been prescribed antibiotics(Disagree)",3),
               rep("I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)",3),
               rep("Antibiotics are safe and hence can be used commonly(Disagree)",3),
               rep("Sick child is given antibiotics, even there is no indication(Disagree)",3),
               rep("Antibiotics can improve fever in children(Disagree)",3),
               rep(" A child with cold is given antibiotics(Disagree)",3),
               rep(" I stop antibiotics when my child condition improves(Disagree)",3),
               rep(" I reusing the same antibiotics for similar symptoms(Disagree)",3),
               rep(" Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)",3),
               rep("Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)",3))

Response <- rep(c("Agree", "Disagree", "Neutral"), 10)
Percentage <- c(16,81,3,16,80,4,28,64,9,20,75,6,64,31,6,62,33,5,26,74,0,27,71,1,15,84,1,52,42,5)
attitude_bar <- data.frame(Attitude, Response, Percentage)

# staked barplot
ggplot(attitude_bar, aes(fill = Response, y = Attitude,
                          x = Percentage)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_pubr(base_size = 6, base_family = "Arial") 
ggsave("Figure2.png", dpi = 300)




