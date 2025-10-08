
################mit Gruppenzuteilung AE-FS#########################16.06.25
###############Regression 3 months###########################

# Loading the packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lme4")
install.packages("GGally") 
install.packages("car")
install.packages("performance")
install.packages("Matrix")
install.packages("generics")
install.packages("tidyselect")
install.packages("brms") 
install.packages("sjPlot")
install.packages ("Rcpp")
install.packages ("rstudioapi")
install.packages("tidyr")
install.packages("bayesplot")

library(dplyr)
library(ggplot2)
library(lme4)
library(GGally)
library(car)
library(performance)
library(Matrix)
library(generics)
library(tidyselect)
library(brms)
library(sjPlot)
library(Rcpp)
library(rstudioapi)
library(tidyr)
library(bayesplot)


# Manual assignment of column names
colnames(data) <- c("study_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance","paindetect1","paindetect2","paindetect3","ndi1","ndi2","ndi3","ndi4","ndi5","ndi6",
                    "ndi7","ndi8","ndi9","ndi10","ndiscore","cortisol1","cortisone1","score_stress_skala_1",
                    "score_stress_skala_2","score_stress_skala_3","gesamtscore_symptom","sci_stress1_1","sci_stress1_2",
                    "sci_stress1_3","sci_stress1_4","sci_stress1_5","sci_stress1_6","sci_stress1_7","sci_stress2_1",
                    "sci_stress2_2","sci_stress2_3","sci_stress2_4","sci_stress2_5","sci_stress2_6","sci_stress3_1",
                    "sci_stress3_2","sci_stress3_3","sci_stress3_4","sci_stress3_5","sci_stress3_6","sci_stress3_7",
                    "sci_symtpom1","sci_symtpom2","sci_symtpom3","sci_symtpom4","sci_symtpom5","sci_symtpom6",
                    "sci_symtpom7","sci_symtpom8","sci_symtpom9","sci_symtpom10","sci_symtpom11","sci_symtpom12",
                    "sci_symtpom13","pps1_light","pps2_light","pps3_light","pps4_light","pps5_light","pps6_light",
                    "pps7_light","pps1_strong","pps2_strong","pps3_strong","pps4_strong","pps5_strong","pps6_strong",
                    "pps7_strong","aefs_dms1","aefs_dms2","score_pps","dms_score","dass21_depression","age", "gender", "cortisol2", "cortisone2")

# Delete the first line
data <- data [-1, ]

# Define columns in which empty positions are to be replaced with NA
columns_to_fill <- c("paindetect1","paindetect2","paindetect3","ndi1","ndi2","ndi3","ndi4","ndi5","ndi6",
                     "ndi7","ndi8","ndi9","ndi10","ndiscore","cortisol1","cortisone1","score_stress_skala_1",
                     "score_stress_skala_2","score_stress_skala_3","gesamtscore_symptom","sci_stress1_1","sci_stress1_2",
                     "sci_stress1_3","sci_stress1_4","sci_stress1_5","sci_stress1_6","sci_stress1_7","sci_stress2_1",
                     "sci_stress2_2","sci_stress2_3","sci_stress2_4","sci_stress2_5","sci_stress2_6","sci_stress3_1",
                     "sci_stress3_2","sci_stress3_3","sci_stress3_4","sci_stress3_5","sci_stress3_6","sci_stress3_7",
                     "sci_symtpom1","sci_symtpom2","sci_symtpom3","sci_symtpom4","sci_symtpom5","sci_symtpom6",
                     "sci_symtpom7","sci_symtpom8","sci_symtpom9","sci_symtpom10","sci_symtpom11","sci_symtpom12",
                     "sci_symtpom13","pps1_light","pps2_light","pps3_light","pps4_light","pps5_light","pps6_light",
                     "pps7_light","pps1_strong","pps2_strong","pps3_strong","pps4_strong","pps5_strong","pps6_strong",
                     "pps7_strong","aefs_dms1","aefs_dms2","score_pps","dms_score","dass21_depression", "age","gender","cortisol2", "cortisone2")

# Replace empty positions (represented as empty strings or explicit NAs) in these columns with NA
data[columns_to_fill] <- lapply(data [columns_to_fill], function(x) {
  x[x == "" | is.na(x)] <- NA
  return(x)
})


#Filtering of rows from different measurements (questionnaires or clinical measurements)
data_depression <- data %>% 
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%  # Selection of the T1 questionnaire data
  dplyr::select(study_id, dass21_depression)  # Selection of the relevant variables (ID + depression)

data_cortisol <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  dplyr::select(study_id, cortisol1)

data_cortisone <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  dplyr::select(study_id, cortisone1)

data_cortisol3mo <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  dplyr::select(study_id, cortisol2)

data_cortisone3mo <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  dplyr::select(study_id, cortisone2)

data_pain_acute <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, paindetect1,paindetect2,paindetect3)

data_pain_3months <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, paindetect1,paindetect2,paindetect3)

data_disability <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, ndiscore)

data_age <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, age)

data_gender <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, gender)

data_activity_patterns <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, score_pps,dms_score)

data_stress <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, score_stress_skala_1,score_stress_skala_2,score_stress_skala_3,gesamtscore_symptom)

######################Umcodierungen für Schmerzdurchschnittswert


# 2. Mittelwert berechnen
data_pain_acute <- data_pain_acute %>%
  mutate(pain_avg_acute = rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE))

data_pain_3months <- data_pain_3months %>%
  mutate(pain_avg_3months = rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE))


# Überprüfen, ob die neue Spalte korrekt erstellt wurde
head(data_pain_acute$pain_avg_acute)
head(data_pain_3months$pain_avg_3months)



################Pearson Korrelation zw. subj. & obj. Stressmessung(Cortisol1):
# 1. Relevante Variablen auswählen und zusammenführen
data_stress_objektiv <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisol1) %>%
  mutate(cortisol1 = as.numeric(cortisol1))  # sicherstellen, dass numerisch

# 2. Subjektive + objektive Daten zusammenführen
data_stress_merged <- data_stress_sums %>%
  left_join(data_stress_objektiv, by = "study_id")

# 3. NA-Fälle entfernen (nur vollständige Daten verwenden)
data_stress_complete <- data_stress_merged %>%
  filter(!is.na(cortisol1),
         !is.na(stress_skala1_sum),
         !is.na(stress_skala2_sum),
         !is.na(stress_skala_symptome_sum))

# 4. Pearson-Korrelation berechnen
cor.test(data_stress_complete$cortisol1, data_stress_complete$stress_skala1_sum, method = "pearson")
cor.test(data_stress_complete$cortisol1, data_stress_complete$stress_skala2_sum, method = "pearson")
cor.test(data_stress_complete$cortisol1, data_stress_complete$stress_skala_symptome_sum, method = "pearson")

#Summe rausgeben
sum(complete.cases(data$stress_skala1_sum, data$cortisol1))


# Scatterplot: Cortisol1 vs. Stress due to uncertainty
ggplot(data_stress_complete, aes(x = stress_skala1_sum, y = cortisol1)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Cortisol1 vs. SCI – Stress due to uncertainty",
    x = "SCI – Stress due to uncertainty",
    y = "Hair Cortisol (pg/mg)"
  )

# Scatterplot: Cortisol1 vs. Stress due to excessive demands
ggplot(data_stress_complete, aes(x = stress_skala2_sum, y = cortisol1)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Cortisol1 vs. SCI – Stress due to excessive demands",
    x = "SCI – Stress due to excessive demands",
    y = "Hair Cortisol (pg/mg)"
  )

# Scatterplot: Cortisol1 vs. Physical and psychological stress symptoms
ggplot(data_stress_complete, aes(x = stress_skala_symptome_sum, y = cortisol1)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Cortisol1 vs. SCI – Physical and psychological stress symptoms",
    x = "SCI – Physical and psychological stress symptoms",
    y = "Hair Cortisol (pg/mg)"
  )


################Pearson Korrelation zw. subj. & obj. Stressmessung (Cortisol2):
data_stress_objektiv2 <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  mutate(cortisol2 = as.character(cortisol2)) %>%
  mutate(cortisol2 = na_if(cortisol2, ""),
         cortisol2 = na_if(cortisol2, "NA"),
         cortisol2 = na_if(cortisol2, ".")) %>%
  mutate(cortisol2 = as.numeric(cortisol2)) %>%
  select(study_id, cortisol2)


# 1. Relevante Variablen auswählen und zusammenführen
data_stress_objektiv2 <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisol2) %>%
  mutate(cortisol2 = as.numeric(cortisol2))

# 2. Subjektive + objektive Daten zusammenführen
data_stress_merged2 <- data_stress_sums %>%
  left_join(data_stress_objektiv2, by = "study_id")

# 3. NA-Fälle entfernen (nur vollständige Daten verwenden)
data_stress_complete2 <- data_stress_merged2 %>%
  filter(!is.na(cortisol2),
         !is.na(stress_skala1_sum),
         !is.na(stress_skala2_sum),
         !is.na(stress_skala_symptome_sum))

# 4. Pearson-Korrelation berechnen
cor.test(data_stress_complete2$cortisol2, data_stress_complete2$stress_skala1_sum, method = "pearson")
cor.test(data_stress_complete2$cortisol2, data_stress_complete2$stress_skala2_sum, method = "pearson")
cor.test(data_stress_complete2$cortisol2, data_stress_complete2$stress_skala_symptome_sum, method = "pearson")

# 5. Zeilenanzahl mit vollständigen Daten
sum(complete.cases(data_stress_merged2$cortisol2, data_stress_merged2$stress_skala1_sum))


# Scatterplot: Cortisol2 vs. Stress due to uncertainty
ggplot(data_stress_complete2, aes(x = stress_skala1_sum, y = cortisol2)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    title = "Cortisol2 vs. SCI – Stress due to uncertainty",
    x = "SCI – Stress due to uncertainty",
    y = "Hair Cortisol2 (pg/mg)"
  )

# Scatterplot: Cortisol2 vs. Stress due to excessive demands
ggplot(data_stress_complete2, aes(x = stress_skala2_sum, y = cortisol2)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    title = "Cortisol2 vs. SCI – Stress due to excessive demands",
    x = "SCI – Stress due to excessive demands",
    y = "Hair Cortisol2 (pg/mg)"
  )

# Scatterplot: Cortisol2 vs. Physical and psychological stress symptoms
ggplot(data_stress_complete2, aes(x = stress_skala_symptome_sum, y = cortisol2)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    title = "Cortisol2 vs. SCI – Physical and psychological stress symptoms",
    x = "SCI – Physical and psychological stress symptoms",
    y = "Hair Cortisol2 (pg/mg)"
  )


################Pearson Korrelation zw. subj. & obj. Stressmessung(Cortisone1):
# 1. Relevante Variablen auswählen und zusammenführen
data_stress_objektiv3 <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisone1) %>%
  mutate(cortisone1 = as.numeric(cortisone1))  # sicherstellen, dass numerisch

# 2. Subjektive + objektive Daten zusammenführen
data_stress_merged3 <- data_stress_sums %>%
  left_join(data_stress_objektiv3, by = "study_id")

# 3. NA-Fälle entfernen (nur vollständige Daten verwenden)
data_stress_complete3 <- data_stress_merged3 %>%
  filter(!is.na(cortisone1),
         !is.na(stress_skala1_sum),
         !is.na(stress_skala2_sum),
         !is.na(stress_skala_symptome_sum))

# 4. Pearson-Korrelation berechnen
cor.test(data_stress_complete3$cortisone1, data_stress_complete3$stress_skala1_sum, method = "pearson")
cor.test(data_stress_complete3$cortisone1, data_stress_complete3$stress_skala2_sum, method = "pearson")
cor.test(data_stress_complete3$cortisone1, data_stress_complete3$stress_skala_symptome_sum, method = "pearson")

#Summe rausgeben
sum(complete.cases(data$stress_skala1_sum, data$cortisone1))

# Scatterplot: Cortisone1 vs. Stress due to uncertainty
ggplot(data_stress_complete3, aes(x = stress_skala1_sum, y = cortisone1)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Cortisone1 vs. SCI – Stress due to uncertainty",
    x = "SCI – Stress due to uncertainty",
    y = "Hair Cortisone1 (pg/mg)"
  )

# Scatterplot: Cortisone1 vs. Stress due to excessive demands
ggplot(data_stress_complete3, aes(x = stress_skala2_sum, y = cortisone1)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Cortisone1 vs. SCI – Stress due to excessive demands",
    x = "SCI – Stress due to excessive demands",
    y = "Hair Cortisone1 (pg/mg)"
  )

# Scatterplot: Cortisone1 vs. Physical and psychological stress symptoms
ggplot(data_stress_complete3, aes(x = stress_skala_symptome_sum, y = cortisone1)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Cortisone1 vs. SCI – Physical and psychological stress symptoms",
    x = "SCI – Physical and psychological stress symptoms",
    y = "Hair Cortisone1 (pg/mg)"
  )

################Pearson Korrelation zw. subj. & obj. Stressmessung (Cortisone 2):
data_stress_objektiv4 <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  mutate(cortisone2 = as.character(cortisone2)) %>%
  mutate(cortisone2 = na_if(cortisone2, ""),
         cortisone2 = na_if(cortisone2, "NA"),
         cortisone2 = na_if(cortisone2, ".")) %>%
  mutate(cortisone2 = as.numeric(cortisone2)) %>%
  select(study_id, cortisone2)


# 1. Relevante Variablen auswählen und zusammenführen
data_stress_objektiv4 <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisone2) %>%
  mutate(cortisone2 = as.numeric(cortisone2))

# 2. Subjektive + objektive Daten zusammenführen
data_stress_merged4 <- data_stress_sums %>%
  left_join(data_stress_objektiv4, by = "study_id")

# 3. NA-Fälle entfernen (nur vollständige Daten verwenden)
data_stress_complete4 <- data_stress_merged4 %>%
  filter(!is.na(cortisone2),
         !is.na(stress_skala1_sum),
         !is.na(stress_skala2_sum),
         !is.na(stress_skala_symptome_sum))

# 4. Pearson-Korrelation berechnen
cor.test(data_stress_complete4$cortisone2, data_stress_complete4$stress_skala1_sum, method = "pearson")
cor.test(data_stress_complete4$cortisone2, data_stress_complete4$stress_skala2_sum, method = "pearson")
cor.test(data_stress_complete4$cortisone2, data_stress_complete4$stress_skala_symptome_sum, method = "pearson")

# 5. Zeilenanzahl mit vollständigen Daten
sum(complete.cases(data_stress_merged4$cortisone2, data_stress_merged4$stress_skala1_sum))


# Scatterplot: Cortisone2 vs. Stress due to uncertainty
ggplot(data_stress_complete4, aes(x = stress_skala1_sum, y = cortisone2)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Cortisone2 vs. SCI – Stress due to uncertainty",
    x = "SCI – Stress due to uncertainty",
    y = "Hair Cortisone2 (pg/mg)"
  )

# Scatterplot: Cortisone2 vs. Stress due to excessive demands
ggplot(data_stress_complete4, aes(x = stress_skala2_sum, y = cortisone2)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Cortisone2 vs. SCI – Stress due to excessive demands",
    x = "SCI – Stress due to excessive demands",
    y = "Hair Cortisone2 (pg/mg)"
  )

# Scatterplot: Cortisone2 vs. Physical and psychological stress symptoms
ggplot(data_stress_complete4, aes(x = stress_skala_symptome_sum, y = cortisone2)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Cortisone2 vs. SCI – Physical and psychological stress symptoms",
    x = "SCI – Physical and psychological stress symptoms",
    y = "Hair Cortisone2 (pg/mg)"
  )

###############Cortisol1 nach male und female aufgeteilt für Baseline characteristics: mean, range, 95%CI  


table(data$gender, useNA = "ifany")
str(data$gender)


intersect(data_cortisol1$study_id, data_gender$study_id)

str(data_cortisol1$study_id)
str(data_gender$study_id)


data_cortisol1 <- data_cortisol1 %>%
  mutate(study_id = as.character(study_id))

data_gender <- data_gender %>%
  mutate(study_id = as.character(study_id))

cortisol_gender_data <- left_join(data_cortisol1, data_gender, by = "study_id")

nrow(cortisol_gender_data)        # Wie viele Zeilen sind übrig?
table(cortisol_gender_data$gender_label, useNA = "ifany")  # Zeigt dir, ob Gender korrekt zugewiesen wurde



# Gruppierte Zusammenfassung für cortisol1
cortisol_summary <- cortisol_gender_data %>%
  filter(!is.na(gender_label)) %>%
  group_by(gender_label) %>%
  summarise(
    n = n(),
    mean = mean(cortisol1, na.rm = TRUE),
    sd = sd(cortisol1, na.rm = TRUE),
    min = min(cortisol1, na.rm = TRUE),
    max = max(cortisol1, na.rm = TRUE),
    ci_lower = mean - qt(0.975, df = n - 1) * sd / sqrt(n),
    ci_upper = mean + qt(0.975, df = n - 1) * sd / sqrt(n)
  )

print(cortisol_summary)

###############Cortisol2 nach male und female aufgeteilt für Baseline characteristics: mean, range, 95%CI  
data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  summarise(
    n_total = n(),
    n_cortisol2_valid = sum(!is.na(cortisol2))
  )

# Sicherstellen, dass cortisol2 numerisch ist
data$cortisol2 <- as.numeric(data$cortisol2)

# Gender-Daten nur von t1 (oder konsistenten Zeitpunkt) extrahieren
gender_data <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  select(study_id, gender)

# Cortisol2-Daten extrahieren vom passenden Zeitpunkt
cortisol2_data <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisol2) %>%
  mutate(cortisol2 = as.numeric(cortisol2))

# Zusammenführen: nur Fälle mit vorhandenen Werten
cortisol2_gender_data <- left_join(cortisol2_data, gender_data, by = "study_id") %>%
  filter(!is.na(cortisol2), !is.na(gender)) %>%
  mutate(gender_label = ifelse(gender == "1", "Male",
                               ifelse(gender == "2", "Female", NA)))


cortisol2_summary <- cortisol2_gender_data %>%
  group_by(gender_label) %>%
  summarise(
    n = n(),
    mean = mean(cortisol2),
    sd = sd(cortisol2),
    min = min(cortisol2),
    max = max(cortisol2),
    ci_lower = mean(cortisol2) - qt(0.975, df = n()-1) * sd(cortisol2) / sqrt(n()),
    ci_upper = mean(cortisol2) + qt(0.975, df = n()-1) * sd(cortisol2) / sqrt(n())
  )

print(cortisol2_summary)

###############Cortisone 1 nach male und female aufgeteilt für Baseline characteristics: mean, range, 95%CI  
data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  summarise(
    n_total = n(),
    n_cortisone1_valid = sum(!is.na(cortisone1))
  )

# Sicherstellen, dass cortisol2 numerisch ist
data$cortisone1 <- as.numeric(data$cortisone1)

# Gender-Daten nur von t1 (oder konsistenten Zeitpunkt) extrahieren
gender_data <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  select(study_id, gender)

# Cortisol2-Daten extrahieren vom passenden Zeitpunkt
cortisone1_data <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisone1) %>%
  mutate(cortisone1 = as.numeric(cortisone1))

# Zusammenführen: nur Fälle mit vorhandenen Werten
cortisone1_gender_data <- left_join(cortisone1_data, gender_data, by = "study_id") %>%
  filter(!is.na(cortisone1), !is.na(gender)) %>%
  mutate(gender_label = ifelse(gender == "1", "Male",
                               ifelse(gender == "2", "Female", NA)))

# Gruppierung und Berechnung der Statistiken


cortisone1_summary <- cortisone1_gender_data %>%
  group_by(gender_label) %>%
  summarise(
    n = n(),
    mean = mean(cortisone1),
    sd = sd(cortisone1),
    min = min(cortisone1),
    max = max(cortisone1),
    ci_lower = mean(cortisone1) - qt(0.975, df = n()-1) * sd(cortisone1) / sqrt(n()),
    ci_upper = mean(cortisone1) + qt(0.975, df = n()-1) * sd(cortisone1) / sqrt(n())
  )

print(cortisone1_summary)

###############Cortisone 2 nach male und female aufgeteilt für Baseline characteristics: mean, range, 95%CI  
data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  summarise(
    n_total = n(),
    n_cortisone1_valid = sum(!is.na(cortisone2))
  )

# Sicherstellen, dass cortisone2 numerisch ist
data$cortisone2 <- as.numeric(data$cortisone2)

# Gender-Daten nur von t1 (oder konsistenten Zeitpunkt) extrahieren
gender_data <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  select(study_id, gender)

# Cortisone2-Daten extrahieren vom passenden Zeitpunkt
cortisone2_data <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisone2) %>%
  mutate(cortisone2 = as.numeric(cortisone2))

# Zusammenführen: nur Fälle mit vorhandenen Werten
cortisone2_gender_data <- left_join(cortisone2_data, gender_data, by = "study_id") %>%
  filter(!is.na(cortisone2), !is.na(gender)) %>%
  mutate(gender_label = ifelse(gender == "1", "Male",
                               ifelse(gender == "2", "Female", NA)))

# Gruppierung und Berechnung der Statistiken


cortisone2_summary <- cortisone2_gender_data %>%
  group_by(gender_label) %>%
  summarise(
    n = n(),
    mean = mean(cortisone2),
    sd = sd(cortisone2),
    min = min(cortisone2),
    max = max(cortisone2),
    ci_lower = mean(cortisone2) - qt(0.975, df = n()-1) * sd(cortisone2) / sqrt(n()),
    ci_upper = mean(cortisone2) + qt(0.975, df = n()-1) * sd(cortisone2) / sqrt(n())
  )

print(cortisone2_summary)



#########################Multikollinearität prüfen

# Vor dem Berechnen numerische Umwandlung sicherstellen
# Schmerzdaten vorbereiten (AKUT, T1)
data_pain_acute <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric)) %>%
  mutate(pain_avg_acute = rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE)) %>%
  select(study_id, pain_avg_acute)

# Schmerzdaten vorbereiten (3 Monate, T2)
data_pain_3months <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric)) %>%
  mutate(pain_avg_3months = rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE)) %>%
  select(study_id, pain_avg_3months)


# 1. Alle anderen Variablen vorbereiten (T1: Alter, Gender, etc.)
data_predictors <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  select(study_id, age, gender, dass21_depression, score_pps, dms_score)

# 2. Cortisol (T1)
data_cortisol <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  select(study_id, cortisol1, cortisone1)

# 3. NDI (T2)
data_ndi_3months <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  select(study_id, ndiscore)

# 4. Stresssummen (T1)
data_stress_sums <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  select(study_id, stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum)

# 5. Alles kombinieren
data_vif_full <- data_predictors %>%
  left_join(data_cortisol, by = "study_id") %>%
  left_join(data_pain_acute, by = "study_id") %>%
  left_join(data_pain_3months, by = "study_id") %>%
  left_join(data_ndi_3months, by = "study_id") %>%
  left_join(data_stress_sums, by = "study_id")

# 6. Nur numerische Variablen imputieren
data_vif_full <- data_vif_full %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

#############Gruppenzuweisung activity patterns:
# Sicherstellen, dass score_pps und dms_score numerisch sind
data_vif_full$score_pps <- as.numeric(data_vif_full$score_pps)
data_vif_full$dms_score <- as.numeric(data_vif_full$dms_score)

# Neue Variable mit Gruppenzugehörigkeit (als Faktor)
data_vif_full <- data_vif_full %>%
  mutate(activity_pattern = case_when(
    score_pps < 3 & dms_score == 2 ~ "FAR",
    score_pps >= 3 & dms_score == 2 ~ "DER",
    score_pps >= 3 & dms_score < 2 ~ "EER",
    score_pps < 3 & dms_score < 2 ~ "AR",
    TRUE ~ NA_character_  # Falls keine Bedingung zutrifft
  )) %>%
  mutate(activity_pattern = factor(activity_pattern,
                                   levels = c("AR", "EER", "DER", "FAR")))

data_vif_full$activity_group_code <- as.numeric(data_vif_full$activity_pattern)
table(data_vif_full$activity_pattern, useNA = "ifany")

# 6. Nur numerische Variablen imputieren
data_vif_full <- data_vif_full %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Modell für ndiscore

# Setze Referenzgruppe, z. B. FAR
data_vif_full$activity_pattern <- factor(data_vif_full$activity_pattern, levels = c("FAR", "DER", "EER", "AR"))

# Lineares Modell mit Faktorvariable
vif_model_ndi <- lm(
  ndiscore ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
    activity_pattern + pain_avg_acute + 
    stress_skala1_sum + stress_skala2_sum + stress_skala_symptome_sum,
  data = data_vif_full
)


# VIF-Werte ausgeben
vif(vif_model_ndi)

# Modell für pain_avg_acute
# Falls noch nicht geschehen: Referenzgruppe festlegen
data_vif_full$activity_pattern <- factor(data_vif_full$activity_pattern, 
                                         levels = c("FAR", "DER", "EER", "AR"))  # Oder eine andere Reihenfolge

# Modell mit Gruppenfaktor
vif_model_pain <- lm(
  pain_avg_acute ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
    activity_pattern +
    stress_skala1_sum + stress_skala2_sum + stress_skala_symptome_sum,
  data = data_vif_full
)

# Ergebnisse ansehen
summary(vif_model_pain)

# VIF-Werte ausgeben
vif(vif_model_pain)

# Visualisierung der Multikollinearität
ggpairs(data_vif_full %>% select(stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum))

#zu starke Korrelation bei Skala 1 und 2:
vif_model_ndi_reduced <- lm(
  ndiscore ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
    activity_pattern + pain_avg_acute +
    stress_skala1_sum + stress_skala_symptome_sum,
  data = data_vif_full
)

# VIF-Werte neu berechnen
vif(vif_model_ndi_reduced)
# Visualisierung der Multikollinearität
ggpairs(data_vif_full %>% select(stress_skala1_sum, stress_skala_symptome_sum))


###################Normalverteilung prüfen

#Disability
lmm_ndi <- lmer(ndiscore ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
    activity_pattern + pain_avg_acute +
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_full)


# Modellgrafiken für Diagnose
check_model(lmm_ndi)


# Residuen extrahieren
resid_ndi <- resid(lmm_ndi)


#Pain
lmm_pain <- lmer(pain_avg_3months ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
                   activity_pattern + pain_avg_acute +
                   stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
                 data = data_vif_full)


# Modellgrafiken für Diagnose
check_model(lmm_pain)

# Residuen extrahieren
resid_pain <- resid(lmm_pain)
# Shapiro-Wilk-Test
shapiro.test(resid_pain)


#Modelle prüfen

print(lmm_ndi)
check_model(lmm_ndi, check = c("linearity", "normality", "homogeneity", "outliers"))
check_collinearity(lmm_ndi)
performance::check_predictions(lmm_ndi)


check_model(lmm_pain)
check_model(lmm_pain, check = c("linearity", "normality", "homogeneity", "outliers"))
check_collinearity(lmm_pain)
performance::check_predictions(lmm_pain)



###############################Verteilung der outcomes prüfen
# 1A: Verteilung NDI anzeigen
table(data_ndi_3months$ndiscore)
barplot(table(data_ndi_3months$ndiscore))

# 1B: Wieviele einzigartige Werte?
length(unique(data_ndi_3months$ndiscore))

# 2A: Verteilung pain intensity anzeigen
table(data_pain_3months$pain_avg_3months)
barplot(table(data_pain_3months$pain_avg_3month))

# 2B: Wieviele einzigartige Werte?
length(unique(data_pain_3months$pain_avg_3months))


###########################logistische Regression für NDI gewählt aufgrund ordinaler Werte der outcomes

 
data_vif_scaled <- data_vif_scaled %>%
  mutate(activity_pattern = case_when(
    score_pps < 3 & dms_score == 2 ~ "FAR",
    score_pps >= 3 & dms_score == 2 ~ "DER",
    score_pps >= 3 & dms_score < 2 ~ "EER",
    score_pps < 3 & dms_score < 2 ~ "AR"
  )) %>%
  

#####AR als Referenz
data_vif_scaled$activity_pattern <- factor(data_vif_scaled$activity_pattern)
data_vif_scaled$activity_pattern <- relevel(data_vif_scaled$activity_pattern, ref = "AR")
#zero inflated model
brm_ndi_zi <- brm(
  ndiscore_clean ~ age + gender + cortisol1 + cortisone1 + activity_pattern +
    dass21_depression + pain_avg_acute + stress_skala1_sum + 
    stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_scaled,
  family = zero_inflated_negbinomial(),
  chains = 2, 
  iter = 2000, 
  cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = TRUE)
)


#output zero inflated ansehen:
summary(brm_ndi_zi)
bayes_R2(brm_ndi_zi)
pp_check(brm_ndi_zi)
fixef(brm_ndi_zi)




#Residuen ansehen:

# 1. Residuen extrahieren
resid_ndi <- residuals(brm_ndi_clean)
head(resid_ndi)

# 2. Histogramm der Residuen
hist(resid_ndi[, "Estimate"], breaks = 30, 
     main = "Verteilung der Residuen", 
     col = "lightblue", xlab = "Residuen")

# 3. Fitted Values extrahieren
fitted_vals <- fitted(brm_ndi_clean)[, "Estimate"]

# 4. Scatterplot: Residuen vs. Vorhersagen
plot(fitted_vals, resid_ndi[, "Estimate"],
     xlab = "Vorhergesagte Werte", 
     ylab = "Residuen", 
     main = "Residuen vs. Vorhersagen",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lwd = 2)



################Pain: 

# 1. pain_avg_3months sauber vorbereiten
data_pain_3months <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric)) %>%
  mutate(pain_avg_3months = round(rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE), 1)) %>%
  select(study_id, pain_avg_3months)


# 3. NEUEN Schmerz-Wert joinen
data_vif_scaled <- data_vif_scaled %>%
  left_join(data_pain_3months, by = "study_id")

# 4. Optional: NA-Prüfung und sauberen Datensatz erstellen
data_vif_scaled_clean <- data_vif_scaled %>%
  filter(!is.na(pain_avg_3months))

# Kontrolle:
summary(data_vif_scaled_clean$pain_avg_3months)
sum(is.na(data_vif_scaled_clean))  # Sollte 0 sein

# Nur vollständige Zeilen behalten – für ALLE Modellvariablen
data_vif_scaled_clean <- data_vif_scaled %>%
  filter(
    !is.na(pain_avg_3months),
    !is.na(age),
    !is.na(gender),
    !is.na(cortisol1),
    !is.na(cortisone1),
    !is.na(score_pps),
    !is.na(dms_score),
    !is.na(dass21_depression),
    !is.na(pain_avg_acute),
    !is.na(stress_skala1_sum),
    !is.na(stress_skala_symptome_sum)
  )



# Nur Modellvariablen prüfen:
sum(is.na(data_vif_scaled_clean[, c(
  "pain_avg_3months", "age", "gender", "cortisol1", "cortisone1", "activity_pattern", "dass21_depression", "pain_avg_acute",
  "stress_skala1_sum", "stress_skala_symptome_sum")]))


#AR als Referenz
data_vif_scaled_clean$activity_pattern <- factor(data_vif_scaled_clean$activity_pattern)
data_vif_scaled_clean$activity_pattern <- relevel(data_vif_scaled_clean$activity_pattern, ref = "AR")

brm_pain_test <- brm(
  pain_avg_3months ~ age + gender + cortisol1 + cortisone1 + 
    activity_pattern + dass21_depression + pain_avg_acute + 
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_scaled_clean,  
  family = student(),  
  chains = 2, iter = 2000, warmup = 500, cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = TRUE)
)

pp_check(brm_pain_test)
summary(brm_pain_test)
bayes_R2(brm_pain_test)
fixef(brm_pain_test)




##########################Plots:

# Effektplots (automatisch generiert für alle Prädiktoren)
plot(marginal_effects(brm_ndi_zi), points = TRUE)
plot(marginal_effects(brm_pain_test), points = TRUE)

# Modellgüte vergleichen mit tatsächlichen Daten
pp_check(brm_ndi_zi, ndraws = 100)
pp_check(brm_pain_test, ndraws = 100)


# Übersicht aller geschätzten Effekte
plot_model(brm_ndi_zi, type = "est", show.values = TRUE, sort.est = TRUE, title = "Effects on Disability")
plot_model(brm_pain_test, type = "est", show.values = TRUE, sort.est = TRUE, title = "Effects on pain intensity")


# Beispiel: Nur ausgewählte Variablen
mcmc_areas(as.array(brm_ndi_zi), pars = c("b_pain_avg_acute", "b_stress_skala1_sum", "b_dass21_depression"))

mcmc_areas(as.array(brm_pain_test), pars = c("b_dass21_depression", "b_stress_skala_symptome_sum", "b_pain_avg_acute"))

# NDI-Modell
resid_ndi <- residuals(brm_ndi_zi)[, "Estimate"]
fitted_ndi <- fitted(brm_ndi_zi)[, "Estimate"]

plot(fitted_ndi, resid_ndi,
     xlab = "Vorhergesagte Werte", ylab = "Residuen",
     main = "NDI: Residuen vs. Vorhersagen")
abline(h = 0, col = "red")

# Schmerzmodell
resid_pain <- residuals(brm_pain_test)[, "Estimate"]
fitted_pain <- fitted(brm_pain_test)[, "Estimate"]

plot(fitted_pain, resid_pain,
     xlab = "Vorhergesagte Werte", ylab = "Residuen",
     main = "Schmerz: Residuen vs. Vorhersagen")
abline(h = 0, col = "red")

#Regression coefficients:

#Disability
# Nur die Fixeffekte extrahieren
posterior <- as.array(brm_ndi_zi)

# Plot: Posterior Intervals der Regression Coefficients
mcmc_areas(posterior, 
           pars = c("b_age", "b_gender2", "b_cortisol1", "b_cortisone1", 
                    "b_score_pps", "b_dms_score", "b_dass21_depression", 
                    "b_pain_avg_acute", "b_stress_skala1_sum", 
                    "b_stress_skala_symptome_sum"),
           prob = 0.95) +
  ggtitle("Posterior distribution of the regression coefficients (Disability)")



# Geordnete Darstellung der Regressionskoeffizienten
plot_model(brm_ndi_zi, 
           type = "est", 
           show.values = TRUE, 
           value.offset = 0.3,
           title = "Fixed effects for Disability",
           sort.est = TRUE)  # <- diese Zeile sortiert die Effekte


#Pain intensity
# Nur die Fixeffekte extrahieren
posterior <- as.array(brm_pain_test)

# Plot: Posterior Intervals der Regression Coefficients
mcmc_areas(posterior,
           pars = c("b_age", "b_gender2", "b_cortisol1", "b_cortisone1",
                    "b_score_pps", "b_dms_score", "b_dass21_depression",
                    "b_pain_avg_acute", "b_stress_skala1_sum",
                    "b_stress_skala_symptome_sum"),
           prob = 0.95) +
  ggtitle("Posterior distribution of the regression coefficients (Pain intensity)")

# Plot Fixeffekte mit Konfidenzintervallen
plot_model(brm_pain_test,
           type = "est",
           show.values = TRUE,
           value.offset = 0.3,
           title = "Fixed effects for Pain intensity")

# Geordnete Darstellung der Regressionskoeffizienten
plot_model(brm_pain_test,
           type = "est",
           show.values = TRUE,
           value.offset = 0.3,
           title = "Fixed effects for Pain intensity",
           sort.est = TRUE)  # <- diese Zeile sortiert die Effekte
