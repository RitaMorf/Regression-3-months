

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
                    "pps7_strong","aefs_dms1","aefs_dms2","score_pps","dms_score","dass21_depression","age", "gender")

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
                     "pps7_strong","aefs_dms1","aefs_dms2","score_pps","dms_score","dass21_depression", "age","gender")

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
# Überprüfung der Datentypen
str(data_pain_acute$paindetect1)
str(data_pain_acute$paindetect2)
str(data_pain_acute$paindetect3)

str(data_pain_3months$paindetect1)
str(data_pain_3months$paindetect2)
str(data_pain_3months$paindetect3)

# Nicht-numerische Werte (wie "NA" als Text) in echte NA umwandeln
data_pain_acute <- data %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), ~na_if(., "NA"))) %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), ~na_if(., "."))) %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric))

data_pain_3months <- data %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), ~na_if(., "NA"))) %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), ~na_if(., "."))) %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric))


# Sicherstellen, dass die Spalten numerisch sind
# 1. Umwandlung in numeric sicherstellen
data_pain_acute <- data_pain_acute %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric))

data_pain_3months <- data_pain_3months %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric))

# Überprüfen, ob die Umwandlung erfolgreich war
str(data_pain_acute$paindetect1)
str(data_pain_acute$paindetect2)
str(data_pain_acute$paindetect3)

str(data_pain_3months$paindetect1)
str(data_pain_3months$paindetect2)
str(data_pain_3months$paindetect3)
# Überprüfen, ob die Spalten NA-Werte enthalten
colSums(is.na(data_pain_acute[, c("paindetect1", "paindetect2", "paindetect3")]))
colSums(is.na(data_pain_3months[, c("paindetect1", "paindetect2", "paindetect3")]))

#Rundung auf eine Nachkommastelle
data_pain_acute <- data_pain_acute %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3),
                ~ round(as.numeric(.), 1)))

data_pain_3months <- data_pain_3months %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3),
                ~ round(as.numeric(.), 1)))
# Überprüfen, ob die Rundung erfolgreich war
str(data_pain_acute$paindetect1)
str(data_pain_acute$paindetect2)
str(data_pain_acute$paindetect3)

# 2. Mittelwert berechnen
data_pain_acute <- data_pain_acute %>%
  mutate(pain_avg_acute = rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE))

data_pain_3months <- data_pain_3months %>%
  mutate(pain_avg_3months = rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE))


# Überprüfen, ob die neue Spalte korrekt erstellt wurde
head(data_pain_acute$pain_avg_acute)
head(data_pain_3months$pain_avg_3months)


################################Umcodierung SCI Werte
# Sicherstellen, dass alle relevanten Spalten numerisch sind
stress_vars <- c("sci_stress1_1", "sci_stress1_2", "sci_stress1_3", "sci_stress1_4",
                 "sci_stress1_5", "sci_stress1_6", "sci_stress1_7", "sci_stress2_1", 
                 "sci_stress2_2", "sci_stress2_3", "sci_stress2_4", "sci_stress2_5", 
                 "sci_stress2_6", "sci_symtpom1", "sci_symtpom2", "sci_symtpom3", 
                 "sci_symtpom4", "sci_symtpom5", "sci_symtpom6", "sci_symtpom7", 
                 "sci_symtpom8", "sci_symtpom9", "sci_symtpom10", "sci_symtpom11", 
                 "sci_symtpom12", "sci_symtpom13")

data[stress_vars] <- lapply(data[stress_vars], as.numeric)

data[stress_vars] <- lapply(data[stress_vars], function(x) {
  x <- as.character(x)  # Falls es Faktoren gibt, in Zeichenketten umwandeln
  x[x == "NA" | x == "." | x == ""] <- NA  # Ersetze problematische Werte durch NA
  return(as.numeric(x))  # Endgültige Umwandlung in numerische Werte
})

# Umkodierungsfunktion für fälschlicherweise codierte Fragen
umkodieren <- function(x, max_value) {
  return ((max_value - 1) * (x - 0) / (max_value - 0) + 1)
}

# Neue Spalten mit umkodierten Werten erstellen
for (i in 1:7) {
  data[[paste0("sci_stress1_", i, "_new")]] <- umkodieren(data[[paste0("sci_stress1_", i)]], 7)
}

for (i in 1:6) {
  data[[paste0("sci_stress2_", i, "_new")]] <- umkodieren(data[[paste0("sci_stress2_", i)]], 6)
}

for (i in 1:13) {
  data[[paste0("sci_symtpom", i, "_new")]] <- umkodieren(data[[paste0("sci_symtpom", i)]], 3)
}

# Erstelle einen Vektor mit den Spaltennamen zur Berechnung der Summen
spalten_zum_summieren <- paste0("sci_stress1_", 1:7, "_new")
spalten_zum_summieren_2 <- paste0("sci_stress2_", 1:6, "_new")
spalten_zum_summieren_3 <- paste0("sci_symtpom", 1:13, "_new")

# Berechne die Summen der einzelnen Stressskalen für jeden Probanden
data$stress_skala1_sum <- rowSums(data[, spalten_zum_summieren], na.rm = TRUE)
data$stress_skala2_sum <- rowSums(data[, spalten_zum_summieren_2], na.rm = TRUE)
data$stress_skala_symptome_sum <- rowSums(data[, spalten_zum_summieren_3], na.rm = TRUE)

#neue Filterung nach Umberechnung
data_stress_sums <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  select(study_id, stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum)


################Pearson Korrelation zw. subj. & obj. Stressmessung:
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


# Modell für ndiscore
vif_model_ndi <- lm(
  ndiscore ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
    score_pps + dms_score + pain_avg_acute + 
    stress_skala1_sum + stress_skala2_sum + stress_skala_symptome_sum,
  data = data_vif_full
)

# VIF-Werte ausgeben
vif(vif_model_ndi)

# Modell für pain_avg_acute
vif_model_pain <- lm(
  pain_avg_acute ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
    score_pps + dms_score +
    stress_skala1_sum + stress_skala2_sum + stress_skala_symptome_sum,
  data = data_vif_full
)

# VIF-Werte ausgeben
vif(vif_model_pain)

# Visualisierung der Multikollinearität
ggpairs(data_vif_full %>% select(stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum))

#zu starke Korrelation bei Skala 1 und 2:
vif_model_ndi_reduced <- lm(
  ndiscore ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
    score_pps + dms_score + pain_avg_acute +
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
    score_pps + dms_score + pain_avg_acute +
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_full)


# Modellgrafiken für Diagnose
check_model(lmm_ndi)


# Residuen extrahieren
resid_ndi <- resid(lmm_ndi)

# Shapiro-Wilk-Test
shapiro.test(resid_ndi)

#Pain
lmm_pain <- lmer(pain_avg_3months ~ age + gender + dass21_depression + cortisol1 + cortisone1 +
                   score_pps + dms_score + pain_avg_acute +
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

# Schmerzen nach 3 Monaten (T2), korrekt runden:
data_pain_3months <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric)) %>%
  mutate(
    pain_avg_3months = round(rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE), 1)
  ) %>%
  select(study_id, pain_avg_3months)

data_pain_acute <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric)) %>%
  mutate(
    pain_avg_acute = round(rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE), 1)
  ) %>%
  select(study_id, pain_avg_acute)

table(data_pain_3months$pain_avg_3months)

###########################logistische Regression für NDI gewählt aufgrund ordinaler Werte der outcomes

# ndiscore sauber auf ganze Zahlen runden
data_vif_full <- data_vif_full %>%
  mutate(ndiscore_clean = round(as.numeric(ndiscore)))


# Daten vorbereiten:Z-Standardisierung (sorgt dafür, dass alle numerischen Variablen Mittelwert 
#0 und Standardabweichung 1 haben.)


data_vif_scaled <- data_vif_full %>%
  mutate(gender = as.factor(gender)  ) %>% # falls noch nicht
 
  mutate(across(
    c(age, cortisol1, cortisone1, dass21_depression, score_pps,
      dms_score, pain_avg_acute, stress_skala1_sum, stress_skala_symptome_sum),
    ~ scale(as.numeric(.))[, 1]  )) # Z-Standardisierung
 

#zero inflated model

brm_ndi_zi <- brm(
  ndiscore_clean ~ age + gender + cortisol1 + cortisone1 + score_pps + 
    dms_score + dass21_depression + pain_avg_acute + stress_skala1_sum + 
    stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_scaled,
  family = zero_inflated_negbinomial(),
  chains = 2, 
  iter = 2000, 
  cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = TRUE))

saveRDS(brm_ndi_zi, "brm_ndi_zi.rds")

#output zero inflated ansehen:
summary(brm_ndi_zi)
bayes_R2(brm_ndi_zi)
pp_check(brm_ndi_zi)
fixef(brm_ndi_zi)



##Ordinales Modell = Ordinal kontinuierlich anschauen (family = student)

# Ordinales Modell: kontinuierlich
brm_ndi_ordinal <- brm(
  ndiscore_clean ~ age + gender + cortisol1 + cortisone1 + score_pps +
    dms_score + dass21_depression + pain_avg_acute + 
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_scaled,
  family = student (),
  chains = 2, 
  iter = 2000, 
  warmup = 500, 
  cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = FALSE))  # Speicheroptimierung


#output ordinal ansehen:
summary(brm_ndi_ordinal)
pp_check(brm_ndi_ordinal)
fixef(brm_ndi_ordinal)
r2(brm_ndi_ordinal)

#Modell nimmt negative WErte im NDI an, Korrektur mit trunktiertem Modell 
#(so dass der Outcome nur ≥0 sein darf):

#NA's rauslöschen damit das MOdell rechnen kann:
colSums(is.na(data_vif_scaled[, c(
  "ndiscore_clean", "age", "gender", "cortisol1", "cortisone1", "score_pps",
  "dms_score", "dass21_depression", "pain_avg_acute",
  "stress_skala1_sum", "stress_skala_symptome_sum"
)]))

data_model <- data_vif_scaled %>%
  drop_na(ndiscore_clean, age, gender, cortisol1, cortisone1, score_pps,
          dms_score, dass21_depression, pain_avg_acute,
          stress_skala1_sum, stress_skala_symptome_sum)

# Welche Variablen sind keine echten numerischen Variablen?
sapply(data_vif_scaled, class)
data_vif_scaled <- data_vif_scaled %>%
  mutate(across(
    c(ndiscore_clean, age, cortisol1, cortisone1, score_pps, dms_score,
      dass21_depression, pain_avg_acute, stress_skala1_sum, stress_skala_symptome_sum),
    ~ as.numeric(as.character(.))))

# Sicherstellen, dass alle Variablen numerisch sind
data_model <- data_vif_scaled %>%
  filter(
    !is.na(ndiscore_clean) &
      !is.na(age) &
      !is.na(gender) &
      !is.na(cortisol1) &
      !is.na(cortisone1) &
      !is.na(score_pps) &
      !is.na(dms_score) &
      !is.na(dass21_depression) &
      !is.na(pain_avg_acute) &
      !is.na(stress_skala1_sum) &
      !is.na(stress_skala_symptome_sum))

#überprüfen
sum(is.na(data_model))  # Sollte jetzt 0 sein
preds <- posterior_predict(brm_ndi_clean)
sum(is.na(preds))  # Sollte 0 sein


#Modell neu rechnen:

brm_ndi_clean <- brm(
  ndiscore_clean ~ age + gender + cortisol1 + cortisone1 + score_pps +
    dms_score + dass21_depression + pain_avg_acute +
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_model,
  family = student(),
  chains = 2, iter = 2000, warmup = 500, cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = TRUE))

# Schritt A: Vorhersagen erzeugen
preds <- posterior_predict(brm_ndi_clean)

# Schritt B: Check auf NA
sum(is.na(preds))  # Sollte 0 sein


#output ansehen:
summary(brm_ndi_clean)
pp_check(brm_ndi_clean)
fixef(brm_ndi_clean)
r2(brm_ndi_clean)

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



#Modellvergleich
#Leave-One-Out Cross Validation (mit moment matching)
loo_trunc <- loo(brm_ndi_clean, moment_match = TRUE)
loo_zi <- loo(brm_ndi_zi, moment_match = TRUE)
loo_compare(loo_trunc, loo_zi)







################neuer Versuch für Pain: 23.05.25

# 1. NEU: pain_avg_3months sauber vorbereiten
data_pain_3months <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  mutate(across(c(paindetect1, paindetect2, paindetect3), as.numeric)) %>%
  mutate(pain_avg_3months = round(rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE), 1)) %>%
  select(study_id, pain_avg_3months)

# 2. Vorhandene pain_avg_3months-Spalten im Hauptdatensatz löschen (falls nötig)
data_vif_scaled <- data_vif_scaled %>%
  select(-starts_with("pain_avg_3months"))

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

# Kontrolle: Jetzt sollte alles NA-frei sein
sum(is.na(data_vif_scaled_clean))  # → Sollte 0 sein


# Nur Modellvariablen prüfen:
sum(is.na(data_vif_scaled_clean[, c(
  "pain_avg_3months", "age", "gender", "cortisol1", "cortisone1", "score_pps",
  "dms_score", "dass21_depression", "pain_avg_acute",
  "stress_skala1_sum", "stress_skala_symptome_sum")]))


brm_pain_trunc <- brm(
  formula = pain_avg_3months | trunc(lb = 0) ~ age + gender + cortisol1 + cortisone1 + 
    score_pps + dms_score + dass21_depression + pain_avg_acute + 
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_scaled_clean,
  family = student(),
  chains = 2,
  iter = 2000,
  warmup = 500,
  cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = TRUE)
)

summary(brm_pain_trunc)
pp_check(brm_pain_trunc)
bayes_R2(brm_pain_trunc)
fixef(brm_pain_trunc)

#Trunkierung macht instabile Vorhersagen: posterior predictive check geht nicht;

# Prüfe zuerst, wie dein Modell heißt
brm_pain_trunc$data  # Gibt den internen Datensatz aus

# Extrahiere den Datensatz aus dem Modell selbst (das ist narrensicher)
data_used <- brm_pain_trunc$data

sum(is.na(data_used))  # Muss 0 sein

pp_check(brm_pain_trunc, newdata = data_used)

preds <- posterior_predict(brm_pain_trunc)
sum(is.na(preds))
#gibt 10904 NA's aus: kann nicht sein: darum anderes MOdell:

brm_pain_test <- brm(
  pain_avg_3months ~ age + gender + cortisol1 + cortisone1 + 
    score_pps + dms_score + dass21_depression + pain_avg_acute + 
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_used,  # Der geprüfte NA-freie Datensatz
  family = student(),  # Kein trunc()!
  chains = 2, iter = 2000, warmup = 500, cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = TRUE)
)

pp_check(brm_pain_test)
summary(brm_pain_test)
bayes_R2(brm_pain_test)
fixef(brm_pain_test)


#zero inflated Model anschauen:
#Achtung: Dieses Modell verlangt Ganzzahlen (integer), darum runden:

# Neue gerundete Variable erstellen
data_vif_scaled_clean$pain_avg_3months_int <- round(data_vif_scaled_clean$pain_avg_3months)

# Dann das Modell rechnen
brm_pain_zi <- brm(
  formula = pain_avg_3months_int ~ age + gender + cortisol1 + cortisone1 + 
    score_pps + dms_score + dass21_depression + pain_avg_acute + 
    stress_skala1_sum + stress_skala_symptome_sum + (1 | study_id),
  data = data_vif_scaled_clean,
  family = zero_inflated_negbinomial(),
  chains = 2,
  iter = 2000,
  warmup = 500,
  cores = 2,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  save_pars = save_pars(all = TRUE)
)

summary(brm_pain_zi)
pp_check(brm_pain_zi)
bayes_R2(brm_pain_zi)
fixef(brm_pain_zi)



#Modellvergleich
loo_ord <- loo(brm_pain_test, moment_match = TRUE)
loo_zi <- loo(brm_pain_zi, moment_match = TRUE)
loo_compare(loo_ord, loo_zi)

#Residuen:
# 1. Residuen extrahieren
resid_pain <- residuals(brm_pain_zi)
head(resid_pain)
# 2. Histogramm der Residuen
hist(resid_pain[, "Estimate"], breaks = 30, 
     main = "Verteilung der Residuen", 
     col = "lightblue", xlab = "Residuen")
# 3. Fitted Values extrahieren
fitted_vals_pain <- fitted(brm_pain_zi)[, "Estimate"]
# 4. Scatterplot: Residuen vs. Vorhersagen
plot(fitted_vals_pain, resid_pain[, "Estimate"],
     xlab = "Vorhergesagte Werte", 
     ylab = "Residuen", 
     main = "Residuen vs. Vorhersagen",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lwd = 2)




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

library(bayesplot)

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
