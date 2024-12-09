library(dplyr)
library(ggplot2)
library(tidyverse)
library(nlme)

# Simulated Data for Female Individuals
data.F <- read.csv("Data.F.csv", stringsAsFactors = T)

data.F$ID = as.factor(data.F$ID)
head(data.F)


# 
ggplot(data.F, aes(x = Age_months, y = HC, color = HCZ_HAZ)) +
  geom_point() + 
  labs(
    title = "",
    x = "Age (months)",
    y = "Head Circumference",
    color = "Z score at Birth"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

length(unique(data.F$ID)) # 160 individuals
Ids <- data.frame(table(data.F$ID))
table(Ids$Freq) #Everyone has 4 measurements in time


# Individuals categorized by anthropometric status at birth, according to whether 
# they had a low or normal Z-score (z = -2 is the threshold) at birth in head circumference
# or body length

table(data.F$HCZ_HAZ) # 40 individuals for each category of growth statys at birth


#Models
n_Param = 4

#Fem
# Head circumference
Count.HC_F = nlme(HC ~ A + B * (Age_months) + C * log(Age_months + 1),
                  data = data.F,
                  fixed = A + B + C ~ HCZ_HAZ,
                  random = A + B + C ~ 1 | ID,
                  start = c(A = rep(33, n_Param), 
                            B = rep(0.14, n_Param), 
                            C = rep(4, n_Param)),
                  correlation = corCAR1(form = ~ Age_months),
                  method = "ML",
                  control = list(pnlsTol = 0.01),
                  verbose = T)

data.F["ResHC"] = residuals(Count.HC_F, type = "pearson")
range(data.F$ResHC)
LimY = c(min(data.F$ResHC) - 0.5, max(data.F$ResHC) + 0.5)


# Residuals study
ggplot(data.F, aes(x = as.factor(floor(Age_months)), y = ResHC)) +
  geom_boxplot(aes(), color = "black", fill = "dark gray", outlier.size = 1) +
  theme_bw() + geom_hline(yintercept = 0, color = "orange", lwd=1.2) + ggtitle("") +
  ylab("Standarized Residuals") + xlab("Age (months)") + coord_cartesian(ylim = LimY) + theme_minimal()

ggplot(data.F, aes(x = HCZ_HAZ, y = ResHC)) +
  geom_boxplot(aes(), color = "black", fill = "dark gray", outlier.size = 1) +
  theme_bw() + geom_hline(yintercept = 0, color = "orange", lwd=1.2) + ggtitle("Count") +
  ylab("Standarized Residuals") + xlab("Age (months)") + coord_cartesian(ylim = LimY) + theme_minimal()


# Body length
Count.BL_F = nlme(BL ~ A + B * (Age_months) + C * log(Age_months + 1),
                  data = data.F,
                  fixed = A + B + C ~ HCZ_HAZ,
                  random = A + B + C ~ 1 | ID,
                  start = c(A = rep(33, n_Param), 
                            B = rep(0.14, n_Param), 
                            C = rep(4, n_Param)),
                  correlation = corCAR1(form = ~ Age_months),
                  method = "ML",
                  control = list(pnlsTol = 0.01),
                  verbose = T)

data.F["ResBL"] = residuals(Count.BL_F, type = "pearson")
range(data.F$ResBL)
LimY = c(min(data.F$ResBL) - 0.5, max(data.F$ResBL) + 0.5)


# Residuals study
ggplot(data.F, aes(x = as.factor(floor(Age_months)), y = ResBL)) +
  geom_boxplot(aes(), color = "black", fill = "dark gray", outlier.size = 1) +
  theme_bw() + geom_hline(yintercept = 0, color = "orange", lwd=1.2) + ggtitle("") +
  ylab("Standarized Residuals") + xlab("Age (months)") + coord_cartesian(ylim = LimY) + theme_minimal()

ggplot(data.F, aes(x = HCZ_HAZ, y = ResBL)) +
  geom_boxplot(aes(), color = "black", fill = "dark gray", outlier.size = 1) +
  theme_bw() + geom_hline(yintercept = 0, color = "orange", lwd=1.2) + ggtitle("Count") +
  ylab("Standarized Residuals") + xlab("Age (months)") + coord_cartesian(ylim = LimY) + theme_minimal()


# Both Curves: Calculate predicted values for the entire range, not just for observed levels
# 500 levels per category

# Number of prediction levels
PredictionLevels = 500

# Create a sequence of ages for prediction (500 points)
age_months = with(data.F, seq(min(Age_months), max(Age_months), length.out = PredictionLevels))

# Initialize data frames for females and males
BaseF = data.frame(matrix(ncol = 3, nrow = PredictionLevels * 4))
colnames(BaseF) = c("Age_months", "Sex", "HCZ_HAZ")

BaseF$Age_months = rep(age_months, 4)

# Assign sex
BaseF$Sex = "F"

# Define HAZ_HCZ levels from data.F
HCZ_HAZ_levels = levels(data.F$HCZ_HAZ)

# Assign the HAZ_HCZ levels
BaseF$HCZ_HAZ = rep(HCZ_HAZ_levels, each = PredictionLevels)

# Make predictions
BaseF$PredHC = predict(Count.HC_F, newdata = BaseF, level = 0)
BaseF$PredBL = predict(Count.BL_F, newdata = BaseF, level = 0)


# Plotting the curves
ggplot(BaseF, aes(Age_months)) +
  geom_point(aes(y = PredHC, color = HCZ_HAZ), size = 1.5) +
  ggtitle("Mean growth trajectory") +
  ylab("Head Circumference (cm)") + xlab("Age (months)")

ggplot(BaseF, aes(Age_months)) +
  geom_point(aes(y = PredBL, color = HCZ_HAZ), size = 1.5) +
  ggtitle("Mean growth trajectory") +
  ylab("Body Length (cm)") + xlab("Age (months)")



#Ratios
# Obtener las referencias de las mediciones de HC y BL para las categorías según el sexo
Ref.F.HC <- (BaseF %>% filter(HCZ_HAZ == "norHC_norL"))$PredHC

Ref.F.BL <- (BaseF%>% filter(HCZ_HAZ == "norHC_norL"))$PredBL

# Calcular los ratios para HC y BL
BaseF$HC_Ratio <- BaseF$PredHC / Ref.F.HC

BaseF$BL_Ratio <- BaseF$PredBL / Ref.F.BL

# Graficar los ratios de crecimiento de HC y BL
ggplot(BaseF, aes(Age_months)) +
  geom_point(aes(y = HC_Ratio, color = HCZ_HAZ), size = 1.5) +
  ggtitle("Growth Trajectory of HC to norHC_norL") +
  ylab("Head Circumference (Ratio)") + xlab("Age (months)")

ggplot(BaseF, aes(Age_months)) +
  geom_point(aes(y = BL_Ratio, color = HCZ_HAZ), size = 1.5) +
  ggtitle("Growth Trajectory of HC to norHC_norL") +
  ylab("Head Circumference (Ratio)") + xlab("Age (months)")
