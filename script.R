# Setting up the environment
rm(list=ls()) #Remove all existing objects
setwd("C:/Users/siyan/research_data/research_mcs/") #Set the working directory
library(tidyverse) #Data Pre-processing
library(lavaan) #SEM
library(psych) #EDA

# Read into the data file
library(haven)
# df <- read_dta("processed211.dta")
load("sem211.RData")

# Check df
dim(df)
head(df, 5)

# save(df, file="sem211.RData")

# EDA

# List of variables
variables <- c("cimp5", "crisk5", "cimp6", "crisk6", "cimp7", "crisk7",
               "cpar4_dibn", "cpar4_ditr", "cpar4_dire", "cpar5_dibn", 
               "cpar5_ditr", "cpar5_dire", 
               "agr7_fig", "agr7_hit", "agr7_wepn", "agr7_srfig",
               "agr6_fig", "agr6_hit", "agr6_wepn", "agr6_hps", "agr6_hpc",
               "agr5_fig", "agr5_hps", "agr5_hpc",
               "gender6", "pared", 
               "race", "married")

# Loop through each variable and print its frequency table, including NA
for (var in variables) {
  cat("\nFrequency of", var, ":\n")
  print(table(df[[var]], useNA = "ifany"))
}

# Apply the function to each variable and combine results
freq_tables <- lapply(variables, frequency_table, data = df)
freq_table_combined <- do.call(rbind, freq_tables)

# Order the data frame by Variable
freq_table_combined <- freq_table_combined[order(freq_table_combined$Variable),]

# View the combined frequency table
print(freq_table_combined)

# Initialize a data frame to store the results
descriptive_stats <- data.frame(Variable = character(),
                                Mean = numeric(),
                                Median = numeric(),
                                SD = numeric(),
                                Min = numeric(),
                                Max = numeric(),
                                Missing = integer(),
                                stringsAsFactors = FALSE)

# Loop through each variable and calculate descriptive statistics
for (var in variables) {
  current_var <- df[[var]]
  
  # Calculating statistics
  mean_val <- mean(current_var, na.rm = TRUE)
  median_val <- median(current_var, na.rm = TRUE)
  sd_val <- sd(current_var, na.rm = TRUE)
  min_val <- min(current_var, na.rm = TRUE)
  max_val <- max(current_var, na.rm = TRUE)
  missing_val <- sum(is.na(current_var))
  
  # Adding the results to the dataframe
  descriptive_stats <- rbind(descriptive_stats, c(var, mean_val, median_val, sd_val, min_val, max_val, missing_val))
}

# Renaming columns appropriately
colnames(descriptive_stats) <- c("Variable", "Mean", "Median", "Standard Deviation", "Minimum", "Maximum", "Missing Values")


# Display the result
print(descriptive_stats)


# Preprocessing 

## Create a new data frame with only the specified variables
subset <- df[, variables]


## View the new data frame
head(subset)

# load the Hmisc package
library(Hmisc)

# Descriptive labels
variable_labels <- c("impulsivity age 11", "risk-taking age 11", "impulsivity age 14",
                     "risk-taking age 14", "impulsivity age 17", "risk-taking age 17", 
                     "parenting_bedroom age 7", "parenting_treats age 7", 
                     "parenting_reason age 7", "parenting_bedroom age 11",
                     "parenting_treats age 11", "parenting_reason age 11",
                     "parents reported fight age 17", "push/shove others age 17",
                     "hit someone with a weapon age 17", 
                     "self-reported fight age 17",
                     "parents reported fight age 14", 
                     "push/shove others age 14",
                     "hit someone with a weapon age 14",
                     "hurt or pick on siblings age 14",
                     "hurt or pick on others age 14",
                     "self-reported fight age 11",
                     "hurt or pick on siblings age 11",
                     "hurt or pick on others age 11",
                     "CM Sex", "parents' education", "ethnicity", "parents' married")

# Corresponding variable names in the data frame
variable_names <- c("cimp5", "crisk5", "cimp6", "crisk6", "cimp7", "crisk7",
                    "cpar4_dibn", "cpar4_ditr", "cpar4_dire", "cpar5_dibn", 
                    "cpar5_ditr", "cpar5_dire", 
                    "agr7_fig", "agr7_hit", "agr7_wepn", "agr7_srfig",
                    "agr6_fig", "agr6_hit", "agr6_wepn", "agr6_hps", "agr6_hpc",
                    "agr5_fig", "agr5_hps", "agr5_hpc",
                    "gender6", "pared", 
                    "race", "married")

## Check for missing varaibles
missing_vars <- variable_names[!variable_names %in% names(subset)]
if (length(missing_vars) > 0) {
  cat("Missing variables in subset: ", paste(missing_vars, collapse = ", "), "\n")
}

missing_vars

# Assign labels to the variables in the subset data frame
for (i in 1:length(variable_names)) {
  label(subset[[variable_names[i]]]) <- variable_labels[i]
}


# recode
subset$agr7_fig <- ifelse(subset$agr7_fig == 1 | subset$agr7_fig == 2, 1, subset$agr7_fig)
subset$agr6_fig <- ifelse(subset$agr6_fig == 1 | subset$agr6_fig == 2, 1, subset$agr6_fig)
subset$agr5_fig <- ifelse(subset$agr5_fig == 1 | subset$agr5_fig == 2, 1, subset$agr5_fig)

# Check for class of all variables in variables
variable_classes <- sapply(subset, class)
print(variable_classes)


# Loop through each variable and calculate descriptive statistics
# Initialize an empty list to store the descriptive statistics
descriptive_stats_list <- list()

# Loop through each variable and calculate descriptive statistics
for (var in variables) {
  current_var <- subset[[var]]
  
  # Convert labelled variables to numeric
  if (inherits(current_var, "labelled")) {
    current_var_numeric <- as.numeric(as.character(current_var))
  } else {
    current_var_numeric <- current_var
  }
  
  # Calculating statistics
  mean_val <- mean(current_var_numeric, na.rm = TRUE)
  median_val <- median(current_var_numeric, na.rm = TRUE)
  sd_val <- sd(current_var_numeric, na.rm = TRUE)
  min_val <- min(current_var_numeric, na.rm = TRUE)
  max_val <- max(current_var_numeric, na.rm = TRUE)
  missing_val <- sum(is.na(current_var_numeric))
  
  # Store the results in the list
  descriptive_stats_list[[var]] <- c(mean = mean_val, median = median_val, sd = sd_val, min = min_val, max = max_val, missing = missing_val)
}

# Convert the list to a dataframe
descriptive_stats <- do.call(rbind, lapply(names(descriptive_stats_list), function(x) {
  cbind(variable = x, t(descriptive_stats_list[[x]]))
}))
rownames(descriptive_stats) <- NULL  # Clean up row names

# Print or return the descriptive_stats dataframe
print(descriptive_stats)


# Load the unlabelled subset
subset <- df[, variables]

## Correlation Matrix
sapply(subset, class)

subset$gender6 <- as.numeric(subset$gender6)
subset$race <- as.numeric(subset$race)


# Recode the 'race' variable: 0 for white (originally 1), 1 for others (originally 2, 3, and 4)
subset <- subset %>%
  mutate(race = ifelse(race == 1, 0, 1))

# Reverse coding cimp5 and crisk7 in the subset data frame
subset$cimp5 <- 6 - subset$cimp5
subset$crisk5 <- 8 - subset$crisk5
subset$cimp6 <- 6 - subset$cimp6
subset$crisk6 <- 8 - subset$crisk6
subset$cimp7 <- 6 - subset$cimp7
subset$crisk7 <- 8 - subset$crisk7

subset$sc5 <- subset$cimp5 + subset$crisk5
subset$sc6 <- subset$cimp6 + subset$crisk6
subset$sc7 <- subset$cimp7 + subset$crisk7

rcorr(as.matrix(subset))

# Visualize the correlation
# Install packages if necessary
if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")

# Load packages
library(Hmisc)
library(corrplot)

# Compute the correlation matrix. Note: rcorr(as.matrix(subset)) returns a list with the matrix at $r
cor_matrix <- rcorr(as.matrix(subset))$r

# Basic corrplot with hierarchical clustering and simplified color scheme
corrplot(cor_matrix, method = "color", 
         type = "upper", # Only show upper half to avoid redundancy
         order = "hclust", # Order variables to cluster highly correlated ones
         tl.col = "black", tl.srt = 45, # Adjust text color and rotation for readability
         cl.lim = c(-1, 1)) # Ensure color scale represents full range of correlations

# Display the modified dataframe
print(subset)

# Write to CSV
write.csv(subset, file = "subset211.csv", row.names = FALSE)



# Missing Pattern
library(naniar)
library(VIM)
library(mice)
library(Amelia)


# Missingness frequency by variable
n_miss <- miss_var_summary(subset)
print(n_miss)

#Frequencies of missing data pattern
md_pattern <- md.pattern(subset)
print(md_pattern)


missmap(subset, main = "Missing Data Pattern", 
        col = c("yellow", "black"), legend = TRUE)


# MCAR TEST (result significant, Not completely at random)
MCAR_result <- mcar_test(subset)
print(MCAR_result)

# Correlation analysis
vis_miss(subset)


# Model Specification
sem1 <- ' 
 # Measurement Model
 
 agr7 =~ agr7_fig + agr7_hit + agr7_wepn
 agr6 =~ agr6_fig + agr6_hit + agr6_wepn + agr5_hpc
 agr5 =~ agr5_fig + agr5_hpc 
 
 
 ## Self-control
 
 ## Self-control at age 11
 fsc5 =~ cimp5 + crisk5
 
 ## Self-control at age 14
 fsc6 =~ cimp6 + crisk6
   
 ## Self-control at age 17
 fsc7 =~ cimp7 + crisk7

   
 ## Parenting
 
 ## Parenting at age 7
 par4 =~ cpar4_dibn + cpar4_ditr + cpar4_dire
 
 ## Parenting at age 11
 par5 =~ cpar5_dibn + cpar5_ditr + cpar5_dire
   
   
 # Structural Model
 
 agr7 ~ fsc6 + agr6 + par5
 agr6 ~ fsc5 + agr5 + par5
 agr5 ~ par4 + gender6 + pared + race + married
   
 fsc7 ~ agr6 + fsc6 + par5
 fsc6 ~ agr5 + fsc5 + par5 
 fsc5 ~ par4 + gender6 + pared + race + married 
   
 par5 ~ par4  
 
## Covariances

 agr7 ~~ fsc7
 agr6 ~~ fsc6
 agr5 ~~ fsc5
 agr6_fig ~~ agr5_fig

 
 crisk6 ~~ crisk7
 crisk5 ~~ crisk6
 crisk5 ~~ crisk7
 
 cimp5 ~~ cimp6
 cimp6 ~~ cimp7
 cimp5 ~~ cimp7
 
 cpar4_dire ~~ cpar5_dire
 cpar4_ditr ~~ cpar5_ditr
 cpar4_dibn ~~ cpar5_dibn
 
 fsc5 ~~ par5
'


fit1 <- sem(model = sem1, data = subset, estimator="MLR")

fit2 <- sem(model = sem1, data = subset, estimator="MLR", missing="fiml", 
            fixed.x=FALSE)

summary(fit1, standardized = TRUE, fit.measures=TRUE)

summary(fit2, standardized = TRUE, fit.measures=TRUE)

modindices(fit1, sort = TRUE, maximum.number = 20)

library(lavaanPlot) # For plotting


## Label the subset
my_label <- list(
  par7 = "Parenting Age 7",
  par11 = "Parenting Age 11",
  fsc11 = "Self-control Age 11",
  fsc14 = "Self-control Age 14",
  fsc17 = "Self-control Age 17",
  cimp5 = "Impulsivity Age 11",
  crisk5 = "Risk-Taking Age 11",
  cimp6 = "Impulsivity Age 14",
  crisk6 = "Risk-Taking Age 14",
  cimp7 = "Impulsivity Age 17",
  crisk7 = "Risk-Taking Age 17",
  cpar4_dibn = "Parenting Bedroom Age 7",
  cpar4_ditr = "Parenting Treats Age 7",
  cpar4_dire = "Parenting Reason Age 7",
  cpar5_dibn = "Parenting Bedroom Age 11",
  cpar5_ditr = "Parenting Treats Age 11",
  cpar5_dire = "Parenting Reason Age 11",
  agr7 = "Aggression Age 17",
  agr6 = "Aggression Age 14",
  agr5 = "Aggression Age 11",
  gender6 = "Gender",
  pared = "Parents Education",
  race = "Ethnicity",
  married = "Parents Married Status")


# Conceptual Model
lavaanPlot(model = fit1, labels = my_label, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)

# WLSMVS Estimates
lavaanPlot(model = fit1, labels = my_label, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs=TRUE, stand = TRUE)

# MLR Estimates
lavaanPlot(model = fit2, labels = my_label, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs=TRUE, stand = TRUE)
