# Setting up the environment
rm(list=ls()) #Remove all existing objects
setwd("C:/Users/siyan/research_data/research_mcs/") #Set the working directory
library(tidyverse) #Data Pre-processing
library(lavaan) #SEM
library(psych) #EDA

# Read into the data file
library(haven) # Process foreign data
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

frequency_table <- function(var, data) {
  # Create frequency table with NA if any
  freq <- table(data[[var]], useNA = "ifany")
  
  # Convert to data frame
  freq_df <- as.data.frame(freq)
  
  # Rename columns
  names(freq_df) <- c("Value", "Frequency")
  
  # Calculate percentage (excluding NAs for percentage calculation)
  total <- sum(freq_df$Frequency[!is.na(freq_df$Value)])
  freq_df$Percentage <- round((freq_df$Frequency / total) * 100, 2)
  
  # Add a header row for the variable name, making the rest of the data align
  freq_df <- rbind(data.frame(Value = var, Frequency = NA, Percentage = NA), freq_df)
  
  return(freq_df)
}

# Combine the table
freq_tables <- lapply(variables, frequency_table, data = df)
freq_table_combined <- do.call(rbind, freq_tables)
# No need to order here since each table starts with the variable name
# View the combined frequency table
print(freq_table_combined)

# Print out a neat frequency table
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
knitr::kable(freq_table_combined, row.names = FALSE)




## Look at the descriptive statistics of the variables
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

# load the Hmisc package for preprocessing
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


# Binary recode fight variable 
#subset$agr7_fig <- ifelse(subset$agr7_fig == 1 | subset$agr7_fig == 2, 1, subset$agr7_fig)
#subset$agr6_fig <- ifelse(subset$agr6_fig == 1 | subset$agr6_fig == 2, 1, subset$agr6_fig)
#subset$agr5_fig <- ifelse(subset$agr5_fig == 1 | subset$agr5_fig == 2, 1, subset$agr5_fig)




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
  mean_val <- round(mean(current_var_numeric, na.rm = TRUE), 3)
  median_val <- median(current_var_numeric, na.rm = TRUE)  # Median typically not rounded, but you can if desired
  sd_val <- round(sd(current_var_numeric, na.rm = TRUE), 3)
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


#sapply(subset, class)


## Recode the two haven_labelled variable as numeric
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

# Create the composite self-control variable
# subset$sc5 <- subset$cimp5 + subset$crisk5
# subset$sc6 <- subset$cimp6 + subset$crisk6
# subset$sc7 <- subset$cimp7 + subset$crisk7


# Check for class of all variables in variables
## Note: They need to be all numeric
variable_classes <- sapply(subset, class)
print(variable_classes)





# Bivariate-level Correlation
cor_matrix <- cor(subset, use = "complete.obs")
rounded_cor_matrix <- round(cor_matrix, 2)

## Get the number of rows (or columns, since it's square) of the matrix
n <- nrow(rounded_cor_matrix)

## Set the lower triangle, including the diagonal, to NA
rounded_cor_matrix[lower.tri(rounded_cor_matrix, diag = TRUE)] <- NA
print(rounded_cor_matrix)

# Outout to excel
library(openxlsx) ## load the package needed
## Create a new workbook
wb <- createWorkbook()
# Add a worksheet to the workbook
addWorksheet(wb, "Upper Half Correlation Matrix")
# Write the rounded correlation matrix to the worksheet
writeData(wb, sheet = "Upper Half Correlation Matrix", x = rounded_cor_matrix)

# Optionally, you can also auto-size the column widths for better readability
setColWidths(wb, sheet = 1, cols = 1:ncol(rounded_cor_matrix), widths = "auto")

# Save the workbook to your working directory
saveWorkbook(wb, "Upper_Half_Correlation_Matrix.xlsx", overwrite = TRUE)



# Visualize the correlation
# Install packages if necessary
if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")

# Load packages (assuming Hmisc is loaded)
library(corrplot)

# Compute the correlation matrix. Note: rcorr(as.matrix(subset)) returns a list with the matrix at $r
cor_matrix <- rcorr(as.matrix(subset))$r

# Basic corrplot with hierarchical clustering and simplified color scheme
corrplot(cor_matrix, method = "color", 
         type = "upper", # Only show upper half to avoid redundancy
         order = "hclust", # Order variables to cluster highly correlated ones
         tl.col = "black", tl.srt = 45, # Adjust text color and rotation for readability
         cl.lim = c(-1, 1)) # Ensure color scale represents full range of correlations

# Write to CSV
write.csv(subset, file = "subset211.csv", row.names = FALSE)





# Missing Pattern
library(naniar)
library(VIM)
library(mice)
library(Amelia)


# Missingness frequency by variable
n_miss <- miss_var_summary(subset)
print(n_miss, n=40)

#Frequencies of missing data pattern
md_pattern <- md.pattern(subset)
print(md_pattern)


# missmap(subset, main = "Missing Data Pattern", 
        #col = c("yellow", "black"), legend = TRUE)


# MCAR TEST (result significant, Not completely at random)
MCAR_result <- mcar_test(subset)
print(MCAR_result)

# Correlation analysis
vis_miss(subset)






#### SEM  ###################

# Model Specification
sem1 <- ' 
 # Measurement Model
 
 agr7 =~ agr7_fig + agr7_hit + agr7_wepn
 agr6 =~ agr6_fig + agr6_hit + agr6_wepn + agr6_hpc
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

sem2<- 
  '
# Random Intercepts
ri_agr =~ 1*agr5_fig + 1*agr6_fig + 1*agr7_fig
ri_fsc =~ 1*cimp5 + 1*cimp6 + 1*cimp7
ri_par =~ 1*cpar4_dibn + 1*cpar5_dibn

# Measurement Models with adjustments for Random Intercepts
agr7 =~ agr7_fig + agr7_hit + agr7_wepn
agr6 =~ agr6_fig + agr6_hit + agr6_wepn + agr6_hpc
agr5 =~ agr5_fig + agr5_hpc

fsc5 =~ cimp5 + crisk5
fsc6 =~ cimp6 + crisk6
fsc7 =~ cimp7 + crisk7

par4 =~ cpar4_dibn + cpar4_ditr + cpar4_dire
par5 =~ cpar5_dibn + cpar5_ditr + cpar5_dire

# Structural Model with Cross-Lagged Paths and Random Intercepts
agr7 ~ fsc6 + agr6 + par5 + ri_agr
agr6 ~ fsc5 + agr5 + par5 + ri_agr
agr5 ~ par4 + gender6 + pared + race + married + ri_agr

fsc7 ~ agr6 + fsc6 + par5 + ri_fsc
fsc6 ~ agr5 + fsc5 + par5 + ri_fsc
fsc5 ~ par4 + gender6 + pared + race + married + ri_fsc

par5 ~ par4 + ri_par

# Adjust Covariances for Random Intercepts
ri_agr ~~ ri_fsc
ri_agr ~~ ri_par
ri_fsc ~~ ri_par

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

# Covariances among measurement errors or specific paths if needed

'

# Model Fitting

## Specify the ordinal variables
# List-wise Deletion
fit1 <- sem(model = sem1, data = subset, estimator="MLR")
# FIML
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
