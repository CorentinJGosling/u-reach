library(readxl)
library(tidyverse)

############################################
#            U-REACH FRAMEWORK             #  
#        -------------------------         #
# UNIFIED SCRIPT TO PROCESS EBIBD DATABASE #
#        -------------------------         #
#       @deprisco.michele@gmail.com        #  
#       @drvincenzo.oliva@gmail.com        #
#       @corentin.gosling@gmail.com        #
#         @marco.solmi83@gmail.com         #
#                                          #
############################################

rm(list=ls())

today_date = format(Sys.Date(), "%Y%m%d")

data = read_xlsx("ebibd_dataset_input_20241119.xlsx") #this is the input database,
                                                      #any database-related change should 
                                                      #be done on the original file!

####################
#      PART I      #
#  --------------  #
# CLEANING & GRADE #
####################

# If you encounter any problems, set part_1_proceed == FALSE,
# then run the code up to the step where the error occurred and check.

##.Steps to be followed:

#1.Removing any leading or trailing whitespaces from the whole database
#2.Checks
#2.1.Check that the effect size estimate is always included between the lower and upper CIs
#2.2.Check that the lower CI is always lower than the upper CI
#2.3.Check for outliers
#3.Create a column about statistical significance
#3.1.Duplicate the rows that are not compared to controls as defined below
#4.Change all SMD, RR, and OR in eSMD
#5.Create NNT column
#6.GRADE
#6.1.Create a column to unify hetereogeneity and inconsistency
#6.2.Create downGRADE and upGRADE columns
#6.3.Create columns for the final GRADE
#7.Create a column with the general name of the intervention (both arms)


#--------------------------------------#

part_1_proceed = TRUE

if (part_1_proceed == TRUE) {
  
  #1.Removing any leading or trailing whitespaces from the whole database
  
  data = data %>%
    mutate(across(where(is.character), str_trim))
  
  
  #2.Checks
  
  #2.1.Check that the effect size estimate is always included between the lower and upper CIs
  
  rows_to_be_checked = data.frame()
  
  
  for (id in 1:length(data$row)) {
    
    if (data$te_input[id] > data$te_low_input[id] &
        data$te_input[id] < data$te_up_input[id]) {
      
      next
      
    } else {
      
      rows_to_be_checked = rbind(rows_to_be_checked, data[id,])
      
    }
    
  }
  
  if (length(rows_to_be_checked) == 0) {
    
    cat("No problems have been encountered (STEP 2.1). Good job!\n")
    
  } else {
    
    cat("Some problems have been encountered (STEP 2.1). Double-check them before proceeding!\n")
    
  }
  
  rm(rows_to_be_checked, id)
  
  
  
  #2.2.Check that the lower CI is always lower than the upper CI
  
  rows_to_be_checked = data.frame()
  
  
  for (id in 1:length(data$row)) {
    
    if (data$te_low_input[id] < data$te_up_input[id]) {
      
      next
      
    } else {
      
      rows_to_be_checked = rbind(rows_to_be_checked, data[id,])
      
    }
    
  }
  
  if (length(rows_to_be_checked) == 0) {
    
    cat("No problems have been encountered (STEP 2.2). Good job!\n")
    
  } else {
    
    cat("Some problems have been encountered (STEP 2.2). Double-check them before proceeding!\n")
    
  }
  
  rm(rows_to_be_checked, id)
  
  
  
  #2.3.Check for outliers
  
  rows_to_be_checked = data.frame()
  
  
  for (id in 1:length(data$row)) {
    
    if (data$measure_te_input[id] == "SMD" | data$measure_te_input[id] == "MD") {
      
      diff_smd = (data$te_up_input[id] - data$te_input[id]) - (data$te_input[id] - data$te_low_input[id])
      Q1 = quantile(diff_smd, .25)
      Q3 = quantile(diff_smd, .75)
      IQR = IQR(diff_smd)
      
      if (diff_smd > 0.1 | diff_smd < (Q1 - 1.5*IQR) | diff_smd > (Q3 + 1.5*IQR)) {
        
        rows_to_be_checked = rbind(rows_to_be_checked, data[id,])
        
        cat("Some problems have been encountered (STEP 2.3). Double-check them before proceeding!\n")
        
      }
      
    } else {
      
      diff_or = (log(data$te_up_input[id]) - log(data$te_input[id])) - (log(data$te_input[id]) - log(data$te_low_input[id]))
      Q1 = quantile(diff_or, .25)
      Q3 = quantile(diff_or, .75)
      IQR = IQR(diff_or)
      
      if (any(diff_or > 0.5 | diff_or < (Q1 - 1.5*IQR) |  diff_or > Q3 + 1.5*IQR)) {
        
        rows_to_be_checked = rbind(rows_to_be_checked, data[id,])
        
        cat("Some problems have been encountered (STEP 2.3). Double-check them before proceeding!\n")
        
      }
      
    }    
  }
  
  
  rm(rows_to_be_checked, id, diff_or, diff_smd, IQR, Q1, Q3)
  

  
  #3.Create a column about statistical significance
  
  data$grade_sig_input = "no"
  
  
  for (id in 1:length(data$row)) {
    
    if (data$measure_te_input[id] == "SMD" | data$measure_te_input[id] == "MD") {
      
      if (data$te_low_input[id] * data$te_up_input[id] > 0) {
        
        data$grade_sig_input[id] = "yes"
        
      }
      
    } else {
      
      if ((data$te_low_input[id] < 1 & data$te_up_input[id] < 1) | 
          (data$te_low_input[id] > 1 & data$te_up_input[id] > 1))  {
        
        data$grade_sig_input[id] = "yes"
        
      }    
    }
    
  }
  
  table(data$grade_sig_input)
  
  rm (id)
  
  
  
  #3.1.Duplicate the rows that are not compared to controls as defined below
  
  controls = c("placebo", "control", "placebo (a)", "placebo/midazolam (a)", 
               "sham (a)", "TAU", "TAU/sham", "TAU/waiting list")
  
  row_rr <- data$row[data$measure_te_input %in% c("OR", "RR")]
  
  data$row_reversed = "not_reversed"
  
  data_reversed = data.frame(matrix(ncol = length(colnames(data)), nrow = 0))
  colnames(data_reversed) = colnames(data)
  
  unique_rows = unique(data$row)
  
  for (value in unique_rows) {
    
    data_loop = filter(data, row == value)
    
    if (data_loop$comparison_input %in% controls) {
      next
    } else {
      
      data_loop_new = data_loop
      
      new_row = data_loop_new %>%
        mutate(
          row_reversed = "reversed",
          intervention_input = data_loop$comparison_input,
          intervention_acro_input = data_loop$comparison_acro_input,
          intervention_type_input = data_loop$comparison_type_input,
          intervention_lai_input = data_loop$comparison_lai_input,
          comparison_input = data_loop$intervention_input,
          comparison_acro_input = data_loop$intervention_acro_input,
          comparison_type_input = data_loop$intervention_type_input,
          comparison_lai_input = data_loop$intervention_lai_input,
          te_input = case_when(
            value %in% row_rr ~ round(1 / data_loop$te_input, 2),
            TRUE ~ -data_loop$te_input
          ),          
          te_low_input = case_when(
            value %in% row_rr ~ round(1 / data_loop$te_up_input, 2),
            TRUE ~ -data_loop$te_up_input
          ),
          te_up_input = case_when(
            value %in% row_rr ~ round(1 / data_loop$te_low_input, 2),
            TRUE ~ -data_loop$te_low_input
          )
        )
      
      data_reversed = rbind(data_reversed, new_row)
    }
  }
  
  data = rbind(data, data_reversed)
  
  data = data[, c("row", "row_reversed", setdiff(names(data), c("row", "row_reversed")))]
  
  rm (data_loop, data_loop_new, data_reversed, new_row,
      controls, row_rr, unique_rows, value)
  

  
  #4.Change all SMD, RR, and OR in eSMD
  
  row_rr = which(data$measure_te_input %in% c("OR", "RR")) #identifying rows that are OR or RR
  #these will be transformed into eSMD
  row_md = which(data$measure_te_input %in% c("MD")) #identifying rows that are MD
  row_smd = which(data$measure_te_input %in% c("SMD")) #identifying rows that are SMD
  
  data$te_final_input = data$te_input
  data$te_low_final_input = data$te_low_input
  data$te_up_final_input = data$te_up_input
  
  data$te_final_input[row_rr] = log(data$te_final_input[row_rr]) * sqrt(3) / pi #eSMD formula
  data$te_low_final_input[row_rr] = log(data$te_low_final_input[row_rr]) * sqrt(3) / pi #eSMD formula
  data$te_up_final_input[row_rr] = log(data$te_up_final_input[row_rr]) * sqrt(3) / pi #eSMD formula
  
  data$inv_te_final_input = -data$te_final_input
  
  data$se_te_final_input = round(abs((data$te_up_final_input - data$te_low_final_input)/ (2*qnorm(.975))), 3)
  
  data$measure_te_final_input = "eSMD"
  data$measure_te_final_input[row_md] = "MD"
  
  rm(row_md, row_rr, row_smd)
  
  
  
  #5.Create NNT column
  
  data$nnt_input = round(1/((2 * pnorm(data$te_final_input/sqrt(2)) - 1)), 3)
  
  data$ennt_input = ifelse(data$nnt_input > 0,
                           paste0("eNNT=", data$nnt_input),
                           ifelse(
                             data$nnt_input == 0,
                             "N/A",
                             paste0("eNNH=", abs(data$nnt_input))))
  data$ennt_input[data$measure_te_input == "MD"] = NA
  
  
  
  #6.GRADE
  
  #6.1.Create a column to unify hetereogeneity and inconsistency
  
  data$grade_het_inc_input = NA #the value will be the highest value between heterogeneity and inconsistency
  
  any(is.na(data$grade_het_input)) #checking if we have NA values in "grade_het_input" 
  #(we SHOULD NOT HAVE missing values here!)
  
  any(is.na(data$grade_inc_input)) #checking if we have NA values in "grade_inc_input" 
  #(we SHOULD HAVE missing values here in case of MA!)
  
  data = data %>%
    mutate(grade_inc_input = replace_na(grade_inc_input, -999))
  
  
  for (id in 1:length(data$row)) {
    
    data$grade_het_inc_input[id] = pmax(data$grade_het_input[id], data$grade_inc_input[id])
    
  }
  
  rm(id)
  
  
  
  #6.2.Create downGRADE and upGRADE columns
  
  #downGRADE will use the following columns:
  #a. "grade_het_inc_input" (values 0,1,2)
  #b. "grade_impr_input" (values 0,1,2)
  #c. "grade_pub_bias_input" (values 0,1)
  #d. "grade_rob_input" (values 0,1,2)
  #e. "grade_indirect_input" (values 0,2)
  
  #check that the values for each columns fall into the ranges provided above
  lapply(data[, c("grade_het_inc_input", "grade_impr_input", "grade_pub_bias_input", 
                  "grade_rob_input", "grade_indirect_input")], table)
  
  
  if (all(data$grade_impr_input[data$grade_sig_input == "no"] != 2)) {
    
    cat("Some problems have been encountered (STEP 6.2). Double-check them before proceeding!\n")
    
    data[data$grade_sig_input == "no" & data$grade_impr_input != 2, ] 
    
  } else {
    
    cat("No problems have been encountered (STEP 6.2). Good job!\n")
    
  }
  
  
  data = data %>%
    mutate(grade_down_input = (grade_het_inc_input + 
                                 grade_impr_input + 
                                 grade_pub_bias_input + 
                                 grade_rob_input + 
                                 grade_indirect_input))
  
  
  #upGRADE will use the following columns:
  #a. "te_final_input"
  #b. "grade_sig_input"
  #c. "grade_rob_input"
  
  data$grade_up_input = ifelse(data$measure_te_final_input == "MD", 0,
                               ifelse(data$grade_rob_input == 2, 0, 
                                      ifelse(abs(data$te_final_input) >= 1.2 & data$grade_sig_input == "yes", 2, 
                                             ifelse(abs(data$te_final_input) >= 0.8 & data$grade_sig_input == "yes", 1, 
                                                    0))))
  
  
  
  #6.3.Create columns for the final GRADE
  
  data$grade_score_input = ifelse(
    (data$grade_up_input - data$grade_down_input) >= 0, "HIGH",
    ifelse((data$grade_up_input - data$grade_down_input) %in% -1:-2, "MODERATE",
           ifelse((data$grade_up_input - data$grade_down_input) %in% -3:-4, "LOW",
                  ifelse((data$grade_up_input - data$grade_down_input) < -4, "VERY LOW", "-999"))))
  
  table(data$grade_score_input)
  
  
  
  #7.Create a column with the general name of the intervention (both arms)
  
  sort(unique(data$intervention_input))
  
  data$intervention_general_input = data$intervention_input
  
  replacements = c(
    "L-iTBS-F3" = "TBS",
    "L-iTBS" = "TBS",
    "R-cTBS-F4" = "TBS",
    "L-HD-tDCS-F3" = "tDCS",
    "L-a-tDCS-F3+R-c-tDCS-F4" = "tDCS",
    "L-a-tDCS-F3+R-c-tDCS-F8" = "tDCS",
    "L-HF-rTMS" = "TMS",
    "R-LF-rTMS" = "TMS",
    "L-HF-rTMS-F3" = "TMS",
    "L-HF-rTMS-F3+R-LF-rTMS-F4" = "TMS",
    "L-LF-rTMS-F3" = "TMS",
    "HF-dTMS" = "TMS",
    "L-dTMS" = "TMS",
    "R-LF-rTMS-F4" = "TMS",
    "rTMS" = "TMS",
    "B-rTMS" = "TMS"
  )
  
  
  data = data %>%
    mutate(intervention_general_input = str_replace_all(intervention_general_input, "\\s?\\(a\\)|\\s?LAI", "")) %>%
    mutate(intervention_general_input = case_when(
      intervention_general_input %in% names(replacements) ~ replacements[intervention_general_input],
      TRUE ~ intervention_general_input))
  
  sort(unique(data$intervention_general_input))
  
  
  
  sort(unique(data$comparison_input))
  
  data$comparison_general_input = data$comparison_input
  
  data = data %>%
    mutate(comparison_general_input = str_replace_all(comparison_general_input, "\\s?\\(a\\)|\\s?LAI", "")) %>%
    mutate(comparison_general_input = case_when(
      comparison_general_input %in% names(replacements) ~ replacements[comparison_general_input],
      TRUE ~ comparison_general_input))
  
  sort(unique(data$comparison_general_input))
  
  rm(replacements, part_1_proceed)
  
  
}




#####################
#      PART II      #
#  --------------   #
#  PREFERENCE TOOL  #
#####################

# If you encounter any problems, set part_2_proceed == FALSE,
# then run the code up to the step where the error occurred and check.

##.Steps to be followed:

#1.Changing intervention label to remove LAI
#2.Defining the molecules used as control treatment for the Preference Tool
#3.Selecting specific rows
#4.Removing from the preference tool all the AEs/Biochemicals according to clinical judgement
#5.Identifying multiple rows with the same combination intervention | outcome | BD stage and removing from the Preference tool the ones with lower k_app 
#OR, in case of same k_app, lower year
#6.Removing from the Preference tool those rows with unspecific adverse events
#7.Identifying multiple rows with specific conditions and removing from the Preference tool the dichotomic ones
#8.Creating a new column to identify the interventions that are considered class I (for any mood state) according to CANMAT 2019
#9.Creating a heatmap for better visualization (safety)
#10.Adding efficacy measures (primary outcomes) to the preference tool
#11.Removing episode specific relapse from the preference tool (MUTED!!!)
#12.Creating outcome column with names that should be displayed in the Preference Tool


#--------------------------------------#

part_2_proceed = TRUE

if (part_2_proceed == TRUE) {
  
  data$preference = "no"
  
  #1.Changing intervention label to remove LAI
  
  sort(unique(data$intervention_input))
  
  data$intervention_tool = data$intervention_input
  
  data = data %>%
    mutate(intervention_tool = str_replace_all(intervention_tool, "\\s?LAI", ""))
  
  sort(unique(data$intervention_tool))
  
  
  
  #2.Defining the molecules used as control treatment for the Preference Tool
  
  controls = c("placebo", "control" #, "placebo (a)", "placebo/midazolam (a)",
               #"sham (a)", "TAU", "TAU/sham", "TAU/waiting list"
  )
  
  
  
  #3.Selecting specific rows
  #a. compared to controls AND
  #b. considering adverse events and biochemical AND
  #c. included in the treatment vector
  
  
  treatment = c("aripiprazole", "asenapine", #"brexpiprazole", "carbamazepine",
                 "cariprazine", "divalproex/valproate", #"chlorpromazine", "clozapine",
                 #"escitalopram", "fluoxetine", "haloperidol", "imipramine",
                 "lamotrigine", "lithium", "lurasidone", "lumateperone", 
                 "paliperidone", "quetiapine", #"paroxetine", "olanzapine",
                 "olanzapine+fluoxetine","risperidone" 
                 #, "sertraline", "ziprasidone"
  )
  
  table(data$preference)
  
  for (id in 1:nrow(data)) {
    if (data$measure_te_final_input[id] == "MD"){
      next
    } else {
      if (data$comparison_input[id] %in% controls) {
        if (startsWith(data$outcome_input[id], "Adverse events") || 
            startsWith(data$outcome_input[id], "Biochemical")) {
          if (data$intervention_tool[id] %in% treatment) {
            data$preference[id] = "yes"
          }
        }
      }
    }
  }
  
  table(data$preference)
  
  rm(id)
  
  
  
  #4.Removing from the preference tool all the AEs/Biochemicals according to clinical judgement
  
  to_be_removed = c(
    "Adverse events, Nasopharyngitis",
    "Biochemical, HbA1c, change from baseline",
    "Adverse events, Anorexia",
    "Adverse events, Pain",
    "Adverse events, Abdominal pain",
    "Adverse events, Accidental injury",
    "Adverse events, Activation",
    "Adverse events, Agitation",
    "Adverse events, Alopecia",
    "Adverse events, Anxiety",
    "Adverse events, Back pain",
    "Adverse events, BMI, change from baseline",
    "Adverse events, Diabetes",
    "Adverse events, Dyspepsia",
    "Adverse events, Dysuria",
    "Adverse events, Fever",
    "Adverse events, Gastroenteric disturbance",
    "Adverse events, Hyperkinesia",
    "Adverse events, Nasal congestion",
    "Adverse events, Nervousness",
    "Adverse events, Rash",
    "Adverse events, Upper respiratory tract infection",
    "Adverse events, Waist circumference, change from baseline",
    "Biochemical, ALT/AST/GPT, increase",
    "Adverse events, Vomiting",
    "Biochemical, TSH, increase",
    "Biochemical, Triglycerids, change from baseline",
    "Biochemical, Glucose, change from baseline",
    "Biochemical, Cholesterol HDL, change from baseline",
    "Biochemical, Cholesterol LDL, change from baseline",
    "Biochemical, Cholesterol total, change from baseline",
    "Adverse events, Prolactin-related adverse events",
    "Adverse events, Orthostatic hypothension",
    "Adverse events, Increased appetite",
    "Adverse events, Hyperkinisia",
    "Adverse events, Depression",
    "Adverse events, Asthenia",
    "Adverse events, Tremor")
  
  
  for (id in 1:nrow(data)) {
    
    if (data$outcome_input[id] %in% to_be_removed) {
      
      data$preference[id] = "no"
      
    } else {
      
      next
      
    }
    
  }
  
  
  sort(unique(filter(data, preference == "yes")$outcome_input))
  
  table(data$preference)
  
  rm(id, to_be_removed)
  
  
  
  #5.Identifying multiple rows with the same combination intervention | outcome and removing from the Preference tool the ones with lower k_app 
  #OR, in case of same k_app, lower year OR, in case of same k_pp and year, conducted on Adults only
  
  data$preference_final = data$preference
  
  for (intervention in unique(data$intervention_tool)) {
    for (outcome in unique(data$outcome_input)) {
      #for (stage in unique(data$bd_stage_input)) {
      
      subset_data = data %>%
        filter(intervention_tool == intervention & 
                 outcome_input == outcome & 
                 #bd_stage_input == stage & 
                 preference == "yes")
      
      if (nrow(subset_data) > 1) {
        
        subset_data$Year = as.numeric(gsub(".*([0-9]{4}).*", "\\1", subset_data$paper_input))
        
        min_year_row = which(subset_data$Year != max(subset_data$Year))
        
        min_k_input_row = which(subset_data$k_input != max(subset_data$k_input))
        
        rows_with_lower_k_input = subset_data$row[min_k_input_row]
        
        rows_with_lower_year = subset_data$row[min_year_row]
        
        if ((min(subset_data$k_input) == max(subset_data$k_input)) && 
            (min(subset_data$Year) == max(subset_data$Year))) {
          
          non_adults_only_rows = subset_data$row[subset_data$age_input != "Adults only"]
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", #stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. The k_input is the same. The Year is the same.
                     Setting preference to 'no' for the row(s) that are not 'Adults only' (",
                     paste(non_adults_only_rows, collapse = ", "), ").\n\n"))
          
          data$preference_final[data$row %in% non_adults_only_rows] = "no"
          
        } else if (min(subset_data$k_input) == max(subset_data$k_input)) {
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", #stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. The k_input is the same. 
                     Setting preference to 'no' for the row(s) with older publication year (",
                     paste(rows_with_lower_year, collapse = ", "), ").\n\n"))
          
          data$preference_final[data$row %in% rows_with_lower_year] = "no"
          
        } else {
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", #stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. Setting preference to 'no' for the row(s) with lower k_input (",
                     paste(rows_with_lower_k_input, collapse = ", "), ").\n\n"))
          
          data$preference_final[data$row %in% rows_with_lower_k_input] = "no"
          
        }
      }
      #    }
    }
  }
  
  
  data$preference = NULL
  
  
  table(data$preference_final)
  
  rm(subset_data, intervention, min_k_input_row,
     min_year_row, outcome, rows_with_lower_k_input, rows_with_lower_year)
  
  
  
  #6.Removing from the Preference tool those rows with unspecific adverse events
  
  for (id in 1:nrow(data)) {
    # Check the specific conditions for outcome_input
    if (startsWith(data$outcome_input[id], "Adverse events, any") ||
        startsWith(data$outcome_input[id], "Adverse events, At least one adverse event") ||
        startsWith(data$outcome_input[id], "Adverse events, Serious, any")) {
      data$preference_final[id] = "no"
    }
  }
  
  sort(unique(filter(data, preference_final == "yes")$outcome_input))
  
  table(data$preference_final)
  
  rm(id)
  
  
  
  #7.Identifying multiple rows with specific conditions and removing from the Preference tool the dichotomic ones
  #a. Akathisia | Akathisia, scale
  #b. Extrapyramidal symptoms | Extrapyramidal symptoms, scale
  #c. Weight, increase | Weight, change from baseline
  #d. Prolactin, increase | Prolactin, change from baseline
  
  
  saveRDS(data, "backup_preference_tool.rds")
  
  # data = readRDS("backup.rds")
  
  
  pairs_to_check = list(
    c("Adverse events, Akathisia", "Adverse events, Akathisia, scale"),
    c("Adverse events, Extrapyramidal symptoms", "Adverse events, Extrapyramidal symptoms, scale"),
    c("Adverse events, Weight, increase", "Adverse events, Weight, change from baseline"),
    c("Biochemical, Prolactin, increase", "Biochemical, Prolactin, change from baseline")
  )
  
  to_remove = c()
  
  for (pair in pairs_to_check) {
    dichotomous = pair[1]
    scaled = pair[2]
    
    # Find rows where both dichotomous and scaled versions exist
    rows_with_both = data %>%
      filter(preference_final == "yes") %>%
      filter(outcome_input %in% c(dichotomous, scaled)) %>%
      group_by(intervention_tool) %>%
      filter(n_distinct(outcome_input) > 1) %>%
      ungroup()
    
    # Check if k_input values differ and select the one with the higher k_input
    if (!nrow(rows_with_both) == 0) {
      rows_with_both = rows_with_both %>%
        group_by(intervention_tool) %>%
        mutate(k_input_max = max(k_input)) %>%
        ungroup()
      
      # Identify the rows to remove
      rows_to_check = rows_with_both %>%
        group_by(intervention_tool) %>%
        filter(k_input != k_input_max) %>%
        ungroup() %>%
        select(row)
      
      # If there are rows to check and they differ, keep the higher k_input
      if (nrow(rows_to_check) > 0) {
        dichotomic_rows_to_remove = rows_with_both %>%
          filter(k_input != k_input_max) %>%
          pull(row)
        
      } else {
        dichotomic_rows_to_remove = rows_with_both %>%
          filter(outcome_input == dichotomous) %>%
          pull(row)
      }
      
      to_remove = c(to_remove, dichotomic_rows_to_remove)
    }
  }
  
  
  data$preference_final[data$row %in% to_remove] = "no"
  
  
  sort(unique(filter(data, preference_final == "yes")$outcome_input))
  
  table(data$preference_final)
  
  rm(pairs_to_check, rows_to_check, rows_with_both, 
     dichotomic_rows_to_remove, dichotomous, pair, scaled, to_remove)
  
  
  
  
  #8.Creating a new column to identify the interventions that are considered class I (for any mood state) according to CANMAT 2019
  
  canmat_first_treatment = c("aripiprazole", "asenapine", "cariprazine", "risperidone",
                             "paliperidone", "quetiapine", "lithium", "divalproex/valproate",
                             "lurasidone", "lamotrigine")
  
  data$canmat_first = NA
  
  for (id in 1:nrow(data)) {
    if (data$intervention_tool[id] %in% canmat_first_treatment) {
      
      data$canmat_first[id] = "first"
      
    } else {
      
      data$canmat_first[id] = "not_first"
      
    }
  }
  
  table(data$preference_final)
  
  
  rm(id, canmat_first_treatment)
  
  
  
  #9.Creating a heatmap for better visualization (safety)
  
  # library(ggplot2)
  # 
  # data_tool = filter(data, preference_final == "yes" & canmat_first == "first" & effsaf_input == "S")
  # 
  # pdf(file.path(getwd(), paste0("heatmap_canmat_first_safety.pdf")), width = 11, height = 11)
  # ggplot(data_tool, aes(x = intervention_tool, y = outcome_input)) +
  #   geom_tile(color = "black", fill = "black") +
  #   theme_minimal() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #   labs(x = "", y = "", title = "ONLY first line according to CANMAT 2019")
  # dev.off()
  # 
  # rm(data_tool)
  
  
  
  #10.Adding efficacy measures (primary outcomes) to the preference tool
  
  data_eff = filter(data, outcome_type_input == "primary" & effsaf_input == "E"
                    & comparison_input %in% controls & intervention_input %in% treatment)
  
  table(data_eff$preference_final) #all values should be == no
  
  rows_data_eff = unique(data_eff$row)
  
  rows_data_eff_no = c()
  
  
  
  
  for (intervention in unique(data$intervention_tool)) {
    for (outcome in unique(data$outcome_input)) {
      
      subset_data = data_eff %>%
        filter(intervention_tool == intervention & 
                 outcome_input == outcome)
      
      if (nrow(subset_data) > 1) {
        
        subset_data$Year = as.numeric(gsub(".*([0-9]{4}).*", "\\1", subset_data$paper_input))
        
        min_year_row = which(subset_data$Year != max(subset_data$Year))
        
        min_k_input_row = which(subset_data$k_input != max(subset_data$k_input))
        
        rows_with_lower_k_input = subset_data$row[min_k_input_row]
        
        rows_with_lower_year = subset_data$row[min_year_row]
        
        if ((min(subset_data$k_input) == max(subset_data$k_input)) && 
            (min(subset_data$Year) == max(subset_data$Year))) {
          
          non_adults_only_rows = subset_data$row[subset_data$age_input != "Adults only"]
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", #stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. The k_input is the same. The Year is the same.
                     Setting preference to 'no' for the row(s) that are not 'Adults only' (",
                     paste(non_adults_only_rows, collapse = ", "), ").\n\n"))
          
          rows_data_eff_no = c(rows_data_eff_no, data$row[data$row %in% non_adults_only_rows])
          
        } else if (min(subset_data$k_input) == max(subset_data$k_input)) {
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", #stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. The k_input is the same. 
                     Setting preference to 'no' for the row(s) with older publication year (",
                     paste(rows_with_lower_year, collapse = ", "), ").\n\n"))
          
          rows_data_eff_no = c(rows_data_eff_no, data$row[data$row %in% rows_with_lower_year])
          
        } else {
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", #stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. Setting preference to 'no' for the row(s) with lower k_input (",
                     paste(rows_with_lower_k_input, collapse = ", "), ").\n\n"))
          
          rows_data_eff_no = c(rows_data_eff_no, data$row[data$row %in% rows_with_lower_k_input])
          
        }
      }
    }
  }
  
  
  
  #11.Removing episode specific relapse from the preference tool
  
  
  for (element in rows_data_eff) {

    data_loop = filter(data_eff, row == element)

    if (data_loop$outcome_input == "Relapse, depressive episode" |
        data_loop$outcome_input == "Relapse, manic/hypomanic episode" |
        data_loop$outcome_input == "Relapse, mixed episode" |
        data_loop$outcome_input == "Relapse, manic/hypomanic/mixed episode") {

      rows_data_eff_no = c(rows_data_eff_no, element)

    }

  }
  
  
  
  #12.Creating outcome column with names that should be displayed in the Preference Tool
  
  data$outcome_tool_input = data$outcome_input
  
  sort(unique(data$outcome_input))
  
  replacements = c(
    "Adverse events, Extrapyramidal symptoms, scale" = "Adverse events, Extrapyramidal symptoms",
    "Adverse events, Akathisia, scale" = "Adverse events, Akathisia",
    "Adverse events, Weight, change from baseline" = "Adverse events, Weight, increase",
    "Biochemical, Prolactin, change from baseline" = "Biochemical, Prolactin",
    "Biochemical, Prolactin, increase" = "Biochemical, Prolactin"
  )
  
  data = data %>%
    mutate(outcome_tool_input = case_when(
      outcome_tool_input %in% names(replacements) ~ replacements[outcome_tool_input],
      TRUE ~ outcome_tool_input))
  
  
  
  data$outcome_acro_tool_input = data$outcome_acro_input
  
  sort(unique(data$outcome_acro_input))
  
  replacements_acro = c(
    "AE, Extrapyramidal symptoms, scale" = "AE, Extrapyramidal symptoms",
    "AE, Akathisia, scale" = "AE, Akathisia",
    "AE, Weight, change" = "AE, Weight, increase",
    "Biochemical, Prolactin, change" = "Biochemical, Prolactin",
    "Biochemical, Prolactin, increase" = "Biochemical, Prolactin"
  )
  
  data = data %>%
    mutate(outcome_acro_tool_input = case_when(
      outcome_acro_tool_input %in% names(replacements_acro) ~ replacements_acro[outcome_acro_tool_input],
      TRUE ~ outcome_acro_tool_input))
  
  
  
  
  rows_data_eff_yes = setdiff(rows_data_eff, rows_data_eff_no)
  
  data$preference_final[data$row %in% rows_data_eff_yes] = "yes"
  
  table(data$preference_final)
  table(filter(data, effsaf_input == "S")$preference_final)
  table(filter(data, effsaf_input == "E")$preference_final)
  
  
  rm(subset_data, intervention, min_k_input_row,
     min_year_row, outcome, rows_with_lower_k_input, rows_with_lower_year,
     controls, treatment, rows_data_eff, rows_data_eff_yes, rows_data_eff_no,
     data_eff, part_2_proceed, data_loop, element, replacements, replacements_acro)
  
}




######################
#      PART III      #
#   --------------   #
#    INTERVENTION    #
#        TAB         #
######################

# If you encounter any problems, set part_3_proceed == FALSE,
# then run the code up to the step where the error occurred and check.

##.Steps to be followed:

#1.Defining the molecules used as control treatment for the Intervention Tab
#2.Defining the efficacy and safety outcomes considered for the Intervention Tab
#3.Selecting specific rows
#4.Removing from the Intervention Tab all the efficacy outcomes that are not co-primary
#5.Identifying multiple rows with the same combination intervention | outcome | BD stage and removing from the Intervention Tab the ones with lower k_app 
#OR, in case of same k_app, lower year
#6.Identifying multiple rows with specific conditions and removing from the Intervention Tab
#the lowest k or the dichotomic ones
#7.Creating a new outcome column for the Intervention Tab that will display standardized names for relapse and prolactin
#8.Creating new estimates columns for the Intervention Tab to display the estimates and CIs only for the interventions
#in monotherapy vs. placebo


#--------------------------------------#

part_3_proceed = TRUE

if (part_3_proceed == TRUE) {
  
  data$intervention_tab = "no"
  
  #1.Defining the molecules used as control treatment for the Intervention Tab
  
  controls = c("placebo" # "placebo (a)", "placebo/midazolam (a)",
               #"sham (a)", "TAU", "TAU/sham", "TAU/waiting list"
  )
  
  
  #2.Defining the efficacy and safety outcomes considered for the Intervention Tab
  
  outcomes_safety = sort(unique((filter(data, effsaf_input == "S")$outcome_input)))
  
  outcomes_efficacy = c("Depression symptoms scale, change from baseline",
               "Mania symptoms scale, change from baseline",
               "Relapse, any mood episode"
               ) 
  
  outcomes = c(outcomes_safety, outcomes_efficacy)
  
  
  #3.Selecting specific rows
  #a. compared to controls AND
  #b. monotherapy only AND
  #c. considering defined outcomes (all safety and specific efficacy)
  
  table(data$intervention_tab)
  
  for (id in 1:nrow(data)) {
    if (data$measure_te_final_input[id] == "MD"){
      next
    } else {
      if (data$comparison_input[id] %in% controls) {
        if (data$intervention_type_input[id] == "mono") {
          if (data$outcome_input[id] %in% outcomes) {
            data$intervention_tab[id] = "yes"
          }
        }
      }
    }
  }
  
  data[data$platform == "no", ]$intervention_tab = "no"
  
  table(data$intervention_tab)
  
  rm(id, outcomes_safety, outcomes_efficacy)
  
  
  
  #4.Removing from the Intervention Tab all the efficacy outcomes that are not co-primary
  
  for (id in 1:nrow(data)) {
    if (data$intervention_tab[id] == "yes"){
      if (data$effsaf_input[id] == "E" & data$outcome_type_input[id] != "primary") {
        data$intervention_tab[id] = "no"
      } else {
        next
      }
    }
  }
  
  table(data$intervention_tab)
  
  rm(id)
  
  
  #5.Identifying multiple rows with the same combination intervention | outcome | BD stage and removing from the Intervention Tab the ones with lower k_app 
  #OR, in case of same k_app, lower year, OR, in case of same k_pp and year, conducted on Adults only
  
  data$intervention_tab_final = data$intervention_tab
  
  for (intervention in unique(data$intervention_tool)) {
    for (outcome in unique(data$outcome_input)) {
      for (stage in unique(data$bd_stage_input)) {
      
      subset_data = data %>%
        filter(intervention_tool == intervention & 
                 outcome_input == outcome & 
                 bd_stage_input == stage & 
                 intervention_tab == "yes")
      
      if (nrow(subset_data) > 1) {
        
        subset_data$Year = as.numeric(gsub(".*([0-9]{4}).*", "\\1", subset_data$paper_input))
        
        min_year_row = which(subset_data$Year != max(subset_data$Year))
        
        min_k_input_row = which(subset_data$k_input != max(subset_data$k_input))
        
        rows_with_lower_k_input = subset_data$row[min_k_input_row]
        
        rows_with_lower_year = subset_data$row[min_year_row]
        
        if ((min(subset_data$k_input) == max(subset_data$k_input)) && 
            (min(subset_data$Year) == max(subset_data$Year))) {
          
          non_adults_only_rows = subset_data$row[subset_data$age_input != "Adults only"]
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. The k_input is the same. The Year is the same.
                     Setting intervention_tab to 'no' for the row(s) that are not 'Adults only' (",
                     paste(non_adults_only_rows, collapse = ", "), ").\n\n"))
          
          data$intervention_tab_final[data$row %in% non_adults_only_rows] = "no"
          
        } else if (min(subset_data$k_input) == max(subset_data$k_input)) {
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. The k_input is the same. 
                     Setting intervention_tab to 'no' for the row(s) with older publication year (",
                     paste(rows_with_lower_year, collapse = ", "), ").\n\n"))
          
          data$intervention_tab_final[data$row %in% rows_with_lower_year] = "no"
          
        } else {
          
          cat(paste0("Multiple rows identified for ", intervention, " | ", outcome, " | ", stage, 
                     ". \nRows ", paste(unique(subset_data$row), collapse = ", "), 
                     " have been identified. Setting intervention_tab to 'no' for the row(s) with lower k_input (",
                     paste(rows_with_lower_k_input, collapse = ", "), ").\n\n"))
          
          data$intervention_tab_final[data$row %in% rows_with_lower_k_input] = "no"
          
        }
      }
          }
    }
  }
  
  # The next rows were NOT automatically detected

  data[data$row == "884", ]$intervention_tab_final = "no" # manually setting "no" to LAI formulation
  data[data$row == "886", ]$intervention_tab_final = "no" # manually setting "no" to LAI formulation
  data[data$row == "888", ]$intervention_tab_final = "no" # manually setting "no" to LAI formulation
  data[data$row == "762", ]$intervention_tab_final = "no" # manually setting "no" to LAI formulation
  data[data$row == "878", ]$intervention_tab_final = "no" # manually setting "no" to LAI formulation
  data[data$row == "879", ]$intervention_tab_final = "no" # manually setting "no" to LAI formulation
  
  
  data$intervention_tab = NULL
  
  table(data$intervention_tab_final)
  
  rm(subset_data, intervention, min_k_input_row,
     min_year_row, outcome, rows_with_lower_k_input, rows_with_lower_year,
     non_adults_only_rows, stage)
  
  
  #6.Identifying multiple rows with specific conditions and removing from the Intervention Tab
  #the lowest k or the dichotomic ones
  #a. Akathisia | Akathisia, scale
  #b. Extrapyramidal symptoms | Extrapyramidal symptoms, scale
  #c. Weight, increase | Weight, change from baseline
  #d. Prolactine, increase | Prolactine, change from baseline
  
  
  saveRDS(data, "backup_intervention_tab.rds")
  
  # data = readRDS("backup.rds")
  
  
  pairs_to_check = list(
    c("Adverse events, Akathisia", "Adverse events, Akathisia, scale"),
    c("Adverse events, Extrapyramidal symptoms", "Adverse events, Extrapyramidal symptoms, scale"),
    c("Adverse events, Weight, increase", "Adverse events, Weight, change from baseline"),
    c("Biochemical, Prolactin, change from baseline", "Biochemical, Prolactin, increase")
  )
  
  to_remove = c()
  
  for (pair in pairs_to_check) {
    dichotomous = pair[1]
    scaled = pair[2]
    
    # Find rows where both dichotomous and scaled versions exist
    rows_with_both = data %>%
      filter(intervention_tab_final == "yes") %>%
      filter(outcome_input %in% c(dichotomous, scaled)) %>%
      group_by(intervention_tool, bd_stage_input) %>%
      filter(n_distinct(outcome_input) > 1) %>%
      ungroup()
    
    # Check if k_input values differ and select the one with the higher k_input
    if (!nrow(rows_with_both) == 0) {
      rows_with_both = rows_with_both %>%
        group_by(intervention_tool, bd_stage_input) %>%
        mutate(k_input_max = max(k_input)) %>%
        ungroup()
      
      # Identify the rows to remove
      rows_to_check = rows_with_both %>%
        group_by(intervention_tool, bd_stage_input) %>%
        filter(k_input != k_input_max) %>%
        ungroup() %>%
        select(row)
      
      # If there are rows to check and they differ, keep the higher k_input
      if (nrow(rows_to_check) > 0) {
        dichotomic_rows_to_remove = rows_with_both %>%
          filter(k_input != k_input_max) %>%
          pull(row)
        
      } else {
        dichotomic_rows_to_remove = rows_with_both %>%
          filter(outcome_input == dichotomous) %>%
          pull(row)
      }
      
      to_remove = c(to_remove, dichotomic_rows_to_remove)
    }
  }
  
  
  data$intervention_tab_final[data$row %in% to_remove] = "no"
  
  
  #7.Creating a new outcome column for the Intervention Tab that will display standardized names for relapse and prolactin
  
  data = data %>%
    mutate(outcome_intervention_tab_input = case_when(
      outcome_input == "Biochemical, Prolactin, change from baseline" ~ "Biochemical, prolactin",
      outcome_input == "Biochemical, Prolactin, increase" ~ "Biochemical, prolactin",
      outcome_input == "Relapse, manic/hypomanic episode" ~ "Relapse, manic/hypomanic/mixed episode",
      TRUE ~ outcome_input  # For all other cases, keep the original value
    ))
  
  
  data = data %>%
    mutate(outcome_acro_intervention_tab_input = case_when(
      outcome_acro_input == "Biochemical, Prolactin, change" ~ "Biochemical, prolactin",
      outcome_acro_input == "Biochemical, Prolactin, increase" ~ "Biochemical, prolactin",
      outcome_acro_input == "Relapse, (hypo)manic" ~ "Relapse, (hypo)manic/mixed",
      TRUE ~ outcome_acro_input  # For all other cases, keep the original value
    ))
  
  
  sort(unique(filter(data, intervention_tab_final == "yes")$outcome_input))
  
  sort(unique(filter(data, intervention_tab_final == "yes")$outcome_intervention_tab_input))
  
  table(data$intervention_tab_final)
  
  
  #8.Creating new estimates columns for the Intervention Tab to display the estimates and CIs only for the interventions
  #in monotherapy vs. placebo
  
  data$te_final_intervention_tab_input = data$te_final_input
  data$te_low_final_intervention_tab_input = data$te_low_final_input
  data$te_up_final_intervention_tab_input = data$te_up_final_input
  
  data$te_final_intervention_tab_input[data$intervention_tab_final == "no"] = NA
  data$te_low_final_intervention_tab_input[data$intervention_tab_final == "no"] = NA
  data$te_up_final_intervention_tab_input[data$intervention_tab_final == "no"] = NA
  
  
  rm(pairs_to_check, rows_to_check, rows_with_both, 
     dichotomic_rows_to_remove, dichotomous, pair, scaled, to_remove,
     controls, outcomes, part_3_proceed)
  
}




######################
#      PART  IV      #
#   --------------   #
#   OUTPUT DATASET   #
######################

# If you encounter any problems, set part_4_proceed == FALSE,
# then run the code up to the step where the error occurred and check.

##.Steps to be followed:

#1.Creating columns that will be used in the platform (ending with _app)
#2.Creating selectors
#3.Exporting the dataset

#--------------------------------------#

part_4_proceed = TRUE

if (part_4_proceed == TRUE) {
  
  #1.Creating columns that will be used in the platform (ending with _app)
  
  colnames(data)
  
  data$row_app = data$row
  data$row_reversed_app = data$row_reversed
  
  data$paper_app = data$paper_input
  data$doi_paper_app = data$doi_paper_input
  
  
  data$intervention_app = data$intervention_input
  data$intervention_acro_app = data$intervention_acro_input
  data$intervention_type_app = data$intervention_type_input
  data$intervention_general_app = data$intervention_general_input #this column displays a cleaner 
                                                                  #name for interventions (e.g., removing the (a) for augmentation)
  data$intervention_tool_app = data$intervention_tool
  data$intervention_lai_app = data$intervention_lai_input
  
  
  data$comparison_app = data$comparison_input
  data$comparison_acro_app = data$comparison_acro_input
  data$comparison_type_app = data$comparison_type_input
  data$comparison_general_app = data$comparison_general_input #this column displays a cleaner 
                                                              #name for comparisons (e.g., removing the (a) for augmentation)
  
  
  data$outcome_app = data$outcome_input
  data$outcome_acro_app = data$outcome_acro_input
  data$outcome_type_app = data$outcome_type_input
  data$outcome_tool_app = data$outcome_tool_input
  data$outcome_acro_tool_app = data$outcome_acro_tool_input
  data$outcome_intervention_tab_app = data$outcome_intervention_tab_input
  data$outcome_acro_intervention_tab_app = data$outcome_acro_intervention_tab_input
  data$effsaf_app = data$effsaf_input
  
  
  data$measure_te_final_app = data$measure_te_final_input
  data$te_final_app = round(data$te_final_input,3)
  data$te_low_final_app = round(data$te_low_final_input,3)
  data$te_up_final_app = round(data$te_up_final_input,3)
  data$inv_te_final_app = round(data$inv_te_final_input,3) #we do not need this column
  data$se_te_final_app = round(data$se_te_final_input,3) #we do not need this column
  data$te_final_intervention_tab_app = round(data$te_final_intervention_tab_input,3)
  data$te_low_final_intervention_tab_app = round(data$te_low_final_intervention_tab_input,3)
  data$te_up_final_intervention_tab_app = round(data$te_up_final_intervention_tab_input,3)

  
  data$nnt_app = data$nnt_input
  data$ennt_app = data$ennt_input
  
  
  data$k_app = data$k_input
  
  
  data$design_app = ifelse(data$design_input == "NMA", "Network meta-analysis", 
                           ifelse(data$design_input == "RCT", "Randomized Controlled Trial", 
                                  ifelse(data$design_input == "Clinical consensus", "Clinical consensus", 
                                         "Pairwise meta-analysis")))
  
  table(data$design_app)
  
  
  data$age_app = ifelse(data$age_input == "Adults only", "Adults", 
                        ifelse(data$age_input == "Mixed children/adolescents/adults", "Mixed (Children-Adults)", 
                               "Children & Adolescents"))
  
  table(data$age_app)
  
  
  data$bd_stage_app = data$bd_stage_input
  
  data$grade_sig_app = data$grade_sig_input
  
  data$grade_het_app = ifelse(data$grade_het_input == 0, "<75%", 
                           ifelse(data$grade_het_input == 1, "≥75% and <90%",
                                  "≥90%"))
  
  data$grade_inc_app = ifelse(data$grade_inc_input == 0, "≥0.10",
                              ifelse(data$grade_inc_input == 1, "≥0.05 and <0.10",
                                     ifelse(data$grade_inc_input == 2, "<0.05",
                                     "Not applicable")))
  
  data$grade_impr_app = ifelse(data$grade_impr_input == 0, "≥200", 
                              ifelse(data$grade_impr_input == 1, "100-199",
                                     ifelse(data$grade_impr_input == 2 & data$grade_sig_input == "yes", "1-99",
                                            "Not significant")))
  
  data$grade_pub_bias_app = ifelse(data$grade_pub_bias_input == 0, "no", 
                                     "yes")
  
  data$grade_rob_app = ifelse(data$grade_rob_input == 0, "<25%", 
                              ifelse(data$grade_rob_input == 1, "≥25% and <50%",
                                     ">50%"))
  
  data$grade_indirect_app = ifelse(data$grade_indirect_input == 0, "no", 
                                   "yes")
  
  
  data$grade_score_app = str_to_title(data$grade_score_input)
  data$grade_rank_app = ifelse(data$grade_score_input == "HIGH", "I", 
                               ifelse(data$grade_score_input == "MODERATE", "II", 
                                      ifelse(data$grade_score_input == "VERY LOW", "IV", 
                                             "III")))
  data$grade_score_pre_manual_check_app = data$grade_score_app
  
  
  data$amstar_app = str_to_title(data$amstar_input)
  
  data$nbn_app = data$nbn_input
  
  
  
  #2.Creating selectors
  
  data$platform_selector = data$platform
  data$preference_final_selector = data$preference_final
  data$canmat_first_selector = data$canmat_first
  data$intervention_tab_selector = data$intervention_tab_final
  
  
  
  #3.Exporting the dataset
  
  data_output = data[, grep("(_app|_selector)$", names(data))]
  
  rm(part_4_proceed)
  
}

table(filter(data_output, platform_selector == "yes" 
             & row_reversed_app == "not_reversed")$grade_score_app)



######################
#      PART  V       #
#   --------------   #
#    MANUAL GRADE    #
######################


#Only for those outcomes reaching HIGH or MODERATE certainty, we will downgrade 
#the associations informed by the following outcomes, after expert consensus and 
#clinical discussion

#--------------------------------------#

part_5_proceed = TRUE

if (part_5_proceed == TRUE) {

data_output_final = data_output

data_output_final$grade_manual_downgrade = "no"


#a. esketamine (a) (Bipolar Depression)

unique_rows <- unique(data_output_final$row_app[
  (data_output_final$intervention_app == "esketamine (a)" |
     data_output_final$comparison_app == "esketamine (a)") &
    (data_output_final$grade_score_app %in% c("High", "Moderate"))
])


#View(data_output_final[data_output_final$row_app %in% unique_rows, ])


rows_to_update <- data_output_final$row_app %in% unique_rows


data_output_final$grade_score_app[rows_to_update] <- "Low"
data_output_final$grade_rank_app[rows_to_update] <- "III"
data_output_final$grade_manual_downgrade[rows_to_update] <- "yes"


rm(rows_to_update, unique_rows)


#b. coenzyme Q10 (a) (Bipolar Depression)

unique_rows <- unique(data_output_final$row_app[
  (data_output_final$intervention_app == "coenzyme Q10 (a)" |
     data_output_final$comparison_app == "coenzyme Q10 (a)") &
    (data_output_final$grade_score_app %in% c("High", "Moderate"))
])


#View(data_output_final[data_output_final$row_app %in% unique_rows, ])


rows_to_update <- data_output_final$row_app %in% unique_rows


data_output_final$grade_score_app[rows_to_update] <- "Low"
data_output_final$grade_rank_app[rows_to_update] <- "III"
data_output_final$grade_manual_downgrade[rows_to_update] <- "yes"


rm(rows_to_update, unique_rows)


#c. celecoxib (a) (Bipolar Mania)

unique_rows <- unique(data_output_final$row_app[
  (data_output_final$intervention_app == "celecoxib (a)" |
     data_output_final$comparison_app == "celecoxib (a)") &
    (data_output_final$grade_score_app %in% c("High", "Moderate"))
])


#View(data_output_final[data_output_final$row_app %in% unique_rows, ])


unique_rows = unique_rows[c(1,2)]


rows_to_update <- data_output_final$row_app %in% unique_rows


data_output_final$grade_score_app[rows_to_update] <- "Low"
data_output_final$grade_rank_app[rows_to_update] <- "III"
data_output_final$grade_manual_downgrade[rows_to_update] <- "yes"


#d. TMS

unique_rows <- unique(data_output_final$row_app[
  (data_output_final$intervention_general_app == "TMS" |
     data_output_final$comparison_general_app == "TMS") &
    (data_output_final$effsaf_app == "E") &
    (data_output_final$grade_score_app %in% c("High", "Moderate"))
])


#View(data_output_final[data_output_final$row_app %in% unique_rows, ])


rows_to_update <- data_output_final$row_app %in% unique_rows


data_output_final$grade_score_app[rows_to_update] <- "Low"
data_output_final$grade_rank_app[rows_to_update] <- "III"
data_output_final$grade_manual_downgrade[rows_to_update] <- "yes"


#e. tDCS

unique_rows <- unique(data_output_final$row_app[
  (data_output_final$intervention_general_app == "tDCS" |
     data_output_final$comparison_general_app == "tDCS") &
    (data_output_final$effsaf_app == "E") &
    (data_output_final$grade_score_app %in% c("High", "Moderate"))
])


#View(data_output_final[data_output_final$row_app %in% unique_rows, ])


rows_to_update <- data_output_final$row_app %in% unique_rows


data_output_final$grade_score_app[rows_to_update] <- "Low"
data_output_final$grade_rank_app[rows_to_update] <- "III"
data_output_final$grade_manual_downgrade[rows_to_update] <- "yes"



#f. Changing the row entering in the preference tool for olanzapine+fluoxetine (Efficacy)

data_output_final$preference_final_selector[data_output_final$row_app == 66] <- "no"   # Bahji A., 2020 (NMA)

data_output_final$preference_final_selector[data_output_final$row_app == 2052] <- "yes" # Yildiz A., 2023 (NMA)




rm(rows_to_update, unique_rows, part_5_proceed)

}




table(filter(data_output_final, platform_selector == "yes" 
             & row_reversed_app == "not_reversed")$grade_score_app)

table(data_output_final$grade_manual_downgrade)



file_name = paste0("ebibd_dataset_output_", today_date, ".xlsx")

writexl::write_xlsx(data_output_final, file_name)

rm(file_name, today_date)