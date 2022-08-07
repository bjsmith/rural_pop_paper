library(dplyr)


get_orfcs_survey_data <- function(data_dir){
  
  
  
  pilot_1 <- readr::read_csv(paste0(data_dir, "cloudresearch_survey_results/Oregon Rural Food Consumption Survey Bespoke CR_October 18, 2021_11.34_pilot1.csv"))
  pilot_2 <- readr::read_csv(paste0(data_dir, "cloudresearch_survey_results/ORFCS Bespoke CR2_October 18, 2021_11.31_pilot2.csv"))
  
  complete_1 <- readr::read_csv(paste0(data_dir, "cloudresearch_survey_results/ORFCS Bespoke CR2_November 9, 2021_13.52.csv"))
  
  
  common_cols <- intersect(intersect(colnames(pilot_1),colnames(pilot_2)),colnames(complete_1))
  
  # cols_to_extract <- c("aid","WEIGHT","HEIGHT_1","LocationLatitude","geo_lat","LocationLongitude","geo_lng", "AddressNum_1", "Address_1","Address_2", "Address_3","DEMO_6","Duration (in seconds)","Finished")
  pilot_1_mergedata <- pilot_1[,common_cols]
  pilot_1_mergedata$survey <- "pilot1"
  pilot_2_mergedata <- pilot_2[,common_cols]
  pilot_2_mergedata$survey <- "pilot2"
  
  complete_1_mergedata <- complete_1[,common_cols]
  complete_1_mergedata$survey <- "complete1"
  
  full_merged_results <- rbind(pilot_1_mergedata,pilot_2_mergedata) %>% rbind(complete_1_mergedata)
  

  return(full_merged_results)
}

match_ppts_with_ruca_zipcode <- function(data_dir, full_merged_results){
  #Based on `match_ppt_to_ruca_with_zip_code.Rmd`.
  
  ruca2010_zip_code_descriptions <- readxl::read_xlsx(paste0(data_dir , "RUCA2010zipcode.xlsx" ),sheet = 1)
  ruca2010_zip_codes <- readxl::read_xlsx(paste0(data_dir , "RUCA2010zipcode.xlsx" ),sheet = "Data")
  ruca2010_zip_codes_details <- readxl::read_xlsx(paste0(data_dir , "RUCA2010zipcode.xlsx" ),sheet = "RUCA code description")
  ruca1010_zip_code_ruca_description_table <- readr::read_csv(paste0(data_dir,"RUCA2010zipcode_primary_code_key.csv"))
  
  #Now we want to get the RUCA codes.
  #Now, match the participant ZIP Codes to the RUCA 2010
  
  ppts_with_zip_ruca <- merge(full_merged_results,ruca2010_zip_codes,by.x="PptZIPCode",by.y="ZIP_CODE",all.x=TRUE,all.y=FALSE)
  ppts_with_zip_ruca <- merge(ppts_with_zip_ruca,ruca1010_zip_code_ruca_description_table, by.x="RUCA1",by.y="PrimaryCode",all.x=TRUE,all.y=FALSE)
  
  return(ppts_with_zip_ruca)
  
}

match_ppts_with_zipcode_data <- function(data_dir, full_merged_results){
  median_income_data_all <- readr::read_csv(paste0(
    data_dir,
    "median-incomes-ACSST5Y2020.S1903_2022-04-18T143338/ACSST5Y2020.S1903_data_with_overlays_2022-03-18T145451.csv"
  ),skip=1
  )
  #there's only one column we need out of all of this
  zipcode_data_selected <- median_income_data_all %>% 
    dplyr::select(`Geographic Area Name`,
           `Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households`)
  #now tidy it up.
  
  zipcode_data_selected <- zipcode_data_selected %>% 
    mutate(ZIPCode = stringr::str_extract(`Geographic Area Name`,pattern = "\\d\\d\\d\\d\\d")) %>%
    mutate(`Geographic Area Name` = NULL) %>% 
    rename(
    ZipcodeMedianHouseholdIncomeRaw = `Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households`
    )
  
  
  zipcode_data_selected <- zipcode_data_selected %>% mutate(ZipcodeMedianHouseholdIncome = case_when(
      ZipcodeMedianHouseholdIncomeRaw=="2,500-" ~ 2500,
      ZipcodeMedianHouseholdIncomeRaw=="250,000+" ~ 250000,
      trimws(ZipcodeMedianHouseholdIncomeRaw)=="-" ~ as.numeric(NA),
      TRUE ~ as.numeric(ZipcodeMedianHouseholdIncomeRaw)
      # is.numeric(ZipcodeMedianHouseholdIncomeRaw)
    ))
  
  # zipcode_data_selected %>% filter(is.na(ZipcodeMedianHouseholdIncome)) %>% .$ZipcodeMedianHouseholdIncomeRaw %>% table
  ppts_with_zipcode_info <- merge(full_merged_results,zipcode_data_selected,all.x=TRUE,all.y=FALSE,by.x="PptZIPCode",by.y="ZIPCode")
  return(ppts_with_zipcode_info)
}

get_demo_data <- function(demo_data_long,acs_income_path){
  #set up the code that grabs all this stuff.
  get_demo_data_single_col <- function(item_code, scored_scale){
    item_rows <- demo_data_long[demo_data_long$item==item_code,]
    item_rows$scale_name <- "DEMO"
    item_rows$scored_scale <- scored_scale
    item_rows <- rename(item_rows, score=value)
    item_rows$n_items <- 1
    item_rows$n_missing <- 0
    item_rows[is.na(item_rows$score),]$n_items <- 0
    item_rows[is.na(item_rows$score),]$n_missing <- 1
    item_rows$method <- NA
    item_rows<-item_rows%>% select(survey_name, scale_name, scored_scale,SID,score,n_items,n_missing,method)
    return(item_rows)
    
  }
  
  
  #survey contains two separate sections on demographics with quite a few repeated questiosn
  #I've tried to summarize and compare here
  
  #SES questionnaire
  hh_income <- get_demo_data_single_col("aSES_02","household_income")
  hh_dependent_sum <- get_demo_data_single_col("aSES_08","household_dependents")
  hh_members_grandparents_other <- get_demo_data_single_col("aSES_07","household_members_grandparents_other")
  hh_members_children <- get_demo_data_single_col("aSES_05","household_members_children")
  hh_members_spouse <- get_demo_data_single_col("aSES_04","household_members_spouse")
  
  #DEMO questionnaire
  hh_income_level <- get_demo_data_single_col("DEMO_5","household_income_level")
  hh_size <- get_demo_data_single_col("DEMO_7","household_size")
  
  
  
  hh_data <- data.frame(do.call(rbind,list(hh_dependent_sum,hh_members_grandparents_other,hh_members_children,hh_members_spouse,hh_income_level,hh_size)))
  
  #flip it out wide to see how well these match
  hh_wider <- hh_data %>% pivot_wider(id_cols = "SID",names_from="scored_scale",values_from="score")
  hh_wider$household_min <- ifelse(hh_wider$household_members_children=="1",1,0) + ifelse(hh_wider$household_members_spouse=="1",1,0) + ifelse(hh_wider$household_members_grandparents_other=="1",1,0) + 1
  
  #I think we should use household size. People have misinterpreted the "dependents" question quite substantially.
  
  #then for household per capita income, we need to code in terms of exact numeric will be
  hh_income_level_medamount <- hh_data %>%
    filter(scored_scale=="household_income_level") %>%
    mutate(
      scored_scale="household_income_level_medamount",
      score = case_when(
        score=="1" ~ 25000/2,
        score=="2" ~ mean(25000,40000),
        score=="3" ~ mean(40000,75000),
        score=="4" ~ mean(75000,100000),
        score=="5" ~ 100000,
        TRUE ~ as.numeric(NA)))
  
  hh_data <- data.frame(do.call(rbind,list(hh_data,hh_income_level_medamount)))
  
  hh_income_per_person <- hh_data %>% 
    filter(scored_scale %in% c("household_income_level_medamount","household_size")) %>%
    pivot_wider(id_cols = c("survey_name","scale_name", "SID"),names_from="scored_scale",values_from="score") %>% 
    mutate("score" = as.numeric(household_income_level_medamount)/as.numeric(household_size),
           "method"=NA,n_items=NA,n_missing=NA,
           scored_scale="household_income_per_person") %>%
    select(-household_income_level_medamount,-household_size)
  
  hh_data <- data.frame(do.call(rbind,list(hh_data,hh_income_per_person)))
  
  
  
  #DEMO questionnaire more
  demo_social_standing <- get_demo_data_single_col("DEMO_8","mcarthur_social_standing")
  own_education_level <- get_demo_data_single_col("DEMO_4","education_own")
  own_education_level[own_education_level$score=="14" & !is.na(own_education_level$score),]$score<-as.character(NA)
  table(own_education_level$score)
  demo_indiv_income <- get_demo_data_single_col("aSES_01","individual_income")
  demo_mothers_education <- get_demo_data_single_col("Q20","mother_education")
  demo_fathers_education <- get_demo_data_single_col("Q21","father_education")
  
  
  
  
  #zipcode data
  demo_zipcode_data <- get_demo_data_single_col("DEMO_6","zipcode")
  
  #using ACS data
  acs_zip_income_data_raw <- readr::read_csv(acs_income_path,skip = 1)
  acs_zip_income_data <- 
    acs_zip_income_data_raw %>% 
    select(id, `Geographic Area Name`, `Estimate!!Households!!Median income (dollars)`, `Estimate!!Households!!Mean income (dollars)`) %>%
    rename(EstimateHouseholdMedianIncome = `Estimate!!Households!!Median income (dollars)`) %>%
    rename(EstimateHouseholdMeanIncome = `Estimate!!Households!!Mean income (dollars)`) %>%
    mutate(Zip = str_match(`Geographic Area Name`,"ZCTA5 (.*)$")[,2]) %>%
    select(-`Geographic Area Name`)
  
  demo_zipcode_moredata <- demo_zipcode_data %>% merge(acs_zip_income_data,by.x="score",by.y="Zip")
  demo_zipcode_median_income <- demo_zipcode_moredata %>% 
    mutate(score=EstimateHouseholdMedianIncome,scored_scale="zipcode_median_income_acs") %>% 
    select(colnames(demo_zipcode_data))
  demo_zipcode_mean_income <- demo_zipcode_moredata %>% 
    mutate(score=EstimateHouseholdMedianIncome,scored_scale="zipcode_mean_income_acs") %>% 
    select(colnames(demo_zipcode_data))
  #demo_zipcode_population <- demo_zipcode_moredata %>% mutate(score=Pop,scored_scale="zipcode_pop_2010") %>% select(colnames(demo_zipcode_data))
  demo_zipcode_info <- do.call(rbind,list(demo_zipcode_data,demo_zipcode_median_income,demo_zipcode_mean_income,
                                          demo_mothers_education,demo_fathers_education))
  
  
  
  demo_all<- do.call(rbind,list(demo_social_standing,own_education_level,demo_zipcode_info,hh_data,demo_indiv_income))
  return(demo_all)
}

zscore <- function(x){
  return((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE))
}



extract_and_label_bootstrap_results<-function(fit_obj){
  fit_to_read <- fit_obj
  #sink("temp_diversion.txt")
  fit_pe<-summary(fit_to_read,ci=TRUE)$PE
  #sink(NULL)
  
  #the main purpose of this function is that we have to select only the "base" labelled coefficients and not the derived ones.
  
  print(nrow(fit_pe))
  fit_pe$index<-1:nrow(fit_pe)
  labelled_coefficients <- fit_pe %>% filter(label!="")
  
  #get the coefficients
  fit_to_read_coef<-fit_to_read@boot$coef
  #find out how many there are
  max_rows_estimated = dim(fit_to_read_coef)[2]
  print(dim(fit_to_read_coef))
  
  #only get the rows where we actually have estimates
  labelled_coefficients_with_estimates <- labelled_coefficients[labelled_coefficients$index<=max_rows_estimated,]
  
  row_ids_to_fetch <- labelled_coefficients_with_estimates$index #labelled_coefficients$index[1:rows]
  
  bootstrap_results<-fit_to_read_coef[,row_ids_to_fetch]
  
  colnames(bootstrap_results)<-labelled_coefficients_with_estimates$label
  return(bootstrap_results)
}



get_two_tailed_score <- function(z_score){
  return((1-pnorm(abs(z_score)))*2)
}

summarize_calculated_effect <- function(effect_vector){
  effect_mean <- mean(effect_vector)
  effect_se <- sd(effect_vector)
  #the z-value here is exactly what lavaan displays.
  #it's going to be a bit different
  #because the mean and se I get are both a bit different for reasons that are unclear to me.
  effect_z_value <- effect_mean/effect_se
  effect_p_value <- get_two_tailed_score(effect_z_value)
  
  cat(paste("Estimate=",signif(effect_mean,4),", Std. Err=",signif(effect_se,4),", z-value=",signif(effect_z_value,4), ", p-value=",signif(effect_p_value,4)))
  cat("\n")
  
  return(data.frame(
    "est"=effect_mean,
    "se"=effect_se,
    "pvalue"=effect_p_value
  ))
  #print(t.test(hybrid_indirect_effect))
  #cat("\n")
}

p_value_asterisks<-function(pvalue){
  p_value_asterisks_by_scalar <- function(pvalue){
    if (pvalue<0.001){
      return("***")
    }else if (pvalue<0.01){
      return("**")
    }else if (pvalue<0.05){
      return("*")
    }else if (pvalue<0.1){
      return("+")
    }
    return("")
  }
  return(sapply(pvalue,p_value_asterisks_by_scalar))
}