library(foreign)
library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)

get_hints5 <- function(location = "/Users/benjaminsmith/Dropbox (University of Oregon)/protected_files_ben_only/protected_HINTS_data/"){
  hines_locations <- Sys.glob(file.path(location,"*/hints5*.sav"))
  
  raw_file_list <- vector(mode="list",length<-length(hines_locations))
  
  for (i in 1:length(raw_file_list)){
    hines_filepath <- hines_locations[[i]]
    filename <- basename(hines_filepath)
    spss_rawfile <- read.spss(hines_filepath, use.value.label=TRUE,to.data.frame=TRUE)
    spss_rawfile[["filename"]] <- filename
    raw_file_list[[i]] <- spss_rawfile
  }
  
  
  detect_key_cols <- function(spss_item){
    print(spss_item[["filename"]][[1]])
    print("Race_Cat2" %in% colnames(spss_item))
    print(paste0("PR_RUCA_2010", "PR_RUCA_2010" %in% colnames(spss_item)))
    print(paste0("RUC2013", "RUC2013" %in% colnames(spss_item)))
    
    print("Age" %in% colnames(spss_item))
    print("BMI" %in% colnames(spss_item))
  }
  
  lapply(raw_file_list,detect_key_cols)
  
  shared_colnames <- colnames(raw_file_list[[1]])
  for (i in 2:length(raw_file_list)){
    shared_colnames <- intersect(colnames(raw_file_list[[i]]),shared_colnames)
  }
  
  #we want a few eextra important columns of interest
  cols_to_use <- c(shared_colnames,"Fruit","Vegetables")
  
  #then trim each file back to just these shared columns.
  #we've checked it will include the ones we want.
  file_list_trimmed <- lapply(
    raw_file_list,
    function(cycle_5_df){
      return(cycle_5_df[intersect(cols_to_use,colnames(cycle_5_df))])
    }
    
  )
  
  hints5 <- rbindlist(file_list_trimmed,fill=TRUE)
  # 
  # lapply(
  #   file_list_trimmed,
  #   function(cycle_5_df){
  #     print(table(cycle_5_df[["PR_RUCA_2010"]]))
  #     #colnames(cycle_5_df)
  #   }
  #   
  #preprocess
  hints5$Age <- as.numeric(as.character(hints5$Age))
  hints5$BMI <- as.numeric(as.character(hints5$BMI))
  
  hints5$Age_c <- hints5$Age-38 #using U.S. median age

  hints5$HINTSIsRural <- grepl("Metropolitan",hints5$PR_RUCA_2010)==FALSE
  hints5$HINTSIsRural_i <- as.numeric(hints5$HINTSIsRural)
  
  
  hints5$RacePreprocessed<- stringr::str_replace(hints5$Race_Cat2," only","")
  #replace small categories
  hints5$RacePreprocessed[hints5$RacePreprocessed %in% c("Guamanian or Chamorro","Native Hawaiian","Other Pacific Islander")]<-"Pacific Islander"
  race_pp_levels <- names(sort(table(hints5$RacePreprocessed),decreasing = TRUE))
  #make "white" category first, because it's the majority category
  hints5$RacePreprocessed2<-hints5$RacePreprocessed #combine a few extra categories to enable processing in lavaan
  hints5$RacePreprocessed2[hints5$RacePreprocessed2 %in% c("Missing data (Not Ascertained)","Missing data (Web partial - Question Never Seen)")]<-"(Missing Data)"
  race_pp2_levels <- names(sort(table(hints5$RacePreprocessed2),decreasing = TRUE))
  
  hints5$HHInc[hints5$HHInc=="Missing Data (Not Ascertained)"]<-NA
  hints5$HHInc[hints5$HHInc=="Missing data (Web partial - Question Never Seen)"]<-NA
  median_hh_inc<-median(as.integer(hints5$HHInc),na.rm=TRUE)
  hints5$HHInc_r <- as.integer(hints5$HHInc)-median_hh_inc
  
  hints5$IncomeRanges[hints5$IncomeRanges=="Missing Data"]<-NA
  hints5$IncomeRanges[hints5$IncomeRanges=="Missing data (Web partial - Question Never Seen)"]<-NA
  hints5$IncomeRanges[hints5$IncomeRanges=="Missing data (Not Ascertained)"]<-NA
  median_income_range <- median(as.integer(hints5$IncomeRanges),na.rm=TRUE)
  hints5$IncomeRanges_r <- as.integer(hints5$IncomeRanges) - median_income_range
  
  hints5$Fruit[
    hints5$Fruit %in% c("Missing data (Not Ascertained)","Multiple responses selected in error","Missing data (Web partial - Question Never Seen)")] <- NA
  
  #some recoding now
  hints5$Fruit[hints5$Fruit=="½ cup or less"]<-"1/2 cup or less"
  hints5$Fruit[hints5$Fruit==" ½ cup to 1 cup"]<-"1/2 cup to 1 cup"
  table(hints5$Fruit)
  #now code factors into integers.
  hints5$Fruit_i <- as.integer(NA)
  hints5$Fruit_i[hints5$Fruit=="None"]<-0
  hints5$Fruit_i[hints5$Fruit=="1/2 cup or less"]<-1
  hints5$Fruit_i[hints5$Fruit=="1/2 cup to 1 cup"]<-2
  hints5$Fruit_i[hints5$Fruit=="1 to 2 cups"]<-3
  hints5$Fruit_i[hints5$Fruit=="2 to 3 cups"]<-4
  hints5$Fruit_i[hints5$Fruit=="3 to 4 cups"]<-5
  hints5$Fruit_i[hints5$Fruit=="4 or more cups"]<-6
  table(hints5$Fruit_i,hints5$Fruit)
  hints5$Fruit_z <- (hints5$Fruit_i - mean(hints5$Fruit_i,na.rm=TRUE))/sd(hints5$Fruit_i,na.rm=TRUE)
  
  hints5$Vegetables[
    hints5$Vegetables %in% c("Missing data (Not Ascertained)","Multiple responses selected in error","Missing data (Web partial - Question Never Seen)")] <- NA
  
  hints5$Vegetables[hints5$Vegetables=="½ cup or less"]<-"1/2 cup or less"
  hints5$Vegetables[hints5$Vegetables==" ½ cup to 1 cup"]<-"1/2 cup to 1 cup"
  table(hints5$Vegetables)
  #now code factors into integers.
  hints5$Vegetables_i <- as.integer(NA)
  hints5$Vegetables_i[hints5$Vegetables=="None"]<-0
  hints5$Vegetables_i[hints5$Vegetables=="1/2 cup or less"]<-1
  hints5$Vegetables_i[hints5$Vegetables=="1/2 cup to 1 cup"]<-2
  hints5$Vegetables_i[hints5$Vegetables=="1 to 2 cups"]<-3
  hints5$Vegetables_i[hints5$Vegetables=="2 to 3 cups"]<-4
  hints5$Vegetables_i[hints5$Vegetables=="3 to 4 cups"]<-5
  hints5$Vegetables_i[hints5$Vegetables=="4 or more cups"]<-6
  table(hints5$Vegetables_i,hints5$Vegetables)
  hints5$Vegetables_z <- (hints5$Vegetables_i - mean(hints5$Vegetables_i,na.rm=TRUE))/sd(hints5$Vegetables_i,na.rm=TRUE)
  
  zscore <- function(x){
    return((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE))
  }
  
  hints5$Fruit_vegetables_z <- zscore(hints5$Fruit_i + hints5$Vegetables_i)
  hints5$RacePreprocessed <- factor(hints5$RacePreprocessed,race_pp_levels)
  hints5$RacePP <- hints5$RacePreprocessed
  hints5$RacePreprocessed2 <- factor(hints5$RacePreprocessed2,race_pp2_levels)
  hints5$RacePP2 <- hints5$RacePreprocessed2
  
  gender_text <- as.character(hints5$SelfGender)
  hints5$Gender3C <- factor(gender_text,levels = c("Female","Male","MissingOrMultiple"))
  hints5$Gender3C[is.na(hints5$Gender3C)]<- "MissingOrMultiple"
  
  hints5$Hisp_Cat2 <- stringr::str_replace(hints5$Hisp_Cat," only","")
  hints5$Hisp_Cat2[hints5$Hisp_Cat2 %in% c("Missing data (Web partial - Question Never Seen)", "Missing data (Not Ascertained)")] <- "Unknown"
  hisp_cat2_levels <- names(sort(table(hints5$Hisp_Cat2),decreasing = TRUE))
  hints5$Hisp_Cat2 <- factor(hints5$Hisp_Cat2,hisp_cat2_levels)
  
  hints5$GeneralHealth <- stringr::str_replace(hints5$GeneralHealth,",","")
  hints5$GeneralHealth <- stringr::str_replace(hints5$GeneralHealth,"\\?","")
  hints5$GeneralHealth <- stringr::str_replace(hints5$GeneralHealth," or","")
  hints5$GeneralHealth_of <- factor(hints5$GeneralHealth,levels=c("Excellent","Very good","Good","Fair","Poor"))
  
  hints5$GeneralHealth_i <- as.integer(hints5$GeneralHealth_of)
  hints5$GeneralHealth_z <- zscore(hints5$GeneralHealth_i)
 
  MedConditions_vars <- c("MedConditions_Diabetes", "MedConditions_HighBP", "MedConditions_HeartCondition")
  for (var_to_factorize in c(MedConditions_vars,"EverHadCancer")){
    hints5[[paste0(var_to_factorize,"_d")]]<-factor(hints5 %>% dplyr::select(var_to_factorize) %>% .[[1]],levels=c("No","Yes"))
    hints5[[paste0(var_to_factorize,"_i")]]<-as.integer(hints5[[paste0(var_to_factorize,"_d")]]=="Yes")
  }
  #take the count of the number of these three med conditions in an aggregate variable.
  #note we're note including hte cancer diagnosis, which was pre-registered separately.
  hints5$MedConditionsAggregate <- rowSums(hints5 %>% dplyr::select(sapply(MedConditions_vars,function(x){paste0(x,"_i")})))
  
  hints5$EducationModified<-as.character(hints5$Education)
  hints5$EducationModified[
    hints5$Education %in% 
      c("Post high school training other than college (vocational or ",
        "Some college"
        )]<-"Post high school training or some college"
  hints5$EducationOrdinal <- factor(
    hints5$EducationModified,
    levels=c('Less than 8 years', '8 through 11 years', '12 years or completed high school', 
             'Post high school training or some college', 'College graduate', 'Postgraduate'),
    ordered = TRUE
    )
  
  return(hints5)
}