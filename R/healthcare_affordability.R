
#' Plot based on Affordability score and age group
#'
#' @return Bar Plot
#' @export
#'
#' @examples plot_afford_score()
plot_afford_score <- function(){

  data_afford <- get_data_affordscore()
  data_afford<-dplyr::mutate(data_afford,Age_Group=ifelse((AGE_P>=0 & AGE_P<=18),"Below 18",ifelse((AGE_P>18 & AGE_P<=24),"18-24",ifelse((AGE_P>24 & AGE_P<=40),"25-40",ifelse((AGE_P>40 & AGE_P<=60),"40-60",ifelse((AGE_P>60 & AGE_P<85),"60-85","85+"))))))
  data_afford <- dplyr::mutate(data_afford,Affordability=ifelse((Affordability==1),"Afford One",ifelse((Affordability==2),"Afford Both","Afford none")))
  ggplot2::ggplot(data_afford) + ggplot2::geom_bar(ggplot2::aes(x = Age_Group, fill=Affordability),position="dodge")
}



#' Affordability Score
#' Gives a dataframe containing persons unique number and affordability score of 0 ,1 or 2 based on persons ability to afford dental care and eyeglasses in the past 12 months. 1 reflects can afford one. 2 reflects both and 0 none.
#' @return dataframe
#' @export
#'
#' @examples compute_Affordability_Score()
compute_affordability_Score<-function(){

  data<-get_afford_data()

  data <- dplyr::filter(data,!is.na(AHCAFYR4) & !is.na(AHCAFYR3))

  data <- dplyr::mutate(data,Affordability=ifelse((AHCAFYR4==1 & AHCAFYR3==1),0,ifelse((AHCAFYR4==2 & AHCAFYR3==2),2,1)))
  return(dplyr::select(data, HHX,FMX,FPX,Affordability))
}

#' Healthcare Affordability metadata
#' Returns a list of variable name with their description for better readability.
#' @return list
#' @export
#'
#' @examples get_afford_metadata()
get_afford_metadata <- function(){

  values <- c("HHX","FMX", "FPX", "AHCAFYR4","AHCAFYR3","AHCCHGYR","AGE_P","AHICOMP","PRDNCOV1","PRDNCOV2",
              "MEDBILL","HINOTYR","HIEMPOF","ERNYR_P","LAHCA1","SEX","CITIZENP","LAHCC1","LCUNIT1","LACHRC1","LCCHRC1","HINOTMYR",
              "NOTCOV","COVER65","COVER65O","MCPART","MDPRINC","HICOSTR1","HISTOP1","HISTOP2","HISTOP3",
              "HISTOP4","HISTOP5","HISTOP6","HISTOP7","HISTOP8","HISTOP11","HISTOP12",
              "HISTOP13","HISTOP14","HISTOP15","FCOVCONF","PLNMGD2","REGIONBR",
              "FHICHNG","YRSINUS")
  displayName <- c("Household Number", "Family Number","Person Number", "Couldn't afford eyeglasses, past 12 m",
                   "Couldn't afford dental care, past 12 m","Change health care place, past 12 m","Age",
                   "Compared with 12 MONTHS AGO, would you say your health is better, worse, or about the same?",
                   "If you have 2 private plans,Does private plan 1 pay for any of the costs for dental care? ",
                   "If you have 2 private plans,Does private plan 2 pay for any of the costs for dental care? ",
                   "In the past 12 months did you face problems paying medical bills?",
                   "In the past 12 months, was there any time when you did not have any health insurance or coverage?",
                   "Was Health insurance offered at workplace?",
                   "What were your total earnings last year?",
                   "If your age is above 18,does your vision cause limitation?",
                   "What is your gender?",
                   "Are you a CITIZEN of the United States?",
                   "If your age is below 18,does your vision cause limitation?",
                   "Is your duration of vision problem days,weeks,months,year or since birth,specify? ",
                   "If your age is above 18, what is your vision problem condition status? chronic or Non-chronic?",
                   "If your age is below 18,what is your vision problem condition status? chronic or Non-chronic?",
                   "Months without coverage in past 12 months?",
                   "Health coverage status as used in Health United States,not covered or covered?",
                   "Health insurance hierarchy for people aged 65+.",
                   "Original health insurance hierarchy aged 65+ given by private persons.",
                   "Type of coverage given by Medicare, Hospital or Medical or both?",
                   "Is the premium paid for this Medicaid plan based on income?",
                   "Out-of-pocket premium cost?",
                   "Is lost job or changed employers the reason you stopped being covered or not have health insurance?",
                   "Is divorced/sep/death of spouse or parent employers the reason you stopped being covered or not have health insurance?",
                   "Is ineligibility because of age/left school the reason you stopped being covered or not have health insurance?",
                   "Is employer does not offer/not eligible for cov the reason you stopped being covered or not have health insurance?",
                   "Is cost being too high the reason you stopped being covered or not have health insurance?",
                   "Is insurance company refused coverage the reason you stopped being covered or not have health insurance?",
                   "Is medicaid/medi plan stopped after pregnancy the reason you stopped being covered or not have health insurance?",
                   "Is lost Medicaid/new job/increase in income the reason you stopped being covered or not have health insurance?",
                   "Did you never have health insurance?",
                   "Is moving from another county/state/country the reason you stopped being covered or not have health insurance?",
                   "Is self-employment the reason you stopped being covered or not have health insurance?",
                   "Do you have no need for itor you choose not to have health insurance?",
                   "Is getting married the reason you stopped being covered or not have health insurance?",
                   "All families with an employer-based health plan, how confident are you of getting affordable health insurance on your own without any help,Very confident,Somewhat confident,Not too confident,Not confident at all?",
                   "What type of private plan do you use?",
                   "What is your geographic region of birth?",
                   "No change in coverage in past 12 months?",
                   "Years since your migration to the U.S.?"

  )
  result <- setNames(as.list(displayName), values)
  return(result)
}

#' Healthcare Affordability metadata with score
#' Returns a list of variable name with their description for better readability.
#'
#' @return list
#' @export
#'
#' @examples get_affordscore_metadata()
get_affordscore_metadata<-function(){

  values <- c("HHX","FMX", "FPX", "AHCAFYR4","AHCAFYR3","AHCCHGYR","AGE_P","AHICOMP","PRDNCOV1","PRDNCOV2",
              "MEDBILL","HINOTYR","HIEMPOF","ERNYR_P","LAHCA1","SEX","CITIZENP","LAHCC1","LCUNIT1","LACHRC1","LCCHRC1","HINOTMYR",
              "NOTCOV","COVER65","COVER65O","MCPART","MDPRINC","HICOSTR1","HISTOP1","HISTOP2","HISTOP3",
              "HISTOP4","HISTOP5","HISTOP6","HISTOP7","HISTOP8","HISTOP11","HISTOP12",
              "HISTOP13","HISTOP14","HISTOP15","FCOVCONF","PLNMGD2","REGIONBR",
              "FHICHNG","YRSINUS","Affordability")
  displayName <- c("Household Number", "Family Number","Person Number", "Couldn't afford eyeglasses, past 12 m",
                   "Couldn't afford dental care, past 12 m","Change health care place, past 12 m","Age",
                   "Compared with 12 MONTHS AGO, would you say your health is better, worse, or about the same?",
                   "If you have 2 private plans,Does private plan 1 pay for any of the costs for dental care? ",
                   "If you have 2 private plans,Does private plan 2 pay for any of the costs for dental care? ",
                   "In the past 12 months did you face problems paying medical bills?",
                   "In the past 12 months, was there any time when you did not have any health insurance or coverage?",
                   "Was Health insurance offered at workplace?",
                   "What were your total earnings last year?",
                   "If your age is above 18,does your vision cause limitation?",
                   "What is your gender?",
                   "Are you a CITIZEN of the United States?",
                   "If your age is below 18,does your vision cause limitation?",
                   "Is your duration of vision problem days,weeks,months,year or since birth,specify? ",
                   "If your age is above 18, what is your vision problem condition status? chronic or Non-chronic?",
                   "If your age is below 18,what is your vision problem condition status? chronic or Non-chronic?",
                   "Months without coverage in past 12 months?",
                   "Health coverage status as used in Health United States,not covered or covered?",
                   "Health insurance hierarchy for people aged 65+.",
                   "Original health insurance hierarchy aged 65+ given by private persons.",
                   "Type of coverage given by Medicare, Hospital or Medical or both?",
                   "Is the premium paid for this Medicaid plan based on income?",
                   "Out-of-pocket premium cost?",
                   "Is lost job or changed employers the reason you stopped being covered or not have health insurance?",
                   "Is divorced/sep/death of spouse or parent employers the reason you stopped being covered or not have health insurance?",
                   "Is ineligibility because of age/left school the reason you stopped being covered or not have health insurance?",
                   "Is employer does not offer/not eligible for cov the reason you stopped being covered or not have health insurance?",
                   "Is cost being too high the reason you stopped being covered or not have health insurance?",
                   "Is insurance company refused coverage the reason you stopped being covered or not have health insurance?",
                   "Is medicaid/medi plan stopped after pregnancy the reason you stopped being covered or not have health insurance?",
                   "Is lost Medicaid/new job/increase in income the reason you stopped being covered or not have health insurance?",
                   "Did you never have health insurance?",
                   "Is moving from another county/state/country the reason you stopped being covered or not have health insurance?",
                   "Is self-employment the reason you stopped being covered or not have health insurance?",
                   "Do you have no need for itor you choose not to have health insurance?",
                   "Is getting married the reason you stopped being covered or not have health insurance?",
                   "All families with an employer-based health plan, how confident are you of getting affordable health insurance on your own without any help,Very confident,Somewhat confident,Not too confident,Not confident at all?",
                   "What type of private plan do you use?",
                   "What is your geographic region of birth?",
                   "No change in coverage in past 12 months?",
                   "Years since your migration to the U.S.?",
                   "This is your affordability Score."

  )
  result <- setNames(as.list(displayName), values)
  return(result)
}


