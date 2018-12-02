load_data <- function(){
  load("data/sampleAdult.rda")
  load("data/nhisPerson.rda")
}

#' compute_Sleepscore
#'
#' @return numeric
#' @export
#'
#' @examples compute_Sleepscore()
compute_Sleepscore <- function(){
  load_data()
  sleep_variables <- dplyr::select(sampleAdult, ASISLEEP, ASIREST, ASISLPFL, ASISLPST, ASISLPMD)
  compute_score <- function(hours, rest, falling, staying, med){
    score = 0
    if(hours>=7 & hours<=9)
      score = score + 40
    else if(hours==6 | hours==10)
      score = score + 30
    else if(hours<6 | hours>10)
      score = score + 10

    if(rest == 0)
      score = score + 0
    else if(rest == 1 | rest == 2)
      score = score + 5
    else if(rest == 3 | rest == 4)
      score = score + 10
    else if(rest == 5 | rest == 6)
      score = score + 15
    else if(rest == 7)
      score = score + 20

    if(med == 0)
      score = score + 20
    else if(med == 1 | med == 2)
      score = score + 15
    else if(med == 3 | med == 4)
      score = score + 10
    else if(med == 5 | med == 6)
      score = score + 5
    else if(med == 7)
      score = score + 0

    if(staying == 0)
      score = score + 10
    else if(staying == 1 | staying == 2)
      score = score + 9
    else if(staying == 3 | staying == 4)
      score = score + 6
    else if(staying == 5 | staying == 6)
      score = score + 3
    else if(staying == 7)
      score = score + 0

    if(falling == 0)
      score = score + 10
    else if(falling == 1 | falling == 2)
      score = score + 9
    else if(falling == 3 | falling == 4)
      score = score + 6
    else if(falling == 5 | falling == 6)
      score = score + 3
    else if(falling == 7)
      score = score + 0

    return(score)
  }
  sleep_score <- apply(sleep_variables,
                       1,
                       function(oneRow) compute_score(oneRow[1],
                                                      oneRow[2],
                                                      oneRow[3],
                                                      oneRow[4],
                                                      oneRow[5]))
  return(sleep_score)
}

#' get_sleep_metadata
#'
#' @return list
#' @export
#'
#' @examples get_sleep_metadata()
get_sleep_metadata <- function(){
  values <- c("HHX", "FMX", "FPX", "SEX", "HYBPLEV", "AHSTATYR",
                   "SMKSTAT2", "ASIRETR", "ASIMEDC", "ASICNHC", "ASINBILL",
                   "ASICCOLL", "ASIHCST", "ASICCMP", "ASIWTHLS", "DIBTYPE",
                   "CIGSDA1", "R_MARITL", "PHSTAT", "PDMED12M", "MCPART",
                   "PLBORN", "CITIZENP", "WRKHRS2", "PNMED12M", "GEOBRTH")
  displayName <- c("Household Number", "Family Number", "Person Number within family", "Male or Female",
              "At that time, were you told that your blood pressure was high, normal, or low?",
              "Compared with 12 MONTHS AGO, would you say your health is better, worse, or about the same?",
              "Have you smoked at least 100 cigarettes in your ENTIRE LIFE? ",
              "How worried are you right now about not having enough money for retirement?",
              "How worried are you right now about not being able to pay medical costs of a serious illness or accident?",
              "How worried are you right now about not being able to pay medical costs for normal healthcare?",
              "How worried are you right now about not having enough to pay your normal monthly bills?",
              "How worried are you right now about not having enough money to pay for your children's college?",
              "How worried are you right now about not being able to pay your rent, mortgage, or other housing costs?",
              "How worried are you right now about not being able to make the minimum payments on your credit cards?",
              "During the PAST 30 DAYS, how often did you feel...worthless?",
              "What type of diabetes do you have?",
              "On the average, how many cigarettes do you now smoke a day?",
              "(Are/Is)(you/person) now married, widowed, divorced, separated, never married, or living with a partner?",
              "Would you say your health in general is excellent, very good, good, fair, or poor?",
              "DURING THE PAST 12 MONTHS, has medical care been delayed for {person} because of worry about the cost?",
              "What type of Medicare {do you/does person} have? Is it Part A - hospital insurance, Part B - medical insurance, or both?",
              "Were you born in the United States?",
              "Are you a CITIZEN of the United States?",
              "How many hours do you USUALLY work at ALL jobs or businesses?",
              "DURING THE PAST 12 MONTHS, was there any time when you needed medical care, but did not get it because you couldn't afford it?",
              "Geographic place of birth"
              )

  result <- setNames(as.list(values), displayName)
  return(result)
}
