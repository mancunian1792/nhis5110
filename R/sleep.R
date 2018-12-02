# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

load_data <- function(){
  load("data/sampleAdult.rda")
  load("data/nhisPerson.rda")
}

get_sleep_data <- function(){
  load_data()
  adult_sleep_set <- dplyr::select(sampleAdult, HHX, FMX, FPX, SEX, HYBPLEV,
                                   AHSTATYR, SMKSTAT2, ASIRETR, ASIMEDC, ASICNHC,
                                   ASINBILL, ASICCOLL, ASIHCST, ASICCMP, ASIWTHLS,
                                   DIBTYPE, CIGSDA1,

                                   # ONEJOB, LOCALL1B, HYPDIFV,
                                   # PREGNOW, CHPAIN6M,
                                   # ALCHRC17, ALCHRC29, SMKSTAT2, SMKQTY,
                                   # CIGSDA2, CIGSDAY, ECIGCUR2,
                                   # VIGNO, ALCSTAT, ALC12MWK, BMI, ASICPUSE,
                                   # ASISTLV, YTQU_YG1
                                   )

  person_sleep_set <- dplyr::select(nhisPerson, HHX, FMX, FPX, R_MARITL, PHSTAT,
                                    PDMED12M, MCPART, PLBORN, CITIZENP, WRKHRS2,
                                    PNMED12M, GEOBRTH,

                                    # PHCDVN2W, NOTCOV, HCSPFYR, MEDBNOP,
                                    # WRKFTALL, ERNYR_P, PSAL, PSEINC, PSSRR,
                                    # PSSRRDB, PSSRRD, PPENS, POPENS, PSSI, PSSID,
                                    # PTANF, POWBEN, PINTRSTR, PDIVD, PCHLDSP, PINCOT
                                    )

  sleep_set <- dplyr::inner_join(adult_sleep_set, person_sleep_set, by = c("HHX" = "HHX", "FMX" = "FMX", "FPX" = "FPX"))

  sleep_set <- naniar::replace_with_na(sleep_set, replace = list( R_MARITL = c(7, 8, 9), PHSTAT = c(7, 8, 9),
                                                                  PDMED12M = c(7, 8, 9), PNMED12M = c(7, 8, 9),
                                                                  MCPART = c(7, 8, 9), PLBORN = c(7, 8, 9),
                                                                  CITIZENP = c(7, 8, 9), GEOBRTH = c(7, 8, 9),
                                                                  HYBPLEV = c(7, 8, 9), DIBTYPE = c(7, 8, 9),
                                                                  AHSTATYR = c(7, 8, 9), SMKSTAT2 = c(7, 8, 9),
                                                                  WRKHRS2 = c(97, 98, 99), CIGSDA1 = c(97, 98, 99),
                                                                  ASIRETR = c(7, 8, 9), ASIMEDC = c(7, 8, 9),
                                                                  ASICNHC = c(7, 8, 9), ASICCOLL = c(7, 8, 9),
                                                                  ASICNHC = c(7, 8, 9), ASINBILL = c(7, 8, 9),
                                                                  ASIHCST = c(7, 8, 9), ASICCMP = c(7, 8, 9),
                                                                  ASIWTHLS = c(7, 8, 9)))

  sleep_set <- dplyr::filter(sleep_set, !is.na(R_MARITL) | !is.na(PHSTAT) | !is.na(PDMED12M) | !is.na(PNMED12M) |
                               !is.na(MCPART) | !is.na(PLBORN) | !is.na(CITIZENP) | !is.na(GEOBRTH) |
                               !is.na(HYBPLEV) | !is.na(DIBTYPE) | !is.na(AHSTATYR) | !is.na(SMKSTAT2) |
                               !is.na(WRKHRS2) | !is.na(CIGSDA1) | !is.na(ASIRETR) | !is.na(ASIMEDC) |
                               !is.na(ASICNHC) | !is.na(ASICCOLL) | !is.na(ASICNHC) | !is.na(ASINBILL) |
                               !is.na(ASIHCST) | !is.na(ASICCMP) | !is.na(ASIWTHLS))

  sleep_set <- transform(sleep_set, R_MARITL = as.factor(R_MARITL))
  sleep_set <- transform(sleep_set, PHSTAT = as.factor(PHSTAT))
  sleep_set <- transform(sleep_set, PDMED12M = as.factor(PDMED12M))
  sleep_set <- transform(sleep_set, PNMED12M = as.factor(PNMED12M))
  sleep_set <- transform(sleep_set, MCPART = as.factor(MCPART))
  sleep_set <- transform(sleep_set, PLBORN = as.factor(PLBORN))
  sleep_set <- transform(sleep_set, CITIZENP = as.factor(CITIZENP))
  sleep_set <- transform(sleep_set, GEOBRTH = as.factor(GEOBRTH))
  sleep_set <- transform(sleep_set, HYBPLEV = as.factor(HYBPLEV))
  sleep_set <- transform(sleep_set, DIBTYPE = as.factor(DIBTYPE))
  sleep_set <- transform(sleep_set, AHSTATYR = as.factor(AHSTATYR))
  sleep_set <- transform(sleep_set, WRKHRS2 = as.numeric(WRKHRS2))
  sleep_set <- transform(sleep_set, CIGSDA1 = as.numeric(CIGSDA1))
  sleep_set <- transform(sleep_set, ASIRETR = as.factor(ASIRETR))
  sleep_set <- transform(sleep_set, ASIMEDC = as.factor(ASIMEDC))
  sleep_set <- transform(sleep_set, ASICNHC = as.factor(ASICNHC))
  sleep_set <- transform(sleep_set, ASINBILL = as.factor(ASINBILL))
  sleep_set <- transform(sleep_set, ASIHCST = as.factor(ASIHCST))
  sleep_set <- transform(sleep_set, ASICCMP = as.factor(ASICCMP))
  sleep_set <- transform(sleep_set, ASIWTHLS = as.factor(ASIWTHLS))
  return(sleep_set)
}

compute_Sleepscore <- function(){
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
