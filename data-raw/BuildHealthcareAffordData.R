#' Healthcare Afford Data
#' Returns a dataframe of healthcare affordability data
#' @return data frame
#' @export
#'
#' @examples
get_afford_data <-function(){

  load_data()
  adult_afford <- dplyr::select(sampleAdult, HHX,FMX, FPX,AHCAFYR4,AHCAFYR3,AHCCHGYR,AGE_P,AHICOMP

                                # ONEJOB, LOCALL1B, HYPDIFV,
                                # PREGNOW, CHPAIN6M,
                                # ALCHRC17, ALCHRC29, SMKSTAT2, SMKQTY,
                                # CIGSDA2, CIGSDAY, ECIGCUR2,
                                # VIGNO, ALCSTAT, ALC12MWK, BMI, ASICPUSE,
                                # ASISTLV, YTQU_YG1
  )

  person_afford <- dplyr::select(nhisPerson, HHX, FMX, FPX,PRDNCOV1,PRDNCOV2,MEDBILL,HINOTYR,HIEMPOF,
                                 ERNYR_P,LAHCA1,SEX,CITIZENP,LAHCC1,LCUNIT1,LACHRC1,LCCHRC1,HINOTMYR,
                                 NOTCOV,COVER65,COVER65O,MCPART,MDPRINC,HICOSTR1,HISTOP1,HISTOP2,HISTOP3,
                                 HISTOP4,HISTOP5,HISTOP6,HISTOP7,HISTOP8,HISTOP11,HISTOP12,
                                 HISTOP13,HISTOP14,HISTOP15,FCOVCONF,PLNMGD2,REGIONBR,
                                 FHICHNG,YRSINUS

                                 # PHCDVN2W, NOTCOV, HCSPFYR, MEDBNOP,
                                 # WRKFTALL, ERNYR_P, PSAL, PSEINC, PSSRR,
                                 # PSSRRDB, PSSRRD, PPENS, POPENS, PSSI, PSSID,
                                 # PTANF, POWBEN, PINTRSTR, PDIVD, PCHLDSP, PINCOT
  )

  afford_set <- dplyr::inner_join(adult_afford, person_afford, by = c("HHX" = "HHX", "FMX" = "FMX", "FPX" = "FPX"))
  afford_set <- naniar::replace_with_na(afford_set, replace = list( AHCAFYR4 = c(7, 8, 9), AHCAFYR3 = c(7, 8, 9),
                                                                    AHCCHGYR = c(7, 8, 9),PRDNCOV1 = c(7, 8, 9),
                                                                    CITIZENP = c(7, 8, 9), MEDBILL = c(7, 8, 9),
                                                                    HINOTYR = c(7, 8, 9), HIEMPOF = c(7, 8, 9),
                                                                    AHICOMP = c(7, 8, 9), ERNYR_P = c(97, 98, 99),
                                                                    LAHCA1 = c(7, 8, 9),HINOTMYR=c(97,98,99),
                                                                    LAHCC1 = c(7, 8, 9), LCUNIT1 = c(7, 8, 9),
                                                                    LACHRC1 = c(7, 8, 9), LCCHRC1 = c(9),
                                                                    NOTCOV = c(7, 8, 9), PRDNCOV2 = c(7, 8, 9),
                                                                    MCPART = c(7, 8, 9), YRSINUS=c(9) ,
                                                                    MDPRINC = c(7, 8, 9),HICOSTR1= c(99997, 99998, 99999),
                                                                    HISTOP1=c(7, 8, 9),HISTOP2=c(7, 8, 9),
                                                                    HISTOP3=c(7, 8, 9),HISTOP4=c(7, 8, 9),
                                                                    HISTOP5=c(7, 8, 9),HISTOP6=c(7, 8, 9),
                                                                    HISTOP7=c(7, 8, 9),HISTOP8=c(7, 8, 9),
                                                                    HISTOP11=c(7, 8, 9),HISTOP12=c(7, 8, 9),
                                                                    HISTOP13=c(7, 8, 9),HISTOP14=c(7, 8, 9),
                                                                    HISTOP15=c(7, 8, 9),PRDNCOV2 = c(7, 8, 9),
                                                                    FCOVCONF=c(7,8,9),PLNMGD2=c(7,8,9),
                                                                    REGIONBR=c(99),FHICHNG=c(7,8,9)))

  afford_set <- dplyr::filter(afford_set, !is.na(AHCAFYR4) | !is.na(AHCAFYR3) | !is.na(AHCCHGYR) | !is.na(PRDNCOV1) |
                                !is.na(MCPART) | !is.na(PRDNCOV2) | !is.na(CITIZENP) | !is.na(MEDBILL) |
                                !is.na(HINOTYR) | !is.na(HIEMPOF) | !is.na(AHICOMP) | !is.na(ERNYR_P) |
                                !is.na(LAHCA1) | !is.na(HINOTMYR) | !is.na(LAHCC1) | !is.na(LCUNIT1) |
                                !is.na(LACHRC1) | !is.na(LCCHRC1) | !is.na(NOTCOV) | !is.na(YRSINUS) |
                                !is.na(MDPRINC) | !is.na(HICOSTR1) | !is.na(FCOVCONF) | !is.na(PLNMGD2)
                              | !is.na(REGIONBR) | !is.na(FHICHNG) | !is.na(HISTOP1) | !is.na(HISTOP2)
                              | !is.na(HISTOP3) | !is.na(HISTOP4) | !is.na(HISTOP5) | !is.na(HISTOP6)
                              | !is.na(HISTOP7) | !is.na(HISTOP8) | !is.na(HISTOP11) | !is.na(HISTOP12)
                              | !is.na(HISTOP13) | !is.na(HISTOP14) | !is.na(HISTOP15))

  afford_set <- transform(afford_set, AHCAFYR4 = as.factor(AHCAFYR4))
  afford_set <- transform(afford_set, AHCAFYR3 = as.factor(AHCAFYR3))
  afford_set <- transform(afford_set, AHCCHGYR = as.factor(AHCCHGYR))
  afford_set <- transform(afford_set, PRDNCOV1 = as.factor(PRDNCOV1))
  afford_set <- transform(afford_set, MCPART = as.factor(MCPART))
  afford_set <- transform(afford_set, PRDNCOV2 = as.factor(PRDNCOV2))
  afford_set <- transform(afford_set, CITIZENP = as.factor(CITIZENP))
  afford_set <- transform(afford_set, MEDBILL = as.factor(MEDBILL))
  afford_set <- transform(afford_set, HINOTYR = as.factor(HINOTYR))
  afford_set <- transform(afford_set, HIEMPOF = as.factor(HIEMPOF))
  afford_set <- transform(afford_set, AHICOMP = as.factor(AHICOMP))
  afford_set <- transform(afford_set, ERNYR_P = as.factor(ERNYR_P))
  afford_set <- transform(afford_set, LAHCA1 = as.factor(LAHCA1))
  afford_set <- transform(afford_set, LCUNIT1 = as.factor(LCUNIT1))
  afford_set <- transform(afford_set, LAHCC1 = as.factor(LAHCC1))
  afford_set <- transform(afford_set, LCUNIT1 = as.factor(LCUNIT1))
  afford_set <- transform(afford_set, LACHRC1 = as.factor(LACHRC1))
  afford_set <- transform(afford_set, LCCHRC1 = as.factor(LCCHRC1))
  afford_set <- transform(afford_set, NOTCOV = as.factor(NOTCOV))
  afford_set <- transform(afford_set, YRSINUS = as.factor(YRSINUS))
  afford_set <- transform(afford_set, MDPRINC = as.factor(MDPRINC))
  afford_set <- transform(afford_set, HICOSTR1 = as.numeric(HICOSTR1))
  afford_set <- transform(afford_set, HINOTMYR = as.numeric(HINOTMYR))
  afford_set <- transform(afford_set, FCOVCONF = as.factor(FCOVCONF))
  afford_set <- transform(afford_set, PLNMGD2 = as.factor(PLNMGD2))
  afford_set <- transform(afford_set, REGIONBR = as.factor(REGIONBR))
  afford_set <- transform(afford_set, FHICHNG = as.factor(FHICHNG))
  afford_set <- transform(afford_set, HISTOP1 = as.factor(HISTOP1))
  afford_set <- transform(afford_set, HISTOP2 = as.factor(HISTOP2))
  afford_set <- transform(afford_set, HISTOP3 = as.factor(HISTOP3))
  afford_set <- transform(afford_set, HISTOP4 = as.factor(HISTOP4))
  afford_set <- transform(afford_set, HISTOP5 = as.factor(HISTOP5))
  afford_set <- transform(afford_set, HISTOP6 = as.factor(HISTOP6))
  afford_set <- transform(afford_set, HISTOP7 = as.factor(HISTOP7))
  afford_set <- transform(afford_set, HISTOP8 = as.factor(HISTOP8))
  afford_set <- transform(afford_set, HISTOP11 = as.factor(HISTOP11))
  afford_set <- transform(afford_set, HISTOP12 = as.factor(HISTOP12))
  afford_set <- transform(afford_set, HISTOP13 = as.factor(HISTOP13))
  afford_set <- transform(afford_set, HISTOP14 = as.factor(HISTOP14))
  afford_set <- transform(afford_set, HISTOP15 = as.factor(HISTOP15))


  return(afford_set)
}

#' Healthcare affordability data with score
#' Returns a data frame containing columns related to healthcare affordability with score.
#' @return dataframe
#' @export
#'
#' @examples
get_data_affordscore <-function(){

  load_data()
  adult_afford <- dplyr::select(sampleAdult, HHX,FMX, FPX,AHCAFYR4,AHCAFYR3,AHCCHGYR,AGE_P,AHICOMP

                                # ONEJOB, LOCALL1B, HYPDIFV,
                                # PREGNOW, CHPAIN6M,
                                # ALCHRC17, ALCHRC29, SMKSTAT2, SMKQTY,
                                # CIGSDA2, CIGSDAY, ECIGCUR2,
                                # VIGNO, ALCSTAT, ALC12MWK, BMI, ASICPUSE,
                                # ASISTLV, YTQU_YG1
  )

  person_afford <- dplyr::select(nhisPerson, HHX, FMX, FPX,PRDNCOV1,PRDNCOV2,MEDBILL,HINOTYR,HIEMPOF,
                                 ERNYR_P,LAHCA1,SEX,CITIZENP,LAHCC1,LCUNIT1,LACHRC1,LCCHRC1,HINOTMYR,
                                 NOTCOV,COVER65,COVER65O,MCPART,MDPRINC,HICOSTR1,HISTOP1,HISTOP2,HISTOP3,
                                 HISTOP4,HISTOP5,HISTOP6,HISTOP7,HISTOP8,HISTOP11,HISTOP12,
                                 HISTOP13,HISTOP14,HISTOP15,FCOVCONF,PLNMGD2,REGIONBR,
                                 FHICHNG,YRSINUS

                                 # PHCDVN2W, NOTCOV, HCSPFYR, MEDBNOP,
                                 # WRKFTALL, ERNYR_P, PSAL, PSEINC, PSSRR,
                                 # PSSRRDB, PSSRRD, PPENS, POPENS, PSSI, PSSID,
                                 # PTANF, POWBEN, PINTRSTR, PDIVD, PCHLDSP, PINCOT
  )

  afford_set <- dplyr::inner_join(adult_afford, person_afford, by = c("HHX" = "HHX", "FMX" = "FMX", "FPX" = "FPX"))
  afford_set <- naniar::replace_with_na(afford_set, replace = list( AHCAFYR4 = c(7, 8, 9), AHCAFYR3 = c(7, 8, 9),
                                                                    AHCCHGYR = c(7, 8, 9),PRDNCOV1 = c(7, 8, 9),
                                                                    CITIZENP = c(7, 8, 9), MEDBILL = c(7, 8, 9),
                                                                    HINOTYR = c(7, 8, 9), HIEMPOF = c(7, 8, 9),
                                                                    AHICOMP = c(7, 8, 9), ERNYR_P = c(97, 98, 99),
                                                                    LAHCA1 = c(7, 8, 9),HINOTMYR=c(97,98,99),
                                                                    LAHCC1 = c(7, 8, 9), LCUNIT1 = c(7, 8, 9),
                                                                    LACHRC1 = c(7, 8, 9), LCCHRC1 = c(9),
                                                                    NOTCOV = c(7, 8, 9), PRDNCOV2 = c(7, 8, 9),
                                                                    MCPART = c(7, 8, 9), YRSINUS=c(9) ,
                                                                    MDPRINC = c(7, 8, 9),HICOSTR1= c(99997, 99998, 99999),
                                                                    HISTOP1=c(7, 8, 9),HISTOP2=c(7, 8, 9),
                                                                    HISTOP3=c(7, 8, 9),HISTOP4=c(7, 8, 9),
                                                                    HISTOP5=c(7, 8, 9),HISTOP6=c(7, 8, 9),
                                                                    HISTOP7=c(7, 8, 9),HISTOP8=c(7, 8, 9),
                                                                    HISTOP11=c(7, 8, 9),HISTOP12=c(7, 8, 9),
                                                                    HISTOP13=c(7, 8, 9),HISTOP14=c(7, 8, 9),
                                                                    HISTOP15=c(7, 8, 9),PRDNCOV2 = c(7, 8, 9),
                                                                    FCOVCONF=c(7,8,9),PLNMGD2=c(7,8,9),
                                                                    REGIONBR=c(99),FHICHNG=c(7,8,9)))

  afford_set <- dplyr::filter(afford_set, !is.na(AHCAFYR4) | !is.na(AHCAFYR3) | !is.na(AHCCHGYR) | !is.na(PRDNCOV1) |
                                !is.na(MCPART) | !is.na(PRDNCOV2) | !is.na(CITIZENP) | !is.na(MEDBILL) |
                                !is.na(HINOTYR) | !is.na(HIEMPOF) | !is.na(AHICOMP) | !is.na(ERNYR_P) |
                                !is.na(LAHCA1) | !is.na(HINOTMYR) | !is.na(LAHCC1) | !is.na(LCUNIT1) |
                                !is.na(LACHRC1) | !is.na(LCCHRC1) | !is.na(NOTCOV) | !is.na(YRSINUS) |
                                !is.na(MDPRINC) | !is.na(HICOSTR1) | !is.na(FCOVCONF) | !is.na(PLNMGD2)
                              | !is.na(REGIONBR) | !is.na(FHICHNG) | !is.na(HISTOP1) | !is.na(HISTOP2)
                              | !is.na(HISTOP3) | !is.na(HISTOP4) | !is.na(HISTOP5) | !is.na(HISTOP6)
                              | !is.na(HISTOP7) | !is.na(HISTOP8) | !is.na(HISTOP11) | !is.na(HISTOP12)
                              | !is.na(HISTOP13) | !is.na(HISTOP14) | !is.na(HISTOP15))

  afford_set <- transform(afford_set, AHCAFYR4 = as.factor(AHCAFYR4))
  afford_set <- transform(afford_set, AHCAFYR3 = as.factor(AHCAFYR3))
  afford_set <- transform(afford_set, AHCCHGYR = as.factor(AHCCHGYR))
  afford_set <- transform(afford_set, PRDNCOV1 = as.factor(PRDNCOV1))
  afford_set <- transform(afford_set, MCPART = as.factor(MCPART))
  afford_set <- transform(afford_set, PRDNCOV2 = as.factor(PRDNCOV2))
  afford_set <- transform(afford_set, CITIZENP = as.factor(CITIZENP))
  afford_set <- transform(afford_set, MEDBILL = as.factor(MEDBILL))
  afford_set <- transform(afford_set, HINOTYR = as.factor(HINOTYR))
  afford_set <- transform(afford_set, HIEMPOF = as.factor(HIEMPOF))
  afford_set <- transform(afford_set, AHICOMP = as.factor(AHICOMP))
  afford_set <- transform(afford_set, ERNYR_P = as.factor(ERNYR_P))
  afford_set <- transform(afford_set, LAHCA1 = as.factor(LAHCA1))
  afford_set <- transform(afford_set, LCUNIT1 = as.factor(LCUNIT1))
  afford_set <- transform(afford_set, LAHCC1 = as.factor(LAHCC1))
  afford_set <- transform(afford_set, LCUNIT1 = as.factor(LCUNIT1))
  afford_set <- transform(afford_set, LACHRC1 = as.factor(LACHRC1))
  afford_set <- transform(afford_set, LCCHRC1 = as.factor(LCCHRC1))
  afford_set <- transform(afford_set, NOTCOV = as.factor(NOTCOV))
  afford_set <- transform(afford_set, YRSINUS = as.factor(YRSINUS))
  afford_set <- transform(afford_set, MDPRINC = as.factor(MDPRINC))
  afford_set <- transform(afford_set, HICOSTR1 = as.numeric(HICOSTR1))
  afford_set <- transform(afford_set, HINOTMYR = as.numeric(HINOTMYR))
  afford_set <- transform(afford_set, FCOVCONF = as.factor(FCOVCONF))
  afford_set <- transform(afford_set, PLNMGD2 = as.factor(PLNMGD2))
  afford_set <- transform(afford_set, REGIONBR = as.factor(REGIONBR))
  afford_set <- transform(afford_set, FHICHNG = as.factor(FHICHNG))
  afford_set <- transform(afford_set, HISTOP1 = as.factor(HISTOP1))
  afford_set <- transform(afford_set, HISTOP2 = as.factor(HISTOP2))
  afford_set <- transform(afford_set, HISTOP3 = as.factor(HISTOP3))
  afford_set <- transform(afford_set, HISTOP4 = as.factor(HISTOP4))
  afford_set <- transform(afford_set, HISTOP5 = as.factor(HISTOP5))
  afford_set <- transform(afford_set, HISTOP6 = as.factor(HISTOP6))
  afford_set <- transform(afford_set, HISTOP7 = as.factor(HISTOP7))
  afford_set <- transform(afford_set, HISTOP8 = as.factor(HISTOP8))
  afford_set <- transform(afford_set, HISTOP11 = as.factor(HISTOP11))
  afford_set <- transform(afford_set, HISTOP12 = as.factor(HISTOP12))
  afford_set <- transform(afford_set, HISTOP13 = as.factor(HISTOP13))
  afford_set <- transform(afford_set, HISTOP14 = as.factor(HISTOP14))
  afford_set <- transform(afford_set, HISTOP15 = as.factor(HISTOP15))

  afford_set <- dplyr::filter(afford_set,!is.na(AHCAFYR4) & !is.na(AHCAFYR3))

  afford_set <- dplyr::mutate(afford_set,Affordability=if_else((AHCAFYR4==1 & AHCAFYR3==1),0,if_else((AHCAFYR4==2 & AHCAFYR3==2),2,1)))

  return(afford_set)
}
