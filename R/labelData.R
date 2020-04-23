#' Label variables according external file
#
#' Open a excel file and apply label to variables frome data base
#
#' @param data data.frame
#'
#' @param taulavariables excel file with description field
#'
#' @param camp_descripcio Field with label in excel file
#'
#' @return Data.frame or tibble with labeled variables
#'
#' @import dplyr
#'
#' @export

labelData<-function(data,taulavariables,camp_descripcio) {

  # data=dt_plana
  # taulavariables = "variables_R.xls"
  # camp_descripcio="descripcio"

  #  Llegir etiquetes i variables a analitzar ####
  variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble()
  # variables[is.na(variables)]<- 0
  variables<-variables %>% dplyr::filter(!is.na(camp))

  # selecciono els camps necessaris (camp i descripcio) i amb etiqueta
  camp_descripcio<-rlang::sym(camp_descripcio)

  variables<-variables %>% dplyr::select(camp,descripcio=!!camp_descripcio)

  # Els que no tenen etiquet assignar el mateix nom del camp
  variables<-variables %>% dplyr::mutate(descripcio=ifelse(descripcio=="0",camp,descripcio))

  # Etiquetar variables
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  descripcio<- as.vector(seleccio$descripcio) #
  ### etiquetar variables seleccionades     #
  for (i in 1:length(descripcio)){if (any(colnames(data) == camp[i])) {Hmisc::label(data[[camp[i]]]) <- descripcio[i]}}
  data
}

