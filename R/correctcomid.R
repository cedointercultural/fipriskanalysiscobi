#' Correct COM_ID field to CVE_LOC
#'
#' @param thiscomponentdata 
#'
#' @return
#' @export
#'
#' @examples
correctcomid <- function(thiscomponentdata){
  
  thiscomponentdatacorr <- thiscomponentdata %>% 
    tidyr::separate_wider_delim(COM_ID, delim = "_", names = c("CVE_ENT","CVE_MUN","CVE_LOC")) %>%
    dplyr::mutate(no_loc = nchar(CVE_LOC), no_mun = nchar(CVE_MUN), no_ent = nchar(CVE_ENT)) %>%
    dplyr::mutate(CVE_ENT = dplyr::if_else(no_ent==1,paste0("0",CVE_ENT), CVE_ENT)) %>%
    dplyr::mutate(CVE_MUN = dplyr::if_else(no_mun==1, paste0(CVE_ENT,"00",CVE_MUN),
                                           dplyr::if_else(no_mun==2, paste0(CVE_ENT,"0", CVE_MUN),
                                                          dplyr::if_else(no_mun==3, paste0(CVE_ENT, CVE_MUN),
                                                                         dplyr::if_else(no_mun==4, paste0("0", CVE_MUN), CVE_MUN))))) %>%
    dplyr::mutate(CVE_LOC=dplyr::if_else(no_loc==1, paste0(CVE_MUN,"000", CVE_LOC),
                                         dplyr::if_else(no_loc==2, paste0(CVE_MUN, "00",CVE_LOC),
                                                        dplyr::if_else(no_loc==3, paste0(CVE_MUN, "0", CVE_LOC),
                                                                       dplyr::if_else(no_loc==4, paste0(CVE_MUN, CVE_LOC),
                                                                                      dplyr::if_else(no_loc==8, paste0("0",CVE_LOC),CVE_LOC)))))) %>%
    dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN, everything())
  
  return(thiscomponentdatacorr)
  
}