xtab <- function(dades, VD, VI = NULL, PES = NULL, territori = NULL, edicions = NULL, na.rm = T){

  # pes no pot ser nul
  if(is.null(PES)){
    dades$PES <- 1
  }
  # llista arguments que no són null
   args <- purrr::compact(list(VD = VD,
                               VI = VI,
                               PES = PES,
                               territori = territori,
                               edicions = edicions))

   # selecciona cols
   dades <- dades %>%
      dplyr::select(tidyselect::all_of(unlist(args)))

   # filtra si na.rm = T
 if(na.rm == T) {
    dades <- dades[complete.cases(dades[,]),]
 }

   dnest <- dades %>%
      dplyr::group_by(
        dplyr::across(
          tidyselect::all_of(
            names(args[-which(names(args) %in% c("VD", "PES"))])))) %>%
      tidyr::nest() %>%
      dplyr::mutate(
         tt = map_int(data, nrow),
         TT = map_dbl(data, compTT)
      ) %>%
      mutate(
         ns = map(data, compN)
      ) %>%
      unnest(cols = c(ns)) %>%
      mutate(
         PP = round(N/TT * 100, 2)
      ) %>%
     select(-c(data))


return(dnest)

}

# funcions auxiliars dins de nest
compTT <- function(dd){
   sum(dd[["PES"]], na.rm = T)
}

compN <- function(dd){
   dd %>%
      group_by(across(all_of(c("VD")))) %>%
      summarise(
         n = n(),
         N = sum(PES, na.rm = T)
      )
}

