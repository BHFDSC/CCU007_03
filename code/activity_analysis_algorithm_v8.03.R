###################################################################################################
## NCHDA activity analysis (AA) algorithm is written by John Stickley
## AA algorithm is developed and used by National Institute for Cardiovascular Outcomes Research (NICOR) (version 8.03)
## 2022-10-06 update by Jiaqiu Wang - re-written for Qreg5 download & incorporating the 2020-22 code changes
###################################################################################################


###################################################################################################
aa_allocation <- function(nchda_main) {
  for (i in 1:nrow(nchda_main)) {
    ###############################################################################################
    ## allocate code list and exclude any invalid procedure codes (intersect - procedures_valid)
    ## remove any valid codes that are ignored (setdiff - procedures_exclude) or minor_excluded
    ## use default dplyr version of setdiff > loaded last
    num_proc <- 8
    procedurecodes_fields <- sprintf("proccode%d", seq(1:num_proc))
    code_list <- as.list(sp[c(procedurecodes_fields)])
    

    code_list <- intersect(code_list, procedures_valid_nchda)
    
    ## remove any procedure codes excluded from the NCHDA list
    code_list <- setdiff(code_list, procedures_exclude_non_nchda)
    
    ## remove any procedure codes excluded from the NCHDA listed in minor_and_excluded list
    code_list <- setdiff(code_list, minor_and_excluded_algorithm)
    
    ## remove any NA codes from list
    code_list <- code_list[!is.na(code_list)]
    ## there should only be valid procedure codes having removed invalid/excluded and minor_and_excluded codes
    
    ## if missing in type of proc
    if (is.na(nchda_main[i, "type_procedure"])) {
      nchda_main[i, "aa_allocation"] <- "UNKNOWN-missing type of proc"
    }else{
    
    ###############################################################################################
    # Step 0: excluded (no valid codes present)
    if (length(code_list) == 0) {
      nchda_main[i, "aa_allocation"] <- "0:no_qualifying_codes"
    }
    
    
    ###############################################################################################
    ## Step 1: bypass
    ## ensure no ecmo/vad included - so remove from code_list and then check for being empty (only ecmo/vad left)
    ## required because no longer using minor_and_excluded in SP
    code_list_remaining <- setdiff(code_list, primary_ecmo_include)
    code_list_remaining <- setdiff(code_list_remaining, vad)
    
    if ((nchda_main[i, "type_procedure"] == 1)
        & (nchda_main[i, "aa_allocation"] == "")
        & (length(code_list_remaining) > 0)) {
      nchda_main[i, "aa_allocation"] <- "1:bypass"
    }
    
    
    ###############################################################################################
    ## Step 2: non-bypass
    ## ensure no ecmo/vad included - so remove from code_list and then check for being empty (only ecmo/vad left)
    ## required because no longer using minor_and_excluded in SP
    code_list_remaining <- setdiff(code_list, primary_ecmo_include)
    code_list_remaining <- setdiff(code_list_remaining, vad)
    
    if ((nchda_main[i, "type_procedure"] %in% c(2, 4, 6, 11) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (length(code_list_remaining) > 0)) {
      nchda_main[i, "aa_allocation"] <- "2:non-bypass"
    }
    
    
    ###############################################################################################
    ## Step 3: hybrid
    code_list_remaining <- setdiff(code_list, primary_ecmo_include)
    code_list_remaining <- setdiff(code_list_remaining, vad)
    
    if ((nchda_main[i, "type_procedure"] == 7)
        & (nchda_main[i, "aa_allocation"] == "")
        & (length(code_list_remaining) > 0)) {
      nchda_main[i, "aa_allocation"] <- "3:hybrid"
    }
    
    
    ###############################################################################################
    ## Step 4: vad
    if ((nchda_main[i, "type_procedure"] %in% c(1, 2, 4, 6) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (any(vad %in% code_list))) {
      nchda_main[i, "aa_allocation"] <- "4:vad"
    }
    
    
    ###############################################################################################
    ## Step 5: set all possible ecmo > ecmo & then update values by iteration
    if ((nchda_main[i, "type_procedure"] %in% c(1, 2, 4, 6, 7) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (any(primary_ecmo_include %in% code_list) == TRUE)) {
      nchda_main[i, "aa_allocation"] <- "5:ecmo_unallocated"
    }
    
    
    
    ###############################################################################################
    ## Step 6: icd (non-surgical)
    code_list_remaining <- setdiff(code_list, icd)
    code_list_remaining <- setdiff(code_list_remaining, pacemaker)
    code_list_remaining <- setdiff(code_list_remaining, ep)
    code_list_remaining <- setdiff(code_list_remaining, diagnostic)
    
    if ((nchda_main[i, "type_procedure"] %in% c(3, 5, 10) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (any(icd %in% code_list))
        & (length(code_list_remaining) == 0)) {
      nchda_main[i, "aa_allocation"] <- "6:icd:non-surgical"
    }
    
    
    ###############################################################################################
    ## Step 7: pacemaker (non-surgical)
    code_list_remaining <- setdiff(code_list, pacemaker)
    code_list_remaining <- setdiff(code_list_remaining, ep)
    code_list_remaining <- setdiff(code_list_remaining, diagnostic)
    
    if ((nchda_main[i, "type_procedure"] %in% c(3, 5, 10) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (any(pacemaker %in% code_list))
        & (length(code_list_remaining) == 0)) {
      nchda_main[i, "aa_allocation"] <- "7:pacemaker:non-surgical"
    }
    
    
    ###############################################################################################
    ## Step 8: ep (non-surgical)
    code_list_remaining <- setdiff(code_list, ep)
    code_list_remaining <- setdiff(code_list_remaining, diagnostic)
    
    if ((nchda_main[i, "type_procedure"] %in% c(3, 5, 10) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (any(ep %in% code_list))
        & (length(code_list_remaining) == 0)) {
      nchda_main[i, "aa_allocation"] <- "8:ep:non-surgical"
    }
    
    
    ###############################################################################################
    ## Step 9: intervention (non-surgical)
    code_list_remaining <- setdiff(code_list, diagnostic)
    
    if ((nchda_main[i, "type_procedure"] %in% c(3, 5) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (length(code_list_remaining) > 0)) {
      nchda_main[i, "aa_allocation"] <- "9:intervention:non-surgical"
    }
    
    
    ###############################################################################################
    ## Step 10: diagnostic (non-surgical)
    code_list_remaining <- setdiff(code_list, diagnostic)
    
    if ((nchda_main[i, "type_procedure"] %in% c(3, 5) == TRUE)
        & (nchda_main[i, "aa_allocation"] == "")
        & (any(diagnostic %in% code_list) == TRUE)
        & (length(code_list_remaining) == 0)) {
      nchda_main[i, "aa_allocation"] <- "10:diagnostic:non-surgical"
    }
    
    
    ###############################################################################################
    ## Step 11: set remaining to unallocated
    if (nchda_main[i, "aa_allocation"] == "") {
      nchda_main[i, "aa_allocation"] <- "11:unallocated"
    }
    
    
    ###############################################################################################
    ##  set fuvh according to diagnosis, previous procedure and procedure
    # nchda_main[i, "fuvh"] <- ifelse(
    #   (
    #     any(fuvh_procedure %in% code_list) |
    #       any(fuvh_procedure %in% prev_procedure_list) |
    #       any(fuvh_diagnosis %in% diagnosis_list)
    #     
    #   )
    #   , "yes", "no"
    # )
    # 
    ###############################################################################################
    }
  }
  return(nchda_main)
}



###################################################################################################
## primary ecmo
ecmo_allocation <- function(nchda_ecmo) {
  for (i in 1:nrow(nchda_ecmo)) {
    ## first patient > cannot check preceding patient
    if (i == 1) {
      if (nchda_ecmo[i, "aa_allocation"] == "5:ecmo_unallocated") {
        nchda_ecmo[i, "aa_allocation"] <- "12:primary_ecmo"
      }
    }
    else {
      ## all subesequent records
      ## if same patient
      if ((nchda_ecmo[i, "patid"]) == (nchda_ecmo[(i - 1), "patid"])) {
        ## check that record is ecmo
        ## & previous record is not ecmo
        ## & procedures are > 30 days apart
        if ((nchda_ecmo[i, "aa_allocation"] == "5:ecmo_unallocated")
            & (nchda_ecmo[(i - 1), "aa_allocation"] != "5:ecmo_unallocated")
            & ((nchda_ecmo[i, "procedure_date"]) - (nchda_ecmo[(i - 1), "procedure_date"]) > 30)
        ) {
          nchda_ecmo[i, "aa_allocation"] <- "12:primary_ecmo"
        }
      }
      else {
        ## different patient & first record for that patient - if ecmo then must be primary_ecmo
        if (nchda_ecmo[i, "aa_allocation"] == "5:ecmo_unallocated") {
          nchda_ecmo[i, "aa_allocation"] <- "12:primary_ecmo"
        }
      }
    }
  }

  ###############################################################################################
  return(nchda_ecmo)
}



