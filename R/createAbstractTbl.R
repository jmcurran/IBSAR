createAbstractTbl = function(ss, create = FALSE){

  ## Create the Abstract Table

  abstractSheet = gs_read(ss, ws ="Abstracts")

  abstractTbl = abstractSheet %>%
    rename(subID = Submission, title = Title, abstract = X3) %>%
    add_column(abstractID = 1:nrow(abstractSheet), .before = 1)

  ss = if(create){
    ss %>% gs_ws_new(ws_title = "abstractTbl", input = abstractTbl, trim = TRUE, row_extent = 3)
  }else{
    ss %>% gs_edit_cells(ws = "abstractTbl", input = abstractTbl, anchor = "A1", trim = TRUE)
  }

  return(ss)
}
