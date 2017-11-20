createTitleTbl = function(ss, create = FALSE){
  ## Create the Title Table

  allSubSheet = gs_read(ss, ws = "All submissions")

  titleTbl = allSubSheet %>%
    select(starts_with("Submission"),Proper) %>%
    rename(title = Proper, subID = Submission)

  correctCapitals = function(x){
    allowed = "(AGM|APES|DLDA|GxE|GLM| IV |LASSO|LIBS|PCA|QTL)"
    x = gsub(allowed, "\\U\\1", x, ignore.case = TRUE, perl = TRUE)
    if(grepl("GXE", x)){
      x = sub("GXE", "GxE", x)
    }else if(grepl("Alzheimer'S", x))
      x = sub("Alzheimer's", x)
    return(x)
  }

  titleTbl = titleTbl %>% mutate(title = correctCapitals(title))

  ss = if(create){
    ss %>% gs_ws_new(ws_title = "titleTbl", input = titleTbl, trim = TRUE, row_extent = 3)
  }else{
    ss %>% gs_edit_cells(ws = "titleTbl", input = titleTbl, anchor = "A1", trim = TRUE)
  }

  db = dbConnect(RSQLite::SQLite(), "ibsar")
  dbWriteTable(db, "titleTbl", titleTbl)
  dbDisconnect(db)

  return(ss)
}
