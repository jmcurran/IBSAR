createOtherAuthorsTbl = function(ss, create = FALSE){
  allSheet = ss %>% gs_read(ws = "All")
  authorTbl = ss %>% gs_read(ws = "authorTbl")
  correspTbl = ss %>% gs_read(ws = "correspTbl")

  allSheet = allSheet %>% add_column(allID = 1:nrow(allSheet), .before = 1)

  otherTbl = authorTbl %>%
    left_join(allSheet, by = c("author" = "Author")) %>%
    select(allID, authorID, Submission) %>%
    rename(otherID = allID, subID = Submission) %>%
    filter(!is.na(subID)) %>%
    arrange(otherID)


  ss = if(create){
    ss %>% gs_ws_new(ws_title = "otherTbl", input = otherTbl, row_extent = 2)
  }else{
    ss %>% gs_edit_cells(ws = "otherTbl", input = otherTbl, anchor = "A1", trim = TRUE)
  }

  # db = dbConnect(RSQLite::SQLite(), "ibsar")
  # dbWriteTable(db, "othersTbl", othersTbl)
  # dbDisconnect(db)

  return(ss)
}
