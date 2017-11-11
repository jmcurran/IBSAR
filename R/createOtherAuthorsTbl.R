createOtherAuthorsTbl = function(ss, create = FALSE){
  allSheet = ss %>% gs_read(ws = "All")
  authorTbl = ss %>% gs_read(ws = "authorTbl")
  correspTbl = ss %>% gs_read(ws = "correspTbl")

  otherTbl = authorTbl %>%
    left_join(allSheet, by = c("author" = "Author")) %>%
    select(authorID,Submission) %>%
    rename(subID = Submission) %>%
    filter(!is.na(subID))

  ss = if(create){
    ss %>% gs_ws_new(ws_title = "otherTbl", input = otherTbl, row_extent = 2)
  }else{
    ss %>% gs_edit_cells(ws = "otherTbl", input = otherTbl, anchor = "A1", trim = TRUE)
  }

  return(ss)
}
