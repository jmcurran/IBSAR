createPosterTbl = function(ss, create = FALSE){
  ss = getProgrammeSheet()
  postersSheet = ss %>% gs_read(ws = "Posters")

  authorTbl = ss %>% gs_read(ws = "authorTbl")
  authorID = postersSheet %>%
    left_join(authorTbl, by = c("Lead Author" = "author")) %>%
    select(authorID)


  posterTbl = data_frame(subID = postersSheet$`Submission Number`,
                         authorID = authorID$authorID)
  ss = if(create){
    ss %>% gs_ws_new(ws_title = "posterTbl", input = posterTbl, trim = TRUE, row_extent = 2)
  }else{
    ss %>% gs_edit_cells(ws = "posterTbl", input = posterTbl, anchor = "A1", trim = TRUE)
  }

  return(ss)
}
