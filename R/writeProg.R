writeProg = function(path = "~/Dropbox/Work/2017/Conferences/IBS-AR/"){
  # templateLines = if(tex){
  #   readLines(system.file("tex", "programme.tex", package = "IBSAR"))
  # }else{
  #   readLines(system.file("Rmd", "programme.Rmd", package = "IBSAR"))
  # }
  #
  # f2 = file(paste0(path, "master", ifelse(tex, ".tex", ".Rmd")), "w")
  # writeLines(templateLines, f2)

  programme = getProgrammeSheet()
  progTbl = programme %>% gs_read(ws = "progTbl") %>% arrange(day, time)
  affilTbl = programme %>% gs_read(ws = "affilTbl")

  ## stupid time out errors from google API
  Sys.sleep(10)
  authorTbl = programme %>% gs_read(ws = "authorTbl")
  otherTbl = programme %>% gs_read(ws = "otherTbl")
  titleTbl = programme %>% gs_read(ws = "titleTbl")

  ## stupid time out errors from google API
  Sys.sleep(10)
  abstractTbl = programme %>% gs_read(ws = "abstractTbl")

  daysOfWeek = paste0(c("Mon", "Tues", "Wed", "Thurs"), "day")

  for(d in 1:4){
    dayProgTbl = progTbl %>% filter(day == d)

    fileName = paste0(path, str_pad(as.character(d), 2, "left", "0"), "-", daysOfWeek[d], ".Rmd")
    f1 = file(fileName, "w")

    writeLines(paste0("# Programme And Abstracts for ",
                      daysOfWeek[d]," ",
                      (27:30)[d], "<sup>th</sup> of November"),
               f1)

    for(row in 1:nrow(dayProgTbl)){
      if(!is.na(dayProgTbl$subID[row])){
        createEntry(f1, dayProgTbl, affilTbl, authorTbl, otherTbl, titleTbl,
                  abstractTbl, row)
      }
    }
    close(f1)
  }


  fileName = paste0(path, "05-Posters.Rmd")
  f1 = file(fileName, "w")
  writeLines("# Poster Abstracts", f1)


  posterTbl = programme %>% gs_read(ws = "posterTbl")

  for(row in 1:nrow(posterTbl)){
    createEntry(f1, posterTbl, affilTbl, authorTbl, otherTbl, titleTbl,
                abstractTbl, row, isTalk = FALSE)
  }
}
