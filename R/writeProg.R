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

    for(row in 1:nrow(dayProgTbl)){
      if(!is.na(dayProgTbl$subID[row])){
        createEntry(f1, dayProgTbl, affilTbl, authorTbl, otherTbl, titleTbl,
                  abstractTbl, row)
      }
    }
    close(f1)
  }

  # for(row in 1:nrow(progTbl)){
  #   if(!is.na(progTbl$subID[row])){
  #     createEntry(progTbl, affilTbl, authorTbl, otherTbl, titleTbl,
  #                 abstractTbl, row, path = path, tex = tex)
  #     if(tex){
  #       writeLines(sprintf("\\input{sub%d.tex}", progTbl$programmeID[row]), f2)
  #     }else{
  #       writeLines(c(sprintf("```{r child = 'sub%d.Rmd'}", progTbl$programmeID[row]),
  #                    "```"), f2)
  #     }
  #   }
  # }
  #
  # writeLines("\\newpage", f2)
  #
  # if(tex){
  #   writeLines("\\section*{Posters}\n\\label{sec:posters}", f2)
  #   writeLines("\\addcontentsline{toc}{section}{Posters}", f2)
  # }else{
  #   writeLines("# Posters", f2)
  # }
  #
  # posterTbl = programme %>% gs_read(ws = "posterTbl")
  #
  # for(row in 1:nrow(posterTbl)){
  #   createEntry(posterTbl, affilTbl, authorTbl, otherTbl, titleTbl,
  #               abstractTbl, row, path = path, isTalk = FALSE, tex = tex)
  #   if(tex){
  #     writeLines(sprintf("\\input{poster%d.tex}", row), f2)
  #   }else{
  #     writeLines(c(sprintf("```{r child = 'poster%d.Rmd'}", row),
  #                  "```"), f2)
  #   }
  # }
  #
  # if(tex){
  #   writeLines("\\end{document}", f2)
  # }
  # close(f2)
}
