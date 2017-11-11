rebuildDB = function(create = FALSE){
  programme = getProgrammeSheet()

  programme =  programme %>%
    createAffilTbl(create) %>%
    createAuthorTbl(create) %>%
    createTitleTbl(create)

  ## workaround so it won't crash
  Sys.sleep(10)
  programme =  programme %>%
    createCorrespondingTbl(create) %>%
    createOtherAuthorsTbl(create) %>%
    createAbstractTbl(create) %>%
    createProgTbl(create)

  Sys.sleep(10)
  programme =  programme %>%
    createPosterTbl(create)

}
