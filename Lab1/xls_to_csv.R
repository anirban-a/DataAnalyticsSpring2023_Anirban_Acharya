library(readxl)

xls.read = list.files(pattern="xls")

# convert xls to csv.
lapply(xls.read, function(f) {
  df = read_excel(f, sheet="EPI2010_all countries")
  write.csv(df, gsub("xls", "csv", f), row.names=FALSE)
})
