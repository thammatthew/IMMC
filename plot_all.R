files <- list.files(path="experimental_results", pattern="*.rds", full.names=TRUE, recursive=FALSE)
for(i in 1:length(files)) {
  print(files[[i]])
  output<-readRDS(files[[i]])
  name = strsplit(files[[i]], "[.]|[/]")[[1]][[2]]
  plot_output(output, name)
}
