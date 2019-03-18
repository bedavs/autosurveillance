saveA4 <- function(q, filename, landscape=T){
  ggsave(filename, plot=q, width=297, height=210, units="mm")
}