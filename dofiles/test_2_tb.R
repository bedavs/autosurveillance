#con <- file("/tmp/computer","r")
#COMPUTER_NAME <- readLines(con,n=1)
#close(con)
#Sys.setenv(COMPUTER=COMPUTER_NAME)

#for(baseFolder in c("/data_clean","/results","/data_app")){
#  files <- list.files(file.path(baseFolder,"normomo"))
#  if(length(files)>0){
#    for(f in files) unlink(file.path(baseFolder,"normomo",f))
#  }
#}

unlink(file.path("/junit","2_tb.xml"))
Sys.sleep(1)

a <- testthat:::JunitReporter$new()
a$start_reporter()
a$out <- file(file.path("/junit","2_tb.xml"), "w+")
a$start_context("2_tb")

output <- processx::run("Rscript","/git/internal_surveillance/dofiles/2_tb.R", error_on_status=F, echo=T)
cat("\n\nstdout\n\n")
cat(output$stdout)
cat("\n\nstderr\n\n")
cat(output$stderr)

if(output$status==0){
  a$add_result("2_tb","RunAll",testthat::expectation("success","Pass"))
} else {
  a$add_result("2_tb","RunAll",testthat::expectation("error","Fail"))
}

a$end_context("2_tb")
a$end_reporter()
close(a$out)



