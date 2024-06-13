


my_env <- new.env()
my_env$msft <- msft


rmarkdown::render("Seeking-Alpha_Capstone-Project.Rmd", envir = my_env)