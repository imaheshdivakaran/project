install.packages("usethis")
install.packages("devtools")
usethis::use_git_config(user.name= "bimalthomas1997",user.email= "bimal.thomas@genproresearch.com")
usethis::use_git()
usethis::create_github_token()
gitcreds::gitcreds_set()
