# scripts/deploy_shinyapps.R
stopifnot(nzchar(Sys.getenv("SHINYAPPS_ACCOUNT")))
stopifnot(nzchar(Sys.getenv("SHINYAPPS_TOKEN")))
stopifnot(nzchar(Sys.getenv("SHINYAPPS_SECRET")))

library(rsconnect)

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_ACCOUNT"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(
  appDir = "App",
  appName = "advicexplorer",
  appTitle = "advicexplorer",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE
)