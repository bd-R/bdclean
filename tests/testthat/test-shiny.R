# context("Test Shiny app")
# 
# 
# # open Shiny app and PhantomJS
# app <- shinytest::ShinyDriver$new(system.file("shiny/bdclean", package = "bdclean"))
# 
# test_that("App startup works", {
#     appTitle <- app$getTitle()[[1]]
#     expect_equal(appTitle, "Shiny Test App") 
# })
# 
# # stop the Shiny app
# app$stop()