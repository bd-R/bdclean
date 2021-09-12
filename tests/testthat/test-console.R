context("bdclean checks")

# Utils

test_that("Quality checks are read correctly from bdchecks", {
    expect_that(
        length(get_checks_list()) > 15, equals(TRUE)
    )
})


# Reference class tests

test_that("BDQuestion reference class initializes correctly", {
    expect_silent(BdQuestion(
        question = "Do you worry about taxonomical aspect of the data?",
        possible.responses = c("Yes", "No"),
        question.type = "Router",
        router.condition = c("Yes", "Y", "yes", 1, TRUE, "TRUE"),
        question.id = "taxonMain",
        ui.type = "single-checkbox"
    ))
})


test_that("BdQuestionContainer reference class initializes correctly", {
    expect_message(BdQuestionContainer(c(question1)), "New BdQuestionContainer instance created")
})

test_that("Default questionnaire is generated", {
    expect_that(
        create_default_questionnaire(),
        is_a("BdQuestionContainer")
    )
})

test_that("BdQuestionContainer reference class prints correctly", {
    expect_output(question1$print_question(), "Do you worry about taxonomical aspect of the data?")
})

test_that("BdQuestionContainer validation function adds correctly", {
    expect_silent(question1$add_validation_function(function() return(TRUE)))
})

test_that("BdQuestionContainer user response adds correctly", {
    expect_silent(question1$set_response("TRUE"))
})

test_that("BdQuestionContainer nested question adds correctly", {
    expect_silent(question1$add_child_question(c(question2)))
})

test_that("BdQuestionContainer quality checks adds correctly", {
    expect_silent(question2$add_quality_checks("taxo_level"))
})


# cleaning

test_that("Cleaning function without data fails", {
    expect_error(clean_data())
})
