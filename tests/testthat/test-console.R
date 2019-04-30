context("bdclean checks")



# Quality Checks

test_that("Taxonomy level works with correct data", {
    expect_true(ncol(taxo_level(mammals_data)) == 85)
})

test_that("Taxonomy level breaks with no data", {
    expect_error(taxo_level())
})


test_that("Taxonomy level breaks with incorrect resolution", {
    expect_warning(taxo_level(mammals_data, "Empty"))
})

test_that("Taxonomy level breaks with incorrect resolution type", {
    expect_warning(taxo_level(mammals_data, 10))
})

test_that("Spatial resolution works with correct data", {
    expect_true(ncol(spatial_resolution(mammals_data)) == 85)
})


test_that("Earliest date works with correct data", {
    expect_true(ncol(earliest_date(mammals_data)) == 85)
})


test_that("Temporal resolution works with correct data", {
    expect_true(ncol(temporal_resolution(mammals_data)) == 85)
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
    expect_silent(question1$add_quality_checks("taxonomy_clean"))
})

test_that("BdQuestionContainer flads data correctly", {
    expect_error(question1$flag_data(mammals_data))
})

test_that("BdQuestionContainer creates docs correctly", {
    expect_warning(question1$add_to_report(mammals_data))
})


# Report Generation

test_that("Reports generates correctly", {
    expect_message(create_report_data(input_data,
                                      flagged_data,
                                      cleaned_data, responses, T, 'md_document'), 
                   "generated simple")
})



# cleaning

#184  84

# test_that("Reports generates correctly", {
#     responses <- readRDS(system.file("testdata/questionnaire-response.rds", package="bdclean"))
#     expect_type(clean_data(data = mammals_data, custom_questionnaire = responses), "data.frame")
# })


test_that("Cleaning function without data fails", {
    expect_error(clean_data())
})


test_that("Cleaning function without data fails", {
    expect_error(clean_data(mammals_data, custom_questionnaire = responses, missing = T))
})

test_that("Cleaning function without data fails", {
    expect_error(clean_data(mammals_data, custom_questionnaire = responses, report = F))
})



# Utils

test_that("Quality checks are read correctly from bdchecks", {
    expect_that(
        length(get_checks_list()) > 15, equals(TRUE)
    )
})


