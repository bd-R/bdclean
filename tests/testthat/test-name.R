context("bdclean checks")


test_that("Default questionnaire is generated", {
    expect_that(create_default_questionnaire(),
                is_a('BdQuestionContainer'))
})


test_that("Quality checks are read correctly from bdchecks", {
    expect_that(length(get_checks_list()) > 15, equals(TRUE))
})


test_that("Cleaning function without data fails", {
    expect_that(clean_data(), throws_error())
})