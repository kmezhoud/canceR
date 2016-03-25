context("canceR functions")

#test_that("set_class", {
# foo <- . %>% .^2 %>% set_class(c("foo", class(.)))
#expect_equal(3 %>% foo %>% class,c("foo","numeric"))
#})

testthat::test_that("cgdsr connection",
                    {
                        mycgds <- cgdsr::CGDS("http://www.cbioportal.org/public-portal/")
                        result <- cgdsr::test(mycgds)
                    })