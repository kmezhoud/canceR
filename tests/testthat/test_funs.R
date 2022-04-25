testthat::context("canceR functions")

#test_that("set_class", {
# foo <- . %>% .^2 %>% set_class(c("foo", class(.)))
#expect_equal(3 %>% foo %>% class,c("foo","numeric"))
#})

testthat::test_that("cbioportal connection",
                    {
                        mycgds <- CGDS("http://www.cbioportal.org/")
                        result <- test.CGDS(mycgds)
                    })
