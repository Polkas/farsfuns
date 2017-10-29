
context("file-existence")

test_that("files exist", {

years = (2013):(2016)

for(i in seq_along(years)) expect_true(file.exists(system.file("exdata",sprintf("accident_%d.csv.bz2", years[i]),package = "farsfuns")))

})

