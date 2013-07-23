library('GSLtools')

if(require("testthat", quietly = TRUE)) {
    test_package("GSLtools")
} else {
    print( "package 'testthat' not available, cannot run unit tests" )
}
