#' @importFrom stringr str_glue_data str_to_title
#' @importFrom glue glue
#' @importFrom purrr walk map
#' @importFrom flexsurvcure pmixsurv pnmixsurv
#' @import stats
#' @import flexsurv
NULL 

#' @tests
#' 
#' .onLoad()
#' expect_equal(
#'  c(
#'      'herosurv.show_call_signature_in_errors',
#'      'herosurv.show_call_signature_in_warnings'
#'  ) %in% names(options()),
#'  c(TRUE, TRUE)
#' )
#' 
.onLoad <- function(libname, pkgname) {


    # Set default values for options upon loading package
    opt <- options()
    props_with_defaults <- names(default_options)
    props_already_set <- names(opt)
    props_to_set <- props_with_defaults[!props_with_defaults %in% props_already_set]
    options_to_set <- default_options[props_to_set]
    if (length(options_to_set) > 0) {
        options(default_options[props_to_set])
    }

    invisible()
}