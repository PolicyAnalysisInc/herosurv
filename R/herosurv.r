#' @importFrom stats pweibull dweibull rweibull qweibull plnorm dlnorm rlnorm qlnorm as.formula model.matrix stepfun
#' @importFrom dplyr transmute left_join mutate filter summarize select distinct `%>%` slice group_by n
#' @importFrom purrr walk map discard map_chr map2 set_names map_lgl imap_dbl
#' @importFrom stringr str_glue_data str_to_title
#' @importFrom flexsurvcure pmixsurv pnmixsurv
#' @importFrom utils capture.output head tail
#' @importFrom tibble tibble as_tibble
#' @importFrom formula.tools rhs
#' @importFrom plyr ldply ddply
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom msm ppexp
#' @import flexsurv
#' @import survival
NULL 

#' @tests
#' options(
#'  list(
#'      herosurv.show_call_signature_in_errors = NULL,
#'      herosurv.show_call_signature_in_warnings = NULL
#'  )
#' )
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
    props_already_set <- names(discard(opt[props_with_defaults], is.null))
    props_to_set <- props_with_defaults[!props_with_defaults %in% props_already_set]
    options_to_set <- default_options[props_to_set]
    if (length(options_to_set) > 0) {
        options(default_options[props_to_set])
    }

    invisible()
}