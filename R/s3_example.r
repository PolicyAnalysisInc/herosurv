
foo <- list(a=1, b=2, c=3)
class(foo) <- c('bar', 'foo', 'blah')

sumAll <- function(x) {
    UseMethod('sumAll', x)
}

sumAll.blah <- function(x) {
    sum(x$a, x$b, x$c)
}

sumAll.foo <- function(x) {
    sum(x$a, x$b)
}

sumAll(foo)