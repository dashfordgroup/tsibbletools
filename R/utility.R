#' Store an object to the global env mid-pipeline
#'
#' @param .data an object to be assigned to name
#' @param name variable name
#'
#' @return .data argument, to allow continued piping, with the side effect of assigning the .data object to an object using the name argument
#' @export
#'
#' @examples
stash <-
    function(.data, name = NULL) {
        if (is.null(name)) name <- "tmp"
        do.call("<<-",list(x = rlang::as_string(rlang::ensym(name)), value = .data))
        .data
    }
