#' Store an object to the global env mid-pipeline
#'
#' @param .data an object to be assigned to name
#' @param name variable name
#'
#' @return .data argument, to allow continued piping
#' @export
#'
#' @examples
stash <-
    function(.data, name) {
        do.call("<<-",list(x = rlang::as_string(rlang::ensym(name)), value = .data))
        .data
    }
