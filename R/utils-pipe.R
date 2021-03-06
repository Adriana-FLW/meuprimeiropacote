#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

utils::globalVariables(c('data',
                         'sistema',
                         'ano',
                         'mes',
                         'volume_porcentagem'))

utils::globalVariables(c('partidas_brasileirao',
                         'temporada',
                         'quem_ganhou',
                         'n_vitorias'))
