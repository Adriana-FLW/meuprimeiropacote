#' Pior time
#'
#' Esse programa retorna o prior time da temporada. teste2 outro
#'
#' @param time nome do time
#'
#' @return uma linha
#' @export
#'
#' @examples encontrar_pior_ano_time('Santos)
encontrar_pior_ano_time <- function(time) {
  partidas_brasileirao %>%
    dplyr::group_by(temporada, quem_ganhou) %>%
    dplyr::filter(quem_ganhou != "Empate", quem_ganhou %in% time) %>%
    dplyr::count(quem_ganhou, sort = TRUE, name = "n_vitorias") %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_vitorias == min(n_vitorias)) %>%
    dplyr::rename("time" = quem_ganhou)
}
