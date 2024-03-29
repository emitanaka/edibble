#' Skittles experiment
#'
#' @description
#' This contains the data from the skittle experiment conducted by
#' Nick Tierney. The goal of the experiment was to assess if people
#' can discern the flavour of the skittle (indicated by color of the skittle)
#' based on taste alone. The participants are blindfolded.
#'
#' The experiment had 3 participants with each participant
#' tasting 10 skittles, 2 of each 5 color, in a random order.
#' \describe{
#' \item{skittle_type}{The type of skittle. Coincides with `real_skittle`.}
#' \item{person}{The participant.}
#' \item{order}{The order the skittle was tasted.}
#' \item{choice}{The participant's choice.}
#' \item{real_skittle}{The actual skittle color.}
#' }
#'
#' @family experimental data
#' @source https://github.com/njtierney/skittles
"skittles"

# TODO Ask Nick whether participants knew if they knew the color was 2 each

#' Lady tasting tea
#'
#' @description
#' Lady tasting tea experiment was described in Fisher (1935) to test
#' the ability of a lady who said she tell whether the tea or milk was added
#' first to a cup of tea.
#'
#' The experiment consisted of preparing eight cups of tea, four with milk poured
#' first and the other four with tea poured first. The lady has been told in
#' advance that there are four of each kind of preparation.
#'
#' This data consists of the same experimental structure and result but
#' the order presented in practice is unknown.
#' \describe{
#' \item{cup}{The cup number.}
#' \item{first}{The cup of tea prepared with milk or tea first.}
#' \item{guess}{The guess by lady which one was poured first.}
#' \item{correct}{Whether the lady's guess was correct.}
#' }
#'
#' @family experimental data
#' @source Fisher, Ronald (1935) The Design of Experiments.
#'
"lady_tasting_tea"

