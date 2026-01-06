#' French Communes Data
#'
#' A dataset containing information about French communes,
#' including their codes, types, and names.
#'
#' @format A data frame with 12 columns:
#' \describe{
#'   \item{TYPECOM}{(character) Commune type, 4 characters.}
#'   \item{COM}{(character) Commune code, 5 digits.}
#'   \item{REG}{(integer) Region code, 2 digits.}
#'   \item{DEP}{(character) Department code, 3 characters.}
#'   \item{CTCD}{(character) Code of the territorial community with departmental powers, 4 characters.}
#'   \item{ARR}{(character) Borough code, 4 characters.}
#'   \item{TNCC}{(integer) Type of clear name, 1 digit.}
#'   \item{NCC}{(character) Clear name (uppercase), up to 200 characters.}
#'   \item{NCCENR}{(character) Clear name with rich typography, up to 200 characters.}
#'   \item{LIBELLE}{(character) Clear name with rich typography and article, up to 200 characters.}
#'   \item{CAN}{(character) Canton code, 5 characters. For “multi-cantonal” communes, the code ranges from 99 to 90 (pseudo-canton) or from 89 to 80 (new communes).}
#'   \item{COMPARENT}{(integer) Parent commune code for municipal borough and associated or delegated communes, 5 digits.}
#' }
#'
#' @source \url{https://www.insee.fr/fr/information/8377162}
"commune_2025"

#' French Departments Data
#'
#' A dataset containing information about French departments,
#' including their codes and names.
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{REG}{(integer) Region code, 2 digits.}
#'   \item{DEP}{(character) Department code, 3 characters.}
#'   \item{CHEFLIEU}{(character) Department capital code, 5 digits.}
#'   \item{TNCC}{(integer) Type of clear name, 1 digit.}
#'   \item{NCC}{(character) Clear name (uppercase), up to 200 characters.}
#'   \item{NCCENR}{(character) Clear name with rich typography, up to 200 characters.}
#'   \item{LIBELLE}{(character) Clear name with rich typography and article, up to 200 characters.}
#' }
#'
#' @source \url{https://www.insee.fr/fr/information/8377162}
"departement_2025"

#' French Regions Data
#'
#' A dataset containing information about French regions,
#' including their codes and names.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{REG}{(integer) Region code, 2 digits.}
#'   \item{CHEFLIEU}{(character) Region capital code, 5 digits.}
#'   \item{TNCC}{(integer) Type of clear name, 1 digit.}
#'   \item{NCC}{(character) Clear name (uppercase), up to 200 characters.}
#'   \item{NCCENR}{(character) Clear name with rich typography, up to 200 characters.}
#'   \item{LIBELLE}{(character) Clear name with rich typography and article, up to 200 characters.}
#' }
#'
#' @source \url{https://www.insee.fr/fr/information/8377162}
"region_2025"
