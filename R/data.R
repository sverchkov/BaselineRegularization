#' A mini subset of the synpuf dataset in OMOP CDM
#'
#' A mini subset of the CMS 2008-2010 Data Entrepreneurs’ Synthetic Public Use File (DE-SynPUF) dataset
#' in OMOP CDM V.5.
#'
#' @docType data
#' @usage data("synpuf_mini")
#' @format a list of dataframes: `observation_period`, `condition_occurrence`, `drug_exposure`
#' @keywords datasets
#' @source
#' The CMS 2008-2010 Data Entrepreneurs’ Synthetic Public Use File (DE-SynPUF)
#' https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html
"synpuf_mini"

#' A subset of the "concept" table for OMOP concepts
#'
#' A subset of the "concept" table for OMOP concepts
#'
#' @docType data
#' @usage data("omop_doi_concept_ancestor")
#' @format a data frame (tibble)
#' @keywords datasets
#' @source OMOP CDM Vocabulary tables
"omop_concept_names"

#' A subset of the "concept ancestor" table for OMOP concepts
#'
#' A subset of the "concept ancestor" table for OMOP concepts
#'
#' @docType data
#' @usage data("omop_doi_concept_ancestor")
#' @format a data frame (tibble) with columns `descendant_concept_id` and `ancestor_concept_id`
#' @keywords datasets
#' @source OMOP CDM Vocabulary tables
"omop_doi_concept_ancestor"

#' A subset of the OMOP CDM "concept ancestor" table
#'
#' A subset of the OMOP CDM "concept ancestor" table
#'
#' @docType data
#' @usage data("omop_doi_concept_ancestor")
#' @format a data frame (tibble)
#' @keywords datasets
#' @source OMOP CDM Vocabulary tables
"cdm_some_concept_names"

#' A mapping from OMOP HOIs to condition concept IDs
#'
#' A mapping from OMOP HOIs to condition concept IDs
#'
#' @docType data
#' @usage data("omop_doi_concept_ancestor")
#' @format a data frame (tibble)
#' @keywords datasets
#' @source OMOP CDM Vocabulary tables and HOI definitions
"omop_hoi_conditions_map"
