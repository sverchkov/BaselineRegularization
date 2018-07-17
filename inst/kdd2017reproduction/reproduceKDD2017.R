library("dplyr")
library("futile.logger")
library("BaselineRegularization")

flog.threshold(TRACE)

source( "connect.R" )

flog.info( "Loading vocabulary DB" )
con2 <- DBI::dbConnect( RSQLite::SQLite(), "../sqlite_dbs/vocab.sqlite3", flags = RSQLite::SQLITE_RO )
concept_ancestor <- tbl( con2, "concept_ancestor" )
concept_relationship <- tbl( con2, "concept_relationship" )
#standard_concepts <- tbl( con2, "concept" ) %>% filter( standard_concept == "S" )

flog.info("Selecting cohort as in Kuang et al. 2017.")

# Drugs of interest:

target_DOI <- 600000001:600000010

if ( !exists( "drugs_map" ) ){
  flog.info( "Preparing map of drugs of interest." )

  drugs_map_file <- "drugs_of_interest.rds"

  if ( !file.exists( drugs_map_file ) ){
    flog.info( "Deriving map from DB" )
    drugs_map <- concept_ancestor %>% filter( ancestor_concept_id %in% target_DOI ) %>% compute()
    saveRDS( collect( drugs_map ), file = drugs_map_file )
    flog.info( "Saved to %s", drugs_map_file )

  } else {

    flog.info( "Reading map from %s", drugs_map_file )
    drugs_map <- readRDS( drugs_map_file )

  }
}

# Conditions of interest:

target_HOI <- bind_rows(
  tibble( HOI = 501L, concept_id = 500000101:500000102 ),
  tibble( HOI = 502L, concept_id = 500000201:500000205 ),
  tibble( HOI = 503L, concept_id = 500000301:500000307 ),
  tibble( HOI = 504L, concept_id = 500000401:500000404 ),
  tibble( HOI = 505L, concept_id = 500000501:500000503 ),
  tibble( HOI = 506L, concept_id = 500000601:500000604 ),
  tibble( HOI = 507L, concept_id = 500000701 ),
  tibble( HOI = 508L, concept_id = 500000801:500000804 ),
  tibble( HOI = 509L, concept_id = 500000901:500000904 ),
  tibble( HOI = 510L, concept_id = 500001001:500001002 ) )

if ( !exists( "conditions_map" ) ){
  flog.info( "Preparing map of conditions of interest." )

  conditions_map_file <- "conditions_of_interest_v2.rds"

  if ( !file.exists( conditions_map_file ) ){
    flog.info( "Deriving map from DB" )
    conditions_map <-
      concept_ancestor %>%
      inner_join( target_HOI, by = c("ancestor_concept_id" = "concept_id"), copy = T ) %>%
      inner_join( concept_relationship %>% filter( relationship_id == "Maps to" ),
                  by = c( descendant_concept_id = "concept_id_2" ) ) %>%
      compute()

    saveRDS( collect( conditions_map ), file = conditions_map_file )
    flog.info( "Saved to %s", conditions_map_file )

  } else {

    flog.info( "Reading map from %s", conditions_map_file )
    conditions_map <- readRDS( conditions_map_file )
  }
}

# Drugs of interest in records:
flog.info( "Filtering drugs of interest in recods." )

drugs_OI <-
  tbl( con, "drug_exposure" ) %>%
  useAncestorConcepts( drugs_map, record_table_column = "drug_concept_id", copy = TRUE ) %>%
  filter( ancestor_concept_id %in% target_DOI ) %>% compute()

# Conditions of interest in records:
flog.info( "Filtering conditions of interest in recods." )

conditions_OI <-
  tbl( con, "condition_occurrence" ) %>%
  useAncestorConcepts(
    conditions_map,
    record_table_column = "condition_concept_id",
    descendant_column = "concept_id_1",
    ancestor_column = "HOI",
    copy = TRUE ) %>%
  filter( !is.na(HOI) ) %>% compute()

flog.info( "Selecting patients" )
patients <- inner_join(
  distinct( drugs_OI, person_id ),
  distinct( conditions_OI, person_id ),
  by = "person_id" ) %>% compute()

flog.info(
  "Patient count:",
  patients %>% collect() %>% summarize( n() ),
  capture = T )

flog.info( "Selecting cohort drugs" )
cohort_drugs <- inner_join( drugs_OI, patients, by="person_id" ) %>% compute()
flog.info( "Selecting cohort conditions" )
cohort_conditions <- inner_join( conditions_OI, patients, by="person_id" )

#DBI::dbDisconnect( con )

