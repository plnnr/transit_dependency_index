library(shiny)
library(shinythemes)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(DT)
library(sf)
library(tigris)
library(purrr)
library(mapview)
library(sp)
library(data.table)

options(
  scipen = 999,
  digits = 4,
  tigris_class = "sf",
  tigris_use_cache = T
)

## List of MSA shortnames
msa_shortname <- rio::import("msa_shortname_brookings.xlsx") %>%
  rename(cbsa13 = `CBSA FIPS (2013)`, cbsa_shortname = `CBSA Short Name (2013)`) #%>% filter(cbsa13 %in% msas$GEOID)

## County to MSA crosswalk
county2msa <- read.csv(textConnection(readLines("geocorr2014_county_to_msa.csv")[-2]), 
                       colClasses = c("character", "character", "character", "character", "integer", "integer"), 
                       header = TRUE, sep=",") %>%
  left_join(., msa_shortname, by = c("cbsa" = "cbsa13"))

##### Functions  ####

## opposite of %in%
`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)

## Force vector to be between 0 and 1
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

## Get counties from cbsa code
get_counties_from_cbsa <- function(cbsa_code) {
  cbsa_code <- as.numeric(cbsa_code)
  county2msa %>%
    filter(cbsa == cbsa_code) %>%
    pull(county)
}

## Get states from vector of 5-digit FIPS
get_states_from_stcnty_fips <- function(counties){
  counties %>%
    as.data.frame() %>%
    setNames("stcnty") %>%
    mutate(st = substr(stcnty, 1, 2)) %>%
    distinct(st) %>%
    pull(st)
}

