library(testthat)
library(readr)
library(purrr)
library(dplyr)

env <- new.env()

test_that("CSV se lee sin errores", {
  expect_failure(expect_warning(env$datos <- readr::read_csv("../../cie10/catalogo.csv")))
})

tipos_columna <- c(
  CAPITULO = "numeric",
  NOMBRE_CAPITULO = "character",
  COD_3 = "character",
  DESCRIPCION_COD_3 = "character",
  COD_4 = "character",
  DESCRIPCION_COD_4 = "character",
  CDM = "numeric",
  DESCRIPCION_CDM = "character",
  CE = "character",
  DESCRIPCION_CE = "character"
)

test_that("CSV de CIE10 sigue los tipos de datos correspondientes", {
  expect_equal(purrr::map_chr(env$datos, class), tipos_columna)
})

test_that("Cada CDM tiene una sola descripcion", {
  combinaciones_cdm <- distinct(env$datos, CDM, DESCRIPCION_CDM)
  expect_equal(sum(duplicated(combinaciones_cdm$CDM)), 0)
  expect_equal(sum(duplicated(combinaciones_cdm$DESCRIPCION_CDM)), 0)
})

test_that("Cada CE tiene una sola descripcion", {
  combinaciones_ce <- distinct(env$datos, CE, DESCRIPCION_CE)
  expect_equal(sum(duplicated(combinaciones_ce$CE)), 0)
  expect_equal(sum(duplicated(combinaciones_ce$DESCRIPCION_CE)), 0)
})

test_that("Cada COD_3 tiene una sola descripcion", {
  combinaciones_cod_3 <- distinct(env$datos, COD_3, DESCRIPCION_COD_3)
  expect_equal(sum(duplicated(combinaciones_cod_3$COD_3)), 0)
})

test_that("Cada COD_4 debe ser único", {
  expect_equal(n_distinct(env$datos$COD_4), nrow(env$datos))
})

