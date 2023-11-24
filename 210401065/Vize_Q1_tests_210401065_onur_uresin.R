library(testthat)

current_dir <- getwd()
relative_path <- file.path(current_dir,"Vize_Q1_210401065_onur_uresin.R")
source(relative_path)

test_that("Test : spotify_token adlı değişken Global Workspace’de mevcuttur.", {
  expect_true(exists("spotify_token", envir = .GlobalEnv), info = "spotify_token adlı değişken mevcut değil.")
})

test_that("Test : spotify_token nesnesi bir function'dur.", {
  expect_is(spotify_token, "function", info = "maps nesnesi bir function değil.")
})

test_that("spotify_token() 2 elemanlı bir liste döndürüyor", {
  result <- spotify_token()
  expect_equal(length(result), 2)
})

test_that("ilk eleman adı status code", {
  result <- spotify_token()
  expect_equal(names(result)[1], 'status_code')
})

test_that("ilk class numeric", {
  result <- spotify_token()
  expect_is(result[[1]], "numeric", info = ".")
})

test_that("ikinci class karakter", {
  result <- spotify_token()
  expect_is(result[[2]], "character", info = ".")
})

test_that("spotify_token() 2 elemanlı bir liste döndürüyor", {
  result <- spotify_token()
  expect_equal(result$status_code, 200)
})

test_that("ikinci eleman adı token", {
  result <- spotify_token()
  expect_equal(names(result)[2], 'token')
})

test_that("ikinci class karakter", {
  result <- spotify_token()
  expect_is(result[[1]], "character", info = ".")
})

test_that("spotify_token() çağrıldığında döndürdüğü listenin ikinci elementi ’Bearer ’ ile başlamalı", {
  result <- spotify_token()
  expect_true(startsWith(result[[2]], "Bearer "))
})

test_that("spotify_token() çağrıldığında döndürdüğü listenin ikinci elementi character değişkeninin içinde 122 adet harf bulunmalı", {
  result <- spotify_token()
  expect_equal(nchar(result[[2]]), 122)
})