

context("New Features: deleteNamedRegions, col2int, protectWorksheet")


## ---------------------------------------------------------------------------
## deleteNamedRegions
## ---------------------------------------------------------------------------

test_that("deleteNamedRegions removes a single named region", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, sheet = 1, x = iris, startCol = 1, startRow = 1)
  createNamedRegion(wb, sheet = 1, name = "iris",
                    rows = 1:(nrow(iris) + 1), cols = 1:ncol(iris))
  
  expect_true("iris" %in% getNamedRegions(wb))
  
  deleteNamedRegions(wb, name = "iris")
  
  expect_null(getNamedRegions(wb))
  
})


test_that("deleteNamedRegions removes multiple named regions", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, sheet = 1, x = iris, startCol = 1, startRow = 1)
  createNamedRegion(wb, sheet = 1, name = "regionA",
                    rows = 1:5, cols = 1:2)
  createNamedRegion(wb, sheet = 1, name = "regionB",
                    rows = 6:10, cols = 1:2)
  createNamedRegion(wb, sheet = 1, name = "regionC",
                    rows = 11:15, cols = 1:2)
  
  expect_equal(length(getNamedRegions(wb)), 3)
  
  deleteNamedRegions(wb, name = c("regionA", "regionC"))
  
  remaining <- getNamedRegions(wb)
  expect_equal(length(remaining), 1)
  expect_equal(remaining[1], "regionB", ignore.attributes = TRUE)
  
})


test_that("deleteNamedRegions errors on missing region", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, sheet = 1, x = iris, startCol = 1, startRow = 1)
  createNamedRegion(wb, sheet = 1, name = "iris",
                    rows = 1:(nrow(iris) + 1), cols = 1:ncol(iris))
  
  expect_error(deleteNamedRegions(wb, name = "noSuchRegion"))
  
})


test_that("deleteNamedRegions errors when workbook has no named regions", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  
  expect_error(deleteNamedRegions(wb, name = "iris"))
  
})


test_that("deleteNamedRegions with delete_data = TRUE removes cell data", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  writeData(wb, sheet = 1, x = data.frame(x = 1:3, y = 4:6),
            startCol = 1, startRow = 1)
  createNamedRegion(wb, sheet = 1, name = "myData",
                    rows = 1:4, cols = 1:2)
  
  deleteNamedRegions(wb, name = "myData", delete_data = TRUE)
  
  expect_null(getNamedRegions(wb))
  
  ## after deleting, the cells should be empty
  out <- read.xlsx(wb, sheet = 1, colNames = FALSE)
  expect_true(is.null(out) || nrow(out) == 0)
  
})


## ---------------------------------------------------------------------------
## col2int
## ---------------------------------------------------------------------------

test_that("col2int converts single letters correctly", {
  
  expect_equal(col2int("A"), 1L)
  expect_equal(col2int("B"), 2L)
  expect_equal(col2int("Z"), 26L)
  
})


test_that("col2int converts multi-letter columns correctly", {
  
  expect_equal(col2int("AA"), 27L)
  expect_equal(col2int("AB"), 28L)
  expect_equal(col2int("AZ"), 52L)
  expect_equal(col2int("BA"), 53L)
  
})


test_that("col2int is the inverse of int2col", {
  
  x <- 1:100
  expect_equal(col2int(int2col(x)), x)
  
})


test_that("col2int handles vectors", {
  
  expect_equal(col2int(c("A", "B", "Z", "AA")), c(1L, 2L, 26L, 27L))
  
})


test_that("col2int is case-insensitive", {
  
  expect_equal(col2int("a"), col2int("A"))
  expect_equal(col2int("aa"), col2int("AA"))
  
})


test_that("col2int errors on non-character input", {
  
  expect_error(col2int(1))
  
})


test_that("col2int errors on cell references (letters + digits)", {
  
  expect_error(col2int("A1"))
  
})


## ---------------------------------------------------------------------------
## protectWorksheet / unprotectWorksheet
## ---------------------------------------------------------------------------

test_that("protectWorksheet adds sheetProtection XML", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  
  protectWorksheet(wb, "Sheet 1")
  
  xml <- wb$worksheets[[1]]$sheetProtection
  expect_true(length(xml) == 1)
  expect_true(grepl("sheetProtection", xml))
  expect_true(grepl('sheet="1"', xml))
  
})


test_that("protectWorksheet with password embeds hash in XML", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  
  protectWorksheet(wb, "Sheet 1", password = "test")
  
  xml <- wb$worksheets[[1]]$sheetProtection
  expect_true(grepl("password=", xml))
  
})


test_that("protectWorksheet with protect = FALSE clears protection", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  
  protectWorksheet(wb, "Sheet 1", password = "secret")
  expect_true(length(wb$worksheets[[1]]$sheetProtection) > 0)
  
  protectWorksheet(wb, "Sheet 1", protect = FALSE)
  expect_equal(length(wb$worksheets[[1]]$sheetProtection), 0)
  
})


test_that("unprotectWorksheet clears protection", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")
  
  protectWorksheet(wb, "Sheet 1")
  expect_true(length(wb$worksheets[[1]]$sheetProtection) > 0)
  
  unprotectWorksheet(wb, "Sheet 1")
  expect_equal(length(wb$worksheets[[1]]$sheetProtection), 0)
  
})


test_that("protectWorksheet round-trips through save/load", {
  
  wb <- createWorkbook()
  addWorksheet(wb, "Protected")
  writeData(wb, "Protected", iris)
  protectWorksheet(wb, "Protected", password = "pw123")
  
  out_file <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, out_file, overwrite = TRUE)
  
  ## Check that the sheetProtection XML is in the saved file
  xml_dir <- tempfile()
  unzip(out_file, exdir = xml_dir)
  sheet_path <- file.path(xml_dir, "xl", "worksheets", "sheet1.xml")
  expect_true(file.exists(sheet_path))
  sheet_xml <- readLines(sheet_path, warn = FALSE, encoding = "UTF-8")
  sheet_xml <- paste(sheet_xml, collapse = "")
  
  expect_true(grepl("sheetProtection", sheet_xml))
  expect_true(grepl("password=", sheet_xml))
  
  unlink(xml_dir, recursive = TRUE)
  
})
