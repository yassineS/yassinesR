test_that("all palettes return named vectors", {
  palettes <- c("main", "cool", "warm", "contrast", "bodl")
  
  for (palette_name in palettes) {
    colors <- yassine_colors(palette = palette_name)
    
    # Check that colors is a named vector
    expect_true(!is.null(names(colors)), 
                info = paste("Palette", palette_name, "should have named colors"))
    
    # Check that all names are non-empty
    expect_true(all(nchar(names(colors)) > 0),
                info = paste("All color names in palette", palette_name, "should be non-empty"))
    
    # Check that there are no duplicate names
    expect_equal(length(names(colors)), length(unique(names(colors))),
                 info = paste("Palette", palette_name, "should not have duplicate color names"))
  }
})

test_that("color names are meaningful and follow naming conventions", {
  # Test main palette
  main_colors <- yassine_colors("main")
  expect_true("Blue" %in% names(main_colors))
  expect_true("Orange" %in% names(main_colors))
  
  # Test bodl palette has Australian-themed names
  bodl_colors <- yassine_colors("bodl")
  expect_true("CentralRed" %in% names(bodl_colors))
  expect_true("BanksiaOrange" %in% names(bodl_colors))
  expect_true("KingfisherBlue" %in% names(bodl_colors))
})

test_that("colors can be accessed by their names", {
  main_colors <- yassine_colors("main")
  
  # Test that we can access colors by name
  expect_equal(main_colors["Blue"], c(Blue = "#0066CC"))
  expect_equal(main_colors["Orange"], c(Orange = "#FF6B35"))
  
  bodl_colors <- yassine_colors("bodl")
  expect_equal(bodl_colors["CentralRed"], c(CentralRed = "#9E2900"))
  expect_equal(bodl_colors["FairyWrenBlue"], c(FairyWrenBlue = "#68B0E3"))
})

test_that("palette subset returns named vectors", {
  # When requesting fewer colors than available, names should be preserved
  main_subset <- yassine_colors("main", n = 3)
  expect_true(!is.null(names(main_subset)))
  expect_equal(length(main_subset), 3)
  expect_equal(names(main_subset)[1], "Blue")
  expect_equal(names(main_subset)[2], "Orange")
  expect_equal(names(main_subset)[3], "DarkBlue")
})

test_that("continuous palette interpolation maintains functionality", {
  # Continuous palettes won't preserve names (colorRampPalette behavior)
  # but the function should still work
  continuous_colors <- yassine_colors("main", n = 10, type = "continuous")
  expect_equal(length(continuous_colors), 10)
  # Continuous palettes from colorRampPalette typically don't have names
})
