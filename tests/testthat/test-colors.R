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
  
  # Test that we can access colors by name and get a named character vector
  blue_color <- main_colors["Blue"]
  expect_true(is.character(blue_color))
  expect_equal(names(blue_color), "Blue")
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", blue_color))  # Valid hex color
  
  orange_color <- main_colors["Orange"]
  expect_equal(names(orange_color), "Orange")
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", orange_color))
  
  bodl_colors <- yassine_colors("bodl")
  central_red <- bodl_colors["CentralRed"]
  expect_equal(names(central_red), "CentralRed")
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", central_red))
  
  fairy_wren <- bodl_colors["FairyWrenBlue"]
  expect_equal(names(fairy_wren), "FairyWrenBlue")
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", fairy_wren))
})

test_that("palette subset returns named vectors", {
  # When requesting fewer colors than available, names should be preserved
  main_full <- yassine_colors("main")
  main_subset <- yassine_colors("main", n = 3)
  
  expect_true(!is.null(names(main_subset)))
  expect_equal(length(main_subset), 3)
  
  # Subset names should match the first n names from the full palette
  expect_equal(names(main_subset), names(main_full)[1:3])
})

test_that("continuous palette interpolation maintains functionality", {
  # Continuous palettes won't preserve names (colorRampPalette behavior)
  # but the function should still work
  continuous_colors <- yassine_colors("main", n = 10, type = "continuous")
  expect_equal(length(continuous_colors), 10)
  # Continuous palettes from colorRampPalette typically don't have names
})

test_that("scale_color_yassine returns unnamed vectors for ggplot2", {
  # The palette function within scale_color_yassine must return unnamed vectors
  # to work correctly with ggplot2's discrete_scale
  
  # Create a scale object
  scale <- scale_color_yassine(palette = "main")
  
  # Extract the palette function from the scale
  # The palette function should be in the scale object
  expect_s3_class(scale, "ScaleDiscrete")
  
  # Test that the palette function returns unnamed vectors
  # We can access the palette function through the scale's palette field
  if (!is.null(scale$palette)) {
    colors_3 <- scale$palette(3)
    expect_null(names(colors_3), 
                info = "Palette function should return unnamed color vectors")
    expect_equal(length(colors_3), 3)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_3)))
  }
})

test_that("scale_fill_yassine returns unnamed vectors for ggplot2", {
  # The palette function within scale_fill_yassine must return unnamed vectors
  # to work correctly with ggplot2's discrete_scale
  
  # Create a scale object
  scale <- scale_fill_yassine(palette = "main")
  
  # Extract the palette function from the scale
  expect_s3_class(scale, "ScaleDiscrete")
  
  # Test that the palette function returns unnamed vectors
  if (!is.null(scale$palette)) {
    colors_3 <- scale$palette(3)
    expect_null(names(colors_3), 
                info = "Palette function should return unnamed color vectors")
    expect_equal(length(colors_3), 3)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_3)))
  }
})
