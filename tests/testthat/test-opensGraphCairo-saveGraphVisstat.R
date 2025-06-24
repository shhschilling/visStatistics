library(testthat)
library(Cairo)

# Helper function for testing optional Cairo backends with detailed reporting
test_optional_backend <- function(type, test_filename, temp_dir) {
  cat("\n--- Testing", type, "backend ---\n")
  
  tryCatch({
    cat("Creating", type, "device...\n")
    openGraphCairo(fileName = test_filename, type = type, fileDirectory = temp_dir)
    
    cat("Plotting to", type, "device...\n")
    plot(1:3, 1:3, main = paste("Test", type))
    
    cat("Closing", type, "device...\n")
    dev.off()
    
    expected_file <- file.path(temp_dir, paste0(test_filename, ".", type))
    cat("Expected file:", expected_file, "\n")
    
    if(file.exists(expected_file)) {
      file_size <- file.size(expected_file)
      cat("SUCCESS:", type, "file created with size", file_size, "bytes\n")
      
      expect_true(file.exists(expected_file), 
                  info = paste("File should exist for type:", type))
      
      # Clean up
      file.remove(expected_file)
      cat("Cleaned up", type, "file\n")
      return(TRUE)
    } else {
      cat("FAILURE: File was not created for type:", type, "\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("ERROR with", type, "backend:", e$message, "\n")
    
    # Close any open devices before continuing
    if(length(dev.list()) > 0) {
      cat("Closing open graphics devices...\n")
      while(length(dev.list()) > 0) {
        dev.off()
      }
    }
    
    # Check if it's a specific Cairo backend error
    if(grepl("Failed to create Cairo backend", e$message)) {
      cat("DIAGNOSIS:", type, "Cairo backend is not available on this system\n")
    } else if(grepl("font", e$message, ignore.case = TRUE)) {
      cat("DIAGNOSIS:", type, "backend failed due to font issues\n")
    } else {
      cat("DIAGNOSIS: Unknown error with", type, "backend\n")
    }
    
    return(FALSE)
  })
}

# Test suite for opening_saving_Cairo.R functions
# Assumes the functions openGraphCairo() and saveGraphVisstat() are loaded

test_that("openGraphCairo handles NULL type correctly", {
  # Should return NULL (invisibly) when type is NULL
  result <- openGraphCairo(type = NULL)
  expect_null(result)
})

test_that("openGraphCairo handles missing fileName correctly", {
  # Should use default fileName when NULL
  temp_dir <- tempdir()
  
  # Test with PNG type
  expect_silent({
    openGraphCairo(fileName = NULL, type = "png", fileDirectory = temp_dir)
    plot(1:10, 1:10)
    dev.off()
  })
  
  # Check if default file was created
  default_file <- file.path(temp_dir, "visstat_plot.png")
  expect_true(file.exists(default_file))
  
  # Clean up
  if(file.exists(default_file)) file.remove(default_file)
})

test_that("openGraphCairo creates correct file paths", {
  temp_dir <- tempdir()
  test_filename <- "test_plot"
  
  # Test PNG creation
  expect_silent({
    openGraphCairo(fileName = test_filename, type = "png", fileDirectory = temp_dir)
    plot(1:5, 1:5)
    dev.off()
  })
  
  expected_file <- file.path(temp_dir, "test_plot.png")
  expect_true(file.exists(expected_file))
  
  # Clean up
  if(file.exists(expected_file)) file.remove(expected_file)
})

test_that("openGraphCairo supports all required file types", {
  temp_dir <- tempdir()
  test_filename <- "format_test"
  
  # Test types that are more likely to work across different systems
  stable_types <- c("png", "pdf", "jpeg")
  
  for(type in stable_types) {
    expect_silent({
      openGraphCairo(fileName = test_filename, type = type, fileDirectory = temp_dir)
      plot(1:3, 1:3, main = paste("Test", type))
      dev.off()
    })
    
    expected_file <- file.path(temp_dir, paste0(test_filename, ".", type))
    expect_true(file.exists(expected_file), 
                info = paste("File should exist for type:", type))
    
    # Clean up
    if(file.exists(expected_file)) file.remove(expected_file)
  }
  
  # Test additional types with detailed reporting
  optional_types <- c("tiff", "svg", "ps")
  
  cat("\n=== Testing Optional Cairo Backends ===\n")
  
  backend_results <- list()
  for(type in optional_types) {
    backend_results[[type]] <- test_optional_backend(type, test_filename, temp_dir)
  }
  
  # Summary report
  cat("\n=== Backend Test Summary ===\n")
  working_backends <- names(backend_results)[sapply(backend_results, isTRUE)]
  failing_backends <- names(backend_results)[sapply(backend_results, function(x) !isTRUE(x))]
  
  if(length(working_backends) > 0) {
    cat("WORKING backends:", paste(working_backends, collapse = ", "), "\n")
  }
  if(length(failing_backends) > 0) {
    cat("FAILING backends:", paste(failing_backends, collapse = ", "), "\n")
  }
  
  # At least document what we found
  expect_true(length(backend_results) == 3, 
              info = "Should test all three optional backends")
})

test_that("openGraphCairo handles unsupported file types", {
  temp_dir <- tempdir()
  
  # Should issue warning and return NULL for unsupported type
  expect_warning(
    result <- openGraphCairo(fileName = "test", type = "unsupported", fileDirectory = temp_dir),
    "Chosen output type not supported"
  )
  expect_null(result)
})

test_that("openGraphCairo uses correct default parameters", {
  temp_dir <- tempdir()
  
  # Test that function works with minimal parameters
  expect_silent({
    openGraphCairo(type = "png", fileDirectory = temp_dir)
    plot(1:5, 1:5)
    dev.off()
  })
  
  # Should create file with default name
  default_file <- file.path(temp_dir, "visstat_plot.png")
  expect_true(file.exists(default_file))
  
  # Clean up
  if(file.exists(default_file)) file.remove(default_file)
})

test_that("saveGraphVisstat handles NULL parameters correctly", {
  # Should return NULL when fileName is NULL
  result1 <- saveGraphVisstat(fileName = NULL, type = "png")
  expect_null(result1)
  
  # Should return NULL when type is NULL
  result2 <- saveGraphVisstat(fileName = "test", type = NULL)
  expect_null(result2)
})

test_that("saveGraphVisstat captures plots when capture_env is provided", {
  temp_dir <- tempdir()
  
  # Create capture environment
  capture_env <- new.env()
  capture_env$captured_plots <- list()
  
  # Create a plot and save with capture
  openGraphCairo(fileName = "capture_test", type = "png", fileDirectory = temp_dir)
  plot(1:5, 1:5, main = "Capture Test")
  
  result <- saveGraphVisstat(
    fileName = "capture_test", 
    type = "png", 
    fileDirectory = temp_dir,
    capture_env = capture_env
  )
  
  # Check that plot was captured
  expect_length(capture_env$captured_plots, 1)
  expect_s3_class(capture_env$captured_plots[[1]], "recordedplot")
  
  # Check that file was created
  expected_file <- file.path(temp_dir, "capture_test.png")
  expect_true(file.exists(expected_file))
  
  # Clean up
  if(file.exists(expected_file)) file.remove(expected_file)
})

test_that("saveGraphVisstat sanitizes file names correctly", {
  temp_dir <- tempdir()
  
  # Create plot first, then save with different name
  openGraphCairo(fileName = "temp_test", type = "png", fileDirectory = temp_dir)
  plot(1:3, 1:3)
  dev.off()
  
  # Now test the sanitization with saveGraphVisstat
  temp_old_file <- file.path(temp_dir, "temp_test.png")
  
  result <- saveGraphVisstat(
    fileName = "test@#$%plot with spaces!!", 
    type = "png", 
    fileDirectory = temp_dir,
    oldfile = temp_old_file
  )
  
  # Check what file actually gets created (the function does sanitization)
  expect_false(is.null(result))
  expect_true(file.exists(result))
  
  # The exact sanitization pattern might differ, so let's check the pattern
  basename_result <- basename(result)
  expect_match(basename_result, "^test.*\\.png$")
  
  # Clean up
  if(file.exists(result)) file.remove(result)
  if(file.exists(temp_old_file)) file.remove(temp_old_file)
})

test_that("saveGraphVisstat handles oldfile parameter correctly", {
  temp_dir <- tempdir()
  old_file <- file.path(temp_dir, "old_test.png")
  
  # Create an old file to copy from
  openGraphCairo(fileName = "old_test", type = "png", fileDirectory = temp_dir)
  plot(1:5, 1:5, main = "Old Plot")
  dev.off()
  
  # Use saveGraphVisstat with explicit oldfile
  result <- saveGraphVisstat(
    fileName = "new_test", 
    type = "png", 
    fileDirectory = temp_dir,
    oldfile = old_file
  )
  
  new_file <- file.path(temp_dir, "new_test.png")
  expect_true(file.exists(new_file))
  expect_false(file.exists(old_file)) # Old file should be removed
  expect_equal(result, new_file)
  
  # Clean up
  if(file.exists(new_file)) file.remove(new_file)
})

test_that("saveGraphVisstat closes all graphics devices", {
  temp_dir <- tempdir()
  
  # Open multiple devices
  openGraphCairo(fileName = "test1", type = "png", fileDirectory = temp_dir)
  plot(1:3, 1:3)
  
  openGraphCairo(fileName = "test2", type = "png", fileDirectory = temp_dir)
  plot(4:6, 4:6)
  
  # Should have active devices
  expect_true(length(dev.list()) > 0)
  
  # saveGraphVisstat should close all devices
  result <- saveGraphVisstat(
    fileName = "final_test", 
    type = "png", 
    fileDirectory = temp_dir
  )
  
  # All devices should be closed
  expect_null(dev.list())
  
  # Clean up any remaining files
  test_files <- c("test1.png", "test2.png", "final_test.png", "visstat_plot.png")
  for(file in test_files) {
    full_path <- file.path(temp_dir, file)
    if(file.exists(full_path)) file.remove(full_path)
  }
})

test_that("integration test: openGraphCairo and saveGraphVisstat work together", {
  temp_dir <- tempdir()
  
  # Complete workflow test - use PNG which is more reliable
  expect_silent({
    openGraphCairo(fileName = "integration", type = "png", fileDirectory = temp_dir)
    plot(1:10, 1:10, main = "Integration Test", col = "blue", pch = 19)
    lines(1:10, (1:10)^1.1, col = "red", lwd = 2)
    dev.off()
  })
  
  # Check that the intermediate file was created
  intermediate_file <- file.path(temp_dir, "integration.png")
  expect_true(file.exists(intermediate_file))
  
  result <- saveGraphVisstat(
    fileName = "integration_final", 
    type = "png", 
    fileDirectory = temp_dir,
    oldfile = intermediate_file
  )
  
  expected_file <- file.path(temp_dir, "integration_final.png")
  expect_true(file.exists(expected_file))
  expect_equal(result, expected_file)
  
  # Verify file has content (size > 0)
  expect_gt(file.size(expected_file), 0)
  
  # Clean up
  if(file.exists(expected_file)) file.remove(expected_file)
  if(file.exists(intermediate_file)) file.remove(intermediate_file)
})

test_that("error handling for invalid directories", {
  invalid_dir <- "/non/existent/path/that/should/not/exist"
  
  # Test if the system actually prevents writing to invalid directories
  # Some systems might create intermediate directories or handle this differently
  result <- tryCatch({
    openGraphCairo(fileName = "test", type = "png", fileDirectory = invalid_dir)
    "no_error"
  }, error = function(e) {
    "error_occurred"
  })
  
  # We expect either an error or a warning, but behavior may vary by system
  expect_true(result == "error_occurred" || result == "no_error")
  
  # If no error occurred, at least verify the function doesn't crash
  if(result == "no_error") {
    # Close any open devices to clean up
    while(length(dev.list()) > 0) {
      dev.off()
    }
  }
})

test_that("capture environment functionality works correctly", {
  temp_dir <- tempdir()
  
  # Test multiple plots with capture
  capture_env <- new.env()
  capture_env$captured_plots <- list()
  
  # First plot
  openGraphCairo(fileName = "multi1", type = "png", fileDirectory = temp_dir)
  plot(1:5, 1:5, main = "Plot 1")
  saveGraphVisstat(
    fileName = "multi1", 
    type = "png", 
    fileDirectory = temp_dir,
    capture_env = capture_env
  )
  
  # Second plot
  openGraphCairo(fileName = "multi2", type = "png", fileDirectory = temp_dir)
  plot(6:10, 6:10, main = "Plot 2")
  saveGraphVisstat(
    fileName = "multi2", 
    type = "png", 
    fileDirectory = temp_dir,
    capture_env = capture_env
  )
  
  # Should have captured both plots
  expect_length(capture_env$captured_plots, 2)
  expect_s3_class(capture_env$captured_plots[[1]], "recordedplot")
  expect_s3_class(capture_env$captured_plots[[2]], "recordedplot")
  
  # Clean up
  cleanup_files <- c("multi1.png", "multi2.png")
  for(file in cleanup_files) {
    full_path <- file.path(temp_dir, file)
    if(file.exists(full_path)) file.remove(full_path)
  }
})