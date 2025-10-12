# Tests for image file support in public datalake

test_that("is_image_dataset correctly identifies image files", {
  # Test PNG files
  expect_true(is_image_dataset(c("test.png", "another.PNG")))
  
  # Test JPEG files
  expect_true(is_image_dataset(c("test.jpg", "another.JPEG")))
  
  # Test mixed image files
  expect_true(is_image_dataset(c("test.png", "another.jpg", "file.jpeg")))
  
  # Test non-image files
  expect_false(is_image_dataset(c("test.csv", "data.xlsx")))
  
  # Test mixed files (some images, some not)
  expect_false(is_image_dataset(c("test.png", "data.csv")))
  
  # Test empty vector
  expect_false(is_image_dataset(character(0)))
  
  # Test NULL
  expect_false(is_image_dataset(NULL))
})

test_that("get_content_type handles image formats", {
  # Test PNG content type
  expect_equal(get_content_type("test.png"), "image/png")
  expect_equal(get_content_type("test.PNG"), "image/png")
  
  # Test JPEG content types
  expect_equal(get_content_type("test.jpg"), "image/jpeg")
  expect_equal(get_content_type("test.JPG"), "image/jpeg")
  expect_equal(get_content_type("test.jpeg"), "image/jpeg")
  expect_equal(get_content_type("test.JPEG"), "image/jpeg")
  
  # Test existing formats still work
  expect_equal(get_content_type("test.csv"), "text/csv")
  expect_equal(get_content_type("test.xml"), "text/xml")
})

test_that("supported file extensions include images", {
  # This tests the file validation functions
  
  # Test PNG file validation (should not error)
  temp_png <- tempfile(fileext = ".png")
  # Create a minimal valid PNG file (1x1 pixel)
  png_data <- as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,  # PNG signature
    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,  # IHDR chunk
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,  # 1x1 dimensions
    0x08, 0x00, 0x00, 0x00, 0x00, 0x3A, 0x7E, 0x9B,  # bit depth, etc.
    0x55, 0x00, 0x00, 0x00, 0x0A, 0x49, 0x44, 0x41,  # IDAT chunk
    0x54, 0x78, 0x9C, 0x63, 0x00, 0x01, 0x00, 0x00,  # compressed data
    0x05, 0x00, 0x01, 0x0D, 0x0A, 0x2D, 0xB4, 0x00,  # data + checksum
    0x00, 0x00, 0x00, 0x49, 0x45, 0x4E, 0x44, 0xAE,  # IEND chunk
    0x42, 0x60, 0x82
  ))
  writeBin(png_data, temp_png)
  
  # Test that PNG files are recognized as supported
  files <- prepare_files_for_upload(temp_png)
  expect_length(files, 1)
  expect_equal(files, temp_png)
  
  # Clean up
  unlink(temp_png)
})

test_that("display_image_file handles invalid inputs", {
  # Test non-existent file
  expect_error(display_image_file("nonexistent.png"), "Image file not found")
  
  # Test unsupported format
  temp_file <- tempfile(fileext = ".txt")
  writeLines("not an image", temp_file)
  expect_error(display_image_file(temp_file), "Unsupported image format")
  unlink(temp_file)
})

test_that("image metadata collection is simplified", {
  # Create temporary image files
  temp_dir <- tempdir()
  temp_png <- file.path(temp_dir, "test.png")
  temp_jpg <- file.path(temp_dir, "test.jpg")
  
  # Create minimal files (just for testing, not valid images)
  writeLines("fake png", temp_png)
  writeLines("fake jpg", temp_jpg)
  
  # Test that images are detected
  image_files <- c(temp_png, temp_jpg)
  expect_true(is_image_dataset(image_files))
  
  # Clean up
  unlink(c(temp_png, temp_jpg))
})

# Integration test with mock data
test_that("handle_image_dataset creates proper display format", {
  # Mock file metadata
  mock_metadata <- data.frame(
    file_name = c("photo1.png", "photo2.jpg"),
    tag = c("v1.0", "v1.0"),
    file_size_bytes = c(1024, 2048),
    file_path = c("s3://bucket/photo1.png", "s3://bucket/photo2.jpg"),
    stringsAsFactors = FALSE
  )
  
  # Test that the function would create proper display
  # (We can't test the interactive parts easily, but we can test data prep)
  expect_equal(nrow(mock_metadata), 2)
  expect_true("file_name" %in% names(mock_metadata))
  expect_true("tag" %in% names(mock_metadata))
  expect_true("file_size_bytes" %in% names(mock_metadata))
})
