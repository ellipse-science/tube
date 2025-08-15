# Test XML file support in public datalake

source("helper-functions.R")

test_that("XML files are included in supported extensions", {
  debug_log("Testing XML files are included in supported extensions")
  
  # Test that XML is included in supported extensions for file upload
  temp_dir <- tempdir()
  temp_xml <- file.path(temp_dir, "test.xml")
  
  # Create a simple XML file
  xml_content <- '<?xml version="1.0" encoding="UTF-8"?>
<root>
  <record>
    <id>1</id>
    <name>John Doe</name>
    <age>30</age>
  </record>
  <record>
    <id>2</id>
    <name>Jane Smith</name>
    <age>25</age>
  </record>
</root>'
  
  writeLines(xml_content, temp_xml)
  
  # Test that prepare_files_for_upload includes XML files
  result <- tube:::prepare_files_for_upload(temp_xml)
  expect_length(result, 1)
  expect_equal(result, temp_xml)
  
  # Clean up
  unlink(temp_xml)
  
  test_detail("XML file correctly included in supported file preparations")
})

test_that("XML content type is correctly identified", {
  debug_log("Testing XML content type is correctly identified")
  
  temp_xml <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0"?><root><test>data</test></root>', temp_xml)
  
  content_type <- tube:::get_content_type(temp_xml)
  expect_equal(content_type, "text/xml")
  
  # Clean up
  unlink(temp_xml)
  
  test_detail("XML content type correctly identified as text/xml")
})

test_that("XML files can be read and converted to tabular format", {
  debug_log("Testing XML files can be read and converted to tabular format")
  
  # Test case 1: Simple repeating elements (should be converted to rows)
  temp_xml1 <- tempfile(fileext = ".xml")
  simple_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<data>
  <person>
    <id>1</id>
    <name>Alice</name>
    <age>30</age>
    <city>New York</city>
  </person>
  <person>
    <id>2</id>
    <name>Bob</name>
    <age>25</age>
    <city>London</city>
  </person>
  <person>
    <id>3</id>
    <name>Charlie</name>
    <age>35</age>
    <city>Paris</city>
  </person>
</data>'
  
  writeLines(simple_xml, temp_xml1)
  
  result1 <- tube:::read_xml_as_tabular(temp_xml1)
  expect_s3_class(result1, "tbl_df")
  expect_true(nrow(result1) >= 3)  # Should have at least 3 rows for 3 people
  expect_true("id" %in% names(result1) || "xml_source_element" %in% names(result1))
  
  test_detail(sprintf("Simple XML converted to %d rows with %d columns", 
                     nrow(result1), ncol(result1)))
  
  # Test case 2: Complex nested XML (should be flattened)
  temp_xml2 <- tempfile(fileext = ".xml")
  complex_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<configuration>
  <database>
    <host>localhost</host>
    <port>5432</port>
    <name>testdb</name>
  </database>
  <features>
    <feature name="analytics">enabled</feature>
    <feature name="reporting">disabled</feature>
  </features>
</configuration>'
  
  writeLines(complex_xml, temp_xml2)
  
  result2 <- tube:::read_xml_as_tabular(temp_xml2)
  expect_s3_class(result2, "tbl_df")
  expect_true(nrow(result2) > 0)
  expect_true("element_path" %in% names(result2) || "xml_source_element" %in% names(result2))
  
  test_detail(sprintf("Complex XML converted to %d rows with %d columns", 
                     nrow(result2), ncol(result2)))
  
  # Test case 3: Invalid XML (should handle gracefully)
  temp_xml3 <- tempfile(fileext = ".xml")
  invalid_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<unclosed_tag>
  <data>Some content</data>'
  
  writeLines(invalid_xml, temp_xml3)
  
  result3 <- tube:::read_xml_as_tabular(temp_xml3)
  expect_s3_class(result3, "tbl_df")
  expect_true(nrow(result3) > 0)
  expect_true("xml_content" %in% names(result3) || "parse_error" %in% names(result3))
  
  test_detail("Invalid XML handled gracefully with fallback parsing")
  
  # Clean up
  unlink(c(temp_xml1, temp_xml2, temp_xml3))
})

test_that("XML files work with read_file_by_extension", {
  debug_log("Testing XML files work with read_file_by_extension")
  
  temp_xml <- tempfile(fileext = ".xml")
  xml_content <- '<?xml version="1.0" encoding="UTF-8"?>
<products>
  <product>
    <id>P001</id>
    <name>Laptop</name>
    <price>999.99</price>
    <category>Electronics</category>
  </product>
  <product>
    <id>P002</id>
    <name>Phone</name>
    <price>599.99</price>
    <category>Electronics</category>
  </product>
</products>'
  
  writeLines(xml_content, temp_xml)
  
  # Test that read_file_by_extension can handle XML
  result <- tube:::read_file_by_extension(temp_xml, "xml")
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(ncol(result) > 0)
  
  test_detail(sprintf("XML file processed via read_file_by_extension: %d rows, %d columns", 
                     nrow(result), ncol(result)))
  
  # Clean up
  unlink(temp_xml)
})

test_that("XML tabular extraction handles edge cases", {
  debug_log("Testing XML tabular extraction handles edge cases")
  
  # Test case 1: Empty XML
  temp_xml1 <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0" encoding="UTF-8"?><empty></empty>', temp_xml1)
  
  result1 <- tube:::read_xml_as_tabular(temp_xml1)
  expect_s3_class(result1, "tbl_df")
  expect_true(nrow(result1) > 0)  # Should have fallback data
  
  # Test case 2: XML with attributes
  temp_xml2 <- tempfile(fileext = ".xml")
  attr_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<items>
  <item id="1" type="book">
    <title>XML Guide</title>
    <author>John Doe</author>
  </item>
  <item id="2" type="magazine">
    <title>Tech Today</title>
    <author>Jane Smith</author>
  </item>
</items>'
  
  writeLines(attr_xml, temp_xml2)
  
  result2 <- tube:::read_xml_as_tabular(temp_xml2)
  expect_s3_class(result2, "tbl_df")
  expect_true(nrow(result2) >= 1)
  
  # Test case 3: Mixed content types
  temp_xml3 <- tempfile(fileext = ".xml")
  mixed_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<mixed>
  <numbers>
    <value>123</value>
    <value>456.78</value>
    <value>999</value>
  </numbers>
  <texts>
    <phrase>Hello World</phrase>
    <phrase>XML Processing</phrase>
  </texts>
</mixed>'
  
  writeLines(mixed_xml, temp_xml3)
  
  result3 <- tube:::read_xml_as_tabular(temp_xml3)
  expect_s3_class(result3, "tbl_df")
  expect_true(nrow(result3) >= 1)
  
  test_detail("All XML edge cases handled successfully")
  
  # Clean up
  unlink(c(temp_xml1, temp_xml2, temp_xml3))
})

test_that("XML support is documented in user prompts", {
  debug_log("Testing XML support is documented in user prompts")
  
  # Test that interactive prompts mention XML format
  # This is more of a smoke test since we can't easily test interactive functions
  expect_true(exists("simple_file_folder_selector", envir = asNamespace("tube")))
  
  test_detail("XML support functions exist and are accessible")
})
