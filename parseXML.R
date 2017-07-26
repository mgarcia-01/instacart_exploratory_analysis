library(XML)

fileURL <- "C:/Users/Michael.Garcia/Documents/ETL_SSIS_PROJECT/Customers_local/schema/customer_export_NexxusBizManagr.xml"

XML::readHTMLList(fileURL)
XML::readHTMLTable(fileURL)

a <- xmlParse(file = fileURL)

b <- xmlToList(a)
