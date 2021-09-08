get_nivabase_data <- function (sql = NULL, source = "NIVABASE") 
{
  library(svSocket)
  sock_port <- 8642L
  sock_con <- "sv_con"
  dataframe_temporary <<- data.frame()
  table_out <- "dataframe_temporary"
  pwd <- unserialize(sodium::simple_decrypt(db_pwd, db_privkey))
  startSocketServer(port = sock_port, server.name = "oracle_query_32", 
                    local = TRUE)
  expr <- "library(svSocket)"
  expr <- c(expr, sprintf("con <- DBI::dbConnect(odbc::odbc(), '%3$s', UID='%1$s', PWD='%2$s', encoding = 'ISO-8859-1')", 
                          db_username, pwd, source))
  expr <- c(expr, sprintf("%1$s <- DBI::dbGetQuery(con, \"%2$s\")", 
                          table_out, sql))
  expr <- c(expr, sprintf("%s <- socketConnection(port=%i)", 
                          sock_con, sock_port))
  expr <- c(expr, sprintf("evalServer(%s, %s, %s)", sock_con, 
                          table_out, table_out))
  expr <- c(expr, sprintf("close(%s)", sock_con))
  expr <- paste(expr, collapse = ";")
  prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
  system2(prog, args = c("-e", shQuote(expr)), stdout = NULL, 
          wait = TRUE, invisible = TRUE)
  stopSocketServer(port = sock_port)
  fact2char_df(dataframe_temporary)
}