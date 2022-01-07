
# https://db.rstudio.com/best-practices/managing-credentials/

get_nivabase_data

keyring::key_set(service = "NIVABASE", username = "DHJ")
# after this, you are prompted to add your password

# Set global variable
db_username <- "DHJ"


