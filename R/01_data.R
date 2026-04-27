# 01_data.R

library(pxweb)


# Hämta data från SCB API


# Befolkning
befolkning <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningR1860N", 
  query = list(
    Kon = c("1", "2"), 
    Alder = c("*"), 
    ContentsCode = c("0000053A"), 
    Tid = as.character(2000:2024)
  )
)

befolkning_df <- as.data.frame(befolkning,
  column.name.type = "text",
  variable.value.type = "text"
)

# Födda
Födda <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101H/FoddaK", 
  query = list( 
    AlderModer = c("*"),
    Kon = c("*"), 
    Region = c("00"),
    ContentsCode = c("BE0101E2"), 
    Tid = as.character(2000:2024)
  )
)

Födda_df <- as.data.frame(Födda,
  column.name.type = "text",
  variable.value.type = "text"
)

# Döda
Döda <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101I/DodaHandelseK", 
  query = list(
    Kon = c("1", "2"), 
    Alder = c("*"),
    Region = c("00"), 
    ContentsCode = c("BE0101D9"), 
    Tid = as.character(1968:2024)
  )
)

Döda_df <- as.data.frame(Döda,
  column.name.type = "text",
  variable.value.type = "text"
)

# Migration
Migration <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/ImmiEmiFlyttN", 
  query = list(
    Alder = c("*"),
    Kon = c("1", "2"),
    ContentsCode = c("*"),
    UtInFlyttnLand = c("TOT"),
    Fodelselandgrupp = c("SAMT2"),
    Tid = as.character(2000:2024)
  )
)

Migration_df <- as.data.frame(Migration,
  column.name.type = "text",
  variable.value.type = "text"
)
