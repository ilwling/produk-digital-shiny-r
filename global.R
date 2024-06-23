# Mempersiapkan libraries

library(shiny)
library(shinydashboard)

options(scipen = 99) # me-non-aktifkan scientific notation
library(dplyr) # data prep
library(lubridate) # date data prep
library(ggplot2) # visualisasi statis
library(plotly) # plot interaktif
library(glue) # setting tooltip
library(scales) # mengatur skala pada plot
# Import Library Read Table
library(DT) # untuk menampilkan dataset
library(ggiraph)
library(shinyWidgets)
library(stringr)


# Membaca data
ppob <-read.csv("produkdigital.csv")

#sapply tipetrans
ppob$tipetrans <- sapply(X = as.character(ppob$tipetrans),
                         FUN = switch, 
                         "1" = "PULSA",
                         "2" = "TOKEN PLN", 
                         "3" = "BPJS", 
                         "4" = "PLN POSTPAID", 
                         "5" = "PDAM", 
                         "6" = "TELEPON PASCA", 
                         "7" = "EWALLET", 
                         "8" = "MULTIFINANCE")

#sapply status
ppob$status <- sapply(X = as.character(ppob$status),
                      FUN = switch, 
                      "0" = "PENDING",
                      "1" = "SUKSES", 
                      "2" = "BATAL")

# Mengubah tipe data
ppob_mutate <- ppob %>% 
  mutate_at(.vars = c("status", "kode_trans", "tipetrans"), 
            .funs = as.factor) %>% 
  mutate(tgl_transaksi = ymd_hms(tgl_transaksi))

#clean
ppob_clean <-
  ppob_mutate %>%
  select(tgl_transaksi, rek_awal, jumlah, margin, status, bayar, kode_trans, tipetrans, noref)

#tambah trans hour dan trans day
ppob_clean$trans_hour <- hour(ppob_clean$tgl_transaksi)
ppob_clean$trans_day <- day(ppob_clean$tgl_transaksi)
ppob_clean$trans_year <- year(ppob_clean$tgl_transaksi)
ppob_clean$tgl_ymd <- date(ppob_clean$tgl_transaksi)

#sukses
ppob_sukses <-
  ppob_clean %>%
  filter(status %in% "SUKSES") 
#gagal
ppob_gagal <-
  ppob_clean %>%
  filter(status %in% "BATAL") %>%
  filter(bayar %in% "1") 
#batal
ppob_batal <-
  ppob_clean %>%
  filter(status %in% "BATAL") %>%
  filter(bayar %in% "0") 
