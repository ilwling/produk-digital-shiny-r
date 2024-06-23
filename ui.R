# Fungsi dashboardPage() diperuntuhkan untuk membuat ketiga bagian pada Shiny
dashboardPage(skin = "yellow",
              
              # Fungsi dashboardHeader() adalah bagian untuk membuat header
              dashboardHeader(title = 'Analisa Produk Digital'),
              
              # Fungsi dashboardSidebar() adalah bagian untuk membuat sidebar
              dashboardSidebar(
                sidebarMenu(
                  menuItem(
                    text= "Gambaran Umum",
                    tabName = "overview",
                    icon = icon("circle-info")
                  ),
                  menuItem(
                    text= "Tren Transaksi - Jam",
                    tabName = "jam",
                    icon = icon("line-chart")
                  ),
                  menuItem(
                    text= "Tren Transaksi - Tanggal",
                    tabName = "tanggal",
                    icon = icon("line-chart")
                  ),
                  menuItem(
                    text= "Grafik Transaksi",
                    tabName = "transaksi",
                    icon = icon("bar-chart")
                  ),
                  menuItem(
                    text= "Master Data",
                    tabName = "data",
                    icon = icon("table")
                  ),
                  menuItem(
                    text= "Github",
                    href = "https://github.com/ilwling/produk-digital-shiny-r",
                    icon = icon("github")
                  )
                )
              ),
              
              # Fungsi dashboardBody() adalah bagian untuk membuat isi body
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "mydashboardtheme.css")
                ),
                tabItems(
                  #overview page
                  tabItem(
                    tabName = "overview",
                    h2("Gambaran Umum"),
                    p("Dashboard ini berisi beberapa visualisasi yang dapat membantu internal perusahaan di bagian marketing, keuangan, dan DevOps untuk melihat insight penjualan Produk Digital seperti pulsa, token listrik, BPJS, dan lainnya."),
                    p("Data diperoleh dari salah satu aplikasi digital penjualan produk PPOB mulai dari", strong("Januari 2022"), " sampai ", strong("Juni 2024.")),
                   div(class = "link", icon("github"), "Source Code: ", a("Klik di sini", href="https://github.com/ilwling/produk-digital-shiny-r")),
                    fluidRow(
                      box(
                        width = 8,
                        plotlyOutput(outputId = "plot_1")
                      ),
                      valueBox(
                        "Jenis Produk Digital",
                        value=length(unique(ppob_clean$tipetrans)),
                        width=4,
                        color="yellow",
                        icon = icon("shopping-bag")),
                      valueBox(
                        "Total Transaksi",
                        value=comma(length(ppob_clean$tgl_transaksi)),
                        width=4,
                        color="green",
                        icon = icon("database")
                      ),
                      valueBox(
                        "Nominal Transaksi(Rp)",
                        value=comma(sum(ppob_clean$jumlah)),
                        width=4,
                        color="blue",
                        icon = icon("bar-chart")
                      )
                    ),
                   fluidRow(
                     box(
                       width = 4,
                       ggiraphOutput("plot_donut")
                     ),
                     infoBox(
                       title="TRANSAKSI SUKSES",
                       value=comma(length(ppob_sukses$tgl_transaksi)),
                       width=8,
                       color="aqua",
                       icon = icon("check")
                     ),infoBox(
                       title="TRANSAKSI GAGAL",
                       value=comma(length(ppob_gagal$tgl_transaksi)),
                       width=8,
                       color="red",
                       icon = icon("close")
                     ) ,infoBox(
                       title="DIBATALKAN ANGGOTA",
                       value=comma(length(ppob_batal$tgl_transaksi)),
                       width=8,
                       color="maroon",
                       icon = icon("warning")
                     ) 
                   ),
                   div(
                     class = "footer",
                     includeHTML("www/footer.html")
                   )
                  ),
                  
                  #page histo jam
                  tabItem(
                    tabName = "jam",
                    h2("Tren Transaksi - Jam"),
                    fluidRow(
                      box(
                        width = 12,
                        pickerInput(
                          inputId = "param_produk",
                          label = "Jenis Produk Digital", 
                          choices = unique(ppob_clean$tipetrans),
                          multiple = TRUE,
                          selected = "PULSA", 
                          options = list(
                            'actions-box' = TRUE,
                            'deselect-all-text' = "Hapus semua",
                            'select-all-text' = "Pilih semua",
                            'none-selected-text' = "Minimal 1 jenis produk digital"
                          ),
                          choicesOpt = list(
                            content = sprintf("<span class='label label-%s'>%s</span>", 
                                              str_replace(unique(ppob_clean$tipetrans), " ", "-"),
                                              unique(ppob_clean$tipetrans)
                                              ))
                        )
                      ),
                      box(
                        width = 12,
                        plotlyOutput(outputId = "plot_jam")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 12,
                        plotlyOutput(outputId = "plot_jam_all")
                      )
                    ),
                    div(
                      class = "footer",
                      includeHTML("www/footer.html")
                    )
                  ),
                  
                  #page histo tanggal
                  tabItem(
                    tabName = "tanggal",
                    h2("Tren Transaksi - Tanggal"),
                    fluidRow(
                      box(
                        width = 12,
                        pickerInput(
                          inputId = "param_produk2",
                          label = "Jenis Produk Digital", 
                          choices = unique(ppob_clean$tipetrans),
                          multiple = TRUE,
                          selected = "PULSA", 
                          options = list(
                            'actions-box' = TRUE,
                            'deselect-all-text' = "Hapus semua",
                            'select-all-text' = "Pilih semua",
                            'none-selected-text' = "Minimal 1 jenis produk digital"
                          ),
                          choicesOpt = list(
                            content = sprintf("<span class='label label-%s'>%s</span>", 
                                              str_replace(unique(ppob_clean$tipetrans), " ", "-"),
                                              unique(ppob_clean$tipetrans)
                            ))
                        )
                      ),
                      box(
                        width = 12,
                        plotlyOutput(outputId = "plot_tgl")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 12,
                        plotlyOutput(outputId = "plot_tgl_all")
                      )
                    ),
                    div(
                      class = "footer",
                      includeHTML("www/footer.html")
                    )
                  ),
                  
                  #page info transaksi
                  tabItem(
                    tabName = "transaksi",
                    h2("Grafik Transaksi"),
                    fluidRow(
                      box(
                        width = 6,
                        dateRangeInput("daterange", "Rentang Tanggal:",
                                       start  = "2024-06-01",
                                       end    = "2024-06-30",
                                       min    = "2022-01-01",
                                       max    = "2024-06-30",
                                       format = "yyyy-mm-dd",
                                       separator = " sampai ")
                      ),
                      box(
                        width = 6,
                        selectInput(
                          inputId = "status",
                          choices = c("SUKSES", "GAGAL"),
                          label = "Status Transaksi" 
                        )
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        plotlyOutput(outputId = "plot_2")
                      ),
                      box(
                        width = 6,
                        plotlyOutput(outputId = "plot_3")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 12,
                        plotlyOutput(outputId = "plot_4")
                      )
                    ),
                    div(
                      class = "footer",
                      includeHTML("www/footer.html")
                    )
                  ),
                  
                  #page data transaksi
                  tabItem(
                    tabName = "data",
                    h2("Master Data"),
                    fluidRow(
                      box(width = 12, dataTableOutput(outputId = "table_data"))  
                    ),
                    div(
                      class = "footer",
                      includeHTML("www/footer.html")
                    )
                  )
                )
              )
)