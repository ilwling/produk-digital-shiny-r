shinyServer(function(input, output) {
  
  #overview
  output$plot_1 <- renderPlotly({
    
    ppob_vol <- ppob_clean %>% 
      filter(status %in% "SUKSES") %>% 
      group_by(tipetrans) %>% 
      summarise(n = n()) %>% 
      arrange(-n) %>% 
      mutate(label = glue("Jenis: {tipetrans} 
                      Volume: {comma(n)}"))
    
    plot1 <- 
      ggplot(data = ppob_vol, mapping = aes(x = n, 
                                            y = reorder(tipetrans, n),
                                            fill = n,
                                            text = label)) +
      geom_col() +
      scale_fill_gradient(low = "darkgreen",
                          high = "black") +
      scale_x_continuous(labels = comma)+
      labs(title = "Total Transaksi Produk Digital berdasarkan Jenis",
           x = "Total Transaksi",
           y = "Produk Digital") + 
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot1, tooltip = 'text')
    
  })
  
  #donutbar
  output$plot_donut <- renderggiraph({
    # Create test data.
    data <- data.frame(
      category=c("SUKSES", "GAGAL", "DIBATALKAN"),
      count=c(length(ppob_sukses$tgl_transaksi), length(ppob_gagal$tgl_transaksi), length(ppob_batal$tgl_transaksi))
    )
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, "\n", data$count, " Transaksi")
    
    # Make the plot
    p <-
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      labs(title = "Ringkasan Status Transaksi") + 
      geom_label( x=4, aes(y=labelPosition, label=label), size=4) +
      scale_fill_brewer(palette=1) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none") +
      theme(plot.title = element_text(size=25))
    
    ggiraph(code = print(p))
  })
  
  #perjam
  output$plot_jam <- renderPlotly({
    
    if(is.null(input$param_produk)){
      showNotification("Grafik tidak dapat ditampilkan! Minimal harus dipilih 1 jenis produk digital", duration = 5,type = c("error"))
      #view only canvas kosong
      plot_jam <-
        ggplot (data = ppob_clean) +
        labs(title = "Jumlah Transaksi Tiap Jenis Produk Digital Berdasarkan Jam Transaksi",
             x = "Jam Transaksi",
             y = "Jumlah Transaksi") +
        theme_minimal()
      
      ggplotly(plot_jam) 
    }else{
      ppob_trend_h_filter <-
        ppob_clean %>%
        filter(tipetrans %in% c(input$param_produk))
      
      ppob_trend_h <- aggregate(noref ~ tipetrans + trans_hour, 
                                data = ppob_trend_h_filter, 
                                FUN = length)
      names(ppob_trend_h) <- c("tipetrans", "trans_hour", "trans_count")
      ppob_trend_h <-
        ppob_trend_h %>%
        mutate(label = glue("{tipetrans}
                      Jam Transaksi: {trans_hour}
                      Transaksi: {trans_count}"))
      
      plot_jam <-
        ggplot (data = ppob_trend_h, mapping = aes (x = trans_hour,
                                                    y= trans_count
        )) +
        geom_line(mapping = aes(group = tipetrans, color= tipetrans), size = 1.2) +
        geom_point(col = "red",mapping = aes(text = label)) +
        labs(title = "Jumlah Transaksi Tiap Jenis Produk Digital Berdasarkan Jam Transaksi",
             x = "Jam Transaksi",
             y = "Jumlah Transaksi",
             color = "Jenis Produk Digital") +
        theme_minimal()
      
      ggplotly(plot_jam, tooltip = 'text') 
    }
  })
  
  #perjam all
  output$plot_jam_all <- renderPlotly({
    
    ppob_trend_h_filter <-
      ppob_clean %>%
      filter(tipetrans %in% c(input$param_produk))
    
    ppob_trend_h <-
      ppob_trend_h_filter %>%
      group_by(trans_hour) %>% 
      summarise(n = n()) %>% 
      mutate(label = glue("Jam Transaksi: {trans_hour}
                      Transaksi: {n}"))
    
    plot_jam <-
      ggplot (data = ppob_trend_h, mapping = aes (x = trans_hour,
                                                  y= n,
                                                  text = label
      )) +
      geom_col(fill='maroon') +
      labs(title = "Kumulatif Produk Digital Terpilih Berdasarkan Jam Transaksi",
           x = "Jam Transaksi",
           y = "Jumlah Transaksi") +
      theme_minimal()
    
    ggplotly(plot_jam, tooltip = 'text')
  })
  
  #pertanggal
  output$plot_tgl <- renderPlotly({
    
    if(is.null(input$param_produk2)){
      showNotification("Grafik tidak dapat ditampilkan! Minimal harus dipilih 1 jenis produk digital", duration = 5,type = c("error"))
      #view only canvas kosong
      plot_tgl <-
        ggplot (data = ppob_clean) +
        labs(title = "Jumlah Transaksi Tiap Jenis Produk Digital Berdasarkan Tanggal",
             x = "Tanggal Transaksi",
             y = "Jumlah Transaksi") +
        theme_minimal()
      
      ggplotly(plot_tgl) 
    }else{
      
      ppob_trend_d_filter <-
        ppob_clean %>%
        filter(tipetrans %in% c(input$param_produk2))
      
      ppob_trend_d <- aggregate(noref ~ tipetrans + trans_day, 
                                data = ppob_trend_d_filter, 
                                FUN = length)
      names(ppob_trend_d) <- c("tipetrans", "trans_day", "trans_count")
      ppob_trend_d <-
        ppob_trend_d %>%
        mutate(label = glue("{tipetrans}
                      Tanggal: {trans_day}
                      Transaksi: {trans_count}"))
      
      plot_tgl <-
        ggplot (data = ppob_trend_d, mapping = aes (x = trans_day,
                                                    y= trans_count
        )) +
        geom_line(mapping = aes(group = tipetrans, color= tipetrans), size = 1.2) +
        geom_point(col = "red",mapping = aes(text = label)) +
        labs(title = "Jumlah Transaksi Tiap Jenis Produk Digital Berdasarkan Tanggal",
             x = "Tanggal Transaksi",
             y = "Jumlah Transaksi",
             color = "Jenis Produk Digital") +
        theme_minimal()
      
      ggplotly(plot_tgl, tooltip = 'text') 
    }
  })
  
  #per tanggal all
  output$plot_tgl_all <- renderPlotly({
    
    if(is.null(input$param_produk2)){
      showNotification("Grafik tidak dapat ditampilkan! Minimal harus dipilih 1 jenis produk digital", duration = 5,type = c("error"))
      #view only canvas kosong
      plot_tgl <-
        ggplot (data = ppob_clean) +
        labs(title = "Kumulatif Produk Digital Terpilih Berdasarkan Tanggal",
             x = "Tanggal Transaksi",
             y = "Jumlah Transaksi") +
        theme_minimal()
      
      ggplotly(plot_tgl) 
    }else{
      ppob_trend_d_filter <-
        ppob_clean %>%
        filter(tipetrans %in% c(input$param_produk2))
      
      ppob_trend_d <- aggregate(noref ~ tipetrans + trans_day, 
                                data = ppob_trend_d_filter, 
                                FUN = length)
      names(ppob_trend_d) <- c("tipetrans", "trans_day", "trans_count")
      ppob_trend_d <-
        ppob_trend_d %>%
        mutate(label = glue("{tipetrans}
                      Tanggal: {trans_day}
                      Transaksi: {trans_count}"))
      
      plot_tgl <-
        ggplot(data = ppob_trend_d, aes(fill = tipetrans,
                                        y = trans_count,
                                        x = trans_day,
                                        text = label
        )) + 
        geom_bar(position="stack", stat="identity") +
        labs(title = "Kumulatif Produk Digital Terpilih Berdasarkan Tanggal",
             x = "Tanggal Transaksi",
             y = "Jumlah Transaksi",
             fill = "Jenis Produk Digital") +
        theme_minimal()
      
      ggplotly(plot_tgl, tooltip = 'text') 
    }
  })
  
  #datatable
  output$table_data <- renderDataTable({
    datatable(data = ppob_clean,
              options = list(scrollX = TRUE)
    )
  })
  
  #transaksi
  output$plot_2 <- renderPlotly({
    
    if(input$status == "SUKSES"){
      ppob_vol <- 
        ppob_clean %>%
        filter(status %in% "SUKSES")
    }else{
      ppob_vol <- 
        ppob_clean %>%
        filter(status %in% "BATAL") %>%
        filter(bayar %in% "1") 
    }
    
    ppob_vol <- ppob_vol %>% 
      filter(tgl_ymd >= input$daterange[1]) %>%
      filter(tgl_ymd <= input$daterange[2]) %>%
      group_by(tipetrans) %>% 
      summarise(n = n()) %>% 
      arrange(-n) %>% 
      mutate(label = glue("Jenis: {tipetrans} 
                      Volume: {comma(n)}"))
    
    plot2 <- 
      ggplot(data = ppob_vol, mapping = aes(x = n, 
                                            y = reorder(tipetrans, n),
                                            fill = n,
                                            text = label)) +
      geom_col() +
      scale_fill_gradient(low = "darkgreen",
                          high = "black") +
      scale_x_continuous(labels = comma)+
      labs(title = glue("Jumlah Transaksi {input$status} Berdasarkan Rentang Tanggal"),
           x = "Total Transaksi",
           y = "") + 
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot2, tooltip = 'text')
    
  })
  
  output$plot_3 <- renderPlotly({
    
    if(input$status == "SUKSES"){
      ppob_nom <- 
        ppob_clean %>%
        filter(status %in% "SUKSES")
    }else{
      ppob_nom <- 
        ppob_clean %>%
        filter(status %in% "BATAL") %>%
        filter(bayar %in% "1") 
    }
    
    ppob_nom <- ppob_nom %>% 
      filter(tgl_ymd >= input$daterange[1]) %>%
      filter(tgl_ymd <= input$daterange[2]) %>%
      group_by(tipetrans) %>% 
      summarise(sum = sum(jumlah)) %>% 
      arrange(-sum) %>% 
      mutate(label = glue("Jenis: {tipetrans} 
                      Nominal: Rp{comma(sum)}"))
    
    plot3 <- 
      ggplot(data = ppob_nom, mapping = aes(x = sum, 
                                            y = reorder(tipetrans, sum),
                                            fill = sum,
                                            text = label)) +
      geom_col() +
      scale_fill_gradient(low = "darkgreen",
                          high = "black") +
      scale_x_continuous(labels = comma)+
      labs(title = glue("Jumlah Nominal(Rp) {input$status} Berdasarkan Rentang Tanggal"),
           x = "Total Nominal(Rp)",
           y = "") + 
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot3, tooltip = 'text')
    
  })
  
  output$plot_4 <- renderPlotly({
    
    if(input$status == "SUKSES"){
      ppob_fee <- 
        ppob_clean %>%
        filter(status %in% "SUKSES")
    }else{
      ppob_fee <- 
        ppob_clean %>%
        filter(status %in% "BATAL") %>%
        filter(bayar %in% "1") 
    }
    
    ppob_fee <- ppob_fee %>% 
      filter(tgl_ymd >= input$daterange[1]) %>%
      filter(tgl_ymd <= input$daterange[2]) %>%
      group_by(tipetrans) %>% 
      summarise(sum = sum(margin)) %>% 
      arrange(-sum) %>% 
      mutate(label = glue("Jenis: {tipetrans} 
                      Nominal: Rp{comma(sum)}"))
    
    plot4 <- 
      ggplot(data = ppob_fee, mapping = aes(x = sum, 
                                            y = reorder(tipetrans, sum),
                                            fill = sum,
                                            text = label)) +
      geom_col() +
      scale_fill_gradient(low = "darkgreen",
                          high = "black") +
      scale_x_continuous(labels = comma)+
      labs(title = glue("Margin Transaksi(Rp) {input$status} Berdasarkan Rentang Tanggal"),
           x = "Total Margin(Rp)",
           y = "") + 
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot4, tooltip = 'text')
    
  })
  
})