# Pustaka ----
library(shiny)
library(tidyverse)
library(scales)

# Antarmuka pengguna ----
ui <- fluidPage(
  title = "Mengapa Distribusi-t?",
  navbarPage("Mengapa Distribusi-t?",
         ## Tab selang kepercayaan ----
         tabPanel("Selang Kepercayaan",
          sidebarLayout(
            sidebarPanel(
              ### Statistik sampel ----
              wellPanel(
                sliderInput("ukuran_sampel",
                            "Ukuran sampel:",
                            min = 5,
                            max = 50,
                            value = 10,
                            step = 1),
                sliderInput("banyak_sampel", "Banyak sampel:",
                            min = 20,
                            max = 1000,
                            value = 100,
                            step = 1)
              ),
              ### Konstruksi SK ----
              wellPanel(
                sliderInput("tingkat_kepercayaan",
                            "Tingkat kepercayaan (%):",
                            min = 80,
                            max = 99,
                            value = 95,
                            step = 1),
                checkboxGroupInput("dist_sampling",
                                   "Distribusi sampling:",
                                   choices = c("Distribusi z" =
                                                 "dist_z",
                                               "Distribusi t" =
                                                 "dist_t"),
                                   selected = "dist_z")
              ),
              ### Parameter populasi ----
              wellPanel(
                sliderInput("rerata_pop", "Rerata populasi:",
                            min = 300,
                            max = 700,
                            value = 500),
                sliderInput("sigma_pop",
                            "Simpangan baku populasi:",
                            min = 50,
                            max = 150,
                            value = 100)
              )
            ),
            ### Panel selang kepercayaan ----
            mainPanel(
              tabsetPanel(
              tabPanel("Cakupan Selang Kepercayaan",
              br(),
              conditionalPanel(
                condition = "input.dist_sampling.includes('dist_z')",
                plotOutput("plot_sk_z", height = "300px"),
                textOutput("teks_sk_z")
                ),
              br(),
              conditionalPanel(
                condition = "input.dist_sampling.includes('dist_t')",
                plotOutput("plot_sk_t", height = "300px"),
                textOutput("teks_sk_t")
                )
              ),
              tabPanel("Ringkasan",
                br(),
                plotOutput("plot_selisih_sk_zt",
                             height = "300px"),
                br(),
                plotOutput("plot_wakil_sampel_sk", height = "300px")
              )
              )
              )
            )
          ),
             ## Tab uji hipotesis ----
             tabPanel("Uji Hipotesis",
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            sliderInput("ukuran_sampel_2",
                                        "Ukuran sampel:",
                                        min = 5,
                                        max = 50,
                                        value = 10,
                                        step = 1),
                            sliderInput("banyak_sampel_2", "Banyak sampel:",
                                        min = 20,
                                        max = 1000,
                                        value = 100,
                                        step = 1)
                          ),
                          ### Distribusi sampling ----
                          wellPanel(
                            sliderInput("tingkat_sig",
                                        "Tingkat signifikansi:",
                                        min = 0.01,
                                        max = 0.2,
                                        value = 0.05,
                                        step = 0.01),
                            selectInput("jenis_uji",
                                        "Jenis uji hipotesis:",
                                        choices = c("Dua ekor" = "dua",
                                                    "Ekor kiri" = "kiri",
                                                    "Ekor kanan" = "kanan")),
                            checkboxGroupInput("dist_sampling_2",
                                               "Distribusi sampling:",
                                               choices = c("Distribusi z" =
                                                             "dist_z",
                                                           "Distribusi t" =
                                                             "dist_t"),
                                               selected = "dist_z")
                          ),
                          ### Parameter populasi ----
                          wellPanel(
                            sliderInput("rerata_pop_2", "Rerata populasi:",
                                        min = 300,
                                        max = 700,
                                        value = 500),
                            sliderInput("sigma_pop_2",
                                        "Simpangan baku populasi:",
                                        min = 50,
                                        max = 150,
                                        value = 100)
                          )
                        ),
                        ### Panel utama uji hipotesis ----
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Statistik Uji",
                              br(),
                              conditionalPanel(
                                condition = "input.dist_sampling_2.includes('dist_z')",
                              plotOutput("plot_stat_z", height = "300px"),
                              textOutput("teks_stat_z")
                              ),
                              br(),
                              conditionalPanel(
                                condition = "input.dist_sampling_2.includes('dist_t')",
                              plotOutput("plot_stat_t", height = "300px"),
                              textOutput("teks_stat_t")
                            )
                            ),
                            tabPanel("Ringkasan"
                              
                            )
                          )
                        )
                      )
               
             ),
             ## Tab informasi ----
             tabPanel("Informasi",
                      sidebarLayout(
                        sidebarPanel(
                          
                        ),
                        mainPanel(
                          
                        )
                      )
                      )
             )
)
# Fungsi peladen ----
server <- function(input, output) {
  ## Menghubungkan nilai pada beberapa input ----
  observeEvent(input$ukuran_sampel, {
    updateSliderInput(inputId = "ukuran_sampel_2",
                      value = input$ukuran_sampel)
    })
  observeEvent(input$ukuran_sampel_2,  {
    updateSliderInput(inputId = "ukuran_sampel",
                      value = input$ukuran_sampel_2)
  })
  
  observeEvent(input$banyak_sampel, {
    updateSliderInput(inputId = "banyak_sampel_2",
                      value = input$banyak_sampel)
  })
  observeEvent(input$banyak_sampel_2,  {
    updateSliderInput(inputId = "banyak_sampel",
                      value = input$banyak_sampel_2)
  })
  
  observeEvent(input$tingkat_sig, {
    updateSliderInput(inputId = "tingkat_kepercayaan",
                      value = (1 - input$tingkat_sig) * 100)
  })
  observeEvent(input$tingkat_kepercayaan,  {
    updateSliderInput(inputId = "tingkat_sig",
                      value = 1 - input$tingkat_kepercayaan / 100)
  })
  
  observeEvent(input$rerata_pop, {
    updateSliderInput(inputId = "rerata_pop_2",
                      value = input$rerata_pop)
  })
  observeEvent(input$rerata_pop_2,  {
    updateSliderInput(inputId = "rerata_pop",
                      value = input$rerata_pop_2)
  })
  
  observeEvent(input$sigma_pop, {
    updateSliderInput(inputId = "sigma_pop_2",
                      value = input$sigma_pop)
  })
  observeEvent(input$sigma_pop_2,  {
    updateSliderInput(inputId = "sigma_pop",
                      value = input$sigma_pop_2)
  })
  
  ## Membuat sampel-sampel acak ----
  membuat_set_sampel <- function(k, n, mu, sigma) {
    seed <- as.numeric(Sys.Date())
    set.seed(seed)
    set_sampel <- matrix(round(rnorm(k * n, mean = mu, sd = sigma)),
                         nrow = n)
    tibble(
      id_sampel = rep(1:k, each = n),
      nilai = as.vector(set_sampel)
    )
  }
  
  rep_membuat_set_sampel <- repeatable(membuat_set_sampel)
  
  menghitung_statistik <- function(data, mu, alternatif = "dua",
                                   sig = .05) {
    seed = as.numeric(Sys.Date())
    tingkat_kepercayaan <- 1 - sig
    statistik <- data %>%
      group_by(id_sampel) %>%
      summarize(
        ukuran = n(),
        rerata = mean(nilai),
        sd = sd(nilai),
        se = sd(nilai) / sqrt(n()),
        t_bawah = mean(nilai) - qt(1 - sig / 2,
                                   n() - 1) * (sd(nilai) / sqrt(n())),
        t_atas = mean(nilai) + qt(1 - sig / 2,
                                  n() - 1) * (sd(nilai) / sqrt(n())),
        z_bawah = mean(nilai) - qnorm(1 - sig / 2,
                                      mean = 0,
                                      sd = 1) * (sd(nilai) / sqrt(n())),
        z_atas = mean(nilai) + qnorm(1 - sig / 2,
                                     mean = 0,
                                     sd = 1) * (sd(nilai) / sqrt(n())),
        stat_uji = (mean(nilai) - mu) / (sd(nilai) / sqrt(n()))
      )
    # Menambahkan variabel apakah selang kepercayaan mencakup mu
    statistik <- mutate(statistik,
                        t_mencakup = t_bawah <= mu & t_atas >= mu,
                        z_mencakup = z_bawah <= mu & z_atas >= mu
    )
    
    # Melakukan uji hipotesis untuk menentukan nilai p
    if (alternatif == "dua") {
      statistik <- mutate(statistik,
                          t_p = 2 * pt(-abs(stat_uji),
                                       df = ukuran - 1),
                          z_p = 2 * pnorm(-abs(stat_uji),
                                          mean = 0, sd = 1)
                          )
    } else if (alternatif == "kiri") {
      statistik <- mutate(statistik,
                          t_p = pt(stat_uji, df = ukuran - 1),
                          z_p = pnorm(stat_uji,
                                      mean = 0, sd = 1)
                          )
    } else if (alternatif == "kanan") {
      statistik <- mutate(statistik,
                          t_p = 1 - pt(stat_uji, df = ukuran - 1),
                          z_p = 1 - pnorm(stat_uji, mean = 0, sd = 1)
                          )
    }
    
    # Menentukan apakah uji hipotesisnya signifikan
    statistik <- mutate(statistik,
                        t_sig = t_p <= sig,
                        z_sig = z_p <= sig
                        )
    return(statistik)
  }
  
  komposisi_sampel_stat <- function(k, n, mu, sigma,
                                    alternatif = "dua", sig = .05) {
    set_sampel <- rep_membuat_set_sampel(k, n, mu, sigma)
    data_stat <- menghitung_statistik(set_sampel, mu, alternatif, sig)
    return(data_stat)
  }
  
  stat_set_sampel <- reactive({
    komposisi_sampel_stat(
      input$banyak_sampel, input$ukuran_sampel, input$rerata_pop,
      input$sigma_pop, alternatif = input$jenis_uji, sig = input$tingkat_sig
    )
  })
  
  ## Plot SK distribusi z ----
    output$plot_sk_z <- renderPlot({
      data_stat <- stat_set_sampel()
      k <- input$banyak_sampel
      alpha_sk <- function(x) {
        1 / 1372000 * (x - 1000)^2 + 3 / 10
      }
    ggplot() +
      geom_segment(data = data_stat,
                   aes(x = id_sampel, xend = id_sampel,
                       y = z_bawah, yend = z_atas,
                       color = factor(z_mencakup)),
                   linewidth = 1,
                   alpha = alpha_sk(k)) +
      geom_point(data = data_stat,
                 aes(x = id_sampel, y = rerata,
                     color = factor(z_mencakup))) +
      geom_hline(yintercept = input$rerata_pop, linetype = "dashed",
                 linewidth = 1, color = "black") +
      theme_bw(base_size = 14) +
      scale_color_manual(name = "Mencakup mu?",
                         values = c("FALSE" = "#d95f02",
                                    "TRUE" = "#1b9e77"),
                         labels = c("FALSE" = "Tidak",
                                    "TRUE" = "Ya")) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(title = "Penggunaan Distribusi-z",
           y = "Nilai")
  })
  ## Teks distribusi z ----
  output$teks_sk_z <- renderText({
    data_stat <- stat_set_sampel()
    if ("dist_t" %in% input$dist_sampling) {
      fig_num <- "1.a"
    } else {
      fig_num <- "1"
    }
    tingkat_keper <- input$tingkat_kepercayaan
    k <- input$banyak_sampel
    persen_mencakup <- round(mean(data_stat$z_mencakup) * 100, 2)
    
    teks <- paste("Gambar ", fig_num, ": Selang-selang kepercayaan dari ", k, " sampel yang dikonstruksi dengan tingkat kepercayaan ",tingkat_keper, "% dan menggunakan distribusi sampling statistik z. Terdapat ", persen_mencakup, "% selang kepercayaan yang memuat rerata populasinya.", sep = "")
  })
  ## Plot SK distribusi t ----
  output$plot_sk_t <- renderPlot({
    data_stat <- stat_set_sampel()
    k <- input$banyak_sampel
    alpha_sk <- function(x) {
      1 / 1372000 * (x - 1000)^2 + 3 / 10
    }
    ggplot() +
      geom_segment(data = data_stat,
                   aes(x = id_sampel, xend = id_sampel,
                       y = t_bawah, yend = t_atas,
                       color = factor(t_mencakup)),
                   linewidth = 1,
                   alpha = alpha_sk(k)) +
      geom_point(data = data_stat,
                 aes(x = id_sampel, y = rerata,
                     color = factor(t_mencakup))) +
      geom_hline(yintercept = input$rerata_pop, linetype = "dashed",
                 linewidth = 1, color = "black") +
      theme_bw(base_size = 14) +
      scale_color_manual(name = "Mencakup mu?",
                         values = c("FALSE" = "#d95f02",
                                    "TRUE" = "#1b9e77"),
                         labels = c("FALSE" = "Tidak",
                                    "TRUE" = "Ya")) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      labs(title = "Penggunaan Distribusi-t",
           y = "Nilai")
  })
  ## Teks distribusi t ----
  output$teks_sk_t <- renderText({
    data_stat <- stat_set_sampel()
    if ("dist_z" %in% input$dist_sampling) {
      fig_num <- "1.b"
    } else {
      fig_num <- "1"
    }
    tingkat_keper <- input$tingkat_kepercayaan
    k <- input$banyak_sampel
    persen_mencakup <- round(mean(data_stat$t_mencakup) * 100, 2)
    
    paste("Gambar ", fig_num, ": Selang-selang kepercayaan dari ", k, " sampel yang dikonstruksi dengan tingkat kepercayaan ",tingkat_keper, "% dan menggunakan distribusi-t. Terdapat ", persen_mencakup, "% selang kepercayaan yang memuat rerata populasinya.", sep = "")
  })
  
  ## Plot statistik uji z ----
  output$plot_stat_z <- renderPlot({
    data_stat <- stat_set_sampel()
    k <- input$banyak_sampel
    alpha_stat_uji <- function(x) {
      1 / 1372000 * (x - 1000)^2 + 3 / 10
    }
    sig <- input$tingkat_sig
    fnorm <- function(x) {
      dnorm(x, mean = 0, sd = 1)
    }
    if (input$jenis_uji == "dua") {
      z_kritis <- qnorm(sig / 2, mean = 0, sd = 1,
                        lower.tail = FALSE)
      ggplot() +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "area", xlim = c(-4, -z_kritis),
                      fill = "red", alpha = .3) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "area", xlim = c(z_kritis, 4),
                      fill = "red", alpha = .3) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "area", xlim = c(-z_kritis, z_kritis),
                      fill = "grey", alpha = .3) +
        geom_segment(aes(x = -z_kritis, xend = -z_kritis,
                         y = 0, yend = fnorm(-z_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(aes(x = z_kritis, xend = z_kritis,
                         y = 0, yend = fnorm(z_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(data = data_stat,
                     aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf, col = z_sig),
                     alpha = alpha_stat_uji(k),
                     linewidth = 1) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "line", linewidth = 1.5) +
        xlim(-4, 4) +
        ylim(0, .5) +
        scale_color_manual(values = c("FALSE" = "#1b9e77",
                                      "TRUE" = "#d95f02"),
                           name = "Menolak H_0",
                           labels = c("FALSE" = "Tidak",
                                      "TRUE" = "Ya")) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Uji Hipotesis dengan Distribusi-z",
             x = "z")
    } else if (input$jenis_uji == "kiri") {
      z_kritis <- qnorm(sig, mean = 0, sd = 1,
                        lower.tail = TRUE)
      ggplot() +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "area", xlim = c(-4, z_kritis),
                      fill = "red", alpha = .3) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "area", xlim = c(z_kritis, -z_kritis),
                      fill = "grey", alpha = .3) +
        geom_segment(aes(x = z_kritis, xend = z_kritis,
                         y = 0, yend = fnorm(z_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(data = data_stat,
                     aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf, col = z_sig),
                     alpha = alpha_stat_uji(k),
                     linewidth = 1) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "line", linewidth = 1.5) +
        xlim(-4, 4) +
        ylim(0, .5) +
        scale_color_manual(values = c("FALSE" = "#1b9e77",
                                      "TRUE" = "#d95f02"),
                           name = "Menolak H_0",
                           labels = c("FALSE" = "Tidak",
                                      "TRUE" = "Ya")) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Uji Hipotesis dengan Distribusi-z",
             x = "z")
    } else if (input$jenis_uji == "kanan") {
      z_kritis <- qnorm(sig, mean = 0, sd = 1,
                        lower.tail = FALSE)
      ggplot() +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "area", xlim = c(z_kritis, 4),
                      fill = "red", alpha = .3) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "area", xlim = c(-z_kritis, z_kritis),
                      fill = "grey", alpha = .3) +
        geom_segment(aes(x = z_kritis, xend = z_kritis,
                         y = 0, yend = fnorm(z_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(data = data_stat,
                     aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf, col = z_sig),
                     alpha = alpha_stat_uji(k),
                     linewidth = 1) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                      geom = "line", linewidth = 1.5) +
        xlim(-4, 4) +
        ylim(0, .5) +
        scale_color_manual(values = c("FALSE" = "#1b9e77",
                                      "TRUE" = "#d95f02"),
                           name = "Menolak H_0",
                           labels = c("FALSE" = "Tidak",
                                      "TRUE" = "Ya")) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Uji Hipotesis dengan Distribusi-z",
             x = "z")
    }
  })
  
  ## Teks statistik uji z ----
  output$teks_stat_z <- renderText({
    data_stat <- stat_set_sampel()
    if ("dist_t" %in% input$dist_sampling_2) {
      fig_num <- "3.a"
    } else {
      fig_num <- "3"
    }
    sig <- input$tingkat_sig
    k <- input$banyak_sampel
    persen_menolak <- round(mean(data_stat$z_sig) * 100, 2)
    
    teks <- paste("Gambar ", fig_num, ": Statistik-statistik uji dari ", k, " sampel yang dibandingkan dengan model matematis kurva-z. Terdapat ", persen_menolak, "% sampel yang menolak hipotesis nol meskipun hipotesis nol tersebut benar.", sep = "")
  })
  
  ## Plot statistik uji t ----
  output$plot_stat_t <- renderPlot({
    data_stat <- stat_set_sampel()
    k <- input$banyak_sampel
    alpha_stat_uji <- function(x) {
      1 / 1372000 * (x - 1000)^2 + 3 / 10
    }
    sig <- input$tingkat_sig
    n <- input$ukuran_sampel
    ft <- function(x) {
      dt(x, df = n - 1)
    }
    if (input$jenis_uji == "dua") {
      t_kritis <- qt(sig / 2, df = n - 1,
                        lower.tail = FALSE)
      ggplot() +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "area", xlim = c(-4, -t_kritis),
                      fill = "red", alpha = .3) +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "area", xlim = c(t_kritis, 4),
                      fill = "red", alpha = .3) +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "area", xlim = c(-t_kritis, t_kritis),
                      fill = "grey", alpha = .3) +
        geom_segment(aes(x = -t_kritis, xend = -t_kritis,
                         y = 0, yend = ft(-t_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(aes(x = t_kritis, xend = t_kritis,
                         y = 0, yend = ft(t_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(data = data_stat,
                     aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf, col = t_sig),
                     alpha = alpha_stat_uji(k),
                     linewidth = 1) +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "line", linewidth = 1.5) +
        xlim(-4, 4) +
        ylim(0, .5) +
        scale_color_manual(values = c("FALSE" = "#1b9e77",
                                      "TRUE" = "#d95f02"),
                           name = "Menolak H_0",
                           labels = c("FALSE" = "Tidak",
                                      "TRUE" = "Ya")) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Uji Hipotesis dengan Distribusi-t",
             x = "t")
    } else if (input$jenis_uji == "kiri") {
      t_kritis <- qt(sig, df = n - 1,
                        lower.tail = TRUE)
      ggplot() +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "area", xlim = c(-4, t_kritis),
                      fill = "red", alpha = .3) +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "area", xlim = c(t_kritis, -t_kritis),
                      fill = "grey", alpha = .3) +
        geom_segment(aes(x = t_kritis, xend = t_kritis,
                         y = 0, yend = ft(t_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(data = data_stat,
                     aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf, col = t_sig),
                     alpha = alpha_stat_uji(k),
                     linewidth = 1) +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "line", linewidth = 1.5) +
        xlim(-4, 4) +
        ylim(0, .5) +
        scale_color_manual(values = c("FALSE" = "#1b9e77",
                                      "TRUE" = "#d95f02"),
                           name = "Menolak H_0",
                           labels = c("FALSE" = "Tidak",
                                      "TRUE" = "Ya")) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Uji Hipotesis dengan Distribusi-t",
             x = "t")
    } else if (input$jenis_uji == "kanan") {
      t_kritis <- qt(sig, df = n - 1,
                        lower.tail = FALSE)
      ggplot() +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "area", xlim = c(t_kritis, 4),
                      fill = "red", alpha = .3) +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "area", xlim = c(-t_kritis, t_kritis),
                      fill = "grey", alpha = .3) +
        geom_segment(aes(x = t_kritis, xend = t_kritis,
                         y = 0, yend = ft(t_kritis)),
                     col = "red", linewidth = 1) +
        geom_segment(data = data_stat,
                     aes(x = stat_uji, xend = stat_uji,
                         y = 0, yend = Inf, col = t_sig),
                     alpha = alpha_stat_uji(k),
                     linewidth = 1) +
        stat_function(fun = dt, args = list(df = n - 1),
                      geom = "line", linewidth = 1.5) +
        xlim(-4, 4) +
        ylim(0, .5) +
      scale_color_manual(values = c("FALSE" = "#1b9e77",
                                    "TRUE" = "#d95f02"),
                         name = "Menolak H_0",
                         labels = c("FALSE" = "Tidak",
                                    "TRUE" = "Ya")) +
        theme_bw(base_size = 14) +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(face = "bold")) +
        labs(title = "Uji Hipotesis dengan Distribusi-t",
             x = "t")
    }
  })
  
  ## Teks statistik uji t ----
  output$teks_stat_t <- renderText({
    data_stat <- stat_set_sampel()
    if ("dist_z" %in% input$dist_sampling_2) {
      fig_num <- "3.b"
    } else {
      fig_num <- "3"
    }
    sig <- input$tingkat_sig
    k <- input$banyak_sampel
    persen_menolak <- round(mean(data_stat$t_sig) * 100, 2)
    
    teks <- paste("Gambar ", fig_num, ": Statistik-statistik uji dari ", k, " sampel yang dibandingkan dengan model matematis distribusi-t. Terdapat ", persen_menolak, "% sampel yang menolak hipotesis nol meskipun hipotesis nol tersebut benar.", sep = "")
  })
  
  ## Plot selisih SK ----
  output$plot_selisih_sk_zt <- renderPlot({
    data_stat <- stat_set_sampel()
    data_stat <- data_stat %>% 
      select(id_sampel, z_mencakup, t_mencakup) %>% 
      summarise(z = mean(z_mencakup), t = mean(t_mencakup)) %>% 
      pivot_longer(cols = c(z, t), names_to = "dist_sampling", 
                   values_to = "prop_mencakup") %>% 
      mutate(persen_mencakup = paste0(round(prop_mencakup * 100, 2),
                                     "%"))
    
    tingkat_keper <- 1 - input$tingkat_sig
    x_maks <- max(data_stat$prop_mencakup, tingkat_keper)
    x_min <- min(data_stat$prop_mencakup, tingkat_keper)
    x_range <- x_maks - x_min
    plot_range <- c(x_min - x_range / 4,
                    x_maks + x_range / 4)
    ggplot(data_stat) +
      geom_segment(aes(x = prop_mencakup, xend = tingkat_keper,
                       y = dist_sampling, yend = dist_sampling,
                       col = dist_sampling),
                   show.legend = FALSE, linewidth = 5,
                   alpha = .6) +
      geom_segment(aes(x = tingkat_keper, xend = tingkat_keper,
                       y = 0, yend = Inf),
                   linewidth = 3, alpha = .6) +
      geom_point(aes(x = prop_mencakup,
                     y = dist_sampling,
                     col = dist_sampling),
                 size = 8) +
      geom_label(aes(x = prop_mencakup, y = dist_sampling,
                    label = persen_mencakup, col = dist_sampling),
                fill = "white", fontface = "bold",
                nudge_y = .175, label.r = unit(0.1, "lines"),
                show.legend = FALSE) +
      scale_x_continuous(labels = label_percent(scale = 100),
                         limits = plot_range) +
      scale_color_brewer(palette = "Dark2", name = "Distribusi sampling") +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      labs(x = "Persentase")
  })
  
  ## Plot perwakilan sampel ----
  output$plot_wakil_sampel_sk <- renderPlot({
    rerata_pop <- input$rerata_pop
    data_sampel <- rep_membuat_set_sampel(input$banyak_sampel,
                                      input$ukuran_sampel,
                                      input$rerata_pop,
                                      input$sigma_pop)
    data_stat <- stat_set_sampel()
    data_stat <- data_stat %>% 
      mutate(mencakup = ifelse(z_mencakup == FALSE & t_mencakup == FALSE,
                               0, ifelse(z_mencakup == FALSE & t_mencakup == TRUE, 
                                         1, 2))) %>% 
      group_by(mencakup) %>% 
      slice_sample(n = 5, replace = FALSE)
    data_stat_simpel <- data_stat %>% 
      select(id_sampel, rerata, se, mencakup)
    
    no_sampel <- data_stat$id_sampel
    
    data_sampel_wakil <- data_sampel %>% 
      filter(id_sampel %in% no_sampel)
    data_sampel_wakil <- left_join(data_sampel_wakil, data_stat_simpel,
                                   by = "id_sampel")
    data_sampel_wakil %>% 
      ggplot(aes(x = fct_reorder(factor(id_sampel), mencakup),
                 y = nilai, color = factor(mencakup))) +
      geom_violin(fill = "whitesmoke", linewidth = .75) +
      geom_point(size = 3, alpha = .6) +
      geom_crossbar(stat = "summary", color = "black",
                    width = .5, fatten = 3) +
      geom_hline(yintercept = rerata_pop,
                 linewidth = 1, linetype = "dashed") +
      theme_bw(base_size = 14) +
      scale_color_brewer(palette = "Dark2") +
      theme(legend.position = "bottom") +
      labs(x = "ID Sampel", y = "Nilai")
  })
  
}
# Objek aplikasi Shiny ----
shinyApp(ui = ui, server = server)