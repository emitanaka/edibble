test_that("agridat examples", {
  skip("skip")

  expect_snapshot({
    initiate_design("Archbold et al (1987) agridat::archbold.apple: Split-split plot") %>%
      set_units(row = 2:16,
                pos = 1:17,
                mp = group(row:pos,
                           pos %in% 1:8 ~ paste0(row, "A"),
                           pos %in% 9:17 ~ paste0(row, "B")))
  })

  expect_snapshot({
    initiate_design("Beall (1940) agridat::beall.webworms") %>%
      set_units(col = 1:20,
                row = 1:65,
                plot = ~col:row,
                block = cut_unit(row, 5),
                mainplot = cut_unit(col, 5),
                wholeplot = cut_unit(col, 10)) %>%
      set_trts(lead = c("N", "Y"),
               spray = c("N", "Y")) %>%
      apply_trts(~mainplot) %>%
      # it might not be split-plot design
      restrict_mapping(lead ~ wholeplot,
                       spray ~ mainplot)
  })


  expect_snapshot({
    initiate_design("Frederick (1926) agridat::beaven.barley") %>%
      set_units(row = 1:5,
                col = 1:32,
                plot = ~col:row) %>%
      set_trts(gen = letters[1:8]) %>%
      apply_trts(gen ~ plot) %>%
      randomise_trts()
  })

  expect_snapshot({
    initiate_design("Davison (2003) agridat::besag.bayesian") %>%
      set_units(col = 1:3,
                row = 1:75,
                plot = ~col:row) %>%
      set_trts(gen = sprintf("G%.2d", 1:75)) %>%
      apply_trts(~plot) %>%
      restrict_mapping(plot = nested_in(col)) %>%
      randomise_trts()
  })

  expect_snapshot({
    initiate_design("Besag & Kempton (1986) agridat::besag.beans") %>%
      set_units(col = 1:38,
                row = 1:4,
                block = cut_unit(col, 6), # 2-7, 8-13, ... were blocks
                plot = ~col:row) %>%
      set_trts(gen = c("Dwarf", "Maris", "Metissa", "Minica", "Stella", "Topless")) %>%
      apply_trts(~plot)
    # col==1 & col==38 are border blocks so same variety applied
    # each variety occurred once as a left-side neighbor and once as a right-side neighbour of every variety (including itself)

  })

  expect_snapshot({
    initiate_design("Besag & Higdon (1999) agridat::besag.elbatan") %>%
      set_units(block = 3,
                plot = nested_in(block, 50)) %>%
      set_trts(gen = sprintf("G%.2d", 1:50)) %>%
      apply_trts( ~plot) %>%
      randomise_trt()
  })

  expect_snapshot({
    initiate_design("Besag & Higdon (1999) agridat::besag.met - incomplete block") %>%
      set_units(county = paste0("C", 1:6),
                row = nested_in(county, 1:18),
                col = nested_in(county, 1:11),
                plot = ~col:row) %>%
      set_trts(gen = sprintf("G%.2d", 1:64)) %>%
      apply_trts(~ plot) %>%
      set_response(yield = plot)
    # blocks 1: row 1-6 (except 2)...
    # rep ...
  })

  expect_snapshot({
    initiate_design("Besag & Kempton (1986) agridat::besag.triticale - factorial design") %>%
      set_units(row = 1:3,
                col = 1:18,
                plot = ~row:col) %>%
      # 36 treatmens in total / 1 or 2 replicate
      set_trts(rate = c(125, 150),
               nitro = c(100, 150),
               regulator = c("Control", "Cycocel", "Terpal"),
               gen = c("Newton", "Torrs", "Warren")) %>%
      apply_trts(~ plot)
  })


  expect_snapshot({
    initiate_design("Brandt (1938) agridat::brandt.switchback") %>%
      set_units(group = c("A", "B"),
                cow = nested_in(group,
                                "A" ~ c("C493", "C647", "C596", "C560", "C319"),
                                "B" ~ c("C634", "C592", "C409", "C480", "C485")),
                period = nested_in(cow, c("P1", "P2", "P3"))) %>%
      set_trts(trt = c("T1", "T2")) %>%
      apply_trts(~ cow:period)
  })

  expect_snapshot({
    initiate_design("Bridges (1989) agridat::bridges.cucumber - 2 x LSD") %>%
      set_units(loc = c("Clemson", "Tifton"),
                row = nested_in(loc, 1:4),
                col = nested_in(loc, 1:4),
                plot = ~row:col) %>%
      set_trts(gen = c("Dasher", "Guardian", "Poinsett", "Sprint")) %>%
      apply_trts(~plot)
  })

  expect_snapshot({
    initiate_design("Burgueno et al (2000) agridat::burgueno.alpha - Incomplete block alpha design") %>%
      set_units(row = 1:6,
                col = 1:8,
                block = ~row:cut_units(col, 4),
                plot = ~row:col) %>%
      set_trts(gen = sprintf("G%.2d", 1:16))
  })

  expect_snapshot({
    initiate_design("Burgueno et al (2000) agridat::burgueno.rowcol - Row-column design") %>%
      set_units(row = 1:8,
                col = 1:16,
                block = cut_unit(row, 4),
                plot = ~row:col
      ) %>%
      set_trts(gen = sprintf("G%.2d", 1:64)) %>%
      apply_trts(~plot)
  })

  expect_snapshot({
    initiate_design("Burgueno et al (2000) agridat::burgueno.unreplicated - Unreplicated design") %>%
      set_units(col = 1:31,
                row = 1:14,
                plot = ~col:row) %>%
      set_trt(gen = sprintf("G%.3d", 0:290)) %>%
      set_rep(gen == "G000" ~ 154,
              TRUE ~ 1)
  })

  expect_snapshot({
    initiate_design("Schabenberger & Pierce (2002) agridat::byers.applen") %>%
      set_units(tree = sprintf("T%.2d", 1:10),
                apple = nested_in(tree, 25)) %>%
      measure_response(diameter = apple)
  })

  expect_snapshot({
    initiate_design("Andrews & Herzberg (1985) agridat::caribbean.maize") %>%
      set_units(island = c("Antigua", "StVincent"),
                site = nested_in(island,
                                 "Antigua" ~ c("DBAN", "LFAN", "NSAN", "ORAN", "OVAN", "TEAN", "WEAN", "WLAN"),
                                 "StVincent" ~ c("AGSV", "CASV", "CPSV", "LPSV", "MPSV", "OOSV", "OTSV", "SSSV", "UISV")),
                block = nested_in(site, 4),
                plot = nested_in(block, 9)) %>%
      set_trts(nitrogen = c("N0", "N1", "N2", "N3"),
               phosphorus = c("P0", "P1", "P2", "P3"),
               potassium = c("K0", "K1", "K2", "K3")
      ) %>%
      apply_trts( ~ plot) # not all replicate present
  })

  expect_snapshot({
    initiate_design("Chinloy et al. (1953) agridat::chinloy.fractionalfactorial - sugar cane") %>%
      set_units(row = 1:9,
                col = 1:9,
                block = ~cut_unit(row, 3):cut_unit(col, 3),
                plot = ~row:col) %>%
      set_trts(N = c(0, 1, 2),
               P = c(0, 1, 2),
               K = c(0, 1, 2),
               B = c(0, 1, 2),
               F = c(0, 1, 2))

    ggplot(chinloy.fractionalfactorial, aes(row, col, label = trt, fill = block)) +
      geom_tile() + geom_text()

  })

  expect_snapshot({
    initiate_design("Christidis (1935) agridat::christidis.competition") %>%
      set_units(row = 3,
                plot = nested_in(row, 1:90),
                block = cut_unit(plot, 18)) %>%
      set_trts(gen = c("Acala", "Carolina", "Cleveland", "Delfos", "Ingold", "King",
                       "Serres", "Sunshine", "Trice")) %>%
      apply_trts(~plot)

    ggplot(christidis.competition, aes(plotrow, plot, fill = factor(block))) +
      geom_tile(color = "black") + geom_text(aes(label = gen), size = 2)
  })

  expect_snapshot({
    initiate_design("Cochran & Cox (1957) agridat::cochran.crd") %>%
      set_units(row = 1:4,
                col = 1:8,
                plot = ~row:col) %>%
      set_trts(period = c("Spring", "Fall", "Not applicable"),
               amount = nested_in(period,
                                  "Spring" ~ c(300, 600, 900),
                                  "Fall" ~ c(300, 600, 900),
                                  "Not applicable" ~ 0))
    # set_trts(trt = c("F12", "F3", "F6", "O", "S12", "S3", "S6"))
  })

  expect_snapshot({
    initiate_design("Cochran & Cox (1950) agridat::cochran.eelworms") %>%
      set_units(row = 1:5,
                col = nested_in(row,
                                1 ~ 2:7,
                                2 ~ c(2:7, 9:11),
                                . ~ 1:11),
                plot = ~row:col,
                block = group_units(row %in% 1:2 & col %in% 2:7  ~ "B1",
                                    row %in% 3:5 & col %in% 1:4  ~ "B2",
                                    row %in% 3:5 & col %in% 5:8  ~ "B3",
                                    row %in% 2:5 & col %in% 9:11 ~ "B4")) %>%
      set_trts(fumigant = c("Control", "Chlorodinitrobenzen", "Cymag", "Carbon Disulphide jelly", "Seekay"),
               dose = c(0, 1, 2))

    ggplot(cochran.eelworms, aes(factor(row), factor(col), fill = block)) +
      geom_tile(color = 'black') +
      geom_text(aes(label = paste(fumigant, dose)))
  })

  expect_snapshot({
    initiate_design("Cochran & Cox (1957) agridat::cochran.factorial") %>%
      set_units(block = 2,
                plot = nested_in(block, 16)) %>%
      set_trts(dung = c(0, 10),
               nitrochalk = c(0, 0.4),
               superphosphate = c(0, 0.6),
               potash = c(0, 1)) %>%
      apply_trts(~plot)

    ggplot(cochran.factorial, aes(trt, rep, fill = block)) +
      geom_tile(color = "black")
  })

  expect_snapshot({
    initiate_design("Cochran & Cox (1957) agridat::cochran.latin") %>%
      set_units(row = 6,
                col = 6,
                plot = ~row:col) %>%
      set_trts(~plot)

    ggplot(cochran.latin, aes(row, col, fill = operator)) +
      geom_tile(color = "black")
  })



  expect_snapshot({
    initiate_design("Wadley (1946) agridat::cochran.lattice") %>%
      set_units(rep = 5,
                row = nested_in(rep, 4),
                col = nested_in(rep, 4),
                plot = ~row:col) %>%
      set_trts(trt = sprintf("T.%2d", 1:16)) %>%
      apply_trts( ~ plot)

    ggplot(cochran.lattice, aes(row, col, fill = trt)) +
      geom_tile(color = "black") + facet_wrap(~rep)
  })

  expect_snapshot({
    initiate_design("Chinloy et al. (1953) agridat::chinloy.fractionalfactorial - sugar cane") %>%
      set_units()
  })

  expect_snapshot({
    initiate_design("Chinloy et al. (1953) agridat::chinloy.fractionalfactorial - sugar cane") %>%
      set_units()
  })

})
