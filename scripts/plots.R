# setup -------------------------------------------------------------------
# library(ggplot2)
# library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Paired"    # good for binary groups scale fill/color brewer

# Theme setting (less is more)
theme_set(
  theme_classic()
)
theme_update(
  legend.position = "top"
)

gg <- ggplot(participantes) +
  scale_color_brewer(palette = ff.pal) +
  scale_fill_brewer(palette = ff.pal)

# plots -------------------------------------------------------------------

gg.angulos <- analytical %>%
  pivot_longer(c(slope_em_pe, slope_sentado, tilt, acb, ia, alfa), names_to = "medida", values_to = "angulo") %>%
  transmute(group, angulo, medida = case_when(
    medida == "slope_em_pe" ~ "Slope (em pé)",
    medida == "slope_sentado" ~ "Slope (sentado)",
    medida == "tilt" ~ "Tilt (em pé)",
    medida == "acb" ~ "ACB",
    medida == "ia" ~"IA",
    medida == "alfa" ~ "Alfa",
    medida == "variacao" ~ "Variação",
  )) %>%
  # rename(Grupo = group) %>%
  ggplot(aes(angulo, fill = group)) +
  facet_wrap(~ medida) +
  geom_density(alpha = .9) +
  scale_fill_brewer(palette = ff.pal) +
  xlab("Ângulo") +
  ylab("Densidade") +
  theme(legend.title = element_blank())

# cool facet trick from https://stackoverflow.com/questions/3695497 by JWilliman
# gg +
#   geom_histogram(bins = 5, aes(outcome, y = ..count../tapply(..count.., ..PANEL.., sum)[..PANEL..]), fill = ff.col) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   xlab(attr(analytical$outcome, "label")) +
#   ylab("") +
#   facet_wrap(~ group, ncol = 2)
