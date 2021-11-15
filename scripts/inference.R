# setup -------------------------------------------------------------------
# library(tableone)
# library(gt)
# library(gtsummary)
# library(infer)

# tables ------------------------------------------------------------------

tab_inf <- analytical %>%
  mutate(group = relevel(group, "Artrose")) %>%
  # select
  select(-id) %>%
  tbl_summary(
    by = group,
    # include = starts_with(c("slope", "tilt")),
    include = -c(sexo, idade, imc, dor, lado),
  ) %>%
  add_difference(
    # test = all_continuous() ~ "cohens_d",
    adj.vars = c(sexo, idade, imc),
  ) %>%
  modify_header(label ~ "**Ângulos**") %>%
  modify_footnote(update = c(estimate, ci, p.value) ~ "ANCOVA (ajustada por sexo, idade e IMC)") %>%
  modify_header(estimate ~ '**Diferença ajustada**') %>%
  bold_p() %>%
  bold_labels()
