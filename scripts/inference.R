# setup -------------------------------------------------------------------
# library(tableone)
# library(gt)
# library(gtsummary)
# library(infer)

# tables ------------------------------------------------------------------

# template p-value table
# tab_inf <- analytical %>%
#   # select
#   select(-id, ) %>%
#   tbl_summary(
#     by = group,
#     include = -c(sexo, idade, imc, dor, tipo, lado),
#   ) %>%
#   # include study N
#   add_overall() %>%
#   # pretty format categorical variables
#   bold_labels() %>%
#   # bring home the bacon!
#   add_p(
#     # use Fisher test (defaults to chi-square)
#     test = all_categorical() ~ "fisher.test",
#     # use 3 digits in pvalue
#     pvalue_fun = function(x) style_pvalue(x, digits = 3)
#   ) %>%
#   # bold significant p values
#   bold_p()

# Template Cohen's D table (obs: does NOT compute p)
tab_inf <- analytical %>%
  # select
  select(-id) %>%
  tbl_summary(
    by = group,
    # include = starts_with(c("slope", "tilt")),
    include = -c(sexo, idade, imc, dor, tipo, lado),
  ) %>%
  add_difference(
    # test = all_continuous() ~ "cohens_d",
    adj.vars = c(sexo, idade, imc),
  ) %>%
  # modify_footnote(update = c(estimate, ci, p.value) ~ "ANCOVA (ajustada por sexo, idade e IMC)") %>%
  # modify_header(estimate ~ '**Diferença ajustada**') %>%
  bold_p() %>%
  bold_labels()
