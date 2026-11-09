

# Fit the overview to the available desktop viewport. Each chart can also be
# expanded with the card's full-screen button; narrow screens stack the cards.
div(
  id = "gmdp-dashboard",
  card(
    class = "gmdp-dashboard-card gmdp-dashboard-diagnosis",
    full_screen = TRUE,
    card_header("Samples by diagnosis status"),
    card_body(tags$img(
      src = "dashboar_Dx.png",
      alt = "Numbers of plasma and fibroblast samples by diagnosis status"
    ))
  ),
  card(
    class = "gmdp-dashboard-card",
    full_screen = TRUE,
    card_header("Subjects by pedigree status"),
    card_body(tags$img(
      src = "dashboar_Ped.png",
      alt = "Numbers of affected and unaffected subjects by pedigree and cohort"
    ))
  ),
  card(
    class = "gmdp-dashboard-card",
    full_screen = TRUE,
    card_header("Subjects by age"),
    card_body(tags$img(
      src = "dashboar_Age.png",
      alt = "Subject ages by pedigree and cohort"
    ))
  ),
  card(
    class = "gmdp-dashboard-card",
    full_screen = TRUE,
    card_header("Subjects by ethnicity and race"),
    card_body(tags$img(
      src = "dashboar_Race.png",
      alt = "Numbers of subjects by ethnicity, race and cohort"
    ))
  ),
  tags$script(src = "gmdp-dashboard.js")
)
