

fluidRow(
  column(8,card(card_header("Samples by diagnosis status"),card_image(file ='www/dashboar_Dx.png'))),
  column(4,
         card(card_header("Subjects by pedigree status"),card_image(file ='www/dashboar_Ped.png')),
         card(card_header("Subjects by age"),card_image(file ='www/dashboar_Age.png')),
         card(card_header("Subjects by ethnicity and race"),card_image(file ='www/dashboar_Race.png')))
)
