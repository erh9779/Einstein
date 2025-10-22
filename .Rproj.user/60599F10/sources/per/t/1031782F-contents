# install necessary packages if not installed yet and load them
# preset constants, but be aware they might be overwritten by following loading of scripts
# load necessary scripts







# define features
    colors           <- c("rot",       "grün",     "blau",    "gelb",      "weiß")      # Hausfarbe
    house_numbers    <- c("eins",      "zwei",     "drei",    "vier",      "fünf")      # Hausposition
    nationalities    <- c("Brite",     "Schwede",  "Däne",    "Deutscher", "Norweger")  # Nationalität
    drinks           <- c("Tee",       "Kaffee",   "Bier",    "Milch",     "Wasser")    # Getränk
    cigarette_brands <- c("Rothmanns", "Winfield", "Dunhill", "Pall Mall", "Marlboro")  # Zigarettenmarke
    pets             <- c("Hund",      "Vogel",    "Katze",   "Pferd",     "Fisch")     # Haustier

# span six-dimensional space and fill it with 2 meaning no statement
    multi_array <- array(data     = 2
                        ,dim      = c(   length(colors), length(house_numbers), length(nationalities), length(drinks), length(cigarette_brands), length(pets))
                        ,dimnames = list(       colors,         house_numbers,         nationalities,         drinks,         cigarette_brands,         pets )
                        )



calculate_fish_owners <- function() {

  # fill space according to statements (0 meaning false, 1 meaning true, kept 2 meaning no statement)
    #  5) Der Brite lebt im roten Haus
      for (element in nationalities[   nationalities    != "Brite"    ]) multi_array["rot"  ,        , element    ,         ,             ,        ] <- 0
      for (element in colors[          colors           != "rot"      ]) multi_array[element,        , "Brite"    ,         ,             ,        ] <- 0
    #  6) Der Schwede hält einen Hund
      for (element in nationalities[   nationalities    != "Schwede"  ]) multi_array[       ,        , element    ,         ,             , "Hund" ] <- 0
      for (element in pets[            pets             != "Hund"     ]) multi_array[       ,        , "Schwede"  ,         ,             , element] <- 0
    #  7) Der Däne trinkt gerne Tee
      for (element in nationalities[   nationalities    != "Däne"     ]) multi_array[       ,        , element    , "Tee"   ,             ,        ] <- 0
      for (element in drinks[          drinks           != "Tee"      ]) multi_array[       ,        , "Däne"     , element ,             ,        ] <- 0
    #  8) Der Deutsche raucht Rothmanns
      for (element in nationalities[   nationalities    != "Detscher" ]) multi_array[       ,        , element    ,         , "Rothmanns" ,        ] <- 0
      for (element in cigarette_brands[cigarette_brands != "Rothmanns"]) multi_array[       ,        , "Deutscher",         , element     ,        ] <- 0
    #  9) Der Besitzer des grünen Hauses trinkt Kaffee
      for (element in colors[          colors           != "grün"     ]) multi_array[element,        ,            , "Kaffee",             ,        ] <- 0
      for (element in drinks[          drinks           != "Kaffee"   ]) multi_array["grün" ,        ,            , element ,             ,        ] <- 0
    # 10) Der Winfield-Raucher trinkt gerne Bier
      for (element in cigarette_brands[cigarette_brands != "Winfield" ]) multi_array[       ,        ,            , "Bier"  , element     ,        ] <- 0
      for (element in drinks[          drinks           != "Bier"     ]) multi_array[       ,        ,            , element , "Winfield"  ,        ] <- 0
    # 11) Der Norweger wohnt im ersten Haus
      for (element in nationalities[   nationalities    != "Norweger" ]) multi_array[       , "eins" , element    ,         ,             ,        ] <- 0
      for (element in house_numbers[   house_numbers    != "eins"     ]) multi_array[       , element, "Norweger" ,         ,             ,        ] <- 0
    # 12) Der Norweger wohnt neben dem blauen Haus -> Das blaue Haus ist das zweite Haus
      for (element in colors[          colors           != "blau"     ]) multi_array[element, "zwei" ,            ,         ,             ,        ] <- 0
      for (element in house_numbers[   house_numbers    != "zwei"     ]) multi_array["blau" , element,            ,         ,             ,        ] <- 0
    # 13) Der Besitzer des gelben Hauses raucht Dunhill
      for (element in colors[          colors           != "gelb"     ]) multi_array[element,        ,            ,         , "Dunhill"   ,        ] <- 0
      for (element in cigarette_brands[cigarette_brands != "Dunhill"  ]) multi_array["gelb" ,        ,            ,         , element     ,        ] <- 0
    # 14) Die Person, die Pall Mall raucht, hält einen Vogel
      for (element in cigarette_brands[cigarette_brands != "Pall Mall"]) multi_array[       ,        ,            ,         , element     , "Vogel"] <- 0
      for (element in pets[            pets             != "Vogel"    ]) multi_array[       ,        ,            ,         , "Pall Mall" , element] <- 0
    # 15) Der Mann, der im mittleren Haus wohnt, trinkt Milch
      for (element in house_numbers[   house_numbers    != "drei"     ]) multi_array[       , element,            , "Milch" ,             ,        ] <- 0
      for (element in drinks[          drinks           != "Milch"    ]) multi_array[       , "drei" ,            , element ,             ,        ] <- 0
    # 16) Das grüne Haus steht unmittelbar links vom weißen Haus
    # 17) Der Marlboro-Raucher wohnt neben dem, der eine Katze hält
    # 18) Der Marlboro-Raucher hat einen Nachbarn, der Wasser trinkt
    # 19) Der Mann, der ein Pferd hält, wohnt neben dem, der Dunhill raucht -> Der Mann, der ein Pferd hält, wohnt neben dem gelben Haus

  # extract possible fish owners
    possible_fish_owners <- c()
    if ((length(which(multi_array[ , , "Brite",     , , "Fisch"] == 2) %/% 5 %/% 5 %/% 5)) > 0) possible_fish_owners <- c(possible_fish_owners, "Brite")
    if ((length(which(multi_array[ , , "Schwede",   , , "Fisch"] == 2) %/% 5 %/% 5 %/% 5)) > 0) possible_fish_owners <- c(possible_fish_owners, "Schwede")
    if ((length(which(multi_array[ , , "Däne",      , , "Fisch"] == 2) %/% 5 %/% 5 %/% 5)) > 0) possible_fish_owners <- c(possible_fish_owners, "Däne")
    if ((length(which(multi_array[ , , "Deutscher", , , "Fisch"] == 2) %/% 5 %/% 5 %/% 5)) > 0) possible_fish_owners <- c(possible_fish_owners, "Deutscher")
    if ((length(which(multi_array[ , , "Norweger",  , , "Fisch"] == 2) %/% 5 %/% 5 %/% 5)) > 0) possible_fish_owners <- c(possible_fish_owners, "Norweger")
    return (possible_fish_owners)

}
