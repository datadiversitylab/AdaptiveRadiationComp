# Functions for fixing the accents in our datasets

##### Galapagos #####
fixnames_galap <- function(dat){
  match_rows <- which(dat$name == "EspaÃ±ola")
  dat$name[match_rows] <- "Española"
  
  match_rows <- which(dat$name == "PinzÃ³n")
  dat$name[match_rows] <- "Pinzón"
  
  match_rows <- which(dat$name == "Santa FÃ©")
  dat$name[match_rows] <- "Santa Fé"
  
  return(dat)
}

##### Caribbean #####
fixnames_carib <- function(dat){
  match_rows <- which(dat$name == "Cayo AlacrÃƒÂ¡n Chico")
  dat$name[match_rows] <- "Cayo Alacrán Chico"
  
  match_rows <- which(dat$name == "Cayo AlgodÃƒÂ³n Grande")
  dat$name[match_rows] <- "Cayo Algodón Grande"
  
  match_rows <- which(dat$name == "Cayo AntÃƒÂ³n Chico")
  dat$name[match_rows] <- "Cayo Antón Chico"
  
  match_rows <- which(dat$name == "Cayo AntÃƒÂ³n Grande")
  dat$name[match_rows] <- "Cayo Antón Grande"
  
  match_rows <- which(dat$name == "Cayo ÃƒÂvalos")
  dat$name[match_rows] <- "Cayo Ávalos"
  
  match_rows <- which(dat$name == "Cayo BahÃƒÂ­a de CÃƒÂ¡diz")
  dat$name[match_rows] <- "Cayo Bahía de Cádiz"
  
  match_rows <- which(dat$name == "Cayo de NavÃƒÂ­o")
  dat$name[match_rows] <- "Cayo de Navío"
  
  match_rows <- which(dat$name == "Cayo del MasÃƒÂ­o")
  dat$name[match_rows] <- "Cayo del Masío"
  
  match_rows <- which(dat$name == "Cayo Diego PÃƒÂ©rez")
  dat$name[match_rows] <- "Cayo Diego Pérez"
  
  match_rows <- which(dat$name == "Cayo FrancÃƒÂ©s")
  dat$name[match_rows] <- "Cayo Francés"
  
  match_rows <- which(dat$name == "Cayo GenovÃƒÂ©s")
  dat$name[match_rows] <- "Cayo Genovés"
  
  match_rows <- which(dat$name == "Cayo GuÃƒÂ¡simas")
  dat$name[match_rows] <- "Cayo Guásimas"
  
  match_rows <- which(dat$name == "Cayo GÃƒÂ¼incho")
  dat$name[match_rows] <- "Cayo Güincho"
  
  match_rows <- which(dat$name == "Cayo GuzmÃƒÂ¡n")
  dat$name[match_rows] <- "Cayo Guzmán"
  
  match_rows <- which(dat$name == "Cayo InÃƒÂ©s de Soto")
  dat$name[match_rows] <- "Cayo Inés de Soto"
  
  match_rows <- which(dat$name == "Cayo Juan RuÃƒÂ­z")
  dat$name[match_rows] <- "Cayo Juan Ruíz"
  
  match_rows <- which(dat$name == "Cayo La AlegrÃƒÂ­a")
  dat$name[match_rows] <- "Cayo La Alegría"
  
  match_rows <- which(dat$name == "Cayo Las PicÃƒÂºas")
  dat$name[match_rows] <- "Cayo Las Picúas"
  
  match_rows <- which(dat$name == "Cayo MajÃƒÂ¡")
  dat$name[match_rows] <- "Cayo Majá"
  
  match_rows <- which(dat$name == "Cayo Mal PaÃƒÂ­s")
  dat$name[match_rows] <- "Cayo Mal País"
  
  match_rows <- which(dat$name == "Cayo MatÃƒÂ­as")
  dat$name[match_rows] <- "Cayo Matías"
  
  match_rows <- which(dat$name == "Cayo MÃƒÂ©gano Grande")
  dat$name[match_rows] <- "Cayo Mégano Grande"
  
  match_rows <- which(dat$name == "Cayo MontaÃƒÂ±ÃƒÂ©s")
  dat$name[match_rows] <- "Cayo Montañés"
  
  match_rows <- which(dat$name == "Cayo ParedÃƒÂ³n Grande")
  dat$name[match_rows] <- "Cayo Paredón Grande"
  
  match_rows <- which(dat$name == "Cayo Punta Tierra del GuzmÃƒÂ¡n")
  dat$name[match_rows] <- "Cayo Punta Tierra del Guzmán"
  
  match_rows <- which(dat$name == "Cayo Rancho de CÃƒÂ¡ndido")
  dat$name[match_rows] <- "Cayo Rancho de Cándido"
  
  match_rows <- which(dat$name == "Cayo TÃƒÂ­o Pepe")
  dat$name[match_rows] <- "Cayo Tío Pepe"
  
  match_rows <- which(dat$name == "ÃƒÅ½le ÃƒÂ  Vache")
  dat$name[match_rows] <- "Île à Vache"
  
  match_rows <- which(dat$name == "ÃƒÅ½le de la GonÃƒÂ¢ve")
  dat$name[match_rows] <- "Île de la Gonâve"
  
  match_rows <- which(dat$name == "ÃƒÅ½le Grosse Caye")
  dat$name[match_rows] <- "Île Grosse Caye"
  
  match_rows <- which(dat$name == "ÃƒÅ½le Saint-BarthÃƒÂ©lemy")
  dat$name[match_rows] <- "Île Saint-Barthélemy"
  
  match_rows <- which(dat$name == "La DÃƒÂ©sirade")
  dat$name[match_rows] <- "La Désirade"
  
  match_rows <- which(dat$name == "Cayo DoÃƒÂ±a MarÃƒÂ­a")
  dat$name[match_rows] <- "Cayo Doña María"
  
  match_rows <- which(dat$name == "ÃƒÅ½le Corny")
  dat$name[match_rows] <- "Île Corny"
  
  match_rows <- which(dat$name == "ÃƒÅ½let ÃƒÂ  Fajou")
  dat$name[match_rows] <- "Îlet à Fajou"
  
  return(dat)
}
