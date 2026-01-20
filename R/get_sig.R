get_sig <- function(txt) {
  pd <- utils::getParseData(parse(text = txt))

  # ⚠️ I Suspect your current file is MISSING this line:
  # pd <- pd[pd$terminal == TRUE, ]

  # Filter comments
  pd <- pd[pd$token != "COMMENT", ]

  # Anonymize
  pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FORMALS")] <- "VAR"
  pd$text[pd$token %in% c("NUM_CONST", "STR_CONST")] <- "CONST"

  return(paste(pd$text, collapse = " "))
}
