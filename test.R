# 1. Create the two "conflicting" functions
code_plus <- "function(x) { x + 10 }"
code_mult <- "function(x) { x * 10 }"

# 2. Parse them exactly how your tool does
# (Simulating the logic currently on your disk)
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

print(paste("Plus Signature:", get_sig(code_plus)))
print(paste("Mult Signature:", get_sig(code_mult)))
