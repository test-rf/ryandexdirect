yadirAuth <- function(Login = NULL, NewUser = FALSE, TokenPath = getwd()) {
  
  # ïðîâåðÿåì õðàíèëèùå
  
  if (!dir.exists(TokenPath)) {
    dir.create(TokenPath)
  }
    
  # äëÿ ñîõðàíåíèÿ òîêíà
  
  if (NewUser == FALSE && file.exists(paste0(paste0(TokenPath, "/", Login, ".yadirAuth.RData")))) {
    message("Load token from ", paste0(paste0(TokenPath, "/", Login, ".yadirAuth.RData")))
    load(paste0(TokenPath, "/", Login, ".yadirAuth.RData"))
    # ïðîâåðÿåì êîãäà èñòåêàåò òîêåí  
    if (as.numeric(token$expire_at - Sys.time(), units = "days") < 30) {
      message("Auto refresh token")
      token_raw  <- httr::POST("https://oauth.yandex.ru/token", body = list(grant_type="refresh_token", 
                                                                            refresh_token = token$refresh_token,
                                                                            client_id = "",
                                                                            client_secret = ""), encode = "form")
      # ïðîâåðÿå íà îøèáêè
      if (!is.null(token$error_description)) {
        stop(paste0(token$error, ": ", token$error_description))
      }
      # ïàðñèì òîêåí
      new_token <- content(token_raw)
      # äîáàâëÿåì èíôîðìàöèþ î òîì êîãäà îí èñòåêàåò
      new_token$expire_at <- Sys.time() + as.numeric(token$expires_in, units = "secs")
      
      # ñîõðàíÿåì òîêåí â ôàéë
      save(new_token, file = paste0(TokenPath, "/", Login, ".yadirAuth.RData"))
      message("Token saved in file ", paste0(TokenPath, "/", Login, ".yadirAuth.RData"))
      
      return(new_token)
    } else {
      message("Token expire in ", round(as.numeric(token$expire_at - Sys.time(), units = "days"), 0), " days")
      
      return(token)
  }
}
  # åñëè òîêåí íå íàéäåí â ôàéëå òî ïîëó÷àåì êîä è ïðîõîäèì âñþ ïðîöåäóðó
  browseURL(paste0("https://oauth.yandex.ru/authorize?response_type=code&client_id=&force_confirm=", as.integer(NewUser), ifelse(is.null(Login), "", paste0("&login_hint=", Login))))
  # çàïðàøèâàåì êîä
  temp_code <- readline(prompt = "Enter authorize code:")
  
  # ïðîâåðêà ââåä¸ííîãî êîäà
  while(nchar(temp_code) != 7) {
    message("Ïðîâåðî÷íûé êîä ââåä¸ííûé âàìè íå ÿâëÿåòñÿ 7-çíà÷íûì, ïîâòîðèòå ïîïûòêó ââîäà êîäà.")
    temp_code <- readline(prompt = "Enter authorize code:")
  }
  
  token_raw <- httr::POST("https://oauth.yandex.ru/token", body = list(grant_type="authorization_code", 
                                                                       code = temp_code, 
                                                                       client_id = "", 
                                                                       client_secret = ""), encode = "form")
  # ïàðñèì òîêåí
  token <- content(token_raw)
  token$expire_at <- Sys.time() + as.numeric(token$expires_in, units = "secs")
  # ïðîâåðÿå íà îøèáêè
  if (!is.null(token$error_description)) {
    stop(paste0(token$error, ": ", token$error_description))
  }
  # ñîõðàíÿåì òîêåí â ôàéë
  save(token, file = paste0(TokenPath, "/", Login, ".yadirAuth.RData"))
  message("Token saved in file ", paste0(TokenPath, "/", Login, ".yadirAuth.RData"))
  
  return(token)
}
