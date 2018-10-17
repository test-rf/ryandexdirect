yadirGetToken <-
function(){
  browseURL("https://oauth.yandex.ru/authorize?response_type=token&client_id=")
  token <- readline(prompt = "Enter your token: ")
  return(token)
}
