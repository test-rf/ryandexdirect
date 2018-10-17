yadirGetToken <-
function(){
  browseURL("https://oauth.yandex.ru/authorize?response_type=token&client_id=dd90e90c94504d23a9ed396473a538a9")
  token <- readline(prompt = "Enter your token: ")
  return(token)
}
