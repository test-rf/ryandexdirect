yadirGetAds <- function(CampaignIds   = NULL, 
                        AdGroupIds    = NA, 
                        Ids           = NA, 
                        States        = c("OFF","ON","SUSPENDED","OFF_BY_MONITORING","ARCHIVED"), 
                        Login         = NULL,
                        Token         = NULL,
                        AgencyAccount = NULL,
                        TokenPath     = getwd()){
  
  #Àâòîðèçàöèÿ
  Token <- tech_auth(login = Login, token = Token, AgencyAccount = AgencyAccount, TokenPath = TokenPath)

  #Ïðîâåðÿåì åñëè íå çàäàí ñïèñîê ðåêëàìíûõ êàìïàíèé çàãðóæàåì åãî è ïîëó÷àåì âñå ãðóïïû
  if (is.null(CampaignIds)) {
    CampaignIds <-  yadirGetCampaignList(Login         = Login,
                                         AgencyAccount = AgencyAccount,
                                         Token         = Token,
                                         TokenPath     = TokenPath)$Id
  }
#Ôèêñèðóåì âðåìÿ íà÷àëà ðàáîòû
start_time  <- Sys.time()

#Ðåçóëüòèðóþùèé äàòà ôðåéì
result      <- data.frame(Id                  = integer(0), 
                         # AdGroupId           = integer(0),
                         # CampaignId          = integer(0),
                         # Type                = character(0),
                         # Subtype             = character(0),
                         # Status              = character(0),
                        #  AgeLabel            = character(0),
                        #  State               = character(0),
                        #  TextAdTitle         = character(0),
                         # TextAdTitle2        = character(0),
                        #  TextAdText          = character(0),
                          TextAdHref          = character(0))
                        #  TextAdDisplayDomain = character(0),
                        #  TextAdMobile        = character(0),
                        #  TextImageAdHref     = character(0))

#Ïåðåâîäèì ôèëüòð ïî ñòàòóñó â json
States          <- paste("\"",States,"\"",collapse=", ",sep="")

#Îïðåäåëÿåì êîëè÷åñòâî êàìïàíèé êîòîðîå òðåáóåòñÿ îáðàáîòàòü
camp_num     <- as.integer(length(CampaignIds))
camp_start   <- 1
camp_step    <- 10

packageStartupMessage("Processing", appendLF = F)
#Çàïóñêàåì öèêë îáðàáîòêè êàìïàíèé
while(camp_start <= camp_num){

#îïðåäåëÿåì êàêîå ê-âî ÐÊ íàäî îáðàáîòàòü
camp_step   <-  if(camp_num - camp_start >= 10) camp_step else camp_num - camp_start + 1

#Ïðåîáðàçóåì ñïèñîê ðåêëàìíûõ êàìïàíèé
Ids             <- ifelse(is.na(Ids), NA,paste0(Ids, collapse = ","))
AdGroupIds      <- ifelse(is.na(AdGroupIds),NA,paste0(AdGroupIds, collapse = ","))
CampaignIdsTmp  <- paste("\"",CampaignIds[camp_start:(camp_start + camp_step - 1)],"\"",collapse=", ",sep="")

#Çàäà¸ì íà÷àëüíûé offset
lim <- 0

while(lim != "stoped"){
  
  queryBody <- paste0("{
  \"method\": \"get\",
                      \"params\": {
                      \"SelectionCriteria\": {
                      \"CampaignIds\": [",CampaignIdsTmp,"],
                      ",ifelse(is.na(Ids),"",paste0("\"Ids\": [",Ids,"],")),"        
                      ",ifelse(is.na(AdGroupIds),"",paste0("\"AdGroupIds\": [",AdGroupIds,"],")),"
                      \"States\": [",States,"]
},
                      
                      \"FieldNames\": [
                      \"Id\"
                     ],
                      \"TextAdFieldNames\": [
                      
                      \"Href\"
                     ],
                      
                      \"Page\": {  
                      \"Limit\": 10000,
                      \"Offset\": ",lim,"}
}
}")

  answer <- POST("https://api.direct.yandex.com/json/v5/ads", body = queryBody, add_headers(Authorization = paste0("Bearer ",Token), 'Accept-Language' = "ru",'Client-Login' = Login))
  stop_for_status(answer)
  dataRaw <- content(answer, "parsed", "application/json")
  
  #Ïðîâåðêà íå âåðíóë ëè çàïðîñ îøèáêó
  if(length(dataRaw$error) > 0){
    stop(paste0(dataRaw$error$error_string, " - ", dataRaw$error$error_detail))
  }
  
#Ïàðñåð îòâåòà
  for(ads_i in 1:length(dataRaw$result$Ads)){
      result      <- rbind(result,
                           data.frame(Id                  = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Id), NA,dataRaw$result$Ads[[ads_i]]$Id), 
                                    #  AdGroupId           = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$AdGroupId), NA,dataRaw$result$Ads[[ads_i]]$AdGroupId),
                                    #  CampaignId          = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$CampaignId), NA,dataRaw$result$Ads[[ads_i]]$CampaignId),
                                    #  Type                = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Type), NA,dataRaw$result$Ads[[ads_i]]$Type),
                                    #  Subtype             = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Subtype), NA,dataRaw$result$Ads[[ads_i]]$Subtype),
                                     # Status              = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Status), NA,dataRaw$result$Ads[[ads_i]]$Status),
                                    #  AgeLabel            = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$AgeLabel), NA,dataRaw$result$Ads[[ads_i]]$AgeLabel),
                                    #  State               = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$State), NA,dataRaw$result$Ads[[ads_i]]$State),
                                    #  TextAdTitle         = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Title), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Title),
                                    #  TextAdTitle2        = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Title2), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Title2),
                                     # TextAdText          = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Text), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Text),
                                    TextAdHref          = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Href), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Href)))
                                    #  TextAdDisplayDomain = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$DisplayDomain), NA,dataRaw$result$Ads[[ads_i]]$TextAd$DisplayDomain),
                                     # TextAdMobile        = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Mobile), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Mobile),
                                     # TextImageAdHref     = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextImageAd$Href), NA,dataRaw$result$Ads[[ads_i]]$TextImageAd$Href)))
}
#Äîáàâëÿåì òî÷êó, ÷òî ïðîöåññ çàãðóçêè èä¸ò
  packageStartupMessage(".", appendLF = F)
#Ïðîâåðÿåì îñòàëèñü ëè åù¸ ñòðîêè êîòîðûå íàäî çàáðàòü
 lim <- ifelse(is.null(dataRaw$result$LimitedBy), "stoped",dataRaw$result$LimitedBy + 1)
}

#Îïðåäåëÿåì ñëåäóþùèé ïóë êàìïàíèé
camp_start <- camp_start + camp_step
}

#Ôèêñèðóåì âðåìÿ çàâåðøåíèÿ îáðàáîòêè
stop_time <- Sys.time()

#Ñîîáùåíèå î òîì, ÷òî çàãðóçêà äàííûõ ïðîøëà óñïåøíî
packageStartupMessage("Done", appendLF = T)
packageStartupMessage(paste0("Êîëè÷åñòâî ïîëó÷åííûõ îáúÿâëåíèé: ", nrow(result)), appendLF = T)
packageStartupMessage(paste0("Äëèòåëüíîñòü ðàáîòû: ", round(difftime(stop_time, start_time , units ="secs"),0), " ñåê."), appendLF = T)
#Âîçâðàùàåì ðåçóëüòàò
return(result)}
