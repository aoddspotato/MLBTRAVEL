#' getMLBtraveldays
#'
#' @param day1 Day to use games as origin travel locations
#' @param day2 Day to use games as destination travel locations
#' @export
getMLBtraveldays <- function(day1,day2){
  ###########################################################################################

  require(jsonlite)
  require(data.table)
  require(lubridate)
  require(foreach)
  require(stringr)
  require(dplyr)
  #install geosphere package if not already installed
  if(!require("geosphere")){install.packages("geosphere")}
  require(geosphere)

  gfd_ymd <- function(year,month,day){
    if(nchar(day)==1){
      day <- paste0("0",day)
    }
    if(nchar(month)==1){
      month <- paste0("0",month)
    }
    return(paste0(year,month,day))
  }

  dyesterday <- gfd_ymd(year=year(day1),month=month(day1),day=day(day1))
  dtoday <- gfd_ymd(year=year(day2),month=month(day2),day=day(day2))


  ###########################################################################################

  gameslist <- list()
  ylist <- list()
  league <- "MLB"
  leaguedisplay <- "MLB"
  sport <- "BASEBALL"

  ###custom functions#####################################################################
  ########################################################################################

  numcomwords <- function (str1, str2){
    mapply(function(x, y) length(intersect(x, y)), strsplit(str1, " "), strsplit(str2, " "))
  }

  ########

  mtn_ts = function(utcTime){
    toTz = "America/Denver"
    utcTime = force_tz(utcTime,tzone= 'GMT')
    dt = as.POSIXct(format(utcTime,tz = toTz,origin ='GMT', usetz=TRUE))
    dt = force_tz(dt,tzone= toTz)
    return(dt)
  }

  ########

  #MLB TEAM LOOKUP (custom)


  download.file("https://docs.google.com/spreadsheets/d/1E01ih5p5YGDP7_UfK6iFkq61OUQBOj1hm_hwpKlBjCc/gviz/tq?tqx=out:csv&sheet=mlb_masterteams",destfile="temp_mlbmasterteams.csv")
  masterdan <- fread("temp_mlbmasterteams.csv")

  opmlbteamlook <- function(teamname, league){
    teamname <- gsub("’","",teamname)

    teamnamenp <- gsub("-"," ",teamname)
    teamnamenp <- gsub("–"," ",teamnamenp)
    teamnamenp <- gsub("\\.","",teamnamenp)
    teamnamenp <- gsub("\\(","",teamnamenp)
    teamnamenp <- gsub("\\)","",teamnamenp)
    teamnamenp <- gsub("\\'","",teamnamenp)


    tcommon <- numcomwords(str1=toupper(masterdan$namenpf),str2=toupper(teamnamenp))



    if(max(tcommon)>0){
      tres <- masterdan[which(tcommon==max(tcommon)),]
      if(nrow(tres)>0){
        result <- tres[which.min(tres$nwords),]
      }
    }else{
      result <- data.frame(name="NF",namenpf="NF",nameCode="NF",opnamecode="NF",slug="NF",nwords=-99,league="NF",stringsAsFactors = F)
    }
    return(result)
  }

  ########################################################################################
  ########################################################################################

  #get venue gps coordinates and team lookup index
  ########################################################################################

  download.file("https://docs.google.com/spreadsheets/d/1p0R5qqR7XjoRG2mR5E1D_trlygHSqMOUdMgMpzq0gjU/gviz/tq?tqx=out:csv",destfile="temp_allvenuegps.csv")
  agps <- fread("temp_allvenuegps.csv")
  agps <- agps[agps$Sport=="MLB",]
  names(agps)[3]<- "Team"
  agps$teamslug <- "NF"
  foreach(ia=1:nrow(agps))%do%{
    tlt <- opmlbteamlook(agps$Team[ia],league=agps$Sport[ia])
    agps$teamslug[ia]<- tlt$slug[1]
  }

  venuegps <- data.frame(league=agps$Sport, teamslug_home=agps$teamslug,lat=agps$Latitude,lon=agps$Longitude, stringsAsFactors = F)


  ########################################################################################
  ########################################################################################

  #get required games and calculate location distances and time
  ########################################################################################


  ################# MLB ###################################

  #get YESTERDAY locations
  url <- paste0("http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/scoreboard?dates=",dyesterday)
  esl <- system(paste0("curl ",url, " --compressed"),intern=T)
  tleague <- fromJSON(esl)
  tleague$events$datetimeutc <- gsub("[[:alpha:]]"," ",as.character(tleague$events$date))
  tleague$events$datetimeutc2 <- ymd_hm(tleague$events$datetimeutc)
  tleague$events$mtntime <- mtn_ts(tleague$events$datetimeutc2)
  tleague$events$mtnhour <- hour(tleague$events$mtntime)
  tleague$events$mlbhour <- 13
  tleague$events$mlbhour[tleague$events$mtnhour<11]<-10
  tleague$events$mlbhour[tleague$events$mtnhour>15]<-20


  if(length(tleague$events)>0){
    if(!is.null(nrow(tleague$events))){
      foreach(x=1:nrow(tleague$events))%do%{
        teams <- tleague$events$competitions[[x]]$competitors[[1]]$team$displayName
        eteam1 <- opmlbteamlook(teamname=teams[1],league=league)
        team1 <- eteam1$slug[1]
        opnamecode1 <- eteam1$opnamecode[1]

        tgame <- system(paste0("curl http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/summary?event=",tleague$events$id[x]," --compressed"),intern=T)
        tga <- fromJSON(tgame)
        if(tga$meta$gameState=="post"){
          ltime <- tga$meta$lastPlayWallClock
          tdz <- data.frame(teamslug_home=team1, timeutc_lastplay=ltime, stringsAsFactors = F)
          tdzz <- left_join(tdz, venuegps)
          tdzz$league <- league
          names(tdzz)[1]<-"teamslug"
          ylist[[length(ylist)+1]]<- tdzz

          eteam2 <- opmlbteamlook(teamname=teams[2],league=league)
          team2 <- eteam2$slug[1]
          tdaw <- data.frame(teamslug=team2, timeutc_lastplay=ltime, stringsAsFactors = F)
          tdaw$lat <- tdzz$lat
          tdaw$lon <- tdzz$lon
          tdaw$league <- league
          ylist[[length(ylist)+1]]<- tdaw
        }
      }
    }

  }

  #compile time and distance data from loop
  ydata <- rbindlist(ylist, use.names = T)

  #get latest game only
  ydata <- ydata %>%
    group_by(teamslug) %>%
    arrange(desc(timeutc_lastplay), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup()


  #get TODAY locations to compare to yesterday locations

  url <- paste0("http://site.api.espn.com/apis/site/v2/sports/baseball/mlb/scoreboard?dates=",dtoday)
  esl <- system(paste0("curl ",url," --compressed"),intern=T)
  tleague <- fromJSON(esl)
  tleague$events$datetimeutc <- gsub("[[:alpha:]]"," ",as.character(tleague$events$date))
  tleague$events$datetimeutc2 <- ymd_hm(tleague$events$datetimeutc)
  tleague$events$mtntime <- mtn_ts(tleague$events$datetimeutc2)
  tleague$events$mtnhour <- hour(tleague$events$mtntime)
  tleague$events$mlbhour <- 13
  tleague$events$mlbhour[tleague$events$mtnhour<11]<-10
  tleague$events$mlbhour[tleague$events$mtnhour>15]<-20


  if(length(tleague$events)>0){
    if(!is.null(nrow(tleague$events))){
      foreach(x=1:nrow(tleague$events))%do%{
        teams <- tleague$events$competitions[[x]]$competitors[[1]]$team$displayName
        eteam1 <- opmlbteamlook(teamname=teams[1],league=league)
        team1 <- eteam1$slug[1]
        opnamecode1 <- eteam1$opnamecode[1]

        eteam2 <- opmlbteamlook(teamname=teams[2],league=league)
        team2 <- eteam2$slug[1]
        opnamecode2 <- eteam2$opnamecode[1]
        gamecode <- paste0(league,"_",gsub(" ","",sort(c(opnamecode1,opnamecode2))[1]),"_",gsub(" ","",sort(c(opnamecode1,opnamecode2))[2]),"_",dtoday,"_",tleague$events$mlbhour[x])
        gameslist[[length(gameslist)+1]] <- data.frame(gamecode=gamecode, sport=sport, league=league, leaguedisplay=leaguedisplay, eventname=tleague$events$name[x], datetime=tleague$events$date[x], espnid=tleague$events$id[x], teamslug_home=team1, teamslug_away=team2, displayname_home=teams[1], displayname_away=teams[2], stringsAsFactors = F)
      }
    }

  }


  ##################################################################### COMPILE RESULTS AND CALCULATE $$$ ##########################
  todaygames <- rbindlist(gameslist, use.names=T, fill=T)
  todaygames <- todaygames[!duplicated(todaygames$gamecode),]
  todaygames <- todaygames[!is.na(todaygames$gamecode),]
  todaygames$datetime <- ymd_hm(todaygames$datetime)

  names(ydata)<- paste0(names(ydata),"_home")
  names(ydata)[names(ydata)=="league_home"]<-"league"

  todaygames <- left_join(todaygames,ydata)

  names(ydata)<-gsub("_home","_away",names(ydata))
  todaygames <- left_join(todaygames,ydata)

  todaygames <- left_join(todaygames,venuegps)

  todaygames$homedistkm <- sapply(c(1:nrow(todaygames)),FUN=function(x){geosphere::distHaversine(p1=c(todaygames$lon_home[x],todaygames$lat_home[x]),p2=c(todaygames$lon[x],todaygames$lat[x]))})
  todaygames$homedistkm <- todaygames$homedistkm/1000
  todaygames$awaydistkm <- sapply(c(1:nrow(todaygames)),FUN=function(x){geosphere::distHaversine(p1=c(todaygames$lon_away[x],todaygames$lat_away[x]),p2=c(todaygames$lon[x],todaygames$lat[x]))})
  todaygames$awaydistkm <- todaygames$awaydistkm/1000

  todaygames$timeutc_lastplay_away <- ymd_hms(todaygames$timeutc_lastplay_away)
  todaygames$traveltime_away <- difftime( todaygames$datetime, todaygames$timeutc_lastplay_away, units="hours")
  todaygames$travelkph_away <- (todaygames$awaydistkm)/as.numeric(todaygames$traveltime_away)

  todaygames$timeutc_lastplay_home <- ymd_hms(todaygames$timeutc_lastplay_home)
  todaygames$traveltime_home <- difftime( todaygames$datetime, todaygames$timeutc_lastplay_home, units="hours")
  todaygames$travelkph_home <- (todaygames$homedistkm)/as.numeric(todaygames$traveltime_home)

  return(todaygames)
}
