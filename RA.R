
# Resident Analysis
# Nicolas Ampuero 12/10/18

# Outline:
# Get list of clubs where top 1000 artists play
# Get details about each club (address, capacity etc)
# Get list of shows at each club
# Get artist details (genre, popularity etc)

# Setup ----
suppressPackageStartupMessages({
  library(xml2)
  library(jsonlite)
  library(curl)
  library(httr)
  library(rvest)
  library(data.table)
  library(proxy)
  library(scales)
  library(fields)
  library(spotifyr)
  library(wbstats)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(ggmap)
  library(treemapify)
  library(network)
  library(sna)
  library(ggnetwork)
  library(ggrepel)
  library(showtext)
})

Sys.setenv(GOOGLE_TOKEN="your_token",
           SPOTIFY_CLIENT_ID="your_id",
           SPOTIFY_CLIENT_SECRET="your_secret")

access_token <- get_spotify_access_token()
register_google(Sys.getenv("GOOGLE_TOKEN"))

family <- "Open Sans"
font_add_google(family,family)
if (.Platform$OS.type=="windows")
  windowsFonts("Open Sans"=windowsFont("Open Sans"))

showtext_auto()

years <- 2018:2016
blank <- element_blank()
text <- element_text(family)

# Define helper functions ----
fail_with <- function(error,...) {
  tryCatch(...,error=function(e) error)
}

node_text <- function(x,css) {
  html_text(html_nodes(x,css))
}

node_attr <- function(x,css) {
  html_attrs(html_nodes(x,css))
}

node_dt <- function(x,css) {
  a <- node_attr(x,css)
  if (length(a)==0)
    return()
  data.table(name=node_text(x,css),href=unlist(a))
}

lapply_progress <- function(x,fun,...) {
  n <- length(x)
  nc <- comma(n)
  
  lapply(seq(n),function(i) {
    value <- i/n
    ic <- comma(i)
    width <- trunc(getOption("width")-14L-nchar(nc)*2)
    bar <- round(width*value)
    percent <- round(100*value)
    space <- nchar(nc)+3L-nchar(ic)-nchar(percent)
    
    cat("\r  |",rep.int("=",bar),rep.int(" ",width-bar),"|",rep(" ",space)," ",ic,"/",nc," (",percent,"%)",sep="")
    flush.console()
    
    do.call(fun,list(x[[i]],...))
  })
}

read_endpoint <- function(endpoint,domain="https://www.residentadvisor.net") {
  lapply_progress(paste0(domain,endpoint),function(url) {
    fail_with(NULL,read_html(url))
  })
}

mapply_bind <- function(fun,...) {
  rbindlist(mapply(fun,...,SIMPLIFY=F),fill=T)
}

node_text_bind <- function(x,css,href) { # vectorized on css, keeps href from other object
  mapply_bind(function(x,href) {
    if (is.null(x))
      return()
    text <- sapply(css,function(css) node_text(x,css),simplify=F)
    text <- text[sapply(text,length)==1]
    c(text,list(href=href))
  },x,href)
}

extract_first <- function(pattern,x,...) {
  x <- regmatches(x,gregexpr(pattern,x,...))
  x[which(sapply(x,length)==0)] <- NA_character_
  sapply(x,"[[",1)
}

google_geocode <- function(address) {
  url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?",
                "address=",curl_escape(address),"&key=",Sys.getenv("GOOGLE_TOKEN"))
  response <- fail_with(NULL,GET(url))
  if (is.null(response) || status_code(response)!=200)
    return()
  content <- content(response,"text")
  json <- fromJSON(content)
  json$results
}

ggmap_wrapper <- function(location,...) {
  center <- location[,unlist(lapply(.SD,mean)),.SDcols=c("lng","lat")]
  zoom <- calc_zoom(lng,lat,location)
  style <- paste(c("feature:road|element:labels|visibility:off",
                   "feature:road.highway|element:geometry|visibility:off",
                   "feature:poi|visibility:off",
                   "feature:transit|visibility:off"),
                 collapse="&style=")
  map <- suppressMessages(get_googlemap(center,zoom=zoom,maptype="roadmap",style=style,...))
  bb <- attr(map,"bb")
  suppressMessages({
    ggmap(map,extent="normal")+
      coord_quickmap(xlim=c(bb$ll.lon,bb$ur.lon),ylim=c(bb$ll.lat,bb$ur.lat),expand=F)+
      theme_blank()
  })
}

top <- function(x,y,decreasing=T,n=3) {
  z <- x[order(y,na.last=T,decreasing=decreasing)]
  unique(z)[seq(n)]
}

# Scrape/extract ----
formatDate <- "%a, %d %b %Y"
patternDate <- "[A-Z][a-z]{2}, [0-9]+ [A-Z][a-z]{2} [0-9]{4}"

topNode <- read_endpoint("/dj.aspx")
topText <- node_dt(topNode[[1]],".pr8 a")
topText <- unique(topText[grepl("/dj/",href) & name!=""])

artistNode <- read_endpoint(topText$href)
css <- c(name="#featureHead h1",name2="#sectionHead h1",country="#detail a span",followers="#MembersFavouriteCount")
artistText <- node_text_bind(artistNode,css,topText$href)

saveRDS(artistText,"data/artistText.rds")

datesNode <- read_endpoint(paste0(artistText$href,"/dates"))
datesText <- mapply_bind(function(x,href) {
  if (is.null(x))
    return()
  
  text <- node_text(x,"#Form1 span")
  i <- which(grepl(patternDate,text))
  text <- data.table(date=text[i],clubName=text[i+2])
  
  link <- node_dt(x,"span:nth-child(1) a")
  if (is.null(link))
    return(text)
  
  setnames(link,c("name","href"),c("clubName","club"))
  full <- merge(unique(text),unique(link),"clubName",all.x=T)
  
  cbind(full,artist=href)
},datesNode,artistText$href)

saveRDS(datesText,"data/datesText.rds")

href <- unique(datesText$club)
clubNode <- read_endpoint(href)
css <- c(detail="#detail .clearfix",pnl="#pnlButton .clearfix")
clubText <- node_text_bind(clubNode,css,href)

saveRDS(clubText,"data/clubText.rds")

eventText <- clubText[rep(seq(.N),each=length(years)),.(club=href)]
eventNode <- read_endpoint(paste0(eventText$club,"&show=events&yr=",years)) #takes a while
eventText <- mapply_bind(function(x,club) {
  if (is.null(x))
    return()
  detail <- node_text(x,"#divArchiveEvents li")
  if (length(detail)==0)
    return()
  data.table(detail,club)
},eventNode,eventText$club)

saveRDS(eventText,"data/eventText.rds")

# Clean/Format ----
artist <- copy(artistText)
artist[which(is.na(name)),name:=name2]
artist[,name2:=NULL]
artist[,followers:=as.numeric(gsub("[[:punct:]]","",followers))]

dates <- copy(datesText)
dates[,date:=as.Date(date,formatDate)]

detail <- gsub("\n|\t|\r","",clubText$detail)
tag <- c("Address","Phone","Capacity","Aka")
pattern <- paste(tag,"/[^/]+/")
detail <- as.data.table(lapply(pattern,extract_first,detail))
setnames(detail,tolower(tag))

pattern <- paste(paste(c(tag,"On the internet"),"/"),collapse="|")
detail <- detail[,lapply(.SD,function(x) gsub(pattern,"",x))]

detail[,capacity:=as.numeric(gsub(",","",capacity))]
detail[,address:=gsub("[[:punct:]]$","",gsub("^ | $","",gsub("[[:space:]]+"," ",gsub("[A-Za-z ]+$","",address))))]
detail[,address:=gsub("\\.","",gsub("Road","Rd",gsub("Avenue","Ave",gsub("Street","St",address))))] #avoid easy dupes

club <- cbind(detail,clubText[,.(followers=as.numeric(gsub("[[:punct:]]|[[:alpha:]]","",pnl)),href)])

event <- eventText[,.(date=as.Date(extract_first(patternDate,detail),formatDate),
                      attending=as.numeric(extract_first("^[0-9]+",gsub(patternDate,"",detail))),
                      name=gsub(paste0(patternDate,"[0-9]+ Attending"),"",detail),
                      club)]

# Query Spotify ----
genreList <- lapply_progress(artist$name,function(name) fail_with(NULL,get_artists(name)))

saveRDS(genreList,"data/genreList.rds")

genre <- mapply_bind(function(artist,result) {
  if (length(result)==0)
    return()
  mapply_bind(function(artistName,followers,name) {
    if (is.null(name))
      return()
    data.table(artist,artistName,followers,name)
  },result$artist_name,result$artist_num_followers,result$artist_genres)
},artist$href,genreList)

genre <- genre[which(!duplicated(paste(artistName,name)))]
genre[,count:=.N,name]
genre[,likely:=count>10]
genre[,score:=mean(likely),.(artist,artistName)]
setorder(genre,artist,-score)
genre[,keep:=artistName==artistName[1],artist]
genre <- genre[which(keep)]
genre[,name:=gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",name,perl=T)] #capitalize

# Geocode ----
address <- club[which(!duplicated(href) & nchar(address)>=5),unique(address)]
geocodeList <- lapply_progress(address,google_geocode)
names(geocodeList) <- address

saveRDS(geocodeList,"data/geocodeList.rds")

geocode <- mapply_bind(function(results,address) {
  formatted <- results$formatted_address
  if (length(formatted)==0)
    return()
  
  coded <- cbind(address,formatted,results$geometry$location)
  
  components <- results$address_components[[1]]
  if (length(components)==0)
    return(coded)
  
  types <- c("street_number","route",paste0("administrative_area_level_",1:2),"postal_code","country")
  x <- lapply(components$types,function(x) x[which(x %in% types)])
  i <- which(sapply(x,length)>0)
  if (length(i)==0)
    return(coded)
  
  type <- sapply(x[i],"[[",1)
  iso <- components$short_name[i][which(type=="country")]
  components <- setNames(components$long_name[i],type)
  components <- as.list(components[!duplicated(names(components))])
  if (length(iso))
    components <- c(components,list(iso=iso))
  cbind(coded,components)
},geocodeList,address)

x <- names(which(sapply(geocode,is.factor)))
geocode[,(x):=lapply(.SD,as.character),.SDcols=x]

km <- rdist.earth(geocode[,.(lng,lat)],miles=F,R=6371)
cluster <- hclust(as.dist(km),method="single")

# Get World Bank country populations ----
country <- as.data.table(wb(indicator="SP.POP.TOTL",startdate=min(years)-1,enddate=max(years)-1))
country[,year:=as.integer(date)+1L] # shift lagged indicator
setnames(country,c("iso2c","value","country"),c("iso","population","name"))

# Merge/Add Features ----
geo <- copy(geocode)
geo[,cluster:=cutree(cluster,h=10)]
geo[,count:=.N,cluster]
geo[,cluster:=as.numeric(reorder(cluster,-count,max))] # label clusters in order of total number of clubs

geo[,region:=ifelse(is.na(administrative_area_level_2),administrative_area_level_1,administrative_area_level_2)]
geo[,N:=.N,.(country,region)]
geo[,region:=region[which.max(N)],cluster]
geo[,N:=NULL]

loc <- merge(club,geo,"address")
setnames(loc,"href","club")
loc <- merge(loc,unique(dates[which(!is.na(club)),.(club,name=clubName)]),"club")
loc[,label:=paste(unique(name),collapse=" | "),.(lat,lng)]
loc[which(nchar(label)>50),label:=paste0(substring(label,1,50),"...")]

master <- copy(event)
master[,year:=year(date)]
master[,month:=month(date)]
master[,ym:=format(date,"%Y%m")]
master <- unique(master[which(year>=min(years) & ym<max(ym))])
master[,top:=paste(club,date) %in% dates[,paste(club,date)]]
master <- master[,.(events=.N,top=sum(top),attending=sum(attending,na.rm=T)),.(club,year,month)]
master <- merge(master,loc,"club")

cols <- c("events","top","attending")
by <- c("cluster","region")
scene <- master[which(year==max(year))]
scene <- merge(scene[,.(clubs=length(unique(club))),by],
               scene[,lapply(.SD,sum,na.rm=T),by,.SDcols=cols],by)
scene[,attending:=round(ifelse(events<10,0,attending/events))]
x <- c("clubs",cols)
scene[,(x):=lapply(.SD,function(y) paste0(comma(y)," (",ordinal(frank(-y,ties.method="min"),big.mark=","),")")),.SDcols=x]
setorder(scene,cluster)

location <- master[which(year==max(year)),lapply(.SD,sum,na.rm=T),.(cluster,region,label,lat,lng),.SDcols=cols]
location[,attending:=ceiling(ifelse(events==0,0,attending/events))] #avg
location[,score:=events*log(pmax(2,attending),10)]
setorder(location,cluster,-score)
location[,top5:=seq(.N)<=5,cluster]
location[,top10:=seq(.N)<=10,cluster]
setorder(location,cluster,score)
location[,quartile:=cut(score,quantile(score,seq(0,1,.25)),paste0("Q",1:4),include.lowest=T)]

sound <- merge(genre[which(likely)],dates[which(!is.na(club))],"artist",allow.cartesian=T)
sound <- merge(sound,loc,"club",suffixes=c("","X"))
sound <- sound[,.(count=.N),.(cluster,region,label,genre=name,year=year(date))]
sound <- merge(sound,location[,.(cluster,label,popular=top10)],c("cluster","label"))
x <- sound[,.(total=sum(count)),genre]
sound[,genre2:=ifelse(genre %in% x[,top(genre,total,n=20)],genre,"Rest")]
sound[,genre2:=reorder(genre2,-count,sum)]

save(artist,dates,club,event,genre,geocode,cluster,country,geo,loc,master,scene,location,sound,file="data/tables.RData")

# Analyze ----
load("data/tables.RData")

x <- artist[which(!is.na(country)),.(artists=.N),country]
x <- merge(x,country[which(year==max(year))],by.x="country",by.y="name")
x[,capita:=artists/population*1e7]
x[,capped:=pmin(mean(capita)+1.5*sd(capita),capita)]
x[,label:=paste0(country,"\n(",comma(ceiling(capita)),")")]

treeArtists <- ggplot(x,aes(area=artists))+
  labs(title="Artists")+
  geom_treemap(aes(fill=capped),color=NA,alpha=.8)+
  geom_treemap_text(aes(label=label,color=capita>quantile(capita,.75)),size=9,family=family)+
  scale_color_manual(values=c("TRUE"="black","FALSE"="white"),guide=F)+
  scale_fill_viridis_c(option="B",guide=F)+
  theme(title=text)

x <- master[which(!is.na(iso)),.(clubs=length(unique(club))),.(country,iso,year)]
x[,small:=max(clubs)<20,iso]
x <- merge(x[which(!small)],country,c("iso","year")) # safer than merging by name
x[,capita:=clubs/population*1e7]
x[,capitaLast:=capita[which.max(year)],country]
x[,label:=paste0(country,"\n(",ceiling(capita),")")]

treeClubs <- ggplot(x[which(year==max(year))],aes(area=clubs))+
  labs(title="Clubs")+
  geom_treemap(aes(fill=capitaLast),color=NA,alpha=.8)+
  geom_treemap_text(aes(label=label,color=capita>quantile(capita,.75)),size=9,family=family)+
  scale_color_manual(values=c("TRUE"="black","FALSE"="white"),guide=F)+
  scale_fill_viridis_c(option="B",guide=F)+
  theme(title=text)

x <- x[which(country %in% top(country,capita,n=20))]
setorder(x,country,year)
x[,yoy:=capita/shift(capita)-1,country]
x[,yoy2:=capita/shift(capita,2)-1,country]
x[,jump:=country %in% c(top(country,yoy),top(country,yoy2))]
x[,drop:=country %in% c(top(country,yoy,F),top(country,yoy2,F))]
x <- x[which(jump | drop)]

slopeCountry <- ggplot(x,aes(x=factor(year),y=capita))+
  geom_line(aes(group=country,color=country),size=2,alpha=.7)+
  geom_point(color="white",size=8)+
  geom_text(aes(label=ceiling(capita)),color="grey50",size=2,check_overlap=T,family=family)+
  geom_text(aes(label=country,hjust=1.25),x[,.SD[which.min(year)],country],size=3,color="grey50",family=family)+
  scale_x_discrete(expand=expand_scale(mult=c(.25,.1)))+
  scale_color_viridis_d(option="E",guide=F)+
  theme(panel.grid=blank,panel.background=blank,
        axis.title=blank,axis.ticks=blank,axis.text.y=blank,axis.text.x=text)

x <- location[which(cluster<=3)]
x <- split(x,x$cluster)
map <- lapply(x,ggmap_wrapper)

mapPoint <- mapply(function(x,map) {
  map+
    geom_point(aes(lng,lat),x,alpha=.3,shape=21,color="grey30",size=2,stroke=3,na.rm=T)
},x,map,SIMPLIFY=F)

mapDensity <- mapply(function(x,map) {
  map+
    stat_density_2d(aes(lng,lat,fill=stat(level),alpha=stat(level)),x,geom="polygon",na.rm=T,bins=20)+
    scale_fill_viridis_c(option="B",guide=F)+
    scale_alpha_continuous(range=c(0,.2),guide=F)
},x,map,SIMPLIFY=F)

# do.call(grid.arrange,c(mapPoint,mapDensity,list(nrow=2)))

# x <- location[which(cluster<=20),.N,.(cluster,region,quartile)] #find interesting
# x[,per:=N/sum(N),cluster]
# x[,sd:=sd(per),cluster]
# x <- x[which(quartile=="Q4"),cluster[c(which.max(per),which.min(sd))]]
x <- location[which(cluster %in% c(19,20))]
x <- split(x,x$cluster)

mapScore <- lapply(x,function(x) {
  ggmap_wrapper(x)+
    geom_point(aes(lng,lat,size=quartile,color=top5),x,alpha=.6,na.rm=T)+
    geom_label_repel(aes(lng,lat,label=label),x[which(top5)],size=3,nudge_y=0.01,
                     fill="black",color="white",alpha=.6,family=family,min.segment.length=0)+
    scale_size_manual(values=c(Q1=2,Q2=4,Q3=7,Q4=11),guide=F)+
    scale_color_manual(values=c("TRUE"="black","FALSE"="grey60"),guide=F)
})

# do.call(grid.arrange,c(mapScore,list(nrow=1)))

x <- master[,lapply(.SD,sum,na.rm=T),.(cluster,region,year,month),.SDcols=c("events","top")]
setnames(x,"events","total")
x[,rest:=total-top]
setorder(x,cluster,year,month)
x[,facet:=ifelse(year==min(year),paste(year,"events (top)"),year)]
x[,facet:=factor(facet,unique(facet))]
x <- melt(x,measure.vars=c("total","rest","top"))

y <- x[which(variable=="total"),.(sd=sd(value,na.rm=T),max=max(value)),cluster]
y <- y[which(max>100)]
y <- y[,cluster[sd %in% range(sd)]]
x <- x[which(cluster %in% y)]
x <- split(x,x$region)

barMonthly <- lapply(x,function(x) {
  y <- x[which(variable!="rest"),.SD[which.max(value)],.(facet,variable)]
  ggplot()+
    labs(title=x[1,region])+
    geom_col(aes(month,value,fill=variable),x[which(variable!="total")],width=.8)+
    geom_text(aes(month,value,label=comma(value),color=variable),y,vjust=-.5,size=4,family=family)+
    facet_wrap(~facet,nrow=1,strip.position="bottom")+
    scale_x_discrete(limits=1:12,labels=substr(month.abb,1,1))+
    scale_y_continuous(limits=c(0,NA),expand=expand_scale(mult=c(0,.2)))+
    scale_fill_manual("",values=c(top="grey60",rest="grey80"),guide=F)+
    scale_color_manual("",values=c(top="grey60",total="black"),guide=F)+
    theme(title=text,panel.background=blank,panel.grid=blank,axis.title=blank,
          axis.text.x=element_text(family,color="grey50"),axis.text.y=blank,axis.ticks=blank,
          strip.background=blank,strip.text=element_text(family,hjust=0,size=10),strip.placement="outside",
          legend.position="top",legend.direction="horizontal",legend.justification="left")
})

# do.call(grid.arrange,c(barMonthly,list(ncol=1)))

overall <- sound[,.(count=sum(count)),.(cluster,region,label,popular,genre)]
setorder(overall,cluster,label,-count)
overall[,top:=seq(.N)<=10,.(cluster,label)]

x <- location[which(cluster<=20),top(label,score)]
x <- overall[which(top & label %in% x)]
x[,label:=paste0(label," (",region,")")]
x <- dcast(x,label~genre,sum,na.rm=T,value.var="count",drop=F) #fill missing
x <- melt(x,"label",variable.name="genre",value.name="count")
x[which(is.na(count)),count:=0]
x[,share:=count/sum(count),label]
x[,genre:=reorder(genre,-share,sum)]

y <-  unique(x[,.(genre)])
setorder(y,genre)
y[,angle:=-365/.N]
y[,angle:=cumsum(shift(angle,fill=90+angle[1]/2))]
y[,ymax:=max(x$share)]

z <- y[which.min(abs(angle))]
z[,ymax:=max(x$share)]

radioGenre <- ggplot(x,aes(x=genre))+
  # geom_segment(aes(genre,0,xend=genre,yend=ymax),z,arrow=arrow(length=unit(2,"mm")),color="grey50")+
  # geom_text(aes(genre,ymax,label="More prevalent"),z,hjust=1,vjust=-.5,size=4,family=family,color="grey50")+
  geom_ribbon(aes(ymin=0,ymax=share,fill=label,group=label),alpha=.3)+
  # geom_line(aes(y=share,color=label,group=label),x,size=.5,alpha=.8)+
  geom_text(aes(y=ymax/3,label=genre,angle=angle),y,hjust=0,size=3,family=family,color="grey10")+
  coord_polar()+
  scale_x_discrete(limits=levels(x$genre))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_viridis_d("",option="E")+
  scale_fill_viridis_d("",option="E")+
  theme(panel.background=blank,panel.grid=blank,
        legend.position="top",legend.justification="left",legend.direction="vertical",legend.text=text,
        axis.title=blank,axis.text=blank,axis.ticks=blank)

x <- overall[which(cluster %in% c(5,6) & popular & top)]
x <- mapply_bind(function(x) { #there must be a better way directly with ggnetwork
  y <- network(x[,.(label,genre)])
  y <- ggnetwork(y)
  y$type <- ifelse(y$vertex.names %in% x$genre,"genre","club")
  cbind(x[1,.(cluster,region)],y)
},split(x,x$cluster))
x[,type2:=paste0(type,"2")]

netGenre <- ggplot(x,aes(x,y,xend=xend,yend=yend))+
  geom_edges(color="grey90",size=.5,curvature=.1)+
  geom_nodes(aes(color=type2,size=type2),alpha=.5)+
  geom_nodelabel_repel(aes(label=vertex.names,color=type,fill=type,size=type),
                       label.r=0,label.size=NA,alpha=.6,family=family,min.segment.length=2)+
  scale_color_manual(values=c(genre="grey50",genre2="grey50",club="white",club2="black"),guide=F)+
  scale_fill_manual(values=c(genre=NA,club="black"),guide=F)+
  scale_size_manual(values=c(genre=3,genre2=2,club=4,club2=4),guide=F)+
  facet_wrap(~region,ncol=1)+
  theme_blank(title=text,strip.text=element_text(family,hjust=0),strip.background=blank)


x <- dcast(sound[which(year>=(max(year)-5))],genre2~year,sum,value.var="count")
x <- melt(x,"genre2",variable.name="year",value.name="count")
x[,year:=as.numeric(as.character(year))]
x[which(is.na(count)),count:=0]
x[,share:=count/sum(count),year]

y <- x[which(year %in% range(year))]
setorder(y,year,-genre2,share)
y[,cum:=cumsum(share),year]
y[,lower:=shift(cum,fill=0),year]
y[,mid:=lower+(cum-lower)/2]
y[,left:=year==min(year)]

areaGenre <- ggplot(x,aes(x=year))+
  geom_area(aes(y=share,fill=genre2),x)+
  geom_text(aes(y=mid,label=genre2,hjust=ifelse(left,1,0)),y,vjust=.5,size=3,family=family,color="grey50",check_overlap=T)+
  geom_text(aes(y=mid,label=percent(share),hjust=ifelse(left,-.2,1.2)),y,vjust=.5,size=2,family=family,color="white",check_overlap=T)+
  scale_fill_viridis_d(guide=F)+
  scale_x_continuous(expand=expand_scale(mult=c(.5,.5)),breaks=unique(x$year))+
  scale_y_continuous(expand=expand_scale(mult=c(.02,0)))+
  theme(panel.background=blank,panel.grid=blank,axis.title=blank,
        axis.text.x=element_text(family,color="grey50"),axis.text.y=blank,axis.ticks=blank)


save(treeArtists,treeClubs,slopeCountry,mapPoint,mapScore,mapDensity,barMonthly,radioGenre,netGenre,areaGenre,file="data/plots.RData")


