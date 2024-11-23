#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(cores=4)
library(shiny)
library(dplyr)
library(shinyWidgets)
library(dbplyr)
library(magick)
library(DBI)
library(shinyjqui)
library(stringx)
library(shinyjs)
library(RSQLite)
library(shinyTime)
library(shinyToastify)
library(jsonlite)
library(lubridate)
library(emayili)
library(glue)
library(stringr)
library(googlesheets4)
library(googleCloudStorageR)
library(gargle)
library(rebus)
library(bigrquery)
options(shiny.port = 1221)
my_project_id<-"spheric-crow-377302"

#
#library(googlesheets4)
#gs4_auth(email = "wankr362@gmail.com", cache = ".secrets")
bq_auth(email = "wankrohit04@gmail.com",path = "WWW/spheric-crow-377302-28e6d3b3143a.json")
Sys.setenv("GCS_AUTH_FILE" = "WWW/spheric-crow-377302-28e6d3b3143a.json")

con<-dbConnect(bigquery(),project="spheric-crow-377302",dataset="Chat",biling=billing)

#con
x1=tbl(con,"Chat")
xx=x1%>%filter(Live=="Live_now")%>%collect()

#xx=x1%>%collect()
Sys.setenv("GCS_AUTH_FILE" = "spheric-crow-377302-28e6d3b3143a.json")



### supply project name in gcs_list_buckets(my_project_id)
gcs_list_buckets(my_project_id)


gcs_global_bucket("bucket-ish")

library(emayili)
library(jsonlite)
YOUR_API_KEY="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmFwcGVhci5pbiIsImF1ZCI6Imh0dHBzOi8vYXBpLmFwcGVhci5pbi92MSIsImV4cCI6OTAwNzE5OTI1NDc0MDk5MSwiaWF0IjoxNzAzMTM4NDI5LCJvcmdhbml6YXRpb25JZCI6MjA0ODI3LCJqdGkiOiIyZDI5ZGEyYS0xOTEwLTQwMTgtOTc0My04Nzk5M2RmMTAxYTEifQ.9y3jkyNnhYecW6LkEiDjY4m8DyDZ5wafNmd9roMOhQo"
library(httr)



email_from="rohitwankhede774@gmail.com"
smtp <-emayili::server(host = "smtp-relay.brevo.com",
                       port = 587,
                       username = "rohitwankhede774@gmail.com",
                       password = "7rchFRfjpQvWJ9bg")



# practice
facetime <- function(src1,cl,link,Name,City) {
  tagList(shiny::tags$div(class="f_time",style= "overflow-y:scroll;color:red;","LIVE ONLINE",shiny::tags$p("LIVE",style="position:absolute;bottom:50%;"), shiny::tags$p(class = cl,shiny::tags$image(class = cl,style="border:5px solid red;display:inline;width:50px;height:50px;border-radius:50%",src=src1,alt="Image not supported"),Name,City),shiny::tags$hr())
  )
}


chat_func<- function(From,Text_Message,Timeline,src1) {
  
  tagList(shiny::tags$div(#style="display: flex; flex-direction: column-reverse; /* ...probably usually along with: */overflow-y: scroll;  /* or hidden or auto */",),
    
    
    #id="message"#,tags$div(tags$script(src="div.js")
    
    #tags$h6(From),
    id="message-display" ,shiny::tags$image(alt="Image not Supported",src=src1,style="opacity:1;border-radius:50%;width:40px;height:40px;position:absolute;left:5%;"), shiny::tags$h6("By:",From,style="border-radius:50%;position:relative;bottom:80%;"),shiny::tags$div(style="overflow-y:scroll",shiny::tags$h6(shiny::tags$b(Timeline)),shiny::tags$p(Text_Message,style="")),style="min-height:100px;overflow:hidden;display:inline-block;
  border: 2px solid #dedede;
  color:white;
  border-color: #ccc;
  background-color: purple;
  border-radius: 20px;
  #opacity:.3;
  border-color:black;
  padding: 10px;
  margin: 10px 0color:blue;text-align:center; width:100% ; "),shiny::tags$br(),shiny::tags$hr(style="height:1px;background-color:darkpink;"),shiny::tags$p(class="p"))#)
  #}
  #e
  
}

#in schedule display and profile pic background color was grey
schedule_display<- function(time_z,to,name,li) {
  
  tagList( shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="sc_display",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=to,#paste0(x()$email,".png"),
                                                                                                                                                                                                                                                                                                                                                                                                                                   alt="Image not supported"),shiny::tags$h3(style="border-color:green;border-width:px;","NAME:",name),shiny::tags$h3("Scheduled Time:",time_z),shiny::tags$a(style="text-decoration: underline;width:100%;color:black;",href=li,"CLICK HERE TO ENTER THE ROOM \U2192"),shiny::tags$p("xxxxxxxxxxxxxxx",style="font-size:24px;"),shiny::tags$hr(style="border-top: 24px solid purple;"),shiny::tags$hr(style="border-top: 2px solid green;") )
  )
  )  
}


group_func<- function(From,Text_Message,Timeline) {
  
  tagList(shiny::tags$div(#style="overflow-y:scoll;"#"display: flex; flex-direction: column-reverse; /* ...probably usually along with: */overflow-y: scroll;  /* or hidden or auto */",),
    
    
    #id="message"#,tags$div(tags$script(src="div.js")
    
    #tags$h6(From),
    id="group-message-display" ,shiny::tags$h5(From,style="position:absolute;left:10%;"), shiny::tags$h6(Timeline),shiny::tags$h5(Text_Message,style="overflow-y:scroll ;"),style="display:inline-block;
  border: 2px solid #dedede;
  color:white;
  border-color: #ccc;
  background-color: purple;
  border-radius: 20px;
  #opacity:.3;
  border-color:black;
  padding: 10px;
  margin: 10px 0color:blue;text-align:center; width:500px ;height:70px; "),shiny::tags$br(),shiny::tags$hr(style="height:1px;background-color:darkpink;"),shiny::tags$p(class="p"))#)
  #}
  #e
  
}



data1=readRDS("db/data1.RDS")



##profile1=readRDS("db/profile_ai.RDS")

## profile1=as.data.frame(profile1)
#Inbox1=readRDS("db/Inbox1.RDS")
#Inbox1=as.data.frame(Inbox1)


Schedule_call=tbl(con,"Schedule")
#10-28-2024 Schedule_call=readRDS("db/Schedule_call1.RDS")
#data1=readRDS("db/data.RDS")
#data1=as.data.frame(data1[7:20,])

data1=as.data.frame(data1)




# Define UI for application that draws a histogra5
ui <- fluidPage(
  # textInputIcon("o","o",icon=icon("sms")),
  useShinyToastify(),
  useShinyjs(), waiter::use_waiter(),
  shiny::tags$script(src='i_js.js'),
  # Application title
  #titlePanel("I Solace Hub"),
  ##modified 11/8/2024tags$link(href="design232l2.css",rel="stylesheet"),
  shiny::tags$link(href="ish.css",rel="stylesheet"),
  shiny::tags$div(class="hider"),
  #tags$br(),#tags$br(),tags$br(),tags$br(),tags$br(),
  # welcome page
  shiny::tags$div(class="welcome", style="top:10%;border-radius:10%;left:12vw;width:70vw;opacity:1;height:50%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);#background-color:rgba(255, 117, 170, 1);
  color:black;
           font-size:20px;", shiny::tags$br(), "Welcome to I Solace Hub",id="welcome",shiny::tags$p(class="p1",),shiny::tags$br(),shiny::tags$br(),actionButton("Login1st","LOGIN",width = "80%"),shiny::tags$br(),shiny::tags$br(),actionButton("Signin1st","CREATE ACCOUNT",width = "80%")),shiny::tags$div("",class="div1p",style="height:10px; position: absolute;left:10%; top:2wv;")
  
  
  
)

# Define server logic re8888quired to draw a histogram
server <- function(input, output,session) {
  
  
  #onStop(function() unlink(paste0("WWW/",input$Login_Name,".png")))
  
  # login logic
  observeEvent(input$Login1st,{
    
    insertUI(selector =  "#Login1st",where = "beforeBegin",shiny::tags$div(style="width:7wv;position:relative;",textInput("Login_Name","Enter Email"),passwordInput("Login_password","Enter Password"),shiny::tags$br(),actionButton("Login2nd","Login",width = "80%")))
    
    
    
    removeUI("#Signin1st")
    
    removeUI("#Login1st")
    
  })
  # first time sign in logic
  observeEvent(once = T,input$Signin1st,{
    
    
    
    ##/ welcome2 logic
    insertUI(".div1p",where = "beforeBegin",ui=tagList( shiny::tags$div(class="welcome2", style="overflow-y:scroll;position:absolute;left:5vw;border-radius:0%;width:280px;height:100vh;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);color:black;
  overflow-y: auto;
           font-size:20px;",  "ENTER YOUR DETAILS",shiny::tags$p(class="p1"),
                                                                        textInput("Name","FULL NAME*"),
                                                                        #numericInput("Age","Age",min = 18,max=120,value = 18),
                                                                        selectInput("Gender","Gender",choices = c("Male","Female","LGBTQ")),
                                                                        textInput("Email","Enter Email*"),
                                                                      #  textInput("Phone","Enter Phone No With country code*"),
                                                                      #  textInput("Country","Enter Country*"),
                                                                       # textInput("City","Enter City*"),
                                                                        passwordInput("password","Set Password*"),
                                                                        fileInput("Photo","Upload Picture*"),
                                                                        textAreaInput("Lifestyle","Your Hobbies and Likes"),
                                                                        
                                                                        actionButton("Signin2nd","Sign In",icon = icon("face-smile-wink")))))
    
    removeUI("#welcome")
    removeUI("#Signin1st")
    
    removeUI("#Login1st")
    
  })
  
  
  pers=reactiveValues(Name="" , Email="" , Phone="" , Country="" , City="" , password=""  ,Photo="",pth="" ,Age=18, Gender="",Live="no",hostRoomUrl="",roomUrl="",Random=FALSE, date1=as.character(Sys.Date()),Lifestyle="")
  
  # sql1=glue_sql("insert into Chat values ({input$Name},{input$Email},{input$Phone},{input$Country},{input$City},{input$password},{pth},{input$Age},{input$Gender},'no','hostRoomUrl','roomUrl',False,{as.character(Sys.Date())},{input$Lifestyle})",.con=con)
  
  #signin2nd logic
  
  observeEvent(input$Signin2nd,ignoreInit = T,{
    
    
    
    
    if(input$Name=="" | input$Email==""   | input$password=="" | is.null(input$Photo)){
      
      
      insertUI(".welcome2","beforeEnd",shiny::tags$div(class="PLEASE LOOK FOR EMPTY FIELDS","PLEASE FILL OUT EMPTY FIELDS",width="20px",style="background-color:lightgreen;"))
      
    }
    else{
      img=image_read(input$Photo$datapath)
      pth=paste0("WWW/",input$Email,".png")
      #image_write(img,path=pth)
      showNotification("Image Sucessfully Saved")
      headers = c(
        `Authorization` = paste("Bearer ", YOUR_API_KEY, sep = ""),
        `Content-Type` = "application/json"
      )
      
      data = '{  "endDate": "2099-02-18T14:23:00.000Z",  "fields": ["hostRoomUrl"]}'
      
      
      res <- as.character(httr::POST(url = "https://api.whereby.dev/v1/meetings", httr::add_headers(.headers=headers), body = data))
      
      
      li=fromJSON(res)
      
      #sql= "INSERT INTO Chat (int64_field_0 , Name, Email ,Phone,Country ,City,Password,Image,Age,Gender, Live, Hos_url, Host_url ,url_link,Random)
      #VALUES =input$Name,Email=input$Email,Phone=input$Phone,Country=input$Country,City=input$City,Password=input$password,Image=pth,Age=input$Age,Gender=input$Gender,Joining_Date=as.character(Sys.Date()),Hos_url="-",Host_url=li$hostRoomUrl,li$roomUrl,'no'','no');"
      
      ## sql_u=glue_sql("insert into Chat values ({input$Name} , {input$Email} , {input$Phone} , {input$Country}, {input$City} , {input$password} ,{pth}, {input$Age}, {input$Gender}, 'no' ,{'hostRoomUrl'}, {'roomUrl'}, False, {as.character(Sys.Date())}, {input$Lifestyle} )",.con=con)
      
      # sql1=glue_sql("insert into Chat values ({input$Name},{input$Email},{input$Phone},{input$Country},{input$City},{input$password},{pth},{input$Age},{input$Gender},'no','hostRoomUrl','roomUrl',False,{as.character(Sys.Date())},{input$Lifestyle})",.con=con)
      # pers=reactiveValues(Name="" , Email="" , Phone="" , Country="" , City="" , password=""  ,Photo="",pth="" ,Age=18,Gender="",Live="no",hostRoomUrl="",roomUrl="",Random=False,date1=as.character(Sys.Date()),input$Lifestyle="")
      pers$Name<-input$Name
      pers$Email=input$Email
      pers$Phone=""
      pers$Country ="-"
      pers$City ="-"
      pers$password=input$password
      pers$pth = pth
      pers$Age =18
      pers$Gender =input$Gender
      pers$Live='no'
      pers$hostRoomUrl=li$hostRoomUrl
      pers$roomUrl=li$roomUrl
      pers$Random=F
      pers$date1=as.character(Sys.Date())
      
      pers$Lifestyle=input$Lifestyle
      
      data_em$em<-input$Email
      data_em$links<-li$hostRoomUrl
      sql_u=glue_sql("insert into Chat (Name, Email , Phone , Country , City, Password,    
                     Image, Age ,Gender ,Live, Host_url, url_link,    
                     Random, Joining_Date ,Bio) values ({input$Name},{input$Email},'-','-','-',{input$password},{paste0('https://storage.googleapis.com/bucket-ish/',input$Email,'.png')},18,{input$Gender},'don',{li$hostRoomUrl},{li$roomUrl},False,{as.character(Sys.Date())},{input$Lifestyle} )",.con=con)
      
      dc=dbGetQuery(con,sql_u)
      print(dc)
      print("saved data")
      
      # 10-28-2024data1=add_row(data1,Name=input$Name,Email=input$Email,Phone=input$Phone,Country=input$Country,City=input$City,Password=input$password,Image=pth,Age=input$Age,Gender=input$Gender,Joining_Date=as.character(Sys.Date()),Hos_url="-",Host_url=li$hostRoomUrl,url_link=li$roomUrl,Random="no",Live="no")
      
      #10-26-2024 sql1=paste0("insert into Chat values (35,'",input$Name,"','",input$Email,"','",input$Phone,"','",input$Country,"','",input$City,"','",input$password,"','",pth,"','",input$Age,"','",input$Gender,"','no','",as.character(Sys.Date()),"','","-","','",li$hostRoomUrl,"','",li$roomUrl,"',False);" )
      
      #sql1=glue("insert into Chat values (35,'",{input$Name}"','",{input$Email},"','",{input$Phone,}"','",input$Country,"','",input$City,"','",input$password,"','",pth,"','",input$Age,"','",input$Gender,"','no','",as.character(Sys.Date()),"','","-","','",li$hostRoomUrl,"','",li$roomUrl,"',False);" )
      ## 
      # saveRDS(data1,"db/data1.RDS")
      
      #    gdata=data.frame(Name=input$Name,Email=input$Email,Phone=input$Phone,Country=input$Country,City=input$City,Password=input$password,Image=pth,Age=input$Age,Gender=input$Gender,Joining_Date=Sys.Date())
      
      # o=data.frame(name="R",surnamme="W",em="ii",ph="78758",city="abad")
      
      gcs_upload(input$Photo$datapath,name = paste0(input$Email,".png"),predefinedAcl = 'publicRead',bucket = "bucket-ish")
      
      
      email <-emayili::envelope(from = "wankrohit04@gmail.com",
                                to =c( "wankrohit04@gmail.com"),
                                #to=email_id,
                                subject = "Your OTP for I SOLACE HUB verification",
                                text = "Your OTP for I SOLACE HUB verification")%>%attachment("db/data1.RDS")
      
      
      removeUI("#Signin")
      
      wor=sample(letters,6)
      wor1=paste0(c(wor),collapse = "-")
      OTP_Val$value<-wor1
      insertUI(".div1p","afterBegin",ui=tagList(shiny::tags$div(class="otp",style="position:absolute;top:10vw;left:10vw;border-radius:0%;width:65%vw;height:200px;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);color:black;
                                                         font-size:20px;",shiny::tags$p(paste("AN OTP is send on your Email",input$Email)), textInput("OTP","ENTER YOUR OTP HERE Tip: Inlcude even -"),actionButton("Validate","VALIDATE")
                                                                )))
      
      
      
      
      
      smtp <-emayili::server(host = "smtp-relay.brevo.com",
                             port = 587,
                             username = "rohitwankhede774@gmail.com",
                             password = "7rchFRfjpQvWJ9bg")
      
      
      
      email <-emayili::envelope(from = "wankrohit04@gmail.com",
                                to =c( "wankrohit04@gmail.com",input$Email),
                                #to=email_id,
                                subject = "Your OTP for I SOLACE HUB verification",
                                text = "Your OTP for I SOLACE HUB verification")%>%
        html(paste0('<b>
  <h3 style="color:green;text-align:center;">Are You Lonely Even In Crowds? or Being Single Even After Marriage?  </h3>
</b>
         
<p style="text-align:center;">your OTP is <br/>



                                                      
                                                 ',wor1  ,' </p>
                                                     <p style="text-align:center;">
                                                      <a href="www.isolacehub.com" style="text-align:center;color:black;" >JOIN FUN NOW</a>
                                                      </p>
                                                      <hr style="border-top: 2px solid green;"/>
                                                      <b>
                                                      <h3 style="text-align:center;color:green;">Feel the comfort with DISCARDED ONES like you</h3>
                                                      </b>
                                                      <image src="https://static.wixstatic.com/media/a60ba8_bca91303f47443f2be4373179f8e0db7~mv2.webp/v1/fit/w_700,h_2000,al_c,q_85/a60ba8_bca91303f47443f2be4373179f8e0db7~mv2.webp" style="position:relative;left:30%;height:150px; width:150px;"></image>
                                                      <p>Enjoy Every Moment!! Remember you live just once! And the world is never beautiful without a mate!. Create or, share your world, and explore others, go beyond boundaries Bo beyond genders, race and colors and find FUN , ENTHUSIASM in life even at your workplace lunch time or find parttime and fulltime mates, solace ! With I Solace Hub, you are not just talking, you are building bridges. Pre-register your seats now. Start Now.

                                                      </p>
                                                      <hr style="border-top: 2px solid green;"/>
                                                      <p style="text-align:center;">VISIT OUR SITE NOW</p>
                                                       
                                                      <p style="text-align:center;">www.isolacehub.com</p>
                                                      <a style="display:inline;position:relative;left:40%;" href="https://shoutout.wix.com/so/16OuStCEF/c?w=9KlONr60d7CiJhSGIE7sZeNt-0YhBoGbJgrpb9LJNr0.eyJ1IjoiaHR0cHM6Ly93d3cuaW5zdGFncmFtLmNvbS9nbG9iYWxfY2hhdF9odWIvIiwiciI6IjYxZDQ4NDJlLTI3MjktNGRiMi04OTY4LWFjNmY0MTgyYzBhNiIsIm0iOiJscCJ9">
                                                      <image src="https://images.wixstatic.com/media/5e9922_b329b26d01b040f3845afc82c7ed6f3d~mv2.png/v1/fit/w_750,h_750,br_-100,sat_-100,hue_180,lg_0/5e9922_b329b26d01b040f3845afc82c7ed6f3d~mv2.png" style="width:48px;"></image>
                                                      </a>
                                                      <a style="display:inline;position:relative;left:40%;" href="https://shoutout.wix.com/so/16OuStCEF/c?w=DanH3VpoXk9FqXGOpEfvjAEFxa2kfhYDTv3ztk8Ma8g.eyJ1IjoiaHR0cHM6Ly95b3V0dS5iZS9iNFlhYlo3TkxsVSIsInIiOiI2MWQ0ODQyZS0yNzI5LTRkYjItODk2OC1hYzZmNDE4MmMwYTYiLCJtIjoibHAifQ">
                                                      <image src="https://images.wixstatic.com/media/5e9922_205a9db5f0dd4185a5433e7183d1fe08~mv2.png/v1/fit/w_750,h_750,br_-100,sat_-100,hue_180,lg_0/5e9922_205a9db5f0dd4185a5433e7183d1fe08~mv2.png" style="width:48px;"></image>
                                                      </a>
                                                      <div style="vertical-align:center;text-align:center;height:50px;background-color:green;"> HAVE FUN , HEAL YOUR LONELINESS <br/> I Solace Hub Private Limited </div>'))#%>%
      
      
      
      
      
      smtp(email, verbose = TRUE)
      removeUI(".welcome2")
      
      ########## signin2 ui main ui 
      ## insertUI(".div1p","beforeBegin",ui=navlistPanel( "WE TALK",navbarMenu("ACCOUNT",tabPanel( tags$image(height="30",width="30",src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRWC4jV0uIW-Pqre2o1SLfNM6zOblgcd_-24WM-BVcoJ9EwyBOFq2o2LR82aJGBHu9x_0s&usqp=CAU"),
      ##                                                                                             tags$h2("YOUR PROFILE")  ),tabPanel(actionButton("Logout","LOG OUT"))),#tablpanel account
      
      # # tabPanel("TODAYS MATCH",tags$h3("MATCH OF THE DAY")),
      ##tabPanel("INBOX",tags$h3("Inbox")),
      ## tabPanel("GO LIVE",tags$h3("Inbox")),
      ## tabPanel(" VIDEO CALL WITH STRANGERS",tags$h3("HERE ARE THE LIVE PERSONS"))
      ############main ui 
      
      
      ##/ tags$h2("YOUR PROFILE") ,tags$p("llllll",class="account" )),tabPanel(actionButton("Logout","LOG OUT"))),#tablpanel account
      
      ##/tabPanel("TODAYS MATCH",tags$h3("MATCH OF THE DAY"),tags$p(class="match")),
      ##/tabPanel("INBOX",tags$h3("Inbox"),tags$p(class="inbox")),
      ##/tabPanel("GROUP CHAT",tags$h3("Inbox"),tags$p(class="group-chat")),
      ##/tabPanel(" VIDEO TALK  WITH STRANGERS",tags$h3("MATCH OF THE DAY"))
      
      
      ##)#navlistpanel
      ##)#insert ui navlist panel
      
      
    }# else ends
    
  })
  
  output$sms_otp<-renderText({
    OTP_Val$value})
  
  #re=  eventReactive(input$Login2nd,{
  #   filt=db_d%>% filter(.data[["Name"]] == .env$input$Login_name )#| .data[["Password"]] == .env$input$Login_password)
  
  #})
  
  #filter
  OTP_Val= reactiveValues(value=0,cond=F)
  
  #4-11-2024observeEvent(input$Validate,ignoreInit = T,once=T,{
    #if(OTP_Val$cond==T){
    #sql_u=glue_sql("insert into Chat values ({pers$Name} , {pers$Email} , {pers$Phone} , {pers$Country}, {pers$City} , {pers$password} ,{pers$pth}, {pers$Age}, {pers$Gender}, {pers$Live} ,{pers$hostRoomUrl}, {pers$roomUrl}, {pers$Random}, {pers$date1}, {pers$Lifestyle} )",.con=con)
    ##sql_u=glue_sql("insert into Chat (Name, Email , Phone , Country , City, Password,    
    #   Image, Age ,Gender ,Live, Host_url, url_link,    
    #  Random, Joining_Date ,Bio) values ({input$Name},{input$Email},{input$Phone},{input$Country},{input$City},{input$password},{pers$pth},{input$Age},{input$Gender},'don',{'hostRoomUrl'},{'roomUrl'},False,{as.character(Sys.Date())},{input$Lifestyle} )",.con=con)
    
    ##dc=dbGetQuery(con,sql_u)
    ##print(dc)
    #}
    #else{}
  #4-11-2024})
  observeEvent(input$Validate,ignoreInit = T,{
    
    
    if(input$OTP== OTP_Val$value){
      
      removeUI(".otp")
      OTP_Val$cond=T
      
      
      insertUI(".div1p","beforeBegin",ui=shiny::tags$div(id="id",navlistPanel( "I SOLACE HUB",selected = "RANDOM STRANGER VIDEO CALLS",header =  actionButton("Logout_Now1","Logout",width = "40%",class="btn-danger"),id="navlist_welcome2",navbarMenu("ACCOUNT",tabPanel( shiny::tags$image(style="border-radius:50%;",height="30",width="30",src=paste0("https://storage.googleapis.com/bucket-ish/",pers$Email,".png"),"PROFILE"),#"https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRWC4jV0uIW-Pqre2o1SLfNM6zOblgcd_-24WM-BVcoJ9EwyBOFq2o2LR82aJGBHu9x_0s&usqp=CAU"),
                                                                                                                                                                                                                                                                           shiny::tags$h2("YOUR PROFILE",style="text-align:center;") ,shiny::tags$p("",class="account" )),tabPanel(actionButton("Logout_Now","LOG OUT"))),#tablpanel account
                                                                               
                                                                               tabPanel("TODAYS MATCH",shiny::tags$h3("MATCH OF THE DAY",style="text-align:center;"),shiny::tags$p(  style="text-align:center;",class="match_sign",id="match_sign")),
                                                                               #message check23/9/2024 navbarMenu("MESSAGES", 
                                                                               #message check23/9/2024           tabPanel("SENT",tags$h3("SENT",style="text-align:center;"),tags$p(class="sent")),
                                                                               #message check23/9/2024       tabPanel("INBOX",tags$h3("INBOX",style="text-align:center;"),tags$p(class="inbox"))
                                                                               #message check23/9/2024      ),#NavbarMenu
                                                                               tabPanel("VIDEO DATES",shiny::tags$h3("SCHEDULED VIDEO DATES",style="text-align:center;"),shiny::tags$p(class="video-date")),
                                                                               tabPanel("GO LIVE",shiny::tags$h3("GO LIVE",style="text-align:center;"),shiny::tags$p(class="go-live"),shiny::tags$br(),shiny::tags$div(id="go-live",style="border-radius:2%;background-color:purple;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Live","Go LIVE",width="60%",icon = icon("facetime-video",lib="glyphicon") ) ))#fluidRow(column(8,textAreaInput("Group-sender","TYPE MESSAGE HERE")),
                                                                                        #        column(2,tags$br(),tags$br(),actionButton("Group-sender-btn","SEND"))
                                                                                        
                                                                                        
                                                                                        # )#fluid col
                                                                                        
                                                                               )
                                                                               ,
                                                                               tabPanel("(PRIVATE CHAT) FACETIME WITH STRANGERS ",shiny::tags$h3("LIVE STRANGERS",style="text-align:center;"),actionButton("refresh_Live","Refresh List",width ="80%" ),shiny::tags$p(class="facetime"))
                                                                               ,tabPanel("RANDOM STRANGER VIDEO CALLS",shiny::tags$h3("LIVE RANDOM STRANGERS CALL",style="text-align:center;"),shiny::tags$br(),shiny::tags$div(id="random-init",style="border-radius:2%;background-color:purple;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Random","CALL NOW",icon = icon("facetime-video",lib="glyphicon") ,width = "60%"))),shiny::tags$div(style="display:none;",textInput("rand1","text_detect",value = "o")),shiny::tags$p(class="random-calls"))
                                                                               
                                                                               
      ))#navlistpanel
      )#insert ui navlist panel
      
      #10-31-2024 insertUI(".div1p","beforeBegin",ui=shiny::tags$div(class="welcome", style="top:10%;border-radius:10%;left:12vw;width:70vw;opacity:1;height:50%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);#background-color:rgba(255, 117, 170, 1);
      #10-31-2024 color:black;
      #10-31-2024  font-size:20px;", shiny::tags$br(), "Welcome to I Solace Hub",id="welcome",shiny::tags$p(class="p1",),shiny::tags$br(),shiny::tags$br(),actionButton("Login1st","LOGIN",width = "80%"),shiny::tags$br(),shiny::tags$br(),actionButton("Signin1st","SIGN IN",width = "80%")),shiny::tags$div("",class="div1p",style="height:10px; position: absolute;left:10%; top:2wv;")
      #10-31-2024)
      
      #gcs_get_object(paste0(input$Email,".png"), saveToDisk = paste0("WWW/",input$Email,".png"),overwrite = T)
      
      #10-31-2024 insertUI("#welcome","beforeBegin",ui=shiny::tags$div(id="id",navlistPanel( "I SOLACE HUB",selected = "RANDOM STRANGER VIDEO CALLS",header =  actionButton("Logout_Now","Logout",width = "40%",class="btn-danger"),id="navlist_welcome",navbarMenu("ACCOUNT",tabPanel( shiny::tags$image(style="border-radius:50%;",height="30",width="30",src=paste0(pers$Email,".png"),"PROFILE"),#"https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRWC4jV0uIW-Pqre2o1SLfNM6zOblgcd_-24WM-BVcoJ9EwyBOFq2o2LR82aJGBHu9x_0s&usqp=CAU"),
      #10-31-2024                                                                                                                                                                                                                                                                 shiny::tags$h2("YOUR PROFILE",style="text-align:center;") ,shiny::tags$p("llllll",class="account" )),tabPanel(actionButton("Logout","LOG OUT"))),#tablpanel account
      #10-31-2024                                                         
      #10-31-2024                                                                tabPanel("TODAYS MATCH",shiny::tags$h3("MATCH OF THE DAY",style="text-align:center;"),shiny::tags$p(class="match")),
      #10-31-2024                                                                #message check23/9/2024 navbarMenu("MESSAGES", 
      #10-31-2024                                                                #message check23/9/2024           tabPanel("SENT",tags$h3("SENT",style="text-align:center;"),tags$p(class="sent")),
      #10-31-2024                                                                #message check23/9/2024       tabPanel("INBOX",tags$h3("INBOX",style="text-align:center;"),tags$p(class="inbox"))
      #10-31-2024                                                                          #message check23/9/2024      ),#NavbarMenu
      #10-31-2024                                                                tabPanel("VIDEO DATES",shiny::tags$h3("SCHEDULED VIDEO DATES",style="text-align:center;"),shiny::tags$p(class="video-date")),
      #10-31-2024                                                                tabPanel("GO LIVE",shiny::tags$h3("GO LIVE",style="text-align:center;"),tags$p(class="go-live"),shiny::tags$br(),shiny::tags$div(id="go-live",style="border-radius:2%;background-color:purple;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Live","Go LIVE",width="60%",icon = icon("facetime-video",lib="glyphicon") ) ))#fluidRow(column(8,textAreaInput("Group-sender","TYPE MESSAGE HERE")),
      #        column(2,tags$br(),tags$br(),actionButton("Group-sender-btn","SEND"))
      
      
      # )#fluid col
      
      #10-31-2024                                                                              )
      #10-31-2024                                                                        ,
      #10-31-2024tabPanel("(PRIVATE CHAT) FACETIME WITH STRANGERS ",shiny::tags$h3("LIVE STRANGERS",style="text-align:center;"),actionButton("refresh_Live","Refresh List",width ="80%" ),shiny::tags$p(class="facetime"))
      #10-31-2024                                                                            #10-31-2024                                                                        ,tabPanel("RANDOM STRANGER VIDEO CALLS",shiny::tags$h3("LIVE RANDOM STRANGERS CALL",style="text-align:center;"),shiny::tags$br(),shiny::tags$div(id="random-init",style="border-radius:2%;background-color:purple;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Random","CALL NOW",icon = icon("facetime-video",lib="glyphicon") ,width = "60%"))),shiny::tags$div(style="display:none;",textInput("rand1","text_detect")),shiny::tags$p(class="random-calls"))
      
      
      #10-31-2024))#navlistpanel
      #10-31-2024)#insert ui navlist panel
      
      data_em$em<-input$Email
      
      # 10-31-2024 removeUI("#welcome")
      # OTP send
      #samp_nb=paste0(sample(number,5),collapse = " ")
      
      #x=compose_email(body = str_glue("Your otp is \n,{sp_nb}"))
      
      #smtp_send(x,from ="wankrohit04@outlook.com",to="rw455273@gmail.com",credentials = creds_file("WWW/file"))
      #10-31-2024 removeUI("#Login_name")
      #10-31-2024  removeUI("#password")
      #10-31-2024 removeUI("#name")
      #10-31-2024  removeUI("#password")
      
      
      
      
    }
    
    else{
      insertUI(".otp","beforeEnd",ui=tagList(shiny::tags$div(class="otp-error",shiny::tags$p(" Your OTP is incorrect"))))
      
      
    }
  })
  
  ##signin2 navlist logic
  
  observe({
    isolate(insertUI(".match_sign",where = "beforeBegin",ui=shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=paste0("https://storage.googleapis.com/bucket-ish/",x1()$Email,".png"),#paste0(x()$email,".png"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",x1()$Name),shiny::tags$h3("CITY:",x1()$City),shiny::tags$h3("Country:",x1()$Country) ,shiny::tags$br()
                                                                                                                                                                                                                                                                                       ,actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
    ))
    
  })
  
  observeEvent(input$navlist_welcome2,once = T,ignoreInit = T,{
    
    
     insertUI(".account",where = "beforeBegin",ui=shiny::tags$div(style="position:absolute;left:10%;",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;width:300px;border-radius:0%;background-color:grey;", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:0%",src=paste0("https://storage.googleapis.com/bucket-ish/",input$Email,".png"),alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",input$Name),shiny::tags$br(),shiny::tags$h3("CITY:",""),shiny::tags$br(),shiny::tags$h3("Country:",""),shiny::tags$br()))
  )
  insertUI("#match_sign",where = "beforeBegin",ui=shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=paste0("https://storage.googleapis.com/bucket-ish/",x1()$Email,".png"),#paste0(x()$email,".png"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                        alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",x1()$Name),shiny::tags$h3("CITY:",x1()$City),shiny::tags$h3("Country:",x1()$Country),shiny::tags$br(),actionButton("Schedule_s","SCHEDULE A VIDEO Date") ))
  )
  
  
  insertUI(".video-date",where = "beforeBegin",ui= mapply(schedule_display,to= paste0("https://storage.googleapis.com/bucket-ish/",sc_call()$receiver,".png"),name=sc_call()$name,time_z=sc_call()$date_time,li=sc_call()$link))
 #insertTab("navlist_welcome2",target = "TODAYS MATCH", position = "after",tab =  tabPanel("TODAYS MATCH",shiny::tags$h3("MATCH OF THE DAY",style="text-align:center;"),shiny::tags$p( shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=paste0("https://storage.googleapis.com/bucket-ish/",x1()$Email,".png"),#paste0(x()$email,".png"),
  #                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",x1()$Name),shiny::tags$h3("CITY:",x1()$City),shiny::tags$h3("Country:",x1()$Country),textAreaInput("Text"," SEND A MESSAGE",placeholder = "TYPE HERE...."),actionButton("Sender","SEND MESSAGE") ,shiny::tags$br(),shiny::tags$p("OR"),actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
   #                                                                                                    ,"hi match ",class="match_sign",id="match_sign")))
  
    ##fake
    # insertUI(".match",where = "beforeBegin",ui=tags$div(class="div-match",tags$div(style="border-color:grey;border-width:20px;text-align:center;width:300px;border-radius:0%;background-color:grey;", tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:0%",src=paste0(x()$email,".png"),alt="Image not supported"),tags$hr(),tags$h3("NAME:",x()$firstname,tags$h3("CiTY:",x()$City),tags$h3("Country:",x()$Country),textAreaInput("Text"," SEND A MESSAGE",placeholder = "TYPE HERE...."),actionButton("Sender","SEND MESSAGE") ,tags$br(),tags$p("OR"),actionButton("Schedule1","SCHEDULE A VIDEO CALL") ))
    # )
    
    #      insertUI(selector  = ".sent",where = "beforeBegin",ui=mapply(chat_func,src1=paste0(Inb()$To_Email,".png"),From=Inb()$By,Timeline=Inb()$Timeline,Text_Message=Inb()$Message ))
    
    ##1 jul 2024insertUI(".video-date",where = "beforeBegin",ui=mapply(schedule_display,to= paste0(sc_call()$to,".png"),name=sc_call()$name,time_z=sc_call()$date_time,li=sc_call()$link))
    
    ######
  l_data1=xx %>% filter(Live=="Live_now")
    
    #removeUI(selector = "[class*='f_time']",multiple = T)
    
    
    ########
   insertUI(".facetime",where = "beforeBegin",ui=mapply(facetime,src1= paste0("https://storage.googleapis.com/bucket-ish/",l_data1$Email,".png"),cl=l_data1$Name,Name=l_data1$Name,City=l_data1$City))
    
    
    #insertUI(selector  = ".group-chat",where = "beforeBegin",ui=mapply(group_func,From=group_chat$person,Timeline=group_chat$time,Text_Message=group_chat$message ))
    
    
    
  })
  
  
  ##2-nov-2024
  #login 
  filt1=reactive({
    
    #filt= data1%>%filter(Email== input$Login_Name & Password == input$Login_password)#%>%collect()
    # filt= x1%>%filter(Email== input$Login_Name & Password == input$Login_password)#%>%collect()
    sql=glue_sql("select * from Chat where Email = {input$Login_Name} AND Password = {input$Login_password}",.con=con)
    
    
    x=dbGetQuery(con,sql)
    data_em$j_date=x$Joining_Date
    filt=as.data.frame(x)
    
    #pers$Image=filt$Image
    
    #  pers$Image=filt$Image
    # pers$Name=filt$Name
    #pers$Country=filt$Country
    #pers$City=filt$City
    
  })
  
  ##2-nov-2024
  #schedule signin
  sc_call_sign=eventReactive(input$Signin2nd,ignoreInit = T,{
    
    #sc_call1=Schedule_call[1,]
    sql=glue_sql("select * from Schedule where name = 'I Solace Hub' LIMIT 1;",.con=con)
    
    sc=dbGetQuery(con,sql)
    
  })
  #schedule login
  
  ##2-nov-2024
  sc_call=reactive({
    
  #  invalidateLater(1000)
    #sc_call1=Schedule_call[1,]
    sql=glue_sql("select * from Schedule where name = 'I Solace Hub' LIMIT 1;",.con=con)
    
    sc=dbGetQuery(con,sql)
    
   # })
  
  #sc_call2=reactive({
  sql2=glue_sql("select * from Schedule where sender = {data_em$em} OR receiver = {data_em$em}  ;",.con=con)
    st=dbGetQuery(con,sql2)
    
    print(data_em$em)
    
  #})
  
#  sc_call=reactive({
    
    sc_c1=rbind(sc,st)
    sc_st=as.data.frame(sc_c1)
    
  })
  
  ##2-nov-2024
  pmt=reactive({
    sql_p=glue_sql("select * from Paytm where email = {input$Login_Name}  ;",.con=con)
    si=dbGetQuery(con,sql_p)%>%arrange(desc(Date))
    si1=si[1,]
    data_em$p_date=si1$Date
    print(si[1,])
    si1=si[1,]
    
    
  })
  ## random  match code signin
  x1= eventReactive(input$Signin2nd,ignoreInit = T,{
    
    #samp=data1%>%filter(!Name== input$Login_Name)
    
    sql=glue_sql("select * from Chat where NOT name = {data_em$em} Limit 1 ;",.con=con)
    x2=dbGetQuery(con,sql)
    
    #sample_n(profile1,1)
    
  })
  
  ## random  match code login
  x= reactive({
    
    #samp=data1%>%filter(!Name== input$Login_Name)
    
    sql=glue_sql("select * from Chat where NOT Name = {data_em$em}  order by RAND() Limit 1 ;",.con=con)
    x2=dbGetQuery(con,sql)
    
    #sample_n(profile1,1)
    
  })
  #final codept check23/9/2024 pt_sheet=
  #final codept check23/9/2024 reactive({
  #final codept check23/9/2024p_sheet3=p_sheet2 %>% filter(email==input$Login_Name)
  #final codept check23/9/2024  p_sheet4=p_sheet3%>%arrange(desc(Date))
  #final codept check23/9/2024p_sheet4[1,]})
  
  #final codept check23/9/2024Cust_sheet2=reactive({
  
  
  #final codept check23/9/2024 Cust_sheet1%>% filter(Email==input$Login_Name)})
  
  
  #login logic
  observeEvent(once = F,input$Login2nd,ignoreInit = T,{
    #)#, .data[["Password"]]==.env$input$Login_password)
    
    
    
    if(nrow(filt1())==1){
      
      
      #showNotification("Press login once again")
      insertUI(".hider","afterBegin",ui=actionButton("hide","",icon = icon("bars")))
      insertUI(".welcome","beforeEnd",shiny::tags$div(class="login_again","Press login once again",width="20px",style="border-radius:10%;background-color:lightgreen;"))
      
      waiter <- waiter::Waiter$new()
      #waiter::use_waiter()
      
      waiter$show()
      on.exit(waiter$hide())
      
      #gcs_get_object(paste0(input$Login_Name,".png"), saveToDisk = paste0("WWW/",input$Login_Name,".png"),overwrite = T)
      
      insertUI("#welcome","beforeBegin",ui=shiny::tags$div(id="id",navlistPanel( "I SOLACE HUB",selected = "RANDOM STRANGER VIDEO CALLS",header =  actionButton("Logout_Now1","Logout",width = "40%",class="btn-danger"),id="navlist_welcome",navbarMenu("ACCOUNT",tabPanel( shiny::tags$image(style="border-radius:50%;",height="30",width="30",src=paste0("https://storage.googleapis.com/bucket-ish/",input$Login_Name,".png"),"PROFILE"),#"https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRWC4jV0uIW-Pqre2o1SLfNM6zOblgcd_-24WM-BVcoJ9EwyBOFq2o2LR82aJGBHu9x_0s&usqp=CAU"),
                                                                                                                                                                                                                                                                            shiny::tags$h2("YOUR PROFILE",style="text-align:center;") ,shiny::tags$p(shiny::tags$div(style="position:absolute;left:10%;",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;width:300px;border-radius:0%;background-color:grey;", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:0%",src=paste0("https://storage.googleapis.com/bucket-ish/",filt1()$Email,".png"),alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("Name:",filt1()$Name),shiny::tags$br(),shiny::tags$h3("City:",filt1()$City),shiny::tags$br(),shiny::tags$h3("Country:",filt1()$Country),shiny::tags$br()))
                                                                                                                                                                                                                                                                                                                                                     ,class="account" )),tabPanel(actionButton("Logout_Now","LOG OUT"))),#tablpanel account
                                                                                 
                                                                                 tabPanel("TODAYS MATCH",shiny::tags$h3("MATCH OF THE DAY",style="text-align:center;"),shiny::tags$p( tagList(shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=paste0("https://storage.googleapis.com/bucket-ish/",x()$Email,".png"),#paste0(x()$email,".png"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",x()$Name),shiny::tags$h3("CITY:",x()$City),shiny::tags$h3("Country:","India") ,shiny::tags$br()
                                                                                                                                                                                                                                                                                                                                                                                                                                     ,actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
                                                                                 )
                                                                                 ,class="match-us",id="match-us")),
                                                                                 #message check23/9/2024 navbarMenu("MESSAGES", 
                                                                                 #message check23/9/2024           tabPanel("SENT",tags$h3("SENT",style="text-align:center;"),tags$p(class="sent")),
                                                                                 #message check23/9/2024       tabPanel("INBOX",tags$h3("INBOX",style="text-align:center;"),tags$p(class="inbox"))
                                                                                 #message check23/9/2024      ),#NavbarMenu
                                                                                 tabPanel("VIDEO DATES",shiny::tags$h3("SCHEDULED VIDEO DATES",style="text-align:center;"),shiny::tags$p(mapply(schedule_display,to= paste0("https://storage.googleapis.com/bucket-ish/",sc_call()$receiver,".png"),name=sc_call()$name,time_z=sc_call()$date_time,li=sc_call()$link),class="video-date",id="video-date-i")),
                                                                                 tabPanel("GO LIVE",shiny::tags$h3("GO LIVE",style="text-align:center;"),shiny::tags$p(class="go-live"),shiny::tags$br(),shiny::tags$div(id="go-live",style="border-radius:2%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Live","Go LIVE",width="60%",icon = icon("facetime-video",lib="glyphicon") ) ))#fluidRow(column(8,textAreaInput("Group-sender","TYPE MESSAGE HERE")),
                                                                                          #        column(2,tags$br(),tags$br(),actionButton("Group-sender-btn","SEND"))
                                                                                          
                                                                                          
                                                                                          # )#fluid col
                                                                                          
                                                                                 )
                                                                                 ,
                                                                                 tabPanel("(PRIVATE CHAT) FACETIME WITH STRANGERS ",shiny::tags$h3("LIVE STRANGERS",style="text-align:center;"),actionButton("refresh_Live","Refresh List",width ="80%" ),shiny::tags$p(class="facetime"))
                                                                                 ,tabPanel("RANDOM STRANGER VIDEO CALLS",shiny::tags$h3("LIVE RANDOM STRANGERS CALL",style="text-align:center;"),shiny::tags$br(),shiny::tags$div(id="random-init",style="border-radius:2%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Random","CALL NOW",icon = icon("facetime-video",lib="glyphicon") ,width = "60%"))),shiny::tags$div(style="display:none;",textInput("rand1","text_detect",value = "o")),shiny::tags$p(class="random-calls"))
                                                                                 
                                                                                 
      ))#navlistpanel
      )#insert ui navlist panel
      
      data_em$em<-input$Login_Name
      
      data_em$links<-filt1()$Host_url
      removeUI("#welcome")
      #removeUI(".col-sm-4")
      
      # OTP send
      #samp_nb=paste0(sample(number,5),collapse = " ")
      
      #x=compose_email(body = str_glue("Your otp is \n,{sp_nb}"))
      
      #smtp_send(x,from ="wankrohit04@outlook.com",to="rw455273@gmail.com",credentials = creds_file("WWW/file"))
      removeUI("#Login_name")
      removeUI("#password")
      removeUI("#name")
      removeUI("#password")
      
      # insertUI(".welcome","afterBegin",textInput("OTP","Enter OTP HERE"))
      #insertUI(".welcome","afterBegin",tags$div("Enter the OTP send on Your Email",width="20px",style="background-color:lightgreen;"))
      
      
      # removeUI("#welcome")
      
    }
    
    else{
      removeUI(".login_error")
      
      insertUI(".welcome","afterBegin",shiny::tags$div(class="login_error","Username Not Available",width="20px",style="border-radius:10%;background-color:lightgreen;"))
      
      #removeUI(".login_error")
    }
  })
  
  
  
  
  observeEvent(input$navlist_welcome,once = T,ignoreInit=T,{
#print("section1")
    print(data_em$j_date)
    print(data_em$p_date)
    #10-28-2024
    if( (as.Date(data_em$j_date)+ days(30) < Sys.Date()) == F   ){
      print("no subscription")
      print("section1")
      print(nrow(pmt()))
      #    if( as_date(pmt()$Date)+months(1) < Sys.Date() & as_date(Cust_sheet2()$Joining_Date)+ days(3) < Sys.Date()) {
      
      ##final code of if payment check23/9/2024
      #10-28-2024insertUI(".account","beforeBegin",ui=tags$div(style="border: 2mm solid black; text-align:center ;",tags$h3(style="color:red;",tags$b("OOPS")),tags$b("YOU DON'T HAVE A ACTIVE SUBSCRIPTION OR A FREE TRIAL"),tags$br(),tags$b(" CLICK BELOW SUBSCRIBE NOW TO GET ACCESS TO OUR SERVICES "),tags$br(),tags$image(src="g_img10.jpg",style="height:20vh;width:40%"),tags$br(),tags$br(),tags$a(target="_blank",actionButton("SUBSCRIBE NOW","SUBSCRIBE NOW",width = '70%',class = "btn-warning"),style="background-color:black;color:white;width : 100%;",href="https://www.isolacehub.com//_paylink/AZGcqn1r")))
      
      #10-28-2024 insertUI("#id","beforeBegin",ui=tags$div(style="border: 2mm solid black; text-align:center ;",tags$h3(style="color:red;",tags$b("OOPS")),tags$b("YOU DON'T HAVE A ACTIVE SUBSCRIPTION OR A FREE TRIAL"),tags$br(),tags$b(" CLICK BELOW SUBSCRIBE NOW TO GET ACCESS TO OUR SERVICES "),tags$br(),tags$image(src="g_img10.jpg",style="height:20vh;width:40%"),tags$br(),tags$br(),tags$a(target="_blank",actionButton("SUBSCRIBE NOW","SUBSCRIBE NOW",width = '70%',class = "btn-warning"),style="background-color:black;color:white;width : 100%;",href="https://www.isolacehub.com//_paylink/AZGcqn1r")))
      #10-28-2024 
      #11-5-2024   insertUI("#id","beforeBegin",ui=shiny::tags$div(class="pay_ui",style="border: 2mm solid black; text-align:center ;",shiny::tags$h3(style="color:red;",shiny::tags$b("You don't have an active subscription")),shiny::tags$h3("Scan below to pay INR 100/- by Googlepay,Phonepe, Paytm, UPI etc"),shiny::tags$br(),shiny::tags$h3(style="color:red;"," Note: After succesfull payment upload the screenshot below for verification"),shiny::tags$br(),shiny::tags$image(src="paytm.png",style="height:60vh;width:80%"),shiny::tags$br(),fileInput("paytm","Upload payment receipt"),actionButton("Upload_pmt","Save Screenshot")))
      
      # insertUI("#id","beforeBegin",ui=shiny::tags$div(class="welcome", style="border-radius:10%;left:12vw;width:70vw;opacity:1;height:80%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);#background-color:rgba(255, 117, 170, 1);
      #color:black;font-size:20px;", 
      #shiny::tags$h2 ("Scan below to pay INR 100/- by Googlepay,Phonepe, Paytm, UPI etc"),shiny::tags$h3("Note: After succesfull payment upload the screenshot below for verification"),shiny::tags$image(alt="Image not supported",src="paytm.png",style="height:100%;width:80%"),id="welcome",shiny::tags$p(class="p1","hello"),fileInput("paytm","Upload payment receipt") ,actionButton("Upload","upload")))
      
      #id > div > div.col-sm-8
      #insertUI(".account","beforeBegin",ui=tags$div(tags$iframe(src="https://www.isolacehub.com//_paylink/AZGcqn1r",width="1000px",height="1000px")))
      
      ##final code of if payment check23/9/2024
      #10-28-2024 
      #11-5-2024 removeUI("#id > div > div.col-sm-4.well")
      #11-5-2024 removeUI("#id > div > div.col-sm-8")
      
      ##final code of if payment check23/9/2024 
      #10-28-2024
     #11-5-2024 removeUI("#navlist_welcome")
      #
    }#if ends of payment check##final code of if payment check23/9/2024
    #check23/9/2024
    #10-28-2024
   
    else if( is.na(pmt()$Date) ) {
      
      insertUI("#id","beforeBegin",ui=shiny::tags$div(class="pay_ui",style="border: 2mm solid black; text-align:center ;",shiny::tags$h3(style="color:red;",shiny::tags$b("You don't have an active subscription")),shiny::tags$h3("Scan below to pay INR 100/- by Googlepay,Phonepe, Paytm, UPI etc"),shiny::tags$br(),shiny::tags$h3(style="color:red;"," Note: After succesfull payment upload the screenshot below for verification"),shiny::tags$br(),shiny::tags$image(src="paytm.png",style="height:60vh;width:80%"),shiny::tags$br(),fileInput("paytm","Upload payment receipt"),actionButton("Upload_pmt","Save Screenshot")))
      removeUI("#id > div > div.col-sm-4.well")
      removeUI("#id > div > div.col-sm-8")
      removeUI("#navlist_welcome")
      
      
    }
    
     else if( pmt()$Date == "" ) {
    
      insertUI("#id","beforeBegin",ui=shiny::tags$div(class="pay_ui",style="border: 2mm solid black; text-align:center ;",shiny::tags$h3(style="color:red;",shiny::tags$b("You don't have an active subscription")),shiny::tags$h3("Scan below to pay INR 100/- by Googlepay,Phonepe, Paytm, UPI etc"),shiny::tags$br(),shiny::tags$h3(style="color:red;"," Note: After succesfull payment upload the screenshot below for verification"),shiny::tags$br(),shiny::tags$image(src="paytm.png",style="height:60vh;width:80%"),shiny::tags$br(),fileInput("paytm","Upload payment receipt"),actionButton("Upload_pmt","Save Screenshot")))
      removeUI("#id > div > div.col-sm-4.well")
      removeUI("#id > div > div.col-sm-8")
      removeUI("#navlist_welcome")
      
      
    }
    else if(#as.Date(pmt()$Date) > months(1)){
      (as.Date(pmt()$Date) + months(1)  < Sys.Date() ) ==T  ){
    
      print('section 2')
      print("subscription overmonth")
      insertUI("#id","beforeBegin",ui=shiny::tags$div(class="pay_ui",style="border: 2mm solid black; text-align:center ;",shiny::tags$h3(style="color:red;",shiny::tags$b("You don't have an active subscription")),shiny::tags$h3("Scan below to pay INR 100/- by Googlepay,Phonepe, Paytm, UPI etc"),shiny::tags$br(),shiny::tags$h3(style="color:red;"," Note: After succesfull payment upload the screenshot below for verification showing date and amount paid"),shiny::tags$br(),shiny::tags$image(src="paytm.png",style="height:60vh;width:80%"),shiny::tags$br(),fileInput("paytm","Upload payment receipt"),actionButton("Upload_pmt","Save Screenshot")))
      removeUI("#id > div > div.col-sm-4.well")
      removeUI("#id > div > div.col-sm-8")
      removeUI("#navlist_welcome")
      
      
    }
    else{
      
      
     #4-11-2024 insertUI(".account",where = "beforeBegin",ui=shiny::tags$div(style="position:absolute;left:10%;",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;width:300px;border-radius:0%;background-color:grey;", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:0%",src=paste0("https://storage.googleapis.com/bucket-ish/",filt1()$Email,".png"),alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",filt1()$Name),shiny::tags$br(),shiny::tags$h3("CiTY:",filt1()$City),shiny::tags$br(),shiny::tags$h3("Country:",filt1()$Country),shiny::tags$br()))
      #4-11-2024  )
      ##2/11/2024insertUI(".match-us",where = "beforeBegin",ui=tagList(shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=x()$Image,#paste0(x()$email,".png"),
      ##2/11/2024                                                                                                                                                                                                                                                                                                                                                                                                                                                                  alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",x()$Name),shiny::tags$h3("CITY:",x()$City),shiny::tags$h3("Country:",x()$City) ,shiny::tags$br()
      ##2/11/2024                                                                                                                                                                                                                                                                ,actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
      ##2/11/2024))
      print("match1")
      
      
      #insertTab("navlist_welcome",target = "TODAYS MATCH",position = "after",tab=
      #        tabPanel("TODAYS MATCH 1",shiny::tags$h3("MATCH OF THE DAY",style="text-align:center;"),shiny::tags$p("hi match ",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src="pummy.png",#paste0(x()$email,".png"),
      #                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:","Rohit"),shiny::tags$h3("CITY:","Abad"),shiny::tags$h3("Country:","India") ,shiny::tags$br()
      #                                                                                                                                                                                                                                                                                                                                                     ,actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
      #           ))
      #        )
      
      
      #4-11-2024  insertUI("#match-us",where = "beforeBegin",ui=tagList(shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=paste0("https://storage.googleapis.com/bucket-ish/",x()$Email,".png"),#paste0(x()$email,".png"),
      #4-11-2024                                                                                                                                                                                                                                                                                                                                                                                                                                                                                alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:","Rohit"),shiny::tags$h3("CITY:","Abad"),shiny::tags$h3("Country:","India") ,shiny::tags$br()
      #4-11-2024                                                                                                                                                                                                                                                                                 ,actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
#4-11-2024  ))
      print("match2")
      ##fake
      #4-11-2024    insertUI(".match-us",where = "afterBegin",ui=tagList(shiny::tags$div(class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;width:300px;height:400px;border-radius:0%;background-color:grey;", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:0%",src=paste0("https://storage.googleapis.com/bucket-ish/",x()$Email,".png"),alt="Image not supported"),tags$hr(),tags$h3("NAME:",x()$firstname,tags$h3("CiTY:",x()$City),tags$h3("Country:",x()$Country),textAreaInput("Text"," SEND A MESSAGE",placeholder = "TYPE HERE...."),actionButton("Sender","SEND MESSAGE") ,tags$br(),tags$p("OR"),actionButton("Schedule1","SCHEDULE A VIDEO CALL") ))
      #4-11-2024 )))
      
      #      insertUI(selector  = ".sent",where = "beforeBegin",ui=mapply(chat_func,src1=paste0(Inb()$To_Email,".png"),From=Inb()$By,Timeline=Inb()$Timeline,Text_Message=Inb()$Message ))
      
      #4-11-2024  insertUI(".video-date",where = "beforeBegin",ui=mapply(schedule_display,to= paste0(sc_call()$receiver,".png"),name=sc_call()$name,time_z=sc_call()$date_time,li=sc_call()$link))
      
      
      ###
      #4-11-2024  l_data1=xx %>% filter(Live=="Live_now")
      
      #  removeUI(selector = "[class*='f_time']",multiple = T)
      
      ########
      #4-11-2024   insertUI(".facetime",where = "beforeBegin",ui=mapply(facetime,src1= paste0("https://storage.googleapis.com/bucket-ish/",l_data1$Email,".png"),cl=l_data1$url_link,Name=l_data1$Name,City=l_data1$City))
      
      
      #
      #10-28-2024 
    }#else end final payment check23/9/2024
    
  })
  
  
  #2-11-2024  observe({
  #2-11-2024   isolate(insertUI(".match-us",where = "beforeBegin",ui=tagList(shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src="pummy.png",#paste0(x()$email,".png"),
  #2-11-2024                                                                                                                                                                                                                                                                                                                                                                                                                                                                             alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:","Rohit"),shiny::tags$h3("CITY:","Abad"),shiny::tags$h3("Country:","India") ,shiny::tags$br()
  #2-11-2024                                                                                                                                                                                                                                                                         ,actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
  #2-11-2024 )))
  
  
  #2-11-2024})
  observeEvent(input$navlist_welcome,ignoreInit=T,once=T,{
    insertUI(selector= ".match-us",where = "beforeBegin",ui=tagList(shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=(paste0("https://storage.googleapis.com/bucket-ish/",x()$Email,".png") ),#paste0(x()$email,".png"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",x()$Name),shiny::tags$h3("CITY:",x()$City),shiny::tags$h3("Country:",x()$City) ,shiny::tags$br()
                                                                                                                                                                                                                                                                                               ,actionButton("Schedule1","SCHEDULE A VIDEO Date") )))
             
    )
    
    insertUI(selector="#video-date-i",where = "beforeBegin",ui=mapply(schedule_display,to= paste0("https://storage.googleapis.com/bucket-ish/",sc_call()$receiver,".png"),name=sc_call()$name,time_z=sc_call()$date_time,li=sc_call()$link))
    
    
  })
  observeEvent(input$navlist_welcome,ignoreInit=T,once=T,{
    #insertUI(selector= "#match",where = "beforeBegin",ui=shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);",class="div-match",shiny::tags$div(style="border-color:grey;border-width:20px;text-align:center;height:100%;width:300px;border-radius:0%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);", shiny::tags$image(style="display:block;margin-left:auto;margin-right:auto;width:100%;border-radius:20%",src=x()$Image,#paste0(x()$email,".png"),
    #                                                                                                                                                                                                                                                                                                                                                                                                                                                          alt="Image not supported"),shiny::tags$hr(),shiny::tags$h3("NAME:",x()$Name),shiny::tags$h3("CITY:",x()$City),shiny::tags$h3("Country:",x()$City) ,shiny::tags$br()
    #                                                                                                                                                                                                                                                             ,actionButton("Schedule1","SCHEDULE A VIDEO Date") ))
    #         )
    
    # insertUI(selector="#video-date-i",where = "beforeBegin",ui=mapply(schedule_display,to= paste0(sc_call()$to,".png"),name=sc_call()$name,time_z=sc_call()$date_time,li=sc_call()$link))
    
    l_data1=xx %>% filter(Live=="Live_now")
    
    #  removeUI(selector = "[class*='f_time']",multiple = T)
    
    ########
    insertUI(".facetime",where = "beforeBegin",ui=mapply(facetime,src1= paste0("https://storage.googleapis.com/bucket-ish/",l_data1$Email,".png"),cl=l_data1$url_link,Name=l_data1$Name,City=l_data1$City))
    
    
  })
  
  
  observeEvent(input$show,{
    
    # updateActionButton(session ,"show",label = "hide",icon = icon("xmark"))
    (jqui_show(".col-sm-4",effect = "fold"))
    removeUI("#show",multiple = T)
    insertUI(".hider","beforeBegin",ui=
               actionButton("hide",label = "",icon = icon("xmark")))
    
    
    
  })
  observeEvent(input$hide,{
    #updateActionButton(session ,"hide",label = "hide",icon = icon("bars"))
    removeUI("#hide",multiple = T)
    
    (jqui_hide(".col-sm-4",effect = "fold"))
    
    insertUI(".hider","beforeBegin",ui=actionButton("show",label = "",icon = icon("bars"))
    )
    
  })
  
  
  
  
  
  observeEvent(input$Upload_pmt,ignoreInit = T,{
    
    if(is.null(input$paytm )){
      
      insertUI("#Upload_pmt","beforeBegin",ui=tagList(shiny::tags$div(class="screenshot-error",shiny::tags$h3(style="color:red;"," Please Upload a Screenshot"))))
      
      
    }
    #xx="wankrohit04@gmail.com"
    else{
    gcs_upload(file = input$paytm$datapath,bucket = "pay-screen",name = glue("pay/,{input$Login_Name},{as.character( Sys.Date()   )}"))
    X=googleCloudVisionR::gcv_get_image_annotations(imagePaths =  input$paytm$datapath,feature =  "TEXT_DETECTION")
    i=paste0(X$description,collapse = " ")
    #print(input$paytm)
    print(X)
    c1=one_or_more(DIGIT) %R% SPACE %R% one_or_more(WRD)%R%  SPACE %R% fixed("2024")
    #c1=one_or_more(DIGIT) %R% SPACE %R% one_or_more(WRD)%R%  SPACE %R% fixed("2024")
    
    #td=dmy(str_extract(string=i,c1))
    td=dmy(regextr2(i,c1))
    
    pat="₹ " %R% one_or_more(DIGIT)
    
    
    pm=regextr2(i,pat)
    
    #pm=str_extract(i,pat)
    id_p= "ID " %R% one_or_more(WRD)
    id= regextr2(i,id_p)
    
    #id= str_extract(i,id-p)
    
    print(td)
    #sql=glue_sql("insert into Paytm values({Email},{name},{date},{Amount},'-')",.con=con)
    sql=glue_sql("insert into Paytm values({input$Login_Name},{filt1()$Name},{td},{pm},{id},{i},{input$Login_Name})",.con=con)
    
    dbGetQuery(con,sql)
    
   #4-11-204 insertUI("#welcome","beforeBegin",ui=shiny::tags$div(id="id",navlistPanel( "I SOLACE HUB",selected = "RANDOM STRANGER VIDEO CALLS",header =  actionButton("Logout_Now","Logout",width = "40%",class="btn-danger"),id="navlist_welcome",navbarMenu("ACCOUNT",tabPanel( shiny::tags$image(style="border-radius:50%;",height="30",width="30",src=paste0("https://storage.googleapis.com/bucket-ish/",filt1()$Email,".png"),"PROFILE"),#"https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRWC4jV0uIW-Pqre2o1SLfNM6zOblgcd_-24WM-BVcoJ9EwyBOFq2o2LR82aJGBHu9x_0s&usqp=CAU"),
    #4-11-204                                                                                                                                                                                                                                                                     shiny::tags$h2("YOUR PROFILE",style="text-align:center;") ,shiny::tags$p("llllll",class="account" )),tabPanel(actionButton("Logout","LOG OUT"))),#tablpanel account
    #4-11-204                                                                  
    #4-11-204                                                                  tabPanel("TODAYS MATCH",shiny::tags$h3("MATCH OF THE DAY",style="text-align:center;"),shiny::tags$p(class="match")),
    #4-11-204                                                                  #message check23/9/2024 navbarMenu("MESSAGES", 
                                                                               #message check23/9/2024           tabPanel("SENT",tags$h3("SENT",style="text-align:center;"),tags$p(class="sent")),
                                                                               #message check23/9/2024       tabPanel("INBOX",tags$h3("INBOX",style="text-align:center;"),tags$p(class="inbox"))
                                                                               #message check23/9/2024      ),#NavbarMenu
    #4-11-204                                                                        tabPanel("VIDEO DATES",shiny::tags$h3("SCHEDULED VIDEO DATES",style="text-align:center;"),shiny::tags$p(class="video-date")),
                                                                               tabPanel("GO LIVE",shiny::tags$h3("GO LIVE",style="text-align:center;"),shiny::tags$p(class="go-live"),shiny::tags$br(),shiny::tags$div(id="go-live",style="border-radius:2%;background-color:purple;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Live","Go LIVE",width="60%",icon = icon("facetime-video",lib="glyphicon") ) ))#fluidRow(column(8,textAreaInput("Group-sender","TYPE MESSAGE HERE")),
                                                                                        #        column(2,tags$br(),tags$br(),actionButton("Group-sender-btn","SEND"))
                                                                                        
                                                                                        
                                                                                        # )#fluid col
                                                                                        
                                                                               )
                                                                               #4-11-204                                                                        ,
                                                                               tabPanel("(PRIVATE CHAT) FACETIME WITH STRANGERS ",shiny::tags$h3("LIVE STRANGERS",style="text-align:center;"),actionButton("refresh_Live","Refresh List",width ="80%" ),shiny::tags$p(class="facetime"))
                                                                               #4-11-204                                                                            ,tabPanel("RANDOM STRANGER VIDEO CALLS",shiny::tags$h3("LIVE RANDOM STRANGERS CALL",style="text-align:center;"),shiny::tags$br(),shiny::tags$div(id="random-init",style="border-radius:2%;background-color:purple;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Random","CALL NOW",icon = icon("facetime-video",lib="glyphicon") ,width = "60%"))),shiny::tags$div(style="display:none;",textInput("rand1","text_detect")),shiny::tags$p(class="random-calls"))
                  insertUI(".div1p","afterBegin",ui=  shiny::tags$div(class="welcome", style="top:10%;border-radius:10%;left:12vw;width:70vw;opacity:1;height:50%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);#background-color:rgba(255, 117, 170, 1);
  color:black;
           font-size:20px;", shiny::tags$br(), "Welcome to I Solace Hub",id="welcome",shiny::tags$p(class="p1",),shiny::tags$br(),shiny::tags$br(),actionButton("Login1st","LOGIN",width = "80%"),shiny::tags$br(),shiny::tags$br(),actionButton("Signin1st","SIGN IN",width = "80%")),shiny::tags$div("",class="div1p",style="height:10px; position: absolute;left:10%; top:2wv;")
                                     
                  )                                                             
                                                                               
                                                                               #4-11-204))#navlistpanel
                                                                               #4-11-204 )#insert ui navlist panel
    
    removeUI("#id")
    removeUI("#hide")
    removeUI(".screenshot-error")
    removeUI("body > div > div.pay_ui")
    }
  })
  
  
  observeEvent(input$refresh_Live,ignoreInit = T,{
    
    
    waiter <- waiter::Waiter$new()
    #waiter::use_waiter()
    
    waiter$show()
    on.exit(waiter$hide())
    
    
    # code 2/102024 l_data=readRDS("db/data1.RDS")
    # invalidateLater(5000)
    
    # code 2/10/2024 l_data1=l_data %>% filter(Live=="Live_now")
    l_data1=xx %>% filter(Live=="Live_now")
    
    removeUI(selector = "[class*='f_time']",multiple = T)
    
    
    
    insertUI(".facetime",where = "beforeBegin",ui=mapply(facetime,src1= paste0("https://storage.googleapis.com/bucket-ish/",l_data1$Email,".png"),cl=l_data1$url_link,Name=l_data1$Name,City=l_data1$City))
    
    
  })
  
  
  
  observeEvent(input$Live,ignoreInit = T,{
    
    waiter <- waiter::Waiter$new()
    #waiter::use_waiter()
    
    waiter$show()
    on.exit(waiter$hide())
    
    
    headers = c(
      `Authorization` = paste("Bearer ", YOUR_API_KEY, sep = ""),
      `Content-Type` = "application/json"
    )
    
    data = '{  "endDate": "2099-02-18T14:23:00.000Z",  "fields": ["hostRoomUrl"]}'
    
    
    #7/9/2024res <- as.character(httr::POST(url = "https://api.whereby.dev/v1/meetings", httr::add_headers(.headers=headers), body = data))
    
    
    #7/9/2024li=fromJSON(res)
    sql_l=glue_sql("UPDATE Chat SET Live = 'Live_now' WHERE Email = {data_em$em};",.con=con)
    
    dbGetQuery(con,sql_l)
    #code2/10/2024 data1$Live<-replace(data1$Live,data1$Email==input$Login_Name,"Live_now")
    #code2/10/2024 data2=data.frame(data1)
    #7/9/2024data1=replace(data1$Host_url,Email=input$Email,li$hostRoomUrl)
    #7/9/2024data1=replace(data1$url_link,Email=input$Email,li$roomUrl)
    
    #sheet_append(live_sheet1, data2)
    
    
    
    # code2/10/2024 saveRDS(data1,"db/data1.RDS")
    
    #insertTab( inputId="navlist_welcome",tabPanel("Live"," ONCE YOU ARE LIVE YOU WILL BE IN LIVE LIST AND BE ABLE TO CHAT ONCE ANY ONE ENTERS THE ROOM",tags$div(style="width:60%;height:60vh;",tags$iframe(style="width:80vw;height:80vh;",src="https://r-testing.whereby.com/8c8ec639-91ff-40d7-8631-83f36a0f5f08",allow="autoplay;camera;microphone;"),actionButton("Close_Window","EXIT ROOM"))),select = T,target ="GO LIVE" )
    insertUI(".go-live","afterBegin",ui=shiny::tags$div(class="Live-chat",style="width:60%;height:60vh;",shiny::tags$iframe(style="width:80vw;height:80vh;",src=data_em$links,allow="autoplay;camera;microphone;"),actionButton("Close_Window","EXIT ROOM",width = "100%")))
    removeUI("#go-live")
    
    #insertUI(".go-live",where = "afterEnd",ui=tags$a("CLICK TO ENTER A Room",href=li$roomUrl,target="_blank"))
    
  })
  
  #close live for login and sign
  
  observeEvent(input$Close_Window,{
    removeUI(".Live-chat")
    insertUI(".go-live","afterBegin",ui=shiny::tags$div(id="go-live",style="border-radius:2%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);;width:70%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="width:60%;height:60vh;position:relative;top:40%;left:40%;",actionButton("Live","Go LIVE",icon = icon("facetime-video",lib="glyphicon") ) )))
    
    sql=glue_sql("UPDATE Chat SET Live = 'FALSE' WHERE Email = {data_em$em};",.con=con)
    
    dbGetQuery(con,sql)
    
    #code 2/102024 data1$Live<-replace(data1$Live,data1$Email==input$Login_Name,"False_no")
    #code 2/102024 data2=data.frame(data1)
    
    #code 2/102024 saveRDS(data1,"db/data1.RDS")
    
    
    
  })
  
  # logic or both sign, login
  #random call init button
  observeEvent(input$Random,ignoreInit =T, {
    
    waiter <- waiter::Waiter$new()
    #waiter::use_waiter()
    
    waiter$show()
    on.exit(waiter$hide())
    
    
    #code 2/102024 data1=readRDS("db/data1.RDS")
    #code 2/102024 data2=data.frame(data1)
    sql_r=glue_sql("Select * from Chat where Random = True AND Not Email = {data_em$em} Limit 1",.con = con)
    
    data3= dbGetQuery(con,sql_r)
    # 
    #10-29-2024data3=xx%>%filter(Random==T & !Email==data_em$em)
    #10-29-2024 rand_d=sample_n(data3,1)
    #10-29-2024 print(rand_d)
    insertUI(".random-calls","afterBegin",ui=shiny::tags$div(class="random",style="width:60%;height:60vh;",shiny::tags$iframe(style="width:80vw;height:80vh;",src=data3$url_link,allow="autoplay;camera;microphone;"),actionButton("Random_again","Join Another Call"),actionButton("Close_random","EXIT RANDOM ROOM",width = "100%")))
    removeUI("#random-init")
    print(data3)
  })
  
  observeEvent(input$Random_again,ignoreInit =T, {
    
    waiter <- waiter::Waiter$new()
    #waiter::use_waiter()
    
    waiter$show()
    on.exit(waiter$hide())
    
    #code 2/102024 data1=readRDS("db/data1.RDS")
    #code 2/102024  data2=data.frame(data1)
    sql_ra=glue_sql("Select * from Chat where Random = True AND Not Email = {data_em$em} Limit 1;",.con = con)
    
    data3= dbGetQuery(con,sql_ra)
    #  # 10-29-2024 data3=xx %>% filter(Random==T & !Email==data_em$em)
    ## 10-29-2024  rand_d=sample_n(data3,1,replace = T)
    #print(rand_d)
    insertUI(".random-calls","afterBegin",ui=shiny::tags$div(class="random",style="width:60%;height:60vh;",shiny::tags$iframe(style="width:80vw;height:80vh;",src=data3$url_link,allow="autoplay;camera;microphone;"),actionButton("Random_again","Join Another Call"),actionButton("Close_random","EXIT RANDOM ROOM",width = "100%")))
    removeUI(".random")
    print(data3)
  })
  data_em=reactiveValues(em="",links="",p_date="",j_date=Sys.Date())
  
  # random for signin
  ##10/9/2024
  observeEvent(input$Random,ignoreInit =T, { 
    
    sql_r=glue_sql("UPDATE Chat SET Random = TRUE WHERE Email = {data_em$em};",.con = con)
    
    dbGetQuery(con,sql_r)
    
    #code 2/102024 data1$Random<-replace(data1$Random,data1$Email==data_em$em,"yes")
    #code 2/102024 data1=data.frame(data1)
    
    #code 2/102024 saveRDS(data1,"db/data1.RDS")
    
  })
  # random for login
  ##10/9/2024
  observeEvent(input$Random,ignoreInit =T, { 
    
    sql_r=glue_sql("UPDATE Chat SET Random = TRUE WHERE Email = {data_em$em};",.con = con)
    
    dbGetQuery(con,sql_r)
    
    #code 2/102024 data1$Random<-replace(data1$Random,data1$Email==data_em$em,"yes")
    #code 2/102024 data1=data.frame(data1)
    
    #code 2/102024 saveRDS(data1,"db/data1.RDS")
  })
  
  
  observeEvent(input$Close_random,ignoreInit = T,{
    
    waiter <- waiter::Waiter$new()
    #waiter::use_waiter()
    
    waiter$show()
    on.exit(waiter$hide())
    
    sql_r=glue_sql("UPDATE Chat SET Random = FALSE WHERE Email = {data_em$em};",.con = con)
    
    dbGetQuery(con,sql_r)
    
    
    
    insertUI(".random-calls","beforeBegin",ui=shiny::tags$div(id="random-init",style="border-radius:2%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);;width:100%;height:200px;",shiny::tags$br(),shiny::tags$br(),shiny::tags$br(),shiny::tags$div(style="position:relative;top:40%;left:20%;",actionButton("Random","CALL NOW",icon = icon("facetime-video",lib="glyphicon") ,width = "60%"))))
    
    removeUI(".random")
    
    
  })
  
  observeEvent(input$Close_p_Window,ignoreInit = T,{
    #1-11-2024removeTab(inputId="navlist_welcome",target = "PRIVATE CHAT")
    
    waiter <- waiter::Waiter$new()
    #waiter::use_waiter()
    
    waiter$show()
    on.exit(waiter$hide())
    
    removeUI(".face_window",multiple = T)
    insertUI(".facetime",where = "beforeBegin",actionButton("refresh_Live","Refresh List",width ="80%" ))
    insertUI(".facetime",where = "beforeBegin",ui=mapply(facetime,src1= paste0("https://storage.googleapis.com/bucket-ish/",xx$Email,".png"),cl=xx$url_link,Name=xx$Name,City=xx$City))
    
    sql_l=glue_sql("UPDATE Chat SET Live = 'no' WHERE Email = {data_em$em};",.con=con)
    
    dbGetQuery(con,sql_l)
    
  })
  
  
  observe({
    print(input$rand1)
  })
  
  observeEvent(input$rand1,ignoreInit = T,{
    if(str_detect(input$rand1,"https") == T )
    {
      print("yes")
      print(input$rand1)
      removeUI(".f_time",multiple = T)
      removeUI("#refresh_Live")
      insertUI(".facetime",where = "beforeBegin",ui=tagList(shiny::tags$div(class="face_window",style="width:60%;height:60vh;",shiny::tags$iframe(style="width:80vw;height:80vh;",src=input$rand1,allow="autoplay;camera;microphone;"),actionButton("Close_p_Window","EXIT ROOM"))))
      #1/11/2024 isolate(insertTab( inputId="navlist_welcome",tab=tabPanel("PRIVATE CHAT","YOU WILL BE ABLE TO CHAT ONCE THEY ALLOW YOU TO ENTER THE ROOM",shiny::tags$div(style="width:60%;height:60vh;",shiny::tags$iframe(style="width:80vw;height:80vh;",src=input$rand1,allow="autoplay;camera;microphone;"),actionButton("Close_p_Window","EXIT ROOM"))),select = T,target ="RANDOM STRANGER VIDEO CALLS" ))
    }
    else{
      
      print("not detected")
      
    }
    
  })
  
  observeEvent(input$Logout_Now1,ignoreInit = T,{
    
    unlink(paste0("WWW/",input$Login_Name,".png"))
    removeUI("#id > div > div.col-sm-4.well")
    
    removeUI("#navlist_welcome")
    removeUI("#id > div > div")
    stopApp()
    #insertUI("body","afterBegin",ui=tags$div(class="welcome", style="top:10%;border-radius:10%;left:12vw;width:70vw;opacity:1;height:50%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);#background-color:rgba(255, 117, 170, 1);
    #color:black;
    #    font-size:20px;", tags$br(), "Welcome to I Solace Hub",id="welcome",tags$p(class="p1",),tags$br(),tags$br(),actionButton("Login1st","LOGIN",width = "80%"),tags$br(),tags$br(),actionButton("Signin1st","SIGN IN",width = "80%")),tags$div("",class="div1p",style="height:10px; position: absolute;left:10%; top:2wv;")
    #)
    
    
  })
  
  observeEvent(input$Logout_Now,ignoreInit = T,{
    
    unlink(paste0("WWW/",input$Login_Name,".png"))
    removeUI("#id > div > div.col-sm-4.well")
    
    removeUI("#navlist_welcome")
    removeUI("#id > div > div")
    stopApp()
    #insertUI("body","afterBegin",ui=tags$div(class="welcome", style="top:10%;border-radius:10%;left:12vw;width:70vw;opacity:1;height:50%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);#background-color:rgba(255, 117, 170, 1);
    #color:black;
    #    font-size:20px;", tags$br(), "Welcome to I Solace Hub",id="welcome",tags$p(class="p1",),tags$br(),tags$br(),actionButton("Login1st","LOGIN",width = "80%"),tags$br(),tags$br(),actionButton("Signin1st","SIGN IN",width = "80%")),tags$div("",class="div1p",style="height:10px; position: absolute;left:10%; top:2wv;")
    #)
    
    
  })
  #Save_schedule_s
  #schedule for signin
  observeEvent(input$Save_schedule_s,ignoreInit = T,{
    
  #11-4-2024  headers = c(
    #11-4-2024  `Authorization` = paste("Bearer ", YOUR_API_KEY, sep = ""),
    #11-4-2024   `Content-Type` = "application/json"
    #11-4-2024  )
    
    #11-4-2024data = '{  "endDate": "2099-02-18T14:23:00.000Z",  "fields": ["hostRoomUrl"]}'
    
    
    #11-4-2024 res <- as.character(httr::POST(url = "https://api.whereby.dev/v1/meetings", httr::add_headers(.headers=headers), body = data))
    
    
    #11-4-2024 li=fromJSON(res)
    
    #10-28-2024 s_call=add_row(Schedule_call,from=input$Login_Name,to=x()$Image,name=x()$Name,date_time=paste(input$Date,strftime(input$Time, "%T"),Sys.timezone()))
    # 3-11-2024sql_s=glue_sql("insert into Schedule values ({input$Email},{x1()$Image},{x1()$Name},{paste(input$Date,strftime(input$Time, '%T'),Sys.timezone()) })  ",.con=con)
    #sql_s=glue_sql("insert into Schedule values ({input$Email},{x1()$Image},{x1()$Name},{paste(input$Date,strftime(input$Time, '%T'),Sys.timezone()) }, {li$hostRoomUrl})  ",.con=con)
   # sql_s=glue_sql("insert into Schedule values ({data_em$em},{x()$Image},{x()$Name},{paste(input$Date,input$Time,Sys.timezone()) }, {li$hostRoomUrl})",.con=con)
    
    #dbGetQuery(con,sql_s)
    showToast(session,input,autoClose = F,type = "success",text = 
                                                                        
                                                                        "This service is not for users with free trial")
    
    
    
    #10-28-2024  saveRDS(s_call,"db/Schedule_call.RDS")
  })
  
  
  #schedule for signin
  observeEvent(input$Save_schedule_s,ignoreInit = T,{
    
    #11-4-2024headers = c(
    #11-4-2024`Authorization` = paste("Bearer ", YOUR_API_KEY, sep = ""),
    #11-4-2024`Content-Type` = "application/json"
    #11-4-2024)
    
    #11-4-2024data = '{  "endDate": "2099-02-18T14:23:00.000Z",  "fields": ["hostRoomUrl"]}'
    
    
    #11-4-2024res <- as.character(httr::POST(url = "https://api.whereby.dev/v1/meetings", httr::add_headers(.headers=headers), body = data))
    
    
    #11-4-2024 li=fromJSON(res)
    
    
    showToast(session,input,autoClose = F,type = "success",text = 
                
                "This service is not for users with free trial")
    
    
    #   s_call=add_row(Schedule_call,from=input$Login_Name,to=x()$Image,name=x()$Name,date_time=paste(input$Date,strftime(input$Time, "%T"),Sys.timezone()))
    #11-4-2024 sql_s=glue_sql("insert into Schedule values ({data_em$em},{x()$Image},{x()$Name},{paste(input$Date,input$Time,Sys.timezone()) }, {li$hostRoomUrl})",.con=con)
    
    #11-4-2024    dbGetQuery(con,sql_s)
    
    
    #10-28-2024  saveRDS(s_call,"db/Schedule_call.RDS")
  })
  
  
  
    #schedule for signin
  observeEvent(input$Save_schedule,ignoreInit = T,{
    
    headers = c(
      `Authorization` = paste("Bearer ", YOUR_API_KEY, sep = ""),
      `Content-Type` = "application/json"
    )
    
    data = '{  "endDate": "2099-02-18T14:23:00.000Z",  "fields": ["hostRoomUrl"]}'
    
    
    res <- as.character(httr::POST(url = "https://api.whereby.dev/v1/meetings", httr::add_headers(.headers=headers), body = data))
    
    
    li=fromJSON(res)
    
    #10-28-2024 s_call=add_row(Schedule_call,from=input$Login_Name,to=x()$Image,name=x()$Name,date_time=paste(input$Date,strftime(input$Time, "%T"),Sys.timezone()))
    # 3-11-2024sql_s=glue_sql("insert into Schedule values ({input$Email},{x1()$Image},{x1()$Name},{paste(input$Date,strftime(input$Time, '%T'),Sys.timezone()) })  ",.con=con)
    #sql_s=glue_sql("insert into Schedule values ({input$Email},{x1()$Image},{x1()$Name},{paste(input$Date,strftime(input$Time, '%T'),Sys.timezone()) }, {li$hostRoomUrl})  ",.con=con)
    sql_s=glue_sql("insert into Schedule values ({data_em$em},{x()$Image},{x()$Name},{paste(input$Date,input$Time,Sys.timezone()) }, {li$hostRoomUrl})",.con=con)
    
    dbGetQuery(con,sql_s)
    showToast(session,input,autoClose = F,type = "success",text = paste("YOUR VIDEO DATE IS SCHEDULED ON",input$Date,"AT"
                                                                        
                  ,input$Time,  "TIMEZONE",Sys.timezone(),"\n",x1()$Name,"will be notified about it via email"))
    
    
    
    #10-28-2024  saveRDS(s_call,"db/Schedule_call.RDS")
  })
  
  
  # schedule for login
  observeEvent(input$Save_schedule,ignoreInit = T,{
    
    headers = c(
      `Authorization` = paste("Bearer ", YOUR_API_KEY, sep = ""),
      `Content-Type` = "application/json"
    )
    
    data = '{  "endDate": "2099-02-18T14:23:00.000Z",  "fields": ["hostRoomUrl"]}'
    
    
    res <- as.character(httr::POST(url = "https://api.whereby.dev/v1/meetings", httr::add_headers(.headers=headers), body = data))
    
    
    li=fromJSON(res)
    
    
    showToast(session,input,autoClose = F,type = "success",text = paste("YOUR VIDEO DATE IS SCHEDULED ON",input$Date,"AT",
                                                                        
                                                             strftime(input$Time, "%T") ,"TIMEZONE",Sys.timezone(),"\n",x()$firstname,"will be notified about it via email"))
    
  #   s_call=add_row(Schedule_call,from=input$Login_Name,to=x()$Image,name=x()$Name,date_time=paste(input$Date,strftime(input$Time, "%T"),Sys.timezone()))
    sql_s=glue_sql("insert into Schedule values ({data_em$em},{x()$Image},{x()$Name},{paste(input$Date,input$Time,Sys.timezone()) }, {li$hostRoomUrl})",.con=con)
    
     dbGetQuery(con,sql_s)
    
    
    #10-28-2024  saveRDS(s_call,"db/Schedule_call.RDS")
      })

  
  # schedule for signin
  observeEvent(input$Schedule_s,once = T,ignoreInit = T,{
    
    insertUI(".div-match","beforeEnd",ui=fluidRow(column(6,dateInput("Date","SELECT A DATE",min = Sys.Date())),column(6,timeInput("Time","SELECT A TIME"))
                                                  ,actionButton("Save_schedule_s","SAVE SCHEDULE") )#col timeinput
    )#ui
    
  })
  
  
    # schedule for login
  observeEvent(input$Schedule1,once = T,{
    
    insertUI(".div-match","beforeEnd",ui=fluidRow(column(6,dateInput("Date","SELECT A DATE",min = Sys.Date())),column(6,timeInput("Time","SELECT A TIME"))
                                                  ,actionButton("Save_schedule","SAVE SCHEDULE") )#col timeinput
    )#ui
    
  })
  
  
  
  react=reactiveValues(x1=character(),y1=character(),Sms1=character())
  
  #logout code
  observeEvent(input$Logout,once = T,{
    
    removeUI("#navlist_welcome")
    #removeUI("#div-nav")
    
    removeUI("#id")
    insertUI(".div1p",where = "beforeEnd",ui= tagList ( shiny::tags$div(class="welcome", style="height:300px; position:absolute; left:30%;top:50%;background-color:rgba(255,255,255,.15);backdrop-filter:blur(5px);# background-color:rgba(255, 117, 170, 1);
    color:black;
           font-size:20px;",  "Welcome to  Talky Box",id="welcome",shiny::tags$p(class="p1"),shiny::tags$br(),shiny::tags$br(),actionButton("Login1st","Login"),actionButton("Signin1st","Sign in")),shiny::tags$div("",class="div1p",style="height:10px; position: absolute;left:50%; top:0%;")
    ))
    #refresh()
  })
  
  
  
  #signin1 logic
  observeEvent(once = T,input$Signin1,{
    
    
    #insertUI(selector =  "#welcome",where = "beforeBegin",
    # sidebarLayout(sidebarPanel("l"),mainPanel("o")))
    
    
    #insertUI(selector =  "#Login",where = "beforeBegin",passwordInput("password","Enter Password"))
    #insertUI(selector =  "#Login",where = "beforeBegin",actionButton("Signin1","Sign in"))
    react$x1<-c(react$x1,input$name)
    react$y1<-c(react$y1,input$password)
    o=cbind(react$x1,react$y1)
    ox=data.frame(o)
    names(ox)<-c("username","password")
    xi=rbind(data,ox)
    saveRDS(xi,"WWW/data.RDS")
    insertUI("#welcome","beforeBegin",sidebarLayout(sidebarPanel("Account",id="Account",shiny::tags$br(), shiny::tags$image(height="30",width="30",src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRWC4jV0uIW-Pqre2o1SLfNM6zOblgcd_-24WM-BVcoJ9EwyBOFq2o2LR82aJGBHu9x_0s&usqp=CAU"),shiny::tags$div(class="account",input$name),actionButton("Logout","Logout"))
                                                    
                                                    
                                                    ,mainPanel("chat history",
                                                               fluidRow(column(4,textInputAddon("Sms","",addon = icon("sms")) ),column(8,shiny::tags$br(), actionButton("Send","Send")))
                                                    )))
    
    
    removeUI("#welcome")
    #insertUI("Account",ui=tags$div("Available  users",tags$br(),input$name),where = "afterEnd"
    #  )
    
    #removeUI("#Login")
    
  })
  
  observeEvent(input$Signin1,{
    insertUI("#Account",ui=checkboxGroupButtons(direction = "vertical",
                                                inputId = "Onlineusers",
                                                label = "Available users",
                                                choices = data$username,checkIcon = list(
                                                  yes = icon("sms"),
                                                  no = icon("user")
                                                ),
                                                status = "info"),where = "beforeEnd"
    )
    
  })
  
  
  #cb=cbind(tr$x,tr$y)
  
  observeEvent(input$Send,{
    
    chatO=chat%>%filter(username %in% input$Onlineusers)
    react$Sms1<-input$Sms
    
    
    
  })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


