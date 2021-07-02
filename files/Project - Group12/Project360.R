# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token = function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data = function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission = function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32737302":2.4,"32939029":2.4,"4066298":2.4,"48740784":2.4,"6676673":2.4, "7061886":2.4, "73318567":2.4, "85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format = function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://46.101.163.177'
u_name = "Group12"
p_word = "FBXNuZDaEgLQiD6b"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data1 = get_data(token=token,url=subm_url)

predictions=unique(data1[,list(product_content_id)])
predictions[,forecast:=2.3]

send_submission(predictions, token, url=subm_url, submit_now=F)

------------


####

require(ggplot2)
library(caTools)
library(xts)
library(zoo)
library(forecast)
library(writexl)
library(gtools)
library(dplyr)
library(corrplot)
library(urca)
library(data.table)

data2 = read.csv("/Users/muhammetenesustun/Desktop/ProjectRawData(1).csv")

write.csv(data1[,c(2,3,1,4,5,6,7,8,9,10,11,12,13)],"/Users/muhammetenesustun/Desktop/onlinedata.csv", row.names = FALSE)

data3 = read.csv("/Users/muhammetenesustun/Desktop/onlinedata.csv")

data3[nrow(data3):1, ]
data4=data3[order(data3$event_date),]
data4=data4[nrow(data4):1, ]
data = rbind(data4,data2)
data=data[nrow(data):1, ]

write_xlsx(data,"/Users/muhammetenesustun/Desktop/rbinddata.xlsx")

str(data)

summary(data)
#Mayis 31 (t-1) >> length 397/ 1 Haziran(t)  /   2 Haziran (t+1) 

n =396

dates = seq(as.Date("2020-05-25"), length = n, by = "days")

validation_dates=seq(as.Date("2021-03-01"), length = n-280, by = "days")
validation_dates




########################48740784 monttt##############################################################################

mont=subset(data, data$product_content_id==48740784)
mont
mont_dates = seq(as.Date("2020-09-29"), length = n-127, by = "days")
mont_xts=xts(mont,order.by=dates)
mont_xts

mont_train_xts=mont_xts[index(mont_xts)<"2021-03-01" & index(mont_xts)>="2020-09-29"]
mont_valid_xts=mont_xts[index(mont_xts)>="2021-03-01"]

#To creata data table of extra regressor for model

cor(mont$sold_count, mont$price)
cor(mont$sold_count, mont$favored_count)
cor(mont$sold_count, mont$basket_count)
cor(mont$sold_count, mont$category_sold)




mont_xreg1=cbind(mont[128:280,"price"],
                  mont[128:280,"basket_count"],
                  mont[128:280,"category_sold"])
mont_xreg2=cbind(mont[281:n,"price"],
                  mont[281:n,"basket_count"],
                  mont[281:n,"category_sold"])

acf(mont$sold_count)
pacf(mont$sold_count)

arima_mont=Arima(as.numeric(mont_train_xts$sold_count),xreg=as.matrix(mont_xreg1),order=c(1,0,0))
mont_train_xts$sold_count
AIC(arima_mont)

forecast_arima_mont=forecast(arima_mont,xreg=as.matrix(mont_xreg2))

forecast_arima_mont_xts=xts(forecast_arima_mont$mean,order.by=validation_dates)

mont$sold_count
plot(dates,mont$sold_count, type="l")
lines(validation_dates,forecast_arima_mont_xts, col ="blue")
###########################

mont_lm_train=mont[128:280,]
mont_lm_valid=mont[281:n,]


mont_lm_model=lm(sold_count~ -1+category_sold+basket_count,data=mont_lm_train)

summary(mont_lm_model)

predict_mont_lm=predict(mont_lm_model,mont_lm_valid)

predict_mont_lm

predict_mont_lm_xts= abs(xts(predict_mont_lm,order.by=validation_dates))


plot(dates,mont$sold_count, type="l")
lines(validation_dates,predict_mont_lm_xts, col="red")
#####Submission Future Xreg
mont_catsol_xreg=as.numeric(mont_xts$category_sold)
mont_catsol_model=auto.arima(mont_catsol_xreg)
mont_catsol_forecast=forecast(mont_catsol_model,h=2)

mont_basket_xreg=as.numeric(mont_xts$basket_count)
mont_basket_model=auto.arima(mont_basket_xreg)
mont_basket_forecast=forecast(mont_basket_model,h=2)

mont_price_xreg=as.numeric(mont_xts$price)
mont_price_model=auto.arima(mont_price_xreg)
mont_price_forecast=forecast(mont_price_model,h=2)


mont_submission_xreg1=cbind(mont[1:n,"category_sold"],mont[1:n,"price"],
                             mont[1:n,"basket_count"])

mont_submission_xreg2=data.table("category_sold"=mont_catsol_forecast$mean, "price"=mont_price_forecast$mean,
                                  "basket_count"=mont_basket_forecast$mean)

########ARIMA submission forecast 
mont_submission_xts=as.numeric(mont_xts$sold_count)

acf(mont_submission_xts)
pacf(mont_submission_xts)

mont_submission_arima_model=Arima(mont_submission_xts,
                                   xreg=as.matrix(mont_submission_xreg1),
                                   order=c(1,0,0))

mont_submission_forecast_arima=forecast(mont_submission_arima_model,
                                         xreg=as.matrix(mont_submission_xreg2))
########Reg Submission

mont_lm_submission=lm(sold_count~-1+basket_count+category_sold,data=mont)

pred_mont_lm_submodel=predict(mont_lm_submission,mont_submission_xreg2)

 
mont_submission_arima=0.5*(mont_submission_forecast_arima$mean[2]+pred_mont_lm_submodel[2])
mont_submission_arima

################################################## MONT ENDS##################################################################



#########################32939029 Oral B dis fircasi###########################################################################


toothbrush=subset(data, data$product_content_id==32939029)

toothbrush

plot(dates,toothbrush$sold_count,type="l", main="ORAL-B TOOTHBRUSH SALES", xlab= "Dates", ylab= "Sales")


toothbrush_xts=xts(toothbrush,order.by=dates)


toothbrush_train_xts=toothbrush_xts[index(toothbrush_xts)<"2021-05-01" & index(toothbrush_xts)>="2020-05-25"]

toothbrush_valid_xts=toothbrush_xts[index(toothbrush_xts)>="2021-05-01"]

validation_dates_toothbrush = seq(as.Date("2021-05-01"), length = n-341, by = "days")


#To creata data table of extra regressor for model

cor(toothbrush$sold_count, toothbrush$price)
cor(toothbrush$sold_count, toothbrush$category_favored)
cor(toothbrush$sold_count, toothbrush$basket_count)
cor(toothbrush$sold_count, toothbrush$category_sold)




toothbrush_xreg1=cbind(toothbrush[1:341,"category_sold"],
                         toothbrush[1:341,"basket_count"],toothbrush[1:341,"category_favored"]
                         )

toothbrush_xreg2=cbind( toothbrush[342:n,"category_sold"],
                         toothbrush[342:n,"basket_count"],toothbrush[342:n,"category_favored"]
                       )






toothbrush_arima_model=Arima(as.numeric(toothbrush_train_xts$sold_count),
                              xreg=as.matrix(toothbrush_xreg1),
                              order=c(2,0,1))


AIC(toothbrush_arima_model)


toothbrush_forecast_arima=forecast(toothbrush_arima_model,xreg=as.matrix(toothbrush_xreg2))

toothbrush_forecast_arima_xts=xts(toothbrush_forecast_arima$mean,order.by=validation_dates_toothbrush)



plot(dates,toothbrush$sold_count, type="l")
lines(validation_dates_toothbrush,toothbrush_forecast_arima_xts,col="blue")


toothbrush_ARIMA_MAPE=100*mean(abs((toothbrush_forecast_arima_xts-as.numeric(toothbrush_valid_xts$sold_count))/as.numeric(toothbrush_valid_xts$sold_count)))
toothbrush_ARIMA_MAPE

######################
toothbrush_lm_train=toothbrush[1:341,]
toothbrush_lm_valid=toothbrush[342:n,]


toothbrush_lm_model=lm(sold_count~-1+category_sold+basket_count+category_favored,data=toothbrush_lm_train)
summary(toothbrush_lm_model)
pred_toothbrush_lm_model=predict(toothbrush_lm_model,toothbrush_lm_valid)


pred_toothbrush_lm_model_xts=xts(pred_toothbrush_lm_model,order.by = validation_dates_toothbrush)

plot(pred_toothbrush_lm_model_xts)

plot(dates,toothbrush$sold_count, type="l")
lines(validation_dates_toothbrush,pred_toothbrush_lm_model_xts,col="red")

toothbrush_reg_MAPE=100*mean(abs((pred_toothbrush_lm_model_xts-as.numeric(toothbrush_valid_xts$sold_count))/as.numeric(toothbrush_valid_xts$sold_count)))
toothbrush_reg_MAPE

###Tootbrush Sub Xreg
toothbrush_catfav_xreg=as.numeric(toothbrush_xts$category_favored)
toothbrush_catfav_model=auto.arima(toothbrush_catfav_xreg)
toothbrush_catfav_forecast=forecast(toothbrush_catfav_model,h=2)

toothbrush_catsol_xreg=as.numeric(toothbrush_xts$category_sold)
toothbrush_catsol_model=auto.arima(toothbrush_catsol_xreg)
toothbrush_catsol_forecast=forecast(toothbrush_catsol_model,h=2)

toothbrush_basket_xreg=as.numeric(toothbrush_xts$basket_count)
toothbrush_basket_model=auto.arima(toothbrush_basket_xreg)
toothbrush_basket_forecast=forecast(toothbrush_basket_model,h=2)


##################################################################

toothbrush_submission_xreg1=cbind(toothbrush[1:n,"category_sold"],
                                   toothbrush[1:n,"category_favored"],
                                   toothbrush[1:n,"basket_count"])

toothbrush_submission_xreg2=data.table("category_sold"=toothbrush_catsol_forecast$mean,
                                        "category_favored"=toothbrush_catfav_forecast$mean,
                                        "basket_count"=toothbrush_basket_forecast$mean)



######ARIMA submission forecast
toothbrush_submission_xts=as.numeric(toothbrush_xts$sold_count)

toothbrush_submission_arima_model=Arima(toothbrush_submission_xts,
                                         xreg=as.matrix(toothbrush_submission_xreg1),
                                         order=c(2,0,1))

toothbrush_submission_forecast_arima=forecast(toothbrush_submission_arima_model,
                                               xreg=as.matrix(toothbrush_submission_xreg2))

#########Regression submission
toothbrush_lm_submission=lm(sold_count~-1+category_sold+category_favored+basket_count,data=toothbrush)

pred_toothbrush_lm_submodel=predict(toothbrush_lm_submission,toothbrush_submission_xreg2)


toothbrush_submission_arima=0.5*(toothbrush_submission_forecast_arima$mean[2]+pred_toothbrush_lm_submodel[2])
toothbrush_submission_arima



############################################ORALB ENDS########################################################################




##########################4066298	Islak mendil havlu ###############################################################

napkin=subset(data, data$product_content_id==4066298)
napkin



napkin_xts=xts(napkin,order.by=dates)


napkin_train_xts=napkin_xts[index(napkin_xts)<"2021-03-01"]
napkin_valid_xts=napkin_xts[index(napkin_xts)>="2021-03-01"]

#To creata data table of extra regressor for model

cor(napkin$sold_count, napkin$price)
cor(napkin$sold_count, napkin$category_favored)
cor(napkin$sold_count, napkin$basket_count)
cor(napkin$sold_count, napkin$category_sold)




napkin_xreg1=cbind(napkin[1:280,"category_sold"],
                    napkin[1:280,"basket_count"],
                   napkin[1:280,"price"],
                    napkin[1:280,"category_favored"])


napkin_xreg2=cbind(napkin[281:n,"category_sold"],
                    napkin[281:n,"basket_count"], napkin[281:n,"price"],
                    napkin[281:n,"category_favored"])



acf(napkin$sold_count)
pacf(napkin$sold_count)

napkin_arima_model=Arima(as.numeric(napkin_train_xts$sold_count),
                          xreg=as.matrix(napkin_xreg1),
                          order=c(1,0,0))

AIC(napkin_arima_model)

forecast_napkin=forecast(napkin_arima_model,xreg=as.matrix(napkin_xreg2))

forecast_napkin_xts=xts(forecast_napkin$mean,order.by=validation_dates)

plot(dates,napkin$sold_count, type="l")
lines(validation_dates,forecast_napkin$mean,col="blue")

napkin_ARIMA_MAPE=100*mean(abs((forecast_napkin_xts-as.numeric(napkin_valid_xts$sold_count))/as.numeric(napkin_valid_xts$sold_count)))
napkin_ARIMA_MAPE


###########Napkin regression    

napkin_lm_train=napkin[1:280,]
napkin_lm_valid=napkin[281:n,]


napkin_lm_model=lm(sold_count~-1+category_sold+basket_count+category_favored,data=napkin_lm_train)

summary(napkin_lm_model)

pred_napkin_lm_model=predict(napkin_lm_model,napkin_lm_valid)

pred_napkin_lm_model_xts=xts(pred_napkin_lm_model,order.by=validation_dates)

plot(pred_napkin_lm_model_xts)

plot(dates,napkin$sold_count,type="l")
lines(validation_dates,pred_napkin_lm_model_xts,col="red")


napkin_reg_MAPE=100*mean(abs((pred_napkin_lm_model_xts-as.numeric(napkin_valid_xts$sold_count))/as.numeric(napkin_valid_xts$sold_count)))
napkin_reg_MAPE

###Earbud ARIMA Submission Forecast
napkin_catfav_xreg=as.numeric(napkin_xts$category_favored)
napkin_catfav_model=auto.arima(napkin_catfav_xreg)
napkin_catfav_forecast=forecast(napkin_catfav_model,h=2)

napkin_catsol_xreg=as.numeric(napkin_xts$category_sold)
napkin_catsol_model=auto.arima(napkin_catsol_xreg)
napkin_catsol_forecast=forecast(napkin_catsol_model,h=2)

napkin_basket_xreg=as.numeric(napkin_xts$basket_count)
napkin_basket_model=auto.arima(napkin_basket_xreg)
napkin_basket_forecast=forecast(napkin_basket_model,h=2)

napkin_price_xreg=as.numeric(napkin_xts$price)
napkin_price_model=auto.arima(napkin_price_xreg)
napkin_price_forecast=forecast(napkin_price_model,h=2)
##################################################################

napkin_submission_xreg1=cbind(napkin[1:n,"category_sold"],
                               napkin[1:n,"category_favored"],
                               napkin[1:n,"basket_count"],napkin[1:n,"price"])
napkin_submission_xreg2=data.table("category_sold"=napkin_catsol_forecast$mean,
                                    "category_favored"=napkin_catfav_forecast$mean,
                                    "basket_count"=napkin_basket_forecast$mean,
                                   "price"=napkin_price_forecast$mean)


###ARIMA Submission FOrecast
napkin_submission_xts=as.numeric(napkin_xts$sold_count)

napkin_submission_arima_model=Arima(napkin_submission_xts,
                                     xreg=as.matrix(napkin_submission_xreg1),
                                     order=c(1,0,0))

napkin_submission_forecast_arima=forecast(napkin_submission_arima_model,
                                           xreg=as.matrix(napkin_submission_xreg2))

#########Regression submission
napkin_lm_submission=lm(sold_count~-1+category_sold+category_favored+basket_count,data=napkin)

pred_napkin_lm_submodel=predict(napkin_lm_submission,napkin_submission_xreg2)

##
napkin_submission_arima=0.5*(napkin_submission_forecast_arima$mean[2]+pred_napkin_lm_submodel[2])
napkin_submission_arima

###############################################NAPKIN ENDS###########################################################















##############################################85004 Kozmetik Y¨uz Temizleyici La Roche Posay###############################################################################



cosmetic=subset(data, data$product_content_id==85004)

cosmetic

plot(dates,cosmetic$sold_count, type ="l")

cosmetic_xts=xts(cosmetic,order.by=dates)

#cosmetic_validation_dates=seq(as.Date("2021-05-01"), length = 31, by = "days")
#cosmetic_validation_dates


cosmetic_train_xts=cosmetic_xts[index(cosmetic_xts)<"2021-03-01"  & index(cosmetic_xts)>="2020-05-25"]
cosmetic_valid_xts=cosmetic_xts[index(cosmetic_xts)>="2021-03-01"]

#To creata data table of extra regressor for model

cor(cosmetic$sold_count, cosmetic$price)
cor(cosmetic$sold_count, cosmetic$favored_count)
cor(cosmetic$sold_count, cosmetic$basket_count)
cor(cosmetic$sold_count, cosmetic$category_sold)
cor(cosmetic$sold_count, cosmetic$category_favored)



cosmetic_xreg1=cbind(cosmetic[1:280,"category_sold"],
                      cosmetic[1:280,"basket_count"],
                      cosmetic[1:280,"price"])

cosmetic_xreg2=cbind(cosmetic[281:n,"category_sold"],
                      cosmetic[281:n,"basket_count"],
                      cosmetic[281:n,"price"])
##############new

cosmetic_xreg1=cbind(cosmetic$basket_count[1:280],cosmetic$category_favored[1:280])
cosmetic_xreg2=cbind(cosmetic$basket_count[281:n],cosmetic$category_favored[281:n])




acf(cosmetic$sold_count)
pacf(cosmetic$sold_count)

test=ur.kpss(cosmetic$sold_count)
summary(test)




cosmetic_arima_model=Arima(as.numeric(cosmetic_train_xts$sold_count),
                            xreg=as.matrix(cosmetic_xreg1),
                            order=c(2,0,0))
#new
cosmetic_arima_model=auto.arima(as.numeric(cosmetic_train_xts$sold_count),
                                xreg=as.matrix(cosmetic_xreg1))
AIC(cosmetic_arima_model)


cosmetic_forecast_arima=forecast(cosmetic_arima_model,xreg=as.matrix(cosmetic_xreg2))
cosmetic_forecast_arima

cosmetic_forecast_cosmetic_xts=xts(cosmetic_forecast_arima$mean,order.by = validation_dates)

cosmetic_forecast_cosmetic_xts

plot(dates,cosmetic$sold_count, type ="l")
lines(validation_dates,cosmetic_forecast_cosmetic_xts,col="blue")

cosmetic_ARIMA_MAPE=100*mean(abs((cosmetic_forecast_cosmetic_xts-as.numeric(cosmetic_valid_xts$sold_count))/as.numeric(cosmetic_valid_xts$sold_count)))
cosmetic_ARIMA_MAPE

#################Cosmetic Regression

cosmetic_lm_train=cosmetic[1:280,]
cosmetic_lm_valid=cosmetic[281:n,]

cosmetic_lm_model=lm(sold_count~-1+basket_count,data=cosmetic_lm_train)
summary(cosmetic_lm_model)

pred_cosmetic_lm_model=predict(cosmetic_lm_model,cosmetic_lm_valid)

pred_cosmetic_lm_model_xts=xts(pred_cosmetic_lm_model,order.by=validation_dates)

plot(pred_cosmetic_lm_model_xts)

plot(dates,cosmetic$sold_count,type="l")
lines(validation_dates,pred_cosmetic_lm_model_xts,col="red")

cosmetic_reg_MAPE=100*mean(abs((pred_cosmetic_lm_model_xts-as.numeric(cosmetic_valid_xts$sold_count))/as.numeric(cosmetic_valid_xts$sold_count)))
cosmetic_reg_MAPE


######
#cosmetic_new=cosmetic_xts[ index(cosmetic_xts)>="2020-03-01"]


###Cosmetic ARIMA Submission Forecast
cosmetic_price_xreg=as.numeric(cosmetic_xts$price)
cosmetic_price_model=auto.arima(cosmetic_price_xreg)
cosmetic_price_forecast=forecast(cosmetic_price_model,h=2)

cosmetic_catsol_xreg=as.numeric(cosmetic_xts$category_sold)
cosmetic_catsol_model=auto.arima(cosmetic_catsol_xreg)
cosmetic_catsol_forecast=forecast(cosmetic_catsol_model,h=2)

cosmetic_basket_xreg=as.numeric(cosmetic_xts$basket_count)
cosmetic_basket_model=auto.arima(cosmetic_basket_xreg)
cosmetic_basket_forecast=forecast(cosmetic_basket_model,h=2)

cosmetic_catfav_xreg=as.numeric(cosmetic_xts$category_favored)
cosmetic_catfav_model=auto.arima(cosmetic_catfav_xreg)
cosmetic_catfav_forecast=forecast(cosmetic_catfav_model,h=2)
##################################################################


cosmetic_submission_xreg1=cbind(cosmetic$basket_count,cosmetic$category_favored)

cosmetic_submission_xreg2=data.table("basket_count"=cosmetic_basket_forecast$mean,"category_favored"=cosmetic_catfav_forecast$mean)
###ARIMA Cosmetic Submission Forecast

cosmetic_submission_xts=as.numeric(cosmetic_xts$sold_count)

#cosmetic_submission_arima_model=Arima(cosmetic_submission_xts, xreg=as.matrix(cosmetic_submission_xreg1),order=c(2,0,0))

cosmetic_submission_arima_model=auto.arima(cosmetic_submission_xts,
                                      xreg=as.matrix(cosmetic_submission_xreg1),)

cosmetic_submission_forecast_arima=forecast(cosmetic_submission_arima_model,
                                             xreg=as.matrix(cosmetic_submission_xreg2))



###########
#cosmetic_new2=cosmetic[307:n,]

cosmetic_lm_submission=lm(sold_count~-1+basket_count,data=cosmetic)

pred_cosmetic_lm_submodel=predict(cosmetic_lm_submission,cosmetic_submission_xreg2)


cosmetic_submission_arima=0.5*(cosmetic_submission_forecast_arima$mean[2]+pred_cosmetic_lm_submodel[2])
cosmetic_submission_arima
##############################################END 85004 Kozmetik Y¨uz Temizleyici La Roche Posay###############################################################################

#################################################6676673 Elektronik Telefon Bluetooth Kulaklýk Xiaomi######################################################################

#### ARIMA Earbud



#Create earbud data
earbud=subset(data, data$product_content_id==6676673)

nrow(earbud)

plot(dates,earbud$sold_count,type="l")

#Corvert to xts for forecasting
earbud_xts=xts(earbud,order.by=dates)
earbud_xts


#Split data to traind and test data
earbud_train_xts=earbud_xts[index(earbud_xts)>="2020-05-25" & index(earbud_xts)<"2021-03-01"]

earbud_valid_xts=earbud_xts[index(earbud_xts)>="2021-03-01"]


#To creata data table of extra regressor for model

cor(earbud$sold_count, earbud$price)
cor(earbud$sold_count, earbud$favored_count)
cor(earbud$sold_count, earbud$basket_count)
cor(earbud$sold_count, earbud$category_sold)
cor(earbud$sold_count, earbud$category_favored)


earbud_xreg1=cbind(earbud[1:280,"category_sold"],
                    earbud[1:280,"price"], earbud[1:280,"basket_count"])

earbud_xreg2=cbind(earbud[281:n,"category_sold"],
                    earbud[281:n,"price"], earbud[281:n, "basket_count"])
###### new
earbud_xreg1=cbind(earbud[1:280,"basket_count"])

earbud_xreg2=cbind(earbud[281:n, "basket_count"])



earbud_arima_model=Arima(as.numeric(earbud_train_xts$sold_count), xreg=as.matrix(earbud_xreg1),  order=c(0,1,4))


AIC(earbud_arima_model)

earbud_forecast_arima=forecast(earbud_arima_model,xreg=as.matrix(earbud_xreg2))

earbud_forecast_arima

earbud_forecast_arima_xts=xts(earbud_forecast_arima$mean,order.by=validation_dates)

plot(dates,earbud$sold_count, type="l")

lines(validation_dates,earbud_forecast_arima_xts,col="blue")

earbud_ARIMA_MAPE=100*mean(abs((earbud_forecast_arima_xts-as.numeric(earbud_valid_xts$sold_count))/as.numeric(earbud_valid_xts$sold_count)))
earbud_ARIMA_MAPE

############################################################################
###Earbud Regression

earbud_lm_train=earbud[1:280,]
earbud_lm_valid=earbud[281:n,]

earbud_lm_model=lm(sold_count~-1+basket_count,data=earbud_lm_train)

summary(earbud_lm_model)

pred_earbud_lm_model=predict(earbud_lm_model,earbud_lm_valid)

pred_earbud_lm_model

pred_earbud_lm_model_xts=xts(pred_earbud_lm_model,order.by=validation_dates)

plot(pred_earbud_lm_model_xts)

plot(dates,earbud$sold_count, type="l")
lines(validation_dates,pred_earbud_lm_model_xts,col="red")

earbud_reg_MAPE=100*mean(abs((pred_earbud_lm_model_xts-as.numeric(earbud_valid_xts$sold_count))/as.numeric(earbud_valid_xts$sold_count)))
earbud_reg_MAPE

#earbud_regression_MAPE=100*mean(abs((pred_earbud_lm_model_xts-earbud_valid_MAPE)/earbud_valid_MAPE))
#earbud_regression_MAPE

#########################################################################
###Earbud ARIMA Submission Forecast
earbud_price_xreg=as.numeric(earbud_xts$price)
earbud_price_model=auto.arima(earbud_price_xreg)
earbud_price_forecast=forecast(earbud_price_model,h=2)

earbud_basket_xreg=as.numeric(earbud_xts$basket_count)
earbud_basket_model=auto.arima(earbud_basket_xreg)
earbud_basket_forecast=forecast(earbud_basket_model,h=2)

earbud_catsol_xreg=as.numeric(earbud_xts$category_sold)
earbud_catsol_model=auto.arima(earbud_catsol_xreg)
earbud_catsol_forecast=forecast(earbud_catsol_model,h=2)

##################################################################



earbud_submission_xreg1=cbind(earbud$basket_count)
                              

earbud_submission_xreg2=data.table("basket_count"=earbud_basket_forecast$mean)

########## Arima submission earbud
earbud_submission_xts=as.numeric(earbud_xts$sold_count)

earbud_submission_arima_model=Arima(earbud_submission_xts,xreg=as.matrix(earbud_submission_xreg1),order=c(0,1,4))

earbud_submission_forecast_arima=forecast(earbud_submission_arima_model,xreg=as.matrix(earbud_submission_xreg2))
#########Regression submission
earbud_lm_submission=lm(sold_count~-1+basket_count,data=earbud)

pred_earbud_lm_submodel=predict(earbud_lm_submission,earbud_submission_xreg2)

##########
earbud_submission_arima=0.5*(earbud_submission_forecast_arima$mean[2]+pred_earbud_lm_submodel[2])
earbud_submission_arima

########################################################




########################################################        END Earbud                    #############################################################################



##############################################7061886 Elektronik S¨up¨urge Fakir ######################################



sweeper=subset(data, data$product_content_id==7061886)

sweeper

plot(dates,sweeper$sold_count, type="l")


sweeper_xts=xts(sweeper,order.by=dates)


sweeper_train_xts=sweeper_xts[index(sweeper_xts)<"2021-03-01" & index(sweeper_xts)>="2020-05-25"]


sweeper_valid_xts=sweeper_xts[index(sweeper_xts)>="2021-03-01"]

#To creata data table of extra regressor for model

cor(sweeper$sold_count, sweeper$price)
cor(sweeper$sold_count, sweeper$favored_count)
cor(sweeper$sold_count, sweeper$basket_count)
cor(sweeper$sold_count, sweeper$category_sold)


sweeper_xreg1=cbind(sweeper[1:280,"basket_count"])

sweeper_xreg2=cbind(sweeper[281:n,"basket_count"])

acf(sweeper$sold_count)
pacf(sweeper$sold_count)

test=ur.kpss(sweeper$sold_count)
summary(test)


sweeper_arima_model=auto.arima(as.numeric(sweeper_train_xts$sold_count),
                          xreg=as.matrix(sweeper_xreg1))
AIC(sweeper_arima_model)



sweeper_forecast_arima=forecast(sweeper_arima_model,xreg=as.matrix(sweeper_xreg2))

sweeper_forecast_arima


sweeper_forecast_arima_xts=xts(sweeper_forecast_arima$mean,order.by=validation_dates)

sweeper_forecast_arima_xts


plot(dates,sweeper$sold_count, type="l")

lines(validation_dates,sweeper_forecast_arima_xts,col="blue")

sweeper_ARIMA_MAPE=100*mean(abs((sweeper_forecast_arima_xts-as.numeric(sweeper_valid_xts$sold_count))/as.numeric(sweeper_valid_xts$sold_count)))
sweeper_ARIMA_MAPE

####Regression Sweeper

sweeper_lm_train=sweeper[1:280,]
sweeper_lm_valid=sweeper[281:n,]

sweeper_lm_model=lm(sold_count~-1+basket_count,data=earbud_lm_train)
summary(sweeper_lm_model)


pred_sweeper_lm_model=predict(sweeper_lm_model,sweeper_lm_valid)

pred_sweeper_lm_model_xts=xts(pred_sweeper_lm_model,order.by = validation_dates)

plot(pred_sweeper_lm_model_xts)

plot(dates,sweeper$sold_count,type="l")
lines(validation_dates,pred_sweeper_lm_model_xts,col="red")

sweeper_reg_MAPE=100*mean(abs((sweeper_forecast_arima_xts-as.numeric(sweeper_valid_xts$sold_count))/as.numeric(sweeper_valid_xts$sold_count)))
sweeper_reg_MAPE




###Earbud ARIMA Submission Forecast
sweeper_price_xreg=as.numeric(sweeper_xts$price)
sweeper_price_model=auto.arima(sweeper_price_xreg)
sweeper_price_forecast=forecast(sweeper_price_model,h=2)

sweeper_catsol_xreg=as.numeric(sweeper_xts$category_sold)
sweeper_catsol_model=auto.arima(sweeper_catsol_xreg)
sweeper_catsol_forecast=forecast(sweeper_catsol_model,h=2)

sweeper_basket_xreg=as.numeric(sweeper_xts$basket_count)
sweeper_basket_model=auto.arima(sweeper_basket_xreg)
sweeper_basket_forecast=forecast(sweeper_basket_model,h=2)

###########
sweeper_submission_xreg1=cbind(sweeper[1:n,"basket_count"])

sweeper_submission_xreg2=data.table("basket_count"=sweeper_basket_forecast$mean)

###ARIMA Submission Forecast
sweeper_submission_xts=as.numeric(sweeper_xts$sold_count)


sweeper_submission_arima_model=auto.arima(sweeper_submission_xts,
                                     xreg=as.matrix(sweeper_submission_xreg1))

sweeper_submission_forecast_arima=forecast(sweeper_submission_arima_model,
                                            xreg=as.matrix(sweeper_submission_xreg2))

#########Regression submission
sweeper_lm_submission=lm(sold_count~-1+basket_count,data=sweeper)

pred_sweeper_lm_submodel=predict(sweeper_lm_submission,sweeper_submission_xreg2)


sweeper_submission_arima=0.5*(sweeper_submission_forecast_arima$mean[2]+pred_sweeper_lm_submodel[2])
sweeper_submission_arima


################################################################SWEEPER ENDS################################################################################################



################################################################31515569 Giyim Tayt TRENDYOLM'ILLA############################################################


############Tight ARIMA



tight=subset(data, data$product_content_id==31515569)

tight

plot(dates,tight$sold_count,type="l")


tight_xts=xts(tight,order.by=dates)

tight_train_xts=tight_xts[index(tight_xts)<"2021-03-01"&index(tight_xts)>="2020-05-25"]
tight_valid_xts=tight_xts[index(tight_xts)>="2021-03-01"]


tight_train_sold_xts=as.numeric(tight_train_xts$sold_count)

tight_valid_MAPE=as.numeric(tight_valid_xts$sold_count)

#To creata data table of extra regressor for model

cor(tight$sold_count, tight$price)
cor(tight$sold_count, tight$favored_count)
cor(tight$sold_count, tight$basket_count)
cor(tight$sold_count, tight$category_sold)



tight_xreg1=cbind(tight[1:280,"basket_count"])

tight_xreg2=cbind(tight[281:n,"basket_count"])

acf(tight$sold_count)
pacf(tight$sold_count)



tight_arima_model=Arima(as.numeric(tight_train_xts$sold_count),
                         xreg=as.matrix(tight_xreg1),
                         order=c(1,2,4))
tight_arima_model=auto.arima(as.numeric(tight_train_xts$sold_count),
                        xreg=as.matrix(tight_xreg1))

AIC(tight_arima_model)


tight_forecast_arima=forecast(tight_arima_model,
                               xreg=as.matrix(tight_xreg2))

tight_forecast_arima


tight_forecast_arima_xts=xts(tight_forecast_arima$mean,order.by=validation_dates)

tight_forecast_arima_xts

plot(dates,tight$sold_count,type="l")

lines(validation_dates,tight_forecast_arima_xts,col="blue")

tight_ARIMA_MAE=mean(abs((tight_forecast_arima_xts-as.numeric(tight_valid_xts$sold_count))))
tight_ARIMA_MAE

####Regression Tight

tight_lm_train=tight[1:280,]
tight_lm_valid=tight[281:n,]


tight_lm_model=lm(sold_count~-1+basket_count,data=tight_lm_train)
summary(tight_lm_model)

pred_tight_lm_model=predict(tight_lm_model,tight_lm_valid)
pred_tight_lm_model_xts=xts(pred_tight_lm_model,order.by = validation_dates)

plot(pred_tight_lm_model_xts)

plot(dates,tight$sold_count,type="l")
lines(validation_dates,pred_tight_lm_model_xts,col="red")

tight_reg_MAE=mean(abs((pred_tight_lm_model_xts-as.numeric(tight_valid_xts$sold_count))))
tight_reg_MAE


#########################################################################
###Earbud ARIMA Submission Forecast
tight_price_xreg=as.numeric(tight_xts$price)
tight_price_model=auto.arima(tight_price_xreg)
tight_price_forecast=forecast(tight_price_model,h=2)

tight_catsol_xreg=as.numeric(tight_xts$category_sold)
tight_catsol_model=auto.arima(tight_catsol_xreg)
tight_catsol_forecast=forecast(tight_catsol_model,h=2)

tight_basket_xreg=as.numeric(tight_xts$basket_count)
tight_basket_model=auto.arima(tight_basket_xreg)
tight_basket_forecast=forecast(tight_basket_model,h=2)
##################################################################

tight_submission_xreg1=cbind(tight[1:n,"basket_count"])
tight_submission_xreg2=data.table("basket_count"=tight_basket_forecast$mean )

#ARIMA
tight_submission_xts=as.numeric(tight_xts$sold_count)

tight_submission_arima_model=Arima(tight_submission_xts,
                                    xreg=as.matrix(tight_submission_xreg1),
                                    order=c(1,2,4))

tight_submission_forecast_arima=forecast(tight_submission_arima_model,
                                          xreg=as.matrix(tight_submission_xreg2))


#########Regression submission
tight_lm_submission=lm(sold_count~-1+basket_count,data=tight)

pred_tight_lm_submodel=predict(tight_lm_submission,tight_submission_xreg2)



tight_submission_arima=pred_tight_lm_submodel[2]
tight_submission_arima

#770.1799 

##############################################TIGHT ENDS#################################################################################################################

##############################################73318567 Giyim bikini77 Ust¨u TRENDYOLM ¨ 'ILLA#############################################################################


bikini7=subset(data, data$product_content_id==73318567)

bikini7

plot(dates,bikini7$sold_count, type="l")

bikini7_xts=xts(bikini7,order.by=dates)

bikini7_train_xts=bikini7_xts[index(bikini7_xts)>="2021-01-23" & index(bikini7_xts)<"2021-05-01"]

bikini7_valid_xts=bikini7_xts[index(bikini7_xts)>="2021-05-01"]


validation_dates_bikini7=seq(as.Date("2021-05-01"), length = n-341, by = "days")

#To creata data table of extra regressor for model

cor(bikini7$sold_count, bikini7$price)
cor(bikini7$sold_count, bikini7$favored_count)
cor(bikini7$sold_count, bikini7$basket_count)
cor(bikini7$sold_count, bikini7$category_sold)


bikini7_xreg1=cbind(bikini7[244:341,"favored_count"],bikini7[244:341,"basket_count"])

bikini7_xreg2=cbind(bikini7[342:n,"favored_count"], bikini7[342:n,"basket_count"])


acf(bikini7$sold_count)
pacf(bikini7$sold_count)



bikini7_arima_model=Arima(as.numeric(bikini7_train_xts$sold_count),
                          xreg=as.matrix(bikini7_xreg1),
                          order=c(2,1,0))

AIC(bikini7_arima_model)


bikini7_forecast_arima=forecast(bikini7_arima_model,
                                xreg=as.matrix(bikini7_xreg2))

bikini7_forecast_arima


bikini7_forecast_arima_xts=xts(bikini7_forecast_arima$mean,order.by=validation_dates_bikini7)

bikini7_forecast_arima_xts

plot(dates,bikini7$sold_count,type="l")

lines(validation_dates_bikini7,bikini7_forecast_arima_xts,col="blue")
####bikini7 REGRESSION

bikini7_lm_train=bikini7[244:341,]
bikini7_lm_valid=bikini7[342:n,]


bikini7_lm_model=lm(sold_count~favored_count+basket_count,data=bikini7_lm_train)

pred_bikini7_lm_model=predict(bikini7_lm_model,bikini7_lm_valid)

pred_bikini7_lm_model_xts=xts(pred_bikini7_lm_model,order.by = validation_dates_bikini7)

plot(pred_bikini7_lm_model_xts)

plot(dates,bikini7$sold_count,type="l")
lines(validation_dates_bikini7,pred_bikini7_lm_model_xts,col="red")

###Earbud ARIMA Submission Forecast
bikini7_basket_xreg=as.numeric(bikini7_xts$basket_count)
bikini7_basket_model=auto.arima(bikini7_basket_xreg)
bikini7_basket_forecast=forecast(bikini7_basket_model,h=2)

bikini7_favored_xreg=as.numeric(bikini7_xts$favored_count)
bikini7_favored_model=auto.arima(bikini7_favored_xreg)
bikini7_favored_forecast=forecast(bikini7_favored_model,h=2)
##################################################################

bikini7_submission_xreg1=cbind(bikini7[1:n,"basket_count"],
                               bikini7[1:n,"favored_count"])
bikini7_submission_xreg2=data.table("basket_count"=bikini7_basket_forecast$mean,
                                    "favored_count"=bikini7_favored_forecast$mean)


###ARIMA Submission Forecast

bikini7_submission_xts=as.numeric(bikini7_xts$sold_count)

bikini7_submission_arima_model=Arima(bikini7_submission_xts,
                                     xreg=as.matrix(bikini7_submission_xreg1),
                                     order=c(2,1,0))

bikini7_submission_forecast_arima=forecast(bikini7_submission_arima_model,
                                           xreg=as.matrix(bikini7_submission_xreg2) )
#########Regression submission
bikini7_lm_submission=lm(sold_count~favored_count+basket_count,data=bikini7)

pred_bikini7_lm_submodel=predict(bikini7_lm_submission,bikini7_submission_xreg2)


bikini7_submission_arima=0.5*(bikini7_submission_forecast_arima$mean[2]+pred_bikini7_lm_submodel[2])
bikini7_submission_arima

#######################################################bikini7 ENDS############################################################
##############################################32737302 bikini3 #############################################################################


bikini3=subset(data, data$product_content_id==32737302)

bikini3

plot(dates,bikini3$sold_count, type="l")

bikini3_xts=xts(bikini3,order.by=dates)

bikini3_train_xts=bikini3_xts[index(bikini3_xts)>="2021-02-20" & index(bikini3_xts)<"2021-05-01"]

bikini3_valid_xts=bikini3_xts[index(bikini3_xts)>="2021-05-01"]


validation_dates_bikini3=seq(as.Date("2021-05-01"), length = n-341, by = "days")

#To creata data table of extra regressor for model

cor(bikini3$sold_count, bikini3$price)
cor(bikini3$sold_count, bikini3$favored_count)
cor(bikini3$sold_count, bikini3$basket_count)
cor(bikini3$sold_count, bikini3$category_sold)


bikini3_xreg1=cbind(bikini3[272:341,"favored_count"],bikini3[272:341,"basket_count"])

bikini3_xreg2=cbind(bikini3[342:n,"favored_count"], bikini3[342:n,"basket_count"])


acf(bikini3$sold_count)
pacf(bikini3$sold_count)



bikini3_arima_model=Arima(as.numeric(bikini3_train_xts$sold_count),
                          xreg=as.matrix(bikini3_xreg1),
                          order=c(1,0,0))

AIC(bikini3_arima_model)


bikini3_forecast_arima=forecast(bikini3_arima_model,
                                xreg=as.matrix(bikini3_xreg2))

bikini3_forecast_arima


bikini3_forecast_arima_xts=xts(bikini3_forecast_arima$mean,order.by=validation_dates_bikini3)

bikini3_forecast_arima_xts

plot(dates,bikini3$sold_count,type="l")

lines(validation_dates_bikini3,bikini3_forecast_arima_xts,col="blue")
####bikini3 REGRESSION

bikini3_lm_train=bikini3[272:341,]
bikini3_lm_valid=bikini3[342:n,]


bikini3_lm_model=lm(sold_count~favored_count+basket_count,data=bikini3_lm_train)

pred_bikini3_lm_model=predict(bikini3_lm_model,bikini3_lm_valid)

pred_bikini3_lm_model_xts=xts(pred_bikini3_lm_model,order.by = validation_dates_bikini3)

plot(pred_bikini3_lm_model_xts)

plot(dates,bikini3$sold_count,type="l")
lines(validation_dates_bikini3,pred_bikini3_lm_model_xts,col="red")

###Earbud ARIMA Submission Forecast
bikini3_basket_xreg=as.numeric(bikini3_xts$basket_count)
bikini3_basket_model=auto.arima(bikini3_basket_xreg)
bikini3_basket_forecast=forecast(bikini3_basket_model,h=2)

bikini3_favored_xreg=as.numeric(bikini3_xts$favored_count)
bikini3_favored_model=auto.arima(bikini3_favored_xreg)
bikini3_favored_forecast=forecast(bikini3_favored_model,h=2)
##################################################################

bikini3_submission_xreg1=cbind(bikini3[1:n,"basket_count"],
                               bikini3[1:n,"favored_count"])

bikini3_submission_xreg2=data.table("basket_count"=bikini3_basket_forecast$mean,
                                    "favored_count"=bikini3_favored_forecast$mean)


###ARIMA Submission Forecast

bikini3_submission_xts=as.numeric(bikini3_xts$sold_count)

bikini3_submission_arima_model=Arima(bikini3_submission_xts,
                                     xreg=as.matrix(bikini3_submission_xreg1),
                                     order=c(1,0,0))

bikini3_submission_forecast_arima=forecast(bikini3_submission_arima_model,
                                           xreg=as.matrix(bikini3_submission_xreg2) )
#########Regression submission
bikini3_lm_submission=lm(sold_count~favored_count+basket_count,data=bikini3)

pred_bikini3_lm_submodel=predict(bikini3_lm_submission,bikini3_submission_xreg2)


bikini3_submission_arima=0.5*(bikini3_submission_forecast_arima$mean[2]+pred_bikini3_lm_submodel[2])
bikini3_submission_arima

#######################################################bikini3 ENDS##########################################


submission_list=list(tight_submission_arima,bikini3_submission_arima,toothbrush_submission_arima,napkin_submission_arima,
                      mont_submission_arima,earbud_submission_arima,sweeper_submission_arima,bikini7_submission_arima, cosmetic_submission_arima)
submission_list


submission_productlist=list(31515569,32737302,32939029,4066298,48740784,6676673,7061886,73318567,85004)



submission_df=data.table("product_content_id"=submission_productlist, "forecast"=submission_list)

submission_df$forecast=as.numeric(submission_df$forecast)

submission_df

check_format(submission_df)

write_xlsx(submission_df,"/Users/muhammetenesustun/Desktop/ourpreds.xlsx")


send_submission(submission_df, token, url=subm_url, submit_now=T)



submission_check_ourpredictions_12Haziran=submission_df
submission_check_ourpredictions_12Haziran

submission_check_real=subset(data,data$event_date=="2020-06-08")
submission_check_real

