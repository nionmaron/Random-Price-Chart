
# The objective of this script is to generate a random price chart for use in various situations or trading tests.


#############################################################
library(plotly)
library(TTR)
library(htmlwidgets)

rm(list = ls(all.names = TRUE)) # Limpar todos objetos do R
getwd() # Verificar local do diretorio

#############################################################
#functions

# Function to generate a sequence of dates without timezone information
generateDateSequence <- function(start_date=Sys.Date(),length_out=241,interval="day", units="days") {
  # Convert the start date to POSIXct format
  start_date <- as.POSIXct(start_date, tz = "UTC")
  
  # Define possible intervals
  intervals <- c("day" = "days", "hour" = "hours", "10min" = "mins", "5min" = "mins", "2min" = "mins", "1min" = "mins")
  
  # Calculate the interval amount based on the units
  quantity <- ifelse(units == "10min", 10, ifelse(units == "5min", 5, ifelse(units == "2min", 2, ifelse(units == "1min", 1, 1))))
  
  # Generate the date sequence
  sequence <- seq(from = start_date, by = paste(quantity, intervals[interval]), length.out = length_out)
  
  # Remove timezone information
  sequence_no_tz <- sequence
  
  return(sequence_no_tz)
}

# test
generateDateSequence()

#############################################################
# Parameters
runif(1,0.1,2)
start<-1
tab<-data.frame()
LINHAS<-generateDateSequence()
length(LINHAS)

###########################################################
# Generate data

for (rr in 1:length(LINHAS)) {
  #rr<-1
  print(rr)
  var<-abs(start*runif(1,0.01, 0.05))
  #var<-0.05
  num<-runif(1,0.01, var)
  num2<-runif(1,0.01, var)
  gap<-runif(1,-0.01,0.01)
  p_abertura<-start+gap
  if(p_abertura==0){print(rr)}
  P_close<- runif(1, min=(p_abertura-num), max=(p_abertura+num))
  P_down <- runif(1, max=(min(p_abertura,P_close)), min=(min(p_abertura,P_close)-num2))
  P_up <- runif(1, min=(max(p_abertura,P_close)), max=(max(p_abertura,P_close)+num2))
  start<-P_close
  tab00<-data.frame("time"=LINHAS[rr],"open"=p_abertura,"close"=P_close,"high"=P_up,"low"=P_down,"volume"=0)
  tab<-rbind(tab,tab00)
}


tab$sma <- SMA(tab$close, n=20) # Ajuste 'n' para o período desejado
tab$rsi <- RSI(tab$close, n=14) # Ajuste 'n' para o período desejado

#############################################################
# Chart

fig <- plot_ly(x = ~time, data = tab, type = "candlestick",
               open = ~open, close = ~close, high = ~high, low = ~low, name = "Candlesticks") %>%
  layout(title = 'Candlestick Plot with Volume, SMA, and RSI')

# Adicionando a média móvel ao gráfico
fig <- fig %>% add_trace(y = ~sma, x = ~time, type = 'scatter', mode = 'lines', name = 'SMA 20', line = list(color = 'blue'))

# Adicionando uma camada de volume embaixo
fig <- fig %>% add_trace(y = ~volume, x = ~time, type = 'bar', name = 'Volume', yaxis = 'y2')

# Ajuste final do layout para acomodar as novas camadas
fig <- fig %>% layout(
  title = 'Random Price Chart',
  yaxis = list(domain = c(0.3, 1)),
  yaxis2 = list(domain = c(0.2, 0.3), title = 'Volume'),
  xaxis = list(
    rangeslider = list(visible = F) # hide zoom
  ),
  showlegend = FALSE  # hide legend
)

#############################################################
# Render Chart
fig

#############################################################
# Save

saveWidget(fig, 'Random Pricer Chart.html', selfcontained = TRUE)
write.csv2(tab, 'Random Pricer Data.csv')


