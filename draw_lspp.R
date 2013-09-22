draw_lspp<-function(lista_coefficienti,tempi_ritorno,comando_plottaggio)

{
#Inizializzazione variabili

	prob_ritorno<-NULL
	for (i in 1:length(tempi_ritorno))
	{
		prob_ritorno<-c(prob_ritorno,(tempi_ritorno[i]-1)/tempi_ritorno[i])
	}
	
	
	times<-NULL
	for(i in 1:length(lista_coefficienti[,1]))
	{
	times<-c(times,if(row.names(lista_coefficienti)[i]=="15min") 0.15
	       else if(row.names(lista_coefficienti)[i]=="30min") 0.30
	       else if(row.names(lista_coefficienti)[i]=="45min") 0.45
	       else if(row.names(lista_coefficienti)[i]=="1ora") 1
	       else if(row.names(lista_coefficienti)[i]=="3ore") 3
	       else if(row.names(lista_coefficienti)[i]=="6ore") 6
	       else if(row.names(lista_coefficienti)[i]=="12ore") 12
	       else if(row.names(lista_coefficienti)[i]=="24ore") 24	)
	}
	return_names<-NULL
	for(i in 1:length(tempi_ritorno))
		{
			return_names<-gsub("X","",c(return_names,paste(tempi_ritorno[i],"anni")))
		}

	colori<-c("red","blue","green","gold","turquoise1","mediumpurple","orange3","coral")

	quantili<-NULL
	lsft<-NULL

if(comando_plottaggio=="N")
{
	path<-file.path(paste(getwd()),"Gumbelplots","Lspp")
	dir.create(path,recursive=TRUE,showWarnings=FALSE)
	path<-paste(path,"Normal",".pdf", sep="_")
	pdf(file=path)	

#Primo plot NORMALE, per inizializzare i successivi aggiunti con lines()

for(g in 1:length(lista_coefficienti[,1]))
	{
		quantili<-c(quantili,qgumbel(prob_ritorno[1],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
	}
lsft<-lsfit(log(times),log(quantili))
plot(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[1])
quantili<-NULL	



#Plots NORMALI successivi
	for(i in 2:length(tempi_ritorno))
	{
		for(g in 1:length(lista_coefficienti[,1]))
		{
			quantili<-c(quantili,qgumbel(prob_ritorno[i],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
		}
		lsft<-lsfit(log(times),log(quantili))
	lines(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[i])
quantili<-NULL
	}
legend("bottomright",0.2,legend=return_names,bty="n",lty=1,col=colori[1:length(tempi_ritorno)])
dev.off()
#-------------------------------FINE PLOT NORMALI---------------------------

	path<-paste(path,"Logxy.pdf", sep="_")
	pdf(file=path)	

#Primo plot LOGARITMICO, per inizializzare i successivi aggiunti con lines()

for(g in 1:length(lista_coefficienti[,1]))
	{
		quantili<-c(quantili,qgumbel(prob_ritorno[1],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
	}
lsft<-lsfit(log(times),log(quantili))
plot(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[1],log="xy")
quantili<-NULL	



#Plots LOGARITMICI successivi
	for(i in 2:length(tempi_ritorno))
	{
		for(g in 1:length(lista_coefficienti[,1]))
		{
			quantili<-c(quantili,qgumbel(prob_ritorno[i],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
		}
		lsft<-lsfit(log(times),log(quantili))
	lines(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[i])
quantili<-NULL
	}
legend("bottomright",0.2,legend=return_names,bty="n",lty=1,col=colori[1:length(tempi_ritorno)])
dev.off()

}
#--------------------------------FINE CODICE SALVATAGGIO DEI PLOT---------------------
#--------------------------------INIZIO CODICE VISUALIZZAZIONE

if(comando_plottaggio=="Y")
{

#Primo plot NORMALE, per inizializzare i successivi aggiunti con lines()

for(g in 1:length(lista_coefficienti[,1]))
	{
		quantili<-c(quantili,qgumbel(prob_ritorno[1],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
	}
lsft<-lsfit(log(times),log(quantili))
plot(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[1])
quantili<-NULL	


#Plots NORMALI successivi
	for(i in 2:length(tempi_ritorno))
	{
		for(g in 1:length(lista_coefficienti[,1]))
		{
			quantili<-c(quantili,qgumbel(prob_ritorno[i],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
		}
		lsft<-lsfit(log(times),log(quantili))
	lines(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[i])
quantili<-NULL
	}
legend("bottomright",0.2,legend=return_names,bty="n",lty=1,col=colori[1:length(tempi_ritorno)])
#-------------------------------FINE PLOT NORMALI---------------------------


#Primo plot LOGARITMICO, per inizializzare i successivi aggiunti con lines()

for(g in 1:length(lista_coefficienti[,1]))
	{
		quantili<-c(quantili,qgumbel(prob_ritorno[1],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
	}
lsft<-lsfit(log(times),log(quantili))
plot(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[1])
quantili<-NULL	



#Plots LOGARITMICI successivi
	for(i in 2:length(tempi_ritorno))
	{
		for(g in 1:length(lista_coefficienti[,1]))
		{
			quantili<-c(quantili,qgumbel(prob_ritorno[i],scale=as.numeric(lista_coefficienti[g,1]),loc=as.numeric(lista_coefficienti[g,2])))
		}
		lsft<-lsfit(log(times),log(quantili))
	lines(times,exp(lsft$coefficients[[1]])*times^lsft$coefficients[[2]],type="l",xlab="t [ore]",ylab="h [mm]",main="Linee Segnalitrici di Possibilita' Pluviometrica",col=colori[i])
quantili<-NULL
	}
legend("bottomright",0.2,legend=return_names,bty="n",lty=1,col=colori[1:length(tempi_ritorno)])

}

}


