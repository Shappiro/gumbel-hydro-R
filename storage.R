storage<-function(dati,comando_plottaggio_gumbel_singoli,comando_plottaggio_gumbel_aggregati,limite_inferiore,limite_superiore)
	
{	
# Caricamento librerie built-in
	library("MASS")
	library("evd")
	
#Caricamento funzioni hand-made
	source("apply_moments.R")
	source("apply_leastsquares.R")
	source("apply_maxverosim.R")
	
	data_names<-gsub("X","",names(dati))
# Inizializzazione lista d'immagazzinamento risultati
	ls_dati<-list(Momenti=NULL,Minimi_quadrati=NULL,Massima_verosimiglianza=NULL,Intervallo_orario=c(data_names[limite_inferiore],data_names[limite_superiore]),Intervallo_colonne_dati=c(limite_inferiore,limite_superiore))

# Esecuzione delle funzioni per ogni durata oraria,immagazzinamento dei loro 
# output nella lista

#			 !!ATTENZIONE!!  				       #
# I parametri ottenuti dall'interpolazione con le curve di Gumbel sono 	       #
# immagazzinati nella riga della lista ls_dati a coppie, in ordine di comparsa #
# nei dati principali (quindi, nel nostro caso, in un ordine incrementale).    #
# Da notare come, ai fini della produzion dei grafici, l'ordine non cambia     #
# alcunché. Potrebbe essere comunque fonte di qualche grattacapo, se prima non #
# si è appurato l'ordine delle labels del data.frame con data_names           #

	for(i in limite_inferiore:limite_superiore)
	{
	ls_dati[[1]]<-c(ls_dati[[1]],apply_moments(dati[[i]],paste(comando_plottaggio_gumbel_singoli),paste(data_names[i])))
	
	ls_dati[[2]]<-c(ls_dati[[2]],apply_leastsquares(dati[[i]],paste(comando_plottaggio_gumbel_singoli),paste(data_names[i])))
	
	ls_dati[[3]]<-c(ls_dati[[3]],apply_maxverosim(dati[[i]],paste(comando_plottaggio_gumbel_singoli),paste(data_names[i])))

	}
	
	if(comando_plottaggio_gumbel_aggregati=="Y")
	{
	g=1

	for(i in limite_inferiore:limite_superiore)
	{
	x<-sort(dati[[i]])

	# dim_matrice_grafici<-(limite_superiore-limite_inferiore)/2
	# par(mfrow=c(dim_matrice_grafici,dim_matrice_grafici))	

plot(x,pgumbel(x,scale=ls_dati[[1]][g],loc=ls_dati[[1]][g+1]),xlab="Precipitazione [mm]",ylab="P[H<h]",main=paste("Curve di Gumbel a", data_names[[i]]),col="MEDIUMBLUE",type="l")
lines(x,pgumbel(x,scale=ls_dati[[2]][g],loc=ls_dati[[2]][g+1]),col="red")
lines(x,pgumbel(x,scale=ls_dati[[3]][g],loc=ls_dati[[3]][g+1]),col="LimeGreen")

plot(ecdf(x),add=T)
legend("bottomright",0.2,legend=c("Momenti","Minimi Quadrati","Massima Verosimiglianza"),bty="n",col=c("MEDIUMBLUE","red","LimeGreen"),lwd=1)

	g=g+2
	}
	}

	else if(comando_plottaggio_gumbel_aggregati=="N")
	{
	g=1
	for(i in limite_inferiore:limite_superiore)								
	{	
	x<-sort(dati[[i]])

	path<-file.path(paste(getwd()),"Gumbelplots","Aggregated/")
	dir.create(path,recursive=TRUE,showWarnings=FALSE)
	path<-paste(paste(path),"Gumbel_aggregated", paste(data_names[i]), ".png",sep="_")
	png(file=path)
	
	# dim_matrice_grafici<-(limite_superiore-limite_inferiore)/2
	# par(mfrow=c(dim_matrice_grafici,dim_matrice_grafici))	
		
plot(x,pgumbel(x,scale=ls_dati[[1]][g],loc=ls_dati[[1]][g+1]),xlab="Precipitazione [mm]",ylab="P[H<h]",main=paste("Curve di Gumbel a", data_names[[i]]),col="MEDIUMBLUE",type="l")
lines(x,pgumbel(x,scale=ls_dati[[2]][g],loc=ls_dati[[2]][g+1]),col="red")
lines(x,pgumbel(x,scale=ls_dati[[3]][g],loc=ls_dati[[3]][g+1]),col="LimeGreen")

plot(ecdf(x),add=T)
legend("bottomright",0.2,legend=c("Momenti","Minimi Quadrati","Massima Verosimiglianza"),bty="n",col=c("MEDIUMBLUE","red","LimeGreen"),lwd=1)
	
	
	dev.off()
	g=g+2
	}
	}
	
# Ritorno dei dati
	return(ls_dati)
	
}
	
