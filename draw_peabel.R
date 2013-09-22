draw_peabel<-function(matrice_coefficienti,comando_plottaggio)

{

	library("evd")
	x<-seq(from=1, to=100,by=0.1)
	nomi_dati<-gsub("X","",row.names(matrice_coefficienti))
	colori<-c(NULL)
	# colori<-c(colori,colors()[sample(10:150,10,replace=F)])		#Seleziona DIECI numeri casuali tra 10 e 150,
										#, sceglie per quel valore il colore associato da colors() e lo immagazzina.

	colori<-c("red","blue","green","gold","turquoise1","mediumpurple","orange3","coral")
	leg_col<-colori[1:length(matrice_coefficienti[,1])]

	if(comando_plottaggio=="Y")
	{
		
		plot(x,pgumbel(x,scale=as.numeric(matrice_coefficienti[1,1]),loc=as.numeric(matrice_coefficienti[1,2])),type="l",col=colori[1],xlab="Precipitazione [mm]",ylab="P[h]",main="Migliori curve di Gumbel approssimanti le precipitazioni cumulate")
		legend(x="bottomright",legend=nomi_dati,col=leg_col,bty="n",lwd=2)

	if(length(matrice_coefficienti[,1])>1)
	{
		for(i in 2:length(matrice_coefficienti[,1]))
		{	
		lines(x,pgumbel(x,scale=as.numeric(matrice_coefficienti[i,1]),loc=as.numeric(matrice_coefficienti[i,2])),col=colori[i])
		}
	}
	}
	else if(comando_plottaggio=="N")

	{
	path<-file.path(paste(getwd()),"Gumbelplots","Best_fit/")
	dir.create(path,recursive=TRUE,showWarnings=FALSE)
	path<-paste(paste(path),"Gumbelbest","from", paste(nomi_dati[1]), "to", paste(nomi_dati[length(nomi_dati)]), ".pdf", sep="_")
	pdf(file=path)

		plot(x,pgumbel(x,scale=as.numeric(matrice_coefficienti[1,1]),loc=as.numeric(matrice_coefficienti[1,2])),type="l",col=colori[1],xlab="Precipitazione [mm]",ylab="P[h]",main="Migliori curve di Gumbel approssimanti le precipitazioni cumulate")
		legend(x="bottomright",legend=nomi_dati,col=leg_col,bty="n",lwd=2)

	if(length(matrice_coefficienti[,1])>1)
	{
		for(i in 2:length(matrice_coefficienti[,1]))
		{	
		lines(x,pgumbel(x,scale=as.numeric(matrice_coefficienti[i,1]),loc=as.numeric(matrice_coefficienti[i,2])),col=colori[i])
		}
	}

	dev.off()
	}
	
}

