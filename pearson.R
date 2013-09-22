pearson<-function(dati,lista_coefficienti)
	
{	
# Caricamento librerie
	library("evd")

# Caricamento variabili	
	lunghezza_intervallo<-(lista_coefficienti[[5]][2]-lista_coefficienti[[5]][1])
	triade_chiquadro<-c(rep(NULL,each=3))
	miglior_chiquadro<-c(rep(NULL,each=(lunghezza_intervallo)))
	posizione_miglior_chiquadro<-c(rep(NULL,each=(lunghezza_intervallo)))
	suddivisione<-c(0.2,0.4,0.6,0.8)
	lista_definitiva<-list(NULL)
	col_names<-c("Best_Gumbel_scale","Best_Gumbel_loc","Best_method","Best_X^2_from_best_method")
	row_names<-c(NULL)

	data_frame_ultimo<-data.frame(matrix(nrow=lunghezza_intervallo,ncol=4))
	
	g=1
	s=0
	l=1

# Spazzolatore della riga dei coefficienti(raggruppati a coppie "scale-loc", nell'ordine orario) 
	for(i in lista_coefficienti[[5]][1]:lista_coefficienti[[5]][2])
	{
	
	row_names<-c(row_names,gsub("X","",names(dati)[i]))# Popolazione del vettore nomi   #
					      # di riga 		       #
	s=s+1
	ec<-ecdf(sort(dati[[i]]))
	lunghezza_dati<-length(sort(dati[[i]]))

# Spazzolatore della colonna dei metodi(suddivisi su tre righe, nell'ordine "Momenti-Minimi Quadrati-Massima verosimiglianza"	
		for(n in 1:3)
		{

# Pearson corevalues	
	quantili<-qgumbel(suddivisione,scale=lista_coefficienti[[n]][g],loc=lista_coefficienti[[n]][g+1])
	es_inf<-c(0,ec(quantili)*lunghezza_dati)
	es_sup<-c(ec(quantili)*lunghezza_dati,lunghezza_dati)
	punti_intervallo<-es_sup-es_inf
	delta<-0.2*lunghezza_dati
	triade_chiquadro[n]<-sum(((punti_intervallo-delta)^2)/delta)
		
		}
# Immagazzinamento dei migliori coefficienti risultati dal test
	miglior_chiquadro[s]<-triade_chiquadro[which.min(triade_chiquadro)]
	posizione_miglior_chiquadro[s]<-(1:length(triade_chiquadro))[which.min(triade_chiquadro)] 

# Posso immagazzinare in una lista o in una matrice semplicemente 	       #
# commentando/decommentando l'opzione preferita. I dati restano comunque       #
# con l'uno o l'altro metodo comprensibilmente ordinati.		       #

#Restituisce una lista
#	lista_definitiva[[s]]<-c(paste(names(dati)[i]),lista_coefficienti[[posizione_miglior_chiquadro[s]]][l],lista_coefficienti[[posizione_miglior_chiquadro[s]]][l#+1],if(posizione_miglior_chiquadro[s]==1) paste("Moments") else if(posizione_miglior_chiquadro[s]==2) paste ("Ls_squares") else if(posizione_miglior_chiquadro[s]==3) #paste("Max_simil"),paste("Best X^2"),miglior_chiquadro[s])

#Restituisce un data.frame
	data_frame_ultimo[s,]<-c(as.numeric(lista_coefficienti[[posizione_miglior_chiquadro[s]]][l]),as.numeric(lista_coefficienti[[posizione_miglior_chiquadro[s]]][l+1]),if(posizione_miglior_chiquadro[s]==1) paste("Moments") else if(posizione_miglior_chiquadro[s]==2) paste ("Ls_squares") else if(posizione_miglior_chiquadro[s]==3) paste("Max_simil"),as.numeric(miglior_chiquadro[s])	)
	

	l=l+2
	g=g+2
	}
	names(data_frame_ultimo)<-col_names
	row.names(data_frame_ultimo)<-row_names
	
	return (data_frame_ultimo)
	#return (lista_definitiva)
}
	
	
