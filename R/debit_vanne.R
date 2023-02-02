#' Fonction de calcul du débit d'une vanne au barrage d'Arzal
#' 
#' Fonction de calcul de débit pour une vanne
#' Choisir canal="bazin" pour avoir les calculs au barrage
#' Aubuisson et horton pour calculer les débits suivant Ferrette (2004) en prenant ou non compte des vitesses
#' Par rapport à Ferette (2004), la formule en orifice dénoyé calcule les vitesses par iteration.
#' Le calcul est effectué à partir d'une indexation des conditions de débits (vecteur de valeurs vrai et faux) entre crochet.
#' Les coefficients de Bazin ont été recalculés par une 'regression' loess sur la base du rapport annexé à Briand et Woimant (2015)
#'
#' @param hvilaine Vecteur de données des niveaux Vilaine.
#' @param hmer Vecteur de données des niveaux mer.
#' @param hvanne Vecteur de données des hauteurs de vanne.
#' @param l Largeur de la vanne, défaut 18.
#' @param g 9.81.
#' @param canal Loi de débit choisir "bazin", "aubuisson", "horton", "manning" ou "ifsw".
#' @param dm Distance à la sonde amont pour formle de Basin défaut 73.
#' @param dv Distance à la sonde aval pour formule de Bazin défaut 94.
#' @param Cma Coefficient canal pour Manning défaut 0.65.
#' @param Cho Coefficient canal pour Horton défaut .79.
#' @param m_oninf4 Coefficient Orifice noyé noyé hvanne<4 (Ferrette) défaut 0.663.
#' @param m_onsup4 Orifice noyé hvanne >4 défaut 0.995.
#' @param Cvw Coefficient ifsw canal défaut 1.
#' @param Cvg Coefficient ifsw orifice h1<=1.5 hvanne défaut 1.5.
#' @param Cvgs Coefficient ifsw h1>1.5 hvanne défaut 1.5.
#' @param loi_orificenoye défaut "ifws", choisir "ferrette" ou "ifws".	
#' @importFrom stats predict
#'
#' @return Un tableau de données avec deux colonnes, 
#' "Q" le débit calculé en m3/s et "typecalc" 
#' le type de calcul "vanne fermee", "hmer>hvilaine", "canal aubuisson", 
#' "canal horton", "canal bazin",
#' "canal manning", "canal ifsw (noye)", "canal ifsw (libre)", 
#' "orifice noye (ferr)", "orifice noye (ifws) inf1.5" ou
#' "orifice noye (ifws) sup1.5".	
#' @export
#' @author Cedric Briand \email{cedric.briand@eaux-et-vilaine.bzh}, 
#' Stephanie Woimant #' \email{stephanie.woimant@eaux-et-vilaine.bzh} 
#' @examples
#' # Exploration des coefficients la formule de Bazin 
#' 
#' H=c(5:11)
#' Rh=c(2,2.25,2.5,2.8,3,3.2,3.4)
#' K=c(118,125,133,140,146,152,156)
#' plot(Rh,K)
#' points(Rh,predict(lm(K~Rh)),type="l")
#' gamma=87*Rh/(K+sqrt(Rh))
#' plot(H,gamma,pch="A")
#' df<-data.frame(H=H,gamma=gamma)
#' loess_gamma_bazin<-loess(gamma~H,data=df,control = 
#' loess.control(surface = "direct")) 
#' # contol pour extrapolation en dehors de la gamme
#' #save(loess_gamma_bazin,file=str_c("inst","loess_gamma_bazin.Rdata"))
#' df2<-data.frame(H=seq(from=5,to=11,by=0.1)) # pour les prédictions
#' df2$gamma<-predict(loess_gamma_bazin,newdata=df2)
#' points(df2$H,df2$gamma,col="red")
#' 
#' # utilisation de la fonction de débit pour une vanne
#' # chargement des paramètres et des données de la base
#' load(system.file("param2012_2014.Rdata", package = "SIVA"))
#' load(system.file("dat2019.Rdata", package = "SIVA"))
#' # Calcul du débit d'une vanne
#' Qva1 <-
#'   debit_vanne(
#'     hvilaine = dat$niveauvilaineb,
#'     hmer = dat$niveaumerb,
#'     hvanne = dat$vanne1,
#'     canal = "ifsw",
#'     Cvg = param["Cvg"],
#'     Cvgs = param["Cvgs"],
#'     Cvw = param["Cvw"],
#'     loi_orificenoye = "ifws"
#'   )
#' plot(Qva1[4000:5000,])
#' 
#' 
debit_vanne <- function(hvilaine, # vecteur
    hmer, #vecteur
    hvanne, # vecteur
    l=18, # largeur de la vanne
    g=9.81,
    canal="ifsw", # choisir bazin, aubuisson ou horton ou manning ou ifsw
    dm=73, #distance à la sonde amont pour formle basin
    dv=94, # distance à la sonde aval pour formule bazin
    Cma=0.65, #coefficient canal pour manning 
    Cho=.79, #coefficient canal pour horton
    m_oninf4=0.663, # Coefficient Orifice noyé noyé hvanne<4 (ferrette)
    m_onsup4=0.995, #Orifice noyé hvanne >4
    #m_od=0.65, # coefficient orifice dénoyé par l'aval (on n'utilise plus cette condition)
    Cvw=1, # coefficient ifsw canal
    Cvg=1.5, #coefficient ifsw orifice h1<=1.5 hvanne
    Cvgs=1.5, # coefficient ifsw h1>1.5 hvanne
    loi_orificenoye="ifws"#choisir "ferrette" ou ifws		
){
  # test initial	
  if (!(length(hmer)==length(hvilaine)&length(hmer)==length(hvanne))) stop("hmer, hmvilaine, hvanne doivent avoir la m\u00eame longueur")
  #initialisation des variables
  res<-data.frame("hvilaine"=hvilaine,"hmer"=hmer,"hvanne"=hvanne)
  res$Q<-rep(NA,length(hmer)) 
  res$typecalc<-rep(NA,length(hmer))
  h2<-hmer+7.72
  h1<-hvilaine+7.72
  delta<-hvilaine-hmer # y dans la formule
  # Conditions de calcul pour formules hydrauliques
  loicanal<-delta>0&hvanne>0&hvilaine<=(hvanne-7.72)
  loicanal[is.na(loicanal)]<-FALSE # pour éviter le plantiage lors de la réindexation
  loinoye<-delta>0&hvanne>0&hvilaine>(hvanne-7.72)#&hmer>=(hvanne-7.72)
  loinoye[is.na(loinoye)]<-FALSE
  # On n'utilise plus les loi d\u00e9noy\u00e9 par l'aval (orifice)
  #loidenoye<-delta>0&hvanne>0&hvilaine>(hvanne-7.72)&hmer<(hvanne-7.72)
  #loidenoye[is.na(loidenoye)]<-FALSE
  # Situations ou le débit est nul
  res$Q[hvanne==0]<-0
  res$typecalc[hvanne==0]<-"vanne fermee"
  res$Q[delta<=0]<-0
  res$typecalc[delta<=0]<-"hmer>hvilaine"	

# I CALCULS EN LOI DITE 'DE CANAL' ---------------------------------------------

  if (sum(loicanal)>0){
	
# I1- formule d'aubuisson simplifiée --------------------------------------------
	
    if (canal=="aubuisson"){
      
      res$typecalc[loicanal]<-"canal aubuisson"
      m=0.9
      res$Q[loicanal]= m*l*h2[loicanal]*(2*g*delta[loicanal])^1/2
      # Note Ferrette la formule est fausse !
      # la formule ci-dessus correspond au calcul d'Aldo

# I2- formule d'aubuisson complète (Horton) ------------------------------------------
			
    }  else if (canal=="horton"){
      # Même formule mais avec coeff de vitesse
      res$typecalc[loicanal]<-"canal horton"			
      a<-0.035
      Q0<-Cho*l*h2[loicanal]*(2*g*delta[loicanal])^1/2
      V<-Q0/(l*hvanne[loicanal])
      Q<-Cho*l*h2[loicanal]*(2*g*delta[loicanal]+a*(1.2*V)^2)^1/2
      res$Q[loicanal]<-Q

# I3- formule de Bazin (telle que calculée au barrage) -----------------------------------
	
    } else if (canal=="bazin"){
      load(file=system.file("loess_gamma_bazin.Rdata", package = "SIVA"))
      res$typecalc[loicanal]<-"canal bazin"
      # Dm=73
      # Dv=94
      # formules du barrage
      # les calculs font intervenir des sondes plus distantes ????
      I<-delta/(dm+dv) # pente hydraulique
      H<-hmer+I*dv+7.72 # # hauteur d'eau au niveau de la vanne
      #Rh=S/perimetre mouille
      Rh=l*H/(2*H+l)
      gamma <- stats::predict(loess_gamma_bazin,data.frame(H=H[loicanal]))
      res$Q[loicanal]<-gamma*H[loicanal]*l*sqrt(I[loicanal])

# I4- formule de Manning ------------------------------------------------------------------
		
    } else if (canal=="manning"){
      # Formule de Manning-Strickler
      res$typecalc[loicanal]<-"canal manning"
      res$Q[loicanal]<-Cma*hvanne*l*sqrt(2*g*delta[loicanal])
      #####################################	
# I5- formule utilisée dans Briand & Woimant (2015) ce rapport
      #####################################				
    }	 else if (canal=="ifsw"){
      # pour la formule il faut d'abord calculer un coefficient de débit.
      # delta=0.01
      # 40 = longueur du radier ?
      # 2 = longueur du bout (rond) du radier ?
      # 18 = largeur=l
      Cd<-((1-0.01*(10)/l)*(1-(0.01/2*h1)*(10)))^1.5	
      canaldenoye<-((h2/h1)<=0.98) & loicanal # free weir flow under gate
      canalnoye<-((h2/h1)>0.98) & loicanal # drowned weir flow under gate
      
      res$Q[canaldenoye]<-Cd[canaldenoye]*Cvw*(2/3)^1.5*g^0.5*l*h1[canaldenoye]^1.5
      res$typecalc[canaldenoye]<-"canal ifsw (libre)"	
      
      res$Q[canalnoye]<-Cd[canalnoye]*Cvw*(2/3)^1.5*g^0.5*l*h1[canalnoye]*((h1[canalnoye]-h2[canalnoye])/(1-0.98))^0.5				
      res$typecalc[canalnoye]<-"canal ifsw (noye)"
      
    }	else	{			
      stop("canal doit \u00eatre bazin, aubusson,  horton, manning ou ifsw")
    } # end canal
  }
	
# II CALCULS EN LOI 'ORIFICE NOYE' ---------------------------------------------

  if (sum(loinoye)>0){

# II 1- formule de ferrette (2004) ---------------------------------------------
	#
    if (loi_orificenoye=="ferrette"){
      m_on<-ifelse(hvanne[loinoye]<4,m_oninf4,m_onsup4) # source fichiers Aldo
      # U1 calculé par itérations successives
      Q0<-rep(0,sum(loinoye)) # vecteur des débits conditions initiales
      Q<-rep(1,sum(loinoye)) # vecteur des débits conditions initiales
      #i<-0
      V<-rep(1,sum(loinoye)) # vecteur des vitesses
      e<-hvanne[loinoye]
      H<-h2[loinoye]
      
      while(any(abs(Q-Q0)>0.01)){
        #cat(i)
        Q0<-m_on*l*e*(2*g*(delta[loinoye]+V^2/(2*g)))^1/2
        #cat(str_c(Q0[1],"\n"))
        V<-pmin(7,Q0/(l*H)) #sinon on a des infinis
        Q<-m_on*l*e*(2*g*(delta[loinoye]+V^2/(2*g)))^1/2
        #i<-i+1
      }
      res$Q[loinoye]<-Q
      res$typecalc[loinoye]<-"orifice noye (ferr)"	

# II 2 - formule utilisée dans Briand & Woimant (2015) --------------------------------
		
    } else if (loi_orificenoye=="ifws"){
      #noyage vanne
      loinoyeinf1.5<-(h1<=1.5*hvanne)&loinoye
      loinoyesup1.5<-(h1>1.5*hvanne)&loinoye
      ce<-0.61*(1+0.15*(l+2*hvanne)/(2*l+2*hvanne))
      Q<-ce[loinoyeinf1.5]*Cvg*hvanne[loinoyeinf1.5]*l*sqrt(2*g*delta[loinoyeinf1.5])
      res$Q[loinoyeinf1.5]<-Q
      res$typecalc[loinoyeinf1.5]<-"orifice noye (ifws) inf1.5"	
      Q<-ce[loinoyesup1.5]*Cvgs*hvanne[loinoyesup1.5]*l*sqrt(2*g*delta[loinoyesup1.5])
      res$Q[loinoyesup1.5]<-Q
      res$typecalc[loinoyesup1.5]<-"orifice noye (ifws) sup1.5"					
    }
  }

# III ORIFICE DENOYE (cette condition n'est plus appliquée car elles n'apparaît que marginalement pour
# des grandes ouvertures de vannes --------------------------------------------------------------

  
#	if (sum(loidenoye)>0){
#			res$typecalc[loidenoye]<-"orifice denoye ferrette"			
#			res$Q[loidenoye]<-m_od*l*hvanne[loidenoye]*sqrt(2*g*h1[loidenoye]/(1+m_od*hvanne[loidenoye]/h1[loidenoye]))
#		}
  return(res)	
}
