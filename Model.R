runModel<-function(mc=1,nB=12,Time = 1632,nrun=34,params=params,inits=inits,bankinits=bankinits,foldername="Rundata/"){
  library(dynlm)
  library(abind)
  ###Argument list
  args<-as.data.frame(array(data=0,dim=c(1,7),dimnames=list(NULL,c("t","m","i","nB","Time","mc","nrun"))))
  args$t=1
  args$m=0
  args$i=0
  args$nB=nB
  args$Time=Time
  args$mc=mc
  args$nrun=nrun

  #####Sectoral Structure####
  
  # HH column names
  HNames<-c("sav_h","V_h","D_h","YD","M","YD_e","c_d","rr_h","rr_he","alpha1","alpha2","M_d","H_dn","H_def","p_h","H","lev_h","iM","iD_h","M_sup","rep_m","M_np","TAA_h","W","u_nw","bal_h","MH","deltaM","V_he","r_mave","gb_h","gb_d","YD_tax","r_mavn","yd_e","v_he","pi_eh","LTV","LTVt","LTVgap")
  Households<-array(data = NA, dim=c(args$Time/args$nrun+48,length(HNames)),dimnames = list(NULL,HNames))
  
  # Firm column names
  FNames<-c("sav_f","V_f","D_f","K","k","LL","iD_f","p","theta","UC","WB","y","c","C","N_d","Y","I","i_d","i_des","lev_f","lev_fe","i","LL_d","u","u_e","u_e2","Pr_f","lev_ft","sav_ft","div_f","rep_LL","LL_np","iLL","bal_f","yfc","meanlev","delta_k","r_LLave","replacegap","u_n","r_LLavn","gk_des","UIC")
  Firms<-array(data = NA, dim=c(args$Time/args$nrun+48,length(FNames)),dimnames = list(NULL,FNames))
  
  # Bank column names (abm)
  bNames<-c("sav_b","pr_b","v_b","v_bb","a_b","r_b","iM_b","iLL_b","iD_hb","iD_fb","D_fb","iIB_b","M_b","M_supb","rep_mb","M_npb","LL_b","LL_supb","rep_LLb","LL_npb", "clear_b","clearalt_b","R_pb","R_paltb","R_tb","R_anb","IBL_sb","IBA_db","IBL_b","IBA_b","R_lb","div_bt","div_b","bb_b","r_LLb","r_Mb","r_db","CARb","LCRb","v_bbt","bal_bb","ir_b","ia_b","D_hb","carshare","rdshare","rllshare","rmshare","M_db","LL_db","bust","IBSshare","IBDshare","rationM","rationLL","rLLrel","LLlag","rMrel","Mlag","rDrel","Dhlag","Dflag","defrandh","defrandf","detshareLL","detshareM","detshareDh","detshareDf","randshareLL","randshareM","randshareDh","randshareDf","defrateM","defrateLL","markup_M","markup_LL","RWA_gap","clear","default_M","default_LL","v_bbe","v_bbe1","v_bbe2","v_bbe3","v_bbe4","fitvbb1","fitvbb2","fitvbb3","fitvbb4","maxfitvbb")
  # Bank column names (macro)
  BNames<-c("sav_B","Pr_b","V_b","V_bb","IBS","IBD","bb","CAR","LL_sup","Div_b","r_dav","r_IB","bal_b1","bal_b2","r_mav","r_LLav","Bust","r_mavr","r_LLavr","r_IBav")
  banks<-array(data = NA,dim=c(args$Time/args$nrun+48,length(bNames),args$nB),dimnames = list(NULL,bNames,NULL))
  Banks<-array(data = NA, dim=c(args$Time/args$nrun+48,length(BNames)),dimnames = list(NULL,BNames))
  
  # gov column names
  GNames<-c("PSBR","sav_g","V_g","r_gb","Tax","g","g_des","G","gb_s","gb","rep_gb","rep_gbcb","rep_gbh","bal_g","iGB","iGB_h","iGB_cb","tau","FTax","gap","gapr")
  Gov<-array(data = NA, dim=c(args$Time/args$nrun+48,length(GNames)),dimnames = list(NULL,GNames))
  
  # CB column names
  CBNames<-c("PCB","sav_cb","V_cb","A","pi_a","pi_m","pi_q","pi_e","r_cbd","r_cbl","R","gb_rcb","gb_cb","R_t","gb_dcb","gb_scb","buff_h","R_pd","cbint","CARt","bal_cb","u_ecb","iA","iR","pi_sa","targetCAR","CARgap")
  CB<-array(data = NA, dim=c(args$Time/args$nrun+48,length(CBNames)),dimnames = list(NULL,CBNames))
  
  # auxiliary stuff
  AuxNames<-c("porthgb","SFCcheck1","SFCcheck2","SFCcheck3","Rshock","sdev_deff","sdev_defh","SFCcheck4")
  Aux<-array(data = NA, dim=c(args$Time/args$nrun+48,length(AuxNames)),dimnames = list(NULL,AuxNames))
  
  #TS needed for estimations
  Estnames<-c("defrateM","defrateLL","r_LLb","r_Mb","iLL_b","iM_b","v_bb")
  Est<-array(data=NA,dim=c(args$Time+48,length(Estnames),args$nB),dimnames=list(NULL,Estnames,NULL))
  
  #####Data Frames#####
  datanames<-c(HNames,FNames,GNames,CBNames,BNames,AuxNames)
  #Annual data
  yearoutput<-as.data.frame(array(data = NA, dim=c((args$Time+48)/48,length(datanames)),dimnames = list(NULL,datanames)))
  #quarterly data
  quarteroutput<-as.data.frame(array(data = NA, dim=c((args$Time+48)/12,length(datanames)),dimnames = list(NULL,datanames)))
  #monthly data
  monthoutput<-as.data.frame(array(data = NA, dim=c((args$Time+48)/4,length(datanames)),dimnames = list(NULL,datanames)))
  #weekly data
  weekoutput<-as.data.frame(array(data = NA, dim=c((args$Time+48),length(datanames)),dimnames = list(NULL,datanames)))
  #average bank data
  banksavm<-as.data.frame(array(data = NA, dim=c((args$Time+48)/4,length(bNames)),dimnames = list(NULL,bNames)))
  banksavq<-as.data.frame(array(data = NA, dim=c((args$Time+48)/12,length(bNames)),dimnames = list(NULL,bNames)))
  banksava<-as.data.frame(array(data = NA, dim=c((args$Time+48)/48,length(bNames)),dimnames = list(NULL,bNames)))
  #cross-section bank data
  mbnames<-paste("bank",1:args$nB,"m",sep="")
  qbnames<-paste("bank",1:args$nB,"q",sep="")
  abnames<-paste("bank",1:args$nB,"a",sep="")
  for(b in 1:args$nB){
    dframem<-as.data.frame(array(data = NA, dim=c((args$Time+48)/4,length(bNames)),dimnames = list(NULL,bNames)))
    dframeq<-as.data.frame(array(data = NA, dim=c((args$Time+48)/12,length(bNames)),dimnames = list(NULL,bNames)))
    dframea<-as.data.frame(array(data = NA, dim=c((args$Time+48)/48,length(bNames)),dimnames = list(NULL,bNames)))
    assign(mbnames[b],dframem)
    assign(qbnames[b],dframeq)
    assign(abnames[b],dframea)
  }
  
  
  ######THIS IS THE MONTE CARLO REPETITION#####
  for(m in 0:(args$mc-1)){
    #update
    args$m=(args$m+1)
    print(c("Entering MC repetition", args$m, "of", args$mc))
    args$t=1
    #setting the seed for reproducibility
    set.seed(args$m)
    drawFirm<-c(NA)
    drawBank<-c(rep(NA,args$nB/4))
    rLLrel<-c(rep(NA,args$nB))
    rDrel<-c(rep(NA,args$nB))
    rMrel<-c(rep(NA,args$nB))
    Dhlag<-c(rep(NA,args$nB))
    Dflag<-c(rep(NA,args$nB))
    Mlag<-c(rep(NA,args$nB))
    LLlag<-c(rep(NA,args$nB))
    Hrandm<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    Hrandd<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    Frand<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    Frandd<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    
    library(MASS)

    mu <- rep(0,args$nB)
    Sigma <- matrix(params[1,"CC_def"], nrow=args$nB, ncol=args$nB) + diag(args$nB)*params[1,"CC_def"]
 
    rawvarsh <- mvrnorm(n=Time+48, mu=mu, Sigma=Sigma)
    rawvarsf <- mvrnorm(n=Time+48, mu=mu, Sigma=Sigma)
    
    pvarsh <- pnorm(rawvarsh)
    pvarsf <- pnorm(rawvarsf)
    
    
  #######Initial values####
  Households[1,"D_h"]<-inits[[1,"D_h"]]
  Firms[1,"D_f"]<-inits[1,"D_f"]
  Households[1,"M"]<-inits[[1,"M"]]
  Households[1,"p_h"]<-inits[[1,"p_h"]]
  Firms[1,"LL"]<-inits[[1,"LL"]]
  Firms[1,"k"]<-inits[[1,"k"]]
  Firms[1,"p"]<-inits[[1,"p"]]
  Households[1,"V_h"]<-inits[[1,"V_h"]]
  Households[1,"c_d"]<-inits[[1,"c_d"]]
  Households[1,"alpha1"]<-inits[[1,"alpha1"]]
  Households[1,"alpha2"]<-inits[[1,"alpha2"]]
  Households[1,"H_dn"]<-inits[[1,"H_dn"]]
  Firms[1,"u"]<-inits[[1,"u"]]
  Households[1,"W"]<-inits[[1,"W"]]
  Firms[1,"theta"]<-inits[[1,"theta"]]
  Firms[1,"lev_f"]<-inits[[1,"lev_f"]]
  Firms[1,"lev_ft"]<-inits[[1,"lev_ft"]]
  Firms[1,"yfc"]<-inits[[1,"yfc"]]
  Firms[1,"y"]<-inits[[1,"y"]]
  Firms[1,"Pr_f"]<-inits[[1,"Pr_f"]]
  Firms[1,"div_f"]<-inits[[1,"div_f"]]
  Firms[1,"sav_ft"]<-inits[[1,"sav_ft"]]
  Banks[1,"r_dav"]<-inits[[1,"r_dav"]]
  Households[1,"TAA_h"]<-inits[[1,"TAA_h"]]
  Aux[1,"porthgb"]<-inits[[1,"porthgb"]]
  Households[1,"gb_h"]<-inits[[1,"gb_h"]]
  Gov[1,"g_des"]<-params[1,"g0"]/48
  Gov[1,"Tax"]<-inits[[1,"Tax"]]
  Gov[1,"tau"]<-params[[1,"tau"]]
  CB[1,"gb_cb"]<-inits[[1,"gb_cb"]]
  Gov[1,"gb"]<-inits[[1,"gb"]]
  Gov[1,"r_gb"]<-inits[[1,"r_gb"]]
  CB[1,"pi_sa"]<-inits[[1,"pi_q"]]
  CB[1,"r_cbd"]<-inits[[1,"r_cbd"]]
  CB[1,"r_cbl"]<-inits[[1,"r_cbl"]]
  CB[1,"buff_h"]<-inits[[1,"buff_h"]]
  Banks[1,"r_IB"]<-inits[[1,"r_IB"]]
  Firms[1,"LL_d"]<-inits[[1,"LL_sup"]]
  Households[1,"M_d"]<-inits[[1,"M_sup"]]
  Households[1,"YD_e"]<-inits[[1,"YD_e"]]
  Households[1,"yd_e"]<-inits[[1,"YD_e"]]
  Households[1,"v_he"]<-inits[[1,"V_h"]]
  Households[1,"YD"]<-inits[[1,"YD"]]
  Households[1,"rr_he"]<-inits[[1,"rr_he"]]
  Households[1,"rr_h"]<-inits[[1,"rr_h"]]
  Firms[1,"lev_fe"]<-inits[[1,"lev_fe"]]
  Firms[1,"c"]<-inits[[1,"c"]]
  Firms[1,"u_e"]<-inits[[1,"u_e"]]
  Firms[1,"u_e2"]<-inits[[1,"u_e"]]
  CB[1,"u_ecb"]<-inits[[1,"u_ecb"]]
  CB[1,"pi_e"]<-inits[[1,"pi_e"]]
  Aux[1,"Rshock"]<-inits[[1,"Rshock"]]
  Banks[1,"r_mav"]<-bankinits[[1,"r_Mb"]]
  Banks[1,"r_LLav"]<-bankinits[[1,"r_LLb"]]
  Banks[1,"r_mavr"]<-bankinits[[1,"r_Mb"]]
  Banks[1,"r_LLavr"]<-bankinits[[1,"r_LLb"]]
  Households[1,"V_he"]<-inits[[1,"V_h"]]
  Households[1,"r_mave"]<-bankinits[[1,"r_Mb"]]
  Firms[1,"r_LLave"]<-bankinits[[1,"r_LLb"]]
  Households[1,"YD_tax"]<-inits[[1,"YD_tax"]]
  Banks[1,"bb"]<-inits[1,"bb"]
  Firms[1,"u_n"]<-inits[1,"u"]
  Gov[1,"PSBR"]<-0
  Firms[1,"replacegap"]<-0
  Firms[1,"r_LLavn"]<-bankinits[1,"r_LLb"]
  Households[1,"r_mavn"]<-bankinits[1,"r_Mb"]
  Firms[1,"gk_des"]<-0
  Firms[1,"UIC"]<-inits[1,"UIC"]
  Firms[1,"iLL"]<-inits[1,"iLL"]
  Firms[1,"iD_f"]<-bankinits[1,"r_db"]*inits[1,"D_f"]/48
  Households[1,"pi_eh"]<-0
  Households[1,"LTV"]<-params[1,"LTV"]
  CB[1,"targetCAR"]<-0.1
  Gov[1,"g"]<-params[1,"g0"]/48
  Households[1,"LTVt"]<-params[1,"LTV"]
  Gov[1,"gap"]<-0
  Gov[1,"gapr"]<-0
  Firms[1,"i_d"]<-params[1,"delta_k"]*inits[1,"k"]
  Households[1,"LTVgap"]<-0
  CB[1,"CARgap"]<-0
  Firms[1,"K"]<-inits[[1,"K"]]
  
    ##bankinits
  banks[1,"D_fb",]<-bankinits[,"D_fb"]
  banks[1,"r_LLb",]<-bankinits[,"r_LLb"]
  banks[1,"r_Mb",]<-bankinits[,"r_Mb"]
  banks[1,"r_db",]<-bankinits[,"r_db"]
  banks[1,"D_hb",]<-bankinits[,"D_hb"]
  banks[1,"LL_b",]<-bankinits[,"LL_b"]
  banks[1,"M_b",]<-bankinits[,"M_b"]
  banks[1,"a_b",]<-bankinits[,"a_b"]
  banks[1,"r_b",]<-bankinits[,"r_b"]
  banks[1,"IBL_b",]<-bankinits[,"IBL_b"]
  banks[1,"IBA_b",]<-bankinits[,"IBA_b"]
  banks[1,"LL_supb",]<-bankinits[,"LL_supb"]
  banks[1,"LL_db",]<-bankinits[,"LL_db"]
  banks[1,"M_supb",]<-bankinits[,"M_supb"]
  banks[1,"M_db",]<-bankinits[,"M_db"]
  banks[1,"v_bb",]<-bankinits[,"v_bb"]
  banks[1,"v_bbt",]<-bankinits[,"v_bb"]
  banks[1,"pr_b",]<-bankinits[,"pr_b"]
  banks[1,"defrandh",]<-1
  banks[1,"defrandf",]<-1
  banks[1,"v_bb",]<-bankinits[,"v_bb"]
  banks[1,"div_bt",]<-bankinits[,"div_b"]
  banks[1,"bb_b",]<-bankinits[,"bb_b"]
  banks[1,"clear_b",]<-bankinits[,"clear_b"]
  banks[1,"defrateM",]<-bankinits[,"defrateM"]
  banks[1,"defrateLL",]<-bankinits[,"defrateLL"]
  banks[1,"markup_M",]<-bankinits[,"markup_M"]
  banks[1,"markup_LL",]<-bankinits[,"markup_LL"]
  banks[1,"v_bbe",]<-bankinits[,"v_bbe"]
  banks[1,"RWA_gap",]<-0
  banks[1,"v_bbe1",]<-bankinits[,"v_bbe1"]
  banks[1,"v_bbe2",]<-bankinits[,"v_bbe2"]
  banks[1,"v_bbe3",]<-bankinits[,"v_bbe3"]
  banks[1,"v_bbe4",]<-bankinits[,"v_bbe4"]
  Est[1,"defrateM",]<-bankinits[,"defrateM"]
  Est[1,"defrateLL",]<-bankinits[,"defrateLL"]
  Est[1,"v_bb",]<-bankinits[,"v_bb"]
  
  
    #####THIS IS THE PERIOD REPETITION WITHIN 1 MONTE CARLO SIMULATION#####
    r=1
    Timer=1
    for(r in 1:args$nrun){
    if(r>1){
    args$t=48
    marker=48
    Households[1:48,]<-Households[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    Households[49:(args$Time/args$nrun+48),]<-NA
    Firms[1:48,]<-Firms[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    Firms[49:(args$Time/args$nrun+48),]<-NA
    banks[1:48,,]<-banks[(args$Time/args$nrun+1):(args$Time/args$nrun+48),,]
    banks[49:(args$Time/args$nrun+48),,]<-NA
    Banks[1:48,]<-Banks[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    Banks[49:(args$Time/args$nrun+48),]<-NA
    Gov[1:48,]<-Gov[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    Gov[49:(args$Time/args$nrun+48),]<-NA
    CB[1:48,]<-CB[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    CB[49:(args$Time/args$nrun+48),]<-NA
    Aux[1:48,]<-Aux[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    Aux[49:(args$Time/args$nrun+48),]<-NA
    }else{
    args$t=1
    marker=1
    }
    for(t in marker:(args$Time/args$nrun+47)){
      #update
      Timer=Timer+1
      args$t=(args$t+1)
      #PERIOD SIMULATION
      print(c(args$m,r,args$t))
      banks<-bankreps(banks=banks,params=params,args=args)
      indreturns<-defindicators(Firms=Firms,Households=Households,params=params,args=args)
      Households<-indreturns$Households
      Firms<-indreturns$Firms
      defreturns<-defaults(banks=banks,Firms=Firms,Households=Households,Aux=Aux,params=params,args=args,pvarsh=pvarsh,pvarsf=pvarsf,Timer=Timer)
      banks<-defreturns$banks
      Aux<-defreturns$Aux
      banks<-rationindicators(banks=banks,Firms=Firms,Households=Households,params=params,args=args)
      banks<-bankinterest(banks=banks,CB=CB,Banks=Banks,params=params,args=args)
      CB<-cbinterest(banks=banks,CB=CB,params=params,args=args)
      Firms<-fbankpayments(Firms=Firms,banks=banks,params=params,args=args)
      Firms<-Fexp(Firms=Firms,Banks=Banks,params=params,args=args,Timer=Timer)
      Firms<-fullcapy(Firms=Firms,params=params,args=args)
      banks<-bankexp(banks=banks,Est=Est,CB=CB,params=params,args=args,Timer=Timer)
      Households<-Hexp(Firms=Firms,CB=CB,Households=Households,Banks=Banks,params=params,args=args)
      banks<-rshares(banks=banks,params=params,args=args)
      Households<-setwage(Firms=Firms,CB=CB,Households=Households,params=params,args=args)
      pricingreturn<-setpricemkup(Firms=Firms,Aux=Aux,Households=Households,params=params,args=args)
      Firms<-pricingreturn$Firms
      Aux<-pricingreturn$Aux
      CB<-infmeasures(Firms=Firms,CB=CB,params=params,args=args)
      CB<-infmeasures2(Firms=Firms,CB=CB,params=params,args=args)
      Banks<-averagerates(banks=banks,Banks=Banks,CB=CB,params=params,args=args)
      Households<-updateMPC(Households=Households,params=params,args=args)
      CB<-CBexp(CB=CB,Firms=Firms,params=params,args=args)
      Households<-decideConsumption(Households=Households,Firms=Firms,params=params,args=args)
      Households<-ndemandhouse(Households=Households,Banks=Banks,params=params,args=args,Timer=Timer)
      Firms<-ftleverage(Firms=Firms,params=params,args=args)
      Firms<-decideInvestment(Firms=Firms,Banks=Banks,params=params,args=args,Timer=Timer)
      Households<-mortdeppayments(Households=Households,banks=banks,params=params,args=args)
      Gov<-gbint(Gov=Gov,CB=CB,Households=Households,params=params,args=args)
      Gov<-decidetax(Gov=Gov,Firms=Firms,Households=Households,Banks=Banks,params=params,args=args)
      CB<-CBprofit(Gov=Gov,CB=CB,params=params,args=args)
      Gov<-decidegovspend(Households=Households,Firms=Firms,CB=CB,Gov=Gov,params=params,args=args)
      CB<-setcbrate(CB=CB,Firms=Firms,Households=Households,params=params,args=args)
      CB<-targetratios(CB=CB,Firms=Firms,Households=Households,Aux=Aux,params=params,args=args)
      estreturn<-bankestimates1(banks=banks,Est=Est,Firms=Firms,Households=Households,params=params,args=args,drawBank=drawBank,Timer=Timer)
      banks<-estreturn$banks
      drawBank<-estreturn$drawBank
      banks<-bankestimates2(banks=banks,CB=CB,Est=Est,Firms=Firms,Households=Households,params=params,args=args,drawBank=drawBank,Timer=Timer)
      ratereturn<-setbankrates(banks=banks,CB=CB,Banks=Banks,Firms=Firms,Households=Households,params=params,args=args,drawBank=drawBank)
      banks<-ratereturn$banks
      Banks<-ratereturn$Banks
      Firms<-tsave(Firms=Firms,params=params,args=args)
      Firms<-floandemand(Firms=Firms,params=params,args=args)
      mdisreturn<-Mdistr(banks=banks,Households=Households,params=params,args=args,Hrandm=Hrandm)
      banks<-mdisreturn$banks
      Hrandm<-mdisreturn$Hrandm
      lldisreturn<-LLdistr(banks=banks,Firms=Firms,Banks=Banks,params=params,args=args,Frand=Frand)
      banks<-lldisreturn$banks
      Frand<-lldisreturn$Frand
      banks<-rationcredit(banks=banks,Firms=Firms,CB=CB,params=params,args=args)
      banks<-loanstocks(banks=banks,CB=CB,params=params,args=args)
      Banks<-llsupply(banks=banks,Banks=Banks,params=params,args=args)
      Households<-newmort(Households=Households,banks=banks,params=params,args=args)
      Firms<-actuali(Firms=Firms,Banks=Banks,params=params,args=args)
      Firms<-decidey(Firms=Firms,Gov=Gov,Households=Households,params=params,args=args)
      returnnominal<-nominalcigy(Firms=Firms,Gov=Gov,params=params,args=args)
      Firms<-returnnominal$Firms
      Gov<-returnnominal$Gov
      Firms<-Fprofit(Firms=Firms,Gov=Gov,params=params,args=args)
      Firms<-divf(Firms=Firms,params=params,args=args)
      Firms<-FSave(Firms=Firms,Gov=Gov,params=params,args=args)
      Firms<-Capital(Firms=Firms,params=params,args=args)
      Firms<-Floanswealth(Firms=Firms,Banks=Banks,params=params,args=args)
      Dfdisreturn<-Dfdistr(banks=banks,Firms=Firms,params=params,args=args,Frandd=Frandd)
      banks<-Dfdisreturn$banks
      Frandd<-Dfdisreturn$Frandd
      Households<-efdemandhouse(Households=Households,params=params,args=args)
      banks<-bprofit(CB=CB,banks=banks,Banks=Banks,params=params,args=args)
      Banks<-Bprofit(banks=banks,Banks=Banks,params=params,args=args)
      div1return<-bbonddivs1(banks=banks,Banks=Banks,CB=CB,params=params,args=args)
      Banks<-div1return$Banks
      banks<-div1return$banks
      Banks<-bankdivsbbint(banks=banks,Banks=Banks,params=params,args=args)
      Gov<-Borrowreq(Gov=Gov,CB=CB,params=params,args=args)
      Gov<-supplygb(Gov=Gov,CB=CB,params=params,args=args)
      Gov<-setgbrate(Gov=Gov,CB=CB,Households=Households,Aux=Aux,Banks=Banks,params=params,args=args)
      hportreturn<-Hportfolio(Households=Households,Gov=Gov,Banks=Banks,Aux=Aux,params=params,args=args)
      Households<-hportreturn$Households
      Aux<-hportreturn$Aux
      CB<-monetisedef(CB=CB,Gov=Gov,Households=Households,params=params,args=args)
      Gov<-gbstock(Gov=Gov,Households=Households,CB=CB,params=params,args=args)
      Gov<-Gwealth(Gov=Gov,params=params,args=args)
      banks<-bsave(CB=CB,banks=banks,params=params,args=args)
      Banks<-Bsave(banks=banks,Banks=Banks,params=params,args=args)
      Banks<-bbstock(banks=banks,Banks=Banks,params=params,args=args)
      banks<-IB1(banks=banks,params=params,args=args)
      Rtreturn<-Rtarget(CB=CB,banks=banks,Aux=Aux,params=params,args=args)
      CB<-Rtreturn$CB
      Aux<-Rtreturn$Aux
      Households<-Hidentities1(Households=Households,Banks=Banks,CB=CB,Firms=Firms,Gov=Gov,params=params,args=args)
      CB<-CBsolvecyc(CB=CB,banks=banks,Banks=Banks,Households=Households,Gov=Gov,params=params,args=args)
      CB<-intervenegb(CB=CB,params=params,args=args)
      CB<-gbcbstock(CB=CB,Gov=Gov,params=params,args=args)
      Households<-hbonds(Households=Households,Gov=Gov,CB=CB,params=params,args=args)
      Households<-Hidentities2(Households=Households,Banks=Banks,CB=CB,Firms=Firms,Gov=Gov,params=params,args=args)
      dhdisreturn<-Dhdistr(banks=banks,Households=Households,params=params,args=args,Hrandd=Hrandd)
      banks<-dhdisreturn$banks
      Hrandd<-dhdisreturn$Hrandd
      Households<-TAAlevh(Households=Households,Banks=Banks,params=params,args=args)
      banks<-IB2(banks=banks,params=params,args=args)
      IB3return<-IB3(banks=banks,Banks=Banks,CB=CB,params=params,args=args)
      banks<-IB3return$banks
      Banks<-IB3return$Banks
      banks<-badv(banks=banks,params=params,args=args)
      banks<-bres(banks=banks,params=params,args=args)
      banks<-bwealth(banks=banks,params=params,args=args)
      Banks<-Bwealth(banks=banks,Banks=Banks,params=params,args=args)
      banks<-regratios(banks=banks,params=params,args=args)
      Banks<-aggregateCAR(banks=banks,CB=CB,Banks=Banks,params=params,args=args)
      CB<-RAstock(CB=CB,banks=banks,params=params,args=args)
      CB<-CBwealth(CB=CB,banks=banks,params=params,args=args)
      Households<-hhreturns(Households=Households,Gov=Gov,Banks=Banks,CB=CB,params=params,args=args)
      SFBreturns<-balances(Households=Households,Firms=Firms,Gov=Gov,CB=CB,banks=banks,Banks=Banks,Aux=Aux,params=params,args=args)
      Households<-SFBreturns$Households
      Firms<-SFBreturns$Firms
      Gov<-SFBreturns$Gov
      CB<-SFBreturns$CB
      banks<-SFBreturns$banks
      Banks<-SFBreturns$Banks
      Aux<-SFBreturns$Aux
      Est<-collectdata(Est=Est,banks=banks,params=params,args=args,Timer=Timer)
    }
    if(r==1){
      Timer=(args$Time/args$nrun+48)
      Householdsw<-Households
      Firmsw<-Firms
      Govw<-Gov
      CBw<-CB
      Banksw<-Banks
      banksw<-banks
      banksavw<-as.data.frame(apply(banks,c(1,2),mean))
      Auxw<-Aux
      weekoutput<-as.data.frame(cbind(Householdsw,Firmsw,Govw,CBw,Banksw,Auxw))
    }else{
      Householdsw<-Households[49:(args$Time/args$nrun+48),]
      Firmsw<-Firms[49:(args$Time/args$nrun+48),]
      Govw<-Gov[49:(args$Time/args$nrun+48),]
      CBw<-CB[49:(args$Time/args$nrun+48),]
      Banksw<-Banks[49:(args$Time/args$nrun+48),]
      banksw2<-banks[49:(args$Time/args$nrun+48),,]
      banksw<-abind(banksw,banksw2,along=1)
      banksavw2<-as.data.frame(apply(banksw2,c(1,2),mean))
      Auxw<-Aux[49:(args$Time/args$nrun+48),]
      weekoutput2<-as.data.frame(cbind(Householdsw,Firmsw,Govw,CBw,Banksw,Auxw))
      weekoutput<-rbind(weekoutput,weekoutput2)
      banksavw<-rbind(banksavw,banksavw2)
    }
    }
  #DATA COLLECTION####
  for(l in 1:ncol(weekoutput)){
        if(is.na(weekoutput[1,l])==TRUE){
          weekoutput[1,l]<-weekoutput[2,l]
        }
      }
  for(l in 1:ncol(banksavw)){
      if(is.na(banksavw[1,l])==TRUE){
      banksavw[1,l]<-banksavw[2,l]
      }
      }
  month=0
  for(j in 1:nrow(weekoutput)){
    if(j/4==round(j/4)){
    month=month+1
    monthoutput$c[(j-3*month)]<-sum(weekoutput$c[(j-3):j])
    monthoutput$c_d[(j-3*month)]<-sum(weekoutput$c_d[(j-3):j])
    monthoutput$sav_h[(j-3*month)]<-sum(weekoutput$sav_h[(j-3):j])
    monthoutput$i_d[(j-3*month)]<-sum(weekoutput$i_d[(j-3):j])
    monthoutput$i[(j-3*month)]<-sum(weekoutput$i[(j-3):j])
    monthoutput$i_des[(j-3*month)]<-mean(weekoutput$i_des[(j-3):j])
    monthoutput$YD_e[(j-3*month)]<-mean(weekoutput$YD_e[(j-3):j])
    monthoutput$M[(j-3*month)]<-mean(weekoutput$M[(j-3):j])
    monthoutput$V_h[(j-3*month)]<-mean(weekoutput$V_h[(j-3):j])
    monthoutput$D_h[(j-3*month)]<-mean(weekoutput$D_h[(j-3):j])
    monthoutput$rr_h[(j-3*month)]<-mean(weekoutput$rr_h[(j-3):j])
    monthoutput$rr_he[(j-3*month)]<-mean(weekoutput$rr_he[(j-3):j])
    monthoutput$alpha1[(j-3*month)]<-mean(weekoutput$alpha1[(j-3):j])
    monthoutput$alpha2[(j-3*month)]<-mean(weekoutput$alpha2[(j-3):j])
    monthoutput$lev_h[(j-3*month)]<-mean(weekoutput$lev_h[(j-3):j])
    monthoutput$p_h[(j-3*month)]<-mean(weekoutput$p_h[(j-3):j])
    monthoutput$H[(j-3*month)]<-mean(weekoutput$H[(j-3):j])
    monthoutput$M_d[(j-3*month)]<-sum(weekoutput$M_d[(j-3):j])
    monthoutput$YD[(j-3*month)]<-sum(weekoutput$YD[(j-3):j])
    monthoutput$bal_h[(j-3*month)]<-sum(weekoutput$bal_h[(j-3):j])
    monthoutput$bal_f[(j-3*month)]<-sum(weekoutput$bal_f[(j-3):j])
    monthoutput$bal_g[(j-3*month)]<-sum(weekoutput$bal_g[(j-3):j])
    monthoutput$bal_cb[(j-3*month)]<-sum(weekoutput$bal_cb[(j-3):j])
    monthoutput$bal_b1[(j-3*month)]<-sum(weekoutput$bal_b1[(j-3):j])
    monthoutput$bal_b2[(j-3*month)]<-sum(weekoutput$bal_b2[(j-3):j])
    monthoutput$H_dn[(j-3*month)]<-sum(weekoutput$H_dn[(j-3):j])
    monthoutput$H_def[(j-3*month)]<-sum(weekoutput$H_def[(j-3):j])
    monthoutput$iM[(j-3*month)]<-sum(weekoutput$iM[(j-3):j])
    monthoutput$iLL[(j-3*month)]<-sum(weekoutput$iLL[(j-3):j])
    monthoutput$iD_h[(j-3*month)]<-sum(weekoutput$iD_h[(j-3):j])
    monthoutput$M_sup[(j-3*month)]<-sum(weekoutput$M_sup[(j-3):j])
    monthoutput$rep_m[(j-3*month)]<-sum(weekoutput$rep_m[(j-3):j])
    monthoutput$rep_LL[(j-3*month)]<-sum(weekoutput$rep_LL[(j-3):j])
    monthoutput$M_np[(j-3*month)]<-sum(weekoutput$M_np[(j-3):j])
    monthoutput$TAA_h[(j-3*month)]<-mean(weekoutput$TAA_h[(j-3):j])
    monthoutput$W[(j-3*month)]<-mean(weekoutput$W[(j-3):j])
    monthoutput$u_nw[(j-3*month)]<-mean(weekoutput$u_nw[(j-3):j])
    monthoutput$MH[(j-3*month)]<-mean(weekoutput$MH[(j-3):j])
    monthoutput$sav_f[(j-3*month)]<-sum(weekoutput$sav_f[(j-3):j])
    monthoutput$deltaM[(j-3*month)]<-sum(weekoutput$deltaM[(j-3):j])
    monthoutput$V_f[(j-3*month)]<-mean(weekoutput$V_f[(j-3):j])
    monthoutput$K[(j-3*month)]<-mean(weekoutput$K[(j-3):j])
    monthoutput$k[(j-3*month)]<-mean(weekoutput$k[(j-3):j])
    monthoutput$LL[(j-3*month)]<-mean(weekoutput$LL[(j-3):j])
    monthoutput$p[(j-3*month)]<-mean(weekoutput$p[(j-3):j])
    monthoutput$theta[(j-3*month)]<-mean(weekoutput$theta[(j-3):j])
    monthoutput$UC[(j-3*month)]<-mean(weekoutput$UC[(j-3):j])
    monthoutput$WB[(j-3*month)]<-sum(weekoutput$WB[(j-3):j])
    monthoutput$y[(j-3*month)]<-sum(weekoutput$y[(j-3):j])
    monthoutput$C[(j-3*month)]<-sum(weekoutput$C[(j-3):j])
    monthoutput$N_d[(j-3*month)]<-sum(weekoutput$N_d[(j-3):j])
    monthoutput$Y[(j-3*month)]<-sum(weekoutput$Y[(j-3):j])
    monthoutput$I[(j-3*month)]<-sum(weekoutput$I[(j-3):j])
    monthoutput$LL_d[(j-3*month)]<-sum(weekoutput$LL_d[(j-3):j])
    monthoutput$lev_f[(j-3*month)]<-mean(weekoutput$lev_f[(j-3):j])
    monthoutput$lev_fe[(j-3*month)]<-mean(weekoutput$lev_fe[(j-3):j])
    monthoutput$u[(j-3*month)]<-mean(weekoutput$u[(j-3):j])
    monthoutput$u_e[(j-3*month)]<-mean(weekoutput$u_e[(j-3):j])
    monthoutput$lev_ft[(j-3*month)]<-mean(weekoutput$lev_ft[(j-3):j])
    monthoutput$Pr_f[(j-3*month)]<-sum(weekoutput$Pr_f[(j-3):j])
    monthoutput$LL_np[(j-3*month)]<-sum(weekoutput$LL_np[(j-3):j])
    monthoutput$sav_ft[(j-3*month)]<-sum(weekoutput$sav_ft[(j-3):j])
    monthoutput$div_f[(j-3*month)]<-sum(weekoutput$div_f[(j-3):j])
    monthoutput$yfc[(j-3*month)]<-mean(weekoutput$yfc[(j-3):j])
    monthoutput$meanlev[(j-3*month)]<-mean(weekoutput$meanlev[(j-3):j])
    monthoutput$sav_g[(j-3*month)]<-sum(weekoutput$sav_g[(j-3):j])
    monthoutput$gb_h[(j-3*month)]<-mean(weekoutput$gb_h[(j-3):j])
    monthoutput$r_gb[(j-3*month)]<-mean(weekoutput$r_gb[(j-3):j])
    monthoutput$V_g[(j-3*month)]<-mean(weekoutput$V_g[(j-3):j])
    monthoutput$PSBR[(j-3*month)]<-sum(weekoutput$PSBR[(j-3):j])
    monthoutput$sav_g[(j-3*month)]<-sum(weekoutput$sav_g[(j-3):j])
    monthoutput$Tax[(j-3*month)]<-sum(weekoutput$Tax[(j-3):j])
    monthoutput$g[(j-3*month)]<-sum(weekoutput$g[(j-3):j])
    monthoutput$G[(j-3*month)]<-sum(weekoutput$G[(j-3):j])
    monthoutput$gb_s[(j-3*month)]<-sum(weekoutput$gb_s[(j-3):j])
    monthoutput$g_des[(j-3*month)]<-mean(weekoutput$g_des[(j-3):j])
    monthoutput$rep_gb[(j-3*month)]<-sum(weekoutput$rep_gb[(j-3):j])
    monthoutput$rep_gbcb[(j-3*month)]<-sum(weekoutput$rep_gbcb[(j-3):j])
    monthoutput$rep_gbh[(j-3*month)]<-sum(weekoutput$rep_gbh[(j-3):j])
    monthoutput$iGB[(j-3*month)]<-sum(weekoutput$iGB[(j-3):j])
    monthoutput$iGB_h[(j-3*month)]<-sum(weekoutput$iGB_h[(j-3):j])
    monthoutput$iGB_cb[(j-3*month)]<-sum(weekoutput$iGB_cb[(j-3):j])
    monthoutput$PCB[(j-3*month)]<-sum(weekoutput$PCB[(j-3):j])
    monthoutput$sav_cb[(j-3*month)]<-sum(weekoutput$sav_cb[(j-3):j])
    monthoutput$V_cb[(j-3*month)]<-mean(weekoutput$V_cb[(j-3):j])
    monthoutput$A[(j-3*month)]<-mean(weekoutput$A[(j-3):j])
    monthoutput$pi_a[(j-3*month)]<-mean(weekoutput$pi_a[(j-3):j])
    monthoutput$pi_m[(j-3*month)]<-mean(weekoutput$pi_m[(j-3):j])
    monthoutput$pi_q[(j-3*month)]<-mean(weekoutput$pi_q[(j-3):j])
    monthoutput$pi_e[(j-3*month)]<-mean(weekoutput$pi_e[(j-3):j])
    monthoutput$r_cbd[(j-3*month)]<-mean(weekoutput$r_cbd[(j-3):j])
    monthoutput$r_cbl[(j-3*month)]<-mean(weekoutput$r_cbl[(j-3):j])
    monthoutput$R[(j-3*month)]<-mean(weekoutput$R[(j-3):j])
    monthoutput$gb_rcb[(j-3*month)]<-sum(weekoutput$gb_rcb[(j-3):j])
    monthoutput$gb_cb[(j-3*month)]<-mean(weekoutput$gb_cb[(j-3):j])
    monthoutput$R_t[(j-3*month)]<-mean(weekoutput$R_t[(j-3):j])
    monthoutput$gb_dcb[(j-3*month)]<-sum(weekoutput$gb_dcb[(j-3):j])
    monthoutput$gb_scb[(j-3*month)]<-sum(weekoutput$gb_scb[(j-3):j])
    monthoutput$buff_h[(j-3*month)]<-mean(weekoutput$buff_h[(j-3):j])
    monthoutput$R_pd[(j-3*month)]<-mean(weekoutput$R_pd[(j-3):j])
    monthoutput$cbint[(j-3*month)]<-sum(weekoutput$cbint[(j-3):j])
    monthoutput$CARt[(j-3*month)]<-mean(weekoutput$CARt[(j-3):j])
    monthoutput$gb[(j-3*month)]<-mean(weekoutput$gb[(j-3):j])
    monthoutput$gb_d[(j-3*month)]<-mean(weekoutput$gb_d[(j-3):j])
    monthoutput$u_ecb[(j-3*month)]<-mean(weekoutput$u_ecb[(j-3):j])
    monthoutput$iA[(j-3*month)]<-sum(weekoutput$iA[(j-3):j])
    monthoutput$iR[(j-3*month)]<-sum(weekoutput$iR[(j-3):j])
    monthoutput$sav_B[(j-3*month)]<-sum(weekoutput$sav_B[(j-3):j])
    monthoutput$Pr_b[(j-3*month)]<-sum(weekoutput$Pr_b[(j-3):j])
    monthoutput$V_b[(j-3*month)]<-mean(weekoutput$V_b[(j-3):j])
    monthoutput$V_bb[(j-3*month)]<-mean(weekoutput$V_bb[(j-3):j])
    monthoutput$IBS[(j-3*month)]<-sum(weekoutput$IBS[(j-3):j])
    monthoutput$IBD[(j-3*month)]<-sum(weekoutput$IBD[(j-3):j])
    monthoutput$bb[(j-3*month)]<-mean(weekoutput$bb[(j-3):j])
    monthoutput$CAR[(j-3*month)]<-mean(weekoutput$CAR[(j-3):j])
    monthoutput$LL_sup[(j-3*month)]<-sum(weekoutput$LL_sup[(j-3):j])
    monthoutput$Div_b[(j-3*month)]<-sum(weekoutput$Div_b[(j-3):j])
    monthoutput$r_dav[(j-3*month)]<-mean(weekoutput$r_dav[(j-3):j])
    monthoutput$r_LLav[(j-3*month)]<-mean(weekoutput$r_LLav[(j-3):j])
    monthoutput$r_mav[(j-3*month)]<-mean(weekoutput$r_mav[(j-3):j])
    monthoutput$r_IB[(j-3*month)]<-mean(weekoutput$r_IB[(j-3):j])
    monthoutput$Bust[(j-3*month)]<-sum(weekoutput$Bust[(j-3):j])
    monthoutput$porthgb[(j-3*month)]<-mean(weekoutput$porthgb[(j-3):j])
    monthoutput$SFCcheck1[(j-3*month)]<-mean(weekoutput$SFCcheck1[(j-3):j])
    monthoutput$SFCcheck2[(j-3*month)]<-mean(weekoutput$SFCcheck2[(j-3):j])
    monthoutput$SFCcheck3[(j-3*month)]<-mean(weekoutput$SFCcheck3[(j-3):j])
    monthoutput$SFCcheck4[(j-3*month)]<-mean(weekoutput$SFCcheck4[(j-3):j])
    monthoutput$Rshock[(j-3*month)]<-mean(weekoutput$Rshock[(j-3):j])
    monthoutput$sdev_deff[(j-3*month)]<-mean(weekoutput$sdev_deff[(j-3):j])
    monthoutput$sdev_defh[(j-3*month)]<-mean(weekoutput$sdev_defh[(j-3):j])
    monthoutput$V_he[(j-3*month)]<-mean(weekoutput$V_he[(j-3):j])
    monthoutput$r_mave[(j-3*month)]<-mean(weekoutput$r_mave[(j-3):j])
    monthoutput$delta_k[(j-3*month)]<-mean(weekoutput$delta_k[(j-3):j])
    monthoutput$r_LLave[(j-3*month)]<-mean(weekoutput$r_LLave[(j-3):j])
    monthoutput$tau[(j-3*month)]<-mean(weekoutput$tau[(j-3):j])
    monthoutput$r_IBav[(j-3*month)]<-mean(weekoutput$r_IBav[(j-3):j])
    monthoutput$r_LLavr[(j-3*month)]<-mean(weekoutput$r_LLavr[(j-3):j])
    monthoutput$r_mavr[(j-3*month)]<-mean(weekoutput$r_mavr[(j-3):j])
    monthoutput$replacegap[(j-3*month)]<-mean(weekoutput$replacegap[(j-3):j])
    monthoutput$u_e2[(j-3*month)]<-mean(weekoutput$u_e2[(j-3):j])
    monthoutput$iD_f[(j-3*month)]<-sum(weekoutput$iD_f[(j-3):j])
    monthoutput$D_f[(j-3*month)]<-mean(weekoutput$D_f[(j-3):j])
    monthoutput$YD_tax[(j-3*month)]<-sum(weekoutput$YD_tax[(j-3):j])
    monthoutput$r_mavn[(j-3*month)]<-mean(weekoutput$r_mavn[(j-3):j])
    monthoutput$yd_e[(j-3*month)]<-mean(weekoutput$yd_e[(j-3):j])
    monthoutput$v_he[(j-3*month)]<-mean(weekoutput$v_he[(j-3):j])
    monthoutput$pi_eh[(j-3*month)]<-mean(weekoutput$pi_eh[(j-3):j])
    monthoutput$LTV[(j-3*month)]<-mean(weekoutput$LTV[(j-3):j])
    monthoutput$LTVt[(j-3*month)]<-mean(weekoutput$LTVt[(j-3):j])
    monthoutput$LTVgap[(j-3*month)]<-mean(weekoutput$LTVgap[(j-3):j])
    monthoutput$u_n[(j-3*month)]<-mean(weekoutput$u_n[(j-3):j])
    monthoutput$r_LLavn[(j-3*month)]<-mean(weekoutput$r_LLavn[(j-3):j])
    monthoutput$gk_des[(j-3*month)]<-mean(weekoutput$gk_des[(j-3):j])
    monthoutput$UIC[(j-3*month)]<-mean(weekoutput$UIC[(j-3):j])
    monthoutput$FTax[(j-3*month)]<-sum(weekoutput$FTax[(j-3):j])
    monthoutput$gap[(j-3*month)]<-mean(weekoutput$gap[(j-3):j])
    monthoutput$gapr[(j-3*month)]<-mean(weekoutput$gapr[(j-3):j])
    monthoutput$pi_sa[(j-3*month)]<-mean(weekoutput$pi_sa[(j-3):j])
    monthoutput$targetCAR[(j-3*month)]<-mean(weekoutput$targetCAR[(j-3):j])
    monthoutput$CARgap[(j-3*month)]<-mean(weekoutput$CARgap[(j-3):j])
    }
  }
  quarter=0
    for(j in 1:nrow(weekoutput)){
    if(j/12==round(j/12)){
    quarter=quarter+1
    quarteroutput$c[(j-11*quarter)]<-sum(weekoutput$c[(j-11):j])
    quarteroutput$sav_h[(j-11*quarter)]<-sum(weekoutput$sav_h[(j-11):j])
    quarteroutput$i_d[(j-11*quarter)]<-sum(weekoutput$i_d[(j-11):j])
    quarteroutput$i[(j-11*quarter)]<-sum(weekoutput$i[(j-11):j])
    quarteroutput$i_des[(j-11*quarter)]<-mean(weekoutput$i_des[(j-11):j])
    quarteroutput$c_d[(j-11*quarter)]<-mean(weekoutput$c_d[(j-11):j])
    quarteroutput$YD_e[(j-11*quarter)]<-mean(weekoutput$YD_e[(j-11):j])
    quarteroutput$M[(j-11*quarter)]<-mean(weekoutput$M[(j-11):j])
    quarteroutput$V_h[(j-11*quarter)]<-mean(weekoutput$V_h[(j-11):j])
    quarteroutput$D_h[(j-11*quarter)]<-mean(weekoutput$D_h[(j-11):j])
    quarteroutput$rr_h[(j-11*quarter)]<-mean(weekoutput$rr_h[(j-11):j])
    quarteroutput$rr_he[(j-11*quarter)]<-mean(weekoutput$rr_he[(j-11):j])
    quarteroutput$alpha1[(j-11*quarter)]<-mean(weekoutput$alpha1[(j-11):j])
    quarteroutput$alpha2[(j-11*quarter)]<-mean(weekoutput$alpha2[(j-11):j])
    quarteroutput$lev_h[(j-11*quarter)]<-mean(weekoutput$lev_h[(j-11):j])
    quarteroutput$p_h[(j-11*quarter)]<-mean(weekoutput$p_h[(j-11):j])
    quarteroutput$H[(j-11*quarter)]<-mean(weekoutput$H[(j-11):j])
    quarteroutput$M_d[(j-11*quarter)]<-sum(weekoutput$M_d[(j-11):j])
    quarteroutput$YD[(j-11*quarter)]<-sum(weekoutput$YD[(j-11):j])
    quarteroutput$bal_h[(j-11*quarter)]<-sum(weekoutput$bal_h[(j-11):j])
    quarteroutput$bal_f[(j-11*quarter)]<-sum(weekoutput$bal_f[(j-11):j])
    quarteroutput$bal_g[(j-11*quarter)]<-sum(weekoutput$bal_g[(j-11):j])
    quarteroutput$bal_cb[(j-11*quarter)]<-sum(weekoutput$bal_cb[(j-11):j])
    quarteroutput$bal_b1[(j-11*quarter)]<-sum(weekoutput$bal_b1[(j-11):j])
    quarteroutput$bal_b2[(j-11*quarter)]<-sum(weekoutput$bal_b2[(j-11):j])
    quarteroutput$H_dn[(j-11*quarter)]<-sum(weekoutput$H_dn[(j-11):j])
    quarteroutput$H_def[(j-11*quarter)]<-sum(weekoutput$H_def[(j-11):j])
    quarteroutput$iM[(j-11*quarter)]<-sum(weekoutput$iM[(j-11):j])
    quarteroutput$iLL[(j-11*quarter)]<-sum(weekoutput$iLL[(j-11):j])
    quarteroutput$iD_h[(j-11*quarter)]<-sum(weekoutput$iD_h[(j-11):j])
    quarteroutput$M_sup[(j-11*quarter)]<-sum(weekoutput$M_sup[(j-11):j])
    quarteroutput$rep_m[(j-11*quarter)]<-sum(weekoutput$rep_m[(j-11):j])
    quarteroutput$rep_LL[(j-11*quarter)]<-sum(weekoutput$rep_LL[(j-11):j])
    quarteroutput$M_np[(j-11*quarter)]<-sum(weekoutput$M_np[(j-11):j])
    quarteroutput$TAA_h[(j-11*quarter)]<-mean(weekoutput$TAA_h[(j-11):j])
    quarteroutput$W[(j-11*quarter)]<-mean(weekoutput$W[(j-11):j])
    quarteroutput$u_nw[(j-11*quarter)]<-mean(weekoutput$u_nw[(j-11):j])
    quarteroutput$MH[(j-11*quarter)]<-mean(weekoutput$MH[(j-11):j])
    quarteroutput$sav_f[(j-11*quarter)]<-sum(weekoutput$sav_f[(j-11):j])
    quarteroutput$deltaM[(j-11*quarter)]<-sum(weekoutput$deltaM[(j-11):j])
    quarteroutput$V_f[(j-11*quarter)]<-mean(weekoutput$V_f[(j-11):j])
    quarteroutput$K[(j-11*quarter)]<-mean(weekoutput$K[(j-11):j])
    quarteroutput$k[(j-11*quarter)]<-mean(weekoutput$k[(j-11):j])
    quarteroutput$LL[(j-11*quarter)]<-mean(weekoutput$LL[(j-11):j])
    quarteroutput$p[(j-11*quarter)]<-mean(weekoutput$p[(j-11):j])
    quarteroutput$theta[(j-11*quarter)]<-mean(weekoutput$theta[(j-11):j])
    quarteroutput$UC[(j-11*quarter)]<-mean(weekoutput$UC[(j-11):j])
    quarteroutput$WB[(j-11*quarter)]<-sum(weekoutput$WB[(j-11):j])
    quarteroutput$y[(j-11*quarter)]<-sum(weekoutput$y[(j-11):j])
    quarteroutput$C[(j-11*quarter)]<-sum(weekoutput$C[(j-11):j])
    quarteroutput$N_d[(j-11*quarter)]<-sum(weekoutput$N_d[(j-11):j])
    quarteroutput$Y[(j-11*quarter)]<-sum(weekoutput$Y[(j-11):j])
    quarteroutput$I[(j-11*quarter)]<-sum(weekoutput$I[(j-11):j])
    quarteroutput$LL_d[(j-11*quarter)]<-sum(weekoutput$LL_d[(j-11):j])
    quarteroutput$lev_f[(j-11*quarter)]<-mean(weekoutput$lev_f[(j-11):j])
    quarteroutput$lev_fe[(j-11*quarter)]<-mean(weekoutput$lev_fe[(j-11):j])
    quarteroutput$u[(j-11*quarter)]<-mean(weekoutput$u[(j-11):j])
    quarteroutput$u_e[(j-11*quarter)]<-mean(weekoutput$u_e[(j-11):j])
    quarteroutput$lev_ft[(j-11*quarter)]<-mean(weekoutput$lev_ft[(j-11):j])
    quarteroutput$Pr_f[(j-11*quarter)]<-sum(weekoutput$Pr_f[(j-11):j])
    quarteroutput$LL_np[(j-11*quarter)]<-sum(weekoutput$LL_np[(j-11):j])
    quarteroutput$sav_ft[(j-11*quarter)]<-sum(weekoutput$sav_ft[(j-11):j])
    quarteroutput$div_f[(j-11*quarter)]<-sum(weekoutput$div_f[(j-11):j])
    quarteroutput$yfc[(j-11*quarter)]<-mean(weekoutput$yfc[(j-11):j])
    quarteroutput$meanlev[(j-11*quarter)]<-mean(weekoutput$meanlev[(j-11):j])
    quarteroutput$sav_g[(j-11*quarter)]<-sum(weekoutput$sav_g[(j-11):j])
    quarteroutput$gb_h[(j-11*quarter)]<-mean(weekoutput$gb_h[(j-11):j])
    quarteroutput$r_gb[(j-11*quarter)]<-mean(weekoutput$r_gb[(j-11):j])
    quarteroutput$V_g[(j-11*quarter)]<-mean(weekoutput$V_g[(j-11):j])
    quarteroutput$PSBR[(j-11*quarter)]<-sum(weekoutput$PSBR[(j-11):j])
    quarteroutput$sav_g[(j-11*quarter)]<-sum(weekoutput$sav_g[(j-11):j])
    quarteroutput$Tax[(j-11*quarter)]<-sum(weekoutput$Tax[(j-11):j])
    quarteroutput$g[(j-11*quarter)]<-sum(weekoutput$g[(j-11):j])
    quarteroutput$G[(j-11*quarter)]<-sum(weekoutput$G[(j-11):j])
    quarteroutput$gb_s[(j-11*quarter)]<-sum(weekoutput$gb_s[(j-11):j])
    quarteroutput$g_des[(j-11*quarter)]<-mean(weekoutput$g_des[(j-11):j])
    quarteroutput$rep_gb[(j-11*quarter)]<-sum(weekoutput$rep_gb[(j-11):j])
    quarteroutput$rep_gbcb[(j-11*quarter)]<-sum(weekoutput$rep_gbcb[(j-11):j])
    quarteroutput$rep_gbh[(j-11*quarter)]<-sum(weekoutput$rep_gbh[(j-11):j])
    quarteroutput$iGB[(j-11*quarter)]<-sum(weekoutput$iGB[(j-11):j])
    quarteroutput$iGB_h[(j-11*quarter)]<-sum(weekoutput$iGB_h[(j-11):j])
    quarteroutput$iGB_cb[(j-11*quarter)]<-sum(weekoutput$iGB_cb[(j-11):j])
    quarteroutput$PCB[(j-11*quarter)]<-sum(weekoutput$PCB[(j-11):j])
    quarteroutput$sav_cb[(j-11*quarter)]<-sum(weekoutput$sav_cb[(j-11):j])
    quarteroutput$V_cb[(j-11*quarter)]<-mean(weekoutput$V_cb[(j-11):j])
    quarteroutput$A[(j-11*quarter)]<-mean(weekoutput$A[(j-11):j])
    quarteroutput$pi_a[(j-11*quarter)]<-mean(weekoutput$pi_a[(j-11):j])
    quarteroutput$pi_m[(j-11*quarter)]<-mean(weekoutput$pi_m[(j-11):j])
    quarteroutput$pi_q[(j-11*quarter)]<-mean(weekoutput$pi_q[(j-11):j])
    quarteroutput$pi_e[(j-11*quarter)]<-mean(weekoutput$pi_e[(j-11):j])
    quarteroutput$r_cbd[(j-11*quarter)]<-mean(weekoutput$r_cbd[(j-11):j])
    quarteroutput$r_cbl[(j-11*quarter)]<-mean(weekoutput$r_cbl[(j-11):j])
    quarteroutput$R[(j-11*quarter)]<-mean(weekoutput$R[(j-11):j])
    quarteroutput$gb_rcb[(j-11*quarter)]<-sum(weekoutput$gb_rcb[(j-11):j])
    quarteroutput$gb_cb[(j-11*quarter)]<-mean(weekoutput$gb_cb[(j-11):j])
    quarteroutput$R_t[(j-11*quarter)]<-mean(weekoutput$R_t[(j-11):j])
    quarteroutput$gb_dcb[(j-11*quarter)]<-sum(weekoutput$gb_dcb[(j-11):j])
    quarteroutput$gb_scb[(j-11*quarter)]<-sum(weekoutput$gb_scb[(j-11):j])
    quarteroutput$buff_h[(j-11*quarter)]<-mean(weekoutput$buff_h[(j-11):j])
    quarteroutput$R_pd[(j-11*quarter)]<-mean(weekoutput$R_pd[(j-11):j])
    quarteroutput$cbint[(j-11*quarter)]<-sum(weekoutput$cbint[(j-11):j])
    quarteroutput$CARt[(j-11*quarter)]<-mean(weekoutput$CARt[(j-11):j])
    quarteroutput$gb[(j-11*quarter)]<-mean(weekoutput$gb[(j-11):j])
    quarteroutput$gb_d[(j-11*quarter)]<-mean(weekoutput$gb_d[(j-11):j])
    quarteroutput$u_ecb[(j-11*quarter)]<-mean(weekoutput$u_ecb[(j-11):j])
    quarteroutput$iA[(j-11*quarter)]<-sum(weekoutput$iA[(j-11):j])
    quarteroutput$iR[(j-11*quarter)]<-sum(weekoutput$iR[(j-11):j])
    quarteroutput$sav_B[(j-11*quarter)]<-sum(weekoutput$sav_B[(j-11):j])
    quarteroutput$Pr_b[(j-11*quarter)]<-sum(weekoutput$Pr_b[(j-11):j])
    quarteroutput$V_b[(j-11*quarter)]<-mean(weekoutput$V_b[(j-11):j])
    quarteroutput$V_bb[(j-11*quarter)]<-mean(weekoutput$V_bb[(j-11):j])
    quarteroutput$IBS[(j-11*quarter)]<-sum(weekoutput$IBS[(j-11):j])
    quarteroutput$IBD[(j-11*quarter)]<-sum(weekoutput$IBD[(j-11):j])
    quarteroutput$bb[(j-11*quarter)]<-mean(weekoutput$bb[(j-11):j])
    quarteroutput$CAR[(j-11*quarter)]<-mean(weekoutput$CAR[(j-11):j])
    quarteroutput$LL_sup[(j-11*quarter)]<-sum(weekoutput$LL_sup[(j-11):j])
    quarteroutput$Div_b[(j-11*quarter)]<-sum(weekoutput$Div_b[(j-11):j])
    quarteroutput$r_dav[(j-11*quarter)]<-mean(weekoutput$r_dav[(j-11):j])
    quarteroutput$r_LLav[(j-11*quarter)]<-mean(weekoutput$r_LLav[(j-11):j])
    quarteroutput$r_mav[(j-11*quarter)]<-mean(weekoutput$r_mav[(j-11):j])
    quarteroutput$r_IB[(j-11*quarter)]<-mean(weekoutput$r_IB[(j-11):j])
    quarteroutput$Bust[(j-11*quarter)]<-sum(weekoutput$Bust[(j-11):j])
    quarteroutput$porthgb[(j-11*quarter)]<-mean(weekoutput$porthgb[(j-11):j])
    quarteroutput$SFCcheck1[(j-11*quarter)]<-mean(weekoutput$SFCcheck1[(j-11):j])
    quarteroutput$SFCcheck2[(j-11*quarter)]<-mean(weekoutput$SFCcheck2[(j-11):j])
    quarteroutput$SFCcheck3[(j-11*quarter)]<-mean(weekoutput$SFCcheck3[(j-11):j])
    quarteroutput$SFCcheck4[(j-11*quarter)]<-mean(weekoutput$SFCcheck4[(j-11):j])
    quarteroutput$Rshock[(j-11*quarter)]<-mean(weekoutput$Rshock[(j-11):j])
    quarteroutput$sdev_deff[(j-11*quarter)]<-mean(weekoutput$sdev_deff[(j-11):j])
    quarteroutput$sdev_defh[(j-11*quarter)]<-mean(weekoutput$sdev_defh[(j-11):j])
    quarteroutput$V_he[(j-11*quarter)]<-mean(weekoutput$V_he[(j-11):j])
    quarteroutput$r_mave[(j-11*quarter)]<-mean(weekoutput$r_mave[(j-11):j])
    quarteroutput$delta_k[(j-11*quarter)]<-mean(weekoutput$delta_k[(j-11):j])
    quarteroutput$r_LLave[(j-11*quarter)]<-mean(weekoutput$r_LLave[(j-11):j])
    quarteroutput$tau[(j-11*quarter)]<-mean(weekoutput$tau[(j-11):j])
    quarteroutput$r_IBav[(j-11*quarter)]<-mean(weekoutput$r_IBav[(j-11):j])
    quarteroutput$r_LLavr[(j-11*quarter)]<-mean(weekoutput$r_LLavr[(j-11):j])
    quarteroutput$r_mavr[(j-11*quarter)]<-mean(weekoutput$r_mavr[(j-11):j])
    quarteroutput$replacegap[(j-11*quarter)]<-mean(weekoutput$replacegap[(j-11):j])
    quarteroutput$u_e2[(j-11*quarter)]<-mean(weekoutput$u_e2[(j-11):j])
    quarteroutput$iD_f[(j-11*quarter)]<-sum(weekoutput$iD_f[(j-11):j])
    quarteroutput$D_f[(j-11*quarter)]<-mean(weekoutput$D_f[(j-11):j])
    quarteroutput$YD_tax[(j-11*quarter)]<-sum(weekoutput$YD_tax[(j-11):j])
    quarteroutput$r_mavn[(j-11*quarter)]<-mean(weekoutput$r_mavn[(j-11):j])
    quarteroutput$yd_e[(j-11*quarter)]<-mean(weekoutput$yd_e[(j-11):j])
    quarteroutput$v_he[(j-11*quarter)]<-mean(weekoutput$v_he[(j-11):j])
    quarteroutput$pi_eh[(j-11*quarter)]<-mean(weekoutput$pi_eh[(j-11):j])
    quarteroutput$LTV[(j-11*quarter)]<-mean(weekoutput$LTV[(j-11):j])
    quarteroutput$LTVt[(j-11*quarter)]<-mean(weekoutput$LTVt[(j-11):j])
    quarteroutput$LTVgap[(j-11*quarter)]<-mean(weekoutput$LTVgap[(j-11):j])
    quarteroutput$u_n[(j-11*quarter)]<-mean(weekoutput$u_n[(j-11):j])
    quarteroutput$r_LLavn[(j-11*quarter)]<-mean(weekoutput$r_LLavn[(j-11):j])
    quarteroutput$gk_des[(j-11*quarter)]<-mean(weekoutput$gk_des[(j-11):j])
    quarteroutput$UIC[(j-11*quarter)]<-mean(weekoutput$UIC[(j-11):j])
    quarteroutput$FTax[(j-11*quarter)]<-sum(weekoutput$FTax[(j-11):j])
    quarteroutput$gap[(j-11*quarter)]<-mean(weekoutput$gap[(j-11):j])
    quarteroutput$gapr[(j-11*quarter)]<-mean(weekoutput$gapr[(j-11):j])
    quarteroutput$pi_sa[(j-11*quarter)]<-mean(weekoutput$pi_sa[(j-11):j])
    quarteroutput$targetCAR[(j-11*quarter)]<-mean(weekoutput$targetCAR[(j-11):j])
    quarteroutput$CARgap[(j-11*quarter)]<-mean(weekoutput$CARgap[(j-11):j])
    }
  }
    year=0
    for(j in 1:nrow(weekoutput)){
    if(j/48==round(j/48)){
    year=year+1
    yearoutput$c[(j-47*year)]<-sum(weekoutput$c[(j-47):j])
    yearoutput$c_d[(j-47*year)]<-sum(weekoutput$c_d[(j-47):j])
    yearoutput$sav_h[(j-47*year)]<-sum(weekoutput$sav_h[(j-47):j])
    yearoutput$i_d[(j-47*year)]<-sum(weekoutput$i_d[(j-47):j])
    yearoutput$i[(j-47*year)]<-sum(weekoutput$i[(j-47):j])
    yearoutput$i_des[(j-47*year)]<-mean(weekoutput$i_des[(j-47):j])
    yearoutput$YD_e[(j-47*year)]<-mean(weekoutput$YD_e[(j-47):j])
    yearoutput$M[(j-47*year)]<-mean(weekoutput$M[(j-47):j])
    yearoutput$V_h[(j-47*year)]<-mean(weekoutput$V_h[(j-47):j])
    yearoutput$D_h[(j-47*year)]<-mean(weekoutput$D_h[(j-47):j])
    yearoutput$rr_h[(j-47*year)]<-mean(weekoutput$rr_h[(j-47):j])
    yearoutput$rr_he[(j-47*year)]<-mean(weekoutput$rr_he[(j-47):j])
    yearoutput$alpha1[(j-47*year)]<-mean(weekoutput$alpha1[(j-47):j])
    yearoutput$alpha2[(j-47*year)]<-mean(weekoutput$alpha2[(j-47):j])
    yearoutput$lev_h[(j-47*year)]<-mean(weekoutput$lev_h[(j-47):j])
    yearoutput$p_h[(j-47*year)]<-mean(weekoutput$p_h[(j-47):j])
    yearoutput$H[(j-47*year)]<-mean(weekoutput$H[(j-47):j])
    yearoutput$M_d[(j-47*year)]<-sum(weekoutput$M_d[(j-47):j])
    yearoutput$YD[(j-47*year)]<-sum(weekoutput$YD[(j-47):j])
    yearoutput$bal_h[(j-47*year)]<-sum(weekoutput$bal_h[(j-47):j])
    yearoutput$bal_f[(j-47*year)]<-sum(weekoutput$bal_f[(j-47):j])
    yearoutput$bal_g[(j-47*year)]<-sum(weekoutput$bal_g[(j-47):j])
    yearoutput$bal_cb[(j-47*year)]<-sum(weekoutput$bal_cb[(j-47):j])
    yearoutput$bal_b1[(j-47*year)]<-sum(weekoutput$bal_b1[(j-47):j])
    yearoutput$bal_b2[(j-47*year)]<-sum(weekoutput$bal_b2[(j-47):j])
    yearoutput$H_dn[(j-47*year)]<-sum(weekoutput$H_dn[(j-47):j])
    yearoutput$H_def[(j-47*year)]<-sum(weekoutput$H_def[(j-47):j])
    yearoutput$iM[(j-47*year)]<-sum(weekoutput$iM[(j-47):j])
    yearoutput$iLL[(j-47*year)]<-sum(weekoutput$iLL[(j-47):j])
    yearoutput$iD_h[(j-47*year)]<-sum(weekoutput$iD_h[(j-47):j])
    yearoutput$M_sup[(j-47*year)]<-sum(weekoutput$M_sup[(j-47):j])
    yearoutput$rep_m[(j-47*year)]<-sum(weekoutput$rep_m[(j-47):j])
    yearoutput$rep_LL[(j-47*year)]<-sum(weekoutput$rep_LL[(j-47):j])
    yearoutput$M_np[(j-47*year)]<-sum(weekoutput$M_np[(j-47):j])
    yearoutput$TAA_h[(j-47*year)]<-mean(weekoutput$TAA_h[(j-47):j])
    yearoutput$W[(j-47*year)]<-mean(weekoutput$W[(j-47):j])
    yearoutput$u_nw[(j-47*year)]<-mean(weekoutput$u_nw[(j-47):j])
    yearoutput$MH[(j-47*year)]<-mean(weekoutput$MH[(j-47):j])
    yearoutput$sav_f[(j-47*year)]<-sum(weekoutput$sav_f[(j-47):j])
    yearoutput$deltaM[(j-47*year)]<-sum(weekoutput$deltaM[(j-47):j])
    yearoutput$V_f[(j-47*year)]<-mean(weekoutput$V_f[(j-47):j])
    yearoutput$K[(j-47*year)]<-mean(weekoutput$K[(j-47):j])
    yearoutput$k[(j-47*year)]<-mean(weekoutput$k[(j-47):j])
    yearoutput$LL[(j-47*year)]<-mean(weekoutput$LL[(j-47):j])
    yearoutput$p[(j-47*year)]<-mean(weekoutput$p[(j-47):j])
    yearoutput$theta[(j-47*year)]<-mean(weekoutput$theta[(j-47):j])
    yearoutput$UC[(j-47*year)]<-mean(weekoutput$UC[(j-47):j])
    yearoutput$WB[(j-47*year)]<-sum(weekoutput$WB[(j-47):j])
    yearoutput$y[(j-47*year)]<-sum(weekoutput$y[(j-47):j])
    yearoutput$C[(j-47*year)]<-sum(weekoutput$C[(j-47):j])
    yearoutput$N_d[(j-47*year)]<-sum(weekoutput$N_d[(j-47):j])
    yearoutput$Y[(j-47*year)]<-sum(weekoutput$Y[(j-47):j])
    yearoutput$I[(j-47*year)]<-sum(weekoutput$I[(j-47):j])
    yearoutput$LL_d[(j-47*year)]<-sum(weekoutput$LL_d[(j-47):j])
    yearoutput$lev_f[(j-47*year)]<-mean(weekoutput$lev_f[(j-47):j])
    yearoutput$lev_fe[(j-47*year)]<-mean(weekoutput$lev_fe[(j-47):j])
    yearoutput$u[(j-47*year)]<-mean(weekoutput$u[(j-47):j])
    yearoutput$u_e[(j-47*year)]<-mean(weekoutput$u_e[(j-47):j])
    yearoutput$lev_ft[(j-47*year)]<-mean(weekoutput$lev_ft[(j-47):j])
    yearoutput$Pr_f[(j-47*year)]<-sum(weekoutput$Pr_f[(j-47):j])
    yearoutput$LL_np[(j-47*year)]<-sum(weekoutput$LL_np[(j-47):j])
    yearoutput$sav_ft[(j-47*year)]<-sum(weekoutput$sav_ft[(j-47):j])
    yearoutput$div_f[(j-47*year)]<-sum(weekoutput$div_f[(j-47):j])
    yearoutput$yfc[(j-47*year)]<-mean(weekoutput$yfc[(j-47):j])
    yearoutput$meanlev[(j-47*year)]<-mean(weekoutput$meanlev[(j-47):j])
    yearoutput$sav_g[(j-47*year)]<-sum(weekoutput$sav_g[(j-47):j])
    yearoutput$gb_h[(j-47*year)]<-mean(weekoutput$gb_h[(j-47):j])
    yearoutput$r_gb[(j-47*year)]<-mean(weekoutput$r_gb[(j-47):j])
    yearoutput$V_g[(j-47*year)]<-mean(weekoutput$V_g[(j-47):j])
    yearoutput$PSBR[(j-47*year)]<-sum(weekoutput$PSBR[(j-47):j])
    yearoutput$sav_g[(j-47*year)]<-sum(weekoutput$sav_g[(j-47):j])
    yearoutput$Tax[(j-47*year)]<-sum(weekoutput$Tax[(j-47):j])
    yearoutput$g[(j-47*year)]<-sum(weekoutput$g[(j-47):j])
    yearoutput$G[(j-47*year)]<-sum(weekoutput$G[(j-47):j])
    yearoutput$gb_s[(j-47*year)]<-sum(weekoutput$gb_s[(j-47):j])
    yearoutput$g_des[(j-47*year)]<-mean(weekoutput$g_des[(j-47):j])
    yearoutput$rep_gb[(j-47*year)]<-sum(weekoutput$rep_gb[(j-47):j])
    yearoutput$rep_gbcb[(j-47*year)]<-sum(weekoutput$rep_gbcb[(j-47):j])
    yearoutput$rep_gbh[(j-47*year)]<-sum(weekoutput$rep_gbh[(j-47):j])
    yearoutput$iGB[(j-47*year)]<-sum(weekoutput$iGB[(j-47):j])
    yearoutput$iGB_h[(j-47*year)]<-sum(weekoutput$iGB_h[(j-47):j])
    yearoutput$iGB_cb[(j-47*year)]<-sum(weekoutput$iGB_cb[(j-47):j])
    yearoutput$PCB[(j-47*year)]<-sum(weekoutput$PCB[(j-47):j])
    yearoutput$sav_cb[(j-47*year)]<-sum(weekoutput$sav_cb[(j-47):j])
    yearoutput$V_cb[(j-47*year)]<-mean(weekoutput$V_cb[(j-47):j])
    yearoutput$A[(j-47*year)]<-mean(weekoutput$A[(j-47):j])
    yearoutput$pi_a[(j-47*year)]<-mean(weekoutput$pi_a[(j-47):j])
    yearoutput$pi_m[(j-47*year)]<-mean(weekoutput$pi_m[(j-47):j])
    yearoutput$pi_q[(j-47*year)]<-mean(weekoutput$pi_q[(j-47):j])
    yearoutput$pi_e[(j-47*year)]<-mean(weekoutput$pi_e[(j-47):j])
    yearoutput$r_cbd[(j-47*year)]<-mean(weekoutput$r_cbd[(j-47):j])
    yearoutput$r_cbl[(j-47*year)]<-mean(weekoutput$r_cbl[(j-47):j])
    yearoutput$R[(j-47*year)]<-mean(weekoutput$R[(j-47):j])
    yearoutput$gb_rcb[(j-47*year)]<-sum(weekoutput$gb_rcb[(j-47):j])
    yearoutput$gb_cb[(j-47*year)]<-mean(weekoutput$gb_cb[(j-47):j])
    yearoutput$R_t[(j-47*year)]<-mean(weekoutput$R_t[(j-47):j])
    yearoutput$gb_dcb[(j-47*year)]<-sum(weekoutput$gb_dcb[(j-47):j])
    yearoutput$gb_scb[(j-47*year)]<-sum(weekoutput$gb_scb[(j-47):j])
    yearoutput$buff_h[(j-47*year)]<-mean(weekoutput$buff_h[(j-47):j])
    yearoutput$R_pd[(j-47*year)]<-mean(weekoutput$R_pd[(j-47):j])
    yearoutput$cbint[(j-47*year)]<-sum(weekoutput$cbint[(j-47):j])
    yearoutput$CARt[(j-47*year)]<-mean(weekoutput$CARt[(j-47):j])
    yearoutput$gb[(j-47*year)]<-mean(weekoutput$gb[(j-47):j])
    yearoutput$gb_d[(j-47*year)]<-mean(weekoutput$gb_d[(j-47):j])
    yearoutput$u_ecb[(j-47*year)]<-mean(weekoutput$u_ecb[(j-47):j])
    yearoutput$iA[(j-47*year)]<-sum(weekoutput$iA[(j-47):j])
    yearoutput$iR[(j-47*year)]<-sum(weekoutput$iR[(j-47):j])
    yearoutput$sav_B[(j-47*year)]<-sum(weekoutput$sav_B[(j-47):j])
    yearoutput$Pr_b[(j-47*year)]<-sum(weekoutput$Pr_b[(j-47):j])
    yearoutput$V_b[(j-47*year)]<-mean(weekoutput$V_b[(j-47):j])
    yearoutput$V_bb[(j-47*year)]<-mean(weekoutput$V_bb[(j-47):j])
    yearoutput$IBS[(j-47*year)]<-sum(weekoutput$IBS[(j-47):j])
    yearoutput$IBD[(j-47*year)]<-sum(weekoutput$IBD[(j-47):j])
    yearoutput$bb[(j-47*year)]<-mean(weekoutput$bb[(j-47):j])
    yearoutput$CAR[(j-47*year)]<-mean(weekoutput$CAR[(j-47):j])
    yearoutput$LL_sup[(j-47*year)]<-sum(weekoutput$LL_sup[(j-47):j])
    yearoutput$Div_b[(j-47*year)]<-sum(weekoutput$Div_b[(j-47):j])
    yearoutput$r_dav[(j-47*year)]<-mean(weekoutput$r_dav[(j-47):j])
    yearoutput$r_LLav[(j-47*year)]<-mean(weekoutput$r_LLav[(j-47):j])
    yearoutput$r_mav[(j-47*year)]<-mean(weekoutput$r_mav[(j-47):j])
    yearoutput$r_IB[(j-47*year)]<-mean(weekoutput$r_IB[(j-47):j])
    yearoutput$Bust[(j-47*year)]<-sum(weekoutput$Bust[(j-47):j])
    yearoutput$porthgb[(j-47*year)]<-mean(weekoutput$porthgb[(j-47):j])
    yearoutput$SFCcheck1[(j-47*year)]<-mean(weekoutput$SFCcheck1[(j-47):j])
    yearoutput$SFCcheck2[(j-47*year)]<-mean(weekoutput$SFCcheck2[(j-47):j])
    yearoutput$SFCcheck3[(j-47*year)]<-mean(weekoutput$SFCcheck3[(j-47):j])
    yearoutput$SFCcheck4[(j-47*year)]<-mean(weekoutput$SFCcheck4[(j-47):j])
    yearoutput$Rshock[(j-47*year)]<-mean(weekoutput$Rshock[(j-47):j])
    yearoutput$sdev_deff[(j-47*year)]<-mean(weekoutput$sdev_deff[(j-47):j])
    yearoutput$sdev_defh[(j-47*year)]<-mean(weekoutput$sdev_defh[(j-47):j])
    yearoutput$V_he[(j-47*year)]<-mean(weekoutput$V_he[(j-47):j])
    yearoutput$r_mave[(j-47*year)]<-mean(weekoutput$r_mave[(j-47):j])
    yearoutput$delta_k[(j-47*year)]<-mean(weekoutput$delta_k[(j-47):j])
    yearoutput$r_LLave[(j-47*year)]<-mean(weekoutput$r_LLave[(j-47):j])
    yearoutput$tau[(j-47*year)]<-mean(weekoutput$tau[(j-47):j])
    yearoutput$r_IBav[(j-47*year)]<-mean(weekoutput$r_IBav[(j-47):j])
    yearoutput$r_LLavr[(j-47*year)]<-mean(weekoutput$r_LLavr[(j-47):j])
    yearoutput$r_mavr[(j-47*year)]<-mean(weekoutput$r_mavr[(j-47):j])
    yearoutput$replacegap[(j-47*year)]<-mean(weekoutput$replacegap[(j-47):j])
    yearoutput$u_e2[(j-47*year)]<-mean(weekoutput$u_e2[(j-47):j])
    yearoutput$iD_f[(j-47*year)]<-sum(weekoutput$iD_f[(j-47):j])
    yearoutput$D_f[(j-47*year)]<-mean(weekoutput$D_f[(j-47):j])
    yearoutput$YD_tax[(j-47*year)]<-sum(weekoutput$YD_tax[(j-47):j])
    yearoutput$r_mavn[(j-47*year)]<-mean(weekoutput$r_mavn[(j-47):j])
    yearoutput$yd_e[(j-47*year)]<-mean(weekoutput$yd_e[(j-47):j])
    yearoutput$v_he[(j-47*year)]<-mean(weekoutput$v_he[(j-47):j])
    yearoutput$pi_eh[(j-47*year)]<-mean(weekoutput$pi_eh[(j-47):j])
    yearoutput$LTV[(j-47*year)]<-mean(weekoutput$LTV[(j-47):j])
    yearoutput$LTVt[(j-47*year)]<-mean(weekoutput$LTVt[(j-47):j])
    yearoutput$LTVgap[(j-47*year)]<-mean(weekoutput$LTVgap[(j-47):j])
    yearoutput$u_n[(j-47*year)]<-mean(weekoutput$u_n[(j-47):j])
    yearoutput$r_LLavn[(j-47*year)]<-mean(weekoutput$r_LLavn[(j-47):j])
    yearoutput$gk_des[(j-47*year)]<-mean(weekoutput$gk_des[(j-47):j])
    yearoutput$UIC[(j-47*year)]<-mean(weekoutput$UIC[(j-47):j])
    yearoutput$FTax[(j-47*year)]<-sum(weekoutput$FTax[(j-47):j])
    yearoutput$gap[(j-47*year)]<-mean(weekoutput$gap[(j-47):j])
    yearoutput$gapr[(j-47*year)]<-mean(weekoutput$gapr[(j-47):j])
    yearoutput$pi_sa[(j-47*year)]<-mean(weekoutput$pi_sa[(j-47):j])
    yearoutput$targetCAR[(j-47*year)]<-mean(weekoutput$targetCAR[(j-47):j])
    yearoutput$CARgap[(j-47*year)]<-mean(weekoutput$CARgap[(j-47):j])
    }
    }

  month=0
  for(j in 1:nrow(banksavw)){
  if(j/4==round(j/4)){
  month=month+1
  banksavm$sav_b[(j-3*month)]<-sum(banksavw$sav_b[(j-3):j])
  banksavm$pr_b[(j-3*month)]<-sum(banksavw$pr_b[(j-3):j])
  banksavm$v_b[(j-3*month)]<-mean(banksavw$v_b[(j-3):j])
  banksavm$v_bb[(j-3*month)]<-mean(banksavw$v_bb[(j-3):j])
  banksavm$v_bbe[(j-3*month)]<-mean(banksavw$v_bbe[(j-3):j])
  banksavm$v_bbt[(j-3*month)]<-mean(banksavw$v_bbt[(j-3):j])
  banksavm$a_b[(j-3*month)]<-mean(banksavw$a_b[(j-3):j])
  banksavm$r_b[(j-3*month)]<-mean(banksavw$r_b[(j-3):j])
  banksavm$iM_b[(j-3*month)]<-sum(banksavw$iM_b[(j-3):j])
  banksavm$iLL_b[(j-3*month)]<-sum(banksavw$iLL_b[(j-3):j])
  banksavm$iD_hb[(j-3*month)]<-sum(banksavw$iD_hb[(j-3):j])
  banksavm$iD_fb[(j-3*month)]<-sum(banksavw$iD_fb[(j-3):j])
  banksavm$iIB_b[(j-3*month)]<-sum(banksavw$iIB_b[(j-3):j])
  banksavm$M_supb[(j-3*month)]<-sum(banksavw$M_supb[(j-3):j])
  banksavm$rep_mb[(j-3*month)]<-sum(banksavw$rep_mb[(j-3):j])
  banksavm$M_npb[(j-3*month)]<-sum(banksavw$M_npb[(j-3):j])
  banksavm$LL_supb[(j-3*month)]<-sum(banksavw$LL_supb[(j-3):j])
  banksavm$rep_LLb[(j-3*month)]<-sum(banksavw$rep_LLb[(j-3):j])
  banksavm$LL_npb[(j-3*month)]<-sum(banksavw$LL_npb[(j-3):j])
  banksavm$M_b[(j-3*month)]<-mean(banksavw$M_b[(j-3):j])
  banksavm$LL_b[(j-3*month)]<-mean(banksavw$LL_b[(j-3):j])
  banksavm$clear_b[(j-3*month)]<-sum(banksavw$clear_b[(j-3):j])
  banksavm$clearalt_b[(j-3*month)]<-sum(banksavw$clearalt_b[(j-3):j])
  banksavm$R_pb[(j-3*month)]<-mean(banksavw$R_pb[(j-3):j])
  banksavm$R_paltb[(j-3*month)]<-mean(banksavw$R_paltb[(j-3):j])
  banksavm$R_tb[(j-3*month)]<-mean(banksavw$R_tb[(j-3):j])
  banksavm$R_anb[(j-3*month)]<-mean(banksavw$R_anb[(j-3):j])
  banksavm$IBL_sb[(j-3*month)]<-mean(banksavw$IBL_sb[(j-3):j])
  banksavm$IBA_db[(j-3*month)]<-mean(banksavw$IBA_db[(j-3):j])
  banksavm$IBA_b[(j-3*month)]<-mean(banksavw$IBA_b[(j-3):j])
  banksavm$IBL_b[(j-3*month)]<-mean(banksavw$IBL_b[(j-3):j])
  banksavm$R_lb[(j-3*month)]<-mean(banksavw$R_lb[(j-3):j])
  banksavm$div_b[(j-3*month)]<-sum(banksavw$div_b[(j-3):j])
  banksavm$div_bt[(j-3*month)]<-sum(banksavw$div_bt[(j-3):j])
  banksavm$bb_b[(j-3*month)]<-mean(banksavw$bb_b[(j-3):j])
  banksavm$r_LLb[(j-3*month)]<-mean(banksavw$r_LLb[(j-3):j])
  banksavm$r_Mb[(j-3*month)]<-mean(banksavw$r_Mb[(j-3):j])
  banksavm$r_db[(j-3*month)]<-mean(banksavw$r_db[(j-3):j])
  banksavm$CARb[(j-3*month)]<-mean(banksavw$CARb[(j-3):j])
  banksavm$LCRb[(j-3*month)]<-mean(banksavw$LCRb[(j-3):j])
  banksavm$ir_b[(j-3*month)]<-sum(banksavw$ir_b[(j-3):j])
  banksavm$ia_b[(j-3*month)]<-sum(banksavw$ia_b[(j-3):j])
  banksavm$bal_bb[(j-3*month)]<-sum(banksavw$bal_bb[(j-3):j])
  banksavm$D_hb[(j-3*month)]<-mean(banksavw$D_hb[(j-3):j])
  banksavm$D_fb[(j-3*month)]<-mean(banksavw$D_fb[(j-3):j])
  banksavm$carshare[(j-3*month)]<-mean(banksavw$carshare[(j-3):j])
  banksavm$rdshare[(j-3*month)]<-mean(banksavw$rdshare[(j-3):j])
  banksavm$rllshare[(j-3*month)]<-mean(banksavw$rllshare[(j-3):j])
  banksavm$rmshare[(j-3*month)]<-mean(banksavw$rmshare[(j-3):j])
  banksavm$IBSshare[(j-3*month)]<-mean(banksavw$IBSshare[(j-3):j])
  banksavm$IBDshare[(j-3*month)]<-mean(banksavw$IBDshare[(j-3):j])
  banksavm$rationM[(j-3*month)]<-mean(banksavw$rationM[(j-3):j])
  banksavm$rationLL[(j-3*month)]<-mean(banksavw$rationLL[(j-3):j])
  banksavm$M_db[(j-3*month)]<-sum(banksavw$M_db[(j-3):j])
  banksavm$LL_db[(j-3*month)]<-sum(banksavw$LL_db[(j-3):j])
  banksavm$bust[(j-3*month)]<-sum(banksavw$bust[(j-3):j])
  banksavm$rLLrel[(j-3*month)]<-mean(banksavw$rLLrel[(j-3):j])
  banksavm$rMrel[(j-3*month)]<-mean(banksavw$rMrel[(j-3):j])
  banksavm$rDrel[(j-3*month)]<-mean(banksavw$rDrel[(j-3):j])
  banksavm$Mlag[(j-3*month)]<-mean(banksavw$Mlag[(j-3):j])
  banksavm$LLlag[(j-3*month)]<-mean(banksavw$LLlag[(j-3):j])
  banksavm$Dhlag[(j-3*month)]<-mean(banksavw$Dhlag[(j-3):j])
  banksavm$Dflag[(j-3*month)]<-mean(banksavw$Dflag[(j-3):j])
  banksavm$default_LL[(j-3*month)]<-mean(banksavw$default_LL[(j-3):j])
  banksavm$default_M[(j-3*month)]<-mean(banksavw$default_M[(j-3):j])
  banksavm$clear[(j-3*month)]<-sum(banksavw$clear[(j-3):j])
  banksavm$RWA_gap[(j-3*month)]<-mean(banksavw$RWA_gap[(j-3):j])
  banksavm$markup_LL[(j-3*month)]<-mean(banksavw$markup_LL[(j-3):j])
  banksavm$markup_M[(j-3*month)]<-mean(banksavw$markup_M[(j-3):j])
  banksavm$defrateLL[(j-3*month)]<-mean(banksavw$defrateLL[(j-3):j])
  banksavm$defrateM[(j-3*month)]<-mean(banksavw$defrateM[(j-3):j])
  banksavm$detshareDh[(j-3*month)]<-mean(banksavw$detshareDh[(j-3):j])
  banksavm$detshareDf[(j-3*month)]<-mean(banksavw$detshareDf[(j-3):j])
  banksavm$detshareLL[(j-3*month)]<-mean(banksavw$detshareLL[(j-3):j])
  banksavm$detshareM[(j-3*month)]<-mean(banksavw$detshareM[(j-3):j])
  banksavm$randshareDh[(j-3*month)]<-mean(banksavw$randshareDh[(j-3):j])
  banksavm$randshareDf[(j-3*month)]<-mean(banksavw$randshareDf[(j-3):j])
  banksavm$randshareLL[(j-3*month)]<-mean(banksavw$randshareLL[(j-3):j])
  banksavm$randshareM[(j-3*month)]<-mean(banksavw$randshareM[(j-3):j])
  banksavm$defrandh[(j-3*month)]<-mean(banksavw$defrandh[(j-3):j])
  banksavm$defrandf[(j-3*month)]<-mean(banksavw$defrandf[(j-3):j])
  banksavm$v_bbe1[(j-3*month)]<-mean(banksavw$v_bbe1[(j-3):j])
  banksavm$v_bbe2[(j-3*month)]<-mean(banksavw$v_bbe2[(j-3):j])
  banksavm$v_bbe3[(j-3*month)]<-mean(banksavw$v_bbe3[(j-3):j])
  banksavm$v_bbe4[(j-3*month)]<-mean(banksavw$v_bbe4[(j-3):j])
  banksavm$fitvbb1[(j-3*month)]<-mean(banksavw$fitvbb1[(j-3):j])
  banksavm$fitvbb2[(j-3*month)]<-mean(banksavw$fitvbb2[(j-3):j])
  banksavm$fitvbb3[(j-3*month)]<-mean(banksavw$fitvbb3[(j-3):j])
  banksavm$fitvbb4[(j-3*month)]<-mean(banksavw$fitvbb4[(j-3):j])
  banksavm$maxfitvbb[(j-3*month)]<-mean(banksavw$maxfitvbb[(j-3):j])
  }}
  quarter=0
  for(j in 1:nrow(banksavw)){
  if(j/12==round(j/12)){
  quarter=quarter+1
  banksavq$sav_b[(j-11*quarter)]<-sum(banksavw$sav_b[(j-11):j])
  banksavq$pr_b[(j-11*quarter)]<-sum(banksavw$pr_b[(j-11):j])
  banksavq$v_b[(j-11*quarter)]<-mean(banksavw$v_b[(j-11):j])
  banksavq$v_bb[(j-11*quarter)]<-mean(banksavw$v_bb[(j-11):j])
  banksavq$v_bbe[(j-11*quarter)]<-mean(banksavw$v_bbe[(j-11):j])
  banksavq$v_bbt[(j-11*quarter)]<-mean(banksavw$v_bbt[(j-11):j])
  banksavq$a_b[(j-11*quarter)]<-mean(banksavw$a_b[(j-11):j])
  banksavq$r_b[(j-11*quarter)]<-mean(banksavw$r_b[(j-11):j])
  banksavq$iM_b[(j-11*quarter)]<-sum(banksavw$iM_b[(j-11):j])
  banksavq$iLL_b[(j-11*quarter)]<-sum(banksavw$iLL_b[(j-11):j])
  banksavq$iD_hb[(j-11*quarter)]<-sum(banksavw$iD_hb[(j-11):j])
  banksavq$iD_fb[(j-11*quarter)]<-sum(banksavw$iD_fb[(j-11):j])
  banksavq$iIB_b[(j-11*quarter)]<-sum(banksavw$iIB_b[(j-11):j])
  banksavq$M_supb[(j-11*quarter)]<-sum(banksavw$M_supb[(j-11):j])
  banksavq$rep_mb[(j-11*quarter)]<-sum(banksavw$rep_mb[(j-11):j])
  banksavq$M_npb[(j-11*quarter)]<-sum(banksavw$M_npb[(j-11):j])
  banksavq$LL_supb[(j-11*quarter)]<-sum(banksavw$LL_supb[(j-11):j])
  banksavq$rep_LLb[(j-11*quarter)]<-sum(banksavw$rep_LLb[(j-11):j])
  banksavq$LL_npb[(j-11*quarter)]<-sum(banksavw$LL_npb[(j-11):j])
  banksavq$M_b[(j-11*quarter)]<-mean(banksavw$M_b[(j-11):j])
  banksavq$LL_b[(j-11*quarter)]<-mean(banksavw$LL_b[(j-11):j])
  banksavq$clear_b[(j-11*quarter)]<-sum(banksavw$clear_b[(j-11):j])
  banksavq$clearalt_b[(j-11*quarter)]<-sum(banksavw$clearalt_b[(j-11):j])
  banksavq$R_pb[(j-11*quarter)]<-mean(banksavw$R_pb[(j-11):j])
  banksavq$R_paltb[(j-11*quarter)]<-mean(banksavw$R_paltb[(j-11):j])
  banksavq$R_tb[(j-11*quarter)]<-mean(banksavw$R_tb[(j-11):j])
  banksavq$R_anb[(j-11*quarter)]<-mean(banksavw$R_anb[(j-11):j])
  banksavq$IBL_sb[(j-11*quarter)]<-mean(banksavw$IBL_sb[(j-11):j])
  banksavq$IBA_db[(j-11*quarter)]<-mean(banksavw$IBA_db[(j-11):j])
  banksavq$IBA_b[(j-11*quarter)]<-mean(banksavw$IBA_b[(j-11):j])
  banksavq$IBL_b[(j-11*quarter)]<-mean(banksavw$IBL_b[(j-11):j])
  banksavq$R_lb[(j-11*quarter)]<-mean(banksavw$R_lb[(j-11):j])
  banksavq$div_b[(j-11*quarter)]<-sum(banksavw$div_b[(j-11):j])
  banksavq$div_bt[(j-11*quarter)]<-sum(banksavw$div_bt[(j-11):j])
  banksavq$bb_b[(j-11*quarter)]<-mean(banksavw$bb_b[(j-11):j])
  banksavq$r_LLb[(j-11*quarter)]<-mean(banksavw$r_LLb[(j-11):j])
  banksavq$r_Mb[(j-11*quarter)]<-mean(banksavw$r_Mb[(j-11):j])
  banksavq$r_db[(j-11*quarter)]<-mean(banksavw$r_db[(j-11):j])
  banksavq$CARb[(j-11*quarter)]<-mean(banksavw$CARb[(j-11):j])
  banksavq$LCRb[(j-11*quarter)]<-mean(banksavw$LCRb[(j-11):j])
  banksavq$ir_b[(j-11*quarter)]<-sum(banksavw$ir_b[(j-11):j])
  banksavq$ia_b[(j-11*quarter)]<-sum(banksavw$ia_b[(j-11):j])
  banksavq$bal_bb[(j-11*quarter)]<-sum(banksavw$bal_bb[(j-11):j])
  banksavq$D_hb[(j-11*quarter)]<-mean(banksavw$D_hb[(j-11):j])
  banksavq$D_fb[(j-11*quarter)]<-mean(banksavw$D_fb[(j-11):j])
  banksavq$carshare[(j-11*quarter)]<-mean(banksavw$carshare[(j-11):j])
  banksavq$rdshare[(j-11*quarter)]<-mean(banksavw$rdshare[(j-11):j])
  banksavq$rllshare[(j-11*quarter)]<-mean(banksavw$rllshare[(j-11):j])
  banksavq$rmshare[(j-11*quarter)]<-mean(banksavw$rmshare[(j-11):j])
  banksavq$IBSshare[(j-11*quarter)]<-mean(banksavw$IBSshare[(j-11):j])
  banksavq$IBDshare[(j-11*quarter)]<-mean(banksavw$IBDshare[(j-11):j])
  banksavq$rationM[(j-11*quarter)]<-mean(banksavw$rationM[(j-11):j])
  banksavq$rationLL[(j-11*quarter)]<-mean(banksavw$rationLL[(j-11):j])
  banksavq$M_db[(j-11*quarter)]<-sum(banksavw$M_db[(j-11):j])
  banksavq$LL_db[(j-11*quarter)]<-sum(banksavw$LL_db[(j-11):j])
  banksavq$bust[(j-11*quarter)]<-sum(banksavw$bust[(j-11):j])
  banksavq$rLLrel[(j-11*quarter)]<-mean(banksavw$rLLrel[(j-11):j])
  banksavq$rMrel[(j-11*quarter)]<-mean(banksavw$rMrel[(j-11):j])
  banksavq$rDrel[(j-11*quarter)]<-mean(banksavw$rDrel[(j-11):j])
  banksavq$Mlag[(j-11*quarter)]<-mean(banksavw$Mlag[(j-11):j])
  banksavq$LLlag[(j-11*quarter)]<-mean(banksavw$LLlag[(j-11):j])
  banksavq$Dhlag[(j-11*quarter)]<-mean(banksavw$Dhlag[(j-11):j])
  banksavq$Dflag[(j-11*quarter)]<-mean(banksavw$Dflag[(j-11):j])
  banksavq$default_LL[(j-11*quarter)]<-mean(banksavw$default_LL[(j-11):j])
  banksavq$default_M[(j-11*quarter)]<-mean(banksavw$default_M[(j-11):j])
  banksavq$clear[(j-11*quarter)]<-sum(banksavw$clear[(j-11):j])
  banksavq$RWA_gap[(j-11*quarter)]<-mean(banksavw$RWA_gap[(j-11):j])
  banksavq$markup_LL[(j-11*quarter)]<-mean(banksavw$markup_LL[(j-11):j])
  banksavq$markup_M[(j-11*quarter)]<-mean(banksavw$markup_M[(j-11):j])
  banksavq$defrateLL[(j-11*quarter)]<-mean(banksavw$defrateLL[(j-11):j])
  banksavq$defrateM[(j-11*quarter)]<-mean(banksavw$defrateM[(j-11):j])
  banksavq$detshareDh[(j-11*quarter)]<-mean(banksavw$detshareDh[(j-11):j])
  banksavq$detshareDf[(j-11*quarter)]<-mean(banksavw$detshareDf[(j-11):j])
  banksavq$detshareLL[(j-11*quarter)]<-mean(banksavw$detshareLL[(j-11):j])
  banksavq$detshareM[(j-11*quarter)]<-mean(banksavw$detshareM[(j-11):j])
  banksavq$randshareDh[(j-11*quarter)]<-mean(banksavw$randshareDh[(j-11):j])
  banksavq$randshareDf[(j-11*quarter)]<-mean(banksavw$randshareDf[(j-11):j])
  banksavq$randshareLL[(j-11*quarter)]<-mean(banksavw$randshareLL[(j-11):j])
  banksavq$randshareM[(j-11*quarter)]<-mean(banksavw$randshareM[(j-11):j])
  banksavq$defrandh[(j-11*quarter)]<-mean(banksavw$defrandh[(j-11):j])
  banksavq$defrandf[(j-11*quarter)]<-mean(banksavw$defrandf[(j-11):j])
  banksavq$v_bbe1[(j-11*quarter)]<-mean(banksavw$v_bbe1[(j-11):j])
  banksavq$v_bbe2[(j-11*quarter)]<-mean(banksavw$v_bbe2[(j-11):j])
  banksavq$v_bbe3[(j-11*quarter)]<-mean(banksavw$v_bbe3[(j-11):j])
  banksavq$v_bbe4[(j-11*quarter)]<-mean(banksavw$v_bbe4[(j-11):j])
  banksavq$fitvbb1[(j-11*quarter)]<-mean(banksavw$fitvbb1[(j-11):j])
  banksavq$fitvbb2[(j-11*quarter)]<-mean(banksavw$fitvbb2[(j-11):j])
  banksavq$fitvbb3[(j-11*quarter)]<-mean(banksavw$fitvbb3[(j-11):j])
  banksavq$fitvbb4[(j-11*quarter)]<-mean(banksavw$fitvbb4[(j-11):j])
  banksavq$maxfitvbb[(j-11*quarter)]<-mean(banksavw$maxfitvbb[(j-11):j])
  }}
  year=0
  for(j in 1:nrow(banksavw)){
  if(j/48==round(j/48)){
  year=year+1
  banksava$sav_b[(j-47*year)]<-sum(banksavw$sav_b[(j-47):j])
  banksava$pr_b[(j-47*year)]<-sum(banksavw$pr_b[(j-47):j])
  banksava$v_b[(j-47*year)]<-mean(banksavw$v_b[(j-47):j])
  banksava$v_bb[(j-47*year)]<-mean(banksavw$v_bb[(j-47):j])
  banksava$v_bbe[(j-47*year)]<-mean(banksavw$v_bbe[(j-47):j])
  banksava$v_bbt[(j-47*year)]<-mean(banksavw$v_bbt[(j-47):j])
  banksava$a_b[(j-47*year)]<-mean(banksavw$a_b[(j-47):j])
  banksava$r_b[(j-47*year)]<-mean(banksavw$r_b[(j-47):j])
  banksava$iM_b[(j-47*year)]<-sum(banksavw$iM_b[(j-47):j])
  banksava$iLL_b[(j-47*year)]<-sum(banksavw$iLL_b[(j-47):j])
  banksava$iD_hb[(j-47*year)]<-sum(banksavw$iD_hb[(j-47):j])
  banksava$iD_fb[(j-47*year)]<-sum(banksavw$iD_fb[(j-47):j])
  banksava$iIB_b[(j-47*year)]<-sum(banksavw$iIB_b[(j-47):j])
  banksava$M_supb[(j-47*year)]<-sum(banksavw$M_supb[(j-47):j])
  banksava$rep_mb[(j-47*year)]<-sum(banksavw$rep_mb[(j-47):j])
  banksava$M_npb[(j-47*year)]<-sum(banksavw$M_npb[(j-47):j])
  banksava$LL_supb[(j-47*year)]<-sum(banksavw$LL_supb[(j-47):j])
  banksava$rep_LLb[(j-47*year)]<-sum(banksavw$rep_LLb[(j-47):j])
  banksava$LL_npb[(j-47*year)]<-sum(banksavw$LL_npb[(j-47):j])
  banksava$M_b[(j-47*year)]<-mean(banksavw$M_b[(j-47):j])
  banksava$LL_b[(j-47*year)]<-mean(banksavw$LL_b[(j-47):j])
  banksava$clear_b[(j-47*year)]<-sum(banksavw$clear_b[(j-47):j])
  banksava$clearalt_b[(j-47*year)]<-sum(banksavw$clearalt_b[(j-47):j])
  banksava$R_pb[(j-47*year)]<-mean(banksavw$R_pb[(j-47):j])
  banksava$R_paltb[(j-47*year)]<-mean(banksavw$R_paltb[(j-47):j])
  banksava$R_tb[(j-47*year)]<-mean(banksavw$R_tb[(j-47):j])
  banksava$R_anb[(j-47*year)]<-mean(banksavw$R_anb[(j-47):j])
  banksava$IBL_sb[(j-47*year)]<-mean(banksavw$IBL_sb[(j-47):j])
  banksava$IBA_db[(j-47*year)]<-mean(banksavw$IBA_db[(j-47):j])
  banksava$IBA_b[(j-47*year)]<-mean(banksavw$IBA_b[(j-47):j])
  banksava$IBL_b[(j-47*year)]<-mean(banksavw$IBL_b[(j-47):j])
  banksava$R_lb[(j-47*year)]<-mean(banksavw$R_lb[(j-47):j])
  banksava$div_b[(j-47*year)]<-sum(banksavw$div_b[(j-47):j])
  banksava$div_bt[(j-47*year)]<-sum(banksavw$div_bt[(j-47):j])
  banksava$bb_b[(j-47*year)]<-mean(banksavw$bb_b[(j-47):j])
  banksava$r_LLb[(j-47*year)]<-mean(banksavw$r_LLb[(j-47):j])
  banksava$r_Mb[(j-47*year)]<-mean(banksavw$r_Mb[(j-47):j])
  banksava$r_db[(j-47*year)]<-mean(banksavw$r_db[(j-47):j])
  banksava$CARb[(j-47*year)]<-mean(banksavw$CARb[(j-47):j])
  banksava$LCRb[(j-47*year)]<-mean(banksavw$LCRb[(j-47):j])
  banksava$ir_b[(j-47*year)]<-sum(banksavw$ir_b[(j-47):j])
  banksava$ia_b[(j-47*year)]<-sum(banksavw$ia_b[(j-47):j])
  banksava$bal_bb[(j-47*year)]<-sum(banksavw$bal_bb[(j-47):j])
  banksava$D_hb[(j-47*year)]<-mean(banksavw$D_hb[(j-47):j])
  banksava$D_fb[(j-47*year)]<-mean(banksavw$D_fb[(j-47):j])
  banksava$carshare[(j-47*year)]<-mean(banksavw$carshare[(j-47):j])
  banksava$rdshare[(j-47*year)]<-mean(banksavw$rdshare[(j-47):j])
  banksava$rllshare[(j-47*year)]<-mean(banksavw$rllshare[(j-47):j])
  banksava$rmshare[(j-47*year)]<-mean(banksavw$rmshare[(j-47):j])
  banksava$IBSshare[(j-47*year)]<-mean(banksavw$IBSshare[(j-47):j])
  banksava$IBDshare[(j-47*year)]<-mean(banksavw$IBDshare[(j-47):j])
  banksava$rationM[(j-47*year)]<-mean(banksavw$rationM[(j-47):j])
  banksava$rationLL[(j-47*year)]<-mean(banksavw$rationLL[(j-47):j])
  banksava$M_db[(j-47*year)]<-sum(banksavw$M_db[(j-47):j])
  banksava$LL_db[(j-47*year)]<-sum(banksavw$LL_db[(j-47):j])
  banksava$bust[(j-47*year)]<-sum(banksavw$bust[(j-47):j])
  banksava$rLLrel[(j-47*year)]<-mean(banksavw$rLLrel[(j-47):j])
  banksava$rMrel[(j-47*year)]<-mean(banksavw$rMrel[(j-47):j])
  banksava$rDrel[(j-47*year)]<-mean(banksavw$rDrel[(j-47):j])
  banksava$Mlag[(j-47*year)]<-mean(banksavw$Mlag[(j-47):j])
  banksava$LLlag[(j-47*year)]<-mean(banksavw$LLlag[(j-47):j])
  banksava$Dhlag[(j-47*year)]<-mean(banksavw$Dhlag[(j-47):j])
  banksava$Dflag[(j-47*year)]<-mean(banksavw$Dflag[(j-47):j])
  banksava$default_LL[(j-47*year)]<-mean(banksavw$default_LL[(j-47):j])
  banksava$default_M[(j-47*year)]<-mean(banksavw$default_M[(j-47):j])
  banksava$clear[(j-47*year)]<-sum(banksavw$clear[(j-47):j])
  banksava$RWA_gap[(j-47*year)]<-mean(banksavw$RWA_gap[(j-47):j])
  banksava$markup_LL[(j-47*year)]<-mean(banksavw$markup_LL[(j-47):j])
  banksava$markup_M[(j-47*year)]<-mean(banksavw$markup_M[(j-47):j])
  banksava$defrateLL[(j-47*year)]<-mean(banksavw$defrateLL[(j-47):j])
  banksava$defrateM[(j-47*year)]<-mean(banksavw$defrateM[(j-47):j])
  banksava$detshareDh[(j-47*year)]<-mean(banksavw$detshareDh[(j-47):j])
  banksava$detshareDf[(j-47*year)]<-mean(banksavw$detshareDf[(j-47):j])
  banksava$detshareLL[(j-47*year)]<-mean(banksavw$detshareLL[(j-47):j])
  banksava$detshareM[(j-47*year)]<-mean(banksavw$detshareM[(j-47):j])
  banksava$randshareDh[(j-47*year)]<-mean(banksavw$randshareDh[(j-47):j])
  banksava$randshareDf[(j-47*year)]<-mean(banksavw$randshareDf[(j-47):j])
  banksava$randshareLL[(j-47*year)]<-mean(banksavw$randshareLL[(j-47):j])
  banksava$randshareM[(j-47*year)]<-mean(banksavw$randshareM[(j-47):j])
  banksava$defrandh[(j-47*year)]<-mean(banksavw$defrandh[(j-47):j])
  banksava$defrandf[(j-47*year)]<-mean(banksavw$defrandf[(j-47):j])
  banksava$v_bbe1[(j-47*year)]<-mean(banksavw$v_bbe1[(j-47):j])
  banksava$v_bbe2[(j-47*year)]<-mean(banksavw$v_bbe2[(j-47):j])
  banksava$v_bbe3[(j-47*year)]<-mean(banksavw$v_bbe3[(j-47):j])
  banksava$v_bbe4[(j-47*year)]<-mean(banksavw$v_bbe4[(j-47):j])
  banksava$fitvbb1[(j-47*year)]<-mean(banksavw$fitvbb1[(j-47):j])
  banksava$fitvbb2[(j-47*year)]<-mean(banksavw$fitvbb2[(j-47):j])
  banksava$fitvbb3[(j-47*year)]<-mean(banksavw$fitvbb3[(j-47):j])
  banksava$fitvbb4[(j-47*year)]<-mean(banksavw$fitvbb4[(j-47):j])
  banksava$maxfitvbb[(j-47*year)]<-mean(banksavw$maxfitvbb[(j-47):j])
  }}
  Modelreturns<-list(weekoutput=weekoutput,monthoutput=monthoutput,quarteroutput=quarteroutput,yearoutput=yearoutput,banksavw=banksavw,banksavm=banksavm,banksavq=banksavq,banksava=banksava,banksw=banksw,bNames=bNames)
  filename<-paste("data","mc",args$m,sep="")
  save(Modelreturns,file = paste(foldername,filename, ".Rdata", sep=''))
}
}



######Define functions here#####

######Accounting identities#####
#####Households####
Hidentities1<-function(Households=stop("need to have households defined!"),CB=stop("Need to have CB defined!"),Banks=stop("need to have Banks (agg) defined!"),Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Households[[args$t,"YD"]]=Firms[[args$t,"WB"]]+Households[[args$t,"iD_h"]]+Firms[[args$t,"div_f"]]+Banks[[args$t,"Div_b"]]+Gov[[args$t,"iGB_h"]]-Gov[[args$t,"Tax"]]-Households[[args$t,"iM"]]+Households[[args$t,"M_np"]]
  Households[[args$t,"YD_tax"]]=Firms[[args$t,"WB"]]+Households[[args$t,"iD_h"]]+Firms[[args$t,"div_f"]]+Gov[[args$t,"iGB_h"]]+Banks[[args$t,"Div_b"]]
  Households[[args$t,"sav_h"]]=Firms[[args$t,"WB"]]+Households[[args$t,"iD_h"]]+Firms[[args$t,"div_f"]]+Banks[[args$t,"Div_b"]]+Gov[[args$t,"iGB_h"]]-Gov[[args$t,"Tax"]]-Households[[args$t,"iM"]]-Firms[[args$t,"C"]]
  Households[[args$t,"deltaM"]]=-Households[[args$t,"rep_m"]]-Households[[args$t,"M_np"]]+Households[[args$t,"M_sup"]]
  Households[[args$t,"M"]]=Households[[(args$t-1),"M"]]+Households[[args$t,"deltaM"]]
  Households[[args$t,"H"]]=Households[[args$t,"p_h"]]*params[[1,"h"]]
  return(Households)
}


Hidentities2<-function(Households=stop("need to have households defined!"),CB=stop("Need to have CB defined!"),Banks=stop("need to have Banks (agg) defined!"),Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Households[[args$t,"D_h"]]=Households[[(args$t-1),"D_h"]]+Households[[args$t,"sav_h"]]+Households[[args$t,"M_sup"]]-Households[[args$t,"rep_m"]]-(Households[[args$t,"gb_h"]]-Households[[(args$t-1),"gb_h"]])
  Households[[args$t,"V_h"]]=Households[[args$t,"D_h"]]+Households[[args$t,"p_h"]]*params[[1,"h"]]-Households[[args$t,"M"]]+Households[[args$t,"gb_h"]]+Banks[[args$t,"bb"]]+params[[1,"E_f"]]
  return(Households)
}



#####Firms####
Fprofit<-function(Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Firms[[args$t,"Pr_f"]]=Firms[[args$t,"C"]]+Firms[[args$t,"I"]]+Gov[[args$t,"G"]]+Firms[[args$t,"iD_f"]]-Firms[[args$t,"WB"]]-Firms[[args$t,"iLL"]]+Firms[[args$t,"LL_np"]]-Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]]*Firms[[(args$t-1),"p"]]
  return(Firms)
}

FSave<-function(Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Firms[[args$t,"sav_f"]]=Firms[[args$t,"C"]]+Gov[[args$t,"G"]]+Firms[[args$t,"I"]]+Firms[[args$t,"iD_f"]]-Firms[[args$t,"WB"]]-Firms[[args$t,"div_f"]]-Firms[[args$t,"iLL"]]-Gov[[args$t,"FTax"]]
  return(Firms)
}

Floanswealth<-function(Firms=stop("need to have firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Firms[[args$t,"D_f"]]=Firms[[(args$t-1),"D_f"]]+Firms[[args$t,"sav_f"]]+Banks[[args$t,"LL_sup"]]-Firms[[args$t,"rep_LL"]]-Firms[[args$t,"I"]]
  Firms[[args$t,"LL"]]=Firms[[(args$t-1),"LL"]]+Banks[[args$t,"LL_sup"]]-Firms[[args$t,"rep_LL"]]-Firms[[args$t,"LL_np"]]
  Firms[[args$t,"V_f"]]=Firms[[args$t,"K"]]+Firms[[args$t,"D_f"]]-Firms[[args$t,"LL"]]-params[[1,"E_f"]]
  Firms[[args$t,"lev_f"]]=Firms[[args$t,"LL"]]/Firms[[args$t,"K"]]
  return(Firms)
}

Capital<-function(Firms=stop("need to have firms defined!"),params=params,args=args){
  Firms[[args$t,"k"]]=Firms[[(args$t-1),"k"]]+Firms[[args$t,"i"]]-Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]]
  Firms[[args$t,"K"]]=Firms[[(args$t-1),"k"]]*Firms[[(args$t-1),"p"]]-Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]]*Firms[[(args$t-1),"p"]]+(Firms[[args$t,"p"]]-Firms[[(args$t-1),"p"]])*(Firms[[(args$t-1),"k"]]-Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]])+Firms[[args$t,"p"]]*Firms[[args$t,"i"]]
  return(Firms)
}

######Gov####
Borrowreq<-function(Gov=stop("need to have gov. defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Gov[[args$t,"PSBR"]]=Gov[[args$t,"G"]]+Gov[[args$t,"iGB"]]-CB[[args$t,"PCB"]]-Gov[[args$t,"Tax"]]-Gov[[args$t,"FTax"]]
  Gov[[args$t,"sav_g"]]=-Gov[[args$t,"PSBR"]]
  return(Gov)
}

Gwealth<-function(Gov=stop("need to have gov. defined!"),params=params,args=args){
  Gov[[args$t,"V_g"]]=-Gov[[args$t,"gb"]]
  return(Gov)
}


######CB####
CBprofit<-function(Gov=stop("need to have gov. defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  CB[[args$t,"PCB"]]=Gov[[args$t,"iGB_cb"]]+CB[[args$t,"iA"]]-CB[[args$t,"iR"]]
  CB[[args$t,"sav_cb"]]=Gov[[args$t,"iGB_cb"]]+CB[[args$t,"iA"]]-CB[[args$t,"iR"]]-CB[[args$t,"PCB"]]
  return(CB)
}

CBwealth<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks defined!"),params=params,args=args){
  CB[[args$t,"V_cb"]]=CB[[args$t,"gb_cb"]]+CB[[args$t,"A"]]-CB[[args$t,"R"]]
  return(CB)
}


######Banks####
#ABM#####
bsave<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks defined!"),params=params,args=args){
    banks[args$t,"sav_b",]=banks[args$t,"iM_b",]+banks[args$t,"iLL_b",]+banks[args$t,"iIB_b",]+banks[args$t,"ir_b",]-banks[args$t,"iD_hb",]-banks[args$t,"iD_fb",]-banks[args$t,"ia_b",]-banks[args$t,"div_b",]
  return(banks)
}

bprofit<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
    banks[args$t,"pr_b",]=banks[args$t,"iM_b",]+banks[args$t,"iLL_b",]+banks[args$t,"iIB_b",]+banks[args$t,"ir_b",]-banks[args$t,"iD_hb",]-banks[args$t,"iD_fb",]-banks[args$t,"ia_b",]-banks[args$t,"LL_npb",]-banks[args$t,"M_npb",]
  return(banks)
}

bwealth<-function(banks=stop("need to have banks defined!"),params=params,args=args){
    banks[args$t,"v_b",]=banks[args$t,"r_b",]+banks[args$t,"LL_b",]+banks[args$t,"M_b",]+banks[args$t,"IBL_b",]-banks[args$t,"D_hb",]-banks[args$t,"D_fb",]-banks[args$t,"IBA_b",]-banks[args$t,"a_b",]-banks[args$t,"bb_b",]
    banks[args$t,"v_bb",]=banks[args$t,"r_b",]+banks[args$t,"LL_b",]+banks[args$t,"M_b",]+banks[args$t,"IBL_b",]-banks[args$t,"D_hb",]-banks[args$t,"D_fb",]-banks[args$t,"IBA_b",]-banks[args$t,"a_b",]
    banks[args$t,"bust",]=ifelse(banks[args$t,"v_bb",]<0,1,0)
  return(banks)
}


badv<-function(banks=stop("need to have banks defined!"),params=params,args=args){
    banks[args$t,"a_b",]=pmax(banks[args$t,"R_tb",]-banks[args$t,"R_lb",],0)
  return(banks)
}

bres<-function(banks=stop("need to have banks defined!"),params=params,args=args){
    banks[args$t,"r_b",]=banks[args$t,"R_lb",]+banks[args$t,"a_b",]
  return(banks)
}

#aggregate#####
Bsave<-function(banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"sav_B"]]=sum(banks[args$t,"sav_b",])
  return(Banks)
}

Bprofit<-function(banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"Pr_b"]]=sum(banks[args$t,"pr_b",])
  return(Banks)
}

Bwealth<-function(banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"V_b"]]=sum(banks[args$t,"v_b",])
  Banks[[args$t,"V_bb"]]=sum(banks[args$t,"v_bb",])
  Banks[[args$t,"Bust"]]=sum(banks[args$t,"bust",])
  return(Banks)
}

#####sectoral balances####
balances<-function(Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),Gov=stop("need to have Gov defined!"),CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have Banks (aggregate) defined!"),Aux=stop("need to have auxiliary array defined!"),params=params,args=args){
  banks[args$t,"bal_bb",]=banks[args$t,"iM_b",]+banks[args$t,"iLL_b",]+banks[args$t,"iIB_b",]+banks[args$t,"ir_b",]-banks[args$t,"iD_hb",]-banks[args$t,"iD_fb",]-banks[args$t,"ia_b",]-banks[args$t,"div_b",]
  Households[[args$t,"bal_h"]]=Firms[[args$t,"WB"]]+Households[[args$t,"iD_h"]]+Firms[[args$t,"div_f"]]-Firms[[args$t,"C"]]-Gov[[args$t,"Tax"]]-Households[[args$t,"iM"]]+Banks[[args$t,"Div_b"]]++Gov[[args$t,"iGB_h"]]
  Firms[[args$t,"bal_f"]]=Firms[[args$t,"C"]]+Gov[[args$t,"G"]]+Firms[[args$t,"iD_f"]]-Firms[[args$t,"WB"]]-Firms[[args$t,"div_f"]]-Firms[[args$t,"iLL"]]
  Banks[[args$t,"bal_b1"]]=sum(banks[args$t,"bal_bb",])
  Banks[[args$t,"bal_b2"]]=Households[[args$t,"iM"]]+Firms[[args$t,"iLL"]]-Banks[[args$t,"Div_b"]]-CB[[args$t,"iA"]]+CB[[args$t,"iR"]]-Households[[args$t,"iD_h"]]-Firms[[args$t,"iD_f"]]
  CB[[args$t,"bal_cb"]]=Gov[[args$t,"iGB_cb"]]+CB[[args$t,"iA"]]-CB[[args$t,"iR"]]-CB[[args$t,"PCB"]]
  Gov[[args$t,"bal_g"]]=Gov[[args$t,"Tax"]]+CB[[args$t,"PCB"]]-Gov[[args$t,"G"]]-Gov[[args$t,"iGB"]]
  Aux[[args$t,"SFCcheck1"]]=Households[[args$t,"bal_h"]]+Firms[[args$t,"bal_f"]]+Banks[[args$t,"bal_b1"]]+CB[[args$t,"bal_cb"]]+Gov[[args$t,"bal_g"]]
  Aux[[args$t,"SFCcheck2"]]=Households[[args$t,"V_h"]]+Firms[[args$t,"V_f"]]+Banks[[args$t,"V_b"]]+Gov[[args$t,"V_g"]]+CB[[args$t,"V_cb"]]-Households[[args$t,"p_h"]]*params[[1,"h"]]-Firms[[args$t,"p"]]*Firms[[args$t,"k"]]
  Aux[[args$t,"SFCcheck4"]]=Households[[args$t,"V_h"]]+Firms[[args$t,"V_f"]]+sum(banks[args$t,"v_b",])+Gov[[args$t,"V_g"]]+CB[[args$t,"V_cb"]]-Households[[args$t,"p_h"]]*params[[1,"h"]]-Firms[[args$t,"p"]]*Firms[[args$t,"k"]]
  Aux[[args$t,"SFCcheck3"]]=Banks[[args$t,"bal_b1"]]-Banks[[args$t,"bal_b2"]]
  return(list(Households=Households,Firms=Firms,banks=banks,Banks=Banks,Gov=Gov,CB=CB,Aux=Aux))
}

######
####Equations
####Households####
decideConsumption<-function(Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args){
  Households[[args$t,"c_d"]]=Households[[(args$t-1),"c_d"]]+(Households[[args$t,"alpha1"]]*Households[[args$t,"yd_e"]]+Households[[args$t,"alpha2"]]/48*Households[[(args$t),"v_he"]]-Households[[(args$t-1),"c_d"]])/12
  return(Households)
}

updateMPC<-function(Households=stop("need to have households defined!"),params=params,args=args){
    Households[[args$t,"alpha1"]]=Households[[(args$t-1),"alpha1"]]+(params[[1,"alpha1L"]]+((params[[1,"alpha1U"]]-params[[1,"alpha1L"]])/(1+exp(params[[1,"sigmampc1"]]*Households[[args$t,"rr_he"]]-params[[1,"sigmampc2"]])))-Households[[(args$t-1),"alpha1"]])/24
    Households[[args$t,"alpha2"]]=Households[[(args$t-1),"alpha2"]]+(params[[1,"alpha2L"]]+((params[[1,"alpha2U"]]-params[[1,"alpha2L"]])/(1+exp(params[[1,"sigmampc1"]]*Households[[args$t,"rr_he"]]-params[[1,"sigmampc2"]])))-Households[[(args$t-1),"alpha2"]])/24
  return(Households)
}


hhreturns<-function(Households=stop("Need to have households defined!"),Gov=stop("Need to have Gov defined!"),Banks=stop("Need to have banks (aggregate) defined!"),CB=stop("Need to have CB defined!"),params=params,args=args){
  Households[[args$t,"rr_h"]]=((1+Banks[[args$t,"r_dav"]])/(1+CB[[args$t,"pi_q"]])-1)*Households[[args$t,"D_h"]]/(Households[[args$t,"TAA_h"]]+Households[[args$t,"p_h"]]*params[1,"h"])+((1+Gov[[args$t,"r_gb"]])/(1+CB[[args$t,"pi_q"]])-1)*Households[[args$t,"gb_h"]]/(Households[[args$t,"TAA_h"]]+Households[[args$t,"p_h"]]*params[1,"h"])+(((1+(Households[args$t,"p_h"]-Households[(args$t-1),"p_h"])/Households[(args$t-1),"p_h"])^48)/(1+CB[args$t,"pi_q"])-1)*(Households[[args$t,"p_h"]]*params[1,"h"])/(Households[[args$t,"TAA_h"]]+Households[[args$t,"p_h"]]*params[1,"h"])
  return(Households)
}

ndemandhouse<-function(Households=stop("need to have households defined!"),Banks=stop("Need to have banks (aggregate) defined!"),params=params,args=args,Timer=Timer){
# if(args$t/12==round(args$t/12)){
#  gph=(Households[(args$t-1),"p_h"]-Households[(args$t-11),"p_h"]/Households[(args$t-11),"p_h"])
#  Households[args$t,"LTVt"]=params[[1,"LTV"]]*(2/(1+exp(10*gph)))
#  Households[args$t,"LTVgap"]=Households[args$t,"LTVt"]-Households[(args$t-1),"LTV"]
#  Households[args$t,"LTV"]=Households[(args$t-1),"LTV"]+(Households[args$t,"LTVgap"])/12
# }else{
#  Households[args$t,"LTVt"]=Households[(args$t-1),"LTVt"]
#  Households[args$t,"LTVgap"]=Households[(args$t-1),"LTVgap"]
#  Households[args$t,"LTV"]=Households[(args$t-1),"LTV"]+(Households[args$t,"LTVgap"])/12
# }
  Households[args$t,"LTV"]=params[[1,"LTV"]]
  Households[args$t,"r_mavn"]=Households[(args$t-1),"r_mavn"]
  Households[[args$t,"H_dn"]]=Households[[(args$t-1),"H_dn"]]+(((params[[1,"rho0"]]+params[[1,"rho2"]]*Households[args$t,"LTV"])/48+params[[1,"rho1"]]*(Households[[(args$t),"V_he"]]-((1-((params[1,"alpha1U"]+params[1,"alpha1L"])/2))/((params[1,"alpha2U"]+params[1,"alpha2L"])/2)*Households[[(args$t),"YD_e"]]*48))+params[[1,"rho3"]]*(Households[[args$t,"r_mavn"]]-Households[[args$t,"r_mave"]]))-Households[[(args$t-1),"H_dn"]])/12
  Households[[args$t,"M_d"]]=Households[args$t,"LTV"]*Households[[args$t,"H_dn"]]
  return(Households)
}

efdemandhouse<-function(Households=stop("need to have households defined!"),params=params,args=args){
  Households[[args$t,"H_def"]]=ifelse(Households[[args$t,"M_d"]]>Households[[args$t,"M_sup"]],Households[[args$t,"M_sup"]]+(1-Households[args$t,"LTV"])*Households[[args$t,"H_dn"]],Households[[args$t,"H_dn"]])
  Households[[args$t,"p_h"]]=Households[[args$t,"H_def"]]/((params[[1,"eta"]]/48)*params[[1,"h"]])
  return(Households)
}

TAAlevh<-function(Households=stop("need to have households defined!"),Banks=stop("Need to have Banks defined!"),params=params,args=args){
  Households[[args$t,"lev_h"]]=Households[[args$t,"M"]]/(Households[[args$t,"p_h"]]*params[[1,"h"]]+Households[[args$t,"D_h"]]+Households[[args$t,"gb_h"]])
  Households[[args$t,"TAA_h"]]=Households[[args$t,"D_h"]]+Households[[args$t,"gb_h"]]
  return(Households)
}

Hportfolio<-function(Households=stop("need to have households defined!"),Gov=stop("Need to have Gov defined!"),Banks=stop("Need to have banks (aggregate) defined!"),Aux=stop("Need to have auxiliary array defined!"),params=params,args=args){
    Aux[[args$t,"porthgb"]]=Aux[[(args$t-1),"porthgb"]]+(params[[1,"lambdah10"]]+params[[1,"lambdah11"]]*Gov[[args$t,"r_gb"]]-params[[1,"lambdah12"]]*Banks[[args$t,"r_dav"]]-Aux[[(args$t-1),"porthgb"]])/12
    Households[[args$t,"gb_d"]]=Aux[[args$t,"porthgb"]]*Households[[(args$t-1),"TAA_h"]]
  return(list(Households=Households,Aux=Aux))
}


hbonds<-function(Households=stop("need to have households defined!"),Gov=stop("Need to have Gov defined!"),CB=stop("Need to have CB defined!"),params=params,args=args){
  Households[[args$t,"gb_h"]]=Households[[(args$t-1),"gb_h"]]+Gov[[args$t,"gb_s"]]-CB[[args$t,"gb_rcb"]]-Gov[[args$t,"rep_gbh"]]-CB[[args$t,"gb_dcb"]]+CB[[args$t,"gb_scb"]]
  return(Households)
}

mortdeppayments<-function(Households=stop("Need to have households defined!"),banks=stop("Need to have banks (ABM) defined!"),params=params,args=args){
  Households[[args$t,"iM"]]<-sum(banks[args$t,"iM_b",])
  Households[[args$t,"M_np"]]<-sum(banks[args$t,"M_npb",])
  Households[[args$t,"rep_m"]]=sum(banks[args$t,"rep_mb",])
  Households[[args$t,"iD_h"]]=sum(banks[args$t,"iD_hb",])
  return(Households)
}

newmort<-function(Households=stop("Need to have households defined!"),banks=stop("Need to have banks defined!"),params=params,args=args){
  Households[[args$t,"M_sup"]]=sum(banks[args$t,"M_supb",])
  return(Households)
}

setwage<-function(Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have households defined!"),params=params,args=args){
    Households[[args$t,"u_nw"]]=Firms[[args$t,"u_n"]]
    Households[[args$t,"W"]]=Households[[(args$t-1),"W"]]+((1+Households[(args$t),"pi_eh"])*(params[1,"W_n"]+(params[[1,"beta1"]]*(Firms[[args$t,"u_e"]]-Households[[args$t,"u_nw"]])))-Households[[(args$t-1),"W"]])/24
    return(Households)
}


#####Firms#####

setpricemkup<-function(Firms=stop("need to have firms defined!"),Aux=stop("Need to have auxiliary array defined!"),Households=stop("Need to have Households defined!"),params=params,args=args){
    if(args$t<48){
    Firms[[args$t,"UIC"]]=Firms[[(args$t-1),"UIC"]]
    }else{
    Firms[[args$t,"UIC"]]=mean(Firms[(args$t-47):(args$t),"iLL"]-Firms[(args$t-47):(args$t),"iD_f"])/((mean(Firms[(args$t-48):(args$t-1),"y"])))
    }
    Firms[[args$t,"theta"]]=Firms[[(args$t-1),"theta"]]
    Firms[[args$t,"p"]]=Firms[[(args$t-1),"p"]]+(((1+Firms[[args$t,"theta"]])*(Households[[args$t,"W"]]/params[[1,"alpha"]]+Firms[args$t,"UIC"]))-Firms[[(args$t-1),"p"]])/24
    return(list(Firms=Firms,Aux=Aux))
}


ftleverage<-function(Firms=stop("Need to have firms defined!"),params=params,args=args){
    Firms[[args$t,"lev_ft"]]=params[[1,"lev_ft1"]]
  return(Firms)
}


decideInvestment<-function(Firms=stop("need to have firms defined!"),Banks=stop("Need to have banks (aggregate) defined!"),params=params,args=args,Timer=Timer){
  Firms[args$t,"r_LLavn"]=Firms[(args$t-1),"r_LLavn"]
  Firms[args$t,"gk_des"]=Firms[(args$t-1),"gk_des"]+(params[1,"gamma1"]*(Firms[[args$t,"u_e2"]]-Firms[[args$t,"u_n"]])/Firms[[args$t,"u_n"]]+params[1,"gamma2"]*((Firms[[args$t,"r_LLavn"]])-(Firms[[args$t,"r_LLave"]]))/(Firms[[args$t,"r_LLavn"]])-Firms[(args$t-1),"gk_des"])/12
  Firms[[args$t,"i_des"]]=(1+Firms[args$t,"gk_des"])*Firms[[(args$t-1),"k"]]-Firms[[(args$t-1),"k"]]
  Firms[[args$t,"i_d"]]=max(0,Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]]+Firms[[args$t,"i_des"]])
  return(Firms)
}

floandemand<-function(Firms=stop("need to have firms defined!"),params=params,args=args){
  Firms[[args$t,"LL_d"]]=max(0,Firms[[args$t,"p"]]*Firms[[args$t,"i_d"]]-Firms[[args$t,"sav_ft"]])
  Firms[[args$t,"replacegap"]]=max(0,min(Firms[[args$t,"p"]]*Firms[[args$t,"i_d"]]-Firms[[args$t,"sav_ft"]],Firms[[args$t,"p"]]*Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]]-Firms[[args$t,"sav_ft"]]))
  return(Firms)
}

actuali<-function(Firms=stop("need to have firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Firms[[args$t,"i"]]=ifelse(Firms[[args$t,"LL_d"]]>Banks[[args$t,"LL_sup"]],max(0,(Firms[[args$t,"sav_ft"]]+Banks[[args$t,"LL_sup"]])/Firms[[args$t,"p"]]),max(0,Firms[[args$t,"i_d"]]))
  return(Firms)
}

tsave<-function(Firms=stop("Need to have firms defined!"),params=params,args=args){
  Firms[[args$t,"sav_ft"]]=(Firms[[(args$t-1),"LL"]]+Firms[[args$t,"p"]]*Firms[[args$t,"i_d"]]-Firms[[args$t,"LL_np"]]-Firms[[args$t,"rep_LL"]]-Firms[[args$t,"lev_ft"]]*(Firms[[(args$t-1),"K"]]-Firms[[(args$t-1),"p"]]*Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]]+(Firms[[args$t,"p"]]-Firms[[(args$t-1),"p"]])*(Firms[[(args$t-1),"k"]]-Firms[[args$t,"delta_k"]]*Firms[[(args$t-1),"k"]])+Firms[[args$t,"p"]]*Firms[[args$t,"i_d"]]))/48
  return(Firms)
}

decidey<-function(Firms=stop("need to have firms defined!"),Gov=stop("need to have gov defined!"),Households=stop("need to have households defined!"),params=params,args=args){
  Firms[[args$t,"y"]]=min(Firms[[args$t,"yfc"]]/48,Households[[args$t,"c_d"]]+Firms[[args$t,"i"]]+Gov[[args$t,"g"]])
  Firms[[args$t,"N_d"]]=Firms[[args$t,"y"]]/params[[1,"alpha"]]
  Firms[[args$t,"WB"]]=Households[[args$t,"W"]]*Firms[[args$t,"N_d"]]
  Firms[[args$t,"u"]]=Firms[[args$t,"y"]]/(Firms[[args$t,"yfc"]]/48)
  Firms[[args$t,"UC"]]=Firms[[args$t,"WB"]]/Firms[[args$t,"y"]]
  Firms[[args$t,"c"]]=ifelse(Households[[args$t,"c_d"]]+Firms[[args$t,"i"]]+Gov[[args$t,"g"]]<=Firms[[args$t,"yfc"]]/48,Households[[args$t,"c_d"]],Firms[[args$t,"y"]]-Firms[[args$t,"i"]]-Gov[[args$t,"g"]])
  return(Firms)
}

fullcapy<-function(Firms=stop("need to have firms defined!"),params=params,args=args){
  if(args$t<4){
    Firms[[args$t,"yfc"]]=Firms[[(args$t-1),"yfc"]]
  }
  else{
    Firms[[args$t,"yfc"]]=Firms[[(args$t-3),"k"]]/params[[1,"kappa"]]
  }
  return(Firms)
}

nominalcigy<-function(Firms=stop("Need to have firms defined!"),Gov=stop("need to have gov defined!"),params=params,args=args){
  Firms[[args$t,"C"]]=Firms[[args$t,"c"]]*Firms[[args$t,"p"]]
  Firms[[args$t,"I"]]=Firms[[args$t,"i"]]*Firms[[args$t,"p"]]
  Firms[[args$t,"Y"]]=Firms[[args$t,"y"]]*Firms[[args$t,"p"]]
  Gov[[args$t,"G"]]=Gov[[args$t,"g"]]*Firms[[args$t,"p"]]
  return(list(Firms=Firms,Gov=Gov))
}

fbankpayments<-function(Firms=stop("Need to have firms defined!"),banks=stop("Need to have banks (ABM) defined!"),params=params,args=args){
  Firms[[args$t,"iD_f"]]=sum(banks[args$t,"iD_fb",])
  Firms[[args$t,"iLL"]]=sum(banks[args$t,"iLL_b",])
  Firms[[args$t,"LL_np"]]=sum(banks[args$t,"LL_npb",])
  Firms[[args$t,"rep_LL"]]=sum(banks[args$t,"rep_LLb",])
  return(Firms)
}


divf<-function(Firms=stop("Need to have firms defined!"),params=params,args=args){
  if(args$t>23){
    Firms[[args$t,"div_f"]]=max(0,Firms[[(args$t-1),"div_f"]]+(sum(Firms[(args$t-23):args$t,"Pr_f"])/length(Firms[(args$t-23):args$t,"Pr_f"])-sum(Firms[(args$t-23):args$t,"sav_ft"])/length(Firms[(args$t-23):args$t,"sav_ft"])-Firms[[(args$t-1),"div_f"]])/24)
  }
  else{
    Firms[[args$t,"div_f"]]=max(0,Firms[[(args$t-1),"div_f"]]+(sum(Firms[(1):args$t,"Pr_f"])/length(Firms[(1):args$t,"Pr_f"])-sum(Firms[(1):args$t,"sav_ft"])/length(Firms[(1):args$t,"sav_ft"])-Firms[[(args$t-1),"div_f"]])/24)
  }
  return(Firms)
}

#####Government#####

decidegovspend<-function(Households=stop("need to have Households defined!"),Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),Gov=stop("need to have gov defined!"),params=params,args=args){
  if(args$t/24==round(args$t/24)){
    gcons=1-(Households[(args$t-1),"c_d"]-Households[(args$t-11),"c_d"])/Households[(args$t-11),"c_d"]
    Gov[[args$t,"g_des"]]=params[[1,"g0"]]/48*gcons^params[[1,"mu1"]]
    Gov[[args$t,"gap"]]=Gov[[args$t,"g_des"]]-Gov[[(args$t-1),"g"]]
    Gov[[args$t,"gapr"]]=Gov[[args$t,"g_des"]]-Gov[[(args$t-1),"g"]]
  }
  else{
    Gov[[args$t,"g_des"]]=Gov[[(args$t-1),"g_des"]]
    Gov[[args$t,"gap"]]=Gov[[(args$t-1),"gap"]]
    Gov[[args$t,"gapr"]]=Gov[[(args$t-1),"gapr"]]-(Gov[[args$t,"gap"]])*1/12
  }
  if(Gov[[args$t,"gapr"]]>0 && Gov[[args$t,"gap"]]>0){
  Gov[[args$t,"g"]]=Gov[[(args$t-1),"g"]]+(Gov[[args$t,"gap"]])*1/12
  }else{
  if(Gov[[args$t,"gapr"]]<0 && Gov[[args$t,"gap"]]<0){
  Gov[[args$t,"g"]]=Gov[[(args$t-1),"g"]]+(Gov[[args$t,"gap"]])*1/12
  }else{
  Gov[[args$t,"g"]]=Gov[[(args$t-1),"g"]]
  }}
  return(Gov)
}

decidetax<-function(Gov=stop("need to have gov defined!"),Firms=stop("Need to have firms defined!"),Households=stop("need to have Households defined!"),Banks=stop("need to have Banks (agg) defined!"),params=params,args=args){
  if(args$t<49){
    Gov[[args$t,"Tax"]]=Gov[[(args$t-1),"Tax"]]
    Gov[[args$t,"tau"]]=Gov[[(args$t-1),"tau"]]
    Gov[[args$t,"FTax"]]=0
  }else{
    Gov[[args$t,"tau"]]=params[[1,"tau"]]
    Gov[[args$t,"Tax"]]=Gov[[args$t,"tau"]]*(sum(Households[(args$t-48):(args$t-1),"YD_tax"]))/48
    if(mean(Gov[(args$t-48):(args$t-1),"PSBR"])>0){
      Gov[[args$t,"FTax"]]=(mean(Gov[(args$t-48):(args$t-1),"PSBR"]))
    }else{
      Gov[[args$t,"FTax"]]=0
    }
  }
  return(Gov)
}

supplygb<-function(Gov=stop("need to have gov defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Gov[[args$t,"gb_s"]]=ifelse(Gov[[args$t,"PSBR"]]>=0,Gov[[args$t,"PSBR"]],0)
  Gov[[args$t,"rep_gb"]]=ifelse(Gov[[args$t,"PSBR"]]<0,abs(Gov[[args$t,"PSBR"]]),0)
  Gov[[args$t,"rep_gbcb"]]=ifelse(CB[[(args$t-1),"gb_cb"]]>Gov[[args$t,"rep_gb"]]*CB[[(args$t-1),"gb_cb"]]/Gov[[(args$t-1),"gb"]],Gov[[args$t,"rep_gb"]]*CB[[(args$t-1),"gb_cb"]]/Gov[[(args$t-1),"gb"]],ifelse(CB[[(args$t-1),"gb_cb"]]>0,CB[[(args$t-1),"gb_cb"]],0))
  Gov[[args$t,"rep_gbh"]]=Gov[[args$t,"rep_gb"]]-Gov[[args$t,"rep_gbcb"]]
  return(Gov)
}

gbstock<-function(Gov=stop("need to have gov defined!"),Households=stop("need to have Households defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Gov[[args$t,"gb"]]=CB[[(args$t-1),"gb_cb"]]+Households[[(args$t-1),"gb_h"]]+Gov[[args$t,"gb_s"]]-Gov[[args$t,"rep_gbcb"]]-Gov[[args$t,"rep_gbh"]]
  return(Gov)
}

gbint<-function(Gov=stop("need to have gov defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have Households defined"),params=params,args=args){
  Gov[[args$t,"iGB_h"]]=(Gov[[(args$t-1),"r_gb"]]*Households[[(args$t-1),"gb_h"]])/48
  Gov[[args$t,"iGB_cb"]]=(Gov[[(args$t-1),"r_gb"]]*CB[[(args$t-1),"gb_cb"]])/48
  Gov[[args$t,"iGB"]]=Gov[[args$t,"iGB_h"]]+Gov[[args$t,"iGB_cb"]]
  return(Gov)
}

setgbrate<-function(Gov=stop("need to have gov defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have Households defined"),Aux=stop("need to have auxiliary array defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Gov[[args$t,"r_gb"]]=(((Households[[(args$t-1),"gb_h"]]+Gov[[args$t,"gb_s"]]-Gov[[args$t,"rep_gbh"]])/Households[[(args$t-1),"TAA_h"]]-Aux[[(args$t-1),"porthgb"]])*12+Aux[[(args$t-1),"porthgb"]]-params[[1,"lambdah10"]]+params[[1,"lambdah12"]]*Banks[[args$t,"r_dav"]])/params[[1,"lambdah11"]]
  return(Gov)
}


#####Central Bank#####
infmeasures2<-function(Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    CB[[args$t,"pi_sa"]]=(1+(Firms[[args$t,"p"]]-Firms[[(args$t-1),"p"]])/Firms[[(args$t-1),"p"]])^24-1
  return(CB)
}

infmeasures<-function(Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    if(args$t<48){
    CB[[args$t,"pi_a"]]=((Firms[[args$t,"p"]]-sum(Firms[1:(args$t-1),"p"])/length(Firms[1:(args$t-1),"p"]))/(sum(Firms[1:(args$t-1),"p"])/length(Firms[1:(args$t-1),"p"])))
    }else{
    CB[[args$t,"pi_a"]]=(Firms[[args$t,"p"]]-Firms[[(args$t-47),"p"]])/Firms[[(args$t-47),"p"]]
  }
    if(args$t<12){
    CB[[args$t,"pi_q"]]=(1+(Firms[[args$t,"p"]]-sum(Firms[1:(args$t-1),"p"])/length(Firms[1:(args$t-1),"p"]))/(sum(Firms[1:(args$t-1),"p"])/length(Firms[1:(args$t-1),"p"])))^4-1  
    }else{
    CB[[args$t,"pi_q"]]=(1+(Firms[[args$t,"p"]]-Firms[[(args$t-11),"p"]])/Firms[[(args$t-11),"p"]])^4-1
  }
    if(args$t<4){
    CB[[args$t,"pi_m"]]=(1+(Firms[[args$t,"p"]]-sum(Firms[1:(args$t-1),"p"])/length(Firms[1:(args$t-1),"p"]))/(sum(Firms[1:(args$t-1),"p"])/length(Firms[1:(args$t-1),"p"])))^12-1
    }
    else{
    CB[[args$t,"pi_m"]]=(1+(Firms[[args$t,"p"]]-Firms[[(args$t-3),"p"]])/Firms[[(args$t-3),"p"]])^12-1
  }
  return(CB)
}


setcbrate<-function(CB=stop("need to have CB defined!"),Households=stop("need to have households defined!"),Firms=stop("need to have firms defined!"),params=params,args=args){
  if(args$t/4==round(args$t/4)){
   gi<-(1+(Firms[(args$t-1),"i_d"]-Firms[(args$t-3),"i_d"])/Firms[(args$t-3),"i_d"])^12-1
   CB[args$t,"r_cbd"]=max(0,params[[1,"r0"]]+CB[[args$t,"pi_e"]]+params[[1,"phiu"]]*(CB[[args$t,"u_ecb"]]-Firms[[args$t,"u_n"]])+params[[1,"phipi"]]*(CB[[args$t,"pi_e"]]-params[[1,"pit"]]))
   #CB[args$t,"r_cbd"]=max(0,params[[1,"r0"]]+CB[[args$t,"pi_e"]]+params[[1,"phipi"]]*(CB[[args$t,"pi_e"]]-params[[1,"pit"]])+0.15*gi)
  }else{
    CB[args$t,"r_cbd"]=CB[(args$t-1),"r_cbd"]
  }
  CB[args$t,"r_cbl"]=CB[args$t,"r_cbd"]+params[[1,"r1"]]
  return(CB)
}

RAstock<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
  CB[[args$t,"A"]]=sum(banks[args$t,"a_b",])
  CB[[args$t,"R"]]=sum(banks[args$t,"r_b",])
  return(CB)
}

monetisedef<-function(CB=stop("need to have CB defined!"),Gov=stop("need to have gov defined!"),Households=stop("need to have Households defined!"),params=params,args=args){
  CB[[args$t,"gb_rcb"]]=max(0,min(Gov[[args$t,"PSBR"]],Gov[[args$t,"PSBR"]]-(Households[[args$t,"gb_d"]]-Households[[(args$t-1),"gb_h"]])))
  return(CB)
}

gbcbstock<-function(CB=stop("need to have CB defined!"),Gov=stop("need to have gov defined!"),params=params,args=args){
  CB[[args$t,"gb_cb"]]=CB[[(args$t-1),"gb_cb"]]+CB[[args$t,"gb_rcb"]]-CB[[args$t,"gb_scb"]]+CB[[args$t,"gb_dcb"]]-Gov[[args$t,"rep_gbcb"]]
  return(CB)
}

Rtarget<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),Aux=stop("Need to have auxiliary array defined!"),params=params,args=args){
  if(params[[1,"CBnoise"]]==1){
  Aux[args$t,"Rshock"]=params[[1,"AR_r"]]*Aux[(args$t-1),"Rshock"]+(1-params[[1,"AR_r"]])*rnorm(1,0,params[[1,"sdev_R"]])
  }else{
  Aux[args$t,"Rshock"]=Aux[(args$t-1),"Rshock"] 
  }
  CB[[args$t,"R_t"]]=sum(banks[args$t,"R_tb",])+Aux[args$t,"Rshock"]
  return(list(CB=CB,Aux=Aux))
}

intervenegb<-function(CB=stop("need to have CB defined!"),params=params,args=args){
  CB[[args$t,"gb_dcb"]]=max(0,CB[[args$t,"R_t"]]-CB[[args$t,"R_pd"]])
  CB[[args$t,"gb_scb"]]=max(0,CB[[args$t,"R_pd"]]-CB[[args$t,"R_t"]])
  return(CB)
}

#solve cycle
CBsolvecyc<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),Households=stop("need to have Households defined!"),Gov=Gov,params=params,args=args){
  CB[[args$t,"buff_h"]]=-(sum(banks[args$t,"R_paltb",])-CB[[(args$t-1),"buff_h"]])+CB[[args$t,"R_t"]]
  CB[[args$t,"R_pd"]]=Households[[args$t,"sav_h"]]+Households[[args$t,"M_sup"]]-Households[[args$t,"rep_m"]]+Gov[[args$t,"rep_gbh"]]-(Gov[[args$t,"gb_s"]]-CB[[args$t,"gb_rcb"]])+(sum(banks[args$t,"R_paltb",]))
  CB[[args$t,"cbint"]]=-(CB[[args$t,"R_pd"]]-CB[[args$t,"R_t"]])
  return(CB)
}


targetratios<-function(CB=stop("need to have CB defined!"),Firms=stop("need to have Firms defined!"),Households=stop("need to have Households defined!"),Aux=stop("need to have Aux defined!"),params=params,args=args){
  CB[[args$t,"CARt"]]=params[[1,"CARt0"]]
  return(CB)
}

cbinterest<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  CB[[args$t,"iA"]]=sum(banks[args$t,"ia_b",])
  CB[[args$t,"iR"]]=sum(banks[args$t,"ir_b",])
  return(CB)
}

###BANKS#####
bbstock<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"bb"]]=Banks[[(args$t-1),"bb"]]
  return(Banks)
}

llsupply<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"LL_sup"]]=sum(banks[args$t,"LL_supb",])
  return(Banks)
}

bankdivsbbint<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"Div_b"]]=sum(banks[args$t,"div_b",])
  return(Banks)
}	

aggregateCAR<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Banks[[args$t,"CAR"]]=sum(banks[args$t,"carshare",])
  return(Banks)
}

averagerates<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Banks[[args$t,"r_dav"]]=sum(banks[args$t,"rdshare",])
  Banks[[args$t,"r_LLav"]]=sum(banks[args$t,"rllshare",])
  Banks[[args$t,"r_mav"]]=sum(banks[args$t,"rmshare",])
  Banks[[args$t,"r_LLavr"]]=(1+Banks[[args$t,"r_LLav"]])/(1+CB[[args$t,"pi_a"]])-1
  Banks[[args$t,"r_mavr"]]=(1+Banks[[args$t,"r_mav"]])/(1+CB[[args$t,"pi_a"]])-1
  return(Banks)
}

#####Agent-based banks####
#collect & pay interest
bankinterest<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
    banks[args$t,"iD_fb",]=(banks[(args$t-1),"r_db",]*banks[(args$t-1),"D_fb",])/48
    banks[args$t,"iD_hb",]=(banks[(args$t-1),"r_db",]*banks[(args$t-1),"D_hb",])/48
    banks[args$t,"ia_b",]=(CB[[(args$t-1),"r_cbl"]]*banks[(args$t-1),"a_b",])/48
    banks[args$t,"ir_b",]=(CB[[(args$t-1),"r_cbd"]]*banks[(args$t-1),"r_b",])/48
    banks[args$t,"iIB_b",]=(Banks[[(args$t-1),"r_IB"]]*(banks[(args$t-1),"IBL_b",]-banks[(args$t-1),"IBA_b",]))/48
    banks[args$t,"iLL_b",]=(banks[(args$t-1),"r_LLb",]*(banks[(args$t-1),"LL_b",]-banks[args$t,"LL_npb",]))/48
    banks[args$t,"iM_b",]=(banks[(args$t-1),"r_Mb",]*(banks[(args$t-1),"M_b",]-banks[args$t,"M_npb",]))/48
    return(banks)
}

rationindicators<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have Firms defined!"),Households=stop("need to have Households defined!"),params=params,args=args){
  for(i in 1:args$nB){
  banks[args$t,"rationLL",i]<-(banks[(args$t-1),"LL_db",i]-params[1,"omega2"]/(params[1,"omega1"]+params[1,"omega2"])*banks[(args$t-1),"RWA_gap",i])/banks[(args$t-1),"LL_db",i]
  banks[args$t,"rationM",i]<-banks[(args$t-1),"M_supb",i]/banks[(args$t-1),"M_db",i]
  banks[args$t,"rationLL",i]=(2/(1+exp(params[1,"iota2"]*(-banks[args$t,"rationLL",i]+1))))
  banks[args$t,"rationM",i]=(2/(1+exp(params[1,"iota2"]*(-banks[args$t,"rationM",i]+1))))
  }
  ifelse(sum(banks[(args$t),"rationLL",])>0,banks[(args$t),"rationLL",]<-(banks[(args$t),"rationLL",]/(sum(banks[(args$t),"rationLL",])/args$nB)),banks[(args$t),"rationLL",]<-1)
  ifelse(sum(banks[(args$t),"rationM",])>0,banks[(args$t),"rationM",]<-(banks[(args$t),"rationM",]/(sum(banks[(args$t),"rationM",])/args$nB)),banks[(args$t),"rationM",]<-1)
  return(banks)
}

  

LLdistr<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have Firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args,Frand=Frand){
  banks[args$t,"rLLrel",]<-1+banks[args$t,"r_LLb",]
  banks[args$t,"rLLrel",]=1/(banks[args$t,"rLLrel",]/(sum(banks[args$t,"rLLrel",])/args$nB))
  ifelse(Firms[[(args$t-1),"LL_d"]]>0,banks[args$t,"LLlag",]<-(banks[(args$t-1),"LL_db",]/Firms[[(args$t-1),"LL_d"]]),banks[args$t,"LLlag",]<-1/args$nB)
  for(i in 1:args$nB){
  banks[args$t,"detshareLL",i]<-max(0.01,banks[args$t,"LLlag",i]*banks[args$t,"rLLrel",i]^params[[1,"iota1"]]*banks[(args$t),"rationLL",i]^params[[1,"iota2"]])
  }
  banks[args$t,"detshareLL",]<-banks[args$t,"detshareLL",]/sum(banks[args$t,"detshareLL",])
  if(params[[1,"randdis"]]==1){
    Frand<-params[[1,"AR_dis"]]*Frand+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    for(i in 1:args$nB){
    banks[args$t,"randshareLL",i]<-max(0.01,banks[args$t,"LLlag",i]*banks[args$t,"rLLrel",i]^params[[1,"iota1"]]*banks[(args$t),"rationLL",i]^params[[1,"iota2"]]*Frand[i])
    }
    banks[args$t,"randshareLL",]=banks[args$t,"randshareLL",]/sum(banks[args$t,"randshareLL",])
      banks[args$t,"LL_db",]=banks[args$t,"randshareLL",]*Firms[args$t,"LL_d"]
  }else{
      banks[args$t,"randshareLL",]=banks[args$t,"detshareLL",]
      banks[args$t,"LL_db",]=banks[args$t,"detshareLL",]*Firms[args$t,"LL_d"]
  }
  return(list(banks=banks,Frand=Frand))
}


Mdistr<-function(banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have Households defined!"),params=params,args=args,Hrandm=Hrandm){
  banks[args$t,"rMrel",]<-1+banks[args$t,"r_Mb",]
  banks[args$t,"rMrel",]=1/(banks[args$t,"rMrel",]/(sum(banks[args$t,"rMrel",])/args$nB))
  ifelse(Households[[(args$t-1),"M_d"]]>0,banks[args$t,"Mlag",]<-(banks[(args$t-1),"M_db",]/Households[[(args$t-1),"M_d"]]),banks[args$t,"Mlag",]<-1/args$nB)
  for(i in 1:args$nB){
  banks[args$t,"detshareM",i]<-max(0.01,banks[args$t,"Mlag",i]*banks[args$t,"rMrel",i]^params[[1,"iota1"]]*banks[(args$t),"rationM",i]^params[[1,"iota2"]])
  }
  banks[args$t,"detshareM",]<-banks[args$t,"detshareM",]/sum(banks[args$t,"detshareM",])
  if(params[[1,"randdis"]]==1){
    Hrandm<-params[[1,"AR_dis"]]*Hrandm+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    for(i in 1:args$nB){
    banks[args$t,"randshareM",i]<-max(0.01,banks[args$t,"Mlag",i]*banks[args$t,"rMrel",i]^params[[1,"iota1"]]*banks[(args$t),"rationM",i]^params[[1,"iota2"]]*Hrandm[i])
    }
    banks[args$t,"randshareM",]<-banks[args$t,"randshareM",]/sum(banks[args$t,"randshareM",])
      banks[args$t,"M_db",]=banks[args$t,"randshareM",]*Households[args$t,"M_d"]
  }else{
      banks[args$t,"randshareM",]=banks[args$t,"detshareM",]
      banks[args$t,"M_db",]=banks[args$t,"detshareM",]*Households[args$t,"M_d"]
  }
  return(list(banks=banks,Hrandm=Hrandm))
}


Dhdistr<-function(banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have Households defined!"),params=params,args=args,Hrandd=Hrandd){
  ifelse(Households[[(args$t-1),"D_h"]]>0,banks[args$t,"Dhlag",]<-(banks[(args$t-1),"D_hb",]/Households[[(args$t-1),"D_h"]]),banks[args$t,"Dhlag",]<-1/args$nB)
  banks[args$t,"detshareDh",]<-banks[args$t,"Dhlag",]*banks[args$t,"rDrel",]^params[[1,"iota1"]]
  banks[args$t,"detshareDh",]<-banks[args$t,"detshareDh",]/sum(banks[args$t,"detshareDh",])
  if(params[[1,"randdis"]]==1){
    Hrandd<-params[[1,"AR_dis"]]*Hrandd+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    banks[args$t,"randshareDh",]<-banks[args$t,"Dhlag",]*banks[args$t,"rDrel",]^params[[1,"iota1"]]*Hrandd
    banks[args$t,"randshareDh",]=banks[args$t,"randshareDh",]/sum(banks[args$t,"randshareDh",])
      banks[args$t,"D_hb",]=banks[args$t,"randshareDh",]*Households[args$t,"D_h"]
  }else{
      banks[args$t,"randshareDh",]=banks[args$t,"detshareDh",]
      banks[args$t,"D_hb",]=banks[args$t,"detshareDh",]*Households[args$t,"D_h"]
  }
  return(list(banks=banks,Hrandd=Hrandd))
}

Dfdistr<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args,Frandd=Frandd){
  banks[args$t,"rDrel",]<-1+banks[args$t,"r_db",]
  banks[args$t,"rDrel",]=banks[args$t,"rDrel",]/(sum(banks[args$t,"rDrel",])/args$nB)
  ifelse(Firms[[(args$t-1),"D_f"]]>0,banks[args$t,"Dflag",]<-(banks[(args$t-1),"D_fb",]/Firms[[(args$t-1),"D_f"]]),banks[args$t,"Dflag",]<-1/args$nB)
  banks[args$t,"detshareDf",]<-banks[args$t,"Dflag",]*banks[args$t,"rDrel",]^params[[1,"iota1"]]
  banks[args$t,"detshareDf",]<-banks[args$t,"detshareDf",]/sum(banks[args$t,"detshareDf",])
  if(params[[1,"randdis"]]==1){
    Frandd<-params[[1,"AR_dis"]]*Frandd+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    banks[args$t,"randshareDf",]<-banks[args$t,"Dflag",]*banks[args$t,"rDrel",]^params[[1,"iota1"]]*Frandd
    banks[args$t,"randshareDf",]=banks[args$t,"randshareDf",]/sum(banks[args$t,"randshareDf",])
      banks[args$t,"D_fb",]=banks[args$t,"randshareDf",]*Firms[args$t,"D_f"]
  }else{
      banks[args$t,"randshareDf",]=banks[args$t,"detshareDf",]
      banks[args$t,"D_fb",]=banks[args$t,"detshareDf",]*Firms[args$t,"D_f"]
  }
  return(list(banks=banks,Frandd=Frandd))
}

bankestimates1<-function(banks=stop("need to have banks (ABM) defined!"),Est=stop("need to have est. array defined!"),Firms=stop("Need to have firms defined!"),Households=stop("Need to have households defined!"),params=params,args=args,drawBank=drawBank,Timer=Timer){
  drawBank<-c(sample(1:args$nB,args$nB/4,replace=FALSE))
  if(Timer<144){
  banks[args$t,"default_M",]=banks[(args$t),"defrateM",]
  banks[args$t,"default_LL",]=banks[(args$t),"defrateLL",]
  }else{
  if(params[1,"est1"]==1){
  for(i in 1:args$nB){
  defrateM<-ts(Est[(Timer-144):(Timer),"defrateM",i])
  defMest<-dynlm(defrateM ~ L(defrateM,1))
  banks[args$t,"default_M",i] = defMest$coefficients[1]+defMest$coefficients[2]*banks[(args$t-1),"defrateM",i]
  defrateLL<-ts(Est[(Timer-144):(Timer),"defrateLL",i])
  defLLest<-dynlm(defrateLL ~ L(defrateLL,1))
  banks[args$t,"default_LL",i] = defLLest$coefficients[1]+defLLest$coefficients[2]*banks[(args$t-1),"defrateLL",i]
  }}else{
  banks[args$t,"default_M",] = banks[(args$t),"defrateM",]
  banks[args$t,"default_LL",] = banks[(args$t),"defrateLL",]
  }}
  return(list(banks=banks,drawBank=drawBank))
}


bankestimates2<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Est=stop("need to have est. array defined!"),Firms=stop("Need to have firms defined!"),Households=stop("Need to have households defined!"),params=params,args=args,drawBank=drawBank,Timer=Timer){
  if(Timer<144){
  if(args$t<12){
  banks[args$t,"markup_M",]=banks[(args$t-1),"markup_M",]
  banks[args$t,"markup_LL",]=banks[(args$t-1),"markup_LL",]
  }else{
  for(i in 1:args$nB){
  if(is.element(i,drawBank)){
  ifelse(sum(banks[(args$t-4):(args$t-1),"iM_b",i])-sum(banks[(args$t-8):(args$t-5),"iM_b",i])>0 && banks[(args$t-1),"r_Mb",i]/(mean(banks[(args$t-1),"r_Mb",])) <= 1,banks[[args$t,"markup_M",i]] <- max(1,banks[[(args$t-1),"markup_M",i]]+max(0,rnorm(1,params[1,"M_step"],params[1,"M_step"]/4))),ifelse(sum(banks[(args$t-4):(args$t-1),"iM_b",i])-sum(banks[(args$t-8):(args$t-5),"iM_b",i])<0 && banks[(args$t-1),"r_Mb",i]/(mean(banks[(args$t-1),"r_Mb",])) >= 1,banks[[args$t,"markup_M",i]] <- max(1,banks[[(args$t-1),"markup_M",i]]-max(0,rnorm(1,params[1,"M_step"],params[1,"M_step"]/4))),banks[[args$t,"markup_M",i]] <- banks[[(args$t-1),"markup_M",i]]))
  ifelse(sum(banks[(args$t-4):(args$t-1),"iLL_b",i])-sum(banks[(args$t-8):(args$t-5),"iLL_b",i])>0 && banks[(args$t-1),"r_LLb",i]/(mean(banks[(args$t-1),"r_LLb",])) <= 1,banks[[args$t,"markup_LL",i]] <- max(1,banks[[(args$t-1),"markup_LL",i]]+max(0,rnorm(1,params[1,"LL_step"],params[1,"LL_step"]/4))),ifelse(sum(banks[(args$t-4):(args$t-1),"iLL_b",i])-sum(banks[(args$t-8):(args$t-5),"iLL_b",i])<0 && banks[(args$t-1),"r_LLb",i]/(mean(banks[(args$t-1),"r_LLb",])) >= 1,banks[[args$t,"markup_LL",i]] <- max(1,banks[[(args$t-1),"markup_LL",i]]-max(0,rnorm(1,params[1,"LL_step"],params[1,"LL_step"]/4))),banks[[args$t,"markup_LL",i]] <- banks[[(args$t-1),"markup_LL",i]]))
  }else{
  banks[args$t,"markup_M",i]=banks[(args$t-1),"markup_M",i]
  banks[args$t,"markup_LL",i]=banks[(args$t-1),"markup_LL",i]
  }}}}
  else{
  if(params[1,"est1"]==1){
  for(i in 1:args$nB){
    if(is.element(i,drawBank)){
    iLL<-ts(Est[(Timer-144):(Timer-1),"iLL_b",i])
    iM<-ts(Est[(Timer-144):(Timer-1),"iM_b",i])
    LLdef<-ts((Est[(Timer-144):(Timer-1),"defrateLL",i])*(Est[(Timer-144):(Timer-1),"LL_b",i]))
    Mdef<-ts((Est[(Timer-144):(Timer-1),"defrateM",i])*(Est[(Timer-144):(Timer-1),"M_b",i]))
    LLrate<-ts(Est[(Timer-144):(Timer-1),"r_LLb",i])
    Mrate<-ts(Est[(Timer-144):(Timer-1),"r_Mb",i])
    Mstep<-max(0,rnorm(1,params[1,"M_step"],params[1,"M_step"]/4))
    LLstep<-max(0,rnorm(1,params[1,"LL_step"],params[1,"LL_step"]/4))
    LLnewu<-max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_LL",i]])*(banks[[(args$t-1),"markup_LL",i]]+LLstep))
    Mnewu<-max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_M",i]])*(banks[[(args$t-1),"markup_M",i]]+Mstep))
    LLnewl<-max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_LL",i]])*(banks[[(args$t-1),"markup_LL",i]]-LLstep))
    Mnewl<-max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_M",i]])*(banks[[(args$t-1),"markup_M",i]]-Mstep))
    LLnewnc<-max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_LL",i]])*(banks[[(args$t-1),"markup_LL",i]]))
    Mnewnc<-max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_M",i]])*(banks[[(args$t-1),"markup_M",i]]))
    iLLest<-dynlm(iLL ~ L(iLL) + L(LLrate)+L(LLrate,2))
    iLLestu<-iLLest$coefficients[1]+iLLest$coefficients[2]*Est[[(Timer-1),"iLL_b",i]]+(iLLest$coefficients[3]+iLLest$coefficients[4])*LLnewu
    iLLestl<-iLLest$coefficients[1]+iLLest$coefficients[2]*Est[[(Timer-1),"iLL_b",i]]+(iLLest$coefficients[3]+iLLest$coefficients[4])*LLnewl
    iLLestnc<-iLLest$coefficients[1]+iLLest$coefficients[2]*Est[[(Timer-1),"iLL_b",i]]+(iLLest$coefficients[3]+iLLest$coefficients[4])*LLnewnc
    iMest<-dynlm(iM ~ L(iM) + L(Mrate)+L(Mrate,2))
    iMestu<-iMest$coefficients[1]+iMest$coefficients[2]*Est[[(Timer-1),"iM_b",i]]+(iMest$coefficients[3]+iMest$coefficients[4])*Mnewu
    iMestl<-iMest$coefficients[1]+iMest$coefficients[2]*Est[[(Timer-1),"iM_b",i]]+(iMest$coefficients[3]+iMest$coefficients[4])*Mnewl
    iMestnc<-iMest$coefficients[1]+iMest$coefficients[2]*Est[[(Timer-1),"iM_b",i]]+(iMest$coefficients[3]+iMest$coefficients[4])*Mnewnc
    ifelse(iLLestu>iLLestl && LLnewu/(mean(banks[(args$t-1),"r_LLb",])) <= 1,banks[[args$t,"markup_LL",i]] <- max(1,banks[[(args$t-1),"markup_LL",i]]+LLstep),ifelse(iLLestl>iLLestu && LLnewl/(mean(banks[(args$t-1),"r_LLb",])) >= 1, banks[[args$t,"markup_LL",i]] <- max(1,banks[[(args$t-1),"markup_LL",i]]-LLstep),banks[[args$t,"markup_LL",i]]<-banks[[(args$t-1),"markup_LL",i]]))
    ifelse(iMestu>iMestl && Mnewu/(mean(banks[(args$t-1),"r_Mb",])) <= 1,banks[[args$t,"markup_M",i]] <- max(1,banks[[(args$t-1),"markup_M",i]]+Mstep),ifelse(iMestl>iMestu && Mnewl/(mean(banks[(args$t-1),"r_Mb",])) >= 1, banks[[args$t,"markup_M",i]] <- max(1,banks[[(args$t-1),"markup_M",i]]-Mstep),banks[[args$t,"markup_M",i]]<-banks[[(args$t-1),"markup_M",i]]))
    }else{
    banks[args$t,"markup_M",i]=banks[(args$t-1),"markup_M",i]
    banks[args$t,"markup_LL",i]=banks[(args$t-1),"markup_LL",i]
    }
  }}else{
  for(i in 1:args$nB){
  if(is.element(i,drawBank)){
  ifelse(sum(banks[(args$t-4):(args$t-1),"iM_b",i])-sum(banks[(args$t-8):(args$t-5),"iM_b",i])>0 && banks[(args$t-1),"r_Mb",i]/(mean(banks[(args$t-1),"r_Mb",])) <= 1,banks[[args$t,"markup_M",i]] <- max(1,banks[[(args$t-1),"markup_M",i]]+max(0,rnorm(1,params[1,"M_step"],params[1,"M_step"]/4))),ifelse(sum(banks[(args$t-4):(args$t-1),"iM_b",i])-sum(banks[(args$t-8):(args$t-5),"iM_b",i])<0 && banks[(args$t-1),"r_Mb",i]/(mean(banks[(args$t-1),"r_Mb",])) >= 1,banks[[args$t,"markup_M",i]] <- max(1,banks[[(args$t-1),"markup_M",i]]-max(0,rnorm(1,params[1,"M_step"],params[1,"M_step"]/4))),banks[[args$t,"markup_M",i]] <- banks[[(args$t-1),"markup_M",i]]))
  ifelse(sum(banks[(args$t-4):(args$t-1),"iLL_b",i])-sum(banks[(args$t-8):(args$t-5),"iLL_b",i])>0 && banks[(args$t-1),"r_LLb",i]/(mean(banks[(args$t-1),"r_LLb",])) <= 1,banks[[args$t,"markup_LL",i]] <- max(1,banks[[(args$t-1),"markup_LL",i]]+max(0,rnorm(1,params[1,"LL_step"],params[1,"LL_step"]/4))),ifelse(sum(banks[(args$t-4):(args$t-1),"iLL_b",i])-sum(banks[(args$t-8):(args$t-5),"iLL_b",i])<0 && banks[(args$t-1),"r_LLb",i]/(mean(banks[(args$t-1),"r_LLb",])) >= 1,banks[[args$t,"markup_LL",i]] <- max(1,banks[[(args$t-1),"markup_LL",i]]-max(0,rnorm(1,params[1,"LL_step"],params[1,"LL_step"]/4))),banks[[args$t,"markup_LL",i]] <- banks[[(args$t-1),"markup_LL",i]]))
  }else{
  banks[args$t,"markup_M",i]=banks[(args$t-1),"markup_M",i]
  banks[args$t,"markup_LL",i]=banks[(args$t-1),"markup_LL",i]
  }}}}
  return(banks)
}


#set interest rates
setbankrates<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Banks=stop("need to have banks (aggregate) defined!"),Firms=stop("Need to have firms defined!"),Households=stop("Need to have households defined!"),params=params,args=args,drawBank=drawBank){
  if(args$t<13){
  Banks[args$t,"r_IBav"]=sum(Banks[1:(args$t-1),"r_IB"])/(args$t-1)
  for(i in 1:args$nB){
  banks[[args$t,"clear",i]]=1+tanh(params[1,"epsilond2"]*sum(banks[1:(args$t-1),"clear_b",i])/(args$t-1))
  }
  }else{
  Banks[args$t,"r_IBav"]=sum(Banks[(args$t-12):(args$t-1),"r_IB"])/12
  for(i in 1:args$nB){
  banks[[args$t,"clear",i]]=1+tanh(params[1,"epsilond2"]*sum(banks[(args$t-12):(args$t-1),"clear_b",i])/12)
  }
  }
  if(args$t<12){
    banks[args$t,"r_db",]=banks[(args$t-1),"r_db",]
    banks[args$t,"r_LLb",]=banks[(args$t-1),"r_LLb",]
    banks[args$t,"r_Mb",]=banks[(args$t-1),"r_Mb",]
  }else{
  for(i in 1:args$nB){
    if(is.element(i,drawBank)){
      banks[[args$t,"r_db",i]]=max(0,min(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+params[1,"epsilond1"]*banks[[args$t,"clear",i]],mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2))
      banks[[args$t,"r_LLb",i]]=max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_LL",i]])*banks[[args$t,"markup_LL",i]])
      banks[[args$t,"r_Mb",i]]=max(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2,(mean(CB[(args$t-12):(args$t),"r_cbl"]+CB[(args$t-12):(args$t),"r_cbd"])/2+banks[[args$t,"default_M",i]])*banks[[args$t,"markup_M",i]])
    }else{
      banks[[args$t,"r_db",i]]=banks[[(args$t-1),"r_db",i]]
      banks[[args$t,"r_LLb",i]]=banks[[(args$t-1),"r_LLb",i]]
      banks[[args$t,"r_Mb",i]]=banks[[(args$t-1),"r_Mb",i]]
    }
  }}
  return(list(banks=banks,Banks=Banks))
}


#decide rationing
rationcredit<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    for(i in 1:args$nB){
    banks[args$t,"RWA_gap",i]=((params[1,"omega1"]*(banks[(args$t-1),"M_b",i]+banks[args$t,"M_db",i]-banks[args$t,"rep_mb",i]-banks[args$t,"M_npb",i])+params[1,"omega2"]*(banks[(args$t-1),"LL_b",i]+banks[args$t,"LL_db",i]-banks[args$t,"rep_LLb",i]-banks[args$t,"LL_npb",i]))-banks[(args$t),"v_bbe",i]/CB[[args$t,"CARt"]])/48
    if(params[[1,"randdis"]]==1){
    banks[args$t,"LL_supb",i]=banks[args$t,"randshareLL",i]*Firms[[args$t,"replacegap"]]+max(0,min(banks[args$t,"LL_db",i]-banks[args$t,"randshareLL",i]*Firms[[args$t,"replacegap"]],banks[args$t,"LL_db",i]-banks[args$t,"randshareLL",i]*Firms[[args$t,"replacegap"]]-params[1,"omega2"]/(params[1,"omega1"]+params[1,"omega2"])*banks[args$t,"RWA_gap",i]))
    }else{
    banks[args$t,"LL_supb",i]=banks[args$t,"detshareLL",i]*Firms[[args$t,"replacegap"]]+max(0,min(banks[args$t,"LL_db",i]-banks[args$t,"detshareLL",i]*Firms[[args$t,"replacegap"]],banks[args$t,"LL_db",i]-banks[args$t,"detshareLL",i]*Firms[[args$t,"replacegap"]]-params[1,"omega2"]/(params[1,"omega1"]+params[1,"omega2"])*banks[args$t,"RWA_gap",i]))
    }
    banks[args$t,"M_supb",i]=max(0,min(banks[args$t,"M_db",i],banks[args$t,"M_db",i]-params[1,"omega1"]/(params[1,"omega1"]+params[1,"omega2"])*banks[args$t,"RWA_gap",i]))
    }
    return(banks)
}

defindicators<-function(Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args){
    if(args$t<48){
    Firms[[args$t,"meanlev"]]=sum(Firms[1:(args$t-1),"lev_f"])/length(Firms[1:(args$t-1),"lev_f"])
    Households[[args$t,"MH"]]=(sum(Households[1:(args$t-1),"M"])/length(Households[1:(args$t-1),"M"]))/((sum(Households[1:(args$t-1),"p_h"])/length(Households[1:(args$t-1),"p_h"]))*params[[1,"h"]])
  }else{
    Firms[[args$t,"meanlev"]]=sum(Firms[(args$t-47):(args$t-1),"lev_f"])/length(Firms[(args$t-47):(args$t-1),"lev_f"])
    Households[[args$t,"MH"]]=(sum(Households[(args$t-47):(args$t-1),"M"])/length(Households[(args$t-47):(args$t-1),"M"]))/((sum(Households[(args$t-47):(args$t-1),"p_h"])/length(Households[(args$t-47):(args$t-1),"p_h"]))*params[[1,"h"]])
  }
  return(list(Firms=Firms,Households=Households))
}

#defaults
defaults<-function(banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),Aux=stop("need to have auxiliary array defined!"),params=params,args=args,pvarsh=pvarsh,pvarsf=pvarsf,Timer=Timer){
      if(params[1,"randdef"]==1){
      Aux[[args$t,"sdev_deff"]]=params[[1,"sdev_def"]]*params[1,"omega1"]/params[1,"omega2"]
      Aux[[args$t,"sdev_defh"]]=params[[1,"sdev_def"]]
      }else{
      Aux[[args$t,"sdev_deff"]]=0
      Aux[[args$t,"sdev_defh"]]=0
      }
      for(i in 1:args$nB){
      logvarsh <- qlogis(pvarsh[Timer,i],(banks[(args$t-1),"r_Mb",i]/(mean(banks[(args$t-1),"r_Mb",]))),Aux[[args$t,"sdev_defh"]])
      logvarsf <- qlogis(pvarsf[Timer,i],(banks[(args$t-1),"r_LLb",i]/(mean(banks[(args$t-1),"r_LLb",]))),Aux[[args$t,"sdev_deff"]])
      banks[args$t,"defrandf",i]=params[1,"AR_def"]*banks[(args$t-1),"defrandf",i]+(1-params[1,"AR_def"])*logvarsf
      banks[args$t,"defrandh",i]=params[1,"AR_def"]*banks[(args$t-1),"defrandh",i]+(1-params[1,"AR_def"])*logvarsh
      }
      banks[args$t,"defrateLL",]=params[[1,"zeta_LL"]]*Firms[[args$t,"meanlev"]]*banks[args$t,"defrandf",]
      banks[args$t,"defrateM",]=params[[1,"zeta_M"]]*Households[[args$t,"MH"]]*banks[args$t,"defrandh",]
      for(i in 1:args$nB){
      banks[[args$t,"LL_npb",i]]=max(0,banks[[(args$t-1),"LL_b",i]]/48*banks[[args$t,"defrateLL",i]])
      banks[[args$t,"M_npb",i]]=max(0,banks[[(args$t-1),"M_b",i]]/48*banks[[args$t,"defrateM",i]])
      }
  return(list(banks=banks,Aux=Aux))
}


#update loans & securitise
bankreps<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
    for(i in 1:args$nB){
    banks[args$t,"rep_mb",i]=params[[1,"chi_M"]]*banks[(args$t-1),"M_b",i]/48
    banks[args$t,"rep_LLb",i]=params[[1,"chi_LL"]]*banks[(args$t-1),"LL_b",i]/48
    }
  return(banks)
}


loanstocks<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    banks[args$t,"M_b",]=banks[(args$t-1),"M_b",]+banks[args$t,"M_supb",]-banks[args$t,"rep_mb",]-banks[args$t,"M_npb",]
    banks[args$t,"LL_b",]=banks[(args$t-1),"LL_b",]+banks[args$t,"LL_supb",]-banks[args$t,"rep_LLb",]-banks[args$t,"LL_npb",]
  return(banks)
}


#interbank part 1
IB1<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
  for(i in 1:args$nB){
    banks[[args$t,"R_tb",i]]=params[[1,"LCRt"]]*params[[1,"omega3"]]*(banks[[(args$t-1),"D_hb",i]]+banks[[(args$t-1),"D_fb",i]])
    banks[[args$t,"clearalt_b",i]]=(banks[[(args$t),"D_fb",i]]-banks[[(args$t-1),"D_fb",i]])+banks[[args$t,"rep_LLb",i]]+banks[[args$t,"rep_mb",i]]-banks[[args$t,"LL_supb",i]]-(banks[[args$t,"M_supb",i]])+banks[[args$t,"iLL_b",i]]+banks[[args$t,"iM_b",i]]-banks[[args$t,"iD_hb",i]]-banks[[args$t,"iD_fb",i]]-banks[[args$t,"div_b",i]]-banks[[args$t,"ia_b",i]]+banks[[args$t,"ir_b",i]]
    banks[[args$t,"R_paltb",i]]=banks[[args$t,"clearalt_b",i]]+banks[[(args$t-1),"r_b",i]]+banks[[(args$t-1),"IBL_b",i]]-banks[[(args$t-1),"IBA_b",i]]-banks[[(args$t-1),"a_b",i]]+banks[[args$t,"iIB_b",i]]
  }
  return(banks)
}

#interbank part 2
IB2<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
  for(i in 1:args$nB){
    banks[[args$t,"clear_b",i]]=(banks[[args$t,"D_hb",i]]+banks[[(args$t),"D_fb",i]]-banks[[(args$t-1),"D_hb",i]]-banks[[(args$t-1),"D_fb",i]])+banks[[args$t,"rep_LLb",i]]+banks[[args$t,"rep_mb",i]]-banks[[args$t,"LL_supb",i]]-(banks[[args$t,"M_supb",i]])+banks[[args$t,"iLL_b",i]]+banks[[args$t,"iM_b",i]]-banks[[args$t,"iD_hb",i]]-banks[[args$t,"iD_fb",i]]-banks[[args$t,"div_b",i]]-banks[[args$t,"ia_b",i]]+banks[[args$t,"ir_b",i]]
    banks[[args$t,"R_pb",i]]=banks[[args$t,"clear_b",i]]+banks[[(args$t-1),"r_b",i]]+banks[[(args$t-1),"IBL_b",i]]-banks[[(args$t-1),"IBA_b",i]]-banks[[(args$t-1),"a_b",i]]+banks[[args$t,"iIB_b",i]]
    banks[[args$t,"R_anb",i]]=banks[[args$t,"R_pb",i]]-banks[[args$t,"R_tb",i]]
    banks[[args$t,"IBL_sb",i]]=ifelse(banks[[args$t,"R_anb",i]]>0,banks[[args$t,"R_anb",i]],0)
    banks[[args$t,"IBA_db",i]]=ifelse(banks[[args$t,"R_anb",i]]<0,abs(banks[[args$t,"R_anb",i]]),0)
  }
  return(banks)
}

#interbank part 3--> interaction
IB3<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[args$t,"IBS"]]=sum(banks[args$t,"IBL_sb",])
  Banks[[args$t,"IBD"]]=sum(banks[args$t,"IBA_db",])
  for(i in 1:args$nB){
    banks[[args$t,"IBSshare",i]]=ifelse(Banks[[args$t,"IBS"]]>0,banks[[args$t,"IBL_sb",i]]/Banks[[args$t,"IBS"]],1/args$nB)
    banks[[args$t,"IBDshare",i]]=ifelse(Banks[[args$t,"IBD"]]>0,banks[[args$t,"IBA_db",i]]/Banks[[args$t,"IBD"]],1/args$nB)
  }
    banks[args$t,"IBA_b",]=pmin(banks[args$t,"IBA_db",],banks[args$t,"IBDshare",]*Banks[[args$t,"IBS"]])
    banks[args$t,"IBL_b",]=pmin(banks[args$t,"IBL_sb",],banks[args$t,"IBSshare",]*Banks[[args$t,"IBD"]])
    banks[args$t,"R_lb",]=banks[args$t,"R_pb",]+banks[args$t,"IBA_b",]-banks[args$t,"IBL_b",]
    Banks[[args$t,"r_IB"]]=CB[[args$t,"r_cbd"]]+(CB[[args$t,"r_cbl"]]-CB[[args$t,"r_cbd"]])/(1+exp(-params[[1,"sigmaIB"]]*sum(banks[args$t,"R_paltb",]-banks[args$t,"R_tb",])))
  return(list(Banks=Banks, banks=banks))
}

#bankbonds
bbonddivs1<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have Banks (agg) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    banks[args$t,"v_bbt",]=CB[[args$t,"CARt"]]*(params[[1,"omega1"]]*banks[args$t,"M_b",]+params[[1,"omega2"]]*banks[args$t,"LL_b",])
    if(args$t<12){
    banks[args$t,"div_bt",]=0
    }else{
    if(args$t/12==round(args$t/12)){
    for(i in 1:args$nB){
    banks[args$t,"div_bt",i]=mean(banks[(args$t-11):args$t,"v_bbe",i]-banks[(args$t-11):args$t,"v_bbt",i])
    }}else{
    banks[args$t,"div_bt",]=banks[(args$t-1),"div_bt",]
    }}
    banks[args$t,"div_b",]=pmax(0,banks[args$t,"pr_b",]+((banks[args$t,"div_bt",])/12))
    banks[args$t,"bb_b",]=banks[(args$t-1),"bb_b",]
  return(list(banks=banks,Banks=Banks))
}

#regulatory ratios####
regratios<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
    banks[args$t,"CARb",]=pmax(0,banks[args$t,"v_bb",]/(params[[1,"omega1"]]*banks[args$t,"M_b",]+params[[1,"omega2"]]*banks[args$t,"LL_b",]))
    banks[args$t,"LCRb",]=banks[args$t,"r_b",]/(params[[1,"omega3"]]*(banks[args$t,"D_hb",]+banks[args$t,"D_fb",]))
    banks[args$t,"carshare",]=banks[args$t,"CARb",]/sum(banks[args$t,"CARb",])*banks[args$t,"CARb",]
  return(banks)
}

rshares<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
    banks[args$t,"rdshare",]=banks[(args$t-1),"r_db",]*banks[(args$t-1),"D_hb",]/sum(banks[(args$t-1),"D_hb",])
    banks[args$t,"rllshare",]=banks[(args$t-1),"r_LLb",]*banks[(args$t-1),"LL_b",]/sum(banks[(args$t-1),"LL_b",])
    banks[args$t,"rmshare",]=banks[(args$t-1),"r_Mb",]*banks[(args$t-1),"M_b",]/sum(banks[(args$t-1),"M_b",])
  return(banks)
}

######EXPECTATIONS etc.#####

bankexp<-function(banks=stop("need to have banks (ABM) defined!"),Est=stop("need to have est. array defined!"),CB=stop("need to have CB defined!"),params=params,args=args,Timer=Timer){
    if(params[[1,"Hommes"]]==1){
    if(args$t<3){
    banks[args$t,"v_bbe",]=banks[(args$t-1),"v_bbe",]+params[[1,"psi_ad"]]*(banks[(args$t-1),"v_bb",]-banks[(args$t-1),"v_bbe",])
    banks[args$t,"v_bbe1",]=banks[(args$t-1),"v_bbe1",]
    banks[args$t,"v_bbe2",]=banks[(args$t-1),"v_bbe2",]
    banks[args$t,"v_bbe3",]=banks[(args$t-1),"v_bbe3",]
    banks[args$t,"v_bbe4",]=banks[(args$t-1),"v_bbe4",]
    }else{
    for(i in 1:args$nB){
    banks[[args$t,"v_bbe1",i]]=banks[[(args$t-1),"v_bbe1",i]]+params[[1,"psi_ad"]]*(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-1),"v_bbe1",i]])
    banks[[args$t,"v_bbe2",i]]=banks[[(args$t-1),"v_bb",i]]+params[[1,"psi_tf1"]]*(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-2),"v_bb",i]])
    banks[[args$t,"v_bbe3",i]]=banks[[(args$t-1),"v_bb",i]]+params[[1,"psi_tf2"]]*(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-2),"v_bb",i]])
    banks[[args$t,"v_bbe4",i]]=params[[1,"psi_aa"]]*(sum(Est[1:(Timer-1),"v_bb",i])/length(Est[1:(Timer-1),"v_bb",i]))+(1-params[[1,"psi_aa"]])*(banks[[(args$t-1),"v_bb",i]])+(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-2),"v_bb",i]])
    #banks[[args$t,"v_bbe4",i]]=banks[(args$t-1),"v_bbt",i]
    banks[[args$t,"fitvbb1",i]]=exp(params[[1,"intensity"]]*(-(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-1),"v_bbe1",i]])^2-params[[1,"memory"]]*(banks[[(args$t-2),"v_bb",i]]-banks[[(args$t-2),"v_bbe1",i]])^2))
    banks[[args$t,"fitvbb2",i]]=exp(params[[1,"intensity"]]*(-(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-1),"v_bbe2",i]])^2-params[[1,"memory"]]*(banks[[(args$t-2),"v_bb",i]]-banks[[(args$t-2),"v_bbe2",i]])^2))
    banks[[args$t,"fitvbb3",i]]=exp(params[[1,"intensity"]]*(-(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-1),"v_bbe3",i]])^2-params[[1,"memory"]]*(banks[[(args$t-2),"v_bb",i]]-banks[[(args$t-2),"v_bbe3",i]])^2))
    banks[[args$t,"fitvbb4",i]]=exp(params[[1,"intensity"]]*(-(banks[[(args$t-1),"v_bb",i]]-banks[[(args$t-1),"v_bbe4",i]])^2-params[[1,"memory"]]*(banks[[(args$t-2),"v_bb",i]]-banks[[(args$t-2),"v_bbe4",i]])^2))
    ifelse(max(banks[[args$t,"fitvbb1",i]],banks[[args$t,"fitvbb2",i]],banks[[args$t,"fitvbb3",i]],banks[[args$t,"fitvbb4",i]])==banks[[args$t,"fitvbb1",i]],banks[[args$t,"maxfitvbb",i]]<-1,ifelse(max(banks[[args$t,"fitvbb1",i]],banks[[args$t,"fitvbb2",i]],banks[[args$t,"fitvbb3",i]],banks[[args$t,"fitvbb4",i]])==banks[[args$t,"fitvbb2",i]],banks[[args$t,"maxfitvbb",i]]<-2,ifelse(max(banks[[args$t,"fitvbb1",i]],banks[[args$t,"fitvbb2",i]],banks[[args$t,"fitvbb3",i]],banks[[args$t,"fitvbb4",i]])==banks[[args$t,"fitvbb3",i]],banks[[args$t,"maxfitvbb",i]]<-3,banks[[args$t,"maxfitvbb",i]]<-4)))
    ifelse(banks[[args$t,"maxfitvbb",i]]==1,banks[[args$t,"v_bbe",i]]<-banks[[args$t,"v_bbe1",i]],ifelse(banks[[args$t,"maxfitvbb",i]]==2,banks[[args$t,"v_bbe",i]]<-banks[[args$t,"v_bbe2",i]],ifelse(banks[[args$t,"maxfitvbb",i]]==3,banks[[args$t,"v_bbe",i]]<-banks[[args$t,"v_bbe3",i]],banks[[args$t,"v_bbe",i]]<-banks[[args$t,"v_bbe4",i]])))
    #banks[[args$t,"v_bbe",i]]=banks[[args$t,"v_bbe4",i]]
    }
    }}else{
    if(params[1,"est2"]==1 && args$t>48){
    for(i in 1:args$nB){
    vbb<-ts(Est[1:(Timer-1),"v_bb",i])
    vbbest<-dynlm(vbb ~ L(vbb,1))
    banks[args$t,"v_bbe",i]=vbbest$coefficients[1]+vbbest$coefficients[2]*banks[(args$t-1),"v_bb",i]
    }  
    }else{
    banks[args$t,"v_bbe",]=banks[(args$t-1),"v_bbe",]+params[[1,"psi_ad"]]*(banks[(args$t-1),"v_bb",]-banks[(args$t-1),"v_bbe",])
    }}
  return(banks)
}

#Households
Hexp<-function(Firms=stop("need to have Firms defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have Households defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  #monthly
  if(args$t<13){
    Households[[args$t,"YD_e"]]=Households[[(args$t-1),"YD_e"]]+params[[1,"psi_ad"]]*((sum(Households[1:(args$t-1),"YD"])/length(Households[1:(args$t-1),"YD"]))-Households[[(args$t-1),"YD_e"]])
    Households[[args$t,"yd_e"]]=Households[[(args$t-1),"YD_e"]]+params[[1,"psi_ad"]]*(mean(Households[1:(args$t-1),"YD"]/Firms[1:(args$t-1),"p"])-Households[[(args$t-1),"yd_e"]])
    Households[[args$t,"V_he"]]=Households[[(args$t-1),"V_he"]]+params[[1,"psi_ad"]]*((sum(Households[1:(args$t-1),"V_h"])/length(Households[1:(args$t-1),"V_h"]))-Households[[(args$t-1),"V_he"]])
    Households[[args$t,"v_he"]]=Households[[(args$t-1),"v_he"]]+params[[1,"psi_ad"]]*(mean(Households[1:(args$t-1),"V_h"]/Firms[1:(args$t-1),"p"])-Households[[(args$t-1),"v_he"]])
  }else{
    Households[[args$t,"YD_e"]]=Households[[(args$t-1),"YD_e"]]+params[[1,"psi_ad"]]*((sum(Households[(args$t-12):(args$t-1),"YD"])/length(Households[(args$t-12):(args$t-1),"YD"]))-Households[[(args$t-1),"YD_e"]])
    Households[[args$t,"V_he"]]=Households[[(args$t-1),"V_he"]]+params[[1,"psi_ad"]]*((sum(Households[(args$t-12):(args$t-1),"V_h"])/length(Households[(args$t-12):(args$t-1),"V_h"]))-Households[[(args$t-1),"V_he"]])
    Households[[args$t,"yd_e"]]=Households[[(args$t-1),"YD_e"]]+params[[1,"psi_ad"]]*(mean(Households[(args$t-12):(args$t-1),"YD"]/Firms[(args$t-12):(args$t-1),"p"])-Households[[(args$t-1),"yd_e"]])
    Households[[args$t,"v_he"]]=Households[[(args$t-1),"v_he"]]+params[[1,"psi_ad"]]*(mean(Households[(args$t-12):(args$t-1),"V_h"]/Firms[(args$t-12):(args$t-1),"p"])-Households[[(args$t-1),"v_he"]])
  }
    if(args$t<13){
    Households[[args$t,"r_mave"]]=Households[[(args$t-1),"r_mave"]]+params[[1,"psi_ad"]]*((sum(Banks[1:(args$t-1),"r_mavr"])/length(Banks[1:(args$t-1),"r_mavr"]))-Households[[(args$t-1),"r_mave"]])
    }else{
    Households[[args$t,"r_mave"]]=Households[[(args$t-1),"r_mave"]]+params[[1,"psi_ad"]]*((sum(Banks[(args$t-12):(args$t-1),"r_mavr"])/length(Banks[(args$t-12):(args$t-1),"r_mavr"]))-Households[[(args$t-1),"r_mave"]])
    }
  if(args$t<25){
    Households[[args$t,"rr_he"]]=Households[[(args$t-1),"rr_he"]]+params[[1,"psi_ad"]]*(sum(Households[1:(args$t-1),"rr_h"])/length(Households[1:(args$t-1),"rr_h"])-Households[[(args$t-1),"rr_he"]])
    Households[[args$t,"pi_eh"]]=Households[[(args$t-1),"pi_eh"]]+params[[1,"psi_ad"]]*(sum(CB[1:(args$t-1),"pi_sa"])/length(CB[1:(args$t-1),"pi_sa"])-Households[[(args$t-1),"pi_eh"]])
    }else{
    Households[[args$t,"pi_eh"]]=Households[[(args$t-1),"pi_eh"]]+params[[1,"psi_ad"]]*(sum(CB[(args$t-24):(args$t-1),"pi_sa"])/length(CB[(args$t-24):(args$t-1),"pi_sa"])-Households[[(args$t-1),"pi_eh"]])
    Households[[args$t,"rr_he"]]=Households[[(args$t-1),"rr_he"]]+params[[1,"psi_ad"]]*(sum(Households[(args$t-24):(args$t-1),"rr_h"])/length(Households[(args$t-24):(args$t-1),"rr_h"])-Households[[(args$t-1),"rr_he"]])
    }
  return(Households)
}

#Firms
Fexp<-function(Firms=stop("need to have Firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args,Timer=Timer){
    Firms[[args$t,"lev_fe"]]=Firms[[(args$t-1),"lev_fe"]]+params[[1,"psi_ad"]]*(Firms[(args$t-1),"lev_f"]-Firms[[(args$t-1),"lev_fe"]])
    Firms[args$t,"u_n"]=Firms[(args$t-1),"u_n"]
    if(args$t<13){
    Firms[[args$t,"u_e2"]]=Firms[[(args$t-1),"u_e2"]]+params[[1,"psi_ad"]]*(sum(Firms[1:(args$t-1),"u"])/length(Firms[1:(args$t-1),"u"])-Firms[[(args$t-1),"u_e2"]])
    Firms[[args$t,"r_LLave"]]=Firms[[(args$t-1),"r_LLave"]]+params[[1,"psi_ad"]]*(sum(Banks[1:(args$t-1),"r_LLavr"])/length(Banks[1:(args$t-1),"r_LLavr"])-Firms[[(args$t-1),"r_LLave"]])
    }else{
    Firms[[args$t,"u_e2"]]=Firms[[(args$t-1),"u_e2"]]+params[[1,"psi_ad"]]*(sum(Firms[(args$t-12):(args$t-1),"u"])/length(Firms[(args$t-12):(args$t-1),"u"])-Firms[[(args$t-1),"u_e2"]])
    Firms[[args$t,"r_LLave"]]=Firms[[(args$t-1),"r_LLave"]]+params[[1,"psi_ad"]]*(sum(Banks[(args$t-12):(args$t-1),"r_LLavr"])/length(Banks[(args$t-12):(args$t-1),"r_LLavr"])-Firms[[(args$t-1),"r_LLave"]])
    }
  if(args$t<25){
    Firms[[args$t,"u_e"]]=Firms[[(args$t-1),"u_e"]]+params[[1,"psi_ad"]]*(sum(Firms[1:(args$t-1),"u"])/length(Firms[1:(args$t-1),"u"])-Firms[[(args$t-1),"u_e"]])
  }else{
    Firms[[args$t,"u_e"]]=Firms[[(args$t-1),"u_e"]]+params[[1,"psi_ad"]]*(sum(Firms[(args$t-24):(args$t-1),"u"])/length(Firms[(args$t-24):(args$t-1),"u"])-Firms[[(args$t-1),"u_e"]])
  }
  Firms[[args$t,"delta_k"]]=params[[1,"delta_k"]]
  return(Firms)
}

#Central Bank
CBexp<-function(CB=stop("need to have CB defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args){
  if(args$t<5){
    CB[[args$t,"u_ecb"]]=CB[[(args$t-1),"u_ecb"]]+params[[1,"psi_ad"]]*(sum(Firms[1:(args$t-1),"u"])/length(Firms[1:(args$t-1),"u"])-CB[[(args$t-1),"u_ecb"]])
    CB[[args$t,"pi_e"]]=CB[[(args$t-1),"pi_e"]]
  }else{
    CB[[args$t,"u_ecb"]]=CB[[(args$t-1),"u_ecb"]]+params[[1,"psi_ad"]]*(sum(Firms[(args$t-4):(args$t-1),"u"])/length(Firms[(args$t-4):(args$t-1),"u"])-CB[[(args$t-1),"u_ecb"]])
    CB[[args$t,"pi_e"]]=CB[[(args$t-1),"pi_e"]]+params[[1,"psi_ad"]]*(CB[[args$t,"pi_m"]]-CB[[(args$t-1),"pi_e"]])
  }
  return(CB)
}

collectdata<-function(Est=stop("need to have est. array defined!"),banks=stop("need to have banks (ABM) defined!"),params=params,args=args,Timer=Timer){
  Est[Timer,"defrateM",]<-banks[args$t,"defrateM",]
  Est[Timer,"defrateLL",]<-banks[args$t,"defrateLL",]
  Est[Timer,"r_Mb",]<-banks[args$t,"r_Mb",]
  Est[Timer,"r_LLb",]<-banks[args$t,"r_LLb",]
  Est[Timer,"iLL_b",]<-banks[args$t,"iLL_b",]
  Est[Timer,"iM_b",]<-banks[args$t,"iM_b",]
  Est[Timer,"v_bb",]<-banks[args$t,"v_bb",]
  return(Est)
}
  
  
  
  
  
  
  