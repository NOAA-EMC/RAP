load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  istart=0
;  istart=94
  titles3 = new(3,string)
  titles3(0)="cor temp"
  titles3(1)="cor Ps"
  titles3(2)="cor vp"
  titles = new(3,string)
  titles(0)="corz"
  titles(1)="hwll"
  titles(2)="vz"
  vars = new(4,string)
  vars(0)=" sf"
  vars(1)=" vp"
  vars(2)="  T"
  vars(3)="  q"

  msig = fbinrecread("./gsi_be.bin",0,1,"integer")
  mlat = fbinrecread("./gsi_be.bin",1,1,"integer")

  agvi = fbinrecread("./gsi_be.bin",2,(/msig,msig,mlat+2/),"float")
  bvi  = fbinrecread("./gsi_be.bin",3,(/msig,mlat+2/),"float")
  wgvi = fbinrecread("./gsi_be.bin",4,(/msig,mlat+2/),"float")

  plot = new(3,graphic)
  wks   = gsn_open_wks ("pdf","gsi_be")               ; open workstation

  res             = True                              ; plot mods desired
  res@gsnDraw     = False              ; Do not draw plot
  res@gsnFrame    = False                             ; don't advance yet
  res@cnLineLabelInterval        = 1             ; default = 2

  resourcesP                    =  True              ; plot mods desired
  resourcesP@gsnMaximize        =  True       ; plot mods desired
  resourcesP@txString           = "GSI BE"
  resourcesP@amJust = "TopLeft"
  resourcesP@gsnPanelFigureStringsFontHeightF = 0.005
  resourcesP@gsnPanelFigureStringsPerimOn = False

  plot(0) = gsn_csm_contour(wks,agvi(:,:,istart+37),res)
  plot(1) = gsn_csm_contour(wks,bvi(:,istart:mlat+1),res)
  plot(2) = gsn_csm_contour(wks,wgvi(:,istart:mlat+1),res)
  resourcesP@gsnPanelFigureStrings = titles3
  gsn_panel(wks,plot,(/3,1/),resourcesP)

  nnn=1
  do nn = 0, 3
    titles2=str_insert(titles, vars(nn), -1)

    nnn = 1 + nn * 3 + 4
    corz = fbinrecread("./gsi_be.bin",nnn,(/msig,mlat/),"float")
    nnn = 2 + nn * 3 + 4
    hwll = fbinrecread("./gsi_be.bin",nnn,(/msig,mlat+2/),"float")
    nnn = 3 + nn * 3 + 4
    vztdq= fbinrecread("./gsi_be.bin",nnn,(/msig,mlat+2/),"float")
    print("numm="+nnn)

    plot(0) = gsn_csm_contour(wks,corz(:,istart:mlat-1),res)
    plot(1) = gsn_csm_contour(wks,hwll(:,istart:mlat+1),res)
    plot(2) = gsn_csm_contour(wks,vztdq(:,istart:mlat+1),res)

    resourcesP@gsnPanelFigureStrings = titles2
    gsn_panel(wks,plot,(/3,1/),resourcesP)

  end do

end

