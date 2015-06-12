load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  steps=asciiread("steps.txt",(/1,2/),"integer")
  nvar = steps(0,0)
  numavg = steps(0,1)
  print(nvar)
  print(numavg)
  costgradient = asciiread("cost_gradient.txt",(/numavg,nvar/),"float")

  titles = new(4,string)
  titles(0)="Cost outer 1"
  titles(1)="Gradient outer 1"
  titles(2)="Cost outer 2"
  titles(3)="Gradient outer 2"

  plot = new(4,graphic)

;  xwks = gsn_open_wks("pdf","GSI_cost_gradient")
  xwks = gsn_open_wks("x11","gsun01n")
  gsn_define_colormap(xwks,"cosam12")

  resources                  =  True              ; plot mods desired
  resources@gsnDraw              = False              ; Do not draw plot
  resources@gsnFrame             = False              ; Do not advance frame

  resources@xyMarkLineModes   = "Markers"                ; choose which have markers
  resources@xyMarkerColor = "black"

  resources@xyMarkLineModes   = "Lines"                ; choose which have markers
  plot(0) = gsn_xy(xwks, costgradient(:,0), costgradient(:,1), resources) 
  plot(1) = gsn_xy(xwks, costgradient(:,0), costgradient(:,2), resources) 
  plot(2) = gsn_xy(xwks, costgradient(:,0), costgradient(:,3), resources) 
  plot(3) = gsn_xy(xwks, costgradient(:,0), costgradient(:,4), resources) 

  resourcesP                    =  True              ; plot mods desired
  resourcesP@gsnPanelFigureStrings = titles
  resourcesP@gsnPanelFigureStringsFontHeightF = 0.02 
  resourcesP@amJust = "TopRight"     
  resourcesP@gsnPanelFigureStringsPerimOn = False  
  gsn_panel(xwks,plot,(/2,2/),resourcesP)

  delete(plot)
  delete(resources)
  delete(resourcesP)
end
