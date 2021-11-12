K=36;
J=10;
length=10;
step=0.001;

F=10;
c=10;
h=1;
b=10;

toStringX[kk_]:=ToString[If[kk<=0,K+kk,If[kk>K,kk-K,kk]]]
toStringY[kk_,jj_]:=If[jj==0,If[kk>1,ToString[kk-1]<>"a"<>ToString[J],ToString[K]<>"a"<>ToString[J]],
				            If[jj>J,If[kk<K,ToString[kk+1]<>"a"<>ToString[jj-J],ToString[kk-K+1]<>"a"<>ToString[jj-J]],
          					ToString[kk]<>"a"<>ToString[jj]]]
                  
 xequation=ToExpression[Table["x"<>ToString[kk]<>"'[t]==(x"<>toStringX[kk+1]<>"[t]-x"<>toStringX[kk-2]<>"[t])*x"<>toStringX[kk-1]<>"[t]-x"<>toStringX[kk]<>
      "[t]+F-h*c*("<>StringRiffle[Table["y"<>ToString[kk]<>"a"<>ToString[jj],{jj,J}],"[t]+"]<>"[t])/J",{kk,K}]];

yequation=ToExpression[Flatten[Table["y"<>toStringY[kk,jj]<>"'[t]==-b*c*y"<>toStringY[kk,jj+1]<>"[t](y"<>toStringY[kk,jj+2]<>"[t]-y"<>toStringY[kk,jj-1]<>
      "[t])-c*y"<>toStringY[kk,jj]<>"[t]+h*c/J*x"<>ToString[kk]<>"[t]",{kk,K},{jj,J}]]];

xinitial=ToExpression[Table["x"<>ToString[kk]<>"[0]==RandomReal[{F-.1,F+.1}]",{kk,K}]];
yinitial=ToExpression[Flatten[Table["y"<>ToString[kk]<>"a"<>ToString[jj]<>"[0]==RandomReal[{-.1,.1}]",{kk,K},{jj,J}]]];

xvar=ToExpression[Table["x"<>ToString[kk],{kk,K}]];
yvar=ToExpression[Flatten[Table["y"<>ToString[kk]<>"a"<>ToString[jj],{kk,K},{jj,J}]]];

result=NDSolve[Join[xequation,yequation,xinitial,yinitial],Join[xvar,yvar],{t,0,length}];

base=Table[Evaluate[Map[#[t]&,xvar]/.result],{t,0,length,step}][[;;,1]];
pertubation=Table[Evaluate[Map[#[t]&,yvar]/.result],{t,0,length,step}][[;;,1]];


plot1=Labeled[Show[Table[RadialAxisPlot[base[[i]],PlotRange->{-F*1.5,F*1.5},GridLines->Range[-F*1.5,F*1.5,5],Axes->Automatic,
	Filling->False,
	PlotLabel->"X",
	(*PlotStyle\[Rule]Blend[{Darker[Blue],Blue,White,Red,Darker[Red]},(i-1000)/1000],*)
	PlotStyle->ColorData["Rainbow"][(i-1000)/1000.],
	BaseStyle->{FontFamily->"Arial",12}],{i,1000,2000,50}],ImageSize->280],
	Labeled[Labeled[ImageCrop[Rasterize[BarLegend["Rainbow",Ticks->None,LegendLayout->"Row"]]],Style["t=0",{FontFamily->"Arial",20}],Left,Spacings->{1, 0}],
	Style["t=1000",{FontFamily->"Arial",20}],Right,Spacings->{1, 0}],Bottom];
plot2=Labeled[Show[Table[RadialAxisPlot[pertubation[[i]]*F,PlotRange->{-F*1.5,F*1.5},GridLines->Range[-F*1.5,F*1.5,5],
	Axes->Table[If[i==0,True,If[Mod[i,J]==0,1,False]],{i,K*J}],
	Filling->False,
	PlotLabel->"Y",
	(*PlotStyle\[Rule]Blend[{Darker[Blue],Blue,White,Red,Darker[Red]},(i-1000)/1000],*)
	PlotStyle->ColorData["Rainbow"][(i-1000)/1000.],
	BaseStyle->{FontFamily->"Arial",12}],{i,1000,2000,50}],ImageSize->280],
	Labeled[Labeled[ImageCrop[Rasterize[BarLegend["Rainbow",Ticks->None,LegendLayout->"Row"]]],Style["t=0",{FontFamily->"Arial",20}],Left,Spacings->{1, 0}],
	Style["t=1000",{FontFamily->"Arial",20}],Right,Spacings->{1, 0}],Bottom];
      
