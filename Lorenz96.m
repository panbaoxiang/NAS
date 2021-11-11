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

      
