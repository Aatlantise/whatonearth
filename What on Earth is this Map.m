(* ::Package:: *)

(* ::Title:: *)
(*What on Earth is this Map?*)


(* ::Chapter:: *)
(*Junghyun Min*)


(* ::Section:: *)
(*Preprocess and Neural Network: que7*)


(* ::Subsubsection:: *)
(*Prepping Images*)


(* ::Input::Initialization:: *)
inception=NetModel["Inception V3 Trained on ImageNet Competition Data"];


(* ::Input::Initialization:: *)
inception2=Take[inception,{1,30}]


(* ::Input::Initialization:: *)
adjust1[x_]:=Binarize[Sharpen[ImageApply[Min,x]]] (*for que7*)


(* ::Input::Initialization:: *)
preprocess[x_List]:=inception2[adjust1/@x]
preprocess[x:_Graphics|_Image]:=inception2[adjust1[x]]


dir=NotebookDirectory[];
SetDirectory[dir];


(* ::Input::Initialization:: *)
mapcat={"map of Asia","map of Europe","map of Oceania","map of North America","map of South America","map of Africa"};


(* ::Input:: *)
(*(* This part of code requires Wolfram Cloud Credits -- instead, the training set of maps has been processed and saved in folder Dumpsaves.;*)
(**)
(*ServiceConnect["BingSearch"]*)
(*import1=ServiceExecute["BingSearch","Search",{"Query"->#,"SearchType"->"Images","MaxItems"->100,"Elements"->"Thumbnails"}] & /@ mapcat;*)
(*mapslist=Flatten[import1];*)
(*adjustedmapslist=adjust/@mapslist;*)
(*adjustedmaps=Partition[adjustedmapslist,100];*)
(*inceptedmapslist=inception2[adjustedmapslist];*)
(*importedimages=ServiceExecute["BingSearch","Search",{"Query"->#,"SearchType"->"Images","MaxItems"->100,"Elements"->"Thumbnails","StartIndex"->101}] & /@ mapcat;*)
(*importedimages=DeleteDuplicates/@importedimages;*)
(**)
(*asparted=Flatten[ImagePartition[#,{100,100},{20,40}]&/@importedimages[[1]]];*)
(*euparted=Flatten[ImagePartition[#,{100,100},{20,40}]&/@importedimages[[2]]];*)
(*ocparted=Flatten[ImagePartition[#,{100,100},{20,40}]&/@importedimages[[3]]];*)
(*naparted=Flatten[ImagePartition[#,{100,100},{20,40}]&/@importedimages[[4]]];*)
(*saparted=Flatten[ImagePartition[#,{100,100},{20,40}]&/@importedimages[[5]]];*)
(*afparted=Flatten[ImagePartition[#,{100,100},{20,40}]&/@importedimages[[6]]];*)
(**)
(*iaspt=inception2[asparted];*)
(*ieupt=inception2[euparted];*)
(*iocpt=inception2[ocparted]; *)
(*inapt=inception2[naparted];*)
(*isapt=inception2[saparted];*)
(*iafpt=inception2[afparted];*)
(**)*)


(* ::Subsubsection:: *)
(*Working with previously saved images*)


(* ::Input:: *)
(*SetDirectory[FileNameJoin[{dir,"Dumpsaves"}]];*)
(*<<iaspt.mx*)
(*<<ieupt.mx*)
(*<<iocpt.mx*)
(*<<inapt.mx*)
(*<<isapt.mx*)
(*<<iafpt.mx*)


(* ::Input:: *)
(*itot={iaspt,ieupt,iocpt,inapt,isapt,iafpt};*)


(* ::Input:: *)
(*inceptedtrain2=RandomSample[Flatten[Thread/@Thread[itot->mapcat]]];*)


(* ::Input:: *)
(*Length[inceptedtrain2] (*check for number of partitioned number of maps to be trained*)*)


(* ::Input:: *)
(*Import["V:\\OneDrive - Johns Hopkins University\\Wolfram Mathematica\\Project\\DumpSaves\\inceptedmapslist.mx"] (*whole maps*)*)


(* ::Input:: *)
(*Length[inceptedmapslist]*)


(* ::Input:: *)
(*inceptedtrain=RandomSample[Flatten[Thread/@Thread[Partition[inceptedmapslist,100]-> mapcat]]];*)


(* ::Input:: *)
(*inceptedtrainu=RandomSample[Union[inceptedtrain,inceptedtrain2]];*)


(* ::Input:: *)
(*dec=NetDecoder[{"Class",mapcat}]*)


(* ::Input:: *)
(*loss=CrossEntropyLossLayer["Target"->NetEncoder[{"Class",mapcat}]]*)


(* ::Input:: *)
(*net6=NetChain[{ReshapeLayer[{128,4,4}],ConvolutionLayer[200,{3,3}],BatchNormalizationLayer[],FlattenLayer[],ElementwiseLayer[Ramp],DotPlusLayer[512],ReshapeLayer[{32,4,4}],ConvolutionLayer[16,{3,3}],BatchNormalizationLayer[],FlattenLayer[],ElementwiseLayer[Ramp],DotPlusLayer[32],ElementwiseLayer[Ramp],ReshapeLayer[{2,4,4}],ConvolutionLayer[6,{3,3}],BatchNormalizationLayer[],FlattenLayer[],DotPlusLayer[6],SoftmaxLayer[]},"Output"->dec,"Input"->2048];*)


(* ::Input:: *)
(*NetInitialize[net6]*)


(* ::Input:: *)
(*l=Length[inceptedtrainu];*)
(*a=Round[l*0.7];*)
(*b=Round[l*0.9];*)


(* ::Input:: *)
(*que7=NetTrain[net6,inceptedtrainu[[1;;a]],loss,MaxTrainingRounds->200,ValidationSet->inceptedtrainu[[a+1;;b]]] *)


(* ::Input:: *)
(*ClassifierMeasurements[que7,inceptedtrainu[[b+1;;l]],"Accuracy"]*)


(* ::Input:: *)
(*Export["V:\\OneDrive - Johns Hopkins University\\Wolfram Mathematica\\What on Earth is this Map\\Updates\\que7.wlnet",que7]*)


(* ::Section:: *)
(*TextRecognize & Threecities*)


(* ::Subsection:: *)
(*Adjust*)


(* ::Text:: *)
(*Partition inputted image, process to facilitate TextRecognize*)


(* ::Input::Initialization:: *)
adjust[x_]:=Module[{w,y,z},
w=ImageResize[#,300]&/@Flatten[ImagePartition[x,{{150},{50}},{75,25}]];
y=ImageApply[Max,#]&/@ImageAdjust/@w;
z=Sharpen/@y;
Binarize/@z] (*for textrecognize*) (*try imageadjust*)


(* ::Subsection:: *)
(*Foundcities*)


(* ::Text:: *)
(*Use TextRecognize to extract labels*)
(*Employ BoundingBox for the labels' locations on maps*)
(*Output: list of cities found on the map*)


(* ::Input::Initialization:: *)
foundcities[map_]:=
Module[{rem,adjusted,textpos,text,bb,int,entitypos,goodtext,goodbb,center,goodint,pos,part,flt,coords,ratio,index},
rem=RemoveAlphaChannel[map];
adjusted=adjust[rem];
textpos=TextRecognize[#, "Line", {"Text", "BoundingBox"}]&/@adjusted;(*partitions with detected text*)
text=Select[Flatten[textpos[[All,All,1]]],StringQ];
bb=Select[Flatten[DeleteMissing[textpos][[All,All,2]]],Head[#]==Rectangle&];
int=Interpreter["City"]/@text;
entitypos=Flatten[Position[int,_Entity]]; (*where the interpreter outputs entities*)
goodtext=text[[entitypos]]; (*valid texts: city names*)
goodbb=bb[[entitypos]]; (*valid positions*)
center=N[Mean/@List@@@goodbb];
goodint=int[[entitypos]]; (*valid entities*)
pos=Flatten[Position[textpos,#]&/@Thread[{goodtext,goodbb}],1 (*list of {{valid text, valid pos},{valid text2,valid pos2},...}*)];
part=ImagePartition[rem,{{150},{50}},{75,25}];
flt=Flatten[part];
coords=findcoo[part,#]&/@pos[[All,1]];
ratio=ImageDimensions/@flt[[pos[[All,1]]]]/ImageDimensions/@adjusted[[pos[[All,1]]]];
index=Association[Thread[goodint[[All,2,1]]->coords+center*ratio]];
DeleteDuplicates[{index}]
]


(* ::Subsubsection:: *)
(*Findcoo*)


(* ::Text:: *)
(*Finds coordinates of the cities found*)


(* ::Input::Initialization:: *)
findcoo[part_,posi_]:=Module[{flt,h,w,dw,dh,x,y,assemblelist,as,origco},
flt=Flatten[part];
h=Length[part];
w=Length[part[[1]]];
dw=Mod[posi,w]/. 0->6;
dh=h-Floor[(posi-1)/w];
x=posi-Reverse[Range[1,dw]];
y=w*Range[0,dh-1]+1;
assemblelist=Plus[x,#]&/@y;
as=ImageAssemble[Partition[flt[[Flatten[assemblelist]]],dw]];
origco=ImageDimensions[as]-{75*(dw-1),25*(dh-1)}-ImageDimensions[flt[[posi]]]
]


(* ::Subsection:: *)
(*Textprocess*)


(* ::Text:: *)
(*Apply twocities to Foundcities, pick ones with highest probability*)


(* ::Input::Initialization:: *)
textprocess[x_List(*of entities*)]:=
Module[{str,tri},
str=Flatten[Keys[x]]; (*All syntax also works with probability as well*)
tri=Subsets[str,{2}];
twocities[#[[1]],#[[2]]]&/@tri(*apply threecities to all, pick the one with highest probability*)
]


(* ::Subsubsection:: *)
(*Twocities*)


(* ::Text:: *)
(*Analog of threecities, but with CityDoubles*)


(* ::Input::Initialization:: *)
twocities[x_String,y_String]:=
Module[{cityone,citytwo,onecoords,twocoords,goodpos1,goodpos2,pop,dist,normmax,max,prob,finalprob,pos,a,b},
cityone=CityData[x];
citytwo=CityData[y];
onecoords=EntityValue[#,"Coordinates"]&/@cityone;
goodpos1=Flatten[Position[onecoords,{_?NumericQ..}]];
twocoords=EntityValue[#,"Coordinates"]&/@citytwo;
goodpos2=Flatten[Position[twocoords,{_?NumericQ..}]];
cityone=cityone[[goodpos1]];
citytwo=citytwo[[goodpos2]];
onecoords=onecoords[[goodpos1]];
twocoords=twocoords[[goodpos2]];
pop=Normalize[EntityValue[#,"Population"]&/@cityone]^2;
dist=QuantityMagnitude[GeoDistance[#1,#2]&[onecoords,twocoords]];
prob=Partition[Normalize[Flatten[2.71^(-dist)]]^2,Length[citytwo]];
finalprob=pop*prob;
normmax=Max[Normalize[Flatten[finalprob]]];
max=Max[finalprob];
pos=Position[finalprob,max];
{{a,b}}=pos;
Association["CityDouble"-> {cityone[[a]],citytwo[[b]]},"Probability"-> normmax^2]
]


(* ::Subsection:: *)
(*Threeprocess*)


(* ::Text:: *)
(*Analog of textprocess, but with threecities*)


(* ::Input::Initialization:: *)
threeprocess[x_List]:=
Module[{str,tri},
str=Flatten[Keys[x]]; (*All syntax also works with probability as well*)
tri=Subsets[str,{3}];
threecities[#[[1]],#[[2]],#[[3]]]&/@tri(*apply threecities to all, pick the one with highest probability*)
]


(* ::Subsubsection:: *)
(*Threecities*)


(* ::Text:: *)
(*Input of strings of city names, outputs an association of CityTriples and their probabilities*)


(* ::Input::Initialization:: *)
threecities[x_String,y_String,z_String]:=
Module[{cityone,citytwo,citythree,onecoords,twocoords,threecoords,goodpos1,goodpos2,goodpos3,pop,dist1,prob1,dist2,prob2,finalprob,max,normmax,pos,a,b,c},
cityone=CityData[x];
citytwo=CityData[y];
citythree=CityData[z];
onecoords=EntityValue[#,"Coordinates"]&/@cityone;
goodpos1=Flatten[Position[onecoords,{_?NumericQ..}]];
twocoords=EntityValue[#,"Coordinates"]&/@citytwo;
goodpos2=Flatten[Position[twocoords,{_?NumericQ..}]];
threecoords=EntityValue[#,"Coordinates"]&/@citythree;
goodpos3=Flatten[Position[threecoords,{_?NumericQ..}]];
cityone=cityone[[goodpos1]];
citytwo=citytwo[[goodpos2]];
citythree=citythree[[goodpos3]];
onecoords=onecoords[[goodpos1]];
twocoords=twocoords[[goodpos2]];
threecoords=threecoords[[goodpos3]];
pop=Normalize[EntityValue[#,"Population"]&/@cityone]^2;
dist1=QuantityMagnitude[GeoDistance[#1,#2]&[onecoords,twocoords]];
prob1=Exp[-dist1];
dist2=QuantityMagnitude[Table[GeoDistance[onecoords[[i]],#]+GeoDistance[twocoords[[j]],#]&/@threecoords,{i,Length[cityone]},{j,Length[citytwo]}]];
prob2=Exp[-dist2];
finalprob=pop*prob1*prob2;
max=Max[finalprob];
normmax=Max[Normalize[Flatten[finalprob]]];
pos=Position[finalprob,max];
{{a,b,c}}=pos;
Association["CityTriple"-> {cityone[[a]],citytwo[[b]],citythree[[c]]},"Probability"-> normmax] (*coordinates, entity, position on map*)
] 


(* ::Subsection:: *)
(*Bestans*)


(* ::Text:: *)
(*Pick best CityTriple based on que7 and textprocess's twocities output*)


(* ::Input::Initialization:: *)
bestans[x_List(*from twocities*),y_String(*from que*)]:=Module[{goodans,ctr,ctr2,cont2,cont,tgtctn,ct,final,i,a,b},
goodans=MaximalBy[x,"Probability"];
ctr=goodans[[All,"CityDouble",All,2,3]]; (*countries in string*)
ctr2=Map[Entity["Country",#]&,ctr,{2}];
cont2=Map[#["Continent"]&,ctr2,{2}];
cont=cont2[[All,All,2]];
tgtctn=conv[y]; (*transformed output from NeuralNet*)
ct=Count[#,tgtctn]&/@cont; (*however many that target continent is present*)
final=goodans[[Ordering[ct,1,Greater]]];
final
]


(* ::Subsubsection:: *)
(*Conv*)


(* ::Text:: *)
(*Adjusts syntax of string variables to match info included in entities*)


(* ::Input::Initialization:: *)
conv[x_List]:=StringReplace[#,{"map of "->""," Am"->"Am"}]&/@x
conv[x_String]:=StringReplace[x,{"map of "->""," Am"->"Am"}]


(* ::Subsubsection:: *)
(*Bestansth*)


(* ::Text:: *)
(*Best answer analog for 3 cities*)


(* ::Input::Initialization:: *)
bestansth[x_List(*from threecities*),y_String(*from que*)]:=Module[{goodans,ctr,ctr2,cont2,cont,tgtctn,ct,final,i,a,b}, (*th to differentiate bestans used to work with city doubles*)
goodans=MaximalBy[x,#["Probability"]&]; (*see issues below*)
ctr=goodans[[All,"CityTriple",All,2,3]]; (*countries in string*)
ctr2=Map[Entity["Country",#]&,ctr,{2}];
cont2=Map[#["Continent"]&,ctr2,{2}];
cont=cont2[[All,All,2]];
tgtctn=conv[y]; (*transformed output from NeuralNet*)
ct=Count[#,tgtctn]&/@cont; (*however many that target continent is present*)
final=goodans[[Ordering[ct,UpTo[3],Greater]]];
final
]


(* ::Subsubsection:: *)
(*Bestansone*)


(* ::Text:: *)
(*Best answer analog for 1 city*)


(* ::Input::Initialization:: *)
bestansone[fnd_,quep_]:=Module[{ct,data,cv,cont,list,sel},
data=CityData[Keys[fnd][[1,1]]];
cv=conv[quep];
cont=EntityClass["Country",cv];
list=EntityList[cont];
sel=Select[data,MemberQ[list,#["Country"]]&];
First[sel, data[[1]]]
]


(* ::Section:: *)
(*Triangle*)


(* ::Chapter:: *)
(*Projection Identify*)


(* ::Subsection:: *)
(*Triangle*)


(* ::Text:: *)
(*Uses info from foundcities to draw triangles on map*)


(* ::Input::Initialization:: *)
triangle[x_(*from foundcities*),y_ (*triples from bestans*)]:=Module[{asc,cts,out},
asc=Association[x];
cts=y["CityTriple"];
out=Association["CityTriple"->cts,"Coordinates"->asc/@cts[[All,2,1]]];
out
]


(* ::Subsection:: *)
(*Analysis*)


(* ::Text:: *)
(*Analyze the triangles*)


(* ::Subsubsection:: *)
(*Legratio*)


(* ::Text:: *)
(*Input: triangle coordinates on map & on eaerth*)
(*Output: Ratio of legs of the triangles*)


(* ::Input::Initialization:: *)
legratio[{x_List,y_List,z_List}]:=Module[{legone,legtwo,legthree,min},
legone=Norm[x-y];
legtwo=Norm[y-z];
legthree=Norm[z-x];
min=Min[{legone,legtwo,legthree}];
{{legone,legtwo,legthree}/min,min}
]


(* ::Input::Initialization:: *)
geolegratio[{x_,y_,z_}]:=Module[{legone,legtwo,legthree,min},
legone=GeoDistance[x,y];
legtwo=GeoDistance[y,z];
legthree=GeoDistance[z,x];
min=Min[{legone,legtwo,legthree}];
{{legone,legtwo,legthree}/min,QuantityMagnitude[min]}
]


(* ::Subsubsection:: *)
(*Angleratio*)


(* ::Text:: *)
(*Input: triangle coordinates*)
(*Output: List of angles*)


(* ::Input::Initialization:: *)
angles[x_List]:=Module[{legone,legtwo,legthree,\[Theta]1,\[Theta]2,\[Theta]3},
legone=x[[1]]-x[[2]];
legtwo=x[[2]]-x[[3]];
legthree=x[[3]]-x[[1]];
\[Theta]1=VectorAngle[legone,-legtwo]*180/\[Pi];
\[Theta]2=VectorAngle[legtwo,-legthree]*180/\[Pi];
\[Theta]3=VectorAngle[legthree,-legone]*180/\[Pi];
{\[Theta]1,\[Theta]2,\[Theta]3}
]


(* ::Subsubsection:: *)
(*Arearatio*)


(* ::Text:: *)
(*On multiple triangles*)


(* ::Input::Initialization:: *)
areacompare[tri_]:=Module[{coo,area,arearat,cts,geopos,geoarea,geoarearat},
coo=#["Coordinates"]&/@tri;
area=Area/@Polygon/@coo;
arearat=TakeLargest[ area/Min[area]];
cts=#["CityTriple"]&/@tri;
geopos=GeoPosition/@#&/@cts;
geoarea=QuantityMagnitude[GeoArea/@Polygon/@geopos];
geoarearat=TakeLargest[geoarea/Min[geoarea]];
(arearat-geoarearat)/geoarearat*100
]


(* ::Subsection:: *)
(*Comparison*)


(* ::Input::Initialization:: *)
compare[x_Association(*output from triangle*)]:=Module[{coo,cts,ctcoo,angle,geoangle,legrat,geolegrat,area,geoarea,angleerr,legerr},
coo=x["Coordinates"];
cts=x["CityTriple"];
ctcoo=EntityValue[#,"Coordinates"]&/@cts;
angle=angles[coo];
geoangle=angles[ctcoo];
legrat=legratio[coo];
geolegrat=geolegratio[cts];
angleerr=N[(angle-geoangle)*100/360];
legerr=N[(legrat-geolegrat)*100/geolegrat];
Print[TableForm[{{"Type","Angles",{"Leg Ratio","Shortest leg"}},{"Map",angle,legrat},{"Earth",geoangle,geolegrat},{"Error",Mean[angleerr],Mean[legerr[[1]]]}},TableAlignments->Center,TableDepth->2]];
{angleerr,legerr[[1]]}
]



(* ::Section:: *)
(*Final*)


(* ::Input::Initialization:: *)
final[x:_Image|_Graphics]:=Module[{fnd,textp,prep,quep,len,besta,tpr,bestath,tri,comp,list,loc,ans},
fnd=foundcities[x]; (*assoc. of strings to map coords*)

prep=preprocess[x]; (*preprecess for que7*)
quep=que7[prep]; (*finds continent*)
len=Length[fnd[[1]]];
Which[len>=4,
	tpr=threeprocess[fnd];
	besta=bestansth[tpr,quep];
			loc=ToString[StringForm["This map shows `` which are located in ``, respectively. ", StringRiffle[besta[[1]]["CityTriple"][[All,2,1]],", "],StringRiffle[DeleteDuplicates[besta[[1]]["CityTriple"][[All,2,3]]],", "]]];
	tri=triangle[fnd,#]&/@besta;
	If[Length[tri]>1,
		comp=compare[tri[[1]]];
		list=List["Conformal"-> comp[[1]],"Equidistant"->comp[[2]],"Equal-area"->areacompare[tri]] ;
		ans=StringJoin[loc,ToString[StringForm["The map is ``.",MinimalBy[list,Last][[1,1]]]]];
		,
		comp=compare[tri[[1]]];
		list=List["Conformal"-> comp[[1]],"Equidistant"->comp[[2]],"Equal-area"->5] ;
		ans=StringJoin[loc,ToString[StringForm["The map is ``.",MinimalBy[list,Last][[1,1]]]]];
	],
len>=2,
	textp=textprocess[fnd];
	besta=Quiet[bestans[textp,quep]];
	ans=StringJoin[ToString[StringForm["This map shows `` , which is located in `` , respectively. "]],StringRiffle[textp[[1]]["CityDouble"][[All,2,1]],", "],StringRiffle[DeleteDuplicates[textp[[1]]["CityDouble"][[All,2,3]]],", "],
	"There is not enough data to determine the projection."];,
len==1,
	besta=Quiet[bestansone[fnd,quep]];
	ans=StringJoin[ToString[StringForm["This map shows ``, which is located in ``. ",besta["Name"],besta["Country"]["Name"]]],
	"There is not enough data to determine the projection."];,
len==0,
	ans=StringJoin[ToString[StringForm["This is a ``. ",quep]],
	"There is not enough data to determine the projection."];
];
ans
]
final[x_]:="This is not a map";;




(* ::Input:: *)
(*map=GeoGraphics[\!\(\**)
(*NamespaceBox["LinguisticAssistant",*)
(*DynamicModuleBox[{Typeset`query$$ = "Baltimore", Typeset`boxes$$ = TemplateBox[{"\"Baltimore\"", RowBox[{"Entity", "[", RowBox[{"\"City\"", ",", RowBox[{"{", RowBox[{"\"Baltimore\"", ",", "\"Maryland\"", ",", "\"UnitedStates\""}], "}"}]}], "]"}], "\"Entity[\\\"City\\\", {\\\"Baltimore\\\", \\\"Maryland\\\", \\\"UnitedStates\\\"}]\"", "\"city\""}, "Entity"], Typeset`allassumptions$$ = {{"type" -> "Clash", "word" -> "Baltimore", "template" -> "Assuming \"${word}\" is ${desc1}. Use as ${desc2} instead", "count" -> "3", "Values" -> {{"name" -> "City", "desc" -> "a city", "input" -> "*C.Baltimore-_*City-"}, {"name" -> "NFLTeam", "desc" -> "an NFL team", "input" -> "*C.Baltimore-_*NFLTeam-"}, {"name" -> "MLBTeam", "desc" -> "an MLB team", "input" -> "*C.Baltimore-_*MLBTeam-"}}}, {"type" -> "SubCategory", "word" -> "Baltimore", "template" -> "Assuming ${desc1}. Use ${desc2} instead", "count" -> "3", "Values" -> {{"name" -> "{Baltimore, Maryland, UnitedStates}", "desc" -> "Baltimore (Maryland, USA)", "input" -> "*DPClash.CityE.Baltimore-_**Baltimore.Maryland.UnitedStates--"}, {"name" -> "{Baltimore, Ohio, UnitedStates}", "desc" -> "Baltimore (Ohio, USA)", "input" -> "*DPClash.CityE.Baltimore-_**Baltimore.Ohio.UnitedStates--"}, {"name" -> "{Baltimore, Vermont, UnitedStates}", "desc" -> "Baltimore (Vermont, USA)", "input" -> "*DPClash.CityE.Baltimore-_**Baltimore.Vermont.UnitedStates--"}}}}, Typeset`assumptions$$ = {}, Typeset`open$$ = {1, 2}, Typeset`querystate$$ = {"Online" -> True, "Allowed" -> True, "mparse.jsp" -> 0.737692`6.319420067215035, "Messages" -> {}}}, *)
(*DynamicBox[ToBoxes[AlphaIntegration`LinguisticAssistantBoxes["", 4, Automatic, Dynamic[Typeset`query$$], Dynamic[Typeset`boxes$$], Dynamic[Typeset`allassumptions$$], Dynamic[Typeset`assumptions$$], Dynamic[Typeset`open$$], Dynamic[Typeset`querystate$$]], StandardForm],*)
(*ImageSizeCache->{155., {9., 20.}},*)
(*TrackedSymbols:>{Typeset`query$$, Typeset`boxes$$, Typeset`allassumptions$$, Typeset`assumptions$$, Typeset`open$$, Typeset`querystate$$}],*)
(*DynamicModuleValues:>{},*)
(*UndoTrackedVariables:>{Typeset`open$$}],*)
(*BaseStyle->{"Deploy"},*)
(*DeleteWithContents->True,*)
(*Editable->False,*)
(*SelectWithContents->True]\),{GeoBackground->Automatic,GeoRange->100000}]//Rasterize (*GeoProjection\[Rule]Automatic gives Mercator in this case*)*)


(* ::Input:: *)
(*final[map]*)


(* ::Input:: *)
(*map2=GeoGraphics[\!\(\**)
(*NamespaceBox["LinguisticAssistant",*)
(*DynamicModuleBox[{Typeset`query$$ = "Toronto", Typeset`boxes$$ = TemplateBox[{"\"Toronto\"", RowBox[{"Entity", "[", RowBox[{"\"City\"", ",", RowBox[{"{", RowBox[{"\"Toronto\"", ",", "\"Ontario\"", ",", "\"Canada\""}], "}"}]}], "]"}], "\"Entity[\\\"City\\\", {\\\"Toronto\\\", \\\"Ontario\\\", \\\"Canada\\\"}]\"", "\"city\""}, "Entity"], Typeset`allassumptions$$ = {{"type" -> "Clash", "word" -> "Toronto", "template" -> "Assuming \"${word}\" is ${desc1}. Use as ${desc2} instead", "count" -> "4", "Values" -> {{"name" -> "City", "desc" -> "a city", "input" -> "*C.Toronto-_*City-"}, {"name" -> "AdministrativeDivision", "desc" -> "an administrative division", "input" -> "*C.Toronto-_*AdministrativeDivision-"}, {"name" -> "NBATeam", "desc" -> "an NBA team", "input" -> "*C.Toronto-_*NBATeam-"}, {"name" -> "MLBTeam", "desc" -> "an MLB team", "input" -> "*C.Toronto-_*MLBTeam-"}}}, {"type" -> "SubCategory", "word" -> "Toronto", "template" -> "Assuming ${desc1}. Use ${desc2} instead", "count" -> "5", "Values" -> {{"name" -> "{Toronto, Ontario, Canada}", "desc" -> "Toronto (Canada)", "input" -> "*DPClash.CityE.Toronto-_**Toronto.Ontario.Canada--"}, {"name" -> "{Toronto, Ohio, UnitedStates}", "desc" -> "Toronto (Ohio, USA)", "input" -> "*DPClash.CityE.Toronto-_**Toronto.Ohio.UnitedStates--"}, {"name" -> "{Toronto, Kansas, UnitedStates}", "desc" -> "Toronto (Kansas, USA)", "input" -> "*DPClash.CityE.Toronto-_**Toronto.Kansas.UnitedStates--"}, {"name" -> "{Toronto, SouthDakota, UnitedStates}", "desc" -> "Toronto (South Dakota, USA)", "input" -> "*DPClash.CityE.Toronto-_**Toronto.SouthDakota.UnitedStates--"}, {"name" -> "{Toronto, Iowa, UnitedStates}", "desc" -> "Toronto (Iowa, USA)", "input" -> "*DPClash.CityE.Toronto-_**Toronto.Iowa.UnitedStates--"}}}}, Typeset`assumptions$$ = {}, Typeset`open$$ = {1, 2}, Typeset`querystate$$ = {"Online" -> True, "Allowed" -> True, "mparse.jsp" -> 0.408726`6.0629772585561765, "Messages" -> {}}}, *)
(*DynamicBox[ToBoxes[AlphaIntegration`LinguisticAssistantBoxes["", 4, Automatic, Dynamic[Typeset`query$$], Dynamic[Typeset`boxes$$], Dynamic[Typeset`allassumptions$$], Dynamic[Typeset`assumptions$$], Dynamic[Typeset`open$$], Dynamic[Typeset`querystate$$]], StandardForm],*)
(*ImageSizeCache->{142., {9., 20.}},*)
(*TrackedSymbols:>{Typeset`query$$, Typeset`boxes$$, Typeset`allassumptions$$, Typeset`assumptions$$, Typeset`open$$, Typeset`querystate$$}],*)
(*DynamicModuleValues:>{},*)
(*UndoTrackedVariables:>{Typeset`open$$}],*)
(*BaseStyle->{"Deploy"},*)
(*DeleteWithContents->True,*)
(*Editable->False,*)
(*SelectWithContents->True]\),{GeoBackground->Automatic,GeoRange->100000}]//Rasterize (*GeoProjection\[Rule]Automatic gives Mercator in this case*)*)


(* ::Input:: *)
(*final[map2]*)
