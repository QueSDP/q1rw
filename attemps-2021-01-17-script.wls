#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Meta-settings*)


(* ::Input:: *)
(*$Version*)


(* ::Input::Initialization:: *)
Charting`ResolvePlotTheme[Automatic,VectorPlot];
vcf=ColorDataFunction["WL12DefaultVectorGradient", "ThemeGradients", {0, 1}, Blend["WL12DefaultVectorGradient", #]& ];


(* ::Input::Initialization:: *)
Monitor`NestList[f_,init_,n_]:=Module[{it=0},PrintTemporary[ProgressIndicator[Dynamic[it/n]]];
NestList[(it++;f[#])&,init,n]]
(*WFQTransit[weights_List,relaxation_:0]:=Function[state,RandomChoice[weights\[RuleDelayed]((#+state)&/@(IdentityMatrix[Length]~Join~(-IdentityMatrix[Length[state]])))]]*)


(* ::Input::Initialization:: *)
SetOptions[SelectedNotebook[],PrintingStyleEnvironment->"Printout",ShowSyntaxStyles->True]


(* ::Section:: *)
(*Time-regime simulation*)


(* ::Subsection:: *)
(*Simulations*)


(* ::Subsubsection:: *)
(*Generic*)


(* ::Input::Initialization:: *)
WFQTransitGenerate[\[Lambda]_,\[Mu]_,\[CapitalDelta]t_,p_,w_]:=With[{K=Length[p]},Function[\[FormalN],Module[{W,Wsum},
W=Function[\[FormalM],Table[If[Indexed[\[FormalM],k]==0,0,w[[k]]],{k,1,K}]];Wsum=Function[\[FormalM],Sum[If[Indexed[\[FormalM],k]==0,0,w[[k]]],{k,1,K}]];
Table[Indexed[\[FormalN],k],{k,1,K}]+
RandomChoice[N[Exp[\[CapitalDelta]t \[Lambda] p]~Join~Exp[\[CapitalDelta]t \[Mu] W[\[FormalN]] If[Wsum[\[FormalN]]==0,0,1/Wsum[\[FormalN]]]]-1]:>Evaluate@Catenate@{IdentityMatrix[K],-IdentityMatrix[K]}]
]]]


(* ::ItemNumbered:: *)
(*Turn into a function*)


(* ::Input::Initialization:: *)
Needs["ComputerArithmetic`"]
ql11MnStd[K_,\[Rho]_:.9,\[Mu]_:1,\[CapitalDelta]t_:.01,magn_:6,maxr_:5]:=Module[
{(*K=1,\[Rho]=.9,*)\[Lambda],(*\[Mu]=1,\[CapitalDelta]t=0.01,magn=6,maxr=5,*)
func,ql11,ql11Time,ql11MnStd,ql11MnTime(*,ql11MnArn,ql11MnRel*)},\[Lambda]=\[Mu] \[Rho];

func=WFQTransitGenerate[\[Lambda],\[Mu],\[CapitalDelta]t,Table[1./K,K],Table[1./K,K]];
{ql11Time,ql11}=Timing@ResourceFunction[ResourceObject[Association["Name" -> "MonitorProgress", "ShortName" -> "MonitorProgress", "UUID" -> "4b77322e-7eb3-4aaf-ac6b-b1a032ebcce4", "ResourceType" -> "Function", "Version" -> "2.0.0", "Description" -> "Monitor the progress of a computation and display the current step and estimated time remaining", "RepositoryLocation" -> URL["https://www.wolframcloud.com/objects/resourcesystem/api/1.0"], "SymbolName" -> "FunctionRepository`$08a8f1cdc2f243a382a99fd2ba5ecd31`MonitorProgress", "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/204fda58-3b6c-4361-91c3-af7328b618a9"]], ResourceSystemBase -> Automatic]][NestList[func,Table[0,K],10^magn],"Label"->StringTemplate["Simulation\n`1`"][<|"K"->K,"\[Lambda]"->\[Lambda],"\[Mu]"->\[Mu],"\[CapitalDelta]t"->\[CapitalDelta]t|>]]//Quiet;
{ql11MnTime,ql11MnStd}=Timing@ResourceFunction[ResourceObject[Association["Name" -> "MonitorProgress", "ShortName" -> "MonitorProgress", "UUID" -> "4b77322e-7eb3-4aaf-ac6b-b1a032ebcce4", "ResourceType" -> "Function", "Version" -> "2.0.0", "Description" -> "Monitor the progress of a computation and display the current step and estimated time remaining", "RepositoryLocation" -> URL["https://www.wolframcloud.com/objects/resourcesystem/api/1.0"], "SymbolName" -> "FunctionRepository`$08a8f1cdc2f243a382a99fd2ba5ecd31`MonitorProgress", "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/204fda58-3b6c-4361-91c3-af7328b618a9"]], ResourceSystemBase -> Automatic]][Table[{Mean[#],If[Length[#]>1,StandardDeviation[#],Nothing]}&@(Moment[#,r UnitVector[K,1]]&/@Partition[N@ql11,length]),{length,Evaluate@Table[10^k,{k,1,magn}]},{r,0,maxr}],"Label"->StringTemplate["Calculating mean and STD\n`1`"][<|"magn"->magn,"maxr"->maxr|>]];
<|"SimulationTime"->ql11Time,"SimulationTrace"->ql11,"StatisticsTime"->ql11MnTime,"Statistics"->ql11MnStd|>
(*
ql11MnArn=[\[FilledSmallSquare]]	MonitorProgress @Map[Around[#\[LeftDoubleBracket]1\[RightDoubleBracket],If[NumberQ[#\[LeftDoubleBracket]2\[RightDoubleBracket]]&&(#\[LeftDoubleBracket]2\[RightDoubleBracket]>0),{Min[#\[LeftDoubleBracket]1\[RightDoubleBracket]-10^-5,#\[LeftDoubleBracket]2\[RightDoubleBracket]],#\[LeftDoubleBracket]2\[RightDoubleBracket]},10^-10]]&,Most[ql11MnStd],{2}]~Join~{Flatten[Last[ql11MnStd]]};
ql11MnRel=(#/Last[ql11MnArn])&/@Most[ql11MnArn];
*)
]


(* ::Input::Initialization:: *)
SetDirectory[FileNameJoin[{$HomeDirectory,"Desktop"}]]
Module[{filename="ql11MnStd.xlsx",K=8,\[Rho]=.9,\[Mu]=1,\[CapitalDelta]t=0.01,magn=8,maxr=10,res,timing,calctime,array,stringargs},
Export[filename,ResourceFunction[ResourceObject[Association["Name" -> "MonitorProgress", "ShortName" -> "MonitorProgress", "UUID" -> "4b77322e-7eb3-4aaf-ac6b-b1a032ebcce4", "ResourceType" -> "Function", "Version" -> "2.0.0", "Description" -> "Monitor the progress of a computation and display the current step and estimated time remaining", "RepositoryLocation" -> URL["https://www.wolframcloud.com/objects/resourcesystem/api/1.0"], "SymbolName" -> "FunctionRepository`$08a8f1cdc2f243a382a99fd2ba5ecd31`MonitorProgress", "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/204fda58-3b6c-4361-91c3-af7328b618a9"]], ResourceSystemBase -> Automatic]][ParallelTable[ResourceFunction[ResourceObject[Association["Name" -> "MonitorProgress", "ShortName" -> "MonitorProgress", "UUID" -> "4b77322e-7eb3-4aaf-ac6b-b1a032ebcce4", "ResourceType" -> "Function", "Version" -> "2.0.0", "Description" -> "Monitor the progress of a computation and display the current step and estimated time remaining", "RepositoryLocation" -> URL["https://www.wolframcloud.com/objects/resourcesystem/api/1.0"], "SymbolName" -> "FunctionRepository`$08a8f1cdc2f243a382a99fd2ba5ecd31`MonitorProgress", "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/204fda58-3b6c-4361-91c3-af7328b618a9"]], ResourceSystemBase -> Automatic]]["SetCurrent"][StringTemplate["K=`1`"][\[FormalCapitalK]]];res=ql11MnStd[\[FormalCapitalK],\[Rho],\[Mu],\[CapitalDelta]t,magn,maxr];{timing,calctime,array}=Values[res[[{"SimulationTime","StatisticsTime","Statistics"}]]];stringargs=Sequence[\[FormalCapitalK],\[Rho],\[Mu],\[CapitalDelta]t,magn,maxr,timing,calctime];StringTemplate["K`1`"][stringargs]->ArrayFlatten[({
 {{{StringTemplate["(`7`s`8`s)\[Rho]`2`\[Mu]`3`\[CapitalDelta]t`4`"][stringargs]}}, {Flatten@Table[{r,"STD"},{r,0,maxr}]}},
 {{Table[10^k,{k,1,magn}]}\[Transpose], Map[Flatten,Map[If[Length[#]==1,Append[#,NaN],#]&,array,{2}],{1,2}]}
})],{\[FormalCapitalK],1,K}],"Label"->StringTemplate["M/M/1-FQ MC\nK\[LessEqual]`1`, \[Rho]=`2`, \[Mu]=`3`, \[CapitalDelta]t=`4`,\nsteps\[LessEqual]\!\(\*SuperscriptBox[\(10\), \(`5`\)]\), r\[LessEqual]`6`"][K,\[Rho],\[Mu],\[CapitalDelta]t,magn,maxr]]]]//Quiet
