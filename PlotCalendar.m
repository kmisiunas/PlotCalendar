(* Mathematica Package *)

(* :Title: PlotCalendar *)
(* :Context: PlotCalendar` *)
(* :Author: kmisiunas *)
(* :Date: 2016-12-29 *)

(* :Package Version: 1.0 *)
(* :Versions:
    v1.0 (2016-12-29) Initial release based on
                      http://mathematica.stackexchange.com/questions/61094/calendar-view-outline

*)
(* :Mathematica Version: 11.0.2 (tested for MAC)*)
(* :Copyright: Use freely *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["PlotCalendar`"]
(* Exported symbols added here with SymbolName::usage *)

PlotCalendar::usage =
  "PlotCalendar[ timeSeries_, options ] plots time series as calendar heat map
  Options:
  \"Years\" -> All   --- plots only specified years
  DataRange -> Automatic   --- can trim data to a different range (All, Automatic or {min, max})
  ColorFunction -> \"TemperatureMap\"
  PlotSize -> 800
  \"Tooltip\" -> True   --- can hide Tooltips
";



Options[PlotCalendar] = {
  "Years" -> All,
  DataRange -> Automatic,
  ColorFunction -> "TemperatureMap",
  ImageSize -> 800,
  "Tooltip"-> True
};



Begin["`Private`"]


PlotCalendar[ timeSeries_, opts: OptionsPattern[]] := Module[
  {data, plotData, years, dataRange, filterOutliers},
  data =  Normal[timeSeries];
  filterOutliers[data_] :=
      Select[data, Function[x, - 3*#[[2]]< x - #[[1]]< 3*#[[2]] ]] &@{ Mean[data], StandardDeviation[data]} ;
  dataRange = Switch[ OptionValue[DataRange] ,
    Automatic ,  MinMax@filterOutliers[data[[All,2]]],
    All , MinMax[data[[All,2]]] ,
    _ , OptionValue[DataRange]
  ];
  plotData = Switch[ OptionValue[ColorFunction],
    _String, {#1, ColorData[ OptionValue[ColorFunction] ][Rescale[#2, dataRange]], #2} &@@@ data  ,
    _, {#1, OptionValue[ColorFunction][#2], #2} &@@@ data
  ];
  years = If[OptionValue["Years"] === All, DeleteDuplicates[DateValue[#, "Year"]&/@data[[All,1]]], OptionValue["Years"]];

  plotYear[plotData, years, opts ]
];





(* === Helper Functions ==== *)

dayindex = {Monday->1,Tuesday->2,Wednesday->3,Thursday->4,Friday->5,Saturday->6,Sunday->7};

monthstart[month_,year_] := DayName[{year,month,1}]/.dayindex;

dayspermonth[month_,year_] := DayCount[{year,month,1},DatePlus[{year,month,1},{{1,"Month"}}]];

monthlayout[month_,year_] :=
    Partition[Range[dayspermonth[month,year]],7,7,{monthstart[month,year],1},""];

startingweek[month_,year_] :=
    Ceiling[(DayCount[{year,1,1},{year,month,1}]+monthstart[1,year])/7.0];

monthgrid[month_,year_] := Module[
  {cd,cds,p1,p2,p3,p4,p5,p6,p7,p8,shift},
  shift=startingweek[month,year];
  cd=monthlayout[month,year];
  cds=Position[cd,x_/;x!=""];
  cds[[All,2]]=7-cds[[All,2]];
  cds[[All,1]]=cds[[All,1]]-1+shift;
  p1={shift,cds[[1,2]]+1};
  p2={shift,0};
  p3={Max[cds[[All,1]]],0};
  p4=Last[cds];
  p5=Last[cds]+{1,0};
  p6={p5[[1]],7};
  p7={shift+1,7};
  p8=p1+{1,0};
  Graphics[{FaceForm[], EdgeForm[Gray], Rectangle[#]&/@cds, Thick, Line[{p1,p2,p3,p4,p5,p6,p7,p8,p1}]}]
];

monthNoDataIndicator[month_,year_] := Module[
  {cd,cds,shift},
  shift=startingweek[month,year];
  cd=monthlayout[month,year];
  cds=Position[cd,x_/;x!=""];
  cds[[All,2]]=7-cds[[All,2]];
  cds[[All,1]]=cds[[All,1]]-1+shift;
  Graphics[{Red, Line[{#, #+{1,1}}]&/@cds}]
];

yeargrid[year_,opts:OptionsPattern[Show]] := Show[Table[monthgrid[i,year],{i,1,12}],opts];



(* dataRaw must contain: {DateObject, Color, Value for Tooltip} *)
monthHeatMap[dataRaw_, {year_, month_}, opts: OptionsPattern[PlotCalendar]]:= Module[
  {data, rectposition, colorBlocks},
  (* select data that is in that month *)
  data = Select[ dataRaw, #[[1,1,1]] === year && #[[1,1,2]] === month &];
  (*calculate xy postion for the each date*)
  rectposition[{y_,m_,d_}]:=Module[
    {lx,ly},
    lx=Ceiling[(DayCount[{y,1,1},{y,m,d}]+monthstart[1,y])/7.0];
    ly=7-DayName[{y,m,d}]/.dayindex;
    {lx,ly}
  ];
  colorBlocks = If[ OptionValue["Tooltip"],
  (* with tooltip *)
    Table[
      {FaceForm[data[[i,2]]], Tooltip[ Rectangle[rectposition[data[[i,1,1]]]], Column[{data[[i,1]], data[[i,3]] }]]},
      {i,1,Length[data]}
    ] ,
  (* without tooltip *)
    Table[
      {FaceForm[data[[i,2]]], Rectangle[rectposition[data[[i,1,1]]]]},
      {i,1,Length[data]}
    ]
  ];
  Show[
    monthNoDataIndicator[month,year],
    Graphics[{EdgeForm[],colorBlocks}],
    monthgrid[month,year]
  ]
]

plotYear[dateAndColor_, year_, opts:OptionsPattern[PlotCalendar]] := Show[
  Graphics[{Rotate[Text[Style[ToString[year],Medium,Black, 20],{-3.5,3.5}], 90 Degree]}],
  Graphics[{Text[Style[#[[1]],Medium,Black],{-1,7-#[[2]]+0.5}]}&/@{StringTake[ToString@#1,2]->#2}&@@@dayindex],
  monthHeatMap[dateAndColor, {year, #}, opts] &/@ Range[1,12],
  ImageSize-> OptionValue[ImageSize]
];

plotYear[dateAndColor_, years_List, opts:OptionsPattern[PlotCalendar]]:=Column[
  plotYear[dateAndColor, #, opts] &/@years,
  Alignment->Full,
  Spacings -> 0
];



End[] (* `Private` *)

EndPackage[]