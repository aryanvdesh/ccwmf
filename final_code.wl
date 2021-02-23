(* ::Package:: *)

(* ::Section:: *)
Importing Data


(* ::Subsection:: *)
Using ServiceConnect:


(* ::Input:: *)
arXiv = ServiceConnect["ArXiv"];
articles = arXiv["Search",{"Query" -> "Circles","MaxItems" -> 1}];
a = Normal@articles[All,{"URL"}];
list = Flatten[Values[a]];
iml = StringReplace[list,"http://arxiv.org/abs/"->"http://arxiv.org/pdf/"];
ipl = Quiet[Import[#,"Plaintext"]&/@ iml];


(* ::Subsection:: *)
Scraping Data:


(* ::Input:: *)
url  = "http://export.arxiv.org/api/query?search_query=all:circle&start=0&max_results=10";
import = Import[url,{"HTML","XMLObject"}];
paperlinks =Cases[import, XMLElement["link", {"title"->"pdf", "href"-> link_, ___, ___},_]:> link, Infinity];
ptpl = Quiet[Import[#,"Plaintext"]&/@ paperlinks];
ss0 = DeleteCases[ptpl,$Failed];


(* ::Section:: *)
Processing Data:


(* ::Text:: *)
Removing whitespace, digit characters, and converting to lowercase


(* ::Input:: *)
ss01 = List[StringRiffle[ss0]];
ss = StringSplit[#, "."]&/@ss01;
ss1 = StringReplace[#,WhitespaceCharacter..->" "]&/@ss;
ss2 = StringDelete[#,DigitCharacter..]&/@ss1;
ss25 = ToLowerCase[#]&/@ss2;


(* ::Text:: *)
Searching term using Regular Expressions, finding position of the word in the data, deleting stopwords, and the searchterm


(* ::Input:: *)
regx = StringCases[#,RegularExpression["\\bcircles?\\b"]]&/@ss25;
pos = Position[#,Except@{},1,Heads->False]&/@regx//Flatten;
ifs = If[# > 3,
List[Sequence[#-3,#+3]]
, If[# <= 3, List[Sequence[#+3]]]
,If[#>=Length[ss25//Flatten],
List[Sequence[# - 3]]]]&/@pos//Flatten;
ss3 = DeleteStopwords[#]&/@ss25//Flatten;
ss4 = StringReplace[#, WhitespaceCharacter..->" "]&/@ss3;
ss5 = Quiet[ss4[[#]]&/@ifs//Flatten];
ss6 = Quiet[StringDelete[#,RegularExpression["\\bcircles?\\b"]]&/@ss5];


(* ::Input:: *)
ss7 = Quiet[StringRiffle[StringCases[#,RegularExpression["[a-zA-Z]+"]]," "]&/@ss6//Flatten];
ss8 = Select[#==RemoveDiacritics[#]&]@ss7;
ss9 = StringSplit[#, " "]&/@ss8//Flatten;
ss10 = {};
If[StringLength[#]>1,
	AppendTo[ss10,#]]&/@ss9;
ss11 = List[StringRiffle[ss10]];


(* ::Subsection:: *)
Final Results:


(* ::Input:: *)
WordCounts[StringRiffle[ss11]]//Dataset
