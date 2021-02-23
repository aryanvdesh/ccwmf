(* ::Package:: *)

(* ::Subsection:: *)
(*Function Definition:*)


ImportingData[pdf_]/;FileExistsQ[pdf] := Module[{import, deleteFailed},
	import = List[StringRiffle[List[Quiet[Import[pdf,"Plaintext"]]]]]]
	
ImportingData[str_]/;ListQ[str] := Module[{lstdata, deleteFailed},
	lstdata = str]

ImportingData[pdflink_]/;EqualTo[Length[StringCases[pdflink, RegularExpression["(/pdf/)|(.pdf)"]]]][1]:= Module[{pdfdata, deleteFailed},
	pdfdata = List[Import[pdflink, "Plaintext"]]]
	
ImportingData[url_]/;!FailureQ[URLRead[url]] && Length[StringCases[url, RegularExpression["(/pdf/)|(.pdf)"]]]!=1 := Module[{importdata, paperlinks, data, deleteFailed},
	importdata = Import[url,{"HTML","XMLObject"}];
	paperlinks = Cases[importdata, XMLElement["link", {"title"->"pdf", "href"-> link_, ___, ___},_]:> link, Infinity];
	data = Quiet[Import[#,"Plaintext"]&/@ paperlinks];
	deleteFailed = DeleteCases[data, $Failed]]
	
arXivData[query_, maxitems_] := Module[{arXiv, articles, urls, initialurls, finalurls, urldata, deleteFail},
	arXiv = ServiceConnect["ArXiv"];
	articles = arXiv["Search",{"Query" -> query,"MaxItems" -> maxitems}];
	urls = Normal@articles[All,{"URL"}];
	initialurls = Flatten[Values[urls]];
	finalurls = StringReplace[initialurls,"http://arxiv.org/abs/"->"http://arxiv.org/pdf/"];
	urldata = Quiet[Import[#,"Plaintext"]&/@ finalurls];
	deleteFail = DeleteCases[urldata, $Failed]]


RemoveWhiteDigitChar[data_]:= Module[{whiteSpace, digitChar,lowerCase},
	whiteSpace = StringReplace[#, WhitespaceCharacter..-> " "]&/@data;
	digitChar = StringDelete[#, DigitCharacter..]&/@ (StringSplit[#, "."]&/@whiteSpace)//Flatten; 
	lowerCase = List[ToLowerCase[#]&/@digitChar]]

RemoveStopWords[data_]:= Module[{ss, regexd, ssif, deleteStopwords},
	ss = StringSplit[#, " "]&/@data;
	regexd = StringCases[#,RegularExpression["[a-zA-Z]+"]]&/@ss//Flatten;
	ssif = Flatten[DeleteCases[If[StringLength[#]>=3, List[#]]&/@regexd, Null]];
	deleteStopwords= List[StringRiffle[DeleteStopwords[#]&/@ssif]];
	WordCounts[StringRiffle[deleteStopwords]]//Dataset]


ReGeX[data_,word_,step_]:= Module[{regx, pos, ifs, printS, regxd},
	regx = StringCases[#, RegularExpression["\\b"<> ToLowerCase[ToString[word]] <>"(s?)\\b"]]&/@data;
	pos = Position[#, Except@{}, 1, Heads->False]&/@ regx//Flatten;
	ifs = If[# > step,
		List[Subtract[#, step],Plus[#, step]],
		If[# <= step, List[Plus[#, step]]],
		If[# >= Length[data//Flatten],
		List[Subtract[#, step]]]]&/@ pos//Flatten;
	printS = Flatten[data][[#]]&/@ifs//Flatten;
	regxd = StringDelete[#,RegularExpression["\\b"<> ToLowerCase[ToString[word]] <>"(s?)\\b"]]&/@printS//Flatten]


ConcordanceWords[source_, searchterm_, steps_] := Module[{filedata, cleaneddata, searchword, results},
	filedata = Quiet[ImportingData[source]];
	cleaneddata = RemoveWhiteDigitChar[filedata];
	searchword = ReGeX[cleaneddata, searchterm, steps];
	results = RemoveStopWords[searchword]]

ConcordanceWords[source_, searchterm_]:=
  ConcordanceWords[source,searchterm, 3]
  
ConcordanceWords[arxivquery_, items_, searchterm_, steps_] := Module[{paperdata, processeddata, regexsearch, result},
	paperdata = Quiet[arXivData[arxivquery, items]];
	processeddata = RemoveWhiteDigitChar[paperdata];
	regexsearch = ReGeX[processeddata, searchterm, steps];
	result = RemoveStopWords[regexsearch]]


(* ::Subsection:: *)
(*Examples:*)


(* ::Input:: *)
(*ConcordanceWords[List[ExampleData[{"Text", "AliceInWonderland"}]],"king"]*)


(* ::Input:: *)
(*ConcordanceWords["Mathematics", 10, "Graph", 3]*)


(* ::Input:: *)
(*ConcordanceWords["http://export.arxiv.org/api/query?search_query=all:gravitation&start=0&max_results=2","gravity"]*)
