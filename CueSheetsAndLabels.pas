unit CueSheetsAndLabels;

interface

const
	sEmpty = '';
	sTab   = #09;
	sSpace = ' ';
	sQuote = '"';
	sCrLf  = #13#10;

	LabelSeparator = sTab;      //ToDo: There might be spaces
	CueSheetSeparator = sSpace; //ToDo: There might be tabs
	CueSheetQuote = sQuote;     //ToDo: There might be '' quotes or none

	iStart = 0;
	iEnd = 1;
	iSkip = 2;
	iTitle = 2;

type

	TLabel = record
		tStartIndex: TDateTime;
		tEndIndex: TDateTime;
		sStartIndex: string;
		sEndIndex: string;
		sTitle: string;
		sFile: string;
	end;

	TCueItem = record
		iType: Byte;
		tIndex: TDateTime;
		sIndex: string;
		sTitle: string;
		sFile: string;
	end;

	TLabels = array of TLabel;
	TCueSheet = array of TCueItem;

implementation

// -------------------------------------------------------------

end.

