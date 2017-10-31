unit Labels;

interface

	function MakeLabels(const CueSheetFileName: string): boolean;

implementation

uses
	SysUtils,
	DateUtils,
	Math,
	CueSheetsAndLabels;

var
	AudioFileName: string;

// -------------------------------------------------------------

procedure SortCueSheetByFile(var CueSheet: TCueSheet);
	var
		TempCueItem: TCueItem;
		i, j: Cardinal;
begin
	for i := 0 to Length(CueSheet)-2 do
		for j := i+1 to Length(CueSheet)-1 do
			if CueSheet[i].sFile > CueSheet[j].sFile then
				begin
					TempCueItem := CueSheet[i];
					CueSheet[i] := CueSheet[j];
					CueSheet[j] := TempCueItem;
				end;
end; // procedure SortCueSheetByFile(var CueSheet: TCueSheets); // }

// -------------------------------------------------------------

function Cue2Time(const CueTime: string): TDateTime;
	var
		iMinute, iSecond, iFrame: Integer;
		S: string;
		P: Cardinal;
		T: TDateTime;
begin
	P := Pos(':', CueTime);
	S := Copy(CueTime, 1, P - 1);
	if TryStrToInt(S, iMinute) then
		begin
			S := Copy(CueTime, P + 1, Length(CueTime) - P + 1);
			P := Pos(':', S);
			if TryStrToInt(Copy(S, 1, P - 1), iSecond) then
				begin
					S := Copy(S, P + 1, Length(S) - P + 1);
					if TryStrToInt(S, iFrame) then
						begin
							T := ((iMinute * 60) + iSecond + (iFrame / 75)) / SecsPerDay;
							Result := T;
						end
					else
						Result := -1;
				end
			else
				Result := -1;
		end
	else
		Result := -1;
end; // function Cue2Time(const CueTime: string): TDateTime; // }

// -------------------------------------------------------------

function Time2Label(const T: TDateTime): string;
	var
		fSeconds: Double;
begin
	fSeconds := T * SecsPerDay;
	Result := FormatFloat('0.000000', fSeconds);
end; // function Time2Label(const T: TDateTime): string; // }

// -------------------------------------------------------------

{ function LabelTime(const CueTime: string): string;
	var
		iMinute, iSecond, iFrame: Integer;
		S: string;
		P: Cardinal;
		fSeconds: Double;
begin
	P := Pos(':', CueTime);
	S := Copy(CueTime, 1, P - 1);
	if TryStrToInt(S, iMinute) then
		begin
			S := Copy(CueTime, P + 1, Length(CueTime) - P + 1);
			P := Pos(':', S);
			if TryStrToInt(Copy(S, 1, P - 1), iSecond) then
				begin
					S := Copy(S, P + 1, Length(S) - P + 1);
					if TryStrToInt(S, iFrame) then
						begin
							fSeconds := iMinute * 60 + iSecond + iFrame / 75;
							S := FormatFloat('0.000000', fSeconds);
							Result := S;
						end
					else
						Result := '';
				end
			else
				Result := '';
		end
	else
		Result := '';
end; // function LabelTime(const CueTime: string): string; // }

// -------------------------------------------------------------

function LoadCueSheet(const CueSheetFileName: string): TCueSheet;

	const
		iFile  = 0;
		iTrack = 1;
		iTitle = 2;
		iIndex = 3;
		iOther = 4;

	function TypeOfCueSheetLine(const CueSheetLine: string): Byte;
		const
			sFile  = 'FILE';
			sTrack = 'TRACK';
			sTitle = 'TITLE';
			sIndex = 'INDEX';
		var
			S: string;
			P: Cardinal;
	begin
		P := Pos(CueSheetSeparator, CueSheetLine);
		if P > 4 then
			begin
				S := Copy(CueSheetLine, 1, P - 1);
				S := UpperCase(S);
				if S = sFile then
					Result := iFile
				else
					if S = sTitle then
						Result := iTitle
					else
						if S = sIndex then
							Result := iIndex
						else
							if S = sTrack then
								Result := iTrack
							else
								Result := iOther;
			end
		else
			Result := iOther;
	end;

	var
		CueSheetFile: TextFile;
		CueSheetLine: string;
		CueSheet: TCueSheet;
		CueItem: TCueItem;
		LabelIndexNo, CueIndexNo, N: Cardinal;
		TopTitle, BottomTitle: string;
		FirstIndexHasTopTitle,
		FirstIndexHasBottomTitle: Boolean;
		S: string;
		T: TDateTime;

	procedure AppendCueSheet();
	begin
		LabelIndexNo := LabelIndexNo + 1;
		SetLength(CueSheet, LabelIndexNo);
		CueSheet[LabelIndexNo-1] := CueItem;
		if (CueIndexNo = 1) then
			if FirstIndexHasTopTitle then
				CueSheet[LabelIndexNo-1].sTitle := TopTitle
			else
				if FirstIndexHasBottomTitle then
					CueSheet[LabelIndexNo-1].sTitle := BottomTitle;
		CueItem.sIndex := sEmpty;
		CueItem.tIndex := 0;
	end;

begin
	AudioFileName := ExtractFileName(CueSheetFileName); // file.mp3.cue
	AudioFileName := Copy(AudioFileName, 1, length(AudioFileName) - (length(ExtractFileExt(AudioFileName)))); // file.mp3

	LabelIndexNo := 0;
	SetLength(CueSheet, LabelIndexNo);
	if FileExists(CueSheetFileName) then
		begin
			AssignFile(CueSheetFile, CueSheetFileName);
			Reset(CueSheetFile);
			if IOResult = 0 then
				begin
					CueItem.iType  := iStart;
					CueItem.sFile  := AudioFileName;
					CueItem.sTitle := sEmpty;
					CueItem.sIndex := sEmpty;
					CueItem.tIndex := 0;
					CueIndexNo := 0;
					TopTitle := '';
					BottomTitle := '';
					FirstIndexHasTopTitle := False;
					FirstIndexHasBottomTitle := False;
					while not EOF(CueSheetFile) do
						begin
							ReadLn(CueSheetFile, CueSheetLine);
							CueSheetLine := Trim(CueSheetLine);
							N := TypeOfCueSheetLine(CueSheetLine);
							case N of
								iFile:
									begin
										if (CueIndexNo = 1) and (CueItem.sIndex <> sEmpty) then
											AppendCueSheet();

										CueItem.sTitle := sEmpty;
										CueIndexNo := 0;
										TopTitle := '';
										BottomTitle := '';
										FirstIndexHasTopTitle := False;
										FirstIndexHasBottomTitle := False;

										N := Pos(CueSheetQuote, CueSheetLine);
										CueItem.sFile := Copy(CueSheetLine, N + 1, Length(CueSheetLine) - N + 1);
										N := Pos(CueSheetQuote, CueItem.sFile);
										CueItem.sFile := Copy(CueItem.sFile, 1, N - 1);
									end;
								iTrack:
									begin
										if (CueIndexNo = 1) and (CueItem.sIndex <> sEmpty) then
											AppendCueSheet();

										CueItem.sTitle := sEmpty;
										CueIndexNo := 0;
										TopTitle := '';
										BottomTitle := '';
										FirstIndexHasTopTitle := False;
										FirstIndexHasBottomTitle := False;
									end;
								iTitle:
									begin
										N := Pos(CueSheetQuote, CueSheetLine);
										CueItem.sTitle := Copy(CueSheetLine, N + 1, Length(CueSheetLine) - N + 1);
										N := Pos(CueSheetQuote, CueItem.sTitle);
										CueItem.sTitle := Copy(CueItem.sTitle, 1, N - 1);

										if CueIndexNo = 0 then
											begin
												FirstIndexHasTopTitle := True;
												TopTitle := CueItem.sTitle;
											end
										else
											if (CueIndexNo = 1) and (not FirstIndexHasTopTitle) and (not FirstIndexHasBottomTitle) then
												begin
													FirstIndexHasBottomTitle := True;
													BottomTitle := CueItem.sTitle;
												end;

									end;
								iIndex:
									begin
										if CueIndexNo = 1 then
											begin
												BottomTitle := '';
												AppendCueSheet();
											end;

										N := Pos(CueSheetSeparator, CueSheetLine);
										S := Copy(CueSheetLine, N + 1, Length(CueSheetLine) - N + 1);
										S := Trim(S);
										N := Pos(CueSheetSeparator, S);
										S := Copy(S, N + 1, Length(S) - N + 1);
										S := Trim(S);

										T := Cue2Time(S);
										if T >=0 then
											begin
												CueIndexNo := CueIndexNo + 1;

												CueItem.tIndex := T;
												CueItem.sIndex := S;

												if CueIndexNo > 1 then
													AppendCueSheet();
											end;
									end;
							end;
						end;
						if (CueIndexNo = 1) and (CueItem.sIndex <> sEmpty) then
							AppendCueSheet();
						SortCueSheetByFile(CueSheet);
						CloseFile(CueSheetFile);
				end;
		end;
	Result := CueSheet;
end; // function LoadCueSheet(const CueSheetFileName: string): TCueSheet; // }

// -------------------------------------------------------------

function CueSheet2Labels(const CueSheet: TCueSheet): TLabels;
	var
		Labels: TLabels;
		i: Cardinal;
begin
	for i := 0 to Length(CueSheet) - 1 do
		begin
			SetLength(Labels, i+1);
			Labels[i].sStartIndex := Time2Label(CueSheet[i].tIndex);
			Labels[i].tStartIndex := CueSheet[i].tIndex;
			Labels[i].sEndIndex   := Labels[i].sStartIndex;
			Labels[i].tEndIndex   := Labels[i].tStartIndex;
			if CueSheet[i].sTitle <> sEmpty then
				Labels[i].sTitle := CueSheet[i].sTitle
			else
				Labels[i].sTitle := CueSheet[i].sTitle; // CueSheet[i].sIndex; //ToDo: HH:MM:SS:ZZZZZZ
			Labels[i].sFile := CueSheet[i].sFile;
		end;

	Result := Labels;
end; // function CueSheet2Labels(const CueSheet: TCueSheet): TLabels; // }

// -------------------------------------------------------------

function MakeLabels(const CueSheetFileName: string): boolean;
	var
		CueSheet: TCueSheet;
		Labels: TLabels;
		LabelsFileName, LabelsFileNameBak: string;
		LabelsFile: TextFile;
		i, t: Cardinal;
		BackupSuffix: string;
begin
	Result := true;

	CueSheet := LoadCueSheet(CueSheetFileName);
	if Length(CueSheet) > 0 then
		begin
			Labels := CueSheet2Labels(CueSheet);
			LabelsFileName := sEmpty;
			for i := 0 to Length(Labels) - 1 do
				begin
					if LabelsFileName <> (Labels[i].sFile + '.txt') then
						begin
							if i > 0 then CloseFile(LabelsFile);
							LabelsFileName := Labels[i].sFile + '.txt';
							if FileExists(LabelsFileName) then
								begin
									// DateTimeToString(BackupSuffix, 'yyyymmdd-hhnnss-zzz', FileDateToDateTime(FileAge(LabelsFileName)));
									t := 0;
									repeat
										Sleep(t);
										DateTimeToString(BackupSuffix, 'yyyymmdd-hhnnss-zzz', Now);
										LabelsFileNameBak := LabelsFileName + '.' + BackupSuffix + '.bak'; // file.mp3.txt.yyyymmdd-hhmmss-zzz.bak
										t:= Random(40) + 10;
									until not FileExists(LabelsFileNameBak);
									RenameFile(LabelsFileName, LabelsFileNameBak);
								end;

							AssignFile(LabelsFile, LabelsFileName);
							Rewrite(LabelsFile);
							if IOResult <> 0 then Result := False;
						end;
					WriteLn(LabelsFile, Labels[i].sStartIndex, LabelSeparator, Labels[i].sEndIndex, LabelSeparator, Labels[i].sTitle);
				end;
			CloseFile(LabelsFile);
		end;
end; // function MakeLabels(const CueSheetFileName: string): boolean; // }

// -------------------------------------------------------------

end.

