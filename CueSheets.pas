unit CueSheets;

interface

	function MakeCueSheet(const LabelsFileName: string): boolean;

implementation

uses
	SysUtils,
	DateUtils,
	Math,
	CueSheetsAndLabels;

var
	AudioFileName: string;

// -------------------------------------------------------------

procedure SortLabelsByIndex(var Labels: TLabels);
	var
		TempLabel: TLabel;
		i, j: Cardinal;
begin
	for i := 0 to Length(Labels)-2 do
		for j := i+1 to Length(Labels)-1 do
			if Labels[i].tStartIndex > Labels[j].tStartIndex then
				begin
					TempLabel := Labels[i];
					Labels[i] := Labels[j];
					Labels[j] := TempLabel;
				end;
end; // procedure SortLabelsByIndex(var Labels: TLabels); // }

// -------------------------------------------------------------

procedure SortCueSheetByIndex(var CueSheet: TCueSheet);
	var
		TempCueItem: TCueItem;
		i, j: Cardinal;
begin
	for i := 0 to Length(CueSheet)-2 do
		for j := i+1 to Length(CueSheet)-1 do
			if CueSheet[i].tIndex > CueSheet[j].tIndex then
				begin
					TempCueItem := CueSheet[i];
					CueSheet[i] := CueSheet[j];
					CueSheet[j] := TempCueItem;
				end;
end; // procedure SortCueSheetByIndex(var CueSheet: TCueSheets); // }

// -------------------------------------------------------------

function Label2Time(const LabelTime: string): TDateTime;
	var
		fSeconds: Double;
		T: TDateTime;
begin
	if TryStrToFloat(LabelTime, fSeconds) then
		T := fSeconds / SecsPerDay
	else
		T := -1;
	Result := T;
end; // Label2Time(const LabelTime: string): TDateTime; // }

// -------------------------------------------------------------

{ function Time2Cue(const T: TDateTime): string;
	var
		fSplitSecond: Double;
		iMinute, iSecond, iFrame: Integer;
		sMinute, sSecond, sFrame: string;
begin
	iMinute := Trunc(T * MinsPerDay);
	iSecond := SecondOf(T);
	fSplitSecond := Frac(T * SecsPerDay);
	iFrame  := Round(fSplitSecond * 75);
	//iFrame  := Round(MilliSecondOf(T) / 1000 * 75); //ToDo: 0.000s <> 0.000499s

	if iFrame >= 75 then
		begin
			iSecond := iSecond + 1;
			iFrame := 0;
			if iSecond >= 60 then
				begin
					iMinute := iMinute + 1;
					iSecond := 0;
				end;
		end;

	sMinute := IntToStr(iMinute); if iMinute <= 9 then sMinute := '0' + sMinute;
	sSecond := IntToStr(iSecond); if iSecond <= 9 then sSecond := '0' + sSecond;
	sFrame  := IntToStr(iFrame);  if iFrame  <= 9 then sFrame  := '0' + sFrame;

	Result := sMinute + ':' + sSecond + ':' + sFrame;
end; // function Time2Cue(const T: TDateTime): string; // }

// -------------------------------------------------------------

function CueTime(const LabelTime: string): string;
	var
		fSeconds: Double;
		iMinute, iSecond, iFrame: Integer;
		sMinute, sSecond, sFrame: string;
begin
	if TryStrToFloat(LabelTime, fSeconds) then
		begin
			iMinute := Trunc(fSeconds / 60);
			iSecond := Trunc(fSeconds - (iMinute * 60));
			iFrame  := Round(Frac(fSeconds) * 75);

			if iFrame >= 75 then
				begin
					iSecond := iSecond + 1;
					iFrame := 0;
					if iSecond >= 60 then
						begin
							iMinute := iMinute + 1;
							iSecond := 0;
						end;
				end;

			sMinute := IntToStr(iMinute); if iMinute <= 9 then sMinute := '0' + sMinute;
			sSecond := IntToStr(iSecond); if iSecond <= 9 then sSecond := '0' + sSecond;
			sFrame  := IntToStr(iFrame);  if iFrame  <= 9 then sFrame  := '0' + sFrame;

			Result := sMinute + ':' + sSecond + ':' + sFrame;
		end
	else
		Result := '';
end; // function CueTime(const LabelTime: string): string; // }

// -------------------------------------------------------------

function LoadLabels(const LabelsFileName: string): TLabels;
	var
		LabelsFile: TextFile;
		LabelLine: string;
		Labels: TLabels;
		Label1: TLabel;
		i, P: Cardinal;
		S: string;
		T: TDateTime;
begin
	i := 0;
	SetLength(Labels, i);
	if FileExists(LabelsFileName) then
		begin
			AssignFile(LabelsFile, LabelsFileName);
			Reset(LabelsFile);
			if IOResult = 0 then
				begin
					while not EOF(LabelsFile) do
						begin
							ReadLn(LabelsFile, LabelLine);
							P := Pos(LabelSeparator, LabelLine);
							if P > 0 then
								begin
									S := Copy(LabelLine, 1, P - 1);
									T := Label2Time(S);
									if T >= 0 then
										begin
											Label1.tStartIndex := T;
											Label1.sStartIndex := S;
											Delete(LabelLine, 1, P);

											P := Pos(LabelSeparator, LabelLine);
											if P > 0 then
												begin
													S := Copy(LabelLine, 1, P - 1);
													T := Label2Time(S);
													if T >= 0 then
														begin
															Label1.tEndIndex := T;
															Label1.sEndIndex := S;
															Delete(LabelLine, 1, P);
															Label1.sTitle := LabelLine;
														end
													else
														begin
															Label1.sEndIndex := Label1.sStartIndex;
															Label1.tEndIndex := Label1.tStartIndex;
															Label1.sTitle := S;
															// Delete(LabelLine, 1, P);
															// Label1.sTitle := LabelLine;
														end;
												end
											else
												begin
													T := Label2Time(LabelLine);
													if T >= 0 then
														begin
															Label1.tEndIndex := T;
															Label1.sEndIndex := LabelLine;
															Label1.sTitle := sEmpty;
														end
													else
														begin
															Label1.tEndIndex := Label1.tStartIndex;
															Label1.sEndIndex := Label1.sStartIndex;
															Label1.sTitle := LabelLine;
														end;
												end;
										end;
								end
							else
								begin
									T := Label2Time(LabelLine);
									if T >= 0 then
										begin
											Label1.tStartIndex := T;
											Label1.tEndIndex   := T;
											Label1.sStartIndex := LabelLine;
											Label1.sEndIndex   := LabelLine;
											Label1.sTitle      := sEmpty; // CueTime(Label1.sStartIndex); //ToDo: HH:MM:SS:ZZZZZZ
										end;
								end;
							if Label1.sStartIndex <> '' then
								begin
									Label1.sFile := AudioFileName;
									i := i + 1;
									SetLength(Labels, i);
									Labels[i-1] := Label1;
								end;
						end;
					CloseFile(LabelsFile);

					SortLabelsByIndex(Labels);
				end;
		end;
	Result := Labels;
end; // function LoadLabels(const LabelsFileName: string): TLabels; // }

// -------------------------------------------------------------

function Labels2CueSheet(const Labels: TLabels): TCueSheet;
	var
		CueSheet: TCueSheet;
		CueTemp:  TCueSheet;
		i, j: Cardinal;
begin
	j := 0;
	SetLength(CueSheet, j);
	SetLength(CueTemp, 2);
	for i := 0 to Length(Labels) - 1 do
		begin
			CueTemp[iStart].sIndex := CueTime(Labels[i].sStartIndex); // Time2Cue(Labels[i].tStartIndex);
			CueTemp[iEnd]  .sIndex := CueTime(Labels[i].sEndIndex);   // Time2Cue(Labels[i].tEndIndex);
			CueTemp[iStart].tIndex := Labels[i].tStartIndex;
			CueTemp[iEnd]  .tIndex := Labels[i].tEndIndex;
			CueTemp[iStart].sFile  := Labels[i].sFile;
			CueTemp[iEnd]  .sFile  := Labels[i].sFile;
			CueTemp[iStart].iType  := iStart;
			CueTemp[iEnd]  .iType  := iEnd;

			if CueTemp[iStart].tIndex = CueTemp[iEnd].tIndex then
				begin
					// CueTemp[iEnd].iType := CueTemp[iEnd].iType + iSkip;
					if Labels[i].sTitle <> sEmpty then
						CueTemp[iStart].sTitle := Labels[i].sTitle
						// CueTemp[iEnd]  .sTitle := Labels[i].sTitle + ' - end';
					else
						CueTemp[iStart].sTitle := Labels[i].sTitle; // CueTemp[iStart].sIndex; //ToDo: HH:MM:SS:ZZZZZZ
						// CueTemp[iEnd]  .sTitle := CueTemp[iStart].sTitle;
					j := j + 1; // j := j + 2;
					SetLength(CueSheet, j);
					CueSheet[j-1] := CueTemp[iStart];
					// CueSheet[j-2] := CueTemp[iStart];
					// CueSheet[j-1] := CueTemp[iEnd];
				end
			else
				begin
					if Labels[i].sTitle <> sEmpty then
						begin
							CueTemp[iStart].sTitle := Labels[i].sTitle; // + ' - start';
							CueTemp[iEnd]  .sTitle := Labels[i].sTitle + ' - end';
						end
					else
						begin
							CueTemp[iStart].sTitle := Labels[i].sTitle; // CueTemp[iStart].sIndex; //ToDo: HH:MM:SS:ZZZZZZ
							CueTemp[iEnd]  .sTitle := Labels[i].sTitle; // CueTemp[iEnd]  .sIndex; //ToDo: HH:MM:SS:ZZZZZZ
						end;
					if i < (Length(Labels) - 1) then
						if Labels[i].tEndIndex = Labels[i+1].tStartIndex then
							CueTemp[iEnd].iType := CueTemp[iEnd].iType + iSkip;
					j := j + 2;
					SetLength(CueSheet, j);
					CueSheet[j-2] := CueTemp[iStart];
					CueSheet[j-1] := CueTemp[iEnd];
				end;
		end;

		SortCueSheetByIndex(CueSheet);

		for i := 0 to Length(CueSheet)-2 do
			if CueSheet[i].iType < iSkip then
				for j := i+1 to Length(CueSheet)-1 do
					if CueSheet[j].iType < iSkip then
						if CueSheet[i].tIndex = CueSheet[j].tIndex then
							begin
								if CueSheet[i].iType = iEnd then
									if CueSheet[j].iType = iEnd then //ToDo: Is it nessesary to reduce equal end indexes?
										CueSheet[j].iType := CueSheet[j].iType + iSkip
									else
										CueSheet[i].iType := CueSheet[i].iType + iSkip
								else //ToDo: Is it nessesary to reduce equal start indexes?
									CueSheet[j].iType := CueSheet[j].iType + iSkip;
							end;

	Result := CueSheet;
end; // function Labels2CueSheet(const Labels: TLabels): TCueSheets; // }

// -------------------------------------------------------------

function MakeCueSheet(const LabelsFileName: string): boolean;
	var
		Labels: TLabels;
		CueSheet: TCueSheet;
		AudioFileType: string;
		CueFileName, CueFileNameBak: string;
		CueFile: TextFile;
		i, j, k, l: Cardinal;
		N: string;
		BackupSuffix: string;
begin
	Result := false;

	Labels := LoadLabels(LabelsFileName);
	if Length(Labels) > 0 then
		begin
			CueSheet := Labels2CueSheet(Labels);

			AudioFileName := ExtractFileName(LabelsFileName); // file.mp3.txt
			AudioFileType := ExtractFileExt(AudioFileName); // .txt
			AudioFileName := Copy(AudioFileName, 1, length(AudioFileName) - (length(AudioFileType))); // file.mp3
			AudioFileType := ExtractFileExt(AudioFileName); // .mp3
			AudioFileType := UpperCase(AudioFileType);
			if AudioFileType = '.MP3' then
				AudioFileType := 'MP3'
			else
				if (AudioFileType = '.AIF') or (AudioFileType = '.AIFF') then
					AudioFileType := 'AIFF'
				else //ToDo: else 'BINARY'?
					AudioFileType := 'WAVE';

			CueFileName := AudioFileName + '.cue'; // file.mp3.cue
			if FileExists(CueFileName) then
				begin
					// DateTimeToString(BackupSuffix, 'yyyymmdd-hhnnss-zzz', FileDateToDateTime(FileAge(CueFileName)));
					l := 0;
					repeat
						Sleep(l);
						DateTimeToString(BackupSuffix, 'yyyymmdd-hhnnss-zzz', Now);
						CueFileNameBak := CueFileName + '.' + BackupSuffix + '.bak'; // file.mp3.cue.yyyymmdd-hhmmss-zzz.bak
						l:= Random(40) + 10;
					until not FileExists(CueFileNameBak);
					RenameFile(CueFileName, CueFileNameBak);
				end;

			AssignFile(CueFile, CueFileName);
			Rewrite(CueFile);
			if IOResult = 0 then
				begin
					WriteLn(CueFile, 'FILE "'  + AudioFileName + '" ' + AudioFileType);
					k := 0;
					l := Length(CueSheet);
					for i := 0 to l - 1 do
						if CueSheet[i].iType < iSkip then
							k := k + 1;

					j := 0;
					for i := 0 to l - 1 do
						if CueSheet[i].iType < iSkip then
							begin
								j := j + 1;
								N := IntToStr(j);
								if (j < 10)   and (k >= 10)   then N := '0' + N;
								if (j < 100)  and (k >= 100)  then N := '0' + N;
								if (j < 1000) and (k >= 1000) then N := '0' + N; //ToDo: j > 9999
								WriteLn(CueFile, '  TRACK '  + N + ' AUDIO');
								WriteLn(CueFile, '    TITLE ' + CueSheetQuote + CueSheet[i].sTitle + CueSheetQuote);
								WriteLn(CueFile, '    INDEX 01 ' + CueSheet[i].sIndex);
							end;
					CloseFile(CueFile);
					Result := true;
				end;
		end;
end; // function MakeCueSheet(const LabelsFileName: string): boolean; // }

// -------------------------------------------------------------

end.

