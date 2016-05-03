unit LabelsAndCueSheets;

interface

	function MakeCueSheet(const LabelsFileName: string): boolean;
	function CueTime(const LabelTime: string): string;

	function MakeLabels(const CueSheetFileName: string): boolean;
	function LabelTime(const CueTime: string): string;

implementation

uses
	SysUtils,
	Math;

const
	LabelSeparator = #09; // Tab

	iIndex0 = 0;
	iIndex1 = 1;
	iTitle  = 2;
	iFile   = 3;

type
	// TLabels      = array of array[0..2] of string; // [Index1, Index2, Title]
	TLabels      = array of array[iIndex0..iFile] of string;
	// TCueSheet    = array of array[0..1] of string; // [Title, Index]
	TCueSheets    = array of array[iIndex1..iFile] of string;

	// // TMultiLabels = array of array[0..2] of string; // [Index, Title, File]
	// TMultiLabels = array of array[iIndex1..iFile] of string;
	// // TCueSheets   = array of array[0..2] of string; // [File, Title, Index]
	// TCueSheets   = array of array[iIndex1..iFile] of string;

// -------------------------------------------------------------

function LoadCueSheet(const CueSheetFileName: string): TCueSheets;

	function GetFileName(S: string): string; //ToDo: dissolve the function?
	begin
		S := Copy(S, Pos('"', S) + 1, Length(S) - Pos('"', S) + 1);
		S := Copy(S, 1, Pos('"', S) - 1);
		Result := S;
	end;

	function GetTitle(S: string): string; //ToDo: dissolve the function?
	begin
		S := Copy(S, Pos('"', S) + 1, Length(S) - Pos('"', S) + 1);
		S := Copy(S, 1, Pos('"', S) - 1);
		Result := S;
	end;

	function GetIndex(S: string): string; //ToDo: dissolve the function?
	begin
		S := Copy(S, Length(S) - 8 + 1, 8);
		Result := S;
	end;

	var
		CueSheetFile: TextFile;
		CueSheetLine, FileName, Title, Index: string;
		i: Cardinal;
		CueSheets: TCueSheets;

begin
	i := 0;
	SetLength(CueSheets, i);
	if FileExists(CueSheetFileName) then
		begin
			AssignFile(CueSheetFile, CueSheetFileName);
			Reset(CueSheetFile);
			if IOResult = 0 then
				begin
					FileName := '';
					Title    := '';
					Index    := '';
					while not EOF(CueSheetFile) do
						begin
							ReadLn(CueSheetFile, CueSheetLine);
							CueSheetLine := Trim(CueSheetLine);
							if Pos('FILE ', UpperCase(CueSheetLine))=1 then
								FileName := GetFileName(CueSheetLine)
							else
								if Pos('TITLE ', UpperCase(CueSheetLine))=1 then
									Title := GetTitle(CueSheetLine)
								else
									if Pos('INDEX ', UpperCase(CueSheetLine))=1 then
										begin
											Index := GetIndex(CueSheetLine);
											i := i + 1;
											SetLength(CueSheets, i);
											CueSheets[i-1, iFile] := FileName;
											CueSheets[i-1, iTitle] := Title;
											CueSheets[i-1, iIndex1] := Index;
										end;
						end;
					CloseFile(CueSheetFile);
				end;
		end;
	Result := CueSheets;
end;

// -------------------------------------------------------------

function LoadLabels(const LabelsFileName: string): TLabels;
	var
		LabelsFile: TextFile;
		LabelLine, LabelText: string;
		i, j: Cardinal;
		Labels: TLabels;
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
							i := i + 1;
							SetLength(Labels, i);
							j := 0;
							while pos(LabelSeparator, LabelLine) <> 0 do
								begin
									LabelText := copy(LabelLine, 1, pos(LabelSeparator, LabelLine) - 1);
									j := j + 1;
									delete(LabelLine, 1, pos(LabelSeparator, LabelLine));
									Labels[i-1, j-1] := LabelText;
								end;
							if pos(LabelSeparator, LabelLine) = 0 then
								begin
									j := j + 1;
									Labels[i-1, j-1] := LabelLine;
								end;
						end;
					CloseFile(LabelsFile);
				end;
		end;
	Result := Labels;
end;

// -------------------------------------------------------------

function LabelTime(const CueTime: string): string;
	var
		iMinute, iSecond, iFrame: Integer; //ToDo: dissolve the vars
		fSeconds: Double;                  //ToDo: dissolve the var
		sSeconds: string;                  //ToDo: dissolve the var
begin
	iMinute := StrToInt(Copy(CueTime, 1, 2));
	iSecond := StrToInt(Copy(CueTime, 4, 2));
	iFrame  := StrToInt(Copy(CueTime, 7, 2));
	fSeconds := iMinute * 60 + iSecond + iFrame / 75;
	sSeconds := FormatFloat('0.000000', fSeconds);
	Result := sSeconds;
end;

// -------------------------------------------------------------

function CueTime(const LabelTime: string): string;
	var
		fSeconds: Double;
		iMinute, iSecond, iFrame: Integer;
		sMinute, sSecond, sFrame: string;
begin
	fSeconds := StrToFloat(LabelTime);

	iMinute := floor(fSeconds / 60);
	iSecond := floor(fSeconds - (iMinute * 60));
	iFrame  := round(frac(fSeconds) * 75);

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
end;

// -------------------------------------------------------------

function CueSheets2Labels(const CueSheets: TCueSheets): TLabels;
	var
		Labels, TempLabels: TLabels;
		i, j: Cardinal;
begin
	for i := 0 to Length(CueSheets) - 1 do
		begin
			SetLength(Labels, i+1);
			Labels[i, iIndex1] := LabelTime(CueSheets[i, iIndex1]);
			if CueSheets[i, iTitle] <> '' then
				Labels[i, iTitle] := CueSheets[i, iTitle]
			else
				Labels[i, iTitle] := CueSheets[i, iIndex1];
			Labels[i, iFile] := CueSheets[i, iFile];
		end;

	SetLength(TempLabels, 1);
	for i := 0 to Length(Labels)-2 do
		for j := i+1 to Length(Labels)-1 do
			if Labels[i, iFile] > Labels[j, iFile] then
				begin
					TempLabels[0] := Labels[i];
					Labels    [i] := Labels[j];
					Labels    [j] := TempLabels[0];
				end;

	Result := Labels;
end;

// -------------------------------------------------------------

function Labels2CueSheet(const Labels: TLabels): TCueSheets;
	var
		Index0, Index1, Title0, Title1: string;
		CueSheet, CueTemp: TCueSheets;
		i, j: Cardinal;
begin
	j := 0;
	SetLength(CueSheet, j);
	for i := 0 to Length(Labels) - 1 do
		begin
			Index0 := CueTime(Labels[i, iIndex0]);
			Index1 := CueTime(Labels[i, iIndex1]);

			if Index0 = Index1 then
				begin
					if Labels[i, iTitle] <> '' then
						Title0 := Labels[i, iTitle]
					else
						Title0 := Index0;
					j := j + 1;
					SetLength(CueSheet, j);
					CueSheet[j-1, iTitle] := Title0;
					CueSheet[j-1, iIndex1] := Index0;
				end
			else
				begin
					if Labels[i, iTitle] <> '' then
						begin
							Title0 := Labels[i, iTitle]; // + ' - start';
							Title1 := Labels[i, iTitle] + ' - end';
						end
					else
						begin
							Title0 := Index0;
							Title1 := Index1;
						end;
					j := j + 1;
					SetLength(CueSheet, j);
					CueSheet[j-1, iTitle] := Title0;
					CueSheet[j-1, iIndex1] := Index0;
					j := j + 1;
					SetLength(CueSheet, j);
					CueSheet[j-1, iTitle] := Title1;
					CueSheet[j-1, iIndex1] := Index1;
				end;
		end;

		SetLength(CueTemp, 1);
		for i := 0 to Length(CueSheet)-2 do
			for j := i+1 to Length(CueSheet)-1 do
				if CueSheet[i, iIndex1] > CueSheet[j, iIndex1] then
					begin
						CueTemp [0] := CueSheet[i];
						CueSheet[i] := CueSheet[j];
						CueSheet[j] := CueTemp [0];
					end;

	Result := CueSheet;
end;

// -------------------------------------------------------------

function MakeLabels(const CueSheetFileName: string): boolean;
	var
		CueSheets: TCueSheets;
		Labels: TLabels;
		LabelsFileName, LabelsFileNameBak: string;
		LabelsFile: TextFile;
		i, t: Cardinal;
		BackupSuffix: string;
begin
	Result := true;

	CueSheets := LoadCueSheet(CueSheetFileName);
	if Length(CueSheets) > 0 then
		begin
			Labels := CueSheets2Labels(CueSheets);
			LabelsFileName := '';
			for i := 0 to Length(Labels) - 1 do
				begin
					if LabelsFileName <> (Labels[i, iFile] + '.txt') then
						begin
							if i > 0 then CloseFile(LabelsFile);
							LabelsFileName := Labels[i, iFile] + '.txt';
							if FileExists(LabelsFileName) then
								begin
									// DateTimeToString(BackupSuffix, 'yyyymmddhhnnsszzz', FileDateToDateTime(FileAge(LabelsFileName)));
									t := 0;
									repeat
										Sleep(t);
										DateTimeToString(BackupSuffix, 'yyyymmddhhnnsszzz', Now);
										LabelsFileNameBak := LabelsFileName + '.' + BackupSuffix + '.bak'; // file.mp3.txt.yyyymmddhhmmsszzz.bak
										t:= 10;
									until not FileExists(LabelsFileNameBak);
									RenameFile(LabelsFileName, LabelsFileNameBak);
								end;

							AssignFile(LabelsFile, LabelsFileName);
							Rewrite(LabelsFile);
							if IOResult <> 0 then Result := False;
						end;
						WriteLn(LabelsFile, Labels[i, iIndex1], LabelSeparator, Labels[i, iIndex1], LabelSeparator, Labels[i, iTitle]);
				end;
			CloseFile(LabelsFile);
		end;
end;

// -------------------------------------------------------------

function MakeCueSheet(const LabelsFileName: string): boolean;
	var
		Labels: TLabels;
		CueSheet: TCueSheets;
		AudioFileName, AudioFileType: string;
		CueFileName, CueFileNameBak: string;
		CueFile: TextFile;
		i, t: Cardinal;
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
					// DateTimeToString(BackupSuffix, 'yyyymmddhhnnsszzz', FileDateToDateTime(FileAge(CueFileName)));
					t := 0;
					repeat
						Sleep(t);
						DateTimeToString(BackupSuffix, 'yyyymmddhhnnsszzz', Now);
						CueFileNameBak := CueFileName + '.' + BackupSuffix + '.bak'; // file.mp3.cue.yyyymmddhhmmsszzz.bak
						t:= 10;
					until not FileExists(CueFileNameBak);
					RenameFile(CueFileName, CueFileNameBak);
				end;

			AssignFile(CueFile, CueFileName);
			Rewrite(CueFile);
			if IOResult = 0 then
				begin
					WriteLn(CueFile, 'FILE "'  + AudioFileName + '" ' + AudioFileType);
					for i := 0 to Length(CueSheet) - 1 do
						begin
							N := IntToStr(i); if i < 10 then N := '0' + N;
							WriteLn(CueFile, '  TRACK '  + N + ' AUDIO');
							WriteLn(CueFile, '    TITLE "' + CueSheet[i, iTitle] + '"');
							WriteLn(CueFile, '    INDEX 01 ' + CueSheet[i, iIndex1]);
						end;
					CloseFile(CueFile);
					Result := true;
				end;
		end;
end;

// -------------------------------------------------------------

end.

