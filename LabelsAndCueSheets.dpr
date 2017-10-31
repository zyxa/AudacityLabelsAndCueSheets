program LabelsAndCueSheets;

uses
  SysUtils, // UpperCase
	StrUtils, // RightStr
	Dialogs,  // OpenDialog, ShowMessage
	CueSheets,
	Labels;

const
	ProgramName = 'Audacity Labels and Cue Sheets Converter 0.03a';

// -------------------------------------------------------------

function Main(): boolean;
	var
		i: Integer;
		P: string;
		OpenDialog: TOpenDialog;
begin
	Result := true;
	if ParamCount > 0 then
		for i := 1 to ParamCount do
			begin
				P := UpperCase(ParamStr(i));
				if (P = '/?') or (P = '-?') or (P = '/H') or (P = '-H') or (P = '/HELP') or (P = '-HELP') or (P = '--HELP') then
					begin
						ShowMessage(ProgramName); // MessageBox('Text', 'Caption', MB_OK); //ToDo: Help text
					end
				else
					if RightStr(P, 4) = '.CUE' then
						begin
							if MakeCueSheet(ParamStr(i)) = false then Result := false;
						end
					else
						begin
							if MakeLabels(ParamStr(i)) = false then Result := false;
						end;
			end
	else
		begin
			OpenDialog := TOpenDialog.Create(nil);
			OpenDialog.Title := ProgramName + ': Select files to convert';
			OpenDialog.Filter := '*.txt;*.cue|*.TXT;*.CUE';
			OpenDialog.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
			while OpenDialog.Execute do
				for i := 0 to OpenDialog.Files.Count - 1 do
					begin
						if UpperCase(RightStr(OpenDialog.Files.Strings[i], 4)) = '.CUE' then
							begin
								if MakeLabels(OpenDialog.Files.Strings[i]) = false then Result := false;
							end
						else
							begin
								if MakeCueSheet(OpenDialog.Files.Strings[i]) = false then Result := false;
							end;
					end;
			OpenDialog.Free;
		end;
end; // function Main(): boolean; // }

// -------------------------------------------------------------

begin
	Main();
end.
