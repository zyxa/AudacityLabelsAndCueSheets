program Labels2CueSheets;

uses
	Dialogs,
	LabelsAndCueSheets;

const
	ProgramName = 'Audacity Labels to Cue Sheets Converter 0.01a';

// -------------------------------------------------------------

function Main(): boolean;
	var
		i: Integer;
		OpenDialog: TOpenDialog;
begin
	Result := true;
	if ParamCount > 0 then
		for i := 1 to ParamCount do
			begin
				if MakeCueSheet(ParamStr(i)) = false then Result := false
			end
	else
		begin
			OpenDialog := TOpenDialog.Create(nil);
			OpenDialog.Title := ProgramName + ': Select files to convert';
			OpenDialog.Filter := '*.txt|*.TXT';
			OpenDialog.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
			while OpenDialog.Execute do // if OpenDialog.Execute then
				for i := 0 to OpenDialog.Files.Count - 1 do
					if MakeCueSheet(OpenDialog.Files.Strings[i]) = false then Result := false;
			OpenDialog.Free;
		end;
end;

// -------------------------------------------------------------

begin
	Main();
end.
