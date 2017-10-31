# LabelsAndCueSheets
Audacity Labels and Cue Sheets Converter v0.03a


This simple Windows tool converts Audacity label files to Cue Sheet files and vice versa.

The tool doesn't have it's own window. Is uses either the list of files from the command line or shows the standart open dialog to select file(s) manually.
The command line parameters are just one or more label and/or Cue Sheet file names.
If the command line is empty, the dialog box shows (multiple times, until Cancel button will be pressed).

The tool doesn't show any extra messages in any cases - either conversion proceeded successfully or not.
It doesn't ask and doesn't overwrite anything. If it is needed, the tool just renames old files by appending ".yyyymmdd-hhmmss-zzz.bak"

User can simultaneously select files in both formats. Any in the list having .cue extension will be processed as Cue Sheets, any other extensions - as Audacity labels.

In order to write correct "FILE" record to the Cue Sheet, the label file must have the name exactly like the corresponding audio file (including the audio extension) plus ".txt" extension at the end. For example, if the audio file is "Track01.wav", the label file must have name "Track01.wav.txt", then the tool will write to the Cue Sheet "Track01.wav.cue" correct file information: 
'FILE "Track01.wav" WAVE'. Otherwise user has to fix the record manually with any text editor.


This is the third alpha version for debug and test purposes.
