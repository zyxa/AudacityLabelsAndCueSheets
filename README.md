# LabelsAndCueSheets v0.01a
Audacity Labels to Cue Sheets Converter

Cue Sheets to Audacity Labels Converter

This simple windows applications converts audacity label files to cue sheet files and back.
This is alpha version for test purposes.

The tools have no it's own windows. They use either file list from command line or shows standart open dialog to select file(s) manually. Command line parameters - just one or more label file names. If command line is empty, then dialog box shows (multiple times, until Cancel button will be pressed). The tools show no extra messages in any case - either conversion happens successfully or not.

In order to write correct "FILE" record to cue sheets, label files must be named exactly like their audio files including audio extension and .txt extension at the end. For example, if audio file has name "track01.wav", label file must have name "track01.wav.txt", and just then the tool will write to cue sheet "track01.wav.cue"correct file information: 'FILE "track01.wav" WAVE'.
