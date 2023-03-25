unit SimulaBetaGUIServices;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ General GUI-related services for use by several units }

{ Version 3.0.1 (Tournado) }

{ (c) Johannes W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://simulabeta.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, Grids, Dialogs, Forms, clipbrd
  {$IFDEF WINDOWS}
  , Windows, Win32Proc, registry
  {$ENDIF}
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  {$IFDEF LCLCocoa}
  , CocoaAll, CocoaUtils
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}
  , SimulaBetaTypes, SimulaBetaResources, SimulaBetaBaseServices,
  EnvironmentInfo, DIFSupport;

function DarkTheme: boolean;
procedure CutorCopyfromGrid(theGrid: TStringGrid; cut: boolean = False);
procedure SetFileName(theForm: TForm; const FileName: string);
procedure SaveGridToFile(theTable: TStringGrid; theFileName: string;
  theDelimiter: char; colNames, rowNames: boolean; var ReturnCode: integer);
procedure ShowFileError;
procedure ShowSaveError;
procedure ShowVersionError;

implementation

{$IFDEF LCLCocoa}
{The following two functions were suggested by Hansaplast at https://forum.lazarus.freepascal.org/index.php/topic,43111.msg304366.html}

// Retrieve key's string value from user preferences. Result is encoded using NSStrToStr's default encoding.
function GetPrefString(const KeyName : string) : string;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
end;
{$ENDIF}

// DarkTheme: Detects if the Dark Theme (true) has been enabled or not (false)
function DarkTheme: boolean;
{$IFDEF Windows}
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
  WindowsDarkModeSupported: boolean = false; // may be set to true in future versions
var
  LightKey: boolean;
  Registry: TRegistry;
{$ENDIF}
begin
  Result := false;
  {$IFDEF Windows}
  if WindowsDarkModeSupported then
  begin
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.OpenKeyReadOnly(KEYPATH) then
        begin
          if Registry.ValueExists(KEYNAME) then
            LightKey := Registry.ReadBool(KEYNAME)
          else
            LightKey := true;
        end
      else
        LightKey := true;
        Result := not LightKey
    finally
      Registry.Free;
    end;
  end
  else
  Result := false;
  {$ELSE}
  {$IFDEF LCLCocoa}
  if MojaveOrNewer then
    Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0
  else
    Result := false;
  {$ELSE}
  Result := false;
  {$ENDIF}
  {$ENDIF}
end;

procedure CutorCopyfromGrid(theGrid: TStringGrid; cut: boolean = False);
{supports cutting or copying cells from a grid}
var
  theSelection: TGridRect;
  r, c: integer;
  textfromSelection: ansistring;
begin
  theSelection      := theGrid.Selection;
  textfromSelection := '';
  for r := theSelection.Top to theSelection.Bottom do
  begin
    for c := theSelection.Left to theSelection.Right do
    begin
      textfromSelection := textfromSelection + theGrid.Cells[c, r];
      if cut then
      begin
        theGrid.Cells[c, r] := '';
      end;
      if c < theSelection.Right then
        textfromSelection := textfromSelection + kTAB;
    end;
    if r < theSelection.Bottom then
      textfromSelection := textfromSelection + kCRLF;
  end;
  ClipBoard.AsText := textfromSelection;
end;

procedure SetFileName(theForm: TForm; const FileName: string);
{sets the title of a window to file name}
begin
  {$IFNDEF LCLCocoa} // temporary solution for a bug in Cocoa, needs evaluation
  theForm.Caption := ExtractFileName(FileName);
  {$ENDIF}
end;

procedure SaveGridToFile(theTable: TStringGrid; theFileName: string;
  theDelimiter: char; colNames, rowNames: boolean; var ReturnCode: integer);
{saves the contents of a string grid}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theString: string;
  r, c: integer;
  startC: integer;
  theContents: TStringList;
  doc: TDIFDocument;
  theCode: integer;
begin
  if rowNames then
    startC := 0
  else
    startC := 1;
  if theDelimiter = 'd' then
  begin {DIF file handling}
    theCode := 0;
    try
      doc := TDIFDocument.Create;
      doc.SetHead('SimulaBeta');

      if colNames then
      begin
        doc.NewTuple;
        theString := '';
        if assigned(theTable.Columns) then
        begin
          if startC = 0 then
          begin
            theString := 'n';
            Doc.AppendCell(theString);
          end;
          for c := 0 to theTable.ColCount - 2 do
          begin
            theString := theTable.Columns[c].Title.Caption;
            Doc.AppendCell(theString);
          end;
        end
        else
        begin
          for c := startC to theTable.ColCount - 1 do
          begin
            theString := theTable.Cells[c, 0];
            Doc.AppendCell(theString);
          end;
        end;
      end;
      for r := 1 to theTable.RowCount - 1 do
      begin
        doc.NewTuple;
        theString := '';
        for c := startC to theTable.ColCount - 1 do
        begin
          theString := theTable.Cells[c, r];
          Doc.AppendCell(theString);
        end;
      end;

      WriteDIFFile(doc, theFileName, theCode);
      if theCode <> 0 then
        ShowSaveError;
    finally
      doc.Free;
      ReturnCode := theCode;
    end;
  end
  else if theDelimiter <> ' ' then {tab delimited and CSV files}
  begin
    if theDelimiter = 't' then
      theDelimiter := kTAB;
    if theDelimiter = 'c' then
      theDelimiter := kSEMICOLON;
    ReturnCode := 0;
    theContents := TStringList.Create;
    theString := '';
    if colNames then
    begin
      if assigned(theTable.Columns) then
      begin
        if startC = 0 then
          theString := 'n' + theDelimiter;
        for c := 0 to theTable.ColCount - 2 do
          theString := theString + theTable.Columns[c].Title.Caption + theDelimiter;
      end
      else
      begin
        for c := startC to theTable.ColCount - 1 do
          theString := theString + theTable.Cells[c, 0] + theDelimiter;
      end;
      theContents.Add(theString);
    end;
    for r := 1 to theTable.RowCount - 1 do
    begin
      theString := '';
      for c := startC to theTable.ColCount - 1 do
        theString := theString + theTable.Cells[c, r] + theDelimiter;
      theContents.Add(theString);
    end;
    try
      try
        theContents.SaveToFile(theFileName);
      except
        on Ex: EFCreateError do
        begin
          ShowMessage(SAVE_ERROR_MESSAGE);
          ReturnCode := -2;
        end;
      end;
    finally
      theContents.Free;
    end;
  end
  else
  begin
    ShowSaveError;
    ReturnCode := -1;
  end;
end;

procedure ShowFileError;
begin
  bell;
  MessageDlg(FILE_FORMAT_ERROR_MESSAGE, mtError, [mbOK], 0);
end;

procedure ShowSaveError;
begin
  bell;
  MessageDlg(SAVE_ERROR_MESSAGE, mtError, [mbOK], 0);
end;

procedure ShowVersionError;
begin
  bell;
  MessageDlg(FILE_VERSION_MESSAGE, mtError, [mbOK], 0);
end;

end.

