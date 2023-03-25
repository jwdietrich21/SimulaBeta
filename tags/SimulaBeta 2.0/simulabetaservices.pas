unit SimulaBetaServices;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ General services for use by several units }

{ Version 2.0.0 (Sleeping Briar Rose) }

{ (c) Johannes W. Dietrich, 1994 - 2021 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2021 }

{ Standard blocks for systems modelling and simulation }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://simulabeta.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Grids, Dialogs, Forms, clipbrd
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
  , DIFSupport;

const
  kNUL = char(0);           {Special ASCII characters}
  kENTER = char(3);
  kTAB = char(9);
  kLF = char(10);
  kRETURN = char(13);
  kESCAPE = char(27);
  kPERIOD = '.';
  kSPACE = ' ';
  kSLASH = '/';
  kCOLON = ':';
  kSEMICOLON = ';';
  kOMIT = '•';
  kCRLF = #13#10;

  SAVE_ERROR_MESSAGE = 'Error saving file.';

  STANDARD_TIME_FORMAT = '"d"D hh:nn:ss';

var
  gDateTimeFormat: String;

function AsTime(x: real): TDateTime;
function FormattedTime(x: real): String;
procedure bell;
procedure CutorCopyfromGrid(theGrid: TStringGrid; cut: boolean = False);
procedure SetFileName(theForm: TForm; const FileName: string);
procedure SaveGridToFile(theTable: TStringGrid; theFileName: string;
  theDelimiter: char; colNames, rowNames: boolean; var ReturnCode: integer);
procedure ShowSaveError;

implementation

function AsTime(x: real): TDateTime;
  {Converts second values to TDateTime representation}
var
  r: longint;
  y, d: word;
  theTime, theDate: TDateTime;
begin
  y := 1900;
  d := 1;
  theDate := EncodeDateDay(y, d);
  r := trunc(x);
  theTime := IncSecond(theDate, r);
  AsTime := theTime;
end;

function FormattedTime(x: real): String;   {Converts second values to a formatted time}
begin
  gDateTimeFormat := STANDARD_TIME_FORMAT;
  FormattedTime := FormatDateTime(gDateTimeFormat, AsTime(x));
end;

procedure bell; {platform-independent implementation of acustical warning}
var
  s: longint;
begin
  {$IFDEF WINDOWS}
  MessageBeep(0);
  {$ELSE}
  {$IFDEF LCLCarbon}
  SysBeep(30);
  {$ELSE}
  s := fpSystem('echo -ne ''\007''');
  {s := fpSystem('echo -ne "\a"');}
  {s := fpSystem('tput bel');}
  {beep;}
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
      doc.SetHead('SimThyr');

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

procedure ShowSaveError;
begin
  bell;
  MessageDlg(SAVE_ERROR_MESSAGE, mtError, [mbOK], 0);
end;

end.

