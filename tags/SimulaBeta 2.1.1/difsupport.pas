unit DIFSupport;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ This unit provides support for DIF file handling }

{ Version 2.1.1 (Turning the tides) }

{ (c) Johannes W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://simulabeta.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}

interface

uses
  Classes, SysUtils;

const
  kCRLF = #13#10;
  kQUOT = #34;

type
  tHeader = Record
    version: Integer;
    title: String;
    vectors, tuples: Integer;
    labels, comments, units, displayUnits: TStringList;
    sizes: array of integer;
  end;
  TDIFDocument = class
    content: TStrings;
    Header: tHeader;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetHead(Identifier: String);
      procedure NewTuple;
      procedure AppendCell(Value: String);
      procedure AppendCell(Value: Real);
      procedure AppendCell(Value: Boolean);
      function NewLabel(Value: string): string;
  end;

procedure WriteDIFFile(Doc: TDIFDocument; path: String; var ReturnCode: integer);

var
  gNumVectors: integer;

implementation

constructor TDIFDocument.Create;
begin
  inherited Create;
  content := TStringList.Create;
  content.TextLineBreakStyle := tlbsCRLF;
end;

destructor TDIFDocument.Destroy;
begin
  content.Free;
  Inherited destroy;
end;

procedure TDIFDocument.SetHead(Identifier: String);
{creates a scaffold for header}
begin
  with Header do
  begin
  title := Identifier;
  version := 1;
  vectors := 0;
  tuples := 0;
  labels := nil;
  comments := nil;
  units := nil;
  displayUnits := nil;
  SetLength(sizes, 0);
  end;
end;

procedure TDIFDocument.NewTuple;
begin
  gNumVectors := 0;
  inc(header.tuples);
  content.Append('-1,0');
  content.Append('BOT');
end;

procedure TDIFDocument.AppendCell(Value: String);
begin
  inc(gNumVectors);
  if gNumVectors > header.vectors then
    header.vectors := gNumVectors;
  content.Append('1,0');
  content.Append(kQUOT + Value + kQUOT);
end;

procedure TDIFDocument.AppendCell(Value: Real);
begin
  AppendCell(FloatToStr(Value));
end;

procedure TDIFDocument.AppendCell(Value: Boolean);
begin
  if value = true then
    AppendCell('TRUE')
  else
    AppendCell('FALSE');
end;

function TDIFDocument.NewLabel(Value: string): string;
var
  tempString: string;
begin
  tempString := '';
  Inc(gNumVectors);
  if gNumVectors > header.vectors then
    header.vectors := gNumVectors;
  tempString := ('LABEL' + kCRLF + IntToStr(gNumVectors) + ',0' + kCRLF);
  tempString := tempString + (kQUOT + Value + kQUOT + kCRLF);
  Result := tempString;
end;

procedure WriteDIFFile(Doc: TDIFDocument; path: String; var ReturnCode: integer);
var
  headerChunk, dataChunk, endChunk: AnsiString;
  i: integer;
begin
  headerChunk := 'TABLE' + kCRLF + '0,' + IntToStr(Doc.Header.version) + kCRLF;
  headerChunk := headerChunk + kQUOT + Doc.Header.title + kQUOT + kCRLF;
  headerChunk := headerChunk + 'VECTORS' + kCRLF + '0,' + IntToStr(Doc.Header.vectors) + kCRLF + kQUOT + kQUOT + kCRLF;
  headerChunk := headerChunk + 'TUPLES' + kCRLF + '0,' + IntToStr(Doc.Header.tuples) + kCRLF + kQUOT + kQUOT + kCRLF;
  if Doc.Header.labels <> nil then
    for i := 1 to Doc.Header.labels.Count do
      headerChunk := headerChunk + Doc.NewLabel(Doc.Header.labels.Strings[i - 1]);
  dataChunk := 'DATA' + kCRLF + '0,0' + kCRLF + kQUOT + kQUOT;
  endChunk := '-1,0' + kCRLF + 'EOD';
  Doc.content.Insert(0, headerChunk + dataChunk);
  Doc.content.Append(endChunk);
  ReturnCode := 6;
  try
    Doc.content.SaveToFile(path);
    ReturnCode := 0;
  finally
  end;
end;

end.

