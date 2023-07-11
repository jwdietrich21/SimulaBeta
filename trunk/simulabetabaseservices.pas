unit SimulaBetaBaseServices;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ General GUI-independent services for use by several units }

{ Version 3.2.0 (Donostia) }

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math, DOM, UnitConverter
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
  , SimulaBetaTypes;

function AsTime(x: real): TDateTime;
function FormattedTime(x: real): String;
function FormatFloatDefault(const Format: string; const Value: Extended; const Default: string): string;
function XMLDateTime2DateTime(const XMLDateTime: string): TDateTime;
function TryXMLDateTime2DateTime(const S: ShortString; out Value: TDateTime): boolean;
function NodeContent(theRoot: TDOMNode; Name: string): string;
procedure VarFromNode(theRoot: TDOMNode; Name: string; var theVar: extended);
function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
procedure bell;

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
  FormattedTime := FormatDateTime(gDateTimeFormat, AsTime(x));
end;

function FormatFloatDefault(const Format: string; const Value: Extended;
  const Default: string): string;
begin
  if IsNan(Value) then
    result := Default
  else
    result := FormatFloat(Format, Value);
end;

function XMLDateTime2DateTime(const XMLDateTime: string): TDateTime;
  { adapted and expanded from a suggestion by Luiz Americo Pereira Camara }
var
  DateOnly, TimeOnly: string;
  theDate, theTime: TDateTime;
  TPos: integer;
begin
  TPos := Pos('T', XMLDateTime);
  if TPos <> 0 then
  begin
    DateOnly := Copy(XMLDateTime, 1, TPos - 1);
    TimeOnly := Copy(XMLDateTime, TPos + 1, Length(XMLDateTime));
    theDate := ScanDateTime(LeftStr(ISO_8601_DATE_FORMAT, 10), DateOnly);
    theTime := ScanDateTime(RightStr(ISO_8601_DATE_FORMAT, 8), TimeOnly);
    Result := theDate + theTime;
  end
  else
  begin
    DateOnly := XMLDateTime;
    Result := ScanDateTime(LeftStr(ISO_8601_DATE_FORMAT, 10), DateOnly);
  end;
end;

function TryXMLDateTime2DateTime(const S: ShortString; out Value: TDateTime): boolean;
begin
  Result := True;
  try
    Value := XMLDateTime2DateTime(s);
  except
    on EConvertError do
      Result := False
  end;
end;

function NodeContent(theRoot: TDOMNode; Name: string): string;
  {supports XML routines, gets the contents of a node in a file}
var
  theNode: TDOMNode;
  theText: string;
begin
  if assigned(theRoot) then
  begin
    Result := 'NA';
    theNode := theRoot.FindNode(Name);
    if assigned(theNode) then
    begin
      try
        theText := UTF8Encode(theNode.TextContent);
        if theText <> '' then
          Result := theText;
      except
        Result := 'NA';
      end;
      theNode.Destroy;
    end
    else
      Result := 'NA';
  end;
end;

procedure VarFromNode(theRoot: TDOMNode; Name: string; var theVar: extended);
{supports XML routines}
var
  oldSep: char;
  theString: string;
begin
  oldSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  theString := NodeContent(theRoot, Name);
  if theString <> 'NA' then
    theVar := StrToFloat(theString);
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
  {supports XML routines, creates an XML node from the contents of a string}
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(Name);
  TextNode := Doc.CreateTextNode(UTF8Decode(Value));
  ItemNode.AppendChild(TextNode);
  Result := ItemNode;
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

end.

