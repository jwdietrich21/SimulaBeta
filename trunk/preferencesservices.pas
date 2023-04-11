unit PreferencesServices;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Basic (GUI-independent) services for Preferences }

{ Version 3.1.1 (Challenger) }

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
  Classes, SysUtils, Dialogs, DOM, XMLRead, XMLWrite,
  UnitConverter, SimulaBetaTypes, SimulaBetaResources, SimulaBetaBaseServices,
  SimulationEngine
  {$IFDEF Windows}
  , Windows
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ELSE}
      {$IFDEF LCLCocoa}
        , CocoaAll, MacOSAll
      {$ENDIF}
    {$ENDIF}
  , Unix
  {$ENDIF};

const
  kInsulinUoM = 'pmol/l'; // default values, can be overriden by preferences
  kGlucoseUoM = 'mmol/l';

procedure InitMetabolicConversionFactors;
procedure SetConversionFactors;
function GetPreferencesFolder: String;
function GetPreferencesFile: String;
procedure ReadPreferences;
procedure SavePreferences;

implementation

procedure InitMetabolicConversionFactors;
begin
  InitConversionFactors;
end;

procedure SetConversionFactors;
begin
  gInsulinConversionFactor := ConvertedValue(1, kInsulinActivity, kInsulinUoM, gUnits.I);
  gGlucoseConversionFactor := ConvertedValue(1, kMolarMassGlucose, kGlucoseUoM, gUnits.G);
  gGlucLoadConversionFactor := kMolarMassGlucose;
end;

function GetPreferencesFolder: String;
{ platform-independend method to search for the location of preferences folder}
const
kMaxPath = 1024;
{$IFDEF DARWIN}
var
theError: OSErr;
theRef: FSRef;
pathBuffer: PChar;
{$ENDIF}
begin
{$IFDEF DARWIN}
  try
    pathBuffer := Allocmem(kMaxPath);
  except on exception do
    begin
      ShowMessage(PREFERENCES_READ_ERROR_MESSAGE);
      exit;
    end;
  end;
  try
    Fillchar(pathBuffer^, kMaxPath, #0);
    Fillchar(theRef, Sizeof(theRef), #0);
    theError := FSFindFolder(kOnAppropriateDisk, kPreferencesFolderType, kDontCreateFolder, theRef);
    if (pathBuffer <> nil) and (theError = noErr) then
    begin
      theError := FSRefMakePath(theRef, pathBuffer, kMaxPath);
      if theError = noErr then
        GetPreferencesFolder := UTF8ToAnsi(StrPas(pathBuffer)) + '/'
      else
        ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
    end;
  finally
    Freemem(pathBuffer);
  end
{$ELSE}
  GetPreferencesFolder := GetAppConfigDir(false);
{$ENDIF}
end;

function GetPreferencesFile: String;
begin
  {$IFDEF DARWIN}
    GetPreferencesFile := GetPreferencesFolder + SIMULABETA_GLOBAL_ID + '.xml';
  {$ELSE}
    GetPreferencesFile := GetAppConfigFile(false);
  {$ENDIF}
end;

procedure SubstitutePreferences;
{get standard values for preferences if preferences file nonexistent or corrupt}
begin
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;
end;

procedure ReadPreferences;
{reads preferences from file}
{should not be called before PreferencesDialog has been created}
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  theFileName: String;
begin
  theFileName := GetPreferencesFile;
  if FileExists(theFileName) then
  try
    ReadXMLFile(Doc, theFileName);
    RootNode := Doc.DocumentElement.FindNode('units');
    gUnits.I := NodeContent(RootNode, 'Insulin');
    gUnits.G := NodeContent(RootNode, 'Glucose');
    if assigned(RootNode) then
      RootNode.Destroy;
    RootNode := Doc.DocumentElement.FindNode('formats');
    if not assigned(RootNode) then
      SubstitutePreferences
    else
    begin
      gNumberFormat := NodeContent(RootNode, 'numbers');
      gDateTimeFormat := NodeContent(RootNode, 'time');
    end;
  finally
    if assigned(RootNode) then
      RootNode.Destroy;
    if assigned(Doc) then
      Doc.Destroy;
  end
  else  {Use standards if preference file does not exist}
    SubstitutePreferences;
  SetConversionFactors;
end;

procedure SavePreferences;
{saves preferences to file}
var
  theFileName, PreferencesFolder: String;
  Doc: TXMLDocument;
  RootNode, ElementNode: TDOMNode;
begin
  theFileName := GetPreferencesFile;
  PreferencesFolder := GetPreferencesFolder;
  try
    Doc := TXMLDocument.Create;

    RootNode := Doc.CreateElement('preferences');
    Doc.Appendchild(RootNode);
    RootNode:= Doc.DocumentElement;

    ElementNode:=Doc.CreateElement('units');

    ElementNode.AppendChild(SimpleNode(Doc, 'Insulin', EncodeGreek(gUnits.I)));
    ElementNode.AppendChild(SimpleNode(Doc, 'Glucose', EncodeGreek(gUnits.G)));

    RootNode.AppendChild(ElementNode);

    ElementNode:=Doc.CreateElement('formats');

    ElementNode.AppendChild(SimpleNode(Doc, 'numbers', gNumberFormat));
    ElementNode.AppendChild(SimpleNode(Doc, 'time', gDateTimeFormat));

    RootNode.AppendChild(ElementNode);

    if not DirectoryExists(PreferencesFolder) then
      if not CreateDir(PreferencesFolder) then
        ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
    if DirectoryExists(PreferencesFolder) then
        begin
          if FileExists(theFileName) then
            SysUtils.DeleteFile(theFileName);
          WriteXMLFile(Doc,theFileName);
        end;
  finally
    ElementNode.Destroy;
    RootNode.Destroy;
    Doc.Destroy;
  end;
end;

initialization
  gInsulinConversionFactor := 1;
  gGlucoseConversionFactor := 1;
  gGlucLoadConversionFactor := kMolarMassGlucose;
  gInsulinLoadConversionFactor := 1 / kInsulinActivity;
  gUnits.I := kInsulinUoM;
  gUnits.G := kGlucoseUoM;
  ReadPreferences;

end.

