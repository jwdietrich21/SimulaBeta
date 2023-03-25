unit EnvironmentInfo;

{ EnvironmentInfo }
{ Provides information on hardware and software environment}

{ Partly based on code provided by Mike Thompson, published at}
{ http://www.lazarus.freepascal.org/index.php/topic,13957.0.html}

{ (c) J. W. Dietrich, 2007 - 2022 }

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, StrUtils, LCLVersion, DOS
  {$IFDEF Darwin}
  , MacOSAll
  {$ENDIF}
  {$IFDEF LCLCocoa}
  , CocoaAll
  {$ENDIF}
  {$IFDEF WINDOWS}
  , Windows, Win32Proc
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}
  , InterfaceBase, versiontypes, versionresource;

type

  TPlatformInfo = record
    CPU, OS: string;
  end;

function DateOfCompiling: TDateTime;
function DateOfCompilingAsString: String;
function CompilerVersion: String;
function LCLVersion: String;
function PlatformInfo: TPlatformInfo;
function CurrentWidgetSet: String;
function OSVersion: String;
function YosemiteORNewer: boolean;
function SierraOrNewer: boolean;
function MojaveOrNewer: boolean;
function XPORNewer: boolean;
function VistaORNewer: boolean;
function Win8OrNewer: boolean;
function ProductVersion: String;
function FileVersion: String;
function SystemVersion: String;

implementation

{$IF (LCL_MAJOR >= 2) OR (LCL_MAJOR >= 1) AND (LCL_MINOR >=8)}
  {$DEFINE NewLaz}
{$ENDIF}
{$IFDEF NewLaz}
  uses
    LCLPlatformDef;
{$ENDIF}

type
  { TVersionInfo }

  TVersionInfo = class
  private
    FBuildInfoAvailable: Boolean;
    FVersResource: TVersionResource;
    function GetFixedInfo: TVersionFixedInfo;
    function GetStringFileInfo: TVersionStringFileInfo;
    function GetVarFileInfo: TVersionVarFileInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(Instance: THandle);
    property BuildInfoAvailable: Boolean Read FBuildInfoAvailable;
    property FixedInfo: TVersionFixedInfo Read GetFixedInfo;
    property StringFileInfo: TVersionStringFileInfo Read GetStringFileInfo;
    property VarFileInfo: TVersionVarFileInfo Read GetVarFileInfo;
  end;

Var
  FInfo: TVersionInfo;

Const
  GTK_WIDGETSET          = 'GTK widget set';
  GTK2_WIDGETSET         = 'GTK 2 widget set';
  GTK3_WIDGETSET         = 'GTK 2 widget set';
  WIN_WIDGETSET          = 'Win32/Win64 widget set';
  WINCE_WIDGETSET        = 'WinCE widget set';
  CARBON_WIDGETSET       = 'Carbon widget set';
  COCOA_WIDGETSET        = 'Cocoa widget set';
  QT_WIDGETSET           = 'QT widget set';
  QT5_WIDGETSET          = 'QT 5 widget set';
  fpGUI_WIDGETSET        = 'fpGUI widget set';
  noGUI_WIDGETSET        = 'no GUI widget set';
  CUSTROMDRAWN_WIDGETSET = 'CustomDrawn widget set';
  MUI_WIDGETSET          = 'MUI widget set';
  OTHER_WIDGETSET        = 'Other gui';

function DateOfCompiling: TDateTime;
var
  tempString, dateString, timeString: String;
  theDate, theTime: TDateTime;
  delims: TSysCharSet;
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DateSeparator := '/';
  theFormat.ShortDateFormat := 'yyyy/mm/dd';
  theFormat.TimeSeparator := ':';
  theFormat.ShortTimeFormat := 'hh:nn:ss';
  tempString := DateOfCompilingAsString;
  delims := [',', ' '];
  dateString := ExtractWord(0, tempString, delims);
  timeString := ExtractWord(1, tempString, delims);
  if not TryStrToDate(dateString, theDate, theFormat) then
    theDate := 0;
  if not TryStrToTime(timeString, theTime, theFormat) then
    thetime := 0;
  result := ComposeDateTime(theDate, theTime);
end;

function DateOfCompilingAsString: String;
var
  theDate, theTime: String;
begin
  theDate := {$I %DATE%};
  theTime := {$I %TIME%};
  Result := theDate + ', ' + theTime;
end;

function CompilerVersion: String;
begin
  result := 'FPC ' + {$I %FPCVERSION%};
end;

function LCLVersion: String;
begin
  result := 'LCL ' + lcl_version;
end;

function PlatformInfo: TPlatformInfo;
begin
  result.CPU := {$I %FPCTARGETCPU%};
  result.OS := {$I %FPCTARGETOS%};
end;

function CurrentWidgetSet: String;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:         result := GTK_WIDGETSET;
    lpGtk2:        result := GTK2_WIDGETSET;
    lpGtk3:        result := GTK3_WIDGETSET;
    lpWin32:       result := WIN_WIDGETSET;
    lpWinCE:       result := WINCE_WIDGETSET;
    lpCarbon:      result := CARBON_WIDGETSET;
    lpCocoa:       result := COCOA_WIDGETSET;
    lpQT:          result := QT_WIDGETSET;
    {$IFDEF NewLaz}
      lpQt5:         result := QT5_WIDGETSET;
    {$ENDIF}
    lpfpGUI:       result := fpGUI_WIDGETSET;
    lpNoGUI:       result := noGUI_WIDGETSET;
    lpCustomDrawn: result := CUSTROMDRAWN_WIDGETSET;
    {$IFDEF NewLaz}
      lpMUI:         result := MUI_WIDGETSET
    {$ENDIF}
  otherwise
    result := OTHER_WIDGETSET;
  end;
end;

function OSVersion: String; {returns the major version of the operating system}
begin
  {$IFDEF LCLcarbon}
  if SierraOrNewer then
    OSVersion := 'macOS 10.'
  else
    OSVersion := 'Mac OS X 10.';
  {$ELSE}
  {$IFDEF LCLCocoa}
  OSVersion := 'macOS ';
  {$ELSE}
  {$IFDEF Linux}
  OSVersion := 'Linux Kernel ';
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Unix ';
  {$ELSE}
  {$IFDEF WINDOWS}
  if WindowsVersion = wv95 then
    OSVersion := 'Windows 95 / '
  else if WindowsVersion = wvNT4 then
    OSVersion := 'Windows NT v.4 / '
  else if WindowsVersion = wv98 then
    OSVersion := 'Windows 98 / '
  else if WindowsVersion = wvMe then
    OSVersion := 'Windows ME / '
  else if WindowsVersion = wv2000 then
    OSVersion := 'Windows 2000 / '
  else if WindowsVersion = wvXP then
    OSVersion := 'Windows XP / '
  else if WindowsVersion = wvServer2003 then
    OSVersion := 'Windows Server 2003 / '
  else if WindowsVersion = wvVista then
    OSVersion := 'Windows Vista / '
  else if WindowsVersion = wv7 then
    OSVersion := 'Windows 7 / '
  {$if FPC_FULlVERSION >= 30000}{Free Pascal 3.0 or newer}
  else if WindowsVersion = wv8 then
    OSVersion := 'Windows 8 / '
  else if WindowsVersion = wv8_1 then
    OSVersion := 'Windows 8.1 / '
  else if WindowsVersion = wv10 then
    OSVersion := 'Windows 10 / '
  else if WindowsVersion = wvLater then
    OSVersion := 'Windows '
  {$ENDIF}
  else
    OSVersion := 'Windows ';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function IsMinMacOS(Maj, Min, Patch: integer): boolean;
  { returns true, if this app runs on a macOS version as specified or newer }
  {$IFDEF DARWIN}
var
  Major, Minor: SInt32;
  theError: SInt16;
  {$IFDEF LCLCocoa}
  minOsVer: NSOperatingSystemVersion;
  {$ENDIF}
  {$ENDIF}
begin
  result := false;
  {$IFDEF DARWIN}
  {$IFDEF LCLCocoa}
  minOsVer.majorVersion:= Maj;
  minOsVer.minorVersion:= Maj;
  minOsVer.patchVersion:= Patch;
  {$ENDIF}
  theError := Gestalt(gestaltSystemVersionMajor, Major);
  if theError = 0 then
    begin
      theError := Gestalt(gestaltSystemVersionMinor, Minor);
      if theError = 0 then
        if (Major = Maj) and (Minor >= Min) or (Major > Maj) then
          Result := True;
    end
  else
  begin
    {$IFDEF LCLCocoa}
    if(NSProcessInfo.ProcessInfo.isOperatingSystemAtLeastVersion(minOSVer)) then
      Result := True
    else   {$ENDIF}
      Result := False
  end;
  {$ENDIF}
end;

function YosemiteORNewer: boolean;
  { returns true, if this app runs on Mac OS X 10.10 Yosemite or newer }
begin
  result := false;
  {$IFDEF DARWIN}
  result := IsMinMacOS(10, 10, 0);
  {$ENDIF}
end;

function SierraOrNewer: boolean;
  { returns true, if this app runs on macOS X 10.12 Sierra or newer }
begin
  result := false;
  {$IFDEF DARWIN}
  result := IsMinMacOS(10, 12, 0);
  {$ENDIF}
end;

function MojaveOrNewer: boolean;
  { returns true, if this app runs on macOS X 10.14 Mojave or newer }
begin
  result := false;
  {$IFDEF DARWIN}
  result := IsMinMacOS(10, 14, 0);
  {$ENDIF}
end;

function XPORNewer: boolean;
  { returns true, if this app runs on Windows XP or a newer Windows version }
begin
  Result := False;
  {$IFDEF WINDOWS}
  if (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1) then
    Result := True;
  {$ENDIF}
end;

function VistaORNewer: boolean;
  { returns true, if this app runs on Windows Vista or a newer Windows version }
begin
  Result := False;
  {$IFDEF WINDOWS}
  if Win32MajorVersion >= 6 then
    Result := True;
  {$ENDIF}
end;

function Win8OrNewer: boolean;
  { returns true, if this app runs on Windows 8 or a newer Windows version }
begin
  Result := False;
  {$IFDEF WINDOWS}
  if (Win32MajorVersion > 6) or (Win32MajorVersion = 6) and (Win32MinorVersion >= 2) then
    Result := True;
  {$ENDIF}
end;

procedure CreateInfo;
begin
  if Not Assigned(FInfo) Then
  begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  end;
end;

function VersionToString(PV: TFileProductVersion): String;
Begin
  Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
End;

function ProductVersion: String;
begin
  CreateInfo;
  if FInfo.BuildInfoAvailable Then
    Result := VersionToString(FInfo.FixedInfo.ProductVersion)
  else
    Result := 'NA';
end;

function FileVersion: String;
begin
  CreateInfo;
  if FInfo.BuildInfoAvailable Then
    Result := VersionToString(FInfo.FixedInfo.FileVersion)
  else
    Result := 'NA';
end;

function SystemVersion: String;
var
  SystemStem, MajVer, MinVer, BugfixVer: String;
  {$IFDEF Darwin}
  Major, Minor, Bugfix: SInt32;
  theError: SInt16;
  {$ENDIF}
begin
  SystemStem := OSVersion;
  {$IFDEF LCLCarbon}
  Major := 0;
  Minor := 0;
  Bugfix := 0;
  theError := Gestalt(gestaltSystemVersionMajor, Major);
  if theError = 0 then
    MajVer := IntToStr(Major)
  else
    MajVer := '';
  theError := Gestalt(gestaltSystemVersionMinor, Minor);
  if theError = 0 then
    MinVer := IntToStr(Minor)
  else
    MinVer := '';
  theError := Gestalt(gestaltSystemVersionBugFix, Bugfix);
  if theError = 0 then
    BugfixVer := IntToStr(Bugfix)
  else
    BugfixVer := '';
  if SystemStem <> 'Mac OS X 10.' then
    SystemStem := 'Mac OS ' + MajVer + '.';
  if SierraOrNewer then
    SystemStem := 'macOS 10.';
  result := SystemStem + MinVer + '.' + BugfixVer;
  {$ELSE}
  {$IFDEF LCLCocoa}
  Major := NSProcessInfo.ProcessInfo.OperatingSystemVersion.majorVersion;
  Minor := NSProcessInfo.ProcessInfo.OperatingSystemVersion.minorVersion;
  Bugfix := NSProcessInfo.ProcessInfo.OperatingSystemVersion.patchVersion;
  MajVer := IntToStr(Major);
  MinVer := IntToStr(Minor);
  BugfixVer := IntToStr(Bugfix);
  result := SystemStem + MajVer + '.' + MinVer + '.' + BugfixVer;
  {$ELSE}
  {$IFDEF WINDOWS}
  MajVer := IntToStr(Win32MajorVersion);
  MinVer := IntToStr(Win32MinorVersion);
  result := SystemStem + MajVer + '.' + MinVer;
  {$ELSE}
  MajVer := IntToStr(Lo(DosVersion) - 4);
  MinVer := IntToStr(Hi(DosVersion));
  result := SystemStem + MajVer + '.' + MinVer;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

{ TVersionInfo }

function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
  result := FVersResource.FixedInfo;
end;

function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
  result := FVersResource.StringFileInfo;
end;

function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
  result := FVersResource.VarFileInfo;
end;

constructor TVersionInfo.Create;
begin
  inherited create;
  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
end;

destructor TVersionInfo.Destroy;
begin
  FVersResource.Free;
  inherited Destroy;
end;

procedure TVersionInfo.Load(Instance: THandle);
  var
    stream: TResourceStream;
    resID: Integer;
    res: TFPResourceHandle;
  begin
    FBuildInfoAvailable := False;
    resID := 1;
    // Defensive code to prevent failure if no resource available...
    res := FindResource(Instance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
    if Res = 0 Then
      Exit;
    stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
    try
      FVersResource.SetCustomRawDataStream(Stream);
      // access some property to load from the stream
      FVersResource.FixedInfo;
      // clear the stream
      FVersResource.SetCustomRawDataStream(nil);
      FBuildInfoAvailable := True;
    finally
      stream.Free;
    end;
end;

initialization
  FInfo := nil;

finalization
  If Assigned(FInfo) Then
    FInfo.Free;

end.

