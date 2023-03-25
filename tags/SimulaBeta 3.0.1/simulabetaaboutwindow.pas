unit SimulaBetaAboutwindow;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Unit showing window with copyright information }

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  lclintf, EnvironmentInfo, SimulaBetaResources, SimulaBetaGUIServices;

type

  { TAboutWindow }

  TAboutWindow = class(TForm)
    BigLogo: TImage;
    CopyrightLabel1: TLabel;
    CopyrightLabel10: TLabel;
    CopyrightLabel11: TLabel;
    CopyrightLabel2: TLabel;
    CopyrightLabel3: TLabel;
    CopyrightLabel4: TLabel;
    CopyrightLabel5: TLabel;
    CopyrightLabel6: TLabel;
    CopyrightLabel7: TLabel;
    CopyrightLabel8: TLabel;
    CopyrightLabel9: TLabel;
    SciCrunchLabel: TLabel;
    SimulaBetaLabel: TImage;
    URL1: TLabel;
    VersionLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SciCrunchLabelClick(Sender: TObject);
    procedure URL1Click(Sender: TObject);
  private

  public

  end;

var
  AboutWindow: TAboutWindow;

implementation

{$R *.lfm}

{ TAboutWindow }

procedure TAboutWindow.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version ' + FileVersion;
  // The following lines force redrawing. This is necessary on certain platforms
  // to ensure proper scaling in the setting of different possible screen
  // resolutions
  {$IFDEF LCLCocoa}
  Height := 312;
  Width := 472;
  Repaint;
  {$ENDIF}
end;

procedure TAboutWindow.FormPaint(Sender: TObject);
begin
  {$IFDEF LCLCocoa}
  Height := 313;
  Width := 473;
  Invalidate;
  {$ENDIF}
  if DarkTheme then
  begin
    URL1.Font.Color := clTeal;
    SciCrunchLabel.Font.Color := clTeal;
  end
  else
  begin
    URL1.Font.Color := clNavy;
    SciCrunchLabel.Font.Color := clNavy;
  end;
end;

procedure TAboutWindow.FormShow(Sender: TObject);
begin
  {$IFDEF LCLCocoa}
  WindowState := wsNormal;
  {$ENDIF}
end;

procedure TAboutWindow.SciCrunchLabelClick(Sender: TObject);
begin
  OpenURL(SCICRUNCH_URL);
end;

procedure TAboutWindow.URL1Click(Sender: TObject);
begin
  OpenURL(BASE_URL);
end;

end.

