unit GUI;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ GUI }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, LCLVersion, Spin, Menus,
  SimulaBetaTypes, SimulationEngine, Prediction, Plot, LogGrid,
  SimulationControl, SimulaBetaGUIServices;

type

  { TToolbarWindow }

  TToolbarWindow = class(TForm)
    AppleMenu: TMenuItem;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    RunItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    SimulationMenu: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    ToolBar1: TToolBar;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure RunItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowAboutWindow(Sender: TObject);
    procedure SetPosition;
  end;

var
  ToolbarWindow: TToolbarWindow;

implementation

{$R *.lfm}

{ TToolbarWindow }

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  ToolbarWindow.WinAboutItem.Visible := False;
  ToolbarWindow.AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  ToolbarWindow.WinAboutItem.Visible := False;
  ToolbarWindow.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  ToolbarWindow.WinAboutItem.Visible := True;
  ToolbarWindow.AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  ToolbarWindow.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  ToolbarWindow.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  ToolbarWindow.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  ToolbarWindow.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  ToolbarWindow.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  ToolbarWindow.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  ToolbarWindow.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  ToolbarWindow.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  ToolbarWindow.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  ToolbarWindow.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  ToolbarWindow.RunItem.ShortCut  := ShortCut(VK_R, modifierKey);
end;

procedure TToolbarWindow.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TToolbarWindow.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('SimulaBeta, a simulator of insulin-glucose homeostasis' +
    LineEnding + LineEnding + 'Version 2.1.1');
end;

procedure TToolbarWindow.SetPosition;
{sets the toolbar to the screen's top margin}
begin
  hide;
  {$IFDEF DARWIN}
  WindowState := wsMaximized;
  left := 0;
  top := 20;
  height := Toolbar1.Height;
  Width := Screen.Width;
  WindowState := wsNormal;
  {$ELSE}
  WindowState := wsNormal;
  AutoSize := True;
  left := 1;
  top := 0;
  height := Toolbar1.Height + MainMenu1.Height;
  AutoSize := False;
  Width := Screen.Width - 7;
  {$ENDIF}
  AlphaBlend := False;
end;

procedure TToolbarWindow.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TToolbarWindow.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TToolbarWindow.RunItemClick(Sender: TObject);
begin
  ControlWindow.Show;
end;

procedure TToolbarWindow.SaveMenuItemClick(Sender: TObject);
var
  theDelimiter: char;
  theFileName:  string;
  theFilterIndex: integer;
begin
  if SaveDialog1.Execute then
  begin
    theFileName    := SaveDialog1.FileName;
    theFilterIndex := SaveDialog1.FilterIndex;
    {$IFDEF LCLcarbon}{compensates for a bug in older versions of carbon widgetset}
      if (lcl_major < 2) and (lcl_minor < 2) then
        theFilterIndex := theFilterIndex + 1;
    {$ENDIF}
    case theFilterIndex of
      1: theDelimiter := kTab; // Tab-delimited
      2: if DefaultFormatSettings.DecimalSeparator = ',' then
          theDelimiter := ';'  // CSV
        else
          theDelimiter := ','; // CSV
      3: theDelimiter := 'd';  // DIF
      // 4: theDelimiter := ' '; { for future extensions }
    end;
    case theFilterIndex of
      1..3: LogWindow.SaveGrid(theFileName, theDelimiter);
      // 4: SaveScenario(theFilename);
    end;
  end;
end;

procedure TToolbarWindow.ToolButton3Click(Sender: TObject);
begin
  SaveMenuItemClick(Sender)
end;

procedure TToolbarWindow.ToolButton4Click(Sender: TObject);
begin
  ToolButton3Click(Sender);
end;

procedure TToolbarWindow.FormCreate(Sender: TObject);
begin
  SetPosition;
  AdaptMenus;
end;

procedure TToolbarWindow.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TToolbarWindow.CopyMenuItemClick(Sender: TObject);
begin
  LogWindow.CopyCells;
end;

end.

