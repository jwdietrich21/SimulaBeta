unit GUI;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ GUI }

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, LCLVersion, Spin, Menus,
  SimulaBetaTypes, SimulationEngine, Prediction, Plot, LogGrid,
  SimulationControl, SimulaBetaGUIServices, ScenarioHandler,
  SimulaBetaAboutwindow, PreferencesGUI, Stats, disptab;

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
    Divider01: TMenuItem;
    MacPreferencesItem: TMenuItem;
    Divider22: TMenuItem;
    DispTabItem: TMenuItem;
    Divider32: TMenuItem;
    Divider2: TToolButton;
    Divider13: TMenuItem;
    PauseItem: TMenuItem;
    PrintItem: TMenuItem;
    PrintButton: TToolButton;
    RunButton: TToolButton;
    PauseButton: TToolButton;
    StatsItem: TMenuItem;
    Divider31: TMenuItem;
    WinPreferencesItem: TMenuItem;
    ResetItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    RunItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    SimulationMenu: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    SaveButton: TToolButton;
    SaveAsButton: TToolButton;
    Divider1: TToolButton;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    MainToolBar: TToolBar;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure DispTabItemClick(Sender: TObject);
    procedure Divider1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MacPreferencesItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure ResetItemClick(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure RunItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure StatsItemClick(Sender: TObject);
    procedure MainToolBarClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure WinPreferencesItemClick(Sender: TObject);
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
  ToolbarWindow.Divider22.Visible := False;
  ToolbarWindow.WinPreferencesItem.Visible := False;
  ToolbarWindow.AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  ToolbarWindow.WinAboutItem.Visible := False;
  ToolbarWindow.Divider22.Visible := False;
  ToolbarWindow.WinPreferencesItem.Visible := False;
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

procedure TToolbarWindow.WinPreferencesItemClick(Sender: TObject);
begin
  MacPreferencesItemClick(Sender);
end;

procedure TToolbarWindow.ShowAboutWindow(Sender: TObject);
begin
  AboutWindow.Show;
end;

procedure TToolbarWindow.SetPosition;
{sets the toolbar to the screen's top margin}
begin
  hide;
  {$IFDEF DARWIN}
  WindowState := wsMaximized;
  left := 0;
  top := 20;
  height := MainToolBar.Height;
  Width := Screen.Width;
  WindowState := wsNormal;
  {$ELSE}
  WindowState := wsNormal;
  AutoSize := True;
  left := 1;
  top := 0;
  height := MainToolBar.Height + MainMenu1.Height;
  AutoSize := False;
  Width := Screen.Width - 7;
  {$ENDIF}
  AlphaBlend := False;
end;

procedure TToolbarWindow.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TToolbarWindow.MacPreferencesItemClick(Sender: TObject);
begin
  PreferencesDialog.ShowModal;
end;

procedure TToolbarWindow.OpenMenuItemClick(Sender: TObject);
var
  theFileName:  string;
  theFilterIndex: integer;
  theVersion: Str13;
begin
  if OpenDialog1.Execute then
  begin
    theFileName    := OpenDialog1.FileName;
    theFilterIndex := OpenDialog1.FilterIndex;
    case theFilterIndex of
      1:
        begin
          theVersion := '';
          ReadScenario(theFileName, theVersion);  {XML file}
        end;
    end;
  end;
end;

procedure TToolbarWindow.PauseButtonClick(Sender: TObject);
begin

end;

procedure TToolbarWindow.PrintButtonClick(Sender: TObject);
begin

end;

procedure TToolbarWindow.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TToolbarWindow.ResetItemClick(Sender: TObject);
begin
  InitSimulation;
  ControlWindow.SetEditControls;
end;

procedure TToolbarWindow.RunButtonClick(Sender: TObject);
begin
  RunItemClick(Sender);
end;

procedure TToolbarWindow.RunItemClick(Sender: TObject);
begin
  ControlWindow.ShowModal;
end;

procedure TToolbarWindow.SaveMenuItemClick(Sender: TObject);
var
  theDelimiter: char;
  theFileName:  string;
  theFilterIndex: integer;
  theForm: TForm;
begin
  theForm := Screen.ActiveForm;
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
    end;
    case theFilterIndex of
      1..3: if theForm = LogWindow then
              LogWindow.SaveGrid(theFileName, theDelimiter)
            else if theForm = DispTabWindow then
              DispTabWindow.SaveGrid(theFileName, theDelimiter)
            else if theForm = StatsForm then
              StatsForm.SaveGrid(theFileName, theDelimiter);
      4: SaveScenario(theFilename);
    end;
  end;
end;

procedure TToolbarWindow.StatsItemClick(Sender: TObject);
begin
  StatsForm.Show;
end;

procedure TToolbarWindow.MainToolBarClick(Sender: TObject);
begin

end;

procedure TToolbarWindow.OpenButtonClick(Sender: TObject);
begin
  OpenMenuItemClick(Sender);
end;

procedure TToolbarWindow.SaveButtonClick(Sender: TObject);
begin
  SaveMenuItemClick(Sender);
end;

procedure TToolbarWindow.SaveAsButtonClick(Sender: TObject);
begin
  SaveButtonClick(Sender);
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
var
  theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if theForm = LogWindow then
    LogWindow.CopyCells
  else if theForm = DispTabWindow then
    DispTabWindow.CopyCells
  else if theForm = StatsForm then
    StatsForm.CopyCells;
end;

procedure TToolbarWindow.DispTabItemClick(Sender: TObject);
begin
  DispTabWindow.Show;
end;

procedure TToolbarWindow.Divider1Click(Sender: TObject);
begin

end;

end.

