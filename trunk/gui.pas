unit GUI;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ GUI }

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
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, Spin, Menus, SimulationEngine,
  Prediction, Plot, LogGrid;

type

  { TControlWindow }

  TControlWindow = class(TForm)
    AppleMenu: TMenuItem;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    DBetaEdit1: TFloatSpinEdit;
    DRLabel: TLabel;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    DBetaEdit: TFloatSpinEdit;
    DBetaLabel: TLabel;
    GEEdit: TFloatSpinEdit;
    GELabel: TLabel;
    HelpMenu: TMenuItem;
    GLabel: TLabel;
    ImageList1: TImageList;
    IterationsSpinEdit: TSpinEdit;
    GBetaLabel: TLabel;
    GBetaEdit: TFloatSpinEdit;
    IUnitLabel: TLabel;
    GUnitLabel: TLabel;
    InitialConditionsLabel: TLabel;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    ILabel: TLabel;
    ISpinEdit: TFloatSpinEdit;
    GSpinEdit: TFloatSpinEdit;
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
    PLabel: TLabel;
    PSpinEdit: TFloatSpinEdit;
    StartButton: TButton;
    ToolBar1: TToolBar;
    IterationsLabel: TLabel;
    GRLabel: TLabel;
    GREdit: TFloatSpinEdit;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowAboutWindow(Sender: TObject);
  end;

var
  ControlWindow: TControlWindow;

implementation

{$R *.lfm}

{ TControlWindow }

procedure TControlWindow.StartButtonClick(Sender: TObject);
var
  P, Glc, Ins: extended;
  iterations: integer;
  Prediction: TPrediction;
begin
  Screen.Cursor := crHourGlass;
  P := PSpinEdit.Value * PFactor;
  Glc := GSpinEdit.Value * GFactor;
  Ins := ISpinEdit.Value * IFactor;
  iterations := IterationsSpinEdit.Value;
  gValues := TValues.Create;
  LogWindow.EmptyGrid;
  PlotForm.PSeries.Clear;
  PlotForm.RSeries.Clear;
  PlotForm.GSeries.Clear;
  PlotForm.SSeries.Clear;
  PlotForm.ISeries.Clear;
  PlotForm.MSeries.Clear;
  PlotForm.NSeries.Clear;
  InitSimulation(P);
  Prediction := PredictedEquilibrium(P, gStrucPars);
  PredictionForm.DisplayPrediction(Prediction);
  application.ProcessMessages;
  RunSimulation(P, Glc, Ins, iterations);
  LogWindow.FillGrid(iterations);
  application.ProcessMessages;
  PlotForm.ShowPlot;
  gValues.Destroy;
  Screen.Cursor := crDefault;
end;

procedure AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  ControlWindow.WinAboutItem.Visible := False;
  ControlWindow.AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  ControlWindow.WinAboutItem.Visible := False;
  ControlWindow.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  ControlWindow.WinAboutItem.Visible := True;
  ControlWindow.AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  ControlWindow.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  ControlWindow.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  ControlWindow.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  ControlWindow.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  ControlWindow.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  ControlWindow.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  ControlWindow.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  ControlWindow.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  ControlWindow.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  ControlWindow.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TControlWindow.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TControlWindow.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('SimulaBeta, a simulator of insulin-glucose homeostasis' +
               LineEnding + LineEnding + 'Prerelease version 2.0.x');
end;

procedure TControlWindow.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TControlWindow.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TControlWindow.FormCreate(Sender: TObject);
begin
  AdaptMenus;
  PSpinEdit.Value := P0 / PFactor;
  GSpinEdit.Value := G0 / GFactor;
  ISpinEdit.Value := I0 / IFactor;
end;

procedure TControlWindow.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

end.

