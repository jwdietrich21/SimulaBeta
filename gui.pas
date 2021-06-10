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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ComCtrls, StdCtrls, ExtCtrls, LCLType, Spin, Menus, SimulationEngine, Prediction, Plot;

type

  { TValuesForm }

  TValuesForm = class(TForm)
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
    ValuesGrid: TStringGrid;
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
  ValuesForm: TValuesForm;

implementation

{$R *.lfm}

{ TValuesForm }

procedure TValuesForm.StartButtonClick(Sender: TObject);
var
  P, Glc, Ins: extended;
  i, j, iterations: integer;
  Prediction: TPrediction;
begin
  Screen.Cursor := crHourGlass;
  P := PSpinEdit.Value * PFactor;
  Glc := GSpinEdit.Value * GFactor;
  Ins := ISpinEdit.Value * IFactor;
  iterations := IterationsSpinEdit.Value;
  gValues := TValues.Create;
  ValuesGrid.RowCount := 26;
  for i := 0 to ValuesGrid.ColCount - 1 do
    for j := 1 to ValuesGrid.RowCount - 1 do
      ValuesGrid.Cells[i, j] := '';
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
  ValuesGrid.BeginUpdate;
  if iterations > ValuesGrid.RowCount then
    ValuesGrid.RowCount := iterations + 1;
  for i := 0 to iterations - 1 do
  begin
    ValuesGrid.Cells[0, i + 1] := IntToStr(i + 1);
    ValuesGrid.Cells[1, i + 1] := FloatToStrF(gValues.P[i] / PFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[2, i + 1] := FloatToStrF(gValues.R[i] / MicroFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[3, i + 1] := FloatToStrF(gValues.G[i] / GFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[4, i + 1] := FloatToStrF(gValues.S[i] / PicoFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[5, i + 1] := FloatToStrF(gValues.I[i] / IFactor, ffFixed, 0, 4);
    ValuesGrid.Cells[6, i + 1] := FloatToStrF(gValues.M[i], ffFixed, 0, 4);
    ValuesGrid.Cells[7, i + 1] := FloatToStrF(gValues.N[i], ffFixed, 0, 4);
  end;
  ValuesGrid.EndUpdate(true);
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
  ValuesForm.WinAboutItem.Visible := False;
  ValuesForm.AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  ValuesForm.WinAboutItem.Visible := False;
  ValuesForm.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  ValuesForm.WinAboutItem.Visible := True;
  ValuesForm.AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  ValuesForm.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  ValuesForm.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  ValuesForm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  ValuesForm.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  ValuesForm.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  ValuesForm.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  ValuesForm.RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  ValuesForm.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  ValuesForm.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  ValuesForm.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
end;

procedure TValuesForm.WinAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TValuesForm.ShowAboutWindow(Sender: TObject);
begin
  ShowMessage('SimulaBeta, a simulator of insulin-glucose homeostasis' +
               LineEnding + LineEnding + 'Prerelease version 2.0.x');
end;

procedure TValuesForm.MacAboutItemClick(Sender: TObject);
begin
  ShowAboutWindow(Sender);
end;

procedure TValuesForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TValuesForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
  PSpinEdit.Value := P0 / PFactor;
  GSpinEdit.Value := G0 / GFactor;
  ISpinEdit.Value := I0 / IFactor;
end;

procedure TValuesForm.CloseMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

end.

