unit Plot;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Plot unit }

{ Version 3.1.0 (Challenger) }

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TALegendPanel,
  TAIntervalSources, TATransformations, TATools, Forms, Controls, Graphics,
  Dialogs, Spin, StdCtrls, ColorBox, ExtCtrls, ComCtrls, DateUtils,
  SimulaBetaTypes, SimulaBetaBaseServices, SimulationEngine,
  SimulaBetaGUIServices, Types;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1PanMouseWheelTool1: TPanMouseWheelTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    StatusBar1: TStatusBar;
    VariablesCheckGroup: TCheckGroup;
    mColorBox: TColorBox;
    IColorBox: TColorBox;
    GColorBox: TColorBox;
    LogCheckbox: TCheckBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    nColorBox: TColorBox;
    NSeries: TLineSeries;
    PSeries: TLineSeries;
    ISeries: TLineSeries;
    ChartLegendPanel1: TChartLegendPanel;
    GSeries: TLineSeries;
    SSeries: TLineSeries;
    MSeries: TLineSeries;
    RSeries: TLineSeries;
    PColorBox: TColorBox;
    SColorBox: TColorBox;
    RColorBox: TColorBox;
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure GColorBoxChange(Sender: TObject);
    procedure IColorBoxChange(Sender: TObject);
    procedure mColorBoxChange(Sender: TObject);
    procedure nColorBoxChange(Sender: TObject);
    procedure PColorBoxChange(Sender: TObject);
    procedure RColorBoxChange(Sender: TObject);
    procedure SColorBoxChange(Sender: TObject);
    procedure VariablesCheckGroupClick(Sender: TObject);
    procedure DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure LogCheckboxChange(Sender: TObject);
    procedure VariablesCheckGroupItemClick(Sender: TObject; Index: integer);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowPlot;
  end;

var
  PlotForm: TPlotForm;

implementation

{$R *.lfm}

{ TPlotForm }

procedure TPlotForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Left := Screen.Width - Width - 26;
  Top := Screen.Height - Height - 78;
  for i := 0 to VariablesCheckGroup.Items.Count - 1 do
    VariablesCheckGroup.Checked[i] := True;
end;

procedure TPlotForm.FormPaint(Sender: TObject);
begin
  ISeries.Title := 'I (' + gUnits.I + ')';
  GSeries.Title := 'G (' + gUnits.G + ')';
  if DarkTheme then
  begin
    Chart1.Color := clDefault;
    Chart1.BackColor := clDefault;
    Chart1.AxisList.Axes[0].Marks.LabelBrush.Color := clDefault;
    Chart1.AxisList.Axes[0].Marks.LabelBrush.Style := bsClear;
  end
  else
  begin
    Chart1.Color := clNone;
    Chart1.BackColor := clWhite;
    Chart1.AxisList.Axes[0].Marks.LabelBrush.Color := clWhite;
    Chart1.AxisList.Axes[0].Marks.LabelBrush.Style := bsSolid;
  end;
  VariablesCheckGroup.Items.Strings[0] := PSeries.Title;
  VariablesCheckGroup.Items.Strings[1] := RSeries.Title;
  VariablesCheckGroup.Items.Strings[2] := GSeries.Title;
  VariablesCheckGroup.Items.Strings[3] := SSeries.Title;
  VariablesCheckGroup.Items.Strings[4] := ISeries.Title;
  VariablesCheckGroup.Items.Strings[5] := MSeries.Title;
  VariablesCheckGroup.Items.Strings[6] := NSeries.Title;
end;

procedure TPlotForm.LogCheckboxChange(Sender: TObject);
begin
  if LogCheckbox.Checked then
  begin
    ChartAxisTransformations1LinearAxisTransform1.Enabled := False;
    ChartAxisTransformations1LogarithmAxisTransform1.Enabled := True;
  end
  else
  begin
    ChartAxisTransformations1LinearAxisTransform1.Enabled := True;
    ChartAxisTransformations1LogarithmAxisTransform1.Enabled := False;
  end;
end;

procedure TPlotForm.VariablesCheckGroupItemClick(Sender: TObject; Index: integer);
begin
  ShowPlot;
end;

procedure TPlotForm.DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
  ASteps: TDateTimeStep);
begin

end;

procedure TPlotForm.VariablesCheckGroupClick(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.PColorBoxChange(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.GColorBoxChange(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
  APoint: TPoint);
var
  x, y: double;
begin
  with ATool as TDatapointClickTool do
    if (Series is TLineSeries) then
      with TLineSeries(Series) do
      begin
        x := GetXValue(PointIndex);
        y := GetYValue(PointIndex);
        Statusbar1.SimpleText := Format('%s: x = %f, y = %f', [Title, x, y]);
      end
    else
      Statusbar1.SimpleText := '';
end;

procedure TPlotForm.IColorBoxChange(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.mColorBoxChange(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.nColorBoxChange(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.RColorBoxChange(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.SColorBoxChange(Sender: TObject);
begin
  ShowPlot;
end;

procedure TPlotForm.ShowPlot;
var
  i: integer;
  theTime: tDateTime;
  theYear, theMonth: word;
begin
  if gValues.size > 0 then
  begin
    Chart1.AxisList.Axes[1].Range.min := AsTime(gValues.t[0]);
    Chart1.AxisList.Axes[1].Range.Max := AsTime(gValues.t[gValues.size - 1]);
    if VariablesCheckGroup.Checked[0] then
      PSeries.Active := True
    else
      PSeries.Active := False;
    if VariablesCheckGroup.Checked[1] then
      RSeries.Active := True
    else
      RSeries.Active := False;
    if VariablesCheckGroup.Checked[2] then
      GSeries.Active := True
    else
      GSeries.Active := False;
    if VariablesCheckGroup.Checked[3] then
      SSeries.Active := True
    else
      SSeries.Active := False;
    if VariablesCheckGroup.Checked[4] then
      ISeries.Active := True
    else
      ISeries.Active := False;
    if VariablesCheckGroup.Checked[5] then
      MSeries.Active := True
    else
      MSeries.Active := False;
    if VariablesCheckGroup.Checked[6] then
      NSeries.Active := True
    else
      NSeries.Active := False;
    PSeries.Clear;
    RSeries.Clear;
    ISeries.Clear;
    GSeries.Clear;
    SSeries.Clear;
    MSeries.Clear;
    NSeries.Clear;
    PSeries.SeriesColor := PColorBox.Selected;
    RSeries.SeriesColor := RColorBox.Selected;
    ISeries.SeriesColor := IColorBox.Selected;
    GSeries.SeriesColor := GColorBox.Selected;
    SSeries.SeriesColor := SColorBox.Selected;
    MSeries.SeriesColor := MColorBox.Selected;
    NSeries.SeriesColor := NColorBox.Selected;
    PSeries.BeginUpdate;
    RSeries.BeginUpdate;
    ISeries.BeginUpdate;
    GSeries.BeginUpdate;
    SSeries.BeginUpdate;
    MSeries.BeginUpdate;
    NSeries.BeginUpdate;
    for i := 0 to gValues.size - 1 do
    begin
      theTime := i; // seconds
      theTime := AsTime(gValues.t[i]);    { TODO -oJWD : Make this work }
      theYear := YearOf(theTime);
      theMonth := MonthOf(theTime);
    {if theYear > 1900 then
      DateTimeIntervalChartSource1.DateTimeFormat := '"y"y "m"m "d"D hh:nn:ss'
    else if theMonth > 1 then
      DateTimeIntervalChartSource1.DateTimeFormat := '"m"m "d"D hh:nn:ss'
    else
      DateTimeIntervalChartSource1.DateTimeFormat := '"d"D hh:nn:ss';  }
      PSeries.AddXY(theTime, gValues.P[i] / PFactor);
      RSeries.AddXY(theTime, gValues.R[i] / MicroFactor);
      ISeries.AddXY(theTime, gValues.I[i] / IFactor * gInsulinConversionFactor);
      GSeries.AddXY(theTime, gValues.G[i] / GFactor * gGlucoseConversionFactor);
      SSeries.AddXY(theTime, gValues.S[i] / PicoFactor);
      MSeries.AddXY(theTime, gValues.M[i]);
      NSeries.AddXY(theTime, gValues.N[i]);
    end;
    PSeries.EndUpdate;
    RSeries.EndUpdate;
    ISeries.EndUpdate;
    GSeries.EndUpdate;
    SSeries.EndUpdate;
    MSeries.EndUpdate;
    NSeries.EndUpdate;
  end;
end;

end.
