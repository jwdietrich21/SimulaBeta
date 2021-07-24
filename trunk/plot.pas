unit Plot;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Plot unit }

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
{ http://simulabeta.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TALegendPanel,
  TAIntervalSources, TATransformations, Forms, Controls, Graphics, Dialogs,
  Spin, StdCtrls, DateUtils, SimulationEngine, SimulaBetaServices;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    LogCheckbox: TCheckBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    NSeries: TLineSeries;
    PSeries: TLineSeries;
    ISeries: TLineSeries;
    ChartLegendPanel1: TChartLegendPanel;
    GSeries: TLineSeries;
    SSeries: TLineSeries;
    MSeries: TLineSeries;
    RSeries: TLineSeries;
    procedure DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure LogCheckboxChange(Sender: TObject);
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
begin
  Left := Screen.Width - Width - 26;
  Top := Screen.Height - Height - 78;
end;

procedure TPlotForm.FormPaint(Sender: TObject);
begin
  left := 13;
  top := screen.Height - height - 65;
end;

procedure TPlotForm.LogCheckboxChange(Sender: TObject);
begin
  if LogCheckbox.Checked then
    begin
      ChartAxisTransformations1LinearAxisTransform1.Enabled := false;
      ChartAxisTransformations1LogarithmAxisTransform1.Enabled := true;
    end
  else
    begin
      ChartAxisTransformations1LinearAxisTransform1.Enabled := true;
      ChartAxisTransformations1LogarithmAxisTransform1.Enabled := false;
    end;
end;

procedure TPlotForm.DateTimeIntervalChartSource1DateTimeStepChange(
  Sender: TObject; ASteps: TDateTimeStep);
begin

end;

procedure TPlotForm.ShowPlot;
var
  i: integer;
  theTime: tDateTime;
  theYear, theMonth: Word;
begin
  Chart1.AxisList.Axes[1].Range.Max := gValues.size - 1;
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
    {theTime := AsTime(t);    { TODO -oJWD : Make this work }
    theYear := YearOf(theTime);
    theMonth := MonthOf(theTime);
    if theYear > 1900 then
      DateTimeIntervalChartSource1.DateTimeFormat := '"y"y "m"m "d"D hh:nn:ss'
    else if theMonth > 1 then
      DateTimeIntervalChartSource1.DateTimeFormat := '"m"m "d"D hh:nn:ss'
    else
      DateTimeIntervalChartSource1.DateTimeFormat := '"d"D hh:nn:ss'; }
    PSeries.AddXY(theTime, gValues.P[i] / PFactor);
    RSeries.AddXY(theTime, gValues.R[i] / MicroFactor);
    ISeries.AddXY(theTime, gValues.I[i] / IFactor);
    GSeries.AddXY(theTime, gValues.G[i] / GFactor);
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

end.

