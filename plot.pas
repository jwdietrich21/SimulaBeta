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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TALegendPanel, Forms,
  Controls, Graphics, Dialogs, Spin, StdCtrls, SimulationEngine;

type

  { TPlotForm }

  TPlotForm = class(TForm)
    Chart1: TChart;
    NSeries: TLineSeries;
    PSeries: TLineSeries;
    ISeries: TLineSeries;
    ChartLegendPanel1: TChartLegendPanel;
    GSeries: TLineSeries;
    SSeries: TLineSeries;
    MSeries: TLineSeries;
    RSeries: TLineSeries;
    procedure FormCreate(Sender: TObject);
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

procedure TPlotForm.ShowPlot;
var
  i: integer;
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
    PSeries.AddXY(i, gValues.P[i] / PFactor);
    RSeries.AddXY(i, gValues.R[i] / MicroFactor);
    ISeries.AddXY(i, gValues.I[i] / IFactor);
    GSeries.AddXY(i, gValues.G[i] / GFactor);
    SSeries.AddXY(i, gValues.S[i] / PicoFactor);
    MSeries.AddXY(i, gValues.M[i]);
    NSeries.AddXY(i, gValues.N[i]);
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

