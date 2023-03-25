unit IPS;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Information Processing Structure }

{ Version 2.1.0 (Turning the tides) }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lclintf, SystemsDiagram, SimulaBetaGUIServices;

type

  { TIPSForm }

  TIPSForm = class(TForm)
    IPSImage: TImage;
    procedure DrawIPS(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  IPSForm: TIPSForm;

implementation

{$R *.lfm}

{ TIPSForm }

procedure TIPSForm.DrawIPS(Sender: TObject);
var
  BlockDiagram: TBlockDiagram;
  IPSBitmap: TBitmap;
  Controller, DInjection1, MiMeCollector1, DInjection2, MiMeCollector2:  TIPSClass;
  VTypeAllostery: TIPSClass;
  G1, G2, G3, G4, G5, xt, DBeta, DR, x1, j1, j2: TIPSClass;
  c1, cP, cR, cG, cD1, cD11, cD12, cS, cI, cM, cN, cV, cG1: TConnectionClass;
  cI1, cD2, cD21, cD22: TConnectionClass;
  DrawColour: TColor;
begin
  BlockDiagram := TBlockDiagram.Create;
  IPSBitmap := TBitmap.Create;
  try
    IPSBitmap.Height := IPSImage.Height;
    IPSBitmap.Width := IPSImage.Width;
    if DarkTheme then
      begin
        IPSBitmap.Canvas.Brush.Color := IPSForm.Color;
        IPSBitmap.Canvas.Pen.Color := IPSForm.Color;
        DrawColour := clSilver;
      end
    else
      begin
        IPSBitmap.Canvas.Brush.Color := clWhite;
        IPSBitmap.Canvas.Pen.Color := clWhite;
        DrawColour := clBlack;
      end;
    IPSBitmap.Canvas.Rectangle(0, 0, IPSBitmap.Width, IPSBitmap.Height);
    IPSBitmap.Canvas.Pen.Color := DrawColour;
    BlockDiagram.canvas := IPSBitmap.Canvas;

    BlockDiagram.canvas.Pen.Color := clGreen;

    xt := TTerminalClass.Create;
    xt.blockDiagram := BlockDiagram;
    BlockDiagram.firstIPSObject := xt;
    SetRect(xt.boundsRect, 21, 10, 41, 30);
    xt.title := 'P(t)';
    xt.font.Style := [fsItalic];
    TTerminalClass(xt).TextMargin := 5;
    TTerminalClass(xt).TextPosition := rightmiddle;
    xt.Draw;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    Controller := TPiClass.Create;
    Controller.blockDiagram := BlockDiagram;
    SetRect(Controller.boundsRect, 10, 50, 50, 90);
    TInvertableClass(Controller).invertedSegments := [bottomSegment];
    Controller.Draw;
    xt.Next := Controller;

    BlockDiagram.canvas.Pen.Color := clGreen;

    cP := TConnectionClass.Create;
    cP.blockDiagram := BlockDiagram;
    cP.sourceObject := xt;
    cP.sourceAnchor := bottommiddle;
    cP.drainObject := Controller;
    cP.drainAnchor := topmiddle;
    cP.Draw;
    Controller.Next := cP;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    G1 := TPClass.Create;
    G1.blockDiagram := BlockDiagram;
    SetRect(G1.boundsRect, 110, 52, 170, 88);
    G1.title := 'G1 (ASIA)';
    G1.font.Style := [fsItalic];
    G1.Draw;
    cP.Next := G1;

    BlockDiagram.canvas.Pen.Color := clBlue;

    cR := TConnectionClass.Create;
    cR.blockDiagram := BlockDiagram;
    cR.sourceObject := Controller;
    cR.sourceAnchor := rightmiddle;
    cR.drainObject := G1;
    cR.drainAnchor := leftmiddle;
    cR.title := 'R(t)';
    cR.font.Style := [fsItalic];
    TConnectionClass(cR).TextMargin := 7;
    TConnectionClass(cR).TextPosition := topmiddle;
    cR.Draw;
    G1.Next := cR;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    MiMeCollector1 := TPiClass.Create;
    MiMeCollector1.blockDiagram := BlockDiagram;
    SetRect(MiMeCollector1.boundsRect, 430, 52, 480, 88);
    TInvertableClass(MiMeCollector1).invertedSegments := [bottomSegment];
    MiMeCollector1.Draw;
    cR.Next := MiMeCollector1;

    G3 := TPClass.Create;
    G3.blockDiagram := BlockDiagram;
    SetRect(G3.boundsRect, 520, 180, 580, 216);
    G3.title := 'G3 (ASIA)';
    G3.font.Style := [fsItalic];
    G3.Draw;
    MiMeCollector1.Next := G3;

    DInjection1 := TSigmaClass.Create;
    DInjection1.blockDiagram := BlockDiagram;
    SetRect(DInjection1.boundsRect, 338, 105, 398, 145);
    DInjection1.Draw;
    G3.Next := DInjection1;

    G2 := TPClass.Create;
    G2.blockDiagram := BlockDiagram;
    SetRect(G2.boundsRect, 338, 52, 398, 88);
    G2.title := 'GBeta';
    G2.font.Style := [fsItalic];
    G2.Draw;
    DInjection1.Next := G2;

    BlockDiagram.canvas.Pen.Color := clBlue;

    cG := TConnectionClass.Create;
    cG.blockDiagram := BlockDiagram;
    cG.sourceObject := G1;
    cG.sourceAnchor := rightmiddle;
    cG.drainObject := G2;
    cG.drainAnchor := leftmiddle;
    cG.chirality := cright;
    cG.title := 'G(t)';
    cG.font.Style := [fsItalic];
    TConnectionClass(cG).TextMargin := 7;
    TConnectionClass(cG).TextPosition := topmiddle;
    cG.Draw;
    G2.Next := cG;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    G5 := TPClass.Create;
    G5.blockDiagram := BlockDiagram;
    SetRect(G5.boundsRect, 110, 280, 170, 316);
    G5.title := 'GE';
    G5.font.Style := [fsItalic];
    G5.Draw;
    cG.Next := G5;

    VTypeAllostery := TSigmaClass.Create;
    VTypeAllostery.blockDiagram := BlockDiagram;
    SetRect(VTypeAllostery.boundsRect, 10, 278, 50, 318);
    VTypeAllostery.Draw;
    G5.Next := VTypeAllostery;

    cN := TConnectionClass.Create;
    cN.blockDiagram := BlockDiagram;
    cN.sourceObject := G5;
    cN.sourceAnchor := leftmiddle;
    cN.drainObject := VTypeAllostery;
    cN.drainAnchor := rightmiddle;
    cN.chirality := cright;
    cN.title := 'N(t)';
    cN.font.Style := [fsItalic];
    TConnectionClass(cN).TextMargin := 7;
    TConnectionClass(cN).TextPosition := bottommiddle;
    cN.Draw;
    VTypeAllostery.Next := cN;

    cV := TConnectionClass.Create;
    cV.blockDiagram := BlockDiagram;
    cV.sourceObject := VTypeAllostery;
    cV.sourceAnchor := topmiddle;
    cV.drainObject := Controller;
    cV.drainAnchor := bottommiddle;
    cV.Draw;
    cN.Next := cV;

    BlockDiagram.canvas.Pen.Color := clBlue;

    j1 := TJunctionClass.Create;
    j1.blockDiagram := BlockDiagram;
    SetRect(j1.boundsRect, 239, 52, 299, 88);
    j1.Draw;
    cV.Next := j1;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    DBeta := TTerminalClass.Create;
    DBeta.blockDiagram := BlockDiagram;
    SetRect(DBeta.boundsRect, 339, 175, 399, 180);
    DBeta.title := 'DBeta';
    DBeta.font.Style := [fsItalic];
    TTerminalClass(DBeta).TextMargin := 5;
    TTerminalClass(DBeta).TextPosition := rightmiddle;
    DBeta.Draw;
    j1.Next := DBeta;

    BlockDiagram.canvas.Pen.Color := clBlue;

    cD11 := TConnectionClass.Create;
    cD11.blockDiagram := BlockDiagram;
    cD11.sourceObject := j1;
    cD11.sourceAnchor := bottommiddle;
    cD11.drainObject := DInjection1;
    cD11.drainAnchor := leftmiddle;
    cD11.Draw;
    DBeta.Next := cD11;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    cD1 := TConnectionClass.Create;
    cD1.blockDiagram := BlockDiagram;
    cD1.sourceObject := DBeta;
    cD1.sourceAnchor := topmiddle;
    cD1.drainObject := DInjection1;
    cD1.drainAnchor := bottommiddle;
    cD1.Draw;
    cD11.Next := cD1;

    cD12 := TConnectionClass.Create;
    cD12.blockDiagram := BlockDiagram;
    cD12.sourceObject := DInjection1;
    cD12.sourceAnchor := rightmiddle;
    cD12.drainObject := MiMeCollector1;
    cD12.drainAnchor := bottommiddle;
    cD12.Draw;
    cD11.Next := cD12;

    cG1 := TConnectionClass.Create;
    cG1.blockDiagram := BlockDiagram;
    cG1.sourceObject := G2;
    cG1.sourceAnchor := rightmiddle;
    cG1.drainObject := MiMeCollector1;
    cG1.drainAnchor := leftmiddle;
    cG1.Draw;
    cD12.Next := cG1;

    BlockDiagram.canvas.Pen.Color := clRed;

    cS := TConnectionClass.Create;
    cS.blockDiagram := BlockDiagram;
    cS.sourceObject := MiMeCollector1;
    cS.sourceAnchor := rightmiddle;
    cS.drainObject := G3;
    cS.drainAnchor := topmiddle;
    cS.chirality := cright;
    cS.title := 'S(t)';
    cS.font.Style := [fsItalic];
    TConnectionClass(cS).TextMargin := 7;
    TConnectionClass(cS).TextPosition := rightmiddle;
    cS.Draw;
    cG1.Next := cS;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    x1 := TTerminalClass.Create;
    x1.blockDiagram := BlockDiagram;
    SetRect(x1.boundsRect, 21, 340, 41, 350);
    x1.title := '1';
    TTerminalClass(x1).TextMargin := 5;
    TTerminalClass(x1).TextPosition := rightmiddle;
    x1.Draw;
    cS.Next := x1;

    c1 := TConnectionClass.Create;
    c1.blockDiagram := BlockDiagram;
    c1.sourceObject := x1;
    c1.sourceAnchor := topmiddle;
    c1.drainObject := VTypeAllostery;
    c1.drainAnchor := bottommiddle;
    c1.Draw;
    x1.Next := c1;

    G4 := TPClass.Create;
    G4.blockDiagram := BlockDiagram;
    SetRect(G4.boundsRect, 378, 280, 438, 316);
    G4.title := 'GR';
    G4.font.Style := [fsItalic];
    G4.Draw;
    c1.Next := G4;

    DInjection2 := TSigmaClass.Create;
    DInjection2.blockDiagram := BlockDiagram;
    SetRect(DInjection2.boundsRect, 378, 225, 438, 261);
    DInjection2.Draw;
    G4.Next := DInjection2;

    MiMeCollector2 := TPiClass.Create;
    MiMeCollector2.blockDiagram := BlockDiagram;
    SetRect(MiMeCollector2.boundsRect, 288, 280, 348, 316);
    TInvertableClass(MiMeCollector2).invertedSegments := [topSegment];
    MiMeCollector2.Draw;
    DInjection2.Next := MiMeCollector2;

    BlockDiagram.canvas.Pen.Color := clRed;

    cI := TConnectionClass.Create;
    cI.blockDiagram := BlockDiagram;
    cI.sourceObject := G3;
    cI.sourceAnchor := bottommiddle;
    cI.drainObject := G4;
    cI.drainAnchor := rightmiddle;
    cI.chirality := cright;
    cI.title := 'I(t)';
    cI.font.Style := [fsItalic];
    cI.Draw;
    MiMeCollector2.Next := cI;

    j2 := TJunctionClass.Create;
    j2.blockDiagram := BlockDiagram;
    SetRect(j2.boundsRect, 478, 280, 508, 316);
    j2.Draw;
    cI.Next := j2;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    DR := TTerminalClass.Create;
    DR.blockDiagram := BlockDiagram;
    SetRect(DR.boundsRect, 379, 185, 439, 221);
    DR.title := 'DR';
    DR.font.Style := [fsItalic];
    TTerminalClass(DR).TextMargin := 5;
    TTerminalClass(DR).TextPosition := leftmiddle;
    DR.Draw;
    j2.Next := DR;

    cD2 := TConnectionClass.Create;
    cD2.blockDiagram := BlockDiagram;
    cD2.sourceObject := DR;
    cD2.sourceAnchor := bottommiddle;
    cD2.drainObject := DInjection2;
    cD2.drainAnchor := topmiddle;
    cD2.Draw;
    DR.Next := cD2;

    BlockDiagram.canvas.Pen.Color := clRed;

    cD21 := TConnectionClass.Create;
    cD21.blockDiagram := BlockDiagram;
    cD21.sourceObject := j2;
    cD21.sourceAnchor := topmiddle;
    cD21.drainObject := DInjection2;
    cD21.drainAnchor := rightmiddle;
    cD21.Draw;
    cD2.Next := cD21;

    cD22 := TConnectionClass.Create;
    cD22.blockDiagram := BlockDiagram;
    cD22.sourceObject := DInjection2;
    cD22.sourceAnchor := leftmiddle;
    cD22.drainObject := MiMeCollector2;
    cD22.drainAnchor := topmiddle;
    cD22.Draw;
    cD21.Next := cD22;

    cI1 := TConnectionClass.Create;
    cI1.blockDiagram := BlockDiagram;
    cI1.sourceObject := G4;
    cI1.sourceAnchor := leftmiddle;
    cI1.drainObject := MiMeCollector2;
    cI1.drainAnchor := rightmiddle;
    cI1.Draw;
    cD22.Next := cI1;

    cM := TConnectionClass.Create;
    cM.blockDiagram := BlockDiagram;
    cM.sourceObject := MiMeCollector2;
    cM.sourceAnchor := leftmiddle;
    cM.drainObject := G5;
    cM.drainAnchor := rightmiddle;
    cM.title := 'M(t)';
    cM.font.Style := [fsItalic];
    cM.Draw;
    cI1.Next := cM;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    IPSImage.Canvas.Draw(0, 0, IPSBitmap);
  finally
    IPSBitmap.Free;
    BlockDiagram.Destroy;
  end;
end;

procedure TIPSForm.FormCreate(Sender: TObject);
begin
  {$IFDEF Darwin}
  OnPaint := @DrawIPS;
  {$ELSE}
  DrawIPS(Sender);
  {$ENDIF}
end;

end.

