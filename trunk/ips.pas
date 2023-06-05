unit IPS;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ Information Processing Structure }

{ Version 3.1.2 (Challenger) }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lclintf, SystemsDiagram, SimulaBetaGUIServices;

const
  clOrangeW = TColor($0024B5FF);

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
  Controller, WInjection, DInjection1, MiMeCollector1, DInjection2: TIPSClass;
  MiMeCollector2:  TIPSClass;
  VTypeAllostery: TIPSClass;
  G1, G2, G3, G4, G5, xt, wt, zt, DBeta, DR, x1, j1, j2: TIPSClass;
  c1, cP, cQ, cW, cZ, cR, cG, cD1, cD11, cD12, cS, cI, cM, cN, cV, cG1: TConnectionClass;
  cI1, cD2, cD21, cD22: TConnectionClass;
  DrawColour: TColor;
  sf: Double;
begin
  //sf := Self.GetCanvasScaleFactor; // for future support of Retina displays (partly functional)
  sf := 1;
  BlockDiagram := TBlockDiagram.Create;
  IPSBitmap := TBitmap.Create;
  try
    IPSBitmap.Height := IPSImage.Height;
    IPSBitmap.Width := IPSImage.Width;
    IPSBitmap.SetSize(round(IPSImage.Width * sf), round(IPSImage.Height * sf));
    IPSBitmap.Canvas.Font.PixelsPerInch := Round(Self.PixelsPerInch * sf);;
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
    SetRect(xt.boundsRect, round(20 * sf), round(50 * sf), round(60 * sf), round(90 * sf));
    xt.title := 'P(t)';
    xt.font.Style := [fsItalic];
    TTerminalClass(xt).TextMargin := 5;
    TTerminalClass(xt).TextPosition := leftmiddle;
    xt.Draw;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    Controller := TPiClass.Create;
    Controller.blockDiagram := BlockDiagram;
    SetRect(Controller.boundsRect, round(70 * sf), round(50 * sf), round(100 * sf), round(90 * sf));
    TInvertableClass(Controller).invertedSegments := [bottomSegment];
    Controller.Draw;
    xt.Next := Controller;

    BlockDiagram.canvas.Pen.Color := clGreen;

    cP := TConnectionClass.Create;
    cP.blockDiagram := BlockDiagram;
    cP.sourceObject := xt;
    cP.sourceAnchor := rightmiddle;
    cP.drainObject := Controller;
    cP.drainAnchor := leftmiddle;
    cP.Draw;
    Controller.Next := cP;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    WInjection := TSigmaClass.Create;
    WInjection.blockDiagram := BlockDiagram;
    SetRect(WInjection.boundsRect, round(150 * sf), round(52 * sf), round(210 * sf), round(88 * sf));
    WInjection.Draw;
    cP.Next := WInjection;

    BlockDiagram.canvas.Pen.Color := clOrangeW;

    wt := TTerminalClass.Create;
    wt.blockDiagram := BlockDiagram;
    SetRect(wt.boundsRect, round(151 * sf), round(20 * sf), round(211 * sf), round(30 * sf));
    wt.title := 'W(t)';
    wt.font.Style := [fsItalic];
    TTerminalClass(wt).TextMargin := 5;
    TTerminalClass(wt).TextPosition := topmiddle;
    wt.Draw;
    WInjection.Next := wt;

    cW := TConnectionClass.Create;
    cW.blockDiagram := BlockDiagram;
    cW.sourceObject := wt;
    cW.sourceAnchor := bottommiddle;
    cW.drainObject := Winjection;
    cW.drainAnchor := topmiddle;
    cW.Draw;
    wt.Next := cW;

    BlockDiagram.canvas.Pen.Color := clBlue;

    cQ := TConnectionClass.Create;
    cQ.blockDiagram := BlockDiagram;
    cQ.sourceObject := Controller;
    cQ.sourceAnchor := rightmiddle;
    cQ.drainObject := Winjection;
    cQ.drainAnchor := leftmiddle;
    cQ.title := 'Q(t)';
    cQ.font.Style := [fsItalic];
    TConnectionClass(cQ).TextMargin := 7;
    TConnectionClass(cQ).TextPosition := topmiddle;
    cQ.Draw;
    cW.Next := cQ;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    G1 := TPClass.Create;
    G1.blockDiagram := BlockDiagram;
    SetRect(G1.boundsRect, round(250 * sf), round(52 * sf), round(310 * sf), round(88 * sf));
    G1.title := 'G1 (ASIA)';
    G1.font.Style := [fsItalic];
    G1.Draw;
    cQ.Next := G1;

    BlockDiagram.canvas.Pen.Color := clBlue;

    cR := TConnectionClass.Create;
    cR.blockDiagram := BlockDiagram;
    cR.sourceObject := WInjection;
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
    SetRect(MiMeCollector1.boundsRect, round(480 * sf), round(52 * sf), round(530 * sf), round(88 * sf));
    TInvertableClass(MiMeCollector1).invertedSegments := [bottomSegment];
    MiMeCollector1.Draw;
    cR.Next := MiMeCollector1;

    G3 := TPClass.Create;
    G3.blockDiagram := BlockDiagram;
    SetRect(G3.boundsRect, round(570 * sf), round(180 * sf), round(630 * sf), round(216 * sf));
    G3.title := 'G3 (ASIA)';
    G3.font.Style := [fsItalic];
    G3.Draw;
    MiMeCollector1.Next := G3;

    DInjection1 := TSigmaClass.Create;
    DInjection1.blockDiagram := BlockDiagram;
    SetRect(DInjection1.boundsRect, round(388 * sf), round(105 * sf), round(448 * sf), round(145 * sf));
    DInjection1.Draw;
    G3.Next := DInjection1;

    G2 := TPClass.Create;
    G2.blockDiagram := BlockDiagram;
    SetRect(G2.boundsRect, round(388 * sf), round(52 * sf), round(448 * sf), round(88 * sf));
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
    SetRect(G5.boundsRect, round(160 * sf), round(280 * sf), round(230 * sf), round(316 * sf));
    G5.title := 'GE';
    G5.font.Style := [fsItalic];
    G5.Draw;
    cG.Next := G5;

    VTypeAllostery := TSigmaClass.Create;
    VTypeAllostery.blockDiagram := BlockDiagram;
    SetRect(VTypeAllostery.boundsRect, round(65 * sf), round(278 * sf), round(105 * sf), round(318 * sf));
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
    SetRect(j1.boundsRect, round(319 * sf), round(52 * sf), round(379 * sf), round(88 * sf));
    j1.Draw;
    cV.Next := j1;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    DBeta := TTerminalClass.Create;
    DBeta.blockDiagram := BlockDiagram;
    SetRect(DBeta.boundsRect, round(389 * sf), round(175 * sf), round(449 * sf), round(180 * sf));
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
    cD1.Next := cD12;

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
    SetRect(x1.boundsRect, round(76 * sf), round(340 * sf), round(96 * sf), round(350 * sf));
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
    SetRect(G4.boundsRect, round(428 * sf), round(280 * sf), round(488 * sf), round(316 * sf));
    G4.title := 'GR';
    G4.font.Style := [fsItalic];
    G4.Draw;
    c1.Next := G4;

    DInjection2 := TSigmaClass.Create;
    DInjection2.blockDiagram := BlockDiagram;
    SetRect(DInjection2.boundsRect, round(428 * sf), round(225 * sf), round(488 * sf), round(261 * sf));
    DInjection2.Draw;
    G4.Next := DInjection2;

    MiMeCollector2 := TPiClass.Create;
    MiMeCollector2.blockDiagram := BlockDiagram;
    SetRect(MiMeCollector2.boundsRect, round(338 * sf), round(280 * sf), round(398 * sf), round(316 * sf));
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
    TConnectionClass(cI).TextMargin := 7;
    TConnectionClass(cI).TextPosition := bottommiddle;
    cI.Draw;
    MiMeCollector2.Next := cI;

    j2 := TJunctionClass.Create;
    j2.blockDiagram := BlockDiagram;
    SetRect(j2.boundsRect, round(528 * sf), round(280 * sf), round(548 * sf), round(316 * sf));
    j2.Draw;
    cI.Next := j2;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    DR := TTerminalClass.Create;
    DR.blockDiagram := BlockDiagram;
    SetRect(DR.boundsRect, round(429 * sf), round(185 * sf), round(489 * sf), round(221 * sf));
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

    BlockDiagram.canvas.Pen.Color := clSkyBlue;

    zt := TTerminalClass.Create;
    zt.blockDiagram := BlockDiagram;
    SetRect(zt.boundsRect, round(481 * sf), round(20 * sf), round(530 * sf), round(30 * sf));
    zt.title := 'Z(t)';
    zt.font.Style := [fsItalic];
    TTerminalClass(zt).TextMargin := 5;
    TTerminalClass(zt).TextPosition := topmiddle;
    zt.Draw;
    cM.Next := zt;

    cZ := TConnectionClass.Create;
    cZ.blockDiagram := BlockDiagram;
    cZ.sourceObject := zt;
    cZ.sourceAnchor := bottommiddle;
    cZ.drainObject := MiMeCollector1;
    cZ.drainAnchor := topmiddle;
    cZ.Draw;
    zt.Next := cZ;

    BlockDiagram.canvas.Pen.Color := DrawColour;

    IPSImage.Canvas.Draw(0, 0, IPSBitmap);
    // For High DPI / Retina:
    //Self.Canvas.StretchDraw(Rect(0, 0, IPSImage.Width, IPSImage.Height), IPSBitmap);
  finally
    IPSBitmap.Free;
    BlockDiagram.Destroy;
  end;
end;

procedure TIPSForm.FormCreate(Sender: TObject);
begin
  Left := 0;
  Top := Screen.Height - Height - 13 - 65;

  {$IFDEF Darwin}
  OnPaint := @DrawIPS;
  {$ELSE}
  DrawIPS(Sender);
  {$ENDIF}
end;

end.

