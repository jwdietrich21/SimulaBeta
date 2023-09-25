unit ScenarioHandler;

{ SimulaBeta }

{ A simulator for insulin-glucose homeostasis }
{ This unit reads and writes scenarios as XML files }

{ Version 3.2.0 (Donostia) }

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
  Classes, SysUtils, DateUtils, DOM, XMLRead, XMLWrite, 
  URIParser, Forms, SimulaBetaGUIServices, EnvironmentInfo, SimulaBetaResources,
  SimulaBetaBaseServices, SimulaBetaTypes;

function NewScenario: TModel;
function emptyModel: TModel;
procedure ReadScenario(theFileName: string; var modelVersion: Str13);
procedure SaveScenario(theFileName: string);


implementation

function ValidFormat(theStream: TStream; const theBaseURI: ansistring): boolean;
const
  SIGNATURE_1 = '<?xml version="1.';
  SIGNATURE_2 = '<scenario';
  SIGNATURE_3 = '</scenario>';
var
  origString, lowerString: ansistring;
begin
  Result := False;
  if theStream.Size > 0 then
  begin
    SetLength(origString, theStream.Size);
    theStream.Read(origString[1], theStream.Size);
    if origString <> '' then
    begin
      lowerString := LowerCase(origString);
      if LeftStr(lowerString, 17) = SIGNATURE_1 then
        if pos(SIGNATURE_2, lowerString) <> 0 then
          if pos(SIGNATURE_3, lowerString) <> 0 then
            Result := True;
    end;
  end;
end;

function ValidFormat(theFileName: string): boolean;
var
  theStream: TStream;
begin
  theStream := TFileStream.Create(theFileName, fmOpenRead + fmShareDenyWrite);
  try
    Result := ValidFormat(theStream, FilenameToURI(theFileName));
  finally
    if theStream <> nil then
      theStream.Free;
  end;
end;

function NewScenario: TModel;
begin
  result := emptyModel;
end;

function emptyModel: TModel;
begin
  result.Name := kSTANDARD_MODEL_NAME;
  result.Reference := kSTANDARD_MODEL_REFERENCE;
  result.Species := kSTANDARD_MODEL_SPECIES;
  result.Creators := kSTANDARD_MODEL_CREATORS;
  result.Created := EncodeDateTime(kSTANDARD_MODEL_CREATED_Y, kSTANDARD_MODEL_CREATED_M, kSTANDARD_MODEL_CREATED_D, kSTANDARD_MODEL_CREATED_H, kSTANDARD_MODEL_CREATED_N, kSTANDARD_MODEL_CREATED_S, 0);
  result.LastModified := EncodeDateTime(kSTANDARD_MODEL_MODIFIED_Y, kSTANDARD_MODEL_MODIFIED_M, kSTANDARD_MODEL_MODIFIED_D, kSTANDARD_MODEL_MODIFIED_H, kSTANDARD_MODEL_MODIFIED_N, kSTANDARD_MODEL_MODIFIED_S, 0);
  result.Terms := kSTANDARD_MODEL_TERMS;
  result.Iterations := 0;
end;

procedure ReadScenario(theFileName: string; var modelVersion: Str13);
{reads a simulation scenario}
var
  i: integer;
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  oldSep: char;
  standardDate: TDateTime;
begin
  if FileExists(theFileName) then
    if ValidFormat(theFileName) then
    begin
      oldSep := DefaultFormatSettings.DecimalSeparator;
      DefaultFormatSettings.DecimalSeparator := kPERIOD;
      try
        standardDate := EncodeDateTime(1904, 01, 01, 00, 00, 00, 00);
        ReadXMLFile(Doc, theFileName);
        if assigned(Doc) then
          RootNode := Doc.DocumentElement;
        if assigned(RootNode) and RootNode.HasAttributes and (RootNode.Attributes.Length > 0) then
          for i := 0 to RootNode.Attributes.Length - 1 do
            with RootNode.Attributes[i] do
            begin
              if NodeName = 'modelversion' then
                modelVersion := UTF8Encode(NodeValue);
            end;
        RootNode := Doc.DocumentElement.FindNode('MIRIAM');
        if assigned(RootNode) then
        begin
          gActiveModel.Name := NodeContent(RootNode, 'Name');
          gActiveModel.Reference := NodeContent(RootNode, 'Reference');
          gActiveModel.Species := NodeContent(RootNode, 'Species');
          gActiveModel.Creators := NodeContent(RootNode, 'Creators');
          if not TryXMLDateTime2DateTime(NodeContent(RootNode, 'Created'),
            gActiveModel.Created) then
            gActiveModel.Created := standardDate;
          if not TryXMLDateTime2DateTime(NodeContent(RootNode, 'LastModified'),
            gActiveModel.LastModified) then
            gActiveModel.LastModified := standardDate;
          gActiveModel.Terms := NodeContent(RootNode, 'Terms');
        end;
        RootNode := Doc.DocumentElement.FindNode('MIASE');
        if assigned(RootNode) then
        begin
          gActiveModel.Code := NodeContent(RootNode, 'Code');
          gActiveModel.Comments := NodeContent(RootNode, 'Comments');
        end;
        if gActiveModel.Code = '' then
          gActiveModel.Code := MIASE_SIMTHYR_STANDARD_CODE;
        if (modelVersion = '') or (LeftStr(modelVersion, 2) = '3.') then
        begin
          RootNode := Doc.DocumentElement.FindNode('strucpars');
          if assigned(RootNode) then
          begin
            VarFromNode(RootNode, 'alphaG', gActiveModel.StrucPars.alphaG);
            VarFromNode(RootNode, 'alphaI', gActiveModel.StrucPars.alphaI);
            VarFromNode(RootNode, 'betaI', gActiveModel.StrucPars.betaI);
            VarFromNode(RootNode, 'GBeta', gActiveModel.StrucPars.GBeta);
            VarFromNode(RootNode, 'DBeta', gActiveModel.StrucPars.DBeta);
            VarFromNode(RootNode, 'GR', gActiveModel.StrucPars.GR);
            VarFromNode(RootNode, 'DR', gActiveModel.StrucPars.DR);
            VarFromNode(RootNode, 'GE', gActiveModel.StrucPars.GE);
          end;
        end
        else
          ShowVersionError;
      finally
        if assigned(Doc) then
          Doc.Free;
      end;
      {$IFDEF GUI}
      if AnnotationForm.Visible then
        AnnotationForm.ShowAnnotation(gActiveModel);
      {$ENDIF}
      DefaultFormatSettings.DecimalSeparator := oldSep;
    end
    else
      ShowFileError;
end;

procedure SaveScenario(theFileName: string); {saves scenario as XML file}
var
  oldSep: char;
  Doc: TXMLDocument;
  RootNode, ElementNode: TDOMNode;
  theDate: ansistring;
begin
  oldSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  try
    Doc := TXMLDocument.Create;

    RootNode := Doc.CreateElement('scenario');
    TDOMElement(RootNode).SetAttribute('modelversion', '3.0');
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;

    ElementNode := Doc.CreateElement('MIRIAM');
    ElementNode.AppendChild(SimpleNode(Doc, 'Name', gActiveModel.Name));
    ElementNode.AppendChild(SimpleNode(Doc, 'Reference', gActiveModel.Reference));
    ElementNode.AppendChild(SimpleNode(Doc, 'Species', gActiveModel.Species));
    ElementNode.AppendChild(SimpleNode(Doc, 'Creators', gActiveModel.Creators));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, gActiveModel.Created);
    ElementNode.AppendChild(SimpleNode(Doc, 'Created', theDate));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, gActiveModel.LastModified);
    ElementNode.AppendChild(SimpleNode(Doc, 'LastModified', theDate));
    ElementNode.AppendChild(SimpleNode(Doc, 'Terms', gActiveModel.Terms));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('MIASE');
    if gActiveModel.Code = '' then
      gActiveModel.Code := MIASE_SIMTHYR_STANDARD_CODE;
    ElementNode.AppendChild(SimpleNode(Doc, 'Code', gActiveModel.Code));
    ElementNode.AppendChild(SimpleNode(Doc, 'Comments', gActiveModel.Comments));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('strucpars');
    ElementNode.AppendChild(SimpleNode(Doc, 'alphaG', FloatToStr(gActiveModel.StrucPars.alphaG, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'betaG', FloatToStr(gActiveModel.StrucPars.betaG, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'alphaI', FloatToStr(gActiveModel.StrucPars.alphaI, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'betaI', FloatToStr(gActiveModel.StrucPars.betaI, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GBeta', FloatToStr(gActiveModel.StrucPars.GBeta, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'DBeta', FloatToStr(gActiveModel.StrucPars.DBeta, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GR', FloatToStr(gActiveModel.StrucPars.GR, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'DR', FloatToStr(gActiveModel.StrucPars.DR, gUSFormatSettings)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GE', FloatToStr(gActiveModel.StrucPars.GE, gUSFormatSettings)));
    RootNode.AppendChild(ElementNode);

    if length(gActiveModel.EventMatrix) > 0
    then
    begin
      ElementNode := Doc.CreateElement('sequence');
      RootNode.AppendChild(ElementNode);
    end;

    WriteXMLFile(Doc, theFileName);
  finally
    Doc.Free;
  end;
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

end.
