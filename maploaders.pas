{Copyright (c) 2023 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit MapLoaders;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, WallManager;

type
  TMapLoader = class
  public
    function LoadFromFile(filename: string): TBGRABitmap; virtual; abstract;
    function LoadWalls(filename: string; WallMgr: TWallManager): Integer; virtual; abstract;
    function LoadLights(filename: string; TokenList: TList): Integer; virtual; abstract;
  end;

  TMapLoaderImg = class(TMapLoader)
  public
    function LoadFromFile(filename: string): TBGRABitmap; override;
    function LoadWalls(filename: string; WallMgr: TWallManager): Integer; override;
    function LoadLights(filename: string; TokenList: TList): Integer; override;
  end;

  TMapLoaderUniversalVTT = class(TMapLoader)
  public
    function LoadFromFile(filename: string): TBGRABitmap; override;
    function LoadWalls(filename: string; WallMgr: TWallManager): Integer; override;
    function LoadLights(filename: string; TokenList: TList): Integer; override;
  end;

function GetMapImage(filename: string): TBGRABitmap;
function GetMapLoader(filename: string): TMapLoader;

//procedure ImportToDB(filename: string);

implementation

uses
  Base64,
  //BGRABitmapTypes,
  fpjson,
  jsonparser,
  StrUtils,
  BGRABitmapTypes,
  DisplayConst,
  RPGTypes;

function GetMapImage(filename: string): TBGRABitmap;
var
  fileext: string;
  loader: TMapLoader;
begin
  if not FileExists(filename) then
    Exit(nil);

  loader := GetMapLoader(filename);
  if Assigned(Loader) then
  begin
    Result := loader.LoadFromFile(filename);
    Loader.Free;
  end;
end;

function GetMapLoader(filename: string): TMapLoader;
var
  fileext: string;
begin
  Result := nil;
  fileext := ExtractFileExt(filename);
  if AnsiContainsText(PicFilterStr, fileExt) then
    Result := TMapLoaderImg.Create
  else if AnsiContainsText(PICFILTERUVTT, fileExt) then
    Result := TMapLoaderUniversalVTT.Create;
end;

{ TMapLoaderImg }

function TMapLoaderImg.LoadFromFile(filename: string): TBGRABitmap;
begin
  Result := TBGRABitmap.Create(Filename);
end;

function TMapLoaderImg.LoadWalls(filename: string; WallMgr: TWallManager): Integer;
begin
  WallMgr.Clear;
  Result := 0;
end;

function TMapLoaderImg.LoadLights(filename: string; TokenList: TList): Integer;
begin
  Result := 0;
end;

{ TMapLoaderUniversalVTT }

function TMapLoaderUniversalVTT.LoadFromFile(filename: string): TBGRABitmap;
var
  uvttfile: TFileStream;
  FileData: TJSONData;
  ImgString: TStringStream;
  DecodeStream: TBase64DecodingStream;
  MemStream: TMemoryStream;
  tmpStr: string;
begin
  Result := nil;
  uvttFile := TFileStream.Create(filename, fmOpenRead);
  try
    FileData := GetJSON(uvttFile);
  finally
    uvttFile.Free;
  end;
  if Assigned(FileData) then
  begin
    tmpStr := TJSONObject(FileData).Get('image');
    if Length(tmpStr) > 0 then
    begin
      ImgString := TStringStream.Create(tmpStr);
      try
        DecodeStream := TBase64DecodingStream.Create(imgString);
        try
          MemStream := TMemoryStream.Create;
          try
            MemStream.CopyFrom(DecodeStream, DecodeStream.Size);
            MemStream.Position := 0;
            Result := TBGRABitmap.Create(MemStream);
          finally
            MemStream.Free;
          end;
        finally
          DecodeStream.Free;
        end;
      finally
        ImgString.Free;
      end;
    end;
    FileData.Free;
  end;
end;

function TMapLoaderUniversalVTT.LoadWalls(filename: string; WallMgr: TWallManager): Integer;
var
  uvttFile: TFileStream;
  FileData, ResolutionData, LoSData, SubData: TJSONData;
  lineEnum, pntEnum: TJSONEnum;
  jLine: TJSONArray;
  jPnt, jPortal: TJSONObject;
  GridSize: Integer;
  TmpPnt, TmpPnt2, FirstPnt, prevPnt: TPoint;
  IsFirst, tmpOpen: Boolean;
  tmpStr: string;
begin
  Result := 0;
  if not Assigned(WallMgr) then
    Exit;
  uvttFile := TFileStream.Create(filename, fmOpenRead);
  try
    FileData := GetJSON(uvttFile);
  finally
    uvttFile.Free;
  end;
  ResolutionData := FileData.FindPath('resolution');
  GridSize := 50;
  if Assigned(ResolutionData) then
  begin
    SubData := ResolutionData.FindPath('pixels_per_grid');
    if Assigned(SubData) then
      GridSize := SubData.AsInteger;
  end;

  WallMgr.Clear;

  LoSData := FileData.FindPath('line_of_sight');
  if Assigned(LoSData) and (LoSData is TJSONArray) then
  begin

    for lineEnum in TJSONArray(LoSData) do
    begin
      //tmpStr := pntEnum.Value.AsJSON;
      jLine := TJSONArray(lineEnum.Value);
      IsFirst := True;
      PrevPnt := Point(0, 0);
      for pntEnum in jLine do
      begin
        jPnt := TJSONObject(pntEnum.Value);
        tmpPnt := Point(Round(jPnt.Floats['x'] * GridSize), Round(jPnt.Floats['y'] * GridSize));
        if IsFirst then
        begin
          FirstPnt := tmpPnt;
          IsFirst := False;
        end
        else
        begin
          WallMgr.AddWall(PrevPnt, tmpPnt);
        end;
        PrevPnt := TmpPnt;
      end;

    end;
    // Close Loop
    //WallMgr.AddWall(tmpPnt, FirstPnt);
  end;
  LoSData := FileData.FindPath('objects_line_of_sight');
  if Assigned(LoSData) and (LoSData is TJSONArray) then
  begin
    for lineEnum in TJSONArray(LoSData) do
    begin
      //tmpStr := pntEnum.Value.AsJSON;
      jLine := TJSONArray(lineEnum.Value);
      IsFirst := True;
      PrevPnt := Point(0, 0);
      for pntEnum in jLine do
      begin
        jPnt := TJSONObject(pntEnum.Value);
        tmpPnt := Point(Round(jPnt.Floats['x'] * GridSize), Round(jPnt.Floats['y'] * GridSize));
        if IsFirst then
        begin
          FirstPnt := tmpPnt;
          IsFirst := False;
        end
        else
        begin
          WallMgr.AddWall(PrevPnt, tmpPnt);
        end;
        PrevPnt := TmpPnt;
      end;

    end;
    // Close Loop
    //WallMgr.AddWall(tmpPnt, FirstPnt);
  end;
  LoSData := FileData.FindPath('portals');
  if Assigned(LoSData) and (LoSData is TJSONArray) then
  begin

    for lineEnum in TJSONArray(LoSData) do
    begin
      tmpStr := lineEnum.Value.AsJSON;

      jPortal := TJSONObject(lineEnum.Value);
      //jLine := jPortal.FindPath('bounds');
      jPnt := TJSONObject(jPortal.FindPath('bounds[0]'));
      TmpPnt := Point(Round(jPnt.Floats['x'] * GridSize), Round(jPnt.Floats['y'] * GridSize));
      
      jPnt := TJSONObject(jPortal.FindPath('bounds[1]'));
      TmpPnt2 := Point(Round(jPnt.Floats['x'] * GridSize), Round(jPnt.Floats['y'] * GridSize));

      tmpOpen := not jPortal.Booleans['closed'];

      WallMgr.AddPortal(TmpPnt, TmpPnt2, TmpOpen);
    end;
  end;
  FileData.Free;
end;

function TMapLoaderUniversalVTT.LoadLights(filename: string; TokenList: TList): Integer; 
var
  uvttFile: TFileStream;
  FileData, ResolutionData, LightData, SubData: TJSONData;
  lightEnum, pntEnum: TJSONEnum;
  jLine: TJSONArray;
  jPnt, jLight: TJSONObject;
  GridSize: Integer;
  pX, pY, pRange: Integer;
  tmpToken: TLightToken;
  ClrStr: string;
  tmpStr: string;
begin
  if not Assigned(TokenList) then
    Exit; 
  uvttFile := TFileStream.Create(filename, fmOpenRead);
  try
    FileData := GetJSON(uvttFile);
  finally
    uvttFile.Free;
  end;

  ResolutionData := FileData.FindPath('resolution');
  GridSize := 50;
  if Assigned(ResolutionData) then
  begin
    SubData := ResolutionData.FindPath('pixels_per_grid');
    if Assigned(SubData) then
      GridSize := SubData.AsInteger;
  end;
           
  LightData := FileData.FindPath('lights');
  if Assigned(LightData) and (LightData is TJSONArray) then
  begin
    for lightEnum in TJSONArray(LightData) do
    begin
      jLight := TJSONObject(lightEnum.Value);
      tmpStr := jLight.AsJSON;
      pX := -1;
      pY := -1;
      if jLight.Find('position', jPnt) then
      begin
        pX := Round(jPnt.Floats['x'] * GridSize);
        pY := Round(jPnt.Floats['y'] * GridSize);
      end;
      pRange := Round(jLight.Floats['range'] * GridSize);
      if (pX >= 0) and (pY >= 0) then
      begin
        tmpToken := TLightToken.Create(pX, pY, pRange);
        tmpToken.MaxStrength := jLight.Floats['intensity'];
        clrStr := jLight.Strings['color'];
        tmpToken.Color := BGRA(StrToInt('$' + Copy(clrStr, 3, 2)), StrToInt('$' + Copy(clrStr, 5, 2)), StrToInt('$' + Copy(clrStr, 7, 2)), StrToInt('$' + Copy(clrStr, 1, 2)));
        TokenList.Add(tmpToken);
      end;
    end;
  end;

end;

end.

