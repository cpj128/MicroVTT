unit ContentManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
           
const
  MAPLIBFILE = 'Maps.txt';
  TOKENLIBFILE = 'Tokens.txt';
  OVERLAYLIBFILE = 'Overlays.txt';
  EMITTERLIBFILE = 'Emitters.txt';

type
  TContentManager = class
  private
    FMapLib,
    FTokenLib,
    FOverlayLib,
    FEmitterLib: TStringList;
  public
    constructor Create(ContentPath: string);
    destructor Destroy;

    function MapCount: Integer;
    function HasMap(path: string): Boolean;
    function GetMapName(idx: Integer): string;
    function GetMapTitle(path: string): string;
    function GetMapGridData(path: string): string;
    procedure SetMapGridData(path, GridData: string);
    procedure SetMapFullData(path, Data: string);

    function TokenCount: Integer;
    function HasToken(path: string): Boolean;
    function GetTokenName(idx: Integer): string;
    function GetTokenData(path: string): string;
    procedure SetTokenData(path, data: string);

    function OverlayCount: Integer;
    function HasOverlay(path: string): Boolean;
    function GetOverlayName(idx: Integer): string;
    function GetOverlayData(path: string): string;
    procedure SetOverlayData(path, data: string);

    function EmitterCount: Integer;
    function HasEmitter(name: string): Boolean;
    function GetEmitterName(idx: Integer): string;
    function GetEmitterData(name: string): string;
    procedure SetEmitterData(name, data: string);

    procedure SaveData;


    property MapLib: TStringList read FMapLib;
    property TokenLib: TStringList read FTokenLib;
    property OverlayLib: TStringList read FOverlayLib;
  end;

var
  ContentLib: TContentManager;

implementation

constructor TContentManager.Create(ContentPath: string);
begin
  FMapLib := TStringList.Create;
  if FileExists(ContentPath + MAPLIBFILE) then
    FMapLib.LoadFromFile(ContentPath + MAPLIBFILE)
  else
    FMapLib.SaveToFile(ContentPath + MAPLIBFILE);
  FTokenLib := TStringList.Create;
  if FileExists(ContentPath + TOKENLIBFILE) then
    FTokenLib.LoadFromFile(ContentPath + TOKENLIBFILE)
  else
    FTokenlib.SaveToFile(ContentPath + TOKENLIBFILE);
  FOverlayLib := TStringList.Create;
  if FileExists(ContentPath + OVERLAYLIBFILE) then
    FOverlayLib.LoadFromFile(ContentPath + OVERLAYLIBFILE)
  else
    FOverlayLib.SaveToFile(ContentPath + OVERLAYLIBFILE);
  FEmitterLib := TStringList.Create;
  if FileExists(ContentPath + EMITTERLIBFILE) then
    FEmitterLib.LoadFromFile(ContentPath + EMITTERLIBFILE)
  else
    FEmitterLib.SaveToFile(ContentPath + EMITTERLIBFILE);
end;

destructor TContentManager.Destroy;
begin
  FMapLib.Free;
  FTokenLib.Free;
  FOverlayLib.Free;
  FEmitterLib.Free;
end;

procedure TContentManager.SaveData;
begin
  FMapLib.SaveToFile(MAPLIBFILE);
  FTokenLib.SaveToFile(TOKENLIBFILE);
  FOverlayLib.SaveToFile(OVERLAYLIBFILE);
  FEmitterLib.SaveToFile(EMITTERLIBFILE);
end;

function TContentManager.MapCount: Integer;
begin
  Result := FMapLib.Count;
end;

function TContentManager.HasMap(path: string): Boolean;
begin
  Result := FMapLib.IndexOfName(path) >= 0;
end;

function TContentManager.GetMapName(idx: Integer): string;
begin
  Result := '';
  if (idx >= 0) and (idx < FMapLib.Count) then
    Result := FMapLib.Names[idx];
end;

function TContentManager.GetMapTitle(path: string): string;
var list: TStringList;
begin
  Result := ExtractFileName(path);
  if not HasMap(path) then
    Exit;
  list := TStringList.Create;
  list.Delimiter := '|';
  list.StrictDelimiter := True;
  try
    list.DelimitedText := FMapLib.Values[path];
    if list.Count > 0 then
      Result := list[0];
  finally
    list.Free;
  end;
end;

function TContentManager.GetMapGridData(path: string): string;
var list: TStringList;
begin
  Result := '';
  if not HasMap(path) then
    Exit;
  list := TStringList.Create;
  list.Delimiter := '|';
  list.StrictDelimiter := True;
  try
    list.DelimitedText := FMapLib.Values[path];
    if list.Count > 1 then
      Result := list[1];
  finally
    list.Free;
  end;
end;

procedure TContentManager.SetMapGridData(path, GridData: string);
var list: TStringList;
begin
  if not HasMap(path) then
    Exit;
  list := TStringList.Create;
  list.Delimiter := '|';
  list.StrictDelimiter := True;
  try
    list.DelimitedText := FMapLib.Values[path];
    if list.Count > 1 then
    begin
      list[1] := GridData;
      FMapLib.Values[path] := list.DelimitedText;
    end;
  finally
    list.Free;
  end;
end;

procedure TContentManager.SetMapFullData(path, Data: string);
begin
  if not HasMap(path) then
    Exit;
  FMapLib.Values[path] := Data;
end;

function TContentManager.TokenCount: Integer;
begin
  Result := FTokenLib.Count;
end;

function TContentManager.HasToken(path: string): Boolean;
begin
  Result := FTokenLib.IndexOfName(path) >= 0;
end;

function TContentManager.GetTokenName(idx: Integer): string;
begin                       
  Result := '';
  if (idx >= 0) and (idx < FTokenLib.Count) then
    Result := FTokenLib.Names[idx];
end;

function TContentManager.GetTokenData(path: string): string;  
begin
  Result := FTokenLib.Values[path];
end;

procedure TContentManager.SetTokenData(path, data: string);
begin
  FTokenLib.Values[path] := data;
end;

function TContentManager.OverlayCount: Integer;
begin
  Result := FOverlayLib.Count;
end;

function TContentManager.HasOverlay(path: string): Boolean;
begin
  Result := FOverlayLib.IndexOfName(path) >= 0;
end;

function TContentManager.GetOverlayName(idx: Integer): string;
begin
  Result := '';
  if (idx >= 0) and (idx < FOverlayLib.Count) then
    Result := FOverlayLib.Names[idx];
end;

function TContentManager.GetOverlayData(path: string): string;
begin
  Result := FOverlayLib.Values[path];
end;

procedure TContentManager.SetOverlayData(path, data: string);
begin
  FOverlayLib.Values[path] := data;
end;

function TContentManager.EmitterCount: Integer;
begin
  Result := FEmitterLib.Count;
end;

function TContentManager.HasEmitter(name: string): Boolean;
begin
  Result := FEmitterLib.IndexOfName(name) >= 0;
end;

function TContentManager.GetEmitterName(idx: Integer): string;
begin
  Result := '';
  if (idx >= 0) and (idx < FEmitterLib.Count) then
    Result := FEmitterLib.Names[idx];
end;

function TContentManager.GetEmitterData(name: string): string;
begin
  Result := FEmitterLib.Values[name];
end;

procedure TContentManager.SetEmitterData(name, data: string);
begin
  FEmitterLib.Values[name] := data;
end;

end.

