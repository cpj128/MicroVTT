{Copyright (c) 2023-2026 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit RPGTypes;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, Graphics, SysUtils, BGRABitmap, IniFiles,
  WallManager,
  Particles;
  
 const
  MAXTOKENANIMSTEPS = 20;

type

  TGridType = (gtRect, gtHexH, gtHexV, gtIsometric);

  TGridData = record
    GridSizeX, GridSizeY, GridOffsetX, GridOffsetY: Single;
    GridColor: TColor;
    GridAlpha: Byte;
    GridType: TGridType;
    function ToString: string;
    procedure FromString(str: string);
  end;

  TGridDataWrapper = class
  public
    GridData: TGridData;
    constructor Create;
    constructor Create(data: TGridData);
  end;
var
  DefaultGridData: TGridData =
  ( GridSizeX: 100;
    GridSizeY: 100;
    GridOffsetX: 0;
    GridOffsetY: 0;
    GridColor: clSilver;
    GridAlpha: 255;
    GridType: gtRect;);

type
  TTokenRotationStyle = (rsRotateToken, rsShowArrow);

  TTokenType = (ttCharacter, ttRange, ttText, ttLight, ttParticleEmitter);

  TTokenNodeData = class
    public
      FullPath: string;
      Name: string;
      DefaultWidth, DefaultHeight: Integer;
      DefaultGridSlotsX, DefaultGridSlotsY: Integer;
      DefaultAngle: Single;
      BaseInitiative: Integer;
      TokenType: TTokenType;
  end;


  TToken = class
    private
      FGlyph: TBGRABitmap;
      FPath: string;
      FName: string;
      FBaseInitiative: Integer;
      FXPos, FYPos,
      FXStartPos, FYStartPos,
      FXTargetPos, FYTargetPos,
      FWidth, FHeight: Integer;
      FAngle: Double;
      FVisible: Boolean;
      FGridSlotsX, FGridSlotsY: Integer;
      FCurAnimationStep: Integer;
      FIsMoving: Boolean;
      FAttached: TList;
    protected
      procedure SetXPos(val: Integer); virtual;
      procedure SetYPos(val: integer); virtual; 
      procedure SetWidth(Val: Integer); virtual;
      procedure SetHeight(Val: Integer); virtual;
      function GetXPos: Integer; virtual;
      function GetYPos: Integer; virtual;
      function GetXEndPos: Integer; virtual;
      function GetYEndPos: Integer; virtual;
      function GetAngle: Double; virtual;
      function GetWidth: Integer; virtual;
      function GetHeight: Integer; virtual;
      procedure SetAngle(val: Double); virtual;
      function GetPlayerGlyph: TBGRABitmap; virtual;
    public
      constructor Create(Path: string; X, Y, pWidth, pHeight: Integer);
      destructor Destroy; override;
      procedure RedrawGlyph; virtual; abstract;
      procedure SnapToGrid(GridData: TGridData);
      function GetCellsAtPosition(PosX, PosY: Integer; GridData: TGridData): TRect;
      procedure StartAnimation;
      procedure StopAnimation;
      procedure DoAnimationStep(var NeedsUpdate: Boolean); virtual;
      function GetBoundingRect: TRect;
      procedure Attach(token: TToken);
      function GetAttached(idx: Integer): TToken;
      function AttachedCount: Integer;
      procedure RemoveAttached(token: TToken);
      procedure UpdateAttached;
      procedure SaveToIni(SaveFile: TIniFile; idx: Integer); virtual;
      procedure DrawForMaster(toCanvas: TBGRABitmap); virtual;       
      procedure DrawForPlayer(toCanvas: TBGRABitmap); virtual;
      property XPos: Integer read GetXPos write SetXPos;
      property YPos: Integer read GetYPos write SetYPos;
      property XEndPos: Integer read GetXEndPos;
      property YEndPos: Integer read GetYEndPos;
      property Width: Integer read GetWidth write SetWidth;
      property Height: Integer read GetHeight write SetHeight;
      property Visible: Boolean read FVisible write FVisible;
      property GridSlotsX: Integer read FGridSlotsX write FGridSlotsX;
      property GridSlotsY: Integer read FGridSlotsY write FGridSlotsY;
      property Angle: Double read GetAngle write SetAngle;
      property Glyph: TBGRABitmap read FGlyph;
      property PlayerGlyph: TBGRABitmap read GetPlayerGlyph; // Glyph for player's view. Same as Glyph, unless overridden.
      property IsMoving: Boolean read FIsMoving;
      property Path: string read FPath;
      property Name: string read FName write FName;
      property BaseInitiative: Integer read FBaseInitiative write FBaseInitiative;
  end;

  TTokenFactory = class
    TokensStartInvisible: Boolean; static;
    TokensSnapToGrid: Boolean; static;
    ShowDirectionArrow: Boolean; static;
    GridData: TGridData; static;
    WallManager: TWallManager; static;
    class function CreateTokenFromIni(SaveFile: TIniFile; idx: Integer): TToken; static;
    class function CreateTokenFromNode(data: TTokenNodeData; X, Y: Integer): TToken; static;
  end;

  TCharacterToken = class(TToken)
  private
    FNumber: Integer;   
    FOverlayIdx: Integer;
    FShowArrow: Boolean;
    FShowLoS: Boolean;
    procedure SetOverlayIdx(val: Integer);
    procedure SetNumber(val: Integer);
    procedure SetAngle(val: Double); override;
  public
    procedure RedrawGlyph; override;
    constructor Create(pPath: string; X, Y, pWidth, pHeight: Integer);
    procedure SaveToIni(SaveFile: TIniFile; idx: Integer); override;
    property Number: Integer read FNumber write SetNumber;
    property OverlayIdx: Integer read FOverlayIdx write SetOverlayIdx;
    property ShowArrow: Boolean read FShowArrow write FShowArrow;
    property ShowLoS: Boolean read FShowLoS write FShowLoS;
  end;

  TAttachableToken = class(TToken)
  private
    FAttachedTo: TToken;  
    function GetXPos: Integer; override;
    function GetYPos: Integer; override;
    function GetXEndPos: Integer; override;
    function GetYEndPos: Integer; override;
    function GetAngle: Double; override;
  public
    procedure AttachTo(token: TToken);
    procedure Detach;
    function IsAttached: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToIni(SaveFile: TIniFile; idx: Integer); override;
  end;

  TRangeIndicator = class(TAttachableToken)
  private
    FSectorAngle: Double;
    FColor: TColor;
    FAlpha: Byte;
    procedure SetColor(val: TColor);
    procedure SetAlpha(val: Byte);
    procedure SetSectorAngle(Val: Double);
    procedure SetWidth(Val: Integer); override;
    procedure SetHeight(Val: Integer); override;
    procedure SetAngle(val: Double); override;
  public
    constructor Create(X, Y, pWidth, pHeight: Integer);
    destructor Destroy; override;
    procedure SaveToIni(SaveFile: TIniFile; idx: Integer); override;
    procedure RedrawGlyph; override;
    property Color: TColor read FColor write SetColor;
    property Alpha: Byte read FAlpha write SetAlpha;
    property SectorAngle: Double read FSectorAngle write SetSectorAngle;
  end;

  TTextToken = class(TToken)
  private
    FText: string;
    procedure SetWidth(Val: Integer); override;
    procedure SetHeight(Val: Integer); override;
    procedure SetText(val: string);
  public
    constructor Create(X, Y, pWidth, pHeight: Integer; text: string);
    destructor Destroy; override;
    procedure SaveToIni(SaveFile: TIniFile; idx: Integer); override;
    procedure RedrawGlyph; override;
    property Text: string read FText write SetText;
  end;

  TLightAnimationType = (latNone, latFlicker, latPulse, latFlash); // Another for steady light that turns off on occasion?

  TLightToken = class(TAttachableToken)
  private
    FColor: TColor;
    FRange: Integer;
    FMaxStrength: Double;
    FWallManager: TWallManager;
    FLightPic, FPlayerGlyph: TBGRABitmap;
    FAnimationType: TLightAnimationType;
    FAnimationStep: Integer;
    // TODO: Add Animation speed setting, make accessible in token dialog
    procedure SetColor(val: TColor);
    procedure SetRange(val: Integer);
    procedure SetMaxStrength(val: Double);
    function GetRange: Integer;
    function GetCurrentLightStrength(step: Integer): Double;
  protected   
    procedure SetWidth(Val: Integer); override;
    procedure SetHeight(Val: Integer); override;
    procedure SetXPos(val: Integer); override;
    procedure SetYPos(val: integer); override;  
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetPlayerGlyph: TBGRABitmap; override;
  public
    constructor Create(X, Y, pRange: Integer);
    destructor Destroy; override;
    procedure DoAnimationStep(var NeedsUpdate: Boolean); override;
    procedure SaveToIni(SaveFile: TIniFile; idx: Integer); override;
    procedure RedrawGlyph; override;
    procedure RedrawPlayerGlyph;
    procedure RedrawLightMap;
    property PlayerGlyph: TBGRABitmap read FPlayerGlyph;
    property Range: Integer read GetRange write SetRange;
    property Color: TColor read FColor write SetColor;
    property MaxStrength: Double read FMaxStrength write SetMaxStrength;
    property AnimationType: TLightAnimationType read FAnimationType write FAnimationType;
    property WallManager: TWallManager read FWallManager write FWallManager;
  end;

  TParticleEmmitterToken = class(TToken)

  end;

implementation

uses
  Math,
  BGRABitmapTypes,
  BGRATextFX,
  BGRAGradientScanner,
  DisplayConst,
  RPGUtils;

{ TGridData }

function TGridData.ToString: string;
var
  fs: TFormatSettings;
  list: TStringList;
begin
  Result := '';
  fs := FormatSettings;
  fs.DecimalSeparator := '.';
  list := TStringList.Create;
  try
    list.Delimiter := ';';
    list.StrictDelimiter := True;
    list.Add(FloatToStrF(GridSizeX, ffNumber, 8, 8, fs));
    list.Add(FloatToStrF(GridSizeY, ffNumber, 8, 8, fs));
    list.Add(FloatToStrF(GridOffsetX, ffNumber, 8, 8, fs));
    list.Add(FloatToStrF(GridOffsetY, ffNumber, 8, 8, fs));
    list.Add(IntToStr(GridColor));
    list.Add(IntToStr(GridAlpha));
    list.Add(IntToStr(Ord(GridType)));
    Result := list.DelimitedText;
  finally
    list.Free;
  end;
end;

procedure TGridData.FromString(str: string);
var
  fs: TFormatSettings;
  list: TStringList;
begin
  fs := FormatSettings;
  fs.DecimalSeparator := '.';
  list := TStringList.Create;
  try
    list.Delimiter := ';';
    list.StrictDelimiter := True;
    list.DelimitedText := str;
    // Set default values
    GridSizeX := 100;
    GridSizeY := 100;
    GridOffsetX := 0;
    GridOffsetY := 0;
    GridColor := clSilver;
    GridAlpha := 255;
    GridType := gtRect;

    if list.Count > 0 then
      GridSizeX := StrToFloat(list[0], fs);
    if list.Count > 1 then
      GridSizeY := StrToFloat(list[1], fs);
    if list.Count > 2 then
      GridOffsetX := StrToFloat(list[2], fs);
    if list.Count > 3 then
      GridOffsetY := StrToFloat(list[3], fs);
    if list.Count > 4 then
      GridColor := StrToInt(list[4]);
    if list.Count > 5 then
      GridAlpha := StrToInt(list[5]);
    if list.Count > 6 then
      GridType := TGridType(StrToInt(list[6]));
  finally
    list.Free;
  end;
end;

{ TGridDataWrapper }

constructor TGridDataWrapper.Create;
begin
  inherited Create;
  GridData := DefaultGridData;
end;

constructor TGridDataWrapper.Create(data: TGridData);
begin
  inherited Create;
  GridData := data;
end;

{ TToken }

constructor TToken.Create(Path: string; X, Y, pWidth, pHeight: Integer);
begin
  inherited Create;
  FXPos := X;
  FYPos := Y;
  FXTargetPos := X;
  FYTargetPos := Y; 
  FXStartPos := X;
  FYStartPos := Y;
  FWidth := pWidth;
  FHeight := pHeight;
  FVisible := True;
  FIsMoving := False;
  FGridSlotsX := 1;
  FGridSlotsY := 1;
  FCurAnimationStep := 0;
  FPath := Path;
  FGlyph := TBGRABitmap.Create(Path);
  FAttached := TList.Create;
  RedrawGlyph;
end;

destructor TToken.Destroy;
var
  i, j: Integer;
  tmpToken: TToken;
begin
  j := AttachedCount - 1;
  for i := 0 to j do
  begin
    tmpToken := GetAttached(i);
    if Assigned(tmpToken) and (tmpToken is TAttachableToken) then
      TAttachableToken(tmpToken).Detach;
  end;
  FGlyph.Free;
  FAttached.Free;
  inherited;
end;

procedure TToken.DrawForMaster(toCanvas: TBGRABitmap);
begin
  //
end;

procedure TToken.DrawForPlayer(toCanvas: TBGRABitmap);
begin
  //
end;

procedure TToken.SaveToIni(SaveFile: TIniFile; idx: Integer);
begin
  saveFile.WriteString(SAVESECTIONTOKENS, 'Path' + IntToStr(idx), Path);

  saveFile.WriteString(SAVESECTIONTOKENS, 'Name' + IntToStr(idx), Name);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'XPos' + IntToStr(idx), XEndPos);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'YPos' + IntToStr(idx), YEndPos);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'Width' + IntToStr(idx), Width);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'Height' + IntToStr(idx), Height);
  saveFile.WriteFloat(SAVESECTIONTOKENS, 'Angle' + IntToStr(idx), Angle);
  saveFile.WriteBool(SAVESECTIONTOKENS, 'Visible' + IntToStr(idx), Visible);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'XSlots' + IntToStr(idx), GridSlotsX);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'YSlots' + IntToStr(idx), GridSlotsY);
end;

procedure TToken.SnapToGrid(GridData: TGridData);//GridSizeX, GridSizeY, XOffset, YOffset: Single; GridType: TGridType);
var
  count: Integer;
  tmpGridSize, tmpOffset: Single;
begin
  if GridData.GridType = gtRect then
  begin
    XPos := Round(Round((FXTargetPos - GridData.GridOffsetX - GridData.GridSizeX / 2) / GridData.GridSizeX) * GridData.GridSizeX + GridData.GridOffsetX + (FGridSlotsX * GridData.GridSizeX / 2));
    YPos := Round(Round((FYTargetPos - GridData.GridOffsetY - GridData.GridSizeY / 2) / GridData.GridSizeY) * GridData.GridSizeY + GridData.GridOffsetY +(FGridSlotsY * GridData.GridSizeY / 2));
  end
  else if GridData.GridType = gtHexH then
  begin
    tmpGridSize := GridData.GridSizeY * 3 / 4;
    Count := Round((FYTargetPos - GridData.GridOffsetY - Height / 2) / tmpGridSize);
    YPos := Round(Count * tmpGridSize + GridData.GridOffsetY +(FGridSlotsY * GridData.GridSizeY / 2));
    tmpOffset := GridData.GridOffsetX;
    if Odd(Count) then
      tmpOffset := tmpOffset + GridData.GridSizeX / 2;
    XPos := Round(Round((FXTargetPos - tmpOffset - Width / 2) / GridData.GridSizeX) * GridData.GridSizeX + tmpOffset + (FGridSlotsX * GridData.GridSizeX / 2));
  end
  else if GridData.GridType = gtHexV then
  begin
    tmpGridSize := GridData.GridSizeX * 3 / 4;
    Count := Round((FXTargetPos - GridData.GridOffsetX - Width / 2) / tmpGridSize);
    XPos := Round(Count * tmpGridSize + GridData.GridOffsetX + (FGridSlotsX * GridData.GridSizeX / 2));
    tmpOffset := GridData.GridOffsetY;
    if Odd(Count) then
      tmpOffset := tmpOffset + GridData.GridSizeY / 2;
    YPos := Round(Round((FYTargetPos - tmpOffset - Height / 2) / GridData.GridSizeY) * GridData.GridSizeY + tmpOffset +(FGridSlotsY * GridData.GridSizeY / 2));
  end
  else if GridData.GridType = gtIsometric then
  begin
    tmpGridSize := GridData.GridSizeY / 2;
    Count := Round((FYTargetPos - GridData.GridOffsetY {- Height / 2}) / tmpGridSize) - 1;
    YPos := Round(Count * tmpGridSize + GridData.GridOffsetY +(FGridSlotsY * GridData.GridSizeY / 2));
    tmpOffset := GridData.GridOffsetX;
    if Odd(Count) then
      tmpOffset := tmpOffset + GridData.GridSizeX / 2;
    XPos := Round(Round((FXTargetPos - tmpOffset - Width / 2) / GridData.GridSizeX) * GridData.GridSizeX + tmpOffset + (FGridSlotsX * GridData.GridSizeX / 2));
  end;
  if FVisible then
    StartAnimation;
end;

function TToken.GetCellsAtPosition(PosX, PosY: Integer; GridData: TGridData): TRect;
var
  count: Integer;
  tmpGridSize, tmpOffset: Single;
begin
  Result := Rect(-1, -1, -1, -1);
  if GridData.GridType = gtRect then
  begin
    Result.Left := Round((PosX - GridData.GridOffsetX - GridData.GridSizeX / 2) / GridData.GridSizeX);
    Result.Top := Round((PosY - GridData.GridOffsetY - GridData.GridSizeX / 2) / GridData.GridSizeY);
    Result.Width := GridSlotsX;
    Result.Height := GridSlotsY;
  end
  else if GridData.GridType = gtHexH then
  begin
    tmpGridSize := GridData.GridSizeY * 3 / 4;
    Result.Top := Round((PosY - GridData.GridOffsetY - Height / 2) / tmpGridSize);
    tmpOffset := GridData.GridOffsetX;
    if Odd(Result.Top) then
      tmpOffset := tmpOffset + GridData.GridSizeX / 2;
    Result.Left := Round((PosX - tmpOffset - FWidth / 2) / GridData.GridSizeX);
    Result.Width := GridSlotsX;
    Result.Height := GridSlotsY;
    {tmpGridSize := GridData.GridSizeY * 3 / 4;
    Count := Round((FYTargetPos - GridData.GridOffsetY - Height / 2) / tmpGridSize);
    YPos := Round(Count * tmpGridSize + GridData.GridOffsetY +(FGridSlotsY * GridData.GridSizeY / 2));
    tmpOffset := GridData.GridOffsetX;
    if Odd(Count) then
      tmpOffset := tmpOffset + GridData.GridSizeX / 2;
    XPos := Round(Round((FXTargetPos - tmpOffset - Width / 2) / GridData.GridSizeX) * GridData.GridSizeX + tmpOffset + (FGridSlotsX * GridData.GridSizeX / 2));}
  end
  else if GridData.GridType = gtHexV then
  begin
    tmpGridSize := GridData.GridSizeX * 3 / 4;
    Result.Left := Round((PosX - GridData.GridOffsetX - Width / 2) / tmpGridSize);
    tmpOffset := GridData.GridOffsetY;
    if Odd(Result.Left) then
      tmpOffset := tmpOffset + GridData.GridSizeY / 2;
    Result.Top := Round((PosY - tmpOffset - Height / 2) / GridData.GridSizeY);
    Result.Width := GridSlotsX;
    Result.Height := GridSlotsY;
    {tmpGridSize := GridData.GridSizeX * 3 / 4;
    Count := Round((FXTargetPos - GridData.GridOffsetX - Width / 2) / tmpGridSize);
    XPos := Round(Count * tmpGridSize + GridData.GridOffsetX + (FGridSlotsX * GridData.GridSizeX / 2));
    tmpOffset := GridData.GridOffsetY;
    if Odd(Count) then
      tmpOffset := tmpOffset + GridData.GridSizeY / 2;
    YPos := Round(Round((FYTargetPos - tmpOffset - Height / 2) / GridData.GridSizeY) * GridData.GridSizeY + tmpOffset +(FGridSlotsY * GridData.GridSizeY / 2));}
  end
  else if GridData.GridType = gtIsometric then
  begin
    tmpGridSize := GridData.GridSizeY / 2;
    Result.Top := Round((PosY - GridData.GridOffsetY {- Height / 2}) / tmpGridSize) - 1;
    tmpOffset := GridData.GridOffsetX;
    if Odd(Result.Top) then
      tmpOffset := tmpOffset + GridData.GridSizeX / 2;
    Result.Left := Round((PosX - tmpOffset - Width / 2) / GridData.GridSizeX);
    Result.Width := GridSlotsX;
    Result.Height := GridSlotsY;
    {tmpGridSize := GridData.GridSizeY / 2;
    Count := Round((FYTargetPos - GridData.GridOffsetY - Height / 2) / tmpGridSize);
    YPos := Round(Count * tmpGridSize + GridData.GridOffsetY +(FGridSlotsY * GridData.GridSizeY / 2));
    tmpOffset := GridData.GridOffsetX;
    if Odd(Count) then
      tmpOffset := tmpOffset + GridData.GridSizeX / 2;
    XPos := Round(Round((FXTargetPos - tmpOffset - Width / 2) / GridData.GridSizeX) * GridData.GridSizeX + tmpOffset + (FGridSlotsX * GridData.GridSizeX / 2));}
  end;
end;

procedure TToken.SetXPos(val: Integer);
begin
  if FVisible then
  begin
    FXStartPos := FXPos;
    FXTargetPos := val;
    if FIsMoving then
      FCurAnimationStep := 0;
  end
  else
  begin
    FXPos := val;
    FXStartPos := val;
    FXTargetPos := val;
  end;
  UpdateAttached;
end;

procedure TToken.SetYPos(val: integer);
begin
  if FVisible then
  begin
    FYStartPos := FYPos;
    FYTargetPos := val;
    if FIsMoving then
      FCurAnimationStep := 0;
  end
  else
  begin
    FYPos := val;
    FYStartPos := val;
    FYTargetPos := val;
  end;
  UpdateAttached;
end;

procedure TToken.SetWidth(Val: Integer);
begin
  FWidth := Val;
end;

procedure TToken.SetHeight(Val: Integer);
begin
  FHeight := Val;
end;

function TToken.GetXPos: Integer;
begin
  if FIsMoving then
    Result := FXPos
  else
    Result := FXStartPos;
end;

function TToken.GetYPos: Integer;
begin
if FIsMoving then
  Result := FYPos
else
  Result := FYStartPos;
end;
         
function TToken.GetXEndPos: Integer;
begin
  Result := FXTargetPos;
end;

function TToken.GetYEndPos: Integer;
begin
  Result := FYTargetPos;
end;

function TToken.GetAngle: Double;
begin
  Result := FAngle;
end;

function TToken.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TToken.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TToken.SetAngle(val: Double);
begin
  FAngle := val;
  UpdateAttached;
  {while FAngle < -PI do
    FAngle := FAngle + PI;
  while FAngle > PI do
    FAngle := FAngle - PI;}
end;

procedure TToken.UpdateAttached;
var i: Integer;
begin
  for i := 0 to AttachedCount - 1 do
    if GetAttached(i) is TAttachableToken then
      GetAttached(i).RedrawGlyph;
end;

procedure TToken.Attach(token: TToken);
begin
  FAttached.Add(token);
  token.RedrawGlyph;
end;

function TToken.GetAttached(idx: Integer): TToken;
begin
  Result := nil;
  if (idx >= 0) and (idx < FAttached.Count) then
    Result := TToken(FAttached[idx]);
end;

function TToken.AttachedCount: Integer;
begin
  Result := FAttached.Count;
end;

procedure TToken.RemoveAttached(token: TToken);
begin
  if FAttached.IndexOf(token) >= 0 then
    FAttached.Remove(token);
end;

procedure TToken.StartAnimation;
begin
  FIsMoving := True;
  FCurAnimationStep := 0;
end;

procedure TToken.StopAnimation;
begin
  FIsMoving := False;
  FXStartPos := FXPos;
  FYStartPos := FYPos;
end;

procedure TToken.DoAnimationStep(var NeedsUpdate: Boolean);
begin
  if not FIsMoving then
  begin
    NeedsUpdate := False;
    Exit;
  end;
  if FCurAnimationStep = MAXTOKENANIMSTEPS then
  begin
    FXPos := FXTargetPos;
    FYPos := FYTargetPos;
    FXStartPos := FXTargetPos;
    FYStartPos := FYTargetPos;
    FIsMoving := False;
  end
  else
  begin
    Inc(FCurAnimationStep);
    FXPos := Round(Ease(FCurAnimationStep, FXStartPos, FXTargetPos - FXStartPos, MAXTOKENANIMSTEPS, etOutQuad));
    FYPos := Round(Ease(FCurAnimationStep, FYStartPos, FYTargetPos - FYStartPos, MAXTOKENANIMSTEPS, etOutQuad));
  end;
  NeedsUpdate := True;
end;

function TToken.GetBoundingRect: TRect;
var
  aSin, aCos: Double;
  MaxX, MaxY, MinX, MinY: Double;
  TransX, TransY: Double;
  pX, pY: array[0..3] of Double;
  i: Integer;
begin
  SinCos(FAngle, aSin, aCos);
  MaxX := -MAXDOUBLE;
  MaxY := -MAXDOUBLE;
  MinX := MAXDOUBLE;
  MinY := MAXDOUBLE;

  pX[0] := FWidth / 2;
  pY[0] := -FHeight / 2;
  px[1] := FWidth / 2;  
  pY[1] := FHeight / 2;
  px[2] := -FWidth / 2;
  pY[2] := FHeight / 2;
  px[3] := -FWidth / 2;
  pY[3] := -FHeight / 2;

  for i := 0 to 3 do
  begin 
    TransX := pX[i] * aCos + pY[i] * aSin;
    TransY := -pX[i] * aSin + pY[i] * aCos;
                                           
    MinX := Min(MinX, TransX);
    MaxX := Max(MaxX, TransX);   
    MinY := Min(MinY, TransY);
    MaxY := Max(MaxY, TransY);
  end;

  Result := Rect(Floor(MinX), Floor(MinY), Ceil(MaxX), Ceil(MaxY));
end;

function TToken.GetPlayerGlyph: TBGRABitmap;
begin
  Result := FGlyph;
end;

{ TAttachableToken }

constructor TAttachableToken.Create;
begin
  FAttachedTo := nil;
end;

destructor TAttachableToken.Destroy;
begin
  if IsAttached then
    Detach;
  inherited;
end;

procedure TAttachableToken.SaveToIni(SaveFile: TIniFile; idx: Integer);
begin
  inherited SaveToIni(SaveFile, idx);
  if IsAttached then
    SaveFile.WriteString(SAVESECTIONTOKENS, 'Attached' + IntToStr(idx), '::Next');
end;

procedure TAttachableToken.AttachTo(token: TToken);
begin
  FAttachedTo := token;
  FAttachedTo.Attach(self);
end;

procedure TAttachableToken.Detach;
begin
  XPos := FAttachedTo.XPos;
  YPos := FAttachedTo.YPos;
  FXTargetPos := FAttachedTo.XEndPos;
  FYTargetPos := FAttachedTo.YEndPos;
  Angle := FAttachedTo.Angle;
  FAttachedTo.RemoveAttached(self);
  FAttachedTo := nil;
end;

function TAttachableToken.IsAttached: Boolean;
begin
  Result := Assigned(FAttachedTo);
end;

function TAttachableToken.GetXPos: Integer;
begin
  Result := inherited;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.XPos;
end;

function TAttachableToken.GetYPos: Integer;
begin
  Result := inherited;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.YPos;
end;

function TAttachableToken.GetXEndPos: Integer;
begin
  Result := FXTargetPos;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.XEndPos;
end;

function TAttachableToken.GetYEndPos: Integer;
begin
  Result := FYTargetPos;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.YEndPos;
end;

function TAttachableToken.GetAngle: Double;
begin
  Result := FAngle;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.Angle;
end;

{ TTokenFactory }

class function TTokenFactory.CreateTokenFromIni(SaveFile: TIniFile; idx: Integer): TToken;
var
  path: string;
begin
  Result := nil;
  path := SaveFile.ReadString(SAVESECTIONTOKENS, 'Path' + IntToStr(idx), '-');
  if SameText(path, '::Range') then // Range indicator
  begin
    Result := TRangeIndicator.Create(saveFile.ReadInteger(SAVESECTIONTOKENS, 'XPos' + IntToStr(idx), 0),
                                       saveFile.ReadInteger(SAVESECTIONTOKENS, 'YPos' + IntToStr(idx), 0),
                                       saveFile.ReadInteger(SAVESECTIONTOKENS, 'Width' + IntToStr(idx), 100),
                                       saveFile.ReadInteger(SAVESECTIONTOKENS, 'Height' + IntToStr(idx), 100));
    TRangeIndicator(Result).Alpha := saveFile.ReadInteger(SAVESECTIONTOKENS, 'Alpha' + IntToStr(idx), 32);
    TRangeIndicator(Result).Color := saveFile.ReadInteger(SAVESECTIONTOKENS, 'Color' + IntToStr(idx), clRed);
    TRangeIndicator(Result).SectorAngle := saveFile.ReadFloat(SAVESECTIONTOKENS, 'Sector' + IntToStr(idx), 90);
  end
  else if SameText(path, '::Text') then // Text token
  begin
    Result := TTextToken.Create(saveFile.ReadInteger(SAVESECTIONTOKENS, 'XPos' + IntToStr(idx), 0),
                                saveFile.ReadInteger(SAVESECTIONTOKENS, 'YPos' + IntToStr(idx), 0),
                                saveFile.ReadInteger(SAVESECTIONTOKENS, 'Width' + IntToStr(idx), 100),
                                saveFile.ReadInteger(SAVESECTIONTOKENS, 'Height' + IntToStr(idx), 100),
                                saveFile.ReadString(SAVESECTIONTOKENS, 'Text' + IntToStr(idx), ''));
  end
  else if SameText(path, '::light') then // Light token
  begin
    Result := TLightToken.Create(saveFile.ReadInteger(SAVESECTIONTOKENS, 'XPos' + IntToStr(idx), 0),
                                 saveFile.ReadInteger(SAVESECTIONTOKENS, 'YPos' + IntToStr(idx), 0),
                                 saveFile.ReadInteger(SAVESECTIONTOKENS, 'Width' + IntToStr(idx), 100));
   TLightToken(Result).Color := saveFile.ReadInteger(SAVESECTIONTOKENS, 'Color' + IntToStr(idx), BGRA(255, 245, 238));
   TLightToken(Result).MaxStrength := saveFile.ReadFloat(SAVESECTIONTOKENS, 'MaxStrength' + IntToStr(idx), 0.5);
   TLightToken(Result).WallManager := WallManager;
   Result.RedrawGlyph;
  end
  else if FileExists(path) then // Character token
  begin
    Result := TCharacterToken.Create(path,
                                     saveFile.ReadInteger(SAVESECTIONTOKENS, 'XPos' + IntToStr(idx), 0),
                                     saveFile.ReadInteger(SAVESECTIONTOKENS, 'YPos' + IntToStr(idx), 0),
                                     saveFile.ReadInteger(SAVESECTIONTOKENS, 'Width' + IntToStr(idx), 100),
                                     saveFile.ReadInteger(SAVESECTIONTOKENS, 'Height' + IntToStr(idx), 100));
    TCharacterToken(Result).Number:= saveFile.ReadInteger(SAVESECTIONTOKENS, 'No' + IntToStr(idx), 0); 
    TCharacterToken(Result).OverlayIdx := saveFile.ReadInteger(SAVESECTIONTOKENS, 'Overlay' + IntToStr(idx), -1);
    TCharacterToken(Result).ShowArrow := ShowDirectionArrow;
    TCharacterToken(Result).ShowLoS := saveFile.ReadBool(SAVESECTIONTOKENS, 'LoS' + IntToStr(idx), False);
  end;

  if Assigned(Result) then
  begin
    Result.Angle := saveFile.ReadFloat(SAVESECTIONTOKENS, 'Angle' + IntToStr(idx), 0);
    Result.Visible := saveFile.ReadBool(SAVESECTIONTOKENS, 'Visible' + IntToStr(idx), TokensStartInvisible);
    Result.GridSlotsX := saveFile.ReadInteger(SAVESECTIONTOKENS, 'XSlots' + IntToStr(idx), 1);
    Result.GridSlotsY := saveFile.ReadInteger(SAVESECTIONTOKENS, 'YSlots' + IntToStr(idx), 1);
    Result.Name := saveFile.ReadString(SAVESECTIONTOKENS, 'Name' + IntToStr(idx), '');
  end;
end;

class function TTokenFactory.CreateTokenFromNode(data: TTokenNodeData; X, Y: Integer): TToken;
begin
  case data.TokenType of
    ttCharacter:
    begin
      Result := TCharacterToken.Create(data.FullPath, X, Y, data.DefaultWidth, data.DefaultHeight);
      Result.GridSlotsX := Data.DefaultGridSlotsX;
      Result.GridSlotsY := Data.DefaultGridSlotsY;
      Result.Angle := Data.DefaultAngle;
      Result.Name := Data.Name;
      Result.BaseInitiative := Data.BaseInitiative;
      Result.Visible := TokensStartInvisible;
      TCharacterToken(Result).ShowArrow := ShowDirectionArrow;
      Result.RedrawGlyph;
      if TokensSnapToGrid then
        Result.SnapToGrid(GridData);
    end;
    ttRange:
    begin
      Result := TRangeIndicator.Create(X, Y, 500, 500);
      TRangeIndicator(Result).SectorAngle := 90;
      TRangeIndicator(Result).Alpha := 32;
      TRangeIndicator(Result).Color := clRed;
      Result.GridSlotsX := 1;
      Result.GridSlotsY := 1;
      Result.Angle := 0;
      Result.Visible := TokensStartInvisible;
    end;
    ttText:
    begin
      Result := TTextToken.Create(X, Y, 200, 100, '');
      Result.GridSlotsX := 1;
      Result.GridSlotsY := 1;
      Result.Angle := 0;
      Result.Visible := TokensStartInvisible;
    end;
    ttLight:
    begin
      Result := TLightToken.Create(X, Y, 200);
      Result.Visible := TokensStartInvisible;
      TLightToken(Result).WallManager := WallManager;
      Result.RedrawGlyph;
      // Other values should be set to defaults already
    end;
  end;
end;

{ TCharacterToken }

constructor TCharacterToken.Create(pPath: string; X, Y, pWidth, pHeight: Integer);
begin
  inherited Create(pPath, X, Y, pWidth, pHeight);
  FOverlayIdx := -1;
  FNumber := 0;
  FShowLoS := False;
end;

procedure TCharacterToken.SaveToIni(SaveFile: TIniFile; idx: Integer);
begin
  inherited SaveToIni(SaveFile, idx);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'No' + IntToStr(idx), Number);
  saveFile.WriteInteger(SAVESECTIONTOKENS, 'Overlay' + IntToStr(idx), OverlayIdx);
  saveFile.WriteBool(SAVESECTIONTOKENS, 'LoS' + IntToStr(idx), FShowLoS);
end;

procedure TCharacterToken.SetOverlayIdx(val: Integer);
begin
  FOverlayIdx := val;
  RedrawGlyph;
end;

procedure TCharacterToken.SetNumber(val: Integer);
begin
  FNumber := val;
  RedrawGlyph;
end;

procedure TCharacterToken.SetAngle(val: Double);
begin
  inherited SetAngle(val);
  RedrawGlyph;
end;

procedure TCharacterToken.RedrawGlyph;
var
  TextRenderer: TBGRATextEffectFontRenderer;
  NumSize: TSize;
  ArrowLen, ArrowWid: Double; 
  ArrowPnt: TPointF;
  ArrowPntsTrans: array[0..3] of TPointF;
  j: Integer;
begin
  FGlyph.Free;
  FGlyph := TBGRABitmap.Create(Path);
  if FNumber > 0 then
  begin
    TextRenderer := TBGRATextEffectFontRenderer.Create;
    TextRenderer.OutlineVisible := True;
    TextRenderer.OutlineColor := clBlack;
    NumSize := FGlyph.TextSize(IntToStr(FNumber));
    FGlyph.FontStyle := [fsBold];
    FGlyph.FontRenderer := TextRenderer;
    // Should the text size change with the zoom factor?
    FGlyph.TextOut((FGlyph.Width - NumSize.Width) div 2, (FGlyph.Height - NumSize.Height) div 2, IntToStr(FNumber), clWhite, taLeftJustify);
  end;
  if ShowArrow then
  begin
    ArrowLen := Min(Width, Height) * 0.4;// * FDisplayScale * FZoomFactor;
    ArrowWid := ArrowLen / 4;
    for j := 0 to 3 do
    begin
      ArrowPnt.x := ARROW[j].x * ArrowWid;
      ArrowPnt.y := ARROW[j].y * ArrowLen;
      ArrowPntsTrans[j].x := FGlyph.Width div 2  + ArrowPnt.x * Cos(-Angle) - ArrowPnt.y * Sin(-Angle);
      ArrowPntsTrans[j].y := FGlyph.Height div 2 + ArrowPnt.x * Sin(-Angle) + ArrowPnt.y * Cos(Angle);
    end;
    FGlyph.FillPoly(ArrowPntsTrans, clWhite);
    FGlyph.DrawPolygonAntialias(ArrowPntsTrans, clBlack, 2);
  end;
end;

{ TRangeIndicator }

constructor TRangeIndicator.Create(X, Y, pWidth, pHeight: Integer);
begin
  inherited Create;
  FXPos := X;
  FYPos := Y;
  FXTargetPos := X;
  FYTargetPos := Y;
  FXStartPos := X;
  FYStartPos := Y;
  FWidth := pWidth;
  FHeight := pHeight;
  FVisible := True;
  FIsMoving := False;
  FGridSlotsX := 1;
  FGridSlotsY := 1;
  FCurAnimationStep := 0;
  FPath := '';
  FColor := clRed;
  FAlpha := 128;
  FSectorAngle := 90;
  //FGlyph := TBGRABitmap.Create(FWidth, FHeight);
  RedrawGlyph;
  FAttached := TList.Create;
end;

destructor TRangeIndicator.Destroy;
begin
  inherited;
end;

procedure TRangeIndicator.SaveToIni(SaveFile: TIniFile; idx: Integer);
begin
  inherited SaveToIni(SaveFile, idx);
  SaveFile.WriteString(SAVESECTIONTOKENS, 'Path' + IntToStr(idx), '::Range');
  SaveFile.WriteInteger(SAVESECTIONTOKENS, 'Alpha' + IntToStr(idx), Alpha);
  SaveFile.WriteInteger(SAVESECTIONTOKENS, 'Color' + IntToStr(idx), Color);
  SaveFile.WriteFloat(SAVESECTIONTOKENS, 'Sector' + IntToStr(idx), SectorAngle);
end;

procedure TRangeIndicator.SetColor(val: TColor);
begin
  FColor := Val;
  RedrawGlyph;
end;

procedure TRangeIndicator.SetAlpha(val: Byte);
begin
  FAlpha := val;
  RedrawGlyph;
end;

procedure TRangeIndicator.SetSectorAngle(Val: Double);
begin
  FSectorAngle := Val;
  RedrawGlyph;
end;

procedure TRangeIndicator.SetWidth(Val: Integer);
begin
  FWidth := Val;
  RedrawGlyph;
end;

procedure TRangeIndicator.SetHeight(Val: Integer);
begin
  FHeight := Val;
  RedrawGlyph;
end;

procedure TRangeIndicator.SetAngle(val: Double);
begin
  FAngle := val;
  RedrawGlyph;
end;

procedure TRangeIndicator.RedrawGlyph;
var
  i: Integer;
  CurAngle: Double;
  aSin, aCos: Double;
  pnts: array of TPointF;
begin
  FGlyph.Free;
  FGlyph := TBGRABitmap.Create(FWidth, FHeight);
  SetLength(pnts, Ceil(FSectorAngle) + 2);
  pnts[0] := PointF(FWidth / 2, FHeight / 2);
  SinCos(Angle, aSin, aCos);
  for i := 0 to Length(pnts) - 2 do
  begin
    CurAngle := DegToRad(-FSectorAngle / 2 + i * FSectorAngle / Ceil(FSectorAngle)) - Angle;
    pnts[i + 1] := PointF(FWidth * 0.5 * (1+ Sin(CurAngle)),
                          FHeight * 0.5 * (1 - Cos(CurAngle)));
  end;
  FGlyph.EraseRect(0, 0, FWidth, FHeight, 255);

  FGlyph.DrawPolygonAntialias(pnts, ColorToBGRA(FColor, FAlpha), 1, ColorToBGRA(FColor, FAlpha));
end;

{ TTextToken }

constructor TTextToken.Create(X, Y, pWidth, pHeight: Integer; text: string);
begin
  FXPos := X;
  FYPos := Y;
  FXTargetPos := X;
  FYTargetPos := Y;
  FXStartPos := X;
  FYStartPos := Y;
  FWidth := pWidth;
  FHeight := pHeight;
  FVisible := True;
  FIsMoving := False;
  FGridSlotsX := 1;
  FGridSlotsY := 1;
  FCurAnimationStep := 0;
  FPath := '';
  FText := text;
  RedrawGlyph;
  FAttached := TList.Create;
end;

destructor TTextToken.Destroy;
begin
  inherited;
end;

procedure TTextToken.SaveToIni(SaveFile: TIniFile; idx: Integer);
begin
  inherited SaveToIni(SaveFile, idx);
  SaveFile.WriteString(SAVESECTIONTOKENS, 'Path' + IntToStr(idx), '::Text');
  SaveFile.WriteString(SAVESECTIONTOKENS, 'Text' + IntToStr(idx), Text);
end;

procedure TTextToken.SetWidth(val: Integer);
begin
  FWidth := Val;
  RedrawGlyph;
end;

procedure TTextToken.SetHeight(Val: Integer);
begin
  FHeight := Val;
  RedrawGlyph;
end;

procedure TTextToken.SetText(val: string);
begin
  FText := Val;
  RedrawGlyph;
end;

procedure TTextToken.RedrawGlyph; 
var
  CurWidth, CurHeight: Double;
  AddedH, AddedV, i: Integer;
  svgpath: string;
const
  TOPLEFT = 'm %d %d c -4 0 -5 0 -5 -4 c 1 -2 0 -3.3333 0 -5 c 0 -2 1 -4 0 -6 c 1 -2 0 -4 0 -6 c 0 -5 0 -3 4 -4 c 2 0 4 2 6 1 c 1 -1 4 0 6 -1 c 11 1 8 0 9 5 ';

  INSERTV1T ='c 0 -8 2 -3 3 -5 c 2 1 4 0 5 0 c 3 1 0 1 5 0 c 2 1 7 0 7 0 c 6 0 4 4 5 5 ';

  INSERTV2T = 'c 1 1 -2 -5 5 -5 c 2 1 2.6667 0 4 0 c 1.3333 0 -1 1 4 0 c 1 1 5 0 7 1 c 2 1 5 -3 5 4 ';

  TOPRIGHT = 'c 1 -7 3.3333 -3.3333 5 -5 c 4 0 -1 2 7 1 c 2 0 2 -1 4 -1 c 1 0 2 1 4 0 c 3 0 4 1 5 5 c 0 3 -1 3 -1 5 c 2 2 -1 4 0 6 c 1 1 1 3 1 4 c -1 2 2 5 -5 5 ';

  INSERTH1R = 'c 8 0 2.6667 3.3333 4 5 c 1 1 0 3 1 5 c 0 1.3333 -1 2 0 4 c -1 1 0 2 -1 3 c 1 2 0 1 1 3 c -1 4 2 5 -5 5 ';

  INSERTH2R = 'c 7 0 3.3333 3.3333 5 5 c 0 4 -1 3 -1 6 c 1 2 1 4 1 6 c -1 1 0 2.6667 0 4 c 0 2 1 4 -5 4 ';

  BOTTOMRIGHT = 'c 7 0 4 3 5 5 c -0.6667 1.3333 1 4 -2 4 c 2 0 2 3 2 5 c -1 1 0 1 0 6 c 0 8 -2 3 -5 5 c -2 -1 -6 0 -6 -1 c -3 1 -6 1 -6 -1 c 0 3 -2 1.3333 -3 2 c -3 0 -5 0 -5 -5 ';

  INSERTV2B = 'c 0 4 -2 7 -5 3 c -1 -1 -1 2 -6 2 c -2 0 -4 -3 -4 -1 c 0 1 -3.3333 0.6667 -5 1 c -2 0 -4 0 -5 -5 ';

  INSERTV1B = 'c -2 -1 1 7 -5 4 c -1 -1 -2 1 -5 1 c -4 -2 -4 0 -10 0 c -2 -1 -5 2 -5 -5 ';

  BOTTOMLEFT = 'c -1 5 -2 5 -5 5 c 0 -1 -3 0 -4 -2 c -1 -1 0 4 -3 1 c -1 -1 -1 1 -4 1 c -1.3333 -0.3333 -4 -2 -4 -1 c -2 2.5 -7 -1 -4 -3 c 1 -1 -1 -1 -1 -5 c 0 -4 3 -3 2 -4 c -3 0 -0.6667 -2.6667 -1 -4 c 0 -1 -1 0 -1 -3 c 0 -8 1 -3 5 -5 ';

  INSERTH2L = 'c -5 0 -4 0 -5 -5 c 2 -2 -1 -4 1 -4 c -2 0 0 -3 -1 -5 c 0 -3 2 0 0 -6 c 0 -6 1 -4 5 -5 ';

  INSERTH1L = 'c -7 0 -4 -3 -4 -5 c -2 -3 0.6667 -1.3333 1 -2 c -1 -1 -3 0 -1 -4 c 1 -1 -1 -1 -1 -4 c 1 -2 0 -2 0 -5 c 0 -5 1 -4 5 -5 ';

  SCALE = 1; // Leave this at 1 for now, otherwise the smallest version of the frame
             // would not fit the 100*200px default text token
             // Maybe scale this with the token size?
begin
  FGlyph.Free;
  FGlyph := TBGRABitmap.Create(FWidth, FHeight);

  CurWidth := 54;  // the segments are all 25*25, but we leave a thin margin
  CurHeight := 54;
  AddedH := 0;
  AddedV := 0;
  while CurWidth + 25 < FWidth / SCALE do
  begin
    CurWidth := CurWidth + 25;
    Inc(AddedV);
  end;
  while CurHeight + 25 < FHeight / SCALE do
  begin
    CurHeight := CurHeight + 25;
    Inc(AddedH);
  end;

  // Assemble the path for our frame
  svgpath := Format(TOPLEFT, [Round(7 + (FWidth - CurWidth * SCALE) / 2 / SCALE), Round(27 + (FHeight - CurHeight * SCALE) / 2 / SCALE)]);

  i := 0;
  while i < AddedV do
  begin
    if Odd(i) then
      svgpath := svgpath + INSERTV2T
    else
      svgpath := svgpath + INSERTV1T;
    Inc(i);
  end;
  svgpath := svgpath + TOPRIGHT;
  i := AddedH;
  while i > 0 do
  begin
    if Odd(i) then
      svgpath := svgpath + INSERTH1R
    else
      svgpath := svgpath + INSERTH2R;
    Dec(i);
  end;
  svgpath := svgpath + BOTTOMRIGHT;
  i := AddedV;
  while i > 0 do
  begin
    if Odd(i) then
      svgpath := svgpath + INSERTV1B
    else
      svgpath := svgpath + INSERTV2B;
    Dec(i);
  end;

  svgpath := svgpath + BOTTOMLEFT;
  i := 0;
  while i < AddedH do
  begin
    if Odd(i) then
      svgpath := svgpath + INSERTH2L
    else
      svgpath := svgpath + INSERTH1L;
    Inc(i);
  end;

  // Change this to the parchment texture?
  FGlyph.Canvas2D.fillStyle('FFDEAD');
  FGlyph.Canvas2D.strokeStyle('DFBE8D');
  FGlyph.Canvas2D.beginPath();
  FGlyph.Canvas2D.lineWidth := 2;

  FGlyph.Canvas2D.scale(SCALE);
  FGlyph.Canvas2D.path(svgpath);
  FGlyph.Canvas2D.closePath();
  FGlyph.Canvas2D.stroke();
  FGlyph.Canvas2D.fill();

  FGlyph.TextRect(Rect(3, 3, FWidth - 3, FHeight - 3), FText, taCenter, tlCenter, clBlack);
end;

{ TLightToken }

constructor TLightToken.Create(X, Y, pRange: Integer);
begin
  inherited Create;
  FXPos := X;
  FYPos := Y;
  FXTargetPos := X;
  FYTargetPos := Y;
  FXStartPos := X;
  FYStartPos := Y;
  FWidth := pRange * 2;
  FHeight := pRange * 2;
  FVisible := True;
  FIsMoving := False;
  FGridSlotsX := 1;
  FGridSlotsY := 1;
  FCurAnimationStep := 0;
  FPath := '';
  FColor := BGRA(255, 245, 238);
  FMaxStrength := 0.5;
  FRange:= pRange;
  RedrawLightMap;
  RedrawPlayerGlyph;
  RedrawGlyph;
  FAttached := TList.Create;
  FAnimationType := latNone;
  FAnimationStep := 0;
end;

destructor TLightToken.Destroy;
begin
  FLightPic.Free;
  FPlayerGlyph.Free;
  inherited;
end;

procedure TLightToken.SaveToIni(SaveFile: TIniFile; idx: Integer);
begin
  inherited SaveToIni(SaveFile, idx);
  SaveFile.WriteString(SAVESECTIONTOKENS, 'Path' + IntToStr(idx), '::light');
  SaveFile.WriteInteger(SAVESECTIONTOKENS, 'Color' + IntToStr(idx), FColor);
  SaveFile.WriteFloat(SAVESECTIONTOKENS, 'MaxStrength' + IntToStr(idx), FMaxStrength);
end;

procedure TLightToken.DoAnimationStep(var NeedsUpdate: Boolean);
var
  LastAniStep: Integer;
begin
  inherited;
  LastAniStep := FAnimationStep;
  FAnimationStep := (FAnimationStep + 1) mod 360;

  RedrawPlayerGlyph;
  NeedsUpdate := GetCurrentLightStrength(LastAniStep) <> GetCurrentLightStrength(FAnimationStep);
  NeedsUpdate := NeedsUpdate or IsMoving;
end;

procedure TLightToken.RedrawGlyph;
var
  MaskPoly: ArrayOfTPointF;
  i: Integer;
begin
  FGlyph.Free;
  FGlyph := TBGRABitmap.Create(FRange * 2, FRange * 2);
  FGlyph.FillRect(0, 0, FRange * 2, FRange * 2, clBlack);
  if Assigned(FWallManager) then
  begin
    MaskPoly := FWallManager.GetLoSPolygon(Point(XEndPos, YEndPos), FWallManager.GetMinBoundingBox);
    for i := 0 to Length(MaskPoly) - 1 do
    begin
      MaskPoly[i].x := MaskPoly[i].X - XEndPos + FRange;
      MaskPoly[i].y := MaskPoly[i].Y - YEndPos + FRange;
    end;
    FGlyph.FillPolyAntialias(MaskPoly, BGRA(255, 255, 255));

  end
  else
  begin
    FGlyph.FillRect(0, 0, FRange * 2, FRange * 2, clWhite);
  end;
  FGlyph.BlendImage(0, 0, FLightPic, boMultiply);
  // Mark center and circumference to make the token _slightly_ easier to see
  FGlyph.DrawLineAntialias(FRange - 5, FRange - 5, FRange + 5, FRange + 5, clNavy, 2);
  FGlyph.DrawLineAntialias(FRange + 5, FRange - 5, FRange - 5, FRange + 5, clNavy, 2);
  FGlyph.EllipseAntialias(FRange, FRange, 5, 5, clNavy, 2);
  FGlyph.EllipseAntialias(FRange, FRange, FRange, FRange, clNavy, 2);
end;

procedure TLightToken.RedrawPlayerGlyph;
var
  MaskPoly: ArrayOfTPointF;
  i: Integer;
  CurrentStrength: Byte;
  CurrentPixel: TBGRAPixel;
begin
  CurrentStrength := Floor(FMaxStrength * GetCurrentLightStrength(FAnimationStep) * 255);
  CurrentPixel := BGRA(CurrentStrength, CurrentStrength, CurrentStrength);

  if Assigned(FPlayerGlyph) then
    FreeAndNil(FPlayerGlyph);
  FPlayerGlyph := TBGRABitmap.Create(FRange * 2, FRange * 2, clBlack);
  FPlayerGlyph.FillRect(0, 0, FRange * 2, FRange * 2, clBlack);
  if Assigned(FWallManager) then
  begin
    MaskPoly := FWallManager.GetLoSPolygon(Point(XPos, YPos), FWallManager.GetMinBoundingBox);
    for i := 0 to Length(MaskPoly) - 1 do
    begin
      MaskPoly[i].x := MaskPoly[i].X - XPos + Width div 2;
      MaskPoly[i].y := MaskPoly[i].Y - YPos + Height div 2;
    end;
    FPlayerGlyph.FillPolyAntialias(MaskPoly, CurrentPixel);
  end
  else
  begin
    FPlayerGlyph.FillRect(0, 0, FRange * 2, FRange * 2, CurrentPixel);
  end;
  FPlayerGlyph.BlendImage(0, 0, FLightPic, boMultiply);
end;

procedure TLightToken.RedrawLightMap;
var
  gradient: TBGRAGradientScanner;
begin
  if Assigned(FLightPic) then
    FreeAndNil(FLightPic);
  FLightPic := TBGRABitmap.Create(FRange * 2, FRange * 2, clBlack);
  gradient := TBGRAGradientScanner.Create(MixPixel(clBlack, FColor, FMaxStrength),
                                          clBlack, gtRadial,
                                          TPointF.Create(FRange, FRange),
                                          TPointF.Create(0, FRange));
  FLightPic.FillEllipseAntialias(FRange, FRange, FRange, FRange, gradient);
  gradient.Free;

end;

procedure TLightToken.SetColor(val: TColor);
begin
  FColor := val;
  RedrawLightMap;  
  RedrawGlyph;  
  RedrawPlayerGlyph;
end;

procedure TLightToken.SetRange(val: Integer);
begin
  FRange := val;
  RedrawLightMap;
  RedrawGlyph;
  RedrawPlayerGlyph;
end;

procedure TLightToken.SetMaxStrength(val: Double);
begin
  FMaxStrength := EnsureRange(val, 0, 1);
  RedrawLightMap;
  RedrawGlyph;
  RedrawPlayerGlyph;
end;

procedure TLightToken.SetWidth(Val: Integer);
begin
  FRange := Val div 2;
end;

procedure TLightToken.SetHeight(Val: Integer);
begin
  FRange := Val div 2;
end;

procedure TLightToken.SetXPos(val: Integer);
begin
  inherited;
  RedrawGlyph;
  RedrawPlayerGlyph;
end;

procedure TLightToken.SetYPos(val: integer);
begin
  inherited;
  RedrawGlyph;      
  RedrawPlayerGlyph;
end;

function TLightToken.GetRange: Integer;
begin
  Result := FRange;
end;

function TLightToken.GetCurrentLightStrength(step: Integer): Double;
begin
  case FAnimationType of
    latFlicker:
    begin
      Result := 0.7 + Random * 0.3;
    end;
    latPulse:
    begin
      Result := 0.5 + 0.5 * Sin(DegToRad(2*step));
    end;
    latFlash:
    begin
      Result := Ord(((2 * step) mod 360) >= 180);
    end;
    else
      Result := 1;
  end;
end;

function TLightToken.GetWidth: Integer;
begin
  Result := FRange * 2;
end;

function TLightToken.GetHeight: Integer;
begin
  Result := FRange * 2;
end;

function TLightToken.GetPlayerGlyph: TBGRABitmap;
begin
  Result := FPlayerGlyph;
end;

end.

