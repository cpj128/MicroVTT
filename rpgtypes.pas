{Copyright (c) 2023 Stephan Breer

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
  Classes, Graphics, SysUtils, BGRABitmap;
  
 const
  MAXTOKENANIMSTEPS = 20;

function Map(Val, InValStart, InValEnd, OutValStart, OutValEnd: Double): Double;

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

  TTokenType = (ttDefault, ttRange, ttText);

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
      FOverlayIdx: Integer;
      FVisible: Boolean;
      FGridSlotsX, FGridSlotsY: Integer;
      FNumber: Integer;
      FCurAnimationStep: Integer;
      FIsMoving: Boolean;
      FAttached: TList;
      procedure SetXPos(val: Integer);
      procedure SetYPos(val: integer); 
      procedure SetWidth(Val: Integer); virtual;
      procedure SetHeight(Val: Integer); virtual;
      function GetXPos: Integer; virtual;
      function GetYPos: Integer; virtual;
      function GetXEndPos: Integer; virtual;
      function GetYEndPos: Integer; virtual;
      function GetAngle: Double; virtual;
      procedure SetAngle(val: Double);
    public
      constructor Create(Path: string; X, Y, pWidth, pHeight: Integer);
      destructor Destroy; override;
      procedure SnapToGrid(GridSizeX, GridSizeY, XOffset, YOffset: Single; GridType: TGridType);
      procedure StartAnimation;
      procedure StopAnimation;
      procedure DoAnimationStep;
      function GetBoundingRect: TRect;
      function GetAttached(idx: Integer): TToken;
      function AttachedCount: Integer;
      procedure RemoveAttached(token: TToken);
      procedure UpdateAttached;
      property XPos: Integer read GetXPos write SetXPos;
      property YPos: Integer read GetYPos write SetYPos;
      property XEndPos: Integer read GetXEndPos;
      property YEndPos: Integer read GetYEndPos;
      property Width: Integer read FWidth write SetWidth;
      property Height: Integer read FHeight write SetHeight;
      property Visible: Boolean read FVisible write FVisible;
      property GridSlotsX: Integer read FGridSlotsX write FGridSlotsX;
      property GridSlotsY: Integer read FGridSlotsY write FGridSlotsY;
      property Angle: Double read GetAngle write SetAngle;
      property OverlayIdx: Integer read FOverlayIdx write FOverlayIdx;
      property Number: Integer read FNumber write FNumber;
      property Glyph: TBGRABitmap read FGlyph;
      property IsMoving: Boolean read FIsMoving;
      property Path: string read FPath;
      property Name: string read FName write FName;
      property BaseInitiative: Integer read FBaseInitiative write FBaseInitiative;
  end;

  TRangeIndicator = class(TToken)
  private
    FSectorAngle: Double;
    FColor: TColor;
    FAlpha: Byte;
    FAttachedTo: TToken;
    procedure SetColor(val: TColor);
    procedure SetAlpha(val: Byte);
    procedure SetSectorAngle(Val: Double);
    procedure SetWidth(Val: Integer); override;
    procedure SetHeight(Val: Integer); override;
    function GetXPos: Integer; override;
    function GetYPos: Integer; override;
    function GetXEndPos: Integer; override;
    function GetYEndPos: Integer; override;
    function GetAngle: Double; override;
  public
    constructor Create(X, Y, pWidth, pHeight: Integer);
    destructor Destroy; override;   
    procedure RedrawGlyph;
    procedure AttachTo(token: TToken);
    procedure Detach;
    function IsAttached: Boolean;
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
    procedure RedrawGlyph;
    property Text: string read FText write SetText;
  end;


// Easings
type TEasingType = (etLinear,
                    etOutQuad,
                    etInQuad,
                    etInOutQuad,
                    etInCubic,
                    etOutCubic,
                    etInOutCubic,
                    etInQuart,
                    etOutQuart,
                    etInOutQuart,
                    etInQuint,
                    etOutQuint,
                    etInOutQuint,
                    etInSine,
                    etOutSine,
                    etInOutSine,
                    etInExpo,
                    etOutExpo,
                    etInOutExpo,
                    etInCirc,
                    etOutCirc,
                    etInOutCirc,
                    etInElastic,
                    etOutElastic,
                    etInOutElastic,
                    etInBack,
                    etOutBack,
                    etInOutBack,
                    etInBounce,
                    etOutBounce,
                    etInOutBounce,
                    etSmoothStep,
                    etSmootherStep);

function Ease(Time, StartVal, ChangeAmt, Duration: Double; eType: TEasingType): Double;


implementation

uses
  Math,
  BGRABitmapTypes;

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
  FOverlayIdx := -1;
  FCurAnimationStep := 0;
  FPath := Path;
  FGlyph := TBGRABitmap.Create(Path);
  FAttached := TList.Create;
end;

destructor TToken.Destroy;
begin
  FGlyph.Free;
  FAttached.Free;
  inherited;
end;

procedure TToken.SnapToGrid(GridSizeX, GridSizeY, XOffset, YOffset: Single; GridType: TGridType);
var
  count: Integer;
  tmpGridSize: Single;
begin
  if GridType = gtRect then
  begin
    XPos := Round(Round((FXTargetPos - XOffset - FWidth / 2) / GridSizeX) * GridSizeX + XOffset + (FGridSlotsX * GridSizeX / 2));
    YPos := Round(Round((FYTargetPos - YOffset - FHeight / 2) / GridSizeY) * GridSizeY + YOffset +(FGridSlotsY * GridSizeY / 2));
  end
  else if GridType = gtHexH then
  begin
    tmpGridSize := GridSizeY * 3 / 4;
    Count := Round((FYTargetPos - YOffset - FHeight / 2) / tmpGridSize);
    YPos := Round(Count * tmpGridSize + YOffset +(FGridSlotsY * GridSizeY / 2));
    if Odd(Count) then
      XOffset := XOffset + GridSizeX / 2;
    XPos := Round(Round((FXTargetPos - XOffset - FWidth / 2) / GridSizeX) * GridSizeX + XOffset + (FGridSlotsX * GridSizeX / 2));
  end
  else if GridType = gtHexV then
  begin
    tmpGridSize := GridSizeX * 3 / 4;
    Count := Round((FXTargetPos - XOffset - FWidth / 2) / tmpGridSize);
    XPos := Round(Count * tmpGridSize + XOffset + (FGridSlotsX * GridSizeX / 2));
    if Odd(Count) then
      YOffset := YOffset + GridSizeY / 2;
    YPos := Round(Round((FYTargetPos - YOffset - FHeight / 2) / GridSizeY) * GridSizeY + YOffset +(FGridSlotsY * GridSizeY / 2));
  end
  else if GridType = gtIsometric then
  begin
    tmpGridSize := GridSizeY / 2;
    Count := Round((FYTargetPos - YOffset - FHeight / 2) / tmpGridSize);
    YPos := Round(Count * tmpGridSize + YOffset +(FGridSlotsY * GridSizeY / 2));
    if Odd(Count) then
      XOffset := XOffset + GridSizeX / 2;
    XPos := Round(Round((FXTargetPos - XOffset - FWidth / 2) / GridSizeX) * GridSizeX + XOffset + (FGridSlotsX * GridSizeX / 2));
  end;
  if FVisible then
    StartAnimation;
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
    if GetAttached(i) is TRangeIndicator then
      TRangeIndicator(GetAttached(i)).RedrawGlyph;
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

procedure TToken.DoAnimationStep;
begin
  if not FIsMoving then
    Exit;
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

function Map(Val, InValStart, InValEnd, OutValStart, OutValEnd: Double): Double;
begin
  Result := OutValStart + (OutValEnd - OutValStart) * ((Val - InValStart) / (InValEnd - InValStart));
end;

{ TRangeIndicator }

constructor TRangeIndicator.Create(X, Y, pWidth, pHeight: Integer);
begin
  //inherited Create;
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
  FOverlayIdx := -1;
  FCurAnimationStep := 0;
  FPath := '';
  FAttachedTo := nil;
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


function TRangeIndicator.GetXPos: Integer;
begin
  Result := inherited;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.XPos;
end;

function TRangeIndicator.GetYPos: Integer;
begin
  Result := inherited;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.YPos;
end;

function TRangeIndicator.GetXEndPos: Integer;
begin
  Result := FXTargetPos;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.XEndPos;
end;

function TRangeIndicator.GetYEndPos: Integer;
begin
  Result := FYTargetPos;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.YEndPos;
end;

function TRangeIndicator.GetAngle: Double;
begin
  Result := FAngle;
  if Assigned(FAttachedTo) then
    Result := FAttachedTo.Angle;
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

procedure TRangeIndicator.AttachTo(token: TToken);
begin
  FAttachedTo := token;
end;

procedure TRangeIndicator.Detach;
begin
  XPos := FAttachedTo.XPos;
  YPos := FAttachedTo.YPos;
  FXTargetPos := FAttachedTo.XEndPos;
  FYTargetPos := FAttachedTo.YEndPos;
  Angle := FAttachedTo.Angle;
  FAttachedTo.RemoveAttached(self);
  FAttachedTo := nil;
end;

function TRangeIndicator.IsAttached: Boolean;
begin
  Result := Assigned(FAttachedTo);
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
  FOverlayIdx := -1;
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


{ Easing-Functions }

// probably won't need all of these for this project, but wth, I ported them, I'm going to use them

function EaseLinear(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * (Time / Duration) + StartVal;
end;

function EaseInQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqr(Time / Duration) + StartVal;
end;

function EaseOutQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * (Time / Duration) * ((Time / Duration) - 2) + StartVal;
end;

function EaseInOutQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(t) + StartVal
  else
    Result := -ChangeAmt * 0.5 * ((t - 1) * (t - 3) - 1) + StartVal;
end;

function EaseInCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * t * t * t + StartVal;
end;

function EaseOutCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := ChangeAmt * (t * t * t + 1) + StartVal;
end;

function EaseInOutCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * t * t * t + StartVal
  else
    Result := ChangeAmt * 0.5 * ((t - 2) * (t - 2) * (t - 2) + 2) + StartVal;
end;

function EaseInQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqr(Sqr(Time / Duration)) + StartVal;
end;

function EaseOutQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := -ChangeAmt * (Sqr(Sqr(t)) - 1) + StartVal;
end;

function EaseInOutQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(Sqr(t)) + StartVal
  else
    Result := -ChangeAmt * 0.5 * (Sqr(Sqr(t - 2)) - 2) + StartVal;
end;

function EaseInQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * Sqr(Sqr(t)) * t + StartVal;
end;

function EaseOutQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := ChangeAmt * (Sqr(sqr(t)) * t + 1) + StartVal;
end;

function EaseInOutQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(Sqr(t)) * t + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sqr(Sqr(t - 2)) * (t - 2) + 2) + StartVal;
end;

function EaseInSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * Cos(Time / Duration * (PI / 2)) + ChangeAmt + StartVal;
end;

function EaseOutSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sin(Time / Duration * (PI / 2)) + StartVal;
end;

function EaseInOutSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * 0.5 * (Cos(PI * Time / Duration) - 1) + StartVal;
end;

function EaseInExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if SameValue(Time, 0.0) then
    Result := StartVal
  else
    Result := ChangeAmt * Power(2, 10 * (Time / Duration - 1)) + StartVal;
end;

function EaseOutExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if SameValue(Time, Duration) then
    Result := StartVal + ChangeAmt
  else
    Result := ChangeAmt * (-Power(2, -10 * Time / Duration) + 1) + StartVal;
end;

function EaseInOutExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if SameValue(Time, 0) then
    Result := StartVal
  else if SameValue(Time, Duration) then
    Result := StartVal + ChangeAmt
  else if (t / 2) < 1 then
    Result := ChangeAmt * 0.5 * Power(2, 10 * (t - 1)) + StartVal
  else
    Result := ChangeAmt * 0.5 * (-Power(2, -10 * (t - 1)) + 2) + StartVal;
end;

function EaseInCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * (Sqrt(1 - Sqr(Time / Duration)) - 1) + StartVal;
end;

function EaseOutCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqrt(1 - Sqr(Time / Duration - 1)) + StartVal;
end;

function EaseInOutCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := -ChangeAmt * 0.5 * (Sqrt(1 - Sqr(t)) - 1) + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sqrt(1 - Sqr(t - 2)) + 1) + StartVal;
end;

function EaseInElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * Sin(13 * PI / 2 * t) * Power(2, 10 * (t - 1)) + StartVal;
end;

function EaseOutElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * (Sin(-13 * PI / 2 * (t + 1)) * Power(2, -10 * t) + 1) + StartVal;
end;

function EaseInOutElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  if t < 0.5 then
    Result := ChangeAmt * 0.5 * Sin(13 * PI * t) * Power(2, 10 * ((2 * t) - 1)) + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sin(-13 * PI * t) * Power(2, -10 * (2 * t - 1)) + 2) + StartVal;
end;

function EaseInBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * (Sqr(t) * t - t * Sin(t * PI)) + StartVal;
end;

function EaseOutBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := 1 - Time / Duration;
  Result := ChangeAmt * (1 - (Sqr(t) * t - t * Sin(t * PI))) + StartVal;
end;

function EaseInOutBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  if (Time / Duration) < 0.5 then
  begin
    t := 2 * Time / Duration;
    Result := ChangeAmt * 0.5 * (Sqr(t) * t - t * Sin(t * PI)) + StartVal;
  end
  else
  begin
    t := 2 - 2 * Time / Duration;
    Result := ChangeAmt * (0.5 * (1 - (Sqr(t) * t - t * Sin(t * PI))) + 0.5) + StartVal;
  end;
end;

function EaseOutBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  if t < (1 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t)) + StartVal
  else if t < (2 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t - (1.5 / 2.75)) + 0.75) + StartVal
  else if t < (2.5 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t - (2.25 / 2.75)) + 0.9375) + StartVal
  else
    Result := ChangeAmt * (7.5625 * Sqr(t - (2.625 / 2.75)) + 0.984375) + StartVal;
end;

function EaseInBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt - EaseOutBounce(Duration - Time, 0, ChangeAmt, Duration) + StartVal;
end;

function EaseInOutBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if Time < (Duration / 2) then
    Result := EaseInBounce(Time * 2, 0, ChangeAmt, Duration) * 0.5 + StartVal
  else
    Result := EaseOutBounce(Time * 2 - Duration, 0, ChangeAmt, Duration) * 0.5 + ChangeAmt * 0.5 + StartVal;
end;

function EaseSmoothStep(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := EnsureRange(Time / Duration, 0, 1);
  Result := ChangeAmt * (t * t * (3 - 2 * t)) + StartVal;
end;

function EaseSmootherStep(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := EnsureRange(Time / Duration, 0, 1);
  Result := ChangeAmt * (t * t * t * (t * (t * 6 - 15) + 10)) + StartVal;
end;

function Ease(Time, StartVal, ChangeAmt, Duration: Double; eType: TEasingType): Double;
begin
  case eType of
    etLinear      : Result := EaseLinear(Time, StartVal, ChangeAmt, Duration);
    etOutQuad     : Result := EaseOutQuad(Time, StartVal, ChangeAmt, Duration);
    etInQuad      : Result := EaseInQuad(Time, StartVal, ChangeAmt, Duration);
    etInOutQuad   : Result := EaseInOutQuad(Time, StartVal, ChangeAmt, Duration);
    etInCubic     : Result := EaseInCubic(Time, StartVal, ChangeAmt, Duration);
    etOutCubic    : Result := EaseOutCubic(Time, StartVal, ChangeAmt, Duration);
    etInOutCubic  : Result := EaseInOutCubic(Time, StartVal, ChangeAmt, Duration);
    etInQuart     : Result := EaseInQuart(Time, StartVal, ChangeAmt, Duration);
    etOutQuart    : Result := EaseOutQuart(Time, StartVal, ChangeAmt, Duration);
    etInOutQuart  : Result := EaseInOutQuart(Time, StartVal, ChangeAmt, Duration);
    etInQuint     : Result := EaseInQuint(Time, StartVal, ChangeAmt, Duration);
    etOutQuint    : Result := EaseOutQuint(Time, StartVal, ChangeAmt, Duration);
    etInOutQuint  : Result := EaseInOutQuint(Time, StartVal, ChangeAmt, Duration);
    etInSine      : Result := EaseInSine(Time, StartVal, ChangeAmt, Duration);
    etOutSine     : Result := EaseOutSine(Time, StartVal, ChangeAmt, Duration);
    etInOutSine   : Result := EaseInOutSine(Time, StartVal, ChangeAmt, Duration);
    etInExpo      : Result := EaseInExpo(Time, StartVal, ChangeAmt, Duration);
    etOutExpo     : Result := EaseOutExpo(Time, StartVal, ChangeAmt, Duration);
    etInOutExpo   : Result := EaseInOutExpo(Time, StartVal, ChangeAmt, Duration);
    etInCirc      : Result := EaseInCirc(Time, StartVal, ChangeAmt, Duration);
    etOutCirc     : Result := EaseOutCirc(Time, StartVal, ChangeAmt, Duration);
    etInOutCirc   : Result := EaseInOutCirc(Time, StartVal, ChangeAmt, Duration);
    etInElastic   : Result := EaseInElastic(Time, StartVal, ChangeAmt, Duration);
    etOutElastic  : Result := EaseOutElastic(Time, StartVal, ChangeAmt, Duration);
    etInOutElastic: Result := EaseInOutElastic(Time, StartVal, ChangeAmt, Duration);
    etInBack      : Result := EaseInBack(Time, StartVal, ChangeAmt, Duration);
    etOutBack     : Result := EaseOutBack(Time, StartVal, ChangeAmt, Duration);
    etInOutBack   : Result := EaseInOutBack(Time, StartVal, ChangeAmt, Duration);
    etInBounce    : Result := EaseInBounce(Time, StartVal, ChangeAmt, Duration);
    etOutBounce   : Result := EaseOutBounce(Time, StartVal, ChangeAmt, Duration);
    etInOutBounce : Result := EaseInOutBounce(Time, StartVal, ChangeAmt, Duration);
    etSmoothStep  : Result := EaseSmoothStep(Time, StartVal, ChangeAmt, Duration);
    etSmootherStep: Result := EaseSmootherStep(Time, StartVal, ChangeAmt, Duration);
  else
    Result := 0;
  end;
end;

end.

