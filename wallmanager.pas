unit WallManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

TPointList = specialize TFPGList<TPoint>;
TIdxList = specialize TFPGList<Integer>;

TWallManager = class
private
  FPoints: TPointList;
  FWalls: TPointList; // Contains indices for the point list
  //FIdcs: TIdxList;
public  
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  procedure AddWall(P1, P2: TPoint);
  function GetPoint(idx: Integer): TPoint;
  function GetPointCount: Integer;
  function GetWall(idx: Integer): TPoint;
  function GetWallCount: Integer;
  function IsNonBlockingCorner(PIdx: Integer; dx, dy: Double): Boolean;
end;

var
  WallMgr: TWallManager;

implementation

uses
  Math;

{ TWallManager }

constructor TWallManager.Create;
begin
  inherited Create;
  FPoints := TPointList.Create;
  FWalls := TPointList.Create;
  //FIdcs := TIdxList.Create;
end;

destructor TWallManager.Destroy;
begin
  FPoints.Free;
  FWalls.Free;
  //FIdcs.Free;
  inherited;
end;

procedure TWallManager.Clear;
begin
  FPoints.Clear;
  FWalls.Free;
  //FIdcs.Clear;
end;

procedure TWallManager.AddWall(P1, P2: TPoint);
var
  i, p1Idx, p2Idx: Integer;
begin
  // Check if points exists in list
  p1Idx := -1;                      
  p2Idx := -1;
  for i := 0 to FPoints.Count - 1 do
  begin
    if FPoints[i] = P1 then
      p1Idx := i;
    if FPoints[i] = P2 then
      p2Idx := i;
  end;
  if p1Idx < 0 then
    p1Idx := FPoints.Add(P1);
  if p2Idx < 0 then
    p2Idx := FPoints.Add(P2);

  FWalls.Add(Point(p1Idx, P2Idx));
end;

function TWallManager.GetPoint(idx: Integer): TPoint;
begin
  Result := Point(0, 0);
  if (idx >= 0) and (idx < FPoints.Count) then
    Result := FPoints[idx];
end;

function TWallManager.GetPointCount: Integer;
begin
  Result := FPoints.Count;
end;

function TWallManager.GetWall(idx: Integer): TPoint;
begin
  Result := Point(-1, -1);
  if (idx >= 0) and (idx < FWalls.Count) then
    Result := FWalls[idx];
end;

function TWallManager.GetWallCount: Integer;
begin
  Result := FWalls.Count;
end;

function TWallManager.IsNonBlockingCorner(PIdx: Integer; dx, dy: Double): Boolean;
var
  pnts: TIdxList;
  i: Integer;
  CurWall: TPoint;
  pA, pC: TPoint;
  Sign0: Integer;
begin
  Result := False;
  pnts := TIdxList.Create;
  try
    // Collect all points that share a wall with PIdx
    for i := 0 to FWalls.Count - 1 do
    begin
      CurWall := FWalls[i];
      if CurWall.x = PIdx then
        pnts.Add(CurWall.Y)
      else if CurWall.Y = PIdx then
        pnts.Add(CurWall.X);
    end;
    // We should not have a point with no connections in the list, but to be save...
    if pnts.Count > 0 then
    begin                                                              
      // Check if all points are on the same side
      pA := GetPoint(PIdx);
      pC := GetPoint(pnts[0]);
      Sign0 := Sign(dx * (pC.Y - pA.Y) - dy * (pC.X - pA.X));

      Result := True;
      for i := 1 to pnts.Count - 1 do
      begin
        pC := GetPoint(pnts[i]);
        Result := Result or (Sign0 = Sign(dx * (pC.Y - pA.Y) - dy * (pC.X - pA.X)));
      end;
    end;
  finally
    pnts.Free;
  end;
end;

end.

