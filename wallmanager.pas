{Copyright (c) 2023-2026 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit WallManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, BGRABitmap, BgraBitmapTypes;

type

TMapWall = class
public
  P1, P2: Integer;
  IsPortal: Boolean;
  IsOpen: Boolean;
  //function DistFromPnt(pnt: TPoint): Double;
  function ToString: string;
  procedure FromString(str: string);
end;

TPointList = specialize TFPGList<TPoint>;
TWallList = specialize TFPGList<TMapWall>;
TIdxList = specialize TFPGList<Integer>;

TWallManager = class
private
  FPoints: TPointList;
  FWalls: TWallList; // Contains indices for the point list
  //function IsNonBlockingCorner(PIdx: Integer; dx, dy: Double; var dir: Integer): Boolean;
  function GetWallDistFromPnt(pnt: TPoint; WallIdx: Integer): Double;
public  
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  function AddPoint(P: TPoint): Integer;
  procedure AddWall(P1, P2: TPoint; IsPortal: Boolean = False; Open: Boolean = False); overload;
  procedure AddWall(P1, P2: Integer; IsPortal: Boolean = False; Open: Boolean = False); overload;
  procedure AddPortal(P1, P2: TPoint; Open: Boolean);
  function GetPoint(idx: Integer): TPoint;
  function GetPointCount: Integer;        
  function GetPointIdxAtPos(x, y: Integer; MaxDist: Double = 4): Integer;
  function GetWall(idx: Integer): TMapWall;
  function GetWallCount: Integer;
  function GetWallIsPortal(idx: Integer): Boolean;
  function GetLoSPolygon(centerPnt: TPoint; BoundingBox: TRect): ArrayOfTPointF;
  function GetLoSMap(centerPnt: TPoint; BoundingBox: TRect; scale: Double): TBGRABitmap;
  function GetMinBoundingBox: TRect;
  function GetWallAtPos(pnt: TPoint; PortalOnly: Boolean = False): Integer;
  procedure RemovePoint(idx: Integer);
  procedure RemoveWall(idx: Integer);
  procedure MovePoint(idx: Integer; NewPos: TPoint);

  procedure SaveToStr(str: string);
  procedure LoadFromStr(str: string);
end;

implementation

uses
  Math,
  RPGUtils, RPGTypes;

{ TMapWall }

function TMapWall.ToString: string;
begin
  Result := IntToStr(P1) + ';' + IntToStr(P2) + ';' + IntToStr(Ord(IsPortal)) + ';' + IntToStr(Ord(IsOpen));
end;

procedure TMapWall.FromString(str: string);
var list: TStringList;
begin
  list := TStringList.Create;
  try                          
    list.Delimiter := ';';
    list.StrictDelimiter := True;
    list.DelimitedText := str;

    if list.Count >= 4 then
    begin
      P1 := StrToIntDef(list[0], 0);
      P2 := StrToIntDef(list[1], 0);
      IsPortal := StrToIntDef(list[2], 0) <> 0;
      IsOpen   := StrToIntDef(list[3], 0) <> 0;
    end;

  finally
    list.Free;
  end;
end;

{function TMapWall.DistFromPnt: Double;
begin
  Result := Abs((Pnt.Y - P1.Y) * (P2.X - P1.X) - (P2.Y - P1.Y) * (Pnt.X - P1.X)) / Hypot(P2.X - P1.X, P2.Y - P1.Y);
end;}

{ TWallManager }

constructor TWallManager.Create;
begin
  inherited Create;
  FPoints := TPointList.Create;
  FWalls := TWallList.Create;
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
  FWalls.Clear;
  //FIdcs.Clear;
end;

procedure TWallManager.SaveToStr(str: string);
begin
  //
end;

procedure TWallManager.LoadFromStr(str: string);
begin
  //
end;

function TWallManager.AddPoint(P: TPoint): Integer;
var i: Integer;
begin         
  // Check if points exists in list
  // Do not add in that case, just return the index
  for i := 0 to FPoints.Count - 1 do
    if FPoints[i] = P then
      Exit(i);
  Result := FPoints.Add(P);
end;

procedure TWallManager.AddWall(P1, P2: TPoint; IsPortal: Boolean = False; Open: Boolean = False);
var
  i, p1Idx, p2Idx: Integer;
  tmpWall: TMapWall;
begin
  p1Idx := AddPoint(P1);
  p2Idx := AddPoint(P2);

  tmpWall := TMapWall.Create;
  tmpWall.P1 := p1Idx;
  tmpWall.P2 := p2Idx;
  tmpWall.IsPortal := IsPortal;
  tmpWall.IsOpen := Open;

  FWalls.Add(tmpWall);
end;

procedure TWallManager.AddWall(P1, P2: Integer; IsPortal: Boolean = False; Open: Boolean = False);
var tmpWall: TMapWall;
begin
  if InRange(P1, 0, FPoints.Count - 1) and InRange(P2, 0, FPoints.Count - 1) and (P1 <> P2) then
  begin
    tmpWall := TMapWall.Create;
    tmpWall.P1 := P1;
    tmpWall.P2 := P2;
    tmpWall.IsPortal := IsPortal;
    tmpWall.IsOpen := Open;

    FWalls.Add(tmpWall);
  end;
end;

procedure TWallManager.AddPortal(P1, P2: TPoint; Open: Boolean);
var
  i, p1Idx, p2Idx: Integer;
  tmpPortal: TMapWall;
begin
  AddWall(P1, P2, True, Open);
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

function TWallManager.GetPointIdxAtPos(x, y: Integer; MaxDist: Double = 4): Integer;
var
  i: Integer;
  SqrDist: Double;
begin
  Result := -1;
  SqrDist := MaxDist * MaxDist;
  for i := 0 to FPoints.Count - 1 do
    if (Sqr(FPoints[i].X - x) + Sqr(FPoints[i].y - y)) <= SqrDist then
      Exit(i);
end;

procedure TWallManager.RemovePoint(idx: Integer);
var
  i: Integer;
  tmpWall, DelWall: TMapWall;
begin
  if (idx < 0) or (idx >= FPoints.Count) then
    Exit;
  // we need to remove all walls and portals that use this point,
  // and shift down every reference to points after this one
  DelWall := nil;
  for i := FWalls.Count - 1 downto 0 do
  begin
    tmpWall := GetWall(i);
    if (tmpWall.P1 = idx) or (tmpWall.P2 = idx) then
    begin
      DelWall := tmpWall;
      FWalls.Delete(i);
      DelWall.Free;
    end
    else
    begin
      if tmpWall.P1 > idx then
        Dec(tmpWall.P1);
      if tmpWall.P2 > idx then
        Dec(tmpWall.P2);
    end;
  end;

  // Finally, delete the point
  FPoints.Delete(idx);
end;

procedure TWallManager.RemoveWall(idx: Integer);
begin
  if (idx >= 0) and (idx < FWalls.Count) then
  begin
    FWalls[idx].Free;
    FWalls.Delete(idx);
  end;
end;

procedure TWallManager.MovePoint(idx: Integer; NewPos: TPoint);
begin
  if (idx >= 0) and (idx < FPoints.Count) then
    FPoints[idx] := NewPos;
end;

function TWallManager.GetWall(idx: Integer): TMapWall;
begin
  Result := nil;
  if (idx >= 0) and (idx < FWalls.Count) then
    Result := FWalls[idx];
end;

function TWallManager.GetWallIsPortal(idx: Integer): Boolean;
var tmpWall: TMapWall;
begin
  tmpWall := GetWall(idx);
  Result := Assigned(tmpWall) and tmpWall.IsPortal;
end;

function TWallManager.GetWallCount: Integer;
begin
  Result := FWalls.Count;
end;

function TWallManager.GetWallDistFromPnt(pnt: TPoint; WallIdx: Integer): Double;
var WallP1, WallP2: TPoint;
begin
  Result := MAXDOUBLE;   
  if (WallIdx >= 0) and (WallIdx < FWalls.Count) then
  begin
    WallP1 := GetPoint(FWalls[WallIdx].P1);
    WallP2 := GetPoint(FWalls[WallIdx].P2);
    Result := GetPointDistFromLine(WallP1, WallP2, pnt);
  end;
end;

function TWallManager.GetWallAtPos(pnt: TPoint; PortalOnly: Boolean = False): Integer;
var
  i: Integer;
  CurDist, MinDist: Double;
const
  MAXDIST = 5;
begin
  Result := -1;
  MinDist := MAXDOUBLE;

  for i := 0 to FWalls.Count - 1 do
  begin
    CurDist := GetWallDistFromPnt(pnt, i);
    if (CurDist <= MAXDIST) and (CurDist < MinDist) and (not PortalOnly or GetWallIsPortal(i)) then
    begin
      MinDist := CurDist;
      Result := i;
    end;
  end;
end;

function TWallManager.GetMinBoundingBox: TRect;
var
  i: Integer;
  CurPnt: TPoint;
begin
  Result := Rect(0, 0, 0, 0);
  for i := 0 to FPoints.Count - 1 do
  begin
    CurPnt := FPoints[i];
    Result.Left := Min(Result.Left, CurPnt.X);
    Result.Top := Min(Result.Top, CurPnt.Y);
    Result.Right := Max(Result.Right, CurPnt.X);
    Result.Bottom := Max(Result.Bottom, CurPnt.Y);
  end;
end;

// Failed attempt to simplify. This is several orders of magnitude slower than the last version.
(*function TWallManager.GetLoSMap(centerPnt: TPoint; BoundingBox: TRect; scale: Double): TBGRABitmap;
var
  i, j, k: Integer;
  p: PBGRAPixel;
  CurPx, CurWall: TPoint;
  pW1, pW2, pWC: TPoint;
  sgnW1, sgnW2, sgnW0: Integer;
  CurVal: integer;
begin
  // Soft Shadow-Function: z = x / (x + y); catch 0/0
  Result := TBGRABitmap.Create(BoundingBox.Width, BoundingBox.Height, BGRA(255, 255, 255));
  for i := 0 to Result.Height - 1 do
  begin
    p := Result.ScanLine[i];
    for j := 0 to Result.Width - 1 do
    begin
      CurPx := Point(Round((BoundingBox.Left + j) / scale), Round((BoundingBox.Top + i) / scale));
      CurVal := 1;
      k := 0;
      while (CurVal > 0) and (k < GetWallCount) do
      begin
        CurWall := GetWall(k);
        pW1 := GetPoint(CurWall.X);
        pW2 := GetPoint(CurWall.Y);
        pWC := Point((pW1.X + pW2.X) div 2, (pW1.Y + pW2.Y) div 2);
        sgnW1 := GetPointSideOfLine(centerPnt, pW1, pWC);
        sgnW2 := GetPointSideOfLine(centerPnt, pW2, pWC);
        sgnW0 := GetPointSideOfLine(pW1, pW2, centerPnt);
        CurVal := CurVal * Ord(not ((sgnW1 = GetPointSideOfLine(centerPnt, pW1, CurPx)) and (sgnW2 = GetPointSideOfLine(centerPnt, pW2, CurPx)) and (sgnW0 <> GetPointSideOfLine(pW1, pW2, CurPx))));

        Inc(k);
      end;
      p^.red := 255 * CurVal;
      p^.green := p^.red;
      p^.blue := p^.red;

      Inc(p);
    end;
  end;
end; *)

// Second attempt to simplify.
// This _works_, and is reasonably fast (still slower than the long solution below),
// but produces a lot of artifacts.
// I am not going to use it in its current state, but this might be a better base
// for a soft shadow-solution.
function TWallManager.GetLoSMap(centerPnt: TPoint; BoundingBox: TRect; scale: Double): TBGRABitmap;
var
  i, j: Integer;
  CurWall: TMapWall;
  pW1, pW2, pWc: TPoint;
  pWLeft, pWRight: TPoint;
  pOffset: TPoint;
  ClipRect: TRect; // map-coordinates
  ClipLinePnts: array[0..3, 0..1] of TPointF;
  StartLineIdx: Integer;
  int: TPointF;
  PolyPnts: ArrayofTPointF;
  PntCount: Integer;
begin
  Result := TBGRABitmap.Create(BoundingBox.Width, BoundingBox.Height, BGRA(255, 255, 255));
  pOffset := Point(-BoundingBox.Left, -BoundingBox.Top);
  ClipRect := Bounds(Boundingbox.Left, BoundingBox.Top, Round(BoundingBox.Width / scale), Round(BoundingBox.Height / scale));
  ClipLinePnts[0][0] := TPointF.Create(ClipRect.TopLeft);
  ClipLinePnts[0][1] := TPointF.Create(ClipRect.Right, ClipRect.Top);
  ClipLinePnts[1][0] := ClipLinePnts[0][1];
  ClipLinePnts[1][1] := TPointF.Create(ClipRect.BottomRight);
  ClipLinePnts[2][0] := ClipLinePnts[1][1];
  ClipLinePnts[2][1] := TPointF.Create(ClipRect.Left, ClipRect.Bottom);
  ClipLinePnts[3][0] := ClipLinePnts[2][1];
  ClipLinePnts[3][1] := TPointF.Create(ClipRect.TopLeft);
  for i := 0 to GetWallCount - 1 do
  begin
    CurWall := GetWall(i);
    pW1 := GetPoint(CurWall.P1);
    pW2 := GetPoint(CurWall.P2);
    pWc := Point((pW1.X + pW2.X) div 2, (pW1.Y + pW2.Y) div 2);
    if GetPointSideOfLine(centerPnt, pWc, pW1) < 0 then
    begin
      pWLeft := pW1;
      pWRight := pW2;
    end
    else
    begin
      pWLeft := pW2;
      pWRight := pW1;
    end;
    SetLength(PolyPnts, 7); // Cannot have more points in a rect
    PntCount := 0;
    // Store points that belong to the view shadow polygon, convert straight to viewport-coordinates
    PolyPnts[PntCount] := TPointF.Create(pWLeft) * scale;
    PolyPnts[PntCount].Offset(pOffset);
    Inc(PntCount);

    StartLineIdx := -1;
    for j := 0 to Length(ClipLinePnts) - 1 do
    begin
      if GetIntersection_RaySegment(TPointF.Create(centerPnt), TPointF.Create(pWLeft - centerPnt), ClipLinePnts[j][0], ClipLinePnts[j][1], int) then
      begin
        StartLineIdx := j;
        PolyPnts[PntCount] := int * scale;
        PolyPnts[PntCount].Offset(pOffset);
        Inc(PntCount);
        Break;
      end;
    end;

    for j := 0 to Length(ClipLinePnts) - 1 do
    begin
      if GetIntersection_RaySegment(TPointF.Create(centerPnt), TPointF.Create(pWRight - centerPnt), ClipLinePnts[(j + StartLineIdx) mod 4][0], ClipLinePnts[(j + StartLineIdx) mod 4][1], int) then
      begin
        PolyPnts[PntCount] := int * scale;
        PolyPnts[PntCount].Offset(pOffset);
        Inc(PntCount);
        Break;
      end
      else
      begin
        PolyPnts[PntCount] := ClipLinePnts[(j + StartLineIdx) mod 4][1] * scale;
        PolyPnts[PntCount].Offset(pOffset);
        Inc(PntCount);
      end;
    end;

    // Add other side of wall
    PolyPnts[PntCount] := TPointF.Create(pWRight) * scale;
    PolyPnts[PntCount].Offset(pOffset);
    Inc(PntCount);

    SetLength(PolyPnts, PntCount);
    Result.FillPolyAntialias(PolyPnts, BGRA(0, 0, 0));
  end;
end;

function TWallManager.GetLoSPolygon(centerPnt: TPoint; BoundingBox: TRect): ArrayOfTPointF;
var
  tmpPnts, tmpWalls: TPointList;
  sortList, OpenWalls: TIdxList;
  i, j, tmp, LowestIdx, CurClosestWall, PrevWall: Integer;
  CurAngle, LowestAngle, ClosestDist, CurDist: Double;
  boundingPnts: array[0..3] of Integer;
  rD, IntPnt, AddPnt, NewPnt: TPointF;
  CurWall, CurPnt: TPoint;
  CurSide: Integer;
  TotalPnts: Integer;
  tmpAng: Double;
  InPnt, OutPnt: TPoint;
  OutIdx: Integer;
  Liststr: string;
  tmpWallOrPortal: TMapWall;

  function AddTmpPnt(pnt: TPoint): Integer;
  var idx, pntNo: Integer;
  begin
    pntNo := -1;
    for idx := 0 to tmpPnts.Count - 1 do
      if pnt = tmpPnts[idx] then
        pntNo := idx;
    if pntNo >= 0 then
      Result := pntNo
    else
      Result := tmpPnts.Add(pnt);
  end;

  function IsNonBlockingCorner(PIdx: Integer; dx, dy: Double; out dir: Integer): Boolean;
  var
    pnts: TIdxList;
    k: Integer;
    CWall: TPoint;
    pA, pC: TPoint;
    Sign0, CurSign: Integer;
  begin
    Result := False;
    dir := 0;
    pnts := TIdxList.Create;
    try
      // Collect all points that share a wall with PIdx
      for k := 0 to tmpWalls.Count - 1 do
      begin
        CWall := tmpWalls[k];
        if CWall.x = PIdx then
          pnts.Add(CWall.Y)
        else if CWall.Y = PIdx then
          pnts.Add(CWall.X);
      end;
      // We should not have a point with no connections in the list, but to be save...
      if pnts.Count > 0 then
      begin
        // Check if all points are on the same side
        pA := tmpPnts[PIdx];
        pC := tmpPnts[pnts[0]];
        Sign0 := Sign(dx * (pC.Y - pA.Y) - dy * (pC.X - pA.X));

        Result := True;
        for k := 1 to pnts.Count - 1 do
        begin
          pC := tmpPnts[pnts[k]];
          CurSign := Sign(dx * (pC.Y - pA.Y) - dy * (pC.X - pA.X));
          if CurSign <> 0 then
          begin
            if Sign0 = 0 then
              Sign0 := CurSign
            else
              Result := Result and (Sign0 = CurSign);
          end;
        end;
        if Result then
          dir := Sign0;
      end;
    finally
      pnts.Free;
    end;
  end;

  function GetCloserWallToPnt(CurPnt, Wall1, Wall2: Integer): Integer;
  var
    P1Idx, P2Idx: Integer;
    tmpWall: TPoint;
    ip: TPointF;
  begin
    tmpWall := tmpWalls[Wall1];
    if tmpWall.X = CurPnt then
      P1Idx := tmpWall.Y
    else
      P1Idx := tmpWall.X;
    tmpWall := tmpWalls[Wall2];
    if tmpWall.X = CurPnt then
      P2Idx := tmpWall.Y
    else
      P2Idx := tmpWall.X;

    // Test if P2 and Center are on the same side of P0-P1
    if GetPointSideOfLine(tmpPnts[CurPnt], tmpPnts[P1Idx], CenterPnt) = GetPointSideOfLine(tmpPnts[CurPnt], tmpPnts[P1Idx], tmpPnts[P2Idx]) then
      Result := Wall2
    else
      Result := Wall1;
  end;

  function DoesWallBegin(pWall: TPoint; pPnt: Integer): Boolean;
  var otherPnt: Integer;
  begin
    if pWall.X = pPnt then
      otherPnt := pWall.Y
    else if pWall.Y = pPnt then
      otherPnt := pWall.X
    else // Wall does not contain the point? Obviously it cannot start there
      Exit(False);

    Result := GetPointSideOfLine(centerPnt, tmpPnts[pPnt], tmpPnts[otherPnt]) > 0;
  end;

begin
  tmpPnts := TPointList.Create;
  tmpWalls := TPointList.Create;
  sortList := TIdxList.Create;
  OpenWalls := TIdxList.Create;
  try
    // Copy values from main list
    // Todo: Remove points and walls completely outside the clipping rect, but
    // keep points that belong to walls partially inside the rect
    tmpPnts.AddList(FPoints);

    // Add all walls and all closed portals
    for i := 0 to GetWallCount - 1 do
    begin
      tmpWallOrPortal := GetWall(i);
      if not (tmpWallOrPortal.IsPortal and tmpWallOrPortal.IsOpen) then
      tmpWalls.Add(Point(tmpWallOrPortal.P1, tmpWallOrPortal.P2));
    end;

    // Add walls for bounding box
    boundingPnts[0] := AddTmpPnt(BoundingBox.TopLeft);
    boundingPnts[1] := AddTmpPnt(Point(BoundingBox.Right, BoundingBox.Top));
    boundingPnts[2] := AddTmpPnt(BoundingBox.BottomRight);
    boundingPnts[3] := AddTmpPnt(Point(BoundingBox.left, BoundingBox.Bottom));

    tmpWalls.Add(Point(boundingPnts[0], boundingPnts[1]));
    tmpWalls.Add(Point(boundingPnts[1], boundingPnts[2]));
    tmpWalls.Add(Point(boundingPnts[2], boundingPnts[3]));
    tmpWalls.Add(Point(boundingPnts[3], boundingPnts[0]));

    // preprocessing: Remove all walls completely outside of the bounding box,
    // clip walls partially outside

    for i := tmpWalls.Count - 5 downto 0 do
    begin
      // TODO: Do proper line clipping here
      if not (BoundingBox.Contains(tmpPnts[tmpWalls[i].X]) or BoundingBox.Contains(tmpPnts[tmpWalls[i].Y])) then
      begin
        //tmpWalls.Delete(i)
      end
      else if not (BoundingBox.Contains(tmpPnts[tmpWalls[i].X]) and BoundingBox.Contains(tmpPnts[tmpWalls[i].Y])) then
      begin
        // move point
        if BoundingBox.Contains(tmpPnts[tmpWalls[i].X]) then
        begin
          InPnt := tmpPnts[tmpWalls[i].X];
          OutPnt := tmpPnts[tmpWalls[i].Y];
          OutIdx := tmpWalls[i].Y;
        end
        else
        begin
          InPnt := tmpPnts[tmpWalls[i].Y];
          OutPnt := tmpPnts[tmpWalls[i].X]; 
          OutIdx := tmpWalls[i].X;
        end;
        rD := TPointF.Create(OutPnt - InPnt);
        rD.Normalize;

        for j := tmpWalls.Count - 4 to tmpWalls.Count - 1 do
        if GetIntersection_RaySegment(TPointF.Create(InPnt), rD, TPointF.Create(tmpPnts[tmpWalls[j].X]), TPointF.Create(tmpPnts[tmpWalls[j].Y]), IntPnt) then
        begin
          tmpPnts[OutIdx] := IntPnt.Round;
          Break;
        end;
      end;
    end;

    for i := 0 to tmpPnts.Count - 1 do
      sortList.Add(i);

    // Estimate result size: should be no more than number of points in the list now

    SetLength(Result, tmpPnts.Count);
    TotalPnts := 0;

    // Sort by angle of points from center point. Since we use indices to local lists,
    // we cannot use the sort-method for this and have to do it by hand. Use Selection sort for now.
    for i := 0 to sortList.Count - 1 do
    begin
      LowestAngle := MAXDOUBLE;
      LowestIdx := -1;
      for j := i to sortList.Count - 1 do
      begin
        CurPnt := tmpPnts[sortList[j]];
        CurAngle := ArcTan2(centerPnt.y - CurPnt.y, centerPnt.X - CurPnt.x);
        tmpAng := RadToDeg(CurAngle);
        if CurAngle < LowestAngle then
        begin
          LowestAngle := CurAngle;
          LowestIdx := j;
        end;
      end;
      sortList.Move(LowestIdx, i);
      listStr := '';
      for tmp := 0 to SortList.Count - 1 do
        listStr := listStr + IntToStr(SortList[tmp]) + ' ';
    end;

    // Now trace list of points...

    // First, check all walls for intersections with ray to first point, add to open list
    rD := TPointF.Create(tmpPnts[sortList[0]] - centerPnt);
    rD.Normalize;
    LowestIdx := -1;
    ClosestDist := MAXDOUBLE;
    for i := 0 to tmpWalls.Count - 1 do
    begin
      CurWall := tmpWalls[i];
      if GetIntersection_RaySegment(TPointF.Create(centerPnt), rD, TPointF.Create(tmpPnts[CurWall.X]), TPointF.Create(tmpPnts[CurWall.Y]), IntPnt) then
      begin
        // Do not add walls that begin in this point!
        if not DoesWallBegin(CurWall, sortList[0]) then
        begin
          OpenWalls.Add(i);
          if IntPnt.Distance(TPointF.Create(centerPnt)) < ClosestDist then
          begin
            ClosestDist := IntPnt.Distance(TPointF.Create(CenterPnt));
            LowestIdx := i;
          end;
        end;
      end;
    end;
    CurClosestWall := LowestIdx;

    for i := 0 to sortList.Count - 1 do
    begin
      // Find all walls that begin / end at this point
      for j := 0 to tmpWalls.Count - 1 do
      begin
        if (sortList[i] = tmpWalls[j].x) then
        begin
          CurSide := GetPointSideOfLine(centerPnt, tmpPnts[tmpWalls[j].x], tmpPnts[tmpWalls[j].y]);
          if CurSide <= 0 then // wall ended at this point - also wall parallel to the ray begins and ends here
            OpenWalls.Remove(j)
          else //if CurSide > 1 then
          begin
            if OpenWalls.IndexOf(j) < 0 then
              OpenWalls.Add(j);
          end;
        end
        else if (sortList[i] = tmpWalls[j].y) then
        begin
          CurSide := GetPointSideOfLine(centerPnt, tmpPnts[tmpWalls[j].y], tmpPnts[tmpWalls[j].x]);
          if CurSide <= 0 then // wall ended at this point - also wall parallel to the ray begins and ends here
            OpenWalls.Remove(j)
          else //if CurSide > 1 then
          begin
            if OpenWalls.IndexOf(j) < 0 then
              OpenWalls.Add(j);
          end;
        end;
      end;

      // Check which of the open wall is the nearest now
      PrevWall := CurClosestWall;
      LowestIdx := -1;
      ClosestDist := MAXDOUBLE;
      rD := TPointF.Create(tmpPnts[sortList[i]] - centerPnt);
      rD.Normalize;
      for j := 0 to OpenWalls.Count - 1 do
      begin
        CurWall := tmpWalls[OpenWalls[j]];
        if GetIntersection_RaySegment(TPointF.Create(centerPnt), rD, TPointF.Create(tmpPnts[CurWall.X]), TPointF.Create(tmpPnts[CurWall.Y]), IntPnt) then
        begin
          CurDist := IntPnt.Distance(TPointF.Create(centerPnt));
          if SameValue(CurDist, ClosestDist, 1E-4) then
          begin
            if LowestIdx >= tmpWalls.Count - 4 then
              LowestIdx := OpenWalls[j]
            else
            begin
              LowestIdx := GetCloserWallToPnt(sortList[i], LowestIdx, OpenWalls[j]);
            end;

          end
          else if CurDist < ClosestDist then
          begin
            ClosestDist := IntPnt.Distance(TPointF.Create(CenterPnt));
            LowestIdx := OpenWalls[j];
          end;
        end;
      end;
      CurClosestWall := LowestIdx;

      if PrevWall <> CurClosestWall then
      begin
        // make room for more points, if required
        if Length(Result) <= TotalPnts + 2 then
          SetLength(Result, Length(Result) * 2);

        NewPnt := TPointF.Create(tmpPnts[sortList[i]]);

        // Check if ray needs to continue past the point
        if IsNonBlockingCorner(sortList[i], rD.x, rD.y, CurSide) then
        begin
          // Check all other open walls for intersections with ray, starting at current point
          LowestIdx := -1;
          ClosestDist := MAXDOUBLE;
          for j := 0 to OpenWalls.Count - 1 do
          begin
            //if OpenWalls[j] <> CurClosestWall then
            begin
              CurWall := tmpWalls[OpenWalls[j]];
              if GetIntersection_RaySegment(TPointF.Create(tmpPnts[sortList[i]]), rD, TPointF.Create(tmpPnts[CurWall.X]), TPointF.Create(tmpPnts[CurWall.Y]), IntPnt) then
              begin
                CurDist := IntPnt.Distance(TPointF.Create(tmpPnts[sortList[i]]));
                if (CurDist < ClosestDist) and (CurDist > 0) then
                begin
                  ClosestDist := CurDist;
                  LowestIdx := j;
                  AddPnt := IntPnt;
                end;
              end;
            end;
          end;
          // Add new point before checked point if all walls are right of the current point, otherwise add after
          if CurSide > 0 then
          begin
            Result[TotalPnts] := AddPnt;
            Inc(TotalPnts);
            Result[TotalPnts] := NewPnt;
            Inc(TotalPnts);
          end
          else
          begin
            Result[TotalPnts] := NewPnt;
            Inc(TotalPnts);
            Result[TotalPnts] := AddPnt;
            Inc(TotalPnts);
          end;
        end
        else
        begin
          // Just add current point to list// Can we see the current point from the center?
          GetIntersection_RaySegment(TPointF.Create(centerPnt), rD,
                                     TPointF.Create(tmpPnts[tmpWalls[CurClosestWall].X]),
                                     TPointF.Create(tmpPnts[tmpWalls[CurClosestWall].Y]),
                                     IntPnt);
          if NewPnt.Distance(TPointF.Create(CenterPnt)) > IntPnt.Distance(TPointF.Create(CenterPnt)) then
            NewPnt := IntPnt;
          Result[TotalPnts] := NewPnt;
          Inc(TotalPnts);
        end;



      end;
    end;

    SetLength(Result, TotalPnts);
    {loop over endpoints:
      remember which wall is nearest
      add any walls that BEGIN at this endpoint to 'walls'
      remove any walls that END at this endpoint from 'walls'

      figure out which wall is now nearest
      if the nearest wall changed:
          fill the current triangle and begin a new one}

  finally
    tmpPnts.Free;
    tmpWalls.Free;
    SortList.Free;
    OpenWalls.Free;
  end;
end;

end.

