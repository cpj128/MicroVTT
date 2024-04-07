{Copyright (c) 2023 Stephan Breer

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

TMapPortal = class
public
  P1, P2: Integer;
  IsOpen: Boolean;
  //function DistFromPnt(pnt: TPoint): Double;
end;

TPointList = specialize TFPGList<TPoint>;
TPortalList = specialize TFPGList<TMapPortal>;
TIdxList = specialize TFPGList<Integer>;

TWallManager = class
private
  FPoints: TPointList;
  FWalls: TPointList; // Contains indices for the point list
  FPortals: TPortalList;
  //FIdcs: TIdxList;                                                   
  //function IsNonBlockingCorner(PIdx: Integer; dx, dy: Double; var dir: Integer): Boolean;
  function GetDoorDistFromPnt(pnt: TPoint; DoorIdx: Integer): Double;
public  
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  procedure AddWall(P1, P2: TPoint);
  procedure AddPortal(P1, P2: TPoint; Open: Boolean);
  function GetPoint(idx: Integer): TPoint;
  function GetPointCount: Integer;
  function GetWall(idx: Integer): TPoint;
  function GetWallCount: Integer;
  function GetPortal(idx: Integer): TMapPortal;
  function GetPortalCount: Integer;
  function GetLoSPolygon(centerPnt: TPoint; BoundingBox: TRect): ArrayOfTPointF;
  function GetLoSMap(centerPnt: TPoint; BoundingBox: TRect; scale: Double): TBGRABitmap;
  function GetMinBoundingBox: TRect;
  function GetPortalAtPos(pnt: TPoint): Integer;
end;

implementation

uses
  Math,
  RPGUtils, RPGTypes;

{ TMapPortal }

{function TMapPortal.DistFromPnt: Double;
begin
  Result := Abs((Pnt.Y - P1.Y) * (P2.X - P1.X) - (P2.Y - P1.Y) * (Pnt.X - P1.X)) / Hypot(P2.X - P1.X, P2.Y - P1.Y);
end;}

{ TWallManager }

constructor TWallManager.Create;
begin
  inherited Create;
  FPoints := TPointList.Create;
  FWalls := TPointList.Create;
  FPortals := TPortalList.Create;
  //FIdcs := TIdxList.Create;
end;

destructor TWallManager.Destroy;
begin
  FPoints.Free;
  FWalls.Free;
  FPortals.Free;
  //FIdcs.Free;
  inherited;
end;

procedure TWallManager.Clear;
begin
  FPoints.Clear;
  FWalls.Clear;
  FPortals.Clear;
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

procedure TWallManager.AddPortal(P1, P2: TPoint; Open: Boolean);
var
  i, p1Idx, p2Idx: Integer;
  tmpPortal: TMapPortal;
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

  //FWalls.Add(Point(p1Idx, P2Idx));
  tmpPortal := TMapPortal.Create;
  tmpPortal.p1 := p1Idx;
  tmpPortal.p2 := p2Idx;
  tmpPortal.IsOpen := Open;
  FPortals.add(tmpPortal);
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

function TWallManager.GetPortal(idx: Integer): TMapPortal;
begin
  Result := nil;
  if (idx >= 0) and (idx < FPortals.Count) then
    Result := FPortals[idx];
end;

function TWallManager.GetPortalCount: Integer;
begin
  Result := FPortals.Count;
end;

function TWallManager.GetDoorDistFromPnt(pnt: TPoint; DoorIdx: Integer): Double;
var DoorP1, DoorP2: TPoint;
begin
  Result := MAXDOUBLE;
  DoorP1 := GetPoint(FPortals[DoorIdx].P1);
  DoorP2 := GetPoint(FPortals[DoorIdx].P2);
  if (DoorIdx >= 0) and (DoorIdx < FPortals.Count) then
    Result := Abs((Pnt.Y - DoorP1.Y) * (DoorP2.X - DoorP1.X)
                  - (DoorP2.Y - DoorP1.Y) * (Pnt.X - DoorP1.X))
                  / Hypot(DoorP2.X - DoorP1.X, DoorP2.Y - DoorP1.Y);
end;

function TWallManager.GetPortalAtPos(pnt: TPoint): Integer;
var
  i: Integer;
  CurDist, MinDist: Double;
const
  MAXDIST = 5;
begin
  Result := -1;
  MinDist := MAXDOUBLE;

  for i := 0 to FPortals.Count - 1 do
  begin
    CurDist := GetDoorDistFromPnt(pnt, i);
    if (CurDist <= MAXDIST) and (CurDist < MinDist) then
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
  CurWall: TPoint;
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
    pW1 := GetPoint(CurWall.X);
    pW2 := GetPoint(CurWall.Y);
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
  tmpPortal: TMapPortal;

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
    tmpWalls.AddList(FWalls);

    // Add Portals only if closed
    for i := 0 to GetPortalCount - 1 do
    begin
      tmpPortal := GetPortal(i);
      if not tmpPortal.IsOpen then
        tmpWalls.Add(Point(tmpPortal.P1, tmpPortal.P2));
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
    // we cannot use the sort-methodfor this and have to do it by hand. Use Selection sort for now.
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

