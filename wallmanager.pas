unit WallManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, BgraBitmapTypes;

type

TPointList = specialize TFPGList<TPoint>;
TIdxList = specialize TFPGList<Integer>;

TWallManager = class
private
  FPoints: TPointList;
  FWalls: TPointList; // Contains indices for the point list
  //FIdcs: TIdxList;                                                   
  //function IsNonBlockingCorner(PIdx: Integer; dx, dy: Double; var dir: Integer): Boolean;
public  
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  procedure AddWall(P1, P2: TPoint);
  function GetPoint(idx: Integer): TPoint;
  function GetPointCount: Integer;
  function GetWall(idx: Integer): TPoint;
  function GetWallCount: Integer;
  function GetLoSPolygon(centerPnt: TPoint; BoundingBox: TRect): ArrayOfTPointF;
end;

implementation

uses
  Math,
  RPGUtils;

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
  FWalls.Clear;
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

function TWallManager.GetLoSPolygon(centerPnt: TPoint; BoundingBox: TRect): ArrayOfTPointF;
var
  tmpPnts, tmpWalls: TPointList;
  sortList, OpenWalls: TIdxList;
  i, j, LowestIdx, CurClosestWall, PrevWall: Integer;
  CurAngle, LowestAngle, ClosestDist: Double;
  boundingPnts: array[0..3] of Integer;
  rD, IntPnt, AddPnt: TPointF;
  CurWall: TPoint;
  CurSide: Integer;
  TotalPnts: Integer;

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
    Sign0: Integer;
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
          Result := Result and (Sign0 = Sign(dx * (pC.Y - pA.Y) - dy * (pC.X - pA.X)));
        end;
        if Result then
          dir := Sign0;
      end;
    finally
      pnts.Free;
    end;
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

    // Add walls for bounding box
    boundingPnts[0] := AddTmpPnt(BoundingBox.TopLeft);
    boundingPnts[1] := AddTmpPnt(Point(BoundingBox.Top, BoundingBox.Right));
    boundingPnts[2] := AddTmpPnt(BoundingBox.BottomRight);
    boundingPnts[3] := AddTmpPnt(Point(BoundingBox.Bottom, BoundingBox.Left));

    tmpWalls.Add(Point(boundingPnts[0], boundingPnts[1]));
    tmpWalls.Add(Point(boundingPnts[1], boundingPnts[2]));
    tmpWalls.Add(Point(boundingPnts[2], boundingPnts[3]));
    tmpWalls.Add(Point(boundingPnts[3], boundingPnts[0]));

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
        CurAngle := ArcTan2(centerPnt.y - tmpPnts[j].y, centerPnt.X - tmpPnts[j].x);
        if CurAngle < LowestAngle then
        begin
          LowestAngle := CurAngle;
          LowestIdx := j;
        end;
      end;
      sortList.Move(LowestIdx, i);
    end;

    // Now trace list of points...

    // First, check all walls for intersections with ray to first point, add to open list
    rD := TPointF.Create(tmpPnts[sortList[0]] - centerPnt);
    LowestIdx := -1;
    ClosestDist := MAXDOUBLE;
    for i := 0 to tmpWalls.Count - 1 do
    begin
      CurWall := tmpWalls[i];
      if GetIntersection_RaySegment(TPointF.Create(centerPnt), rD, TPointF.Create(tmpPnts[CurWall.X]), TPointF.Create(tmpPnts[CurWall.Y]), IntPnt) then
      begin
        OpenWalls.Add(i);
        if IntPnt.Distance(TPointF.Create(centerPnt)) < ClosestDist then
        begin
          ClosestDist := IntPnt.Distance(TPointF.Create(CenterPnt));
          LowestIdx := i;
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
            OpenWalls.Add(j);
          end;
        end;
      end;

      // Check which of the open wall is the nearest now
      PrevWall := CurClosestWall;
      LowestIdx := -1;
      ClosestDist := MAXDOUBLE;
      for j := 0 to OpenWalls.Count - 1 do
      begin
        CurWall := tmpWalls[OpenWalls[j]];
        if GetIntersection_RaySegment(TPointF.Create(centerPnt), rD, TPointF.Create(tmpPnts[CurWall.X]), TPointF.Create(tmpPnts[CurWall.Y]), IntPnt) then
        begin
          if IntPnt.Distance(TPointF.Create(centerPnt)) < ClosestDist then // TODO: In case of a tie, select wall where the other end is closer
          begin
            ClosestDist := IntPnt.Distance(TPointF.Create(CenterPnt));
            LowestIdx := j;
          end;
        end;
      end;
      CurClosestWall := LowestIdx;

      if PrevWall <> CurClosestWall then
      begin
        // make room for more points, if required
        if Length(Result) <= TotalPnts + 2 then
          SetLength(Result, Length(Result) * 2);

        rD := TPointF.Create(tmpPnts[sortList[i]] - centerPnt);
        rD.Normalize;
        // Check if ray needs to continue past the point
        if IsNonBlockingCorner(sortList[i], rD.x, rD.y, CurSide) then
        begin
          // Check all other open walls for intersections with ray, starting at current point
          LowestIdx := -1;
          ClosestDist := MAXDOUBLE;
          for j := 0 to OpenWalls.Count - 1 do
          begin
            if OpenWalls[j] <> CurClosestWall then
            begin
              CurWall := tmpWalls[OpenWalls[j]];
              if GetIntersection_RaySegment(TPointF.Create(tmpPnts[sortList[i]]), rD, TPointF.Create(tmpPnts[CurWall.X]), TPointF.Create(tmpPnts[CurWall.Y]), IntPnt) then
              begin
                if IntPnt.Distance(TPointF.Create(tmpPnts[sortList[i]])) < ClosestDist then
                begin
                  ClosestDist := IntPnt.Distance(TPointF.Create(tmpPnts[sortList[i]]));
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
            Result[TotalPnts] := TPointF.Create(tmpPnts[sortList[i]]);
            Inc(TotalPnts);
          end
          else
          begin
            Result[TotalPnts] := TPointF.Create(tmpPnts[sortList[i]]);
            Inc(TotalPnts);
            Result[TotalPnts] := AddPnt;
            Inc(TotalPnts);
          end;
        end
        else
        begin
          // Just add current point to list
          Result[TotalPnts] := TPointF.Create(tmpPnts[sortList[i]]);
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

