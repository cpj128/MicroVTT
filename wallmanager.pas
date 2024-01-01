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

  function GetCloserWallToPnt(CurPnt, Wall1, Wall2: Integer): Integer;
  var
    P1Idx, P2Idx: Integer;
    tmpWall: TPoint;
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
    if TPointF.Create(CenterPnt).Distance(TPointF.Create(tmpPnts[P1Idx])) < TPointF.Create(CenterPnt).Distance(TPointF.Create(tmpPnts[P2Idx])) then
      Result := Wall1
    else
      Result := Wall2;
  end;

  function DoesWallBegin(pWall: TPoint; pPnt: Integer): Boolean;
  var otherPnt: Integer;
  begin
    if pWall.X = pPnt then
      otherPnt := pWall.Y
    else if pWall.Y = pPnt then
      otherPnt := pWall.X
    else // Wall doea not contain the point? Obviously it cannot start there
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
            // When in doubt, change walls away from the bounding box - this is the only way we should have intersecting walls
            // TODO: In case of a tie, select wall where the other end is closer


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
          {GetIntersection_RaySegment(TPointF.Create(centerPnt), rD,
                                     TPointF.Create(tmpPnts[tmpWalls[CurClosestWall].X]),
                                     TPointF.Create(tmpPnts[tmpWalls[CurClosestWall].Y]),
                                     NewPnt);}
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

