{Copyright (c) 2023-2025 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit particles;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math,
  BGRABitmap,
  BGRATransform;
type

  // Data of one individual particle
  TParticleData = record    
    // Coordinates of the particles
    PosX, PosY: Single;         
    // Rotations of the particles
    Rot: Single;
    // Set Rotation to movement direction
    RByDir: Boolean;
    // Size (in %)
    Size: Single;
    // Transparency
    Alpha: Single;
    // Delta-X/Y/Rotation/Size/Alpha of the particles
    DX, DY, DRot, DSize: Single;
    DAlpha: LongInt;
    // Time to life per particle, in frames
    TTL: Integer;
    // Going to need emitter settings for every single one of these...
  public
    procedure DoTick(dTime: Double);
  end;

  TParticle = class
  // represents a collection of particles of the same type
  private
    // Max number of particles managed by this object. Set at creation, cannot change.
    FMaxCount: DWORD;
    // Highest index of currently active particles
    FMaxIdx: DWORD;
    // Graphic of the particle
    FGraphic: TBGRABitmap;

    FData: array of TParticleData;

    // Future extensions:
    // Transparency + delta
    // Color modulation H/S/L + delta
    // Animation...

    // Particle value initialization ranges
    // These belong into the spawner
    {FPosXMin, FPosXMax, FPosYMin, FPosYMax: Double;
    FRotMin, FRotMax: Double;
    FDPosXMin, FDPosXMax, FDPosYMin, FDPosYMax: Double;
    FDRotMin, FDRotMax: Double;}

    FTTLMin, FTTLMax: Integer;

    // Set particle rotation by movement direction
    FSetRByDir: Boolean;

    // Draw particle independently of screen movement
    FScreenStatic: Boolean;

  protected
    function GetWidth: Integer;
    function GetHeight: Integer;

  public
    constructor Create(DefFileName: string);
    destructor Destroy; override;
    procedure Draw(target: TBGRABitmap; ZoomFactor, OffsetX, OffsetY: Double);
    procedure DoTick(DeltaT: Double);
    procedure AddParticle(X, Y, R, S, DX, DY, DR, DS: Single; TTL: Integer);
    property RByDir: Boolean read FSetRByDir;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

TParticleManager = class
  // Manages a collection of all TParticle-objects
  private
    //FParticleList: TList;
    FParticleList: TStringList;

    procedure SetParticleDir(dir: string);

    function GetParticleByName(name: string): TParticle;
    function GetParticle(idx: Integer): TParticle;
  public
    constructor Create(ParticleDir: string);
    destructor Destroy; override;
    procedure DoTick(DeltaT: Double);
    procedure Draw(target: TBGRABitmap; ZoomFactor, OffsetX, OffsetY: Double);

    function HasParticle(name: string): Boolean;
    function GetParticleCount: Integer;
    function ParticleList: TStringList;
    function ParticleNameByIdx(idx: Integer): string;

    property ParticleByName[s: string]: TParticle read GetParticleByName;
    property Particle[i: Integer]: TParticle read GetParticle;
    property ParticleDir: string write SetParticleDir;
end;

implementation

uses
  IniFiles,
  FileUtil;

  { TParticleData }

procedure TParticleData.DoTick(dTime: Double);
begin
  PosX := PosX + DX * dTime;
  PosY := PosY + DY * dTime;
  if RByDir then
    Rot := RadToDeg(ArcTan2(DY, DX))
  else
    Rot  := Rot + DRot * dTime;
  Size := Size + DSize * dTime;
  Alpha := EnsureRange(Alpha + DAlpha * dTime, 0, 255);
  if (Alpha <= 0) or (Size <= 0) then
    TTL := 0
  else
    TTL  := TTL  - 1;
end;

  { TParticle }

constructor TParticle.Create(DefFileName: string);
var
  particleFile: TIniFile;
  graphicFileName: string;
  RelPath: string;
begin
  FGraphic := nil;
  if not FileExists(DefFileName) then
    raise Exception.Create('Particle definition file "' + DefFileName + '" not found.');
  particleFile := TIniFile.Create(DefFileName);
  try
    RelPath := ExtractFilePath(DefFileName);
    graphicFileName := particleFile.ReadString('Particle', 'GraphicFile', '');
    if graphicFileName = '' then
      raise Exception.Create('Particle definition "' + DefFileName + '" does not contain graphic file name.');
    FGraphic := TBGRABitmap.Create(RelPath + graphicFileName);
    FMaxCount := particleFile.ReadInteger('Particle', 'MaxCount', 1000);
    FMaxIdx := 0;

    FSetRByDir := particleFile.ReadBool('Particle', 'RByDir', False);
    FScreenStatic := particleFile.ReadBool('Particle', 'ScreenStatic', False);

    FTTLMin := particleFile.ReadInteger('Particle', 'TTLMin', 100);
    FTTLMax := particleFile.ReadInteger('Particle', 'TTLMax', 100);

    SetLength(FData, FMaxCount);
  finally
    particleFile.Free;
  end;

end;

destructor TParticle.Destroy;
begin
  if Assigned(FGraphic) then
    FGraphic.Free;
end;

function TParticle.GetWidth: Integer;
begin
  Result := 0;
  if Assigned(FGraphic) then
    Result := FGraphic.Width;
end;

function TParticle.GetHeight: Integer;
begin
  Result := 0;
  if Assigned(FGraphic) then
    Result := FGraphic.Height;
end;

procedure TParticle.Draw(target: TBGRABitmap; ZoomFactor, OffsetX, OffsetY: Double);
var
  i: Integer;
  fixMat, tmpMat: TAffineMatrix;
begin
  if FScreenStatic then
    fixMat := AffineMatrixScale(ZoomFactor, ZoomFactor)
  else
    fixMat := AffineMatrixTranslation(-OffsetX, -OffsetY) * AffineMatrixScale(ZoomFactor, ZoomFactor);
  for i := 0 to FMaxIdx do
  begin
    tmpMat := fixMat * AffineMatrixTranslation(FData[i].PosX, FData[i].PosY);
    tmpMat := tmpMat * AffineMatrixRotationDeg(FData[i].Rot); 
    tmpMat := tmpMat * AffineMatrixScale(FData[i].Size / 100, FData[i].Size / 100);
    tmpMat := tmpMat * AffineMatrixTranslation(-Width / 2, -Height / 2);
    target.PutImageAffine(tmpMat, FGraphic, Round(FData[i].Alpha));
  end;
end;

procedure TParticle.DoTick(DeltaT: Double);
var i, IdxOffset: Integer;
begin
  IdxOffset := 0;
  i := 0;

  while {((i + IdxOffset) < FMaxIdx) and} (i <= FMaxIdx) do
  begin
    if FData[i + IdxOffset].TTL <= 0 then
    begin
      // This particle is dead, replace with next living one
      while (i + IdxOffset < FMaxIdx) and (FData[i + IdxOffset].TTL <= 0) do
        Inc(IdxOffset);
    end;

    if (i + idxOffset) >= FMaxIdx then
      Break;

    if (IdxOffset > 0) and (i + IdxOffset < FMaxCount) then
    begin
      // Move next living particle forward, overwriting dead ones
      FData[i]  := FData[i+IdxOffset];
    end;

    // Set data for the next step
    FData[i].DoTick(DeltaT);
    Inc(i);
  end;

  // Update number of active particles
  FMaxIdx := FMaxIdx - IdxOffset;
end;

procedure TParticle.AddParticle(X, Y, R, S, DX, DY, DR, DS: Single; TTL: Integer);
var
  CanCreate: Boolean;
  tmpData: TParticleData;
begin
  // Test if we can create this particle at all
  // Not possible if we are at capacity, chance to fail when at more than 50%
  CanCreate := FMaxIdx < FMaxCount - 1;
  CanCreate := CanCreate and (not (FMaxIdx > FMaxCount / 2) or (Random < 0.5));

  if CanCreate then
  begin
    Inc(FMaxIdx);
    tmpData.PosX   := X;
    tmpData.PosY   := Y;
    tmpData.Rot    := R;
    tmpData.Size   := S;
    tmpData.Alpha  := 255;
    tmpData.DX     := DX;
    tmpData.DY     := DY;
    tmpData.DRot   := DR;
    tmpData.DSize  := DS;
    tmpData.DAlpha := -2;
    tmpData.TTL    := FTTLMin + Random(FTTLMax - FTTLMin);
    tmpData.RByDir := FSetRByDir;
    FData[FMaxIdx] := tmpData;
  end;
end;

{ TParticleManager }

constructor TParticleManager.Create(ParticleDir: string);
begin
  FParticleList := TStringList.Create;
  FParticleList.OwnsObjects := True;
  SetParticleDir(ParticleDir);
end;

destructor TParticleManager.Destroy;
begin
  FParticleList.Free;
  inherited;
end;

procedure TParticleManager.SetParticleDir(dir: string);
var
  FileList: TStringList;
  i: Integer;
  tmpParticleFile: TIniFile;
  ParticleName, path: string;
begin
  // Scan directory for particle definitions...
  FileList := TStringList.Create;
  FindAllFiles(FileList, dir, '*.txt', True);
  try
    for i := 0 to FileList.Count - 1 do
    begin
      // Check if we have a particle definition file
      tmpParticleFile := TIniFile.Create(FileList[i]);
      try
        if tmpParticleFile.SectionExists('Particle') then
        begin
          ParticleName := ExtractFileNameWithoutExt(FileList[i]);
          path := ExtractFilePath(FileList[i]);
          ParticleName := Copy(ParticleName, Length(path) + 1, Length(ParticleName));
          FParticleList.AddObject(ParticleName, TParticle.Create(FileList[i]));
        end;
      finally
        tmpParticleFile.Free;
      end;
    end;

  finally
    FileList.Free;
  end;
end;

procedure TParticleManager.DoTick(DeltaT: Double);
var i: Integer;
begin
  for i := 0 to FParticleList.Count - 1 do
    TParticle(FParticleList.Objects[i]).DoTick(DeltaT);
end;

procedure TParticleManager.Draw(target: TBGRABitmap; ZoomFactor, OffsetX, OffsetY: Double);
var i: Integer;
begin
  for i := 0 to FParticleList.Count - 1 do
    TParticle(FParticleList.Objects[i]).Draw(target, ZoomFactor, OffsetX, OffsetY);
end;

function TParticleManager.GetParticleByName(name: string): TParticle;
var idx: Integer;
begin
  Result := nil;
  idx := FParticleList.IndexOf(name);
  if idx >= 0 then
    Result := TParticle(FParticleList.Objects[idx]);
end;

function TParticleManager.GetParticle(idx: Integer): TParticle;
begin
  Result := TParticle(FParticleList.Objects[idx]);
end;

function TParticleManager.HasParticle(name: string): Boolean;
begin
  Result := FParticleList.IndexOf(name) >= 0;
end;

function TParticleManager.GetParticleCount: Integer;
begin
  Result := FParticleList.Count;
end;

function TParticleManager.ParticleList: TStringList;
var i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FParticleList.Count - 1 do
    Result.Add(FParticleList[i]);
end;

function TParticleManager.ParticleNameByIdx(idx: Integer): string;
begin
  Result := '';
  if (idx >= 0) and (idx < FParticleList.Count) then
    Result := FParticleList[idx];
end;

end.

