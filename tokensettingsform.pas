{Copyright (c) 2023-2026 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit TokenSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Spin, ExtCtrls, Buttons, SpinEx, RPGTypes;

type

  { TfmTokenSettings }

  TfmTokenSettings = class(TForm)
    bAddToInitiative: TButton;
    bBringToFront: TButton;
    bCancel: TButton;
    bDelete: TButton;
    bDetach: TButton;
    bOk: TButton;
    bSendToBack: TButton;
    bShowEmissionSettings: TButton;
    cbRotationDirDistribution: TComboBox;
    cbSizeChangeDistribution: TComboBox;
    cbAlphaChangeDistribution: TComboBox;
    cbSizeDistribution: TComboBox;
    cbAlphaDistribution: TComboBox;
    cbSpeedDistribution: TComboBox;
    cbAnimationType4: TComboBox;
    cbEmitterShape5: TComboBox;
    cbLockPosition1: TCheckBox;
    cbLockPosition2: TCheckBox;
    cbLockPosition3: TCheckBox;
    cbLockPosition4: TCheckBox;
    cbOverlay1: TComboBox;
    cbParticleType5: TComboBox;
    cbShowLoS1: TCheckBox;
    cbRotationDistribution: TComboBox;
    cbVisible1: TCheckBox;
    cbVisible2: TCheckBox;
    cbVisible3: TCheckBox;
    cbVisible4: TCheckBox;
    cbActive5: TCheckBox;
    cdIndicatorColor: TColorDialog;
    cbAngleDistribution: TComboBox;
    cbEmitterPresets: TComboBox;
    eGridSlotsX1: TEdit;
    eGridSlotsY1: TEdit;
    eHeight1: TEdit;
    eHeight2: TEdit;
    eHeight3: TEdit;
    eHeight5: TEdit;
    eWidth1: TEdit;
    eWidth2: TEdit;
    eWidth3: TEdit;
    eRange4: TEdit;
    eWidth5: TEdit;
    fseAngleRange: TFloatSpinEdit;
    fseCenterRotation: TFloatSpinEdit;
    fseCenterSize: TFloatSpinEdit;
    fseCenterAlpha: TFloatSpinEdit;
    fseCenterSizeChange: TFloatSpinEdit;
    fseCenterAlphaChange: TFloatSpinEdit;
    fsePps: TFloatSpinEdit;
    fseCenterRotationDir: TFloatSpinEdit;
    fseRotationDirRange: TFloatSpinEdit;
    fseSizeChangeRange: TFloatSpinEdit;
    fseAlphaChangeRange: TFloatSpinEdit;
    fseSizeRange: TFloatSpinEdit;
    fseAlphaRange: TFloatSpinEdit;
    fseSpeedRange: TFloatSpinEdit;
    fseCenterAngle: TFloatSpinEdit;
    fseCenterSpeed: TFloatSpinEdit;
    fseMaxStrength4: TFloatSpinEdit;
    fseRotation1: TFloatSpinEdit;
    fseRotation2: TFloatSpinEdit;
    fseRotation3: TFloatSpinEdit;
    fseRotationRange: TFloatSpinEdit;
    lCenterRotation: TLabel;
    lCenterSize: TLabel;
    lCenterAlpha: TLabel;
    lCenterSizeChange: TLabel;
    lCenterAlphaChange: TLabel;
    lInitialAlpha: TLabel;
    lAlphaChangeDistribution: TLabel;
    lAlphaChangeRange: TLabel;
    lPresets: TLabel;
    lSizeChangeRate: TLabel;
    lInitialSize: TLabel;
    lPps: TLabel;
    lCenterRotationDir: TLabel;
    lInitialRotation: TLabel;
    lChangeRate: TLabel;
    lRotationDirDistribution: TLabel;
    lSizeChangeDistribution: TLabel;
    lRotationDirRange: TLabel;
    lSizeChangeRange: TLabel;
    lAlphaChangeRate: TLabel;
    lSizeDistribution: TLabel;
    lAlphaDistribution: TLabel;
    lSizeRange: TLabel;
    lAlphaRange: TLabel;
    lSpeedDistribution: TLabel;
    lRotationDistribution: TLabel;
    lSpeedRange: TLabel;
    lCenterAngle: TLabel;
    lAlpha2: TLabel;
    lAngleRange: TLabel;
    lAngleDistribution: TLabel;
    lCenterSpeed: TLabel;
    lHeight2: TLabel;
    lHeight3: TLabel;
    lHeight5: TLabel;
    lMaxStrength4: TLabel;
    lColor4: TLabel;
    lAnimationType4: TLabel;
    lAnimationSpeed4: TLabel;
    lShape5: TLabel;
    lParticleType5: TLabel;
    lRotationRange: TLabel;
    lText3: TLabel;
    lRotation2: TLabel;
    lRotation3: TLabel;
    lSectorAngle2: TLabel;
    lColor2: TLabel;
    lWidth1: TLabel;
    lHeight1: TLabel;
    lGridSlotsX1: TLabel;
    lGridSlotsY1: TLabel;
    lRotation1: TLabel;
    lOverlay1: TLabel;
    lNumber1: TLabel;
    lWidth2: TLabel;
    lWidth3: TLabel;
    lRange4: TLabel;
    lWidth5: TLabel;
    mText3: TMemo;
    pcEmissionSettings: TPageControl;
    pcSettings: TPageControl;
    pnColor2: TPanel;
    pnColor4: TPanel;
    pRight: TPanel;
    seAlpha2: TSpinEditEx;
    seNumber1: TSpinEditEx;
    seSectorAngle2: TSpinEditEx;
    seAnimationSpeed4: TSpinEditEx;
    sbSavePreset: TSpeedButton;
    sbLoadPreset: TSpeedButton;
    tsAlpha: TTabSheet;
    tsSize: TTabSheet;
    tsRotation: TTabSheet;
    tsSpeed: TTabSheet;
    tsAngle: TTabSheet;
    tsParticleEmitter: TTabSheet;
    tsLight: TTabSheet;
    tsText: TTabSheet;
    tsRangeIndicator: TTabSheet;
    tsCharacter: TTabSheet;
    udGridSlotsX1: TUpDown;
    udGridSlotsY1: TUpDown;
    udHeight1: TUpDown;
    udHeight2: TUpDown;
    udHeight3: TUpDown;
    udHeight5: TUpDown;
    udWidth1: TUpDown;
    udWidth2: TUpDown;
    udWidth3: TUpDown;
    udRange4: TUpDown;
    udWidth5: TUpDown;
    procedure bAddToInitiativeClick(Sender: TObject);
    procedure bBringToFrontClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bDetachClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure bSendToBackClick(Sender: TObject);
    procedure bShowEmissionSettingsClick(Sender: TObject);
    procedure cbEmitterPresetsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnColor2Click(Sender: TObject);
    procedure sbLoadPresetClick(Sender: TObject);
    procedure sbSavePresetClick(Sender: TObject);
  private
    FLinkedToken: TToken;
    FCombatMode: Boolean;
    procedure SetToken(token: TToken);
    procedure RefreshEmitterPresets;
  public
    property LinkedToken: TToken read FLinkedToken write SetToken;
    property CombatMode: Boolean write FCombatMode;
  end;

var
  fmTokenSettings: TfmTokenSettings;

implementation

{$R *.lfm}

uses
  Math,
  RPGUtils,
  LangStrings,
  ContentManager,
  ControllerForm,
  DisplayForm,
  InitiativeForm;

const
  LIGHTANIMATIONSTR: array[0..6] of string = ('TokenSettingsLightNone', 'TokenSettingsLightFlicker',
                                              'TokenSettingsLightPulse', 'TokenSettingsLightFlash',
                                              'TokenSettingsLightUnstable', 'TokenSettingsLightRampUp',
                                              'TokenSettingsLightRampDown');

  PARTICLEEMITTERSHAPES: array [0..3] of string = ('TokenSettingsEmitterShapePoint',
                                                   'TokenSettingsEmitterShapeRect',
                                                   'TokenSettingsEmitterShapeEllipse',
                                                   'TokenSettingsEmitterShapeRing');

  DISTRIBUTIONS: array[0..2] of string = ('TokenSettingsEmitterDistributionUniform',
                                          'TokenSettingsEmitterDistributionGaussian',
                                          'TokenSettingsEmitterDistributionArcSine');

{ TfmTokenSettings }

procedure TfmTokenSettings.SetToken(token: TToken);
begin
  FLinkedToken := token;
  if not Assigned(token) then
    Exit;

  if token is TRangeIndicator then
  begin
    pcSettings.ActivePage := tsRangeIndicator;
    cbVisible2.Checked := token.Visible;
    cbLockPosition2.Checked := token.LockPos; 
    udWidth2.Position  := token.Width;
    udHeight2.Position := token.Height;
    fseRotation2.Value := -RadToDeg(token.Angle);
    seSectorAngle2.Value := Round(TRangeIndicator(token).SectorAngle);
    pnColor2.Color := TRangeIndicator(token).Color;
    seAlpha2.Value := TRangeIndicator(token).Alpha;

    bDetach.Show;
    bDetach.Enabled := TRangeIndicator(token).IsAttached;
    bBringToFront.Enabled := not TRangeIndicator(token).IsAttached;
    bSendToBack.Enabled := bBringToFront.Enabled;
    bAddToInitiative.Enabled := False;
  end
  else if token is TTextToken then
  begin                     
    pcSettings.ActivePage := tsText;
    cbVisible3.Checked := token.Visible;
    cbLockPosition3.Checked := token.LockPos;
    udWidth3.Position  := token.Width;
    udHeight3.Position := token.Height;
    fseRotation2.Value := -RadToDeg(token.Angle);
    mText3.Text := TTextToken(token).Text;

    bDetach.Hide;
    bBringToFront.Enabled := True;
    bSendToBack.Enabled := True;
    bAddToInitiative.Enabled := False;
  end
  else if token is TLightToken then
  begin                                 
    pcSettings.ActivePage := tsLight;
    cbVisible4.Checked := token.Visible;
    cbLockPosition4.Checked := token.LockPos;
    udRange4.Position := TLightToken(token).Range;
    fseMaxStrength4.Value := TLightToken(token).MaxStrength;
    cbAnimationType4.ItemIndex := Ord(TLightToken(token).AnimationType);
    seAnimationSpeed4.Value := TLightToken(token).AnimationSpeed; 
    pnColor4.Color := TLightToken(token).Color;
    
    bDetach.Show;
    bDetach.Enabled := TLightToken(token).IsAttached;
    bBringToFront.Enabled := True;
    bSendToBack.Enabled := True;
    bAddToInitiative.Enabled := False;
  end
  else if token is TParticleEmitterToken then
  begin                                                  
    pcSettings.ActivePage := tsParticleEMitter;
    cbActive5.Checked := TParticleEmitterToken(token).Active;
    udWidth5.Position  := token.Width;
    udHeight5.Position := token.Height;
    cbParticleType5.ItemIndex := cbParticleType5.Items.IndexOf(TParticleEmitterToken(token).ParticleName);
    cbEmitterShape5.ItemIndex := Ord(TParticleEmitterToken(token).Shape);
    fsePps.Value := TParticleEmitterToken(token).ParticlesPerSec;

    fseCenterAngle.Value := TParticleEmitterToken(token).CentralAngle;
    fseAngleRange.Value := TParticleEmitterToken(token).AngleRange;
    cbAngleDistribution.ItemIndex := Ord(TParticleEmitterToken(token).AngleDistribution);

    fseCenterSpeed.Value := TParticleEmitterToken(token).CentralSpeed;
    fseSpeedRange.Value := TParticleEmitterToken(token).SpeedRange;
    cbSpeedDistribution.ItemIndex := Ord(TParticleEmitterToken(token).SpeedDistribution);

    fseCenterRotation.Value := TParticleEmitterToken(token).CentralRotation;
    fseRotationRange.Value := TParticleEmitterToken(token).RotationRange;
    cbRotationDistribution.ItemIndex := Ord(TParticleEmitterToken(token).RotationDistribution);

    fseCenterRotationDir.Value := TParticleEmitterToken(token).CentralRotationDir;
    fseRotationDirRange.Value := TParticleEmitterToken(token).RotationDirRange;
    cbRotationDirDistribution.ItemIndex := Ord(TParticleEmitterToken(token).RotationDirDistribution);

    fseCenterSize.Value := TParticleEmitterToken(token).CentralSize;
    fseSizeRange.Value := TParticleEmitterToken(token).SizeRange;
    cbSizeDistribution.ItemIndex := Ord(TParticleEmitterToken(token).SizeDistribution);

    fseCenterSizeChange.Value := TParticleEmitterToken(token).CentralSizeChange;
    fseSizeChangeRange.Value := TParticleEmitterToken(token).SizeChangeRange;
    cbSizeChangeDistribution.ItemIndex := Ord(TParticleEmitterToken(token).SizeChangeDistribution);

    fseCenterAlpha.Value := TParticleEmitterToken(token).CentralAlpha;
    fseAlphaRange.Value := TParticleEmitterToken(token).AlphaRange;
    cbAlphaDistribution.ItemIndex := Ord(TParticleEmitterToken(token).AlphaDistribution);

    fseCenterAlphaChange.Value := TParticleEmitterToken(token).CentralAlphaChange;
    fseAlphaChangeRange.Value := TParticleEmitterToken(token).AlphaChangeRange;
    cbAlphaChangeDistribution.ItemIndex := Ord(TParticleEmitterToken(token).AlphaChangeDistribution);

    bDetach.Show;
    bDetach.Enabled := TParticleEmitterToken(token).IsAttached; 
    bBringToFront.Enabled := False;
    bSendToBack.Enabled := False;  
    bAddToInitiative.Enabled := False;

    tsRotation.TabVisible := not TParticleEmitterToken(Token).IsRByDir;
  end
  else if token is TCharacterToken then
  begin                   
    pcSettings.ActivePage := tsCharacter;
    cbVisible1.Checked := token.Visible;      
    cbShowLoS1.Checked := TCharacterToken(token).ShowLoS;
    cbLockPosition1.Checked := token.LockPos;  
    udWidth1.Position  := token.Width;
    udHeight1.Position := token.Height;  
    fseRotation1.Value := -RadToDeg(token.Angle);
    seNumber1.Value := TCharacterToken(token).Number;
    cbOverlay1.ItemIndex := TCharacterToken(token).OverlayIdx + 1;
    udGridSlotsX1.Position := token.GridSlotsX;
    udGridSlotsY1.Position := token.GridSlotsY;
                                  
    bDetach.Hide;
    bBringToFront.Enabled := True;
    bSendToBack.Enabled := True; 
    bAddToInitiative.Enabled := not FCombatMode;
  end;
end;

procedure TfmTokenSettings.RefreshEmitterPresets;
var i: Integer;
begin
  cbEmitterPresets.Items.BeginUpdate;
  try
    cbEmitterPresets.Items.Clear;
    cbEmitterPresets.Items.Add(GetString(LangStrings.LanguageID, 'TokenSettingsEmitterNewPreset'));
    for i := 0 to ContentLib.EmitterCount - 1 do
      cbEmitterPresets.Items.Add(ContentLib.GetEmitterName(i));
  finally
    cbEmitterPresets.Items.EndUpdate;
  end;
end;

procedure TfmTokenSettings.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfmTokenSettings.bOkClick(Sender: TObject);
begin
  if Assigned(LinkedToken) then
  begin
    if LinkedToken is TCharacterToken then
    begin       
      LinkedToken.Visible := cbVisible1.Checked;
      TCharacterToken(LinkedToken).ShowLoS := cbShowLoS1.Checked;
      LinkedToken.LockPos := cbLockPosition1.Checked;
      LinkedToken.Width   := udWidth1.Position;
      LinkedToken.Height  := udHeight1.Position;
      LinkedToken.Angle   := -DegToRad(fseRotation1.Value); 
      TCharacterToken(LinkedToken).Number := seNumber1.Value;
      TCharacterToken(LinkedToken).OverlayIdx := cbOverlay1.ItemIndex - 1;
      LinkedToken.GridSlotsX := udGridSlotsX1.Position;
      LinkedToken.GridSlotsY := udGridSlotsY1.Position;
      fmController.SnapTokenToGrid(LinkedToken);
    end;
    if LinkedToken is TRangeIndicator then
    begin             
      LinkedToken.Visible := cbVisible2.Checked;
      LinkedToken.LockPos := cbLockPosition2.Checked;
      LinkedToken.Width   := udWidth2.Position;
      LinkedToken.Height  := udHeight2.Position;
      LinkedToken.Angle   := -DegToRad(fseRotation2.Value);
      TRangeIndicator(LinkedToken).SectorAngle := seSectorAngle2.Value;
      TRangeIndicator(LinkedToken).Color := pnColor2.Color;
      TRangeIndicator(LinkedToken).Alpha := seAlpha2.Value;
    end;
    if LinkedToken is TTextToken then
    begin       
      LinkedToken.Visible := cbVisible3.Checked;
      LinkedToken.LockPos := cbLockPosition3.Checked; 
      LinkedToken.Width   := udWidth3.Position;
      LinkedToken.Height  := udHeight3.Position;
      LinkedToken.Angle   := -DegToRad(fseRotation3.Value);
      TTextToken(LinkedToken).Text := mText3.Text;
    end;
    if LinkedToken is TLightToken then
    begin          
      LinkedToken.Visible := cbVisible4.Checked;
      LinkedToken.LockPos := cbLockPosition4.Checked;
      TLightToken(LinkedToken).Range := udRange4.Position;
      TLightToken(LinkedToken).MaxStrength := fseMaxStrength4.Value;
      TLightToken(LinkedToken).AnimationType := TLightAnimationType(cbAnimationType4.ItemIndex);
      TLightToken(LinkedToken).AnimationSpeed := seAnimationSpeed4.Value; 
      TLightToken(LinkedToken).Color := pnColor4.Color;
    end;
    if LinkedToken is TParticleEmitterToken then
    begin
      TParticleEmitterToken(LinkedToken).Active := cbActive5.Checked; 
      LinkedToken.Width   := udWidth5.Position;
      LinkedToken.Height  := udHeight5.Position;
      TParticleEmitterToken(LinkedToken).ParticleName := cbParticleType5.Items[cbParticleType5.ItemIndex];
      TParticleEmitterToken(LinkedToken).Shape := TParticleEmitterShape(cbEmitterShape5.ItemIndex);
      TParticleEmitterToken(LinkedToken).ParticlesPerSec := fsePps.Value;
      
      TParticleEmitterToken(LinkedToken).CentralAngle := fseCenterAngle.Value;
      TParticleEmitterToken(LinkedToken).AngleRange := fseAngleRange.Value;
      TParticleEmitterToken(LinkedToken).AngleDistribution := TRandomDistribution(cbAngleDistribution.ItemIndex);

      TParticleEmitterToken(LinkedToken).CentralSpeed := fseCenterSpeed.Value;
      TParticleEmitterToken(LinkedToken).SpeedRange := fseSpeedRange.Value;
      TParticleEmitterToken(LinkedToken).SpeedDistribution := TRandomDistribution(cbSpeedDistribution.ItemIndex);

      TParticleEmitterToken(LinkedToken).CentralRotation := fseCenterRotation.Value;
      TParticleEmitterToken(LinkedToken).RotationRange := fseRotationRange.Value;
      TParticleEmitterToken(LinkedToken).RotationDistribution := TRandomDistribution(cbRotationDistribution.ItemIndex);

      TParticleEmitterToken(LinkedToken).CentralRotationDir := fseCenterRotationDir.Value;
      TParticleEmitterToken(LinkedToken).RotationDirRange := fseRotationDirRange.Value;
      TParticleEmitterToken(LinkedToken).RotationDirDistribution := TRandomDistribution(cbRotationDirDistribution.ItemIndex);

      TParticleEmitterToken(LinkedToken).CentralSize := fseCenterSize.Value;
      TParticleEmitterToken(LinkedToken).SizeRange := fseSizeRange.Value;
      TParticleEmitterToken(LinkedToken).SizeDistribution := TRandomDistribution(cbSizeDistribution.ItemIndex);

      TParticleEmitterToken(LinkedToken).CentralSizeChange := fseCenterSizeChange.Value;
      TParticleEmitterToken(LinkedToken).SizeChangeRange := fseSizeChangeRange.Value;
      TParticleEmitterToken(LinkedToken).SizeChangeDistribution := TRandomDistribution(cbSizeChangeDistribution.ItemIndex);

      TParticleEmitterToken(LinkedToken).CentralAlpha := fseCenterAlpha.Value;
      TParticleEmitterToken(LinkedToken).AlphaRange := fseAlphaRange.Value;
      TParticleEmitterToken(LinkedToken).AlphaDistribution := TRandomDistribution(cbAlphaDistribution.ItemIndex);

      TParticleEmitterToken(LinkedToken).CentralAlphaChange := fseCenterAlphaChange.Value;
      TParticleEmitterToken(LinkedToken).AlphaChangeRange := fseAlphaChangeRange.Value;
      TParticleEmitterToken(LinkedToken).AlphaChangeDistribution := TRandomDistribution(cbAlphaChangeDistribution.ItemIndex);
    end;
    LinkedToken.UpdateAttached;
    LinkedToken := nil;
    fmController.pbViewPort.Invalidate;
    fmDisplay.Invalidate;
  end;
  Close;
end;

procedure TfmTokenSettings.bCancelClick(Sender: TObject);
begin               
  LinkedToken := nil;
  Close;
end;

procedure TfmTokenSettings.bAddToInitiativeClick(Sender: TObject);
begin
  if Assigned(LinkedToken) then
  begin
    fmSetInitiative.udBaseInitiative.Position := LinkedToken.BaseInitiative;
    fmSetInitiative.udRolledInitiative.Position := 1;
    fmSetInitiative.TokenName := LinkedToken.Name;
    fmSetInitiative.TokenPath := LinkedToken.Path;
    fmSetInitiative.TokenNo := seNumber1.Value;
    bOkClick(self);
    fmSetInitiative.Show;
  end;
end;

procedure TfmTokenSettings.bBringToFrontClick(Sender: TObject);
begin
  if Assigned(LinkedToken) then
  begin
    fmController.TokenToFront(LinkedToken);
    fmController.pbViewport.Invalidate;
    fmDisplay.Invalidate;
  end;
end;

procedure TfmTokenSettings.bSendToBackClick(Sender: TObject);
begin
  if Assigned(LinkedToken) then
  begin
    fmController.TokenToBack(LinkedToken);
    fmController.pbViewport.Invalidate;
    fmDisplay.Invalidate;
  end;
end;

procedure TfmTokenSettings.bShowEmissionSettingsClick(Sender: TObject);
begin
  if pcEmissionSettings.Visible then
  begin
    Height := 267;
    pcEmissionSettings.Hide;
  end
  else
  begin
    Height := 555;
    pcEmissionSettings.Show;
  end;
end;

procedure TfmTokenSettings.cbEmitterPresetsChange(Sender: TObject);
begin
  sbLoadPreset.Enabled := cbEmitterPresets.ItemIndex <> 0;
end;

procedure TfmTokenSettings.FormCreate(Sender: TObject);
var i: Integer;
begin
  // Light animations
  for i := Low(LIGHTANIMATIONSTR) to High(LIGHTANIMATIONSTR) do
    cbAnimationType4.Items.Add(GetString(LangStrings.LanguageID, LIGHTANIMATIONSTR[i]));
  // Particle Types
  for i := 0 to fmController.ParticleManager.GetParticleCount - 1 do
    cbParticleType5.Items.Add(fmController.ParticleManager.ParticleNameByIdx(i));
  // Emitter shapes
  for i := Low(PARTICLEEMITTERSHAPES) to High(PARTICLEEMITTERSHAPES) do
    cbEmitterShape5.Items.Add(GetString(LangStrings.LanguageID, PARTICLEEMITTERSHAPES[i]));
  // Distributions
  cbRotationDirDistribution.Items.Clear;
  for i := Low(DISTRIBUTIONS) to High(DISTRIBUTIONS) do
    cbRotationDirDistribution.Items.Add(GetString(LangStrings.LanguageID, DISTRIBUTIONS[i]));
  
  cbAngleDistribution.Items.Clear;
  cbAngleDistribution.Items.AddStrings(cbRotationDirDistribution.Items);  
  cbSpeedDistribution.Items.Clear;
  cbSpeedDistribution.Items.AddStrings(cbRotationDirDistribution.Items);   
  cbRotationDistribution.Items.Clear;
  cbRotationDistribution.Items.AddStrings(cbRotationDirDistribution.Items);  
  cbSizeDistribution.Items.Clear;
  cbSizeDistribution.Items.AddStrings(cbRotationDirDistribution.Items);
  cbSizeChangeDistribution.Items.Clear;
  cbSizeChangeDistribution.Items.AddStrings(cbRotationDirDistribution.Items);
  cbAlphaDistribution.Items.Clear;
  cbAlphaDistribution.Items.AddStrings(cbRotationDirDistribution.Items); 
  cbAlphaChangeDistribution.Items.Clear;
  cbAlphaChangeDistribution.Items.AddStrings(cbRotationDirDistribution.Items);

  RefreshEmitterPresets;
  cbEmitterPresets.ItemIndex := 0;
end;

procedure TfmTokenSettings.bDeleteClick(Sender: TObject);
begin
  fmController.RemoveToken(LinkedToken);
  LinkedToken.Free;
  LinkedToken := nil;
  Close;
end;

procedure TfmTokenSettings.bDetachClick(Sender: TObject);
begin
  if Assigned(LinkedToken) and (LinkedToken is TAttachableToken) then
    TAttachableToken(LinkedToken).Detach;
  bDetach.Enabled := False;
end;

procedure TfmTokenSettings.FormShow(Sender: TObject);
var
  i, PrevIdx: Integer;
  ContentList: TStringList;
begin
  Caption := GetString(LangStrings.LanguageID, 'TokenSettingsCaption');
  Height := 267;
  pcEmissionSettings.Hide;
  // Character token
  cbVisible1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsVisible');
  cbLockPosition1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsLockPosition');
  cbShowLoS1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsShowLoS');
  lWidth1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsWidth');
  lHeight1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsHeight');
  lRotation1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsRotation'); 
  lNumber1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsNumber');      
  lOverlay1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsOverlay');
  lGridSlotsX1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSlotsX');  
  lGridSlotsY1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSlotsY');

  // range indicator  
  cbVisible2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsVisible');
  cbLockPosition2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsLockPosition');
  lWidth2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsWidth');
  lHeight2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsHeight');
  lRotation2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsRotation');  
  lSectorAngle2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSectorAngle');
  lColor2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsColor');
  lAlpha2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsAlpha');

  // text token  
  cbVisible3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsVisible');
  cbLockPosition3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsLockPosition');
  lWidth3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsWidth');
  lHeight3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsHeight'); 
  lRotation3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsRotation'); 
  lText3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsText');

  // light token     
  cbVisible4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsVisible');
  cbLockPosition4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsLockPosition'); 
  lRange4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsRange');
  lMaxStrength4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsMaxStrength');
  lAnimationType4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsLightAnimation');
  lAnimationSpeed4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsAnimationSpeed');  
  lColor4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsColor');

  // particle emitter                                                          
  cbActive5.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterActive');
  lWidth5.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsWidth');
  lHeight5.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsHeight');
  lParticleType5.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterParticleType');
  lShape5.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterShape');
  lPps.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterPps');

  tsAngle.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAngleTab');
  lCenterAngle.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAngleCenter');
  lAngleRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAngleRange');
  lAngleDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAngleDistribution');

  tsSpeed.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSpeedTab');
  lCenterSpeed.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSpeedCenter');
  lSpeedRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSpeedRange');
  lSpeedDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSpeedDistribution');

  tsRotation.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationTab');
  lInitialRotation.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterInitialRotation');
  lCenterRotation.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationCenter');
  lRotationRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationRange');
  lRotationDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationDistribution');    
  lChangeRate.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationChangeRate');
  lCenterRotationDir.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationDirCenter');
  lRotationDirRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationDirRange');
  lRotationDirDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterRotationDirDistribution');  

  tsSize.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeTab');
  lInitialSize.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterInitialSize');
  lCenterSize.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeCenter');
  lSizeRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeRange');
  lSizeDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeDistribution');
  lSizeChangeRate.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeChangeRate');
  lCenterSizeChange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeDirCenter');
  lSizeChangeRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeDirRange');
  lSizeChangeDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterSizeDirDistribution');           

  tsAlpha.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaTab');
  lInitialAlpha.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterInitialAlpha');
  lCenterAlpha.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaCenter');
  lAlphaRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaRange');
  lAlphaDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaDistribution');
  lAlphaChangeRate.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaChangeRate');
  lCenterAlphaChange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaChangeCenter');
  lAlphaChangeRange.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaChangeRange');
  lAlphaChangeDistribution.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsEmitterAlphaChangeDistribution');

  cbEmitterPresets.ItemIndex := 0;

  // Buttons
  bDelete.Caption := GetString(LangStrings.LanguageID, 'ButtonDelete');
  bCancel.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');
  bOk.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');
  bAddToInitiative.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsAddToInit');
  bBringToFront.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsBringToFront');
  bSendToBack.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSendToBack');
  bDetach.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsDetach');

  // TODO: Move to FormCreate?
  PrevIdx := cbOverlay1.ItemIndex;
  cbOverlay1.Items.Clear;
  cbOverlay1.Items.Add('-');
  cbOverlay1.ItemIndex := 0;

  ContentList := TStringList.Create;
  try
    ContentList.Delimiter := '|';
    ContentList.StrictDelimiter := True;
    for i := 0 to ContentLib.OverlayCount - 1 do
    begin
      ContentList.DelimitedText := ContentLib.GetOverlayData(ContentLib.GetOverlayName(i));
      if ContentList.Count >= 1 then
        cbOverlay1.Items.Add(ContentList[0]);
    end;
    cbOverlay1.ItemIndex := PrevIdx;
  finally
    ContentList.Free;
  end;


end;

procedure TfmTokenSettings.pnColor2Click(Sender: TObject);
begin
  cdIndicatorColor.Color := TPanel(Sender).Color;
  if cdIndicatorColor.Execute then
  begin
    TPanel(Sender).Color := cdIndicatorColor.Color;
  end;
end;

procedure TfmTokenSettings.sbLoadPresetClick(Sender: TObject);
var
  list: TStringList;
  fs: TFormatSettings;
begin
  if cbEmitterPresets.ItemIndex = 0 then
    Exit;
  fs := FormatSettings;
  fs.DecimalSeparator := '.';
  list := TStringList.Create;
  list.Delimiter := '|';
  list.StrictDelimiter := True;
  list.DelimitedText := ContentLib.GetEmitterData(cbEmitterPresets.Items[cbEmitterPresets.ItemIndex]);

  if List.Count >= 24 then
  begin
    fseCenterAngle.Value := StrToFloat(list[0], fs);
    fseAngleRange.Value := StrToFloat(list[1], fs);
    cbAngleDistribution.ItemIndex := StrToInt(list[2]);

    fseCenterSpeed.Value := StrToFloat(list[3], fs);
    fseSpeedRange.Value := StrToFloat(list[4], fs);
    cbSpeedDistribution.ItemIndex := StrToInt(list[5]);
    
    fseCenterRotation.Value := StrToFloat(list[6], fs);
    fseRotationRange.Value := StrToFloat(list[7], fs);
    cbRotationDistribution.ItemIndex := StrToInt(list[8]);

    fseCenterRotationDir.Value := StrToFloat(list[9], fs);
    fseRotationDirRange.Value := StrToFloat(list[10], fs);
    cbRotationDirDistribution.ItemIndex := StrToInt(list[11]);

    fseCenterSize.Value := StrToFloat(list[12], fs);
    fseSizeRange.Value := StrToFloat(list[13], fs);
    cbSizeDistribution.ItemIndex := StrToInt(list[14]);

    fseCenterSizeChange.Value := StrToFloat(list[15], fs);
    fseSizeChangeRange.Value := StrToFloat(list[16], fs);
    cbSizeChangeDistribution.ItemIndex := StrToInt(list[17]);

    fseCenterAlpha.Value := StrToFloat(list[18], fs);
    fseAlphaRange.Value := StrToFloat(list[19], fs);
    cbAlphaDistribution.ItemIndex := StrToInt(list[20]);

    fseCenterAlphaChange.Value := StrToFloat(list[21], fs);
    fseAlphaChangeRange.Value := StrToFloat(list[22], fs);
    cbAlphaChangeDistribution.ItemIndex := StrToInt(list[23]);
  end;

end;

procedure TfmTokenSettings.sbSavePresetClick(Sender: TObject);
var
  list: TStringList; 
  fs: TFormatSettings;
  PresetName: string;
begin
  list := TStringList.Create;
  list.Delimiter := '|';
  list.StrictDelimiter := True;
  fs := FormatSettings;
  fs.DecimalSeparator := '.';

  try
    list.Add(FloatToStrF(fseCenterAngle.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseAngleRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbAngleDistribution.ItemIndex));

    list.Add(FloatToStrF(fseCenterSpeed.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseSpeedRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbSpeedDistribution.ItemIndex));

    list.Add(FloatToStrF(fseCenterRotation.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseRotationRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbRotationDistribution.ItemIndex));

    list.Add(FloatToStrF(fseCenterRotationDir.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseRotationDirRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbRotationDirDistribution.ItemIndex));

    list.Add(FloatToStrF(fseCenterSize.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseSizeRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbSizeDistribution.ItemIndex));

    list.Add(FloatToStrF(fseCenterSizeChange.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseSizeChangeRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbSizeChangeDistribution.ItemIndex));

    list.Add(FloatToStrF(fseCenterAlpha.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseAlphaRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbAlphaDistribution.ItemIndex));

    list.Add(FloatToStrF(fseCenterAlphaChange.Value, ffNumber, 4, 4, fs));
    list.Add(FloatToStrF(fseAlphaChangeRange.Value, ffNumber, 4, 4, fs));
    list.Add(IntToStr(cbAlphaChangeDistribution.ItemIndex));

    PresetName := '';
    if cbEmitterPresets.ItemIndex <= 0 then
    begin
      if not InputQuery(GetString(LangStrings.LanguageID, 'TokenSettingsEmitterNewPresetCaption'), GetString(LangStrings.LanguageID, 'TokenSettingsEmitterNewPresetPrompt'), PresetName) then
        PresetName := '';
      if ContentLib.HasEmitter(PresetName) then
      begin
        if MessageDlg(GetString(LangStrings.LanguageID, 'TokenSettingsEmitterPresetOverwriteCaption'), Format(GetString(LangStrings.LanguageID, 'TokenSettingsEmitterPresetExistsPrompt'), [PresetName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          PresetName := '';
      end;
    end
    else
    begin
      PresetName := cbEmitterPresets.Items[cbEmitterPresets.ItemIndex];
      if MessageDlg(GetString(LangStrings.LanguageID, 'TokenSettingsEmitterPresetOverwriteCaption'), Format(GetString(LangStrings.LanguageID, 'TokenSettingsEmitterPresetOverwritePrompt'), [PresetName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        PresetName := '';
    end;

    if PresetName <> '' then
      ContentLib.SetEmitterData(PresetName, list.DelimitedText);

  finally
    list.Free;
  end;

  RefreshEmitterPresets;
  ContentLib.SaveEmitterData;
end;

end.

