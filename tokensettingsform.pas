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
  Spin, ExtCtrls, SpinEx, RPGTypes;

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
    cbAnimationType4: TComboBox;
    cbEmitterShape5: TComboBox;
    cbLockPosition1: TCheckBox;
    cbLockPosition2: TCheckBox;
    cbLockPosition3: TCheckBox;
    cbLockPosition4: TCheckBox;
    cbOverlay1: TComboBox;
    cbParticleType5: TComboBox;
    cbShowLoS1: TCheckBox;
    cbVisible1: TCheckBox;
    cbVisible2: TCheckBox;
    cbVisible3: TCheckBox;
    cbVisible4: TCheckBox;
    cbActive5: TCheckBox;
    cdIndicatorColor: TColorDialog;
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
    fseMaxStrength4: TFloatSpinEdit;
    fseRotation1: TFloatSpinEdit;
    fseRotation2: TFloatSpinEdit;
    fseRotation3: TFloatSpinEdit;
    lAlpha2: TLabel;
    lHeight2: TLabel;
    lHeight3: TLabel;
    lHeight5: TLabel;
    lMaxStrength4: TLabel;
    lColor4: TLabel;
    lAnimationType4: TLabel;
    lAnimationSpeed4: TLabel;
    lShape5: TLabel;
    lParticleType5: TLabel;
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
    pcSettings: TPageControl;
    pnColor2: TPanel;
    pnColor4: TPanel;
    pRight: TPanel;
    seAlpha2: TSpinEditEx;
    seNumber1: TSpinEditEx;
    seSectorAngle2: TSpinEditEx;
    seAnimationSpeed4: TSpinEditEx;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnColor2Click(Sender: TObject);
  private
    FLinkedToken: TToken;
    procedure SetToken(token: TToken);
    procedure SetCombatMode(val: Boolean);
  public
    property LinkedToken: TToken read FLinkedToken write SetToken;
    property CombatMode: Boolean write SetCombatMode;
  end;

var
  fmTokenSettings: TfmTokenSettings;

implementation

{$R *.lfm}

uses
  Math,
  LangStrings,
  ControllerForm,
  DisplayForm,
  InitiativeForm;

const
  LIGHTANIMATIONSTR: array[0..6] of string = ('TokenSettingsLightNone', 'TokenSettingsLightFlicker',
                                              'TokenSettingsLightPulse', 'TokenSettingsLightFlash',
                                              'TokenSettingsLightUnstable', 'TokenSettingsLightRampUp',
                                              'TokenSettingsLightRampDown');

  PARTICLEEMITTERSHAPES: array [0..2] of string = ('TokenSettingsEmitterShapePoint',
                                                   'TokenSettingsEmitterShapeRect',
                                                   'TokenSettingsEmitterShapeEllipse');

{ TfmTokenSettings }

procedure TfmTokenSettings.SetCombatMode(val: Boolean);
begin
  bAddToInitiative.Enabled := not val;
end;

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
    bAddToInitiative.Enabled := False;
  end
  else if token is TParticleEmitterToken then
  begin                                                  
    pcSettings.ActivePage := tsParticleEMitter;
    cbActive5.checked := TParticleEmitterToken(token).Active; 
    udWidth5.Position  := token.Width;
    udHeight5.Position := token.Height;
    cbParticleType5.ItemIndex := cbParticleType5.Items.IndexOf(TParticleEmitterToken(token).ParticleName);
    cbEmitterShape5.ItemIndex := Ord(TParticleEmitterToken(token).Shape);

    bAddToInitiative.Enabled := False;
    bDetach.Show;
    bDetach.Enabled := TParticleEmitterToken(token).IsAttached;
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

    bBringToFront.Enabled := True;
    bSendToBack.Enabled := True;
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
  AniType: TLightAnimationType;
  ContentList: TStringList;
  tmp: string;
begin
  Caption := GetString(LangStrings.LanguageID, 'TokenSettingsCaption');
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

  // buttons
  bDelete.Caption := GetString(LangStrings.LanguageID, 'ButtonDelete');
  bCancel.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');
  bOk.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');
  bAddToInitiative.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsAddToInit');
  bBringToFront.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsBringToFront');
  bSendToBack.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSendToBack');
  bDetach.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsDetach');

  PrevIdx := cbOverlay1.ItemIndex;
  cbOverlay1.Items.Clear;
  cbOverlay1.Items.Add('-');
  cbOverlay1.ItemIndex := 0;

  ContentList := TStringList.Create;
  try
    ContentList.Delimiter := '|';
    ContentList.StrictDelimiter := True;
    for i := 0 to fmController.OverlayLib.Count - 1 do
    begin
      ContentList.DelimitedText := fmController.OverlayLib.ValueFromIndex[i];
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
  //if not ((LinkedToken is TRangeIndicator) or (LinkedToken is TLightToken)) then
  //  Exit;
  cdIndicatorColor.Color := TPanel(Sender).Color;
  if cdIndicatorColor.Execute then
  begin
    TPanel(Sender).Color := cdIndicatorColor.Color;
  end;
end;

end.

