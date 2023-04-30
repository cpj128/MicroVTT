{Copyright (c) 2023 Stephan Breer

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
    bSendToBack: TButton;
    bOk: TButton;
    bDelete: TButton;
    bCancel: TButton;
    bBringToFront: TButton;
    cbVisible: TCheckBox;
    cbOverlay: TComboBox;
    cdIndicatorColor: TColorDialog;
    eGridSlotsY: TEdit;
    eWidth: TEdit;
    eHeight: TEdit;
    eGridSlotsX: TEdit;
    fseRotation: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    pnColor: TPanel;
    seNumber: TSpinEditEx;
    seSectorAngle: TSpinEditEx;
    seAlpha: TSpinEditEx;
    udGridSlotsY: TUpDown;
    udWidth: TUpDown;
    udHeight: TUpDown;
    udGridSlotsX: TUpDown;
    procedure bAddToInitiativeClick(Sender: TObject);
    procedure bBringToFrontClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure bSendToBackClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnColorClick(Sender: TObject);
  private

  public
    LinkedToken: TToken;
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

{ TfmTokenSettings }

procedure TfmTokenSettings.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfmTokenSettings.bOkClick(Sender: TObject);
begin
  if Assigned(LinkedToken) then
  begin
    LinkedToken.Visible := cbVisible.Checked;
    LinkedToken.Width   := udWidth.Position;
    LinkedToken.Height  := udHeight.Position;
    LinkedToken.Angle   := -DegToRad(fseRotation.Value);
    LinkedToken.GridSlotsX := udGridSlotsX.Position;
    LinkedToken.GridSlotsY := udGridSlotsY.Position;
    LinkedToken.OverlayIdx := cbOverlay.ItemIndex - 1;
    LinkedToken.Number  := seNumber.Value;
    fmController.SnapTokenToGrid(LinkedToken);
    if LinkedToken is TRangeIndicator then
    begin 
      TRangeIndicator(LinkedToken).Color := pnColor.Color;
      TRangeIndicator(LinkedToken).SectorAngle := seSectorAngle.Value;
      TRangeIndicator(LinkedToken).Alpha := seAlpha.Value;
    end;

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
    fmSetInitiative.TokenNo := seNumber.Value;
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

procedure TfmTokenSettings.bDeleteClick(Sender: TObject);
begin
  fmController.RemoveToken(LinkedToken);
  LinkedToken.Free;
  LinkedToken := nil;
  Close;
end;

procedure TfmTokenSettings.FormShow(Sender: TObject);
var
  i, PrevIdx: Integer;
  ContentList: TStringList;
begin
  Caption := GetString(LangStrings.LanguageID, 'TokenSettingsCaption');
  cbVisible.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsVisible');
  Label1.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsWidth');
  Label2.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsHeight');
  Label5.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsRotation');
  Label4.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSlotsY');
  if LinkedToken is TRangeIndicator then
  begin                                                          
    Label3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsAlpha');
    Label6.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsColor');
    Label7.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSectorAngle');
  end
  else
  begin
    Label3.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSlotsX');
    Label6.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsOverlay');
    Label7.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsNumber');
  end;
  bDelete.Caption := GetString(LangStrings.LanguageID, 'ButtonDelete');
  bCancel.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');
  bOk.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');
  bAddToInitiative.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsAddToInit');
  bBringToFront.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsBringToFront');
  bSendToBack.Caption := GetString(LangStrings.LanguageID, 'TokenSettingsSendToBack');

  PrevIdx := cbOverlay.ItemIndex;
  cbOverlay.Items.Clear;
  cbOverlay.Items.Add('-');
  cbOverlay.ItemIndex := 0;
  ContentList := TStringList.Create;
  try
    ContentList.Delimiter := '|';
    ContentList.StrictDelimiter := True;
    for i := 0 to fmController.OverlayLib.Count - 1 do
    begin
      ContentList.DelimitedText := fmController.OverlayLib.ValueFromIndex[i];
      if ContentList.Count >= 1 then
        cbOverlay.Items.Add(ContentList[0]);
    end;
    cbOverlay.ItemIndex := PrevIdx;
  finally
    ContentList.Free;
  end;
end;

procedure TfmTokenSettings.pnColorClick(Sender: TObject);
begin
  if not (LinkedToken is TRangeIndicator) then
    Exit;
  cdIndicatorColor.Color := pnColor.Color;
  if cdIndicatorColor.Execute then
  begin
    pnColor.Color := cdIndicatorColor.Color;
  end;
end;

end.

