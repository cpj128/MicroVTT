{Copyright (c) 2023-2025 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, ExtCtrls;

type

  { TfmSettings }

  TfmSettings = class(TForm)
    bOk: TButton;
    bCancel: TButton;
    cbInitiativeOrder: TComboBox;
    cbLanguage: TComboBox;
    cbTokenRotation: TComboBox;
    cbTokensStartInvisible: TCheckBox;
    cdClrSelect: TColorDialog;
    eMapDirectory: TEdit;
    eOverlayDirectory: TEdit;
    eParticleDirectory: TEdit;
    eTokenDirectory: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pnHiddenMark: TPanel;
    pnMarker: TPanel;
    pnMeasure: TPanel;
    pnTokenShadow: TPanel;
    pcSettings: TPageControl;
    pnWalls: TPanel;
    pnPortals: TPanel;
    sbSelectMapDirectory: TSpeedButton;
    sbSelectOverlayDirectory: TSpeedButton;
    sbSelectParticleDirectory: TSpeedButton;
    sbSelectTokenDirectory: TSpeedButton;
    tsInterface: TTabSheet;
    tsRules: TTabSheet;
    tsAssets: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure pnTokenShadowClick(Sender: TObject);
    procedure sbSelectMapDirectoryClick(Sender: TObject);
    procedure sbSelectOverlayDirectoryClick(Sender: TObject);
    procedure sbSelectParticleDirectoryClick(Sender: TObject);
    procedure sbSelectTokenDirectoryClick(Sender: TObject);
  private

  public

  end;

var
  fmSettings: TfmSettings;

implementation

{$R *.lfm}

uses
  ControllerForm,
  LangStrings;

{ TfmSettings }

procedure TfmSettings.sbSelectMapDirectoryClick(Sender: TObject);
var OutDir: string;
begin
  if SelectDirectory(GetString(LangStrings.LanguageID, 'SelectMapDirCaption'), eMapDirectory.Text, OutDir) then
    eMapDirectory.Text := OutDir;
end;

procedure TfmSettings.sbSelectOverlayDirectoryClick(Sender: TObject);
var OutDir: string;
begin
  if SelectDirectory(GetString(LangStrings.LanguageID, 'SelectOverlayDirCaption'), eOverlayDirectory.Text, OutDir) then
    eOverlayDirectory.Text := OutDir;
end;

procedure TfmSettings.sbSelectParticleDirectoryClick(Sender: TObject);
var OutDir: string;
begin
  if SelectDirectory(GetString(LangStrings.LanguageID, 'SelectParticleDirCaption'), eParticleDirectory.Text, OutDir) then
    eParticleDirectory.Text := OutDir;
end;
    
procedure TfmSettings.sbSelectTokenDirectoryClick(Sender: TObject);
var OutDir: string;
begin
  if SelectDirectory(GetString(LangStrings.LanguageID, 'SelectTokenDirCaption'), eTokenDirectory.Text, OutDir) then
    eTokenDirectory.Text := OutDir;
end;

procedure TfmSettings.FormShow(Sender: TObject);
begin
  Caption := GetString(LangStrings.LanguageID, 'SettingsCaption');
  tsAssets.Caption := GetString(LangStrings.LanguageID, 'SettingsHeaderDirectories');
  tsRules.Caption := GetString(LangStrings.LanguageID, 'SettingsHeaderRules');
  tsInterface.Caption := GetString(LangStrings.LanguageID, 'SettingsHeaderInterface');

  Label1.Caption := GetString(LangStrings.LanguageID, 'SettingsMapDir');
  Label2.Caption := GetString(LangStrings.LanguageID, 'SettingsTokenDir');  
  Label6.Caption := GetString(LangStrings.LanguageID, 'SettingsOverlayDir');
  Label14.Caption := GetString(LangStrings.LanguageID, 'SettingsParticleDir');
  Label3.Caption := GetString(LangStrings.LanguageID, 'SettingsInitiativeOrder');
  cbInitiativeOrder.Items[0] := GetString(LangStrings.LanguageID, 'SettingsInitiativeOrderHighest');
  cbInitiativeOrder.Items[1] := GetString(LangStrings.LanguageID, 'SettingsInitiativeOrderLowest');
  cbTokensStartInvisible.Caption := GetString(LangStrings.LanguageID, 'SettingsTokenStartInvisible');
  Label7.Caption := GetString(LangStrings.LanguageID, 'SettingsTokenRotation');
  while cbTokenRotation.Items.Count < 2 do
    cbTokenRotation.Items.Add('');
  cbTokenRotation.Items[0] := GetString(LangStrings.LanguageID, 'SettingsTokenRotationStyle1');
  cbTokenRotation.Items[1] := GetString(LangStrings.LanguageID, 'SettingsTokenRotationStyle2');
  Label4.Caption := GetString(LangStrings.LanguageID, 'SettingsLanguage');
  Label5.Caption := GetString(LangStrings.LanguageID, 'SettingsLanguageHint');

  Label8.Caption := GetString(LangStrings.LanguageID, 'SettingsInterfaceTokenShadow');
  Label9.Caption := GetString(LangStrings.LanguageID, 'SettingsInterfaceWall');
  Label10.Caption := GetString(LangStrings.LanguageID, 'SettingsInterfacePortal');
  Label11.Caption := GetString(LangStrings.LanguageID, 'SettingsInterfaceHiddenToken');
  Label12.Caption := GetString(LangStrings.LanguageID, 'SettingsInterfaceMarker');
  Label13.Caption := GetString(LangStrings.LanguageID, 'SettingsInterfaceMeasure');

  bCancel.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');
  bOk.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');
end;

procedure TfmSettings.pnTokenShadowClick(Sender: TObject);
begin
  cdClrSelect.Color := TPanel(Sender).Color;
  if cdClrSelect.Execute then
    TPanel(Sender).Color := cdClrSelect.Color;
end;

end.

