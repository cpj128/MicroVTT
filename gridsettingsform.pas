{Copyright (c) 2023 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit GridSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  RPGTypes, ExtCtrls, SpinEx;

type

  { TfmGridSettings }

  TfmGridSettings = class(TForm)
    bSaveToLibrary: TButton;
    bOk: TButton;
    bCancel: TButton;
    cdGridColor: TColorDialog;
    cbGridType: TComboBox;
    fseGridOffsetY: TFloatSpinEditEx;
    fseGridSizeY: TFloatSpinEditEx;
    fseGridSizeX: TFloatSpinEditEx;
    fseGridOffsetX: TFloatSpinEditEx;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    pGridColor: TPanel;
    tbGridAlpha: TTrackBar;
    procedure bSaveToLibraryClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure fseGridSizeXChange(Sender: TObject);
    procedure pGridColorClick(Sender: TObject);
  private
    FCurGridData: TGridData;
    FUpdateMainForm: Boolean;
  public
    procedure SetData(data: TGridData; FromMainForm: Boolean);
    function GetData: TGridData;
  end;

var
  fmGridSettings: TfmGridSettings;

implementation

{$R *.lfm}

uses
  ControllerForm,
  LangStrings;

{ TfmGridSettings }

procedure TfmGridSettings.SetData(data: TGridData; FromMainForm: Boolean);
begin
  FUpdateMainForm := FromMainForm;
  bSaveToLibrary.Visible := FromMainForm;
  FCurGridData := data;
  fseGridSizeX.Value := FCurGridData.GridSizeX;
  fseGridSizeY.Value := FCurGridData.GridSizeY;
  fseGridOffsetX.Value := FCurGridData.GridOffsetX;
  fseGridOffsetY.Value := FCurGridData.GridOffsetY;
  cbGridType.ItemIndex := Ord(FCurGridData.GridType);
  tbGridAlpha.Position := FCurGridData.GridAlpha;
end;

function TfmGridSettings.GetData: TGridData;
begin
  Result := FCurGridData;
end;

procedure TfmGridSettings.pGridColorClick(Sender: TObject);
begin
  cdGridColor.Color := pGridColor.Color;
  if cdGridColor.Execute then
  begin
    pGridColor.Color := cdGridColor.Color;
    FCurGridData.GridColor := cdGridColor.Color;
    fmController.GridData := FCurGridData;
    fmController.pbViewport.Invalidate;
  end;
end;

procedure TfmGridSettings.FormShow(Sender: TObject);
begin
  Caption := GetString(LangStrings.LanguageID, 'GridSettingsCaption');
  Label6.Caption := GetString(LangStrings.LanguageID, 'GridSettingsType');
  Label1.Caption := GetString(LangStrings.LanguageID, 'GridSettingsSizeX'); 
  Label5.Caption := GetString(LangStrings.LanguageID, 'GridSettingsSizeY');
  Label2.Caption := GetString(LangStrings.LanguageID, 'GridSettingsOffsetX');
  Label3.Caption := GetString(LangStrings.LanguageID, 'GridSettingsOffsetY');
  Label4.Caption := GetString(LangStrings.LanguageID, 'GridSettingsColor');
  Label7.Caption := GetString(LangStrings.LanguageID, 'GridSettingsOpacity');
  cbGridType.Items[0] := GetString(LangStrings.LanguageID, 'GridSettingsType0');
  cbGridType.Items[1] := GetString(LangStrings.LanguageID, 'GridSettingsType1');
  cbGridType.Items[2] := GetString(LangStrings.LanguageID, 'GridSettingsType2');
  cbGridType.Items[3] := GetString(LangStrings.LanguageID, 'GridSettingsType3');
  bCancel.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');
  bOk.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');
  bSaveToLibrary.Caption := GetString(LangStrings.LanguageID, 'GridSettingsSaveToLibrary');
end;

procedure TfmGridSettings.bSaveToLibraryClick(Sender: TObject);
var
  ContentList: TStringList;
begin
  if fmController.MapLib.IndexOfName(fmController.MapFileName) < 0 then
    Exit;
  ContentList := TStringList.Create;
  try
    ContentList.Delimiter := '|';
    ContentList.StrictDelimiter := True;
    ContentList.DelimitedText := fmController.MapLib.Values[fmController.MapFileName];
    if ContentList.Count > 1 then
    begin
      ContentList[1] := FCurGridData.ToString;
      fmController.MapLib.Values[fmController.MapFileName] := ContentList.DelimitedText;
    end;
  finally
    ContentList.Free;
  end;
end;

procedure TfmGridSettings.fseGridSizeXChange(Sender: TObject);
begin
  FCurGridData.GridSizeX   := fseGridSizeX.Value;
  FCurGridData.GridSizeY   := fseGridSizeY.Value;
  FCurGridData.GridOffsetX := fseGridOffsetX.Value;
  FCurGridData.GridOffsetY := fseGridOffsetY.Value;
  FCurGridData.GridType    := TGridType(cbGridType.ItemIndex);
  FCurGridData.GridAlpha   := tbGridAlpha.Position;
  if FUpdateMainForm then
  begin
    fmController.GridData := FCurGridData;
    fmController.pbViewport.Invalidate;
  end;
end;

end.

