program RPGPres;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, ControllerForm, DisplayForm, DisplayConst,
  GridSettingsForm, TokenSettingsForm, SettingsForm, LangStrings, RPGTypes,
  LibraryForm, InitiativeForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfmController, fmController);
  Application.CreateForm(TfmDisplay, fmDisplay);
  Application.CreateForm(TfmGridSettings, fmGridSettings);
  Application.CreateForm(TfmTokenSettings, fmTokenSettings);
  Application.CreateForm(TfmSettings, fmSettings);
  Application.CreateForm(TfmLibrary, fmLibrary);
  Application.CreateForm(TfmSetInitiative, fmSetInitiative);
  Application.Run;
end.

