{Copyright (c) 2023 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit ControllerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, DateTimePicker, IniFiles, BGRABitmap, RPGTypes, HtmlView,
  Notes, HtmlGlobals, HTMLUn2, LCLType, WallManager;

const
  MAPLIBFILE = 'Maps.txt';
  TOKENLIBFILE = 'Tokens.txt';
  OVERLAYLIBFILE = 'Overlays.txt';

type

  { TfmController }

  TfmController = class(TForm)
    bResetZoom: TButton;
    bAddCategory: TButton;
    bDeleteCategory: TButton;
    bOkCategory: TButton;
    bCancelCategory: TButton;
    bAddAnnotation: TButton;
    bEditAnnotation: TButton;
    bDeleteAnnotation: TButton;
    bEntryOK: TButton;
    bEntryCancel: TButton;
    bAddEntry: TButton;
    bDeleteEntry: TButton;
    bOKEntryList: TButton;
    bCancelEntryList: TButton;
    cbCategory: TComboBox;
    cbDMOnly: TCheckBox;
    cbAnnotationDMOnly: TCheckBox;
    dtpTimestamp: TDateTimePicker;
    dtpAnnotationTimestamp: TDateTimePicker;
    eNewEntry: TEdit;
    eEntryName: TEdit;
    eNewCategory: TEdit;
    gbAnnotations: TGroupBox;
    hvNotesDisplay: THtmlViewer;
    ilMapIcons: TImageList;
    ilMenuItems: TImageList;
    Image1: TImage;
    lbCategories: TListBox;
    lbEntryList: TListBox;
    lvAnnotations: TListView;
    lvInitiative: TListView;
    lvMaps: TListView;
    lZoom: TLabel;
    MainMenu1: TMainMenu;
    mAnnotationContent: TMemo;
    mEntryContent: TMemo;
    miFile: TMenuItem;
    miSettings: TMenuItem;
    miQuit: TMenuItem;
    odLoadSession: TOpenDialog;
    pcNotesMain: TPageControl;
    pcMain: TPageControl;
    pbViewport: TPaintBox;
    pPortrait: TPanel;
    sdSaveSession: TSaveDialog;
    sddExportDir: TSelectDirectoryDialog;
    tbLoadNotes: TToolButton;
    tbSaveNotes: TToolButton;
    tbHistoryBack: TToolButton;
    tbHistoryForward: TToolButton;
    ToolButton4: TToolButton;
    tbExportForDM: TToolButton;
    tbExportForPlayers: TToolButton;
    tbMeasure: TToolButton;
    tsCategoryEditor: TTabSheet;
    tsEntryListEditor: TTabSheet;
    tsEntryEditor: TTabSheet;
    tsDisplay: TTabSheet;
    tbNotes: TToolBar;
    tsNotes: TTabSheet;
    tsController: TTabSheet;
    tbClearInitiative: TToolButton;
    tbClearTokens: TToolButton;
    tbCombatMode: TToolButton;
    tbControl: TToolBar;
    tbFullscreen: TToolButton;
    tbGridSettings: TToolButton;
    tbHideMarker: TToolButton;
    tbHidePortrait: TToolButton;
    tbHideTokens: TToolButton;
    tbLibrary: TToolButton;
    tbLoadSession: TToolButton;
    tbMapZoom: TTrackBar;
    tbNextCombatant: TToolButton;
    tbRefreshMaps: TToolButton;
    tbRefreshTokens: TToolButton;
    tbRemoveFromInitiative: TToolButton;
    tbSaveSession: TToolButton;
    tbShowGrid: TToolButton;
    tbShowMap: TToolButton;
    tbSnapTokensToGrid: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tvTokens: TTreeView;
    procedure bAddAnnotationClick(Sender: TObject);
    procedure bAddCategoryClick(Sender: TObject);
    procedure bAddEntryClick(Sender: TObject);
    procedure bCancelCategoryClick(Sender: TObject);
    procedure bDeleteAnnotationClick(Sender: TObject);
    procedure bDeleteCategoryClick(Sender: TObject);
    procedure bDeleteEntryClick(Sender: TObject);
    procedure bEditAnnotationClick(Sender: TObject);
    procedure bEntryCancelClick(Sender: TObject);
    procedure bEntryOKClick(Sender: TObject);
    procedure bFullscreenClick(Sender: TObject);
    procedure bOkCategoryClick(Sender: TObject);
    procedure bOKEntryListClick(Sender: TObject);
    procedure bRefreshMapsClick(Sender: TObject);
    procedure bResetZoomClick(Sender: TObject);
    procedure eEntryNameKeyPress(Sender: TObject; var Key: char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hvNotesDisplayHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure hvNotesDisplayImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream);
    procedure lvAnnotationsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvInitiativeCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvInitiativeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lvInitiativeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvMapsDblClick(Sender: TObject);
    procedure pbViewportDblClick(Sender: TObject);
    procedure pbViewportDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pbViewportDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure pbViewportMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbViewportMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbViewportMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbViewportPaint(Sender: TObject);
    procedure pPortraitDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pPortraitDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure pPortraitResize(Sender: TObject);
    procedure tbClearInitiativeClick(Sender: TObject);
    procedure tbClearTokensClick(Sender: TObject);
    procedure tbCombatModeClick(Sender: TObject);
    procedure tbExitClick(Sender: TObject);
    procedure tbExportForDMClick(Sender: TObject);
    procedure tbGridSettingsClick(Sender: TObject);
    procedure tbHideMarkerClick(Sender: TObject);
    procedure tbHidePortraitClick(Sender: TObject);
    procedure tbHideTokensClick(Sender: TObject);
    procedure tbHistoryBackClick(Sender: TObject);
    procedure tbHistoryForwardClick(Sender: TObject);
    procedure tbLibraryClick(Sender: TObject);
    procedure tbLoadNotesClick(Sender: TObject);
    procedure tbLoadSessionClick(Sender: TObject);
    procedure tbMapZoomChange(Sender: TObject);
    procedure tbMeasureClick(Sender: TObject);
    procedure tbNextCombatantClick(Sender: TObject);
    procedure tbRefreshMapsClick(Sender: TObject);
    procedure tbRefreshTokensClick(Sender: TObject);
    procedure tbRemoveFromInitiativeClick(Sender: TObject);
    procedure tbSaveNotesClick(Sender: TObject);
    procedure tbSaveSessionClick(Sender: TObject);
    procedure tbSettingsClick(Sender: TObject);
    procedure tbShowGridClick(Sender: TObject);
    procedure tbShowMapClick(Sender: TObject);
    procedure tbSnapTokensToGridClick(Sender: TObject);
    procedure tvTokensDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvTokensDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FMapPic: TBGRABitmap;
    FMapFileName: string;
    FViewRectWidth, FViewRectHeight: Integer;
    FViewRectXOffset, FViewRectYOffset: Integer;
    FViewRectMaxXOffset, FViewRectMaxYOffset: Integer;
    FDisplayScale: Double;
    FZoomFactor: Double;
    FIsDragging: Boolean;
    FIsDraggingToken: Boolean;
    FIsRotatingToken: Boolean;
    FCurDraggedToken: TToken;
    FSnapTokensToGrid: Boolean;
    FDragStartX, FDragStartY, // In pbViewPort-coordinates
    FLastMouseX, FLastMouseY, // In pbViewPort-coordinates
    FStartDragXOffset, FStartDragYOffset: Integer;
    FLastClicked, FLastLastClicked: TPoint; // MapPic-coordinates
    FCurMeasure: Double; 
    FShowGrid: Boolean;
    FGridData: TGridData;
    FMarkerPosX, FMarkerPosY: Integer; // In MapPic-coordinates
    FShowMap: Boolean;
    FShowMarker: Boolean;
    FShowTokens: Boolean;
    FCombatMode: Boolean;
    FInitiativePicList: TList;
    FInitiativeNumList: TList;
    FCurInitiativeIndex: Integer;
    FTokenList: TList;
    FMapDir, FTokenDir, FOverlayDir: string;
    FThumbnailDir: string;
    FInitiativeDesc, FTokensStartInvisible: Boolean;
    FTokenRotationStyle: TTokenRotationStyle;
    FAppSettings: TIniFile;
    FMapLib, FTokenLib, FOverlayLib: TStringList;
    // Notes module
    FNotesList: TEntryList; 
    FEditedEntry: TNoteEntry;
    FHistoryList: TStringList;
    FHistoryIdx: Integer;
    FNotesSaved: Boolean;
    FCurTokenRect: TRect;
    FWallManager: TWallManager;

    procedure UpdateMapList;
    procedure UpdateTokenList;
    procedure UpdateOverlayList;
    function MapToViewPortX(MapX: Single): Integer;
    function MapToViewPortY(MapY: Single): Integer;
    function ViewPortToMapX(ViewPortX: Integer): Integer;
    function ViewPortToMapY(ViewPortY: Integer): Integer;
    function GetTokenAtPos(X, Y: Integer): TToken;
    function GetTokenExRangeAtPos(X, Y: Integer): TToken;
    procedure SetCurInitiativeIndex(val: Integer);
    procedure LoadMap(FileName: string);
    procedure SetCombatMode(val: Boolean);
    procedure SetCurTokenRect(val: TRect);
    // Notes module
    procedure UpdateHistoryButtons;
    //procedure WMAppCommand(var Msg: TMessage); message WM_APPCOMMAND;
  public
    procedure UpdateViewport;
    function GetToken(idx: Integer): TToken;
    function GetTokenCount: Integer;
    function GetOverlay(idx: Integer): TBGRABitmap;
    procedure RemoveToken(token: TToken);
    procedure TokenToFront(token: TToken);
    procedure TokenToBack(token: TToken);
    function GetInitiative(idx: Integer): TPicture;
    function GetInitiativeNum(idx: Integer): Integer;
    function GetInitiativeCount: Integer;
    function GetCurInitiativeIdx: Integer;
    //procedure SnapAllTokensToGrid;
    procedure SnapTokenToGrid(token: TToken);
    procedure SaveLibraryData;
    procedure AddToInitiative(pName, Path: string; Num, Value: Integer);
    function GetThumbnailName(filename: string): string;
    // Notes module
    procedure LoadHTML(Entry: string);
    procedure AddToHistory(Entry: string);

    property GridData: TGridData read FGridData write FGridData;
    property MapFileName: string read FMapFileName;
    property TokenRotationStyle: TTokenRotationStyle read FTokenRotationStyle;
    property CurInitiativeIndex: Integer read FCurInitiativeIndex write SetCurInitiativeIndex;
    property MapLib: TStringList read FMapLib;
    property TokenLib: TStringList read FTokenLib;
    property OverlayLib: TStringList read FOverlayLib;
    property MapDir: string read FMapDir;
    property TokenDir: string read FTokenDir;
    property OverlayDir: string read FOverlayDir;
    property ThumbnailDir: string read FThumbnailDir;
    property NotesList: TEntryList read FNotesList;
    property ShowMap: Boolean read FShowMap;
    property ShowMarker: Boolean read FShowMarker;
    property ShowGrid: Boolean read FShowGrid;
    property ShowTokens: Boolean read FShowTokens;
    property TokenSlotRect: TRect read FCurTokenRect write SetCurTokenRect;
  end;


  TPicLoaderThread = class(TThread)
    private
      FFileList: TStringList;
      CurIdx: Integer;
      Thumbnail: TBitmap;
      procedure SetThumbnail;
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended : Boolean; FileList: TStringList; Width, Height: Integer);
  end;

var
  fmController: TfmController;

implementation

{$R *.lfm}

uses
  FileUtil,
  StrUtils,
  Math,
  GetText,
  LCLIntf,
  RegExpr,
  BGRABitmapTypes, 
  BGRATextFX,
  BGRATransform,
  DisplayConst,
  LangStrings,
  MapLoaders,
  DisplayForm,
  GridSettingsForm,
  SettingsForm,
  LibraryForm,
  TokenSettingsForm,
  InitiativeForm;

{ TfmController }

procedure TfmController.FormShow(Sender: TObject);
begin
  Caption := GetString(LangStrings.LanguageID, 'ControllerCaption');

  miFile.Caption := GetString(LangStrings.LanguageID, 'MainMenuFile');             
  miSettings.Caption := GetString(LangStrings.LanguageID, 'MainMenuSettings');
  miQuit.Caption := GetString(LangStrings.LanguageID, 'MainMenuQuit');

  tsController.Caption := GetString(LangStrings.LanguageID, 'ControllerTabCaption');

  tbRefreshMaps.Hint := GetString(LangStrings.LanguageID, 'ControllerRefreshMapsHint');
  tbRefreshTokens.Hint := GetString(LangStrings.LanguageID, 'ControllerRefreshTokensHint');  
  tbLibrary.Hint := GetString(LangStrings.LanguageID, 'ControllerLibraryHint');
  tbLoadSession.Hint := GetString(LangStrings.LanguageID, 'ControllerLoadHint');
  tbSaveSession.Hint := GetString(LangStrings.LanguageID, 'ControllerSaveHint');

  tbFullScreen.Hint := GetString(LangStrings.LanguageID, 'ControllerFullscreenHint');
  tbShowMap.Hint := GetString(LangStrings.LanguageID, 'ControllerHideMapHint');
  tbShowgrid.Hint := GetString(LangStrings.LanguageID, 'ControllerToggleGridHint');
  tbGridSettings.Hint := GetString(LangStrings.LanguageID, 'ControllerGridSettingsHint'); 
  tbSnapTokensToGrid.Hint := GetString(LangStrings.LanguageID, 'ControllerSnapToGridHint');
  tbHideTokens.Hint := GetString(LangStrings.LanguageID, 'ControllerHideAllTokensHint');       
  tbMeasure.Hint := GetString(LangStrings.LanguageID, 'ControllerShowMeasurementHint');

  tbHideMarker.Hint := GetString(LangStrings.LanguageID, 'ControllerHideMarkerHint');
  tbHidePortrait.Hint := GetString(LangStrings.LanguageID, 'ControllerHidePortraitHint');
  tbClearTokens.Hint := GetString(LangStrings.LanguageID, 'ControllerClearTokensHint');

  tbCombatMode.Hint := GetString(LangStrings.LanguageID, 'ControllerCombatModeHint');
  tbNextCombatant.Hint := GetString(LangStrings.LanguageID, 'ControllerCombatNextHint');
  tbClearInitiative.Hint := GetString(LangStrings.LanguageID, 'ControllerClearInitiativeHint');
  tbRemoveFromInitiative.Hint := GetString(LangStrings.LanguageID, 'ControllerRemoveFromInitiativeHint');

  lvInitiative.Column[1].Caption := GetString(LangStrings.LanguageID, 'ControllerIniListHeaderName');
  lvInitiative.Column[2].Caption := GetString(LangStrings.LanguageID, 'ControllerIniListHeaderInitiative');
  lvInitiative.Column[3].Caption := GetString(LangStrings.LanguageID, 'ControllerIniListHeaderPath');

  fmDisplay.Show;
  UpdateMapList;
  UpdateTokenList;
  UpdateOverlayList;
  UpdateViewport;

  // Notes module
  tbLoadNotes.Hint := GetString(LangStrings.LanguageID, 'NotesLoadDBHint');
  tbSaveNotes.Hint := GetString(LangStrings.LanguageID, 'NotesSaveDBHint');
  tbHistoryBack.Hint := GetString(LangStrings.LanguageID, 'NotesHistoryBackHint');
  tbHistoryForward.Hint := GetString(LangStrings.LanguageID, 'NotesHistoryForwardHint');
  tbExportForDM.Hint := GetString(LangStrings.LanguageID, 'NotesExportAllHint');        
  tbExportForPlayers.Hint := GetString(LangStrings.LanguageID, 'NotesExportPlayersHint');
  bOkCategory.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');
  bCancelCategory.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');
  bAddCategory.Caption := GetString(LangStrings.LanguageID, 'ButtonAdd');
  bDeleteCategory.Caption := GetString(LangStrings.LanguageID, 'ButtonDelete');
  eNewCategory.TextHint := GetString(LangStrings.LanguageID, 'NotesNewCategoryHint');
  bEntryOK.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');    
  bEntryCancel.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');  
  gbAnnotations.Caption := GetString(LangStrings.LanguageID, 'NotesGroupAnnotations');
  bAddAnnotation.Caption := GetString(LangStrings.LanguageID, 'ButtonAdd');
  bDeleteAnnotation.Caption := GetString(LangStrings.LanguageID, 'ButtonDelete');
  bEditAnnotation.Caption := GetString(LangStrings.LanguageID, 'ButtonEdit');
  cbDMOnly.Caption := GetString(LangStrings.LanguageID, 'NotesCbDMOnly');
  cbAnnotationDMOnly.Caption := GetString(LangStrings.LanguageID, 'NotesCbDMOnly');
  lvAnnotations.Column[0].Caption := GetString(LangStrings.LanguageID, 'NotesAnnotHeaderDate');
  lvAnnotations.Column[1].Caption := GetString(LangStrings.LanguageID, 'NotesAnnotHeaderContent');
  lvAnnotations.Column[2].Caption := GetString(LangStrings.LanguageID, 'NotesAnnotHeaderDMOnly');
  eNewEntry.TextHint := GetString(LangStrings.LanguageID, 'NotesNewEntryHint');
  bAddEntry.Caption := GetString(LangStrings.LanguageID, 'ButtonAdd');
  bDeleteEntry.Caption := GetString(LangStrings.LanguageID, 'ButtonDelete');
  bOKEntryList.Caption := GetString(LangStrings.LanguageID, 'ButtonOk');
  bCancelEntryList.Caption := GetString(LangStrings.LanguageID, 'ButtonCancel');
end;

procedure TfmController.lvInitiativeCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var Ini1, Ini2: Integer;
begin
  Ini1 := StrToInt(Item1.SubItems[1]);
  Ini2 := StrToInt(Item2.SubItems[1]);
  Compare := CompareValue(Ini1, Ini2);
  if FInitiativeDesc then
    Compare := -Compare;
end;

procedure TfmController.lvMapsDblClick(Sender: TObject);
begin
  LoadMap(lvMaps.Items[lvMaps.ItemIndex].SubItems[0]);
end;

procedure TfmController.LoadMap(FileName: string);
var
  ContentList: TStringList;
  GridSettings: string;
  Loader: TMapLoader;
begin
  if not FileExists(FileName) then
    Exit;
  GridSettings := '';
  ContentList := TStringList.Create;
  try
    ContentList.Delimiter := '|';
    ContentList.StrictDelimiter := True;
    if FMapLib.IndexOfName(FileName) >= 0 then
    begin
      ContentList.DelimitedText := FMapLib.Values[FileName];
      if ContentList.Count > 1 then
        GridSettings := ContentList[1];
    end;
  finally
    ContentList.Free;
  end;
  if Length(GridSettings) > 0 then
  begin
    FGridData.FromString(GridSettings);
    fmDisplay.GridData := FGridData;
  end;
  FMapFileName := FileName;
  fmDisplay.MapFileName := FMapFileName;
  if Assigned(FMapPic) then
    FMapPic.Free;
  //FMapPic := TBGRABitmap.Create(FMapFileName, True);
  Loader := GetMapLoader(Filename);
  try
    FMapPic := loader.LoadFromFile(Filename);
    loader.LoadWalls(Filename, FWallManager);
  finally
    loader.Free;
  end;
  FViewRectXOffset := 0;
  FViewRectYOffset := 0;
  TokenSlotRect := Bounds(-1, -1, 0, 0);
  UpdateViewport;
  pbViewport.Invalidate;
  tbMapZoom.Enabled := True;
  bResetZoom.Enabled := True;
end;

procedure TfmController.pbViewportDblClick(Sender: TObject);
begin
  if Assigned(FMapPic) then
  begin
    FShowMarker := True;
    FIsDragging := False;
    FIsDraggingToken := False;
    FMarkerPosX := ViewPortToMapX(FLastMouseX);
    FMarkerPosY := ViewPortToMapY(FLastMouseY);
    fmDisplay.MarkerX := FMarkerPosX;
    fmDisplay.MarkerY := FMarkerPosY;
    pbViewPort.Invalidate;
  end;
end;

procedure TfmController.pbViewportDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  tmpToken: TToken;
begin
  if (Source is TTreeView) and Assigned(TTreeView(Source).Selected) and Assigned(TTreeView(Source).Selected.Data) then
  begin
    TTokenFactory.GridData := FGridData;
    TTokenFactory.TokensSnapToGrid := FSnapTokensToGrid;
    TTokenFactory.TokensStartInvisible := FTokensStartInvisible;
    TTokenFactory.ShowDirectionArrow := FTokenRotationStyle = rsShowArrow;
    tmpToken := TTokenFactory.CreateTokenFromNode(TTokenNodeData(TTreeView(Source).Selected.Data), ViewPortToMapX(X), ViewPortToMapY(Y));

    FTokenlist.Add(tmpToken);
    pbViewPort.Invalidate;
    fmDisplay.Invalidate;
  end
  else if (Source = lvMaps) then
  begin
    LoadMap(lvMaps.Items[lvMaps.ItemIndex].SubItems[0]);
  end;
end;

procedure TfmController.pbViewportDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Assigned(FMapPic) and
            (Source is TTreeView) and
            Assigned(TTreeView(Source).Selected) and
            Assigned(TTreeView(Source).Selected.Data)) or
            (Source = lvMaps);
end;

procedure TfmController.lvInitiativeDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := not FCombatMode and
            (Source is TTreeView) and
            Assigned(TTreeView(Source).Selected) and
            Assigned(TTreeView(Source).Selected.Data);
end;

procedure TfmController.lvInitiativeDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if not FCombatMode then
  begin
    fmSetInitiative.udBaseInitiative.Position := TTokenNodeData(TTreeView(Source).Selected.Data).BaseInitiative;
    fmSetInitiative.udRolledInitiative.Position := 1;
    fmSetInitiative.TokenName := TTokenNodeData(TTreeView(Source).Selected.Data).Name;
    fmSetInitiative.TokenPath := TTokenNodeData(TTreeView(Source).Selected.Data).FullPath;
    fmSetInitiative.TokenNo := 0;
    fmSetInitiative.Show;
  end;
end;

procedure TfmController.pbViewportMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FIsDragging := True;
    FCurDraggedToken := GetTokenAtPos(X, Y);
    FIsDraggingToken := Assigned(FCurDraggedToken);
    FDragStartX := X;
    FDragStartY := Y;
    if FIsDraggingToken then
    begin
      FIsRotatingToken := ssShift in Shift;
      {if FIsRotatingToken then
      begin
        FDragStartX := FCurDraggedToken.XEndPos + FCurDraggedToken.Width div 2;
        FDragStartY := FCurDraggedToken.YEndPos + FCurDraggedToken.Height div 2;
      end;}
      FCurDraggedToken.StopAnimation;
      FStartDragXOffset := FCurDraggedToken.XEndPos;
      FStartDragYOffset := FCurDraggedToken.YEndPos;
    end
    else
    begin
      FIsRotatingToken := False;
      FStartDragXOffset := FViewRectXOffset;
      FStartDragYOffset := FViewRectYOffset;
    end;
  end;
end;

procedure TfmController.pbViewportMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin 
  FLastMouseX := X;
  FLastMouseY := Y;
  if FIsDragging then
  begin
    if FIsDraggingToken then
    begin
      if FIsRotatingToken then
      begin
        // Rotate token
        FCurDraggedToken.Angle := ArcTan2((Y - FDragStartY), -(X - FDragStartX)) + PI/2;
      end
      else
      begin
        // Move token
        FCurDraggedToken.XPos := Round(FStartDragXOffset + (X - FDragStartX) / FDisplayScale / FZoomFactor);
        FCurDraggedToken.YPos := Round(FStartDragYOffset + (Y - FDragStartY) / FDisplayScale / FZoomFactor);
        TokenSlotRect := FCurDraggedToken.GetCellsAtPosition(FCurDraggedToken.XEndPos, FCurDraggedToken.YEndPos, FGridData);
        fmDisplay.DraggedTokenPos := Point(FCurDraggedToken.XEndPos, FCurDraggedToken.YEndPos);
      end;
      //fmDisplay.Invalidate;
    end
    else
    begin
      // Move map
      FViewRectXOffset := EnsureRange(FStartDragXOffset + FDragStartX - X, 0, FViewRectMaxXOffset);
      FViewRectYOffset := EnsureRange(FStartDragYOffset + FDragStartY - Y, 0, FViewRectMaxYOffset);
      //fmDisplay.MapOffsetX := Round(FViewRectXOffset / FDisplayScale);
      //fmDisplay.MapOffsetY := Round(FViewRectYOffset / FDisplayScale);
    end;
    pbViewPort.Invalidate;
  end;
end;

procedure TfmController.pbViewportMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedToken: TToken;
  AttachToken: TToken;
  AttachedIdx, CurIdx: Integer;
  diffGrid: TPointF;
  //Modal: TModalResult;
begin
  if Button = mbLeft then
  begin
    if FIsDragging then
    begin
      fmDisplay.MapOffsetX := Round(FViewRectXOffset / FDisplayScale);
      fmDisplay.MapOffsetY := Round(FViewRectYOffset / FDisplayScale);
      pbViewPort.Invalidate;
      fmDisplay.Invalidate;
    end;
    if FIsDraggingToken then
    begin
      if FIsRotatingToken then
      begin
        FCurDraggedToken.Angle := ArcTan2((Y - FDragStartY), -(X - FDragStartX)) + PI/2;
      end
      else
      begin
        if (FCurDraggedToken is TAttachableToken) and (ssCtrl in Shift) then
        begin
          AttachToken := GetTokenExRangeAtPos(X, Y);
          if Assigned(AttachToken) then
          begin
            TAttachableToken(FCurDraggedToken).AttachTo(AttachToken);
            // sort directly before attached token in list
            AttachedIdx := FTokenList.IndexOf(AttachToken);
            CurIdx := FTokenList.IndexOf(FCurDraggedToken);
            FTokenList.Move(CurIdx, Max(0, AttachedIdx - 1));
          end;
        end;
        if FSnapTokensToGrid then
          FCurDraggedToken.SnapToGrid(FGridData);
        FCurDraggedToken.StartAnimation;
        TokenSlotRect := Bounds(-1, -1, 0, 0);
        fmDisplay.DraggedTokenPos := Point(-1, -1);
      end;
    end;
    if (Abs(Y - FDragStartY) < 2) and (Abs(X - FDragStartX) < 2) then
    begin
      FLastLastClicked := FLastClicked;
      FLastClicked := Point(ViewportToMapX(X), ViewPortToMapY(Y));
      if (FLastClicked.X >= 0) and (FLastClicked.Y >= 0) and
         (FLastLastClicked.X >= 0) and (FLastLastClicked.Y >= 0) then
      begin
        DiffGrid.x := Abs(FLastLastClicked.X - FLastClicked.X) / FGridData.GridSizeX;
        DiffGrid.y := Abs(FLastLastClicked.Y - FLastClicked.Y) / FGridData.GridSizeY;
        if tbMeasure.Down then
        begin
          pbViewport.ShowHint := False;
          FCurMeasure := Hypot(DiffGrid.X, DiffGrid.Y);
          pbViewport.Hint := FloatToStrF(FCurMeasure, ffFixed, 4, 2);
          pbViewport.ShowHint := True;
        end;
      end;
    end;

    FIsDragging := False;
    FIsDraggingToken := False;
    FIsRotatingToken := False;
    FCurDraggedToken := nil;

  end
  else if Button = mbRight then
  begin
    ClickedToken := GetTokenAtPos(X, Y);
    if Assigned(ClickedToken) then
    begin

      fmTokenSettings.Left := Left + pbViewPort.Left + X;
      fmTokenSettings.Top  := Top  + pbViewPort.Top  + Y;
      fmTokenSettings.CombatMode := FCombatMode;
      fmTokenSettings.LinkedToken := ClickedToken;

      fmTokenSettings.Show; // Treat this one more like a popup menu

    end;
  end;
end;

procedure TfmController.pbViewportPaint(Sender: TObject);
var
  ScaledBmp, DrawnMapSegment, TokenBmp: TBGRABitmap;
  LoSMap: TBGRABitmap;
  i, j, k, CurGridPos: Integer;
  SegmentWidth, SegmentHeight: Integer;
  CurMarkerX, CurMarkerY: Integer;
  CurToken: TToken;
  CellRect, BoundingRect: TRect;
  Hex: array[0..5] of TPoint;
  Iso: array[0..3] of TPoint;
  Wall, WallP1, WallP2: TPoint;
  tmpGridSize: Single;
  Rotation: TBGRAAffineBitmapTransform;
  RotatedBmp, OverlayBmp, OverlayScaled: TBGRABitmap;
  AngleText: string;
  TextAngle: Integer;
  TextSize: TSize;
  LoSPoly: ArrayOfTPointF;
begin
  // Draw Map
  if Assigned(FMapPic) then
  begin
    LoSMap := TBGRABitmap.Create(FMapPic.Width, FMapPic.Height, clBlack);

    // TODO: Resampling takes a lot of time, probably faster to save and just do when zoom or scale changes
    ScaledBmp := FMapPic.Resample(Round(FMapPic.Width * FDisplayScale * FZoomFactor), Round(FMapPic.Height * FDisplayScale * FZoomFactor), rmSimpleStretch);
    try
      DrawnMapSegment := TBGRABitmap.Create(pbViewPort.Width, pbViewPort.Height, clWhite);
      try
        SegmentWidth := Max(Min(pbViewport.Width, ScaledBmp.Width - FViewRectXOffset), 0);
        SegmentHeight := Max(Min(pbViewport.Height, ScaledBmp.Height - FViewRectYOffset), 0);
        DrawnMapSegment.Canvas.CopyRect(Rect(0, 0, SegmentWidth, SegmentHeight),
                                        ScaledBmp.Canvas,
                                        Bounds(FViewRectXOffset, FViewRectYOffset, SegmentWidth, SegmentHeight));
        //DrawnMapSegment.SaveToFile('testOutput.jpg');
        // Grid
        if FShowGrid then
        begin
          case FGridData.GridType of
            gtRect:
            begin
              // Horizontal lines
              for i := 0 to Ceil(((FMapPic.Height - (FGridData.GridOffsetY mod FGridData.GridSizeY)) / FGridData.GridSizeY)) do
              begin
                CurGridPos := MapToViewPortY(FGridData.GridOffsetY + i * FGridData.GridSizeY);
                if InRange(CurGridPos, 0, MapToViewPortY(FMapPic.Height)) then
                begin
                  DrawnMapSegment.DrawLineAntialias(0.0, CurGridPos,
                                                    MapToViewPortX(FMapPic.Width), CurGridPos,
                                                    ColorToBGRA(FGridData.GridColor, FGridData.GridAlpha), 1);
                end;
              end;
              // Vertical lines
              for i := 0 to Ceil(((FMapPic.Width - (FGridData.GridOffsetX mod FGridData.GridSizeX)) / FGridData.GridSizeX)) do
              begin
                CurGridPos := MapToViewPortX(FGridData.GridOffsetX + i * FGridData.GridSizeX);
                if InRange(CurGridPos, 0, MapToViewPortX(FMapPic.Width)) then
                begin
                  DrawnMapSegment.DrawLineAntialias(CurGridPos, 0,
                                                    CurGridPos, MapToViewPortY(FMapPic.Height),
                                                    ColorToBGRA(FGridData.GridColor, FGridData.GridAlpha), 1);
                end;
              end;
              if FSnapTokensToGrid then
              begin
                for i := TokenSlotRect.Left to FCurTokenRect.Right - 1 do
                  for j := TokenSlotRect.Top to FCurTokenRect.Bottom - 1 do
                  begin
                    DrawnMapSegment.EllipseAntialias(MapToViewPortX((i + 0.5) * FGridData.GridSizeX + FGridData.GridOffsetX),
                                                     MapToViewPortY((j + 0.5) * FGridData.GridSizeY + FGridData.GridOffsetY),
                                                     0.4 * FGridData.GridSizeX * FDisplayScale * FZoomFactor,
                                                     0.4 * FGridData.GridSizeY * FDisplayScale * FZoomFactor, clGray, 2);
                  end;
              end;
            end;
            gtHexH:
            begin
              tmpGridSize := FGridData.GridSizeY  * 3 / 4;
              for i := 0 to Ceil(((FMapPic.Height - (FGridData.GridOffsetY mod tmpGridSize)) / tmpGridSize)) do
                for j := 0 to Ceil(((FMapPic.Width - (FGridData.GridOffsetX mod FGridData.GridSizeX)) / FGridData.GridSizeX)) do
                begin
                  CellRect := Rect(MapToViewPortX(FGridData.GridOffsetX + j * FGridData.GridSizeX),
                                   MapToViewportY(FGridData.GridOffsetY + i * tmpGridSize),
                                   MapToViewPortX(FGridData.GridOffsetX + (j + 1) * FGridData.GridSizeX),
                                   MapToViewportY(FGridData.GridOffsetY + i * tmpGridSize + FGridData.GridSizeY));
                  if Odd(i) then
                    CellRect.Offset(Round(FGridData.GridSizeX  * FDisplayScale * FZoomFactor / 2), 0);
                  Hex[0] := Point(CellRect.Left, CellRect.Bottom - CellRect.Height div 4);
                  Hex[1] := Point(CellRect.Left, CellRect.Top + CellRect.Height div 4);
                  Hex[2] := Point(CellRect.Left + CellRect.Width div 2, CellRect.Top);
                  Hex[3] := Point(CellRect.Right, CellRect.Top + CellRect.Height div 4);
                  Hex[4] := Point(CellRect.Right, CellRect.Bottom - CellRect.Height div 4);
                  Hex[5] := Point(CellRect.Left + CellRect.Width div 2, CellRect.Bottom);
                  DrawnMapSegment.DrawPolygonAntialias(Hex, ColorToBGRA(FGridData.GridColor, FGridData.GridAlpha), 1, BGRAPixelTransparent);
                  if FSnapTokensToGrid and TokenSlotRect.Contains(Point(j, i)) then
                    DrawnMapSegment.EllipseAntialias(CellRect.CenterPoint.X, CellRect.CenterPoint.Y,
                                                     0.4 * FGridData.GridSizeX * FDisplayScale * FZoomFactor,
                                                     0.4 * FGridData.GridSizeY * FDisplayScale * FZoomFactor, clGray, 2);
                end;
            end;
            gtHexV:
            begin
              tmpGridSize := FGridData.GridSizeX  * 3 / 4;
              for i := 0 to Ceil(((FMapPic.Height - (FGridData.GridOffsetY mod FGridData.GridSizeY)) / FGridData.GridSizeY)) do
                for j := 0 to Ceil(((FMapPic.Width - (FGridData.GridOffsetX mod tmpGridSize)) / tmpGridSize)) do
                begin
                  CellRect := Rect(MapToViewPortX(FGridData.GridOffsetX + j * tmpGridSize),
                                   MapToViewportY(FGridData.GridOffsetY + i * FGridData.GridSizeY),
                                   MapToViewPortX(FGridData.GridOffsetX + j * tmpGridSize + FGridData.GridSizeX),
                                   MapToViewportY(FGridData.GridOffsetY + (i + 1) * FGridData.GridSizeY));
                  if Odd(j) then
                    CellRect.Offset(0, Round(FGridData.GridSizeY  * FDisplayScale * FZoomFactor / 2));
                  Hex[0] := Point(CellRect.Left, CellRect.Top + CellRect.Height div 2);
                  Hex[1] := Point(CellRect.Left + CellRect.Width div 4, CellRect.Top);
                  Hex[2] := Point(CellRect.Right - CellRect.Width div 4, CellRect.Top);
                  Hex[3] := Point(CellRect.Right, CellRect.Top + CellRect.Height div 2);
                  Hex[4] := Point(CellRect.Right - CellRect.Width div 4, CellRect.Bottom);
                  Hex[5] := Point(CellRect.Left + CellRect.Width div 4, CellRect.Bottom);
                  DrawnMapSegment.DrawPolygonAntialias(Hex, ColorToBGRA(FGridData.GridColor, FGridData.GridAlpha), 1, BGRAPixelTransparent);
                  if FSnapTokensToGrid and TokenSlotRect.Contains(Point(j, i)) then
                    DrawnMapSegment.EllipseAntialias(CellRect.CenterPoint.X, CellRect.CenterPoint.Y,
                                                     0.4 * FGridData.GridSizeX * FDisplayScale * FZoomFactor,
                                                     0.4 * FGridData.GridSizeY * FDisplayScale * FZoomFactor, clGray, 2);
                end;
            end;
            gtIsometric:
            begin
              tmpGridSize := FGridData.GridSizeY / 2;
              for i := 0 to Ceil(((FMapPic.Height - (FGridData.GridOffsetY mod tmpGridSize)) / tmpGridSize)) do
                for j := 0 to Ceil(((FMapPic.Width - (FGridData.GridOffsetX mod FGridData.GridSizeX)) / FGridData.GridSizeX)) do
                begin
                  CellRect := Rect(MapToViewPortX(FGridData.GridOffsetX + j * FGridData.GridSizeX),
                                   MapToViewportY(FGridData.GridOffsetY + i * tmpGridSize),
                                   MapToViewPortX(FGridData.GridOffsetX + (j + 1) * FGridData.GridSizeX),
                                   MapToViewportY(FGridData.GridOffsetY + i * tmpGridSize + FGridData.GridSizeY));
                  if Odd(i) then
                    CellRect.Offset(Round(FGridData.GridSizeX  * FDisplayScale * FZoomFactor / 2), 0);

                  Iso[0] := Point(CellRect.Left, (CellRect.Bottom + CellRect.Top) div 2);
                  Iso[1] := Point((CellRect.Left + CellRect.Right) div 2, CellRect.Top);
                  Iso[2] := Point(CellRect.Right, (CellRect.Top + CellRect.Bottom) div 2);
                  Iso[3] := Point((CellRect.Left + CellRect.Right) div 2, CellRect.Bottom);

                  DrawnMapSegment.DrawPolygonAntialias(Iso, ColorToBGRA(FGridData.GridColor, FGridData.GridAlpha), 1, BGRAPixelTransparent);
                  if FSnapTokensToGrid and TokenSlotRect.Contains(Point(j, i)) then
                    DrawnMapSegment.EllipseAntialias(CellRect.CenterPoint.X, CellRect.CenterPoint.Y,
                                                     0.4 * FGridData.GridSizeX * FDisplayScale * FZoomFactor,
                                                     0.4 * FGridData.GridSizeY * FDisplayScale * FZoomFactor, clGray, 2);
                end;
            end;
          end;
        end;
        // Draw walls
        for i := 0 to FWallManager.GetWallCount - 1 do
        begin
          Wall := FWallManager.GetWall(i);
          WallP1 := FWallManager.GetPoint(Wall.X);
          WallP2 := FWallManager.GetPoint(Wall.Y);
          DrawnMapSegment.DrawLineAntialias(MapToViewPortX(WallP1.X), MapToViewPortY(WallP1.Y),
                                            MapToViewPortX(WallP2.X), MapToViewPortY(WallP2.Y), clGreen, 1);
        end;
        // Draw Tokens
        // TODO: Draw Tokens on the map before downsampling?
        // Probably not an improvement, because I'd have to resample to the desired size anyway
        for i := 0 to FTokenList.Count - 1 do
        begin
          CurToken := TToken(FTokenList[i]);
          TokenBmp := CurToken.Glyph.Resample(Round(CurToken.Width * FDisplayScale * FZoomFactor), Round(CurToken.Height * FDisplayScale * FZoomFactor));
          try
            if not CurToken.Visible then
            begin
              TokenBmp.DrawLineAntialias(0, 0, TokenBmp.Width, TokenBmp.Height, clRed, 2);
            end;
            if not FShowTokens then
            begin
              TokenBmp.DrawLineAntialias(TokenBmp.Width, 0, 0, TokenBmp.Height, clRed, 2);
            end;

            // Rotation for range indicator: Redraw entirely
            if CurToken is TRangeIndicator then
              CurToken.RedrawGlyph;

            Rotation := TBGRAAffineBitmapTransform.Create(TokenBmp);
            if ((FTokenRotationStyle = rsRotateToken) and not (CurToken is TRangeIndicator)) or
               (CurToken is TTextToken) then
            begin           
              BoundingRect := CurToken.GetBoundingRect;
              Rotation.Translate(-TokenBmp.Width / 2, -TokenBmp.Height / 2);
              Rotation.RotateRad(CurToken.Angle);
              Rotation.Translate(BoundingRect.Width * FDisplayScale * FZoomFactor / 2, BoundingRect.Height * FDisplayScale * FZoomFactor / 2);
            end
            else
              BoundingRect := Bounds(CurToken.XPos - CurToken.Width div 2, CurToken.YPos - CurToken.Height div 2, CurToken.Width, CurToken.Height);
            try
              RotatedBmp := TBGRABitmap.Create(Round(BoundingRect.Width * FDisplayScale * FZoomFactor),
                                               Round(BoundingRect.Height * FDisplayScale * FZoomFactor));
              RotatedBmp.Fill(Rotation, dmDrawWithTransparencY);

              // This and other stuff like this should probably be done by the tokens themselves
              if CurToken is TCharacterToken then
              begin                              
                // Add overlay
                OverlayBmp := GetOverlay(TCharacterToken(CurToken).OverlayIdx);
                if Assigned(OverlayBmp) then
                begin
                  OverlayScaled := OverlayBmp.Resample(Round(OverlayBmp.Width * FDisplayScale * FZoomFactor), Round(OverlayBmp.Height  * FDisplayScale * FZoomFactor));
                  OverlayScaled.Draw(RotatedBmp.Canvas,
                                     (RotatedBmp.Width - TokenBmp.Width) div 2,
                                     (RotatedBmp.Height - TokenBmp.Height) div 2,
                                     False);
                  OverlayScaled.Free;
                end;

              end;

              if CurToken is TLightToken then
              begin
                DrawnMapSegment.BlendImage(MapToViewPortX(CurToken.XEndPos) - RotatedBmp.Width div 2,
                                           MapToViewPortY(CurToken.YEndPos) - RotatedBmp.Height div 2,
                                           RotatedBmp, boAdditive);
              end
              else
              begin
                RotatedBmp.Draw(DrawnMapSegment.Canvas,
                                MapToViewPortX(CurToken.XEndPos) - RotatedBmp.Width div 2,
                                MapToViewPortY(CurToken.YEndPos) - RotatedBmp.Height div 2,
                                False);
                if CurToken is TCharacterToken then
                begin
                  {for j := 0 to 8 - 1 do
                  begin
                    LoSPoly := FWallManager.GetLoSPolygon(Point(Round(CurToken.XEndPos + 0.5 * CurToken.Width * Sin(2 * PI / 8 * j)),
                                                                Round(CurToken.YEndPos + 0.5 * CurToken.Width * Cos(2 * PI / 8 * j))),
                                                          Bounds(0, 0, FMapPic.Width, FMapPic.Height));
                                                          //Bounds(FViewRectXOffset, FViewRectYOffset, SegmentWidth, SegmentHeight));
                    for k := 0 to Length(LoSPoly) - 1 do
                      LoSPoly[k] := TPointF.Create(MapToViewPortX(LoSPoly[k].X), MapToViewPortY(LoSPoly[k].Y));
                    LoSMap.FillPolyAntialias(LoSPoly, BGRA(255, 255, 255, 85));
                    //DrawnMapSegment.DrawPolygonAntialias(LoSPoly, clRed, 4);
                  end;}
                  LoSPoly := FWallManager.GetLoSPolygon(Point(CurToken.XEndPos, CurToken.YEndPos),
                                                        Bounds(0, 0, FMapPic.Width, FMapPic.Height));
                  for k := 0 to Length(LoSPoly) - 1 do
                    LoSPoly[k] := TPointF.Create(MapToViewPortX(LoSPoly[k].X), MapToViewPortY(LoSPoly[k].Y));
                  LoSMap.FillPolyAntialias(LoSPoly, BGRA(255, 255, 255));
                end;
              end;
            finally
              Rotation.Free;
              RotatedBmp.Free;
            end;
          finally
            TokenBmp.Free;
          end;
        end;

        if FTokenList.Count > 0 then
          DrawnMapSegment.BlendImage(Bounds(0, 0, DrawnMapSegment.Width, DrawnMapSegment.Height), LoSMap,
                                     0, 0, boMultiply);
        LoSMap.Free;

        // Draw Marker
        if FShowMarker then
        begin
          CurMarkerX := MapToViewPortX(FMarkerPosX);
          CurMarkerY := MapToViewPortY(FMarkerPosY);
          DrawnMapSegment.EllipseAntialias(CurMarkerX, CurMarkerY, 3, 3, clRed, 2);
        end;

        // Draw Measurement
        if (FLastClicked.X >= 0) and (FLastClicked.Y >= 0) and
           (FLastLastClicked.X >= 0) and (FLastLastClicked.Y >= 0) and
           tbMeasure.Down then
        begin
          DrawnMapSegment.DrawLineAntialias(MapToViewPortX(FLastClicked.X) - 5,
                                            MapToViewPortY(FLastClicked.Y),
                                            MapToViewPortX(FLastClicked.X) + 5,
                                            MapToViewPortY(FLastClicked.Y),
                                            clYellow, 1);
          DrawnMapSegment.DrawLineAntialias(MapToViewPortX(FLastClicked.X),
                                            MapToViewPortY(FLastClicked.Y) - 5,
                                            MapToViewPortX(FLastClicked.X),
                                            MapToViewPortY(FLastClicked.Y) + 5,
                                            clYellow, 1);
          DrawnMapSegment.DrawLineAntialias(MapToViewPortX(FLastLastClicked.X) - 5,
                                            MapToViewPortY(FLastLastClicked.Y),
                                            MapToViewPortX(FLastLastClicked.X) + 5,
                                            MapToViewPortY(FLastLastClicked.Y),
                                            clYellow, 1);
          DrawnMapSegment.DrawLineAntialias(MapToViewPortX(FLastLastClicked.X),
                                            MapToViewPortY(FLastLastClicked.Y) - 5,
                                            MapToViewPortX(FLastLastClicked.X),
                                            MapToViewPortY(FLastLastClicked.Y) + 5,
                                            clYellow, 1);
          DrawnMapSegment.DrawLineAntialias(MapToViewPortX(FLastClicked.X),
                                            MapToViewPortY(FLastClicked.Y),
                                            MapToViewPortX(FLastLastClicked.X),
                                            MapToViewPortY(FLastLastClicked.Y),
                                            clYellow, 1);
          TextAngle := Round(RadToDeg(-ArcTan2(FLastClicked.Y - FLastLastClicked.Y, FLastClicked.X - FLastLastClicked.X))) * 10;
          if TextAngle > 900 then
            TextAngle := TextAngle - 1800
          else if TextAngle < -900 then
            TextAngle := TextAngle + 1800;
          AngleText := FloatToStrF(FCurMeasure, ffFixed, 3, 2);
          TextSize := DrawnMapSegment.TextSize(AngleText);
          DrawnMapSegment.TextOutAngle(MapToViewPortX((FLastClicked.X + FLastLastClicked.X - (Cos(DegToRad(TextAngle / 10)) * TextSize.cX)) / 2),
                                       MapToViewPortY((FLastClicked.Y + FLastLastClicked.Y + (Sin(DegToRad(TextAngle / 10)) * TextSize.cX)) / 2),
                                       TextAngle,
                                       AngleText, clYellow);
        end;

        // Draw marker if player's view is disabled
        if not FShowMap then
          DrawnMapSegment.DrawLineAntialias(0, 0, FViewRectWidth, FViewRectHeight, clRed, 3);

        DrawnMapSegment.Draw(pbViewPort.Canvas, 0, 0, False);
      finally
        DrawnMapSegment.Free;
      end;

    finally
      ScaledBmp.Free;
    end;
  end;

  // Draw Bounds of Display window
  pbViewPort.Canvas.Brush.Style := bsClear;
  pbViewPort.Canvas.Pen.Width := 3;
  pbViewPort.Canvas.Pen.Color := clBlack;
  pbViewPort.Canvas.Rectangle(0, 0, FViewRectWidth, FViewRectHeight);
  if not (FShowMap and Assigned(FMapPic)) then
  begin
    // Draw marker here, if player's view is disabled and no map is loaded
    pbViewPort.Canvas.Pen.Color := clRed;
    pbViewPort.Canvas.MoveTo(0, 0);
    pbViewPort.Canvas.LineTo(FViewRectWidth, FViewRectHeight);
  end;
end;

procedure TfmController.pPortraitDragDrop(Sender, Source: TObject; X, Y: Integer
  );
begin
  if (Source is TTreeView) and
     Assigned(TTreeView(Source).Selected) and
     Assigned(TTreeView(Source).Selected.Data) and
     (TTokenNodeData(TTreeView(Source).Selected.Data).TokenType = ttCharacter) then
    fmDisplay.PortraitFileName := TTokenNodeData(TTreeView(Source).Selected.Data).FullPath;
end;

procedure TfmController.pPortraitDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TTreeView);
end;

procedure TfmController.pPortraitResize(Sender: TObject);
begin
  UpdateViewPort;
end;

procedure TfmController.tbClearInitiativeClick(Sender: TObject);
begin
  lvInitiative.Clear;
end;

procedure TfmController.tbClearTokensClick(Sender: TObject);
var
  i: Integer;
  CurToken: TToken;
begin
  for i := FTokenlist.Count - 1 downto 0 do
  begin
    CurToken := TToken(FTokenList[i]);
    CurToken.Free;
    FTokenlist.Delete(i);
  end;
  pbViewPort.Invalidate;
  fmDisplay.Invalidate;
end;

procedure TfmController.tbCombatModeClick(Sender: TObject);
begin
  SetCombatMode(tbCombatMode.Down);
end;

procedure TfmController.SetCombatMode(val: Boolean);  
var
  i: Integer;
  bmp: TPicture;
begin
  FCombatMode := val;
  fmDisplay.CombatMode := FCombatMode;
  tbNextCombatant.Enabled := False;
  tbCombatMode.Down := FCombatMode;
  tbClearInitiative.Enabled := not FCombatMode;
  tbRemoveFromInitiative.Enabled := FCombatMode;
  if FCombatMode and (lvInitiative.Items.Count > 0) then
  begin
    tbNextCombatant.Enabled := True;
    // Sort List by initiative, descending
    lvInitiative.Sort;
    FCurInitiativeIndex := 0;
    // Generate list of bitmaps
    for i := 0 to lvInitiative.Items.Count - 1 do
    begin
      bmp := TPicture.Create;
      try
        bmp.LoadFromFile(lvInitiative.Items[i].SubItems[2]);
        FInitiativePicList.Add(bmp);
        FInitiativeNumList.Add(lvInitiative.Items[i].Data); // TODO
      except
        bmp.Free;
      end;
    end;
    // Mark first item
    lvInitiative.Items[0].Caption := '>';
  end;
  if not FCombatMode then
  begin
    for i := 0 to lvInitiative.Items.Count - 1 do
      lvInitiative.Items[i].Caption := '';
    for i := FInitiativePicList.Count - 1 downto 0 do
    begin
      bmp := TPicture(FInitiativePicList[i]);
      FInitiativePicList.Delete(i);
      FInitiativeNumList.Delete(i);
      bmp.Free;
    end;
  end;
end;

procedure TfmController.SetCurTokenRect(val: TRect);
begin
  FCurTokenRect := val;
  fmDisplay.TokenSlotRect := val;
  fmDisplay.Invalidate;
end;

function TfmController.MapToViewPortX(MapX: Single): Integer;
begin
  Result := Round(MapX * FDisplayScale * FZoomFactor - FViewRectXOffset);
end;

function TfmController.MapToViewPortY(MapY: Single): Integer;
begin
  Result := Round(MapY * FDisplayScale * FZoomFactor - FViewRectYOffset);
end;

function TfmController.ViewPortToMapX(ViewPortX: Integer): Integer;
begin
  Result := Round((ViewPortX + FViewRectXOffset) / FDisplayScale / FZoomFactor);
end;

function TfmController.ViewPortToMapY(ViewPortY: Integer): Integer;
begin
  Result := Round((ViewPortY + FViewRectYOffset) / FDisplayScale / FZoomFactor);
end;

procedure TfmController.SetCurInitiativeIndex(val: Integer);
begin
  FCurInitiativeIndex := val mod lvInitiative.Items.Count;
end;

function TfmController.GetTokenAtPos(X, Y: Integer): TToken;
var
  i: Integer;
  SearchPnt: TPoint;
  CurToken: TToken;
  TokenRect: TRect;
begin
  Result := nil;
  SearchPnt := Point(ViewPortToMapX(X), ViewPortToMapY(Y));
  // We search the list backwards, because the tokens are drawn first to last,
  // meaning the later ones are the highest in z-order
  for i := FTokenList.Count - 1 downto 0 do
  begin
    CurToken := TToken(FTokenList.Items[i]);
    TokenRect := Bounds(CurToken.XEndPos - CurToken.Width div 2,
                        CurToken.YEndPos - CurToken.Height div 2,
                        CurToken.Width,
                        CurToken.Height);
    if TokenRect.Contains(SearchPnt) then
    begin
      Result := CurToken;
      Break;
    end;
  end;
end;

function TfmController.GetTokenExRangeAtPos(X, Y: Integer): TToken;
var
  i: Integer;
  SearchPnt: TPoint;
  CurToken: TToken;
  TokenRect: TRect;
begin
  Result := nil;
  SearchPnt := Point(ViewPortToMapX(X), ViewPortToMapY(Y));
  // We search the list backwards, because the tokens are drawn first to last,
  // meaning the later ones are the highest in z-order
  for i := FTokenList.Count - 1 downto 0 do
  begin
    CurToken := TToken(FTokenList.Items[i]);
    if CurToken is TRangeIndicator then
      Continue;
    TokenRect := Bounds(CurToken.XEndPos - CurToken.Width div 2,
                        CurToken.YEndPos - CurToken.Height div 2,
                        CurToken.Width,
                        CurToken.Height);
    if TokenRect.Contains(SearchPnt) then
    begin
      Result := CurToken;
      Break;
    end;
  end;
end;

procedure TfmController.tbExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmController.tbGridSettingsClick(Sender: TObject);
var
  OldGridData: TGridData;
  i: Integer;
  CurToken: TToken;
begin
  OldGridData := FGridData;
  fmGridSettings.SetData(FGridData, True);

  if fmGridSettings.ShowModal = mrOK then
  begin
    FGridData := fmGridSettings.GetData;
    fmDisplay.GridData := GridData;
    if FSnapTokensToGrid then
    begin
      for i := 0 to FTokenlist.Count - 1 do
      begin
        CurToken := TToken(FTokenList[i]);
        CurToken.XPos := CurToken.XEndPos - Round(OldGridData.GridOffsetX + FGridData.GridOffsetX);
        CurToken.YPos := CurToken.YEndPos - Round(OldGridData.GridOffsetY + FGridData.GridOffsetY);
        CurToken.SnapToGrid(FGridData);
      end;
    end;
  end
  else
  begin
    FGridData := OldGridData;
  end;
  pbViewPort.Invalidate;
end;

procedure TfmController.tbHideMarkerClick(Sender: TObject);
begin
  FShowMarker := False;
  pbViewPort.Invalidate;
end;

procedure TfmController.tbHidePortraitClick(Sender: TObject);
begin
  fmDisplay.PortraitFileName := '';
end;

procedure TfmController.tbHideTokensClick(Sender: TObject);
begin
  FShowTokens := tbHideTokens.Down;
  pbViewPort.Invalidate;
  fmDisplay.Invalidate;
end;

procedure TfmController.tbLibraryClick(Sender: TObject);
begin
  fmLibrary.ShowModal;
  {FMapLib.SaveToFile(MAPLIBFILE);
  FTokenLib.SaveToFile(TOKENLIBFILE);
  FOverlayLib.SaveToFile(OVERLAYLIBFILE);

  UpdateMapList; // Maybe change just names here?
  UpdateTokenList;
  UpdateOverlayList;}
end;

procedure TfmController.tbMapZoomChange(Sender: TObject);
begin
  // Gives *0.2 at -100 and *5 at +100
  FZoomFactor := Power(5, (tbMapZoom.Position - 100) / 100);
  fmDisplay.MapZoom := FZoomFactor;
  lZoom.Caption := IntToStr(Round(100 * FZoomFactor)) + '%';
  UpdateViewPort;
end;

procedure TfmController.tbMeasureClick(Sender: TObject);
begin
  pbViewPort.ShowHint := tbMeasure.Down;
end;

procedure TfmController.tbNextCombatantClick(Sender: TObject);
begin
  if not FCombatMode then
    Exit;
  lvInitiative.Items[CurInitiativeIndex].Caption := '';
  CurInitiativeIndex := CurInitiativeIndex + 1;
  lvInitiative.Items[CurInitiativeIndex].Caption := '>';
  fmDisplay.Invalidate;
end;

procedure TfmController.tbRefreshMapsClick(Sender: TObject);
begin
  UpdateMapList;
end;

procedure TfmController.tbRefreshTokensClick(Sender: TObject);
begin
  UpdateTokenList;
end;

procedure TfmController.tbRemoveFromInitiativeClick(Sender: TObject);
var
  i, NewIndex: Integer;
  tmpPic: TPicture;
begin
  if (lvInitiative.ItemIndex >= 0) and FCombatMode then
  begin
    NewIndex := FCurInitiativeIndex;
    if FCurInitiativeIndex > lvInitiative.ItemIndex then
      Dec(NewIndex);
    tmpPic := TPicture(FInitiativePicList.Items[lvInitiative.ItemIndex]);
    FInitiativePicList.Delete(lvInitiative.ItemIndex);
    FInitiativeNumList.Delete(lvInitiative.ItemIndex);
    tmpPic.Free;
    lvInitiative.Items.Delete(lvInitiative.ItemIndex);
    FCurInitiativeIndex := NewIndex;
    for i := 0 to lvInitiative.Items.Count - 1 do
      lvInitiative.Items[i].Caption := IfThen(i = NewIndex, '>', '');
    fmDisplay.Invalidate;
  end;
end;

procedure TfmController.tbSaveSessionClick(Sender: TObject);
var
  saveFile: TIniFile;
  i: Integer;
  tmpItem: TListItem;
begin
  if sdSaveSession.Execute then
  begin
    try
      saveFile := TIniFile.Create(sdSaveSession.FileName);

      // Map data
      saveFile.WriteString(SAVESECTIONMAP, 'MapFile', FMapFileName);
      saveFile.WriteInteger(SAVESECTIONMAP, 'OffsetX', FViewRectXOffset);
      saveFile.WriteInteger(SAVESECTIONMAP, 'OffsetY', FViewRectYOffset);
      saveFile.WriteFloat(SAVESECTIONMAP, 'ZoomFactor', FZoomFactor);
      saveFile.WriteBool(SAVESECTIONMAP, 'MarkerVisible', FShowMarker);
      saveFile.WriteInteger(SAVESECTIONMAP, 'MarkerX', FMarkerPosX);
      saveFile.WriteInteger(SAVESECTIONMAP, 'MarkerY', FMarkerPosY);

      // Grid data
      saveFile.WriteInteger(SAVESECTIONGRID, 'Type', Ord(FGridData.GridType));
      saveFile.WriteBool(SAVESECTIONGRID, 'GridVisible', FShowGrid); 
      saveFile.WriteBool(SAVESECTIONGRID, 'Snap', FSnapTokensToGrid);
      saveFile.WriteFloat(SAVESECTIONGRID, 'SizeX', FGridData.GridSizeX);
      saveFile.WriteFloat(SAVESECTIONGRID, 'SizeY', FGridData.GridSizeY);
      saveFile.WriteFloat(SAVESECTIONGRID, 'OffsetX', FGridData.GridOffsetX);
      saveFile.WriteFloat(SAVESECTIONGRID, 'OffsetY', FGridData.GridOffsetY);
      saveFile.WriteInteger(SAVESECTIONGRID, 'Color', FGridData.GridColor);
      saveFile.WriteInteger(SAVESECTIONGRID, 'Alpha', FGridData.GridAlpha);

      // Portrait
      saveFile.WriteString(SAVESECTIONPORTRAIT, 'FileName', fmDisplay.PortraitFileName);

      // Initiative
      saveFile.WriteBool(SAVESECTIONINITIATIVE, 'CombatMode', FCombatMode);
      saveFile.WriteInteger(SAVESECTIONINITIATIVE, 'CurIndex', FCurInitiativeIndex);
      for i := 0 to lvInitiative.Items.Count - 1 do
      begin
        tmpItem := lvInitiative.Items[i];
        saveFile.WriteString(SAVESECTIONINITIATIVE, 'Name' + IntToStr(i), tmpItem.SubItems[0]);
        saveFile.WriteString(SAVESECTIONINITIATIVE, 'Value' + IntToStr(i), tmpItem.SubItems[1]);
        saveFile.WriteString(SAVESECTIONINITIATIVE, 'Path' + IntToStr(i), tmpItem.SubItems[2]);
      end;

      // Tokens
      saveFile.WriteBool(SAVESECTIONTOKENS, 'TokenVisible', FShowTokens);
      for i := 0 to FTokenList.Count - 1 do
      begin
        TToken(FTokenList[i]).SaveToIni(SaveFile, i);
      end;

      saveFile.UpdateFile;
    finally
      saveFile.Free;
    end;
  end;

end;

procedure TfmController.tbLoadSessionClick(Sender: TObject);
var
  saveFile: TIniFile;
  i: Integer;
  tmpItem: TListItem;
  CurToken: TToken;
  attachList: TList;
begin
  if odLoadSession.Execute then
  begin
    saveFile := TIniFile.Create(odLoadSession.FileName);
    try
      // Map data
      LoadMap(saveFile.ReadString(SAVESECTIONMAP, 'MapFile', ''));
      FViewRectXOffset := saveFile.ReadInteger(SAVESECTIONMAP, 'OffsetX', 0);
      FViewRectYOffset := saveFile.ReadInteger(SAVESECTIONMAP, 'OffsetY', 0);
      FZoomFactor := saveFile.ReadFloat(SAVESECTIONMAP, 'ZoomFactor', 1);
      FShowMarker := saveFile.ReadBool(SAVESECTIONMAP, 'MarkerVisible', False);
      FMarkerPosX := saveFile.ReadInteger(SAVESECTIONMAP, 'MarkerX', -1);      
      FMarkerPosy := saveFile.ReadInteger(SAVESECTIONMAP, 'MarkerY', -1);
      // This also sets the label and the zoom for player's view
      tbMapZoom.Position := 100 + Round(100 * Ln(FZoomFactor) / Ln(5));

      // Grid data
      FGridData.GridType := TGridType(saveFile.ReadInteger(SAVESECTIONGRID, 'Type', 0));
      FShowGrid := saveFile.ReadBool(SAVESECTIONGRID, 'GridVisible', True);
      FSnapTokensToGrid := saveFile.ReadBool(SAVESECTIONGRID, 'Snap', False);
      FGridData.GridSizeX := saveFile.ReadInteger(SAVESECTIONGRID, 'SizeX', 100);     
      FGridData.GridSizeY := saveFile.ReadInteger(SAVESECTIONGRID, 'SizeY', 100);     
      FGridData.GridOffsetX := saveFile.ReadInteger(SAVESECTIONGRID, 'OffsetX', 0);
      FGridData.GridOffsetY := saveFile.ReadInteger(SAVESECTIONGRID, 'OffsetY', 0);
      FGridData.GridColor := saveFile.ReadInteger(SAVESECTIONGRID, 'Color', clSilver);
      FGridData.GridAlpha := saveFile.ReadInteger(SAVESECTIONGRID, 'Alpha', 255);
       
      // Portrait
      fmDisplay.PortraitFileName := saveFile.ReadString(SAVESECTIONPORTRAIT, 'FileName', '');

      // Initiative
      i := 0;
      lvInitiative.BeginUpdate;
      lvInitiative.Items.Clear;
      while saveFile.ValueExists(SAVESECTIONINITIATIVE, 'Name' + IntToStr(i)) do
      begin
        tmpItem := lvInitiative.Items.Add;
        tmpItem.Caption := '';
        tmpItem.SubItems.Add(saveFile.ReadString(SAVESECTIONINITIATIVE, 'Name' + IntToStr(i), ''));
        tmpItem.SubItems.Add(saveFile.ReadString(SAVESECTIONINITIATIVE, 'Value' + IntToStr(i), '0'));
        tmpItem.SubItems.Add(saveFile.ReadString(SAVESECTIONINITIATIVE, 'Path' + IntToStr(i), ''));
        Inc(i);
      end;
      lvInitiative.EndUpdate;
      SetCombatMode(saveFile.ReadBool(SAVESECTIONINITIATIVE, 'CombatMode', False));
      FCurInitiativeIndex := saveFile.ReadInteger(SAVESECTIONINITIATIVE, 'CurIndex', 0);
      if FCombatMode then
        for i := 0 to lvInitiative.Items.Count - 1 do
          lvInitiative.Items[i].Caption := IfThen(i = FCurInitiativeIndex, '>', '');

      // Tokens
      for i := FTokenList.Count - 1 downto 0 do
      begin
        CurToken := TToken(FTokenList[i]);
        CurToken.Free;
        FTokenList.Delete(i);
      end;

      FShowTokens := saveFile.ReadBool(SAVESECTIONTOKENS, 'TokenVisible', True);

      attachList := TList.Create;

      i := 0;
      while saveFile.ValueExists(SAVESECTIONTOKENS, 'XPos' + IntToStr(i)) do
      begin
        TTokenFactory.TokensStartInvisible := FTokensStartInvisible;
        TTokenFactory.ShowDirectionArrow := FTokenRotationStyle = rsShowArrow;
        CurToken := TTokenFactory.CreateTokenFromIni(SaveFile, i);
        if Assigned(CurToken) then
        begin
          if SaveFile.ValueExists(SAVESECTIONTOKENS, 'Attached' + IntToStr(i)) then
            attachList.Add(CurToken);
          if not (CurToken is TAttachableToken) then
          begin
            while attachList.Count > 0 do
            begin
              TAttachableToken(attachList[0]).AttachTo(CurToken);
              attachList.Delete(0);
            end;
          end;
          FTokenList.Add(CurToken);
        end;
        Inc(i);
      end;
      attachList.Free;

      pbViewPort.Invalidate;
      fmDisplay.Invalidate;

    finally
      saveFile.Free;
    end;

  end;
end;

procedure TfmController.tbSettingsClick(Sender: TObject);
var
  i: Integer;
  CurToken: TToken;
begin
  fmSettings.eMapDirectory.Text := FMapDir;
  fmSettings.eTokenDirectory.Text := FTokenDir;
  fmSettings.eOverlayDirectory.Text := FOverlayDir;
  fmSettings.cbTokensStartInvisible.Checked := FTokensStartInvisible;
  fmSettings.cbInitiativeOrder.ItemIndex := IfThen(FInitiativeDesc, 0, 1);
  fmSettings.cbTokenRotation.ItemIndex := Ord(FTokenRotationStyle);
  fmSettings.cbLanguage.Items := GetLanguages;
  fmSettings.cbLanguage.ItemIndex := fmSettings.cbLanguage.Items.IndexOf(LanguageID);
  if fmSettings.ShowModal = mrOK then
  begin
    if not SameText(FMapDir, fmSettings.eMapDirectory.Text) then
    begin
      FMapDir := fmSettings.eMapDirectory.Text;
      UpdateMapList;
    end;
    if not SameText(FTokenDir, fmSettings.eTokenDirectory.Text) then
    begin
      FTokenDir := fmSettings.eTokenDirectory.Text;
      UpdateTokenList;
    end;
    if not SameText(FOverlayDir, fmSettings.eOverlayDirectory.Text) then
    begin
      FOverlayDir := fmSettings.eOverlayDirectory.Text;
      UpdateOverlayList;
    end;
    FTokensStartInvisible := fmSettings.cbTokensStartInvisible.Checked;
    FInitiativeDesc := fmSettings.cbInitiativeOrder.ItemIndex = 0;
    FTokenRotationStyle := TTokenRotationStyle(fmSettings.cbTokenRotation.ItemIndex);
    LanguageID := fmSettings.cbLanguage.Items[fmSettings.cbLanguage.ItemIndex];
    for i := 0 to FTokenList.Count - 1 do
    begin
      CurToken := TToken(FTokenList[i]);
      if CurToken is TCharacterToken then
      begin
        TCharacterToken(CurToken).ShowArrow := FTokenRotationStyle = rsShowArrow;
        CurToken.RedrawGlyph;
      end;
    end;
    // Save changes to ini
    FAppSettings.WriteString('Settings', 'MapDir', FMapDir);
    FAppSettings.WriteString('Settings', 'TokenDir', FTokenDir);
    FAppSettings.WriteString('Settings', 'OverlayDir', FOverlayDir);
    FAppSettings.WriteString('Settings', 'InitiativeDesc', BoolToStr(FInitiativeDesc));
    FAppSettings.WriteString('Settings', 'TokensStartInvisible', BoolToStr(FTokensStartInvisible));
    FAppSettings.WriteInteger('Settings', 'TokenRotationStyle', Ord(FTokenRotatioNStyle));
    FAppSettings.WriteString('Settings', 'Language', LanguageID);
    FAppSettings.UpdateFile;

    pbViewPort.Invalidate;
    fmDisplay.Invalidate;
  end;
end;

procedure TfmController.tbShowGridClick(Sender: TObject);
begin
  FShowGrid := tbShowGrid.Down;
  fmDisplay.Invalidate;;
  pbViewPort.Invalidate;
end;

procedure TfmController.tbShowMapClick(Sender: TObject);
begin
  FShowMap := tbShowMap.Down;
  pbViewPort.Invalidate;
  fmDisplay.Invalidate;
end;

procedure TfmController.tbSnapTokensToGridClick(Sender: TObject);
var i: Integer;
begin
  FSnapTokensToGrid := tbSnapTokensToGrid.Down;
  fmDisplay.SnapTokensToGrid := FSnapTokensToGrid;
  if FSnapTokensToGrid then
  begin
    for i := 0 to FTokenList.Count - 1 do
      TToken(FTokenList[i]).SnapToGrid(FGridData);
    pbViewPort.Invalidate;
    fmDisplay.Invalidate;
  end;
end;

procedure TfmController.tvTokensDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
  begin
    TTokenNodeData(Node.Data).Free;
    Node.Data := nil;
  end;
end;

procedure TfmController.tvTokensDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
end;

procedure TfmController.UpdateMapList;
var
  i: Integer;
  FileList, ContentList, ToBeLoaded: TStringList;
  FileName, ThumbnailName: string;
  title: string;
  Thumbnail: TJPEGImage;
begin
  lvMaps.Clear;
  FileList := TStringList.Create;
  ContentList := TStringList.Create;
  ToBeLoaded := TStringList.Create;
  try
    ContentList.Delimiter := '|';
    ContentList.StrictDelimiter := True;
    FindAllFiles(FileList, FMapDir, PicFilterStrAll, True);
    for i := 0 to FileList.Count - 1 do
    begin
      FileName := FileList[i];                    
      ThumbnailName := GetThumbnailName(FileName);

      with lvMaps.Items.Add do
      begin
        if FMapLib.IndexOfName(FileName) >= 0 then
        begin
          ContentList.DelimitedText := FMapLib.Values[FileName];
          if ContentList.Count > 0 then
            title := ContentList[0]
          else
            title := ExtractFileName(FileName);
        end
        else
          title := ExtractFileName(FileName);
        Caption := title;
        if FileExists(ThumbnailName) then
        begin
          Thumbnail := TJPEGImage.Create;
          Thumbnail.LoadFromFile(ThumbnailName);
          ImageIndex := ilMapIcons.Add(Thumbnail, nil);
        end
        else
        begin
          ToBeLoaded.AddObject(FileList[i], TObject(Pointer(i)));
          ImageIndex := 0;
        end;
        SubItems.Add(FileName);
      end;
    end;
  finally
    ContentList.Free;
    FileList.Free;
    // ToBeLoaded is freed by the thread
  end;
  TPicLoaderThread.Create(False, ToBeLoaded, ilMapIcons.Width, ilMapIcons.Height);
end;

procedure TfmController.UpdateTokenList;
var
  NodeData: TTokenNodeData;

  procedure ParseDir(Dir: string; ParentNode: TTreeNode);
  var
    FileList, DirList, ContentList: TStringList;
    tmp: string;
    i: Integer;
    tmpNode: TTreeNode;
    title: string;
  begin
    if DirectoryExists(Dir) then
    begin
      FileList := FindAllFiles(Dir, PicFilterStr, False);
      DirList := FindAllDirectories(Dir, False);
      tmp := DirList.CommaText;
      for i := 0 to FileList.Count - 1 do
      begin
        title := ExtractFilename(FileList[i]); 
        NodeData := TTokenNodeData.Create;
        NodeData.TokenType := ttCharacter;
        if FTokenLib.IndexOfName(FileList[i]) >= 0 then
        begin
          ContentList := TStringList.Create;
          try
            ContentList.Delimiter := '|';
            ContentList.StrictDelimiter := True;
            ContentList.DelimitedText := FTokenLib.Values[FileList[i]];
            if Length(ContentList[0]) > 0 then
              title := ContentList[0];

            NodeData.Name              := title;
            NodeData.BaseInitiative    := StrToIntDef(ContentList[1], 0);
            NodeData.DefaultWidth      := StrToIntDef(ContentList[2], Round(FGridData.GridSizeX));
            NodeData.DefaultHeight     := StrToIntDef(ContentList[3], Round(FGridData.GridSizeY));
            NodeData.DefaultGridSlotsX := StrToIntDef(ContentList[4], 1);
            NodeData.DefaultGridSlotsY := StrToIntDef(ContentList[5], 1);
            NodeData.DefaultAngle      := -DegToRad(StrToFloatDef(ContentList[6], 0.0));
          finally
            ContentList.Free;
          end;
        end
        else
        begin
          NodeData.Name := title;
          NodeData.BaseInitiative := 0;
          NodeData.DefaultWidth := Round(FGridData.GridSizeX);
          NodeData.DefaultHeight := Round(FGridData.GridSizeY);
          NodeData.DefaultGridSlotsX := 1;
          NodeData.DefaultGridSlotsY := 1;
          NodeData.DefaultAngle := 0.0;
        end;

        with tvTokens.Items.AddChild(ParentNode, title) do
        begin
          Data := NodeData;
          TTokenNodeData(data).FullPath := FileList[i];
          // show thumbnail in treeview?
        end;
      end;

      for i := 0 to DirList.Count - 1 do
      begin
        tmp := DirList[i];
        tmp := Copy(tmp, Length(IncludeTrailingPathDelimiter(ExtractFileDir(DirList[i]))) + 1, Length(tmp));
        tmpNode := tvTokens.Items.AddChild(ParentNode, tmp);
        ParseDir(DirList[i], tmpNode);
      end;

      FileList.Free;
      DirList.Free;
    end;
  end;

begin
  tvTokens.BeginUpdate;
  tvTokens.Items.Clear;
  ParseDir(FTokenDir, nil);
  with tvTokens.Items.AddChild(nil, GetString(LangStrings.LanguageID, 'ControllerTokenRangeIndicator')) do
  begin
    NodeData := TTokenNodeData.Create;
    NodeData.TokenType := ttRange;
    Data := NodeData;
  end;                           
  with tvTokens.Items.AddChild(nil, GetString(LangStrings.LanguageID, 'ControllerTokenText')) do
  begin
    NodeData := TTokenNodeData.Create;
    NodeData.TokenType := ttText;
    Data := NodeData;
  end;
  with tvTokens.Items.AddChild(nil, GetString(LangStrings.LanguageID, 'ControllerTokenLight')) do
  begin
    NodeData := TTokenNodeData.Create;
    NodeData.TokenType := ttLight;
    Data := NodeData;
  end;
  tvTokens.EndUpdate;
end;

procedure TfmController.UpdateOverlayList;
var
  i: Integer;
  FilePath: string;
  ContentList: TStringList;
  {FullPic, ScaledPic: TBGRABitmap;
  vWidth, vHeight: Integer;}
begin
  ContentList := TStringList.Create;
  try
    ContentList.Delimiter := '|';
    ContentList.StrictDelimiter := True;

    for i := 0 to FOverlayLib.Count - 1 do
    begin
      FilePath := FOverlayLib.Names[i];
      FOverlayLib.Objects[i] := nil;
      if FileExists(FilePath) then
      begin
        ContentList.DelimitedText := FOverlayLib.Values[FilePath];
        if ContentList.Count = 3 then
        begin
          vWidth := StrToIntDef(ContentList[1], 32);
          vHeight := StrToIntDef(ContentList[2], 32);
          {FullPic := TBGRABitmap.Create(FilePath, True);
          try
            ScaledPic := FullPic.Resample(vWidth, vHeight);
            FOverlayLib.Objects[i] := ScaledPic;
          finally
            FullPic.Free;
          end;}
        end;
      end;
    end;

  finally
    ContentList.Free;
  end;
end;

function TfmController.GetToken(idx: Integer): TToken;
begin
  Result := nil;
  if (idx >= 0) and (idx < FTokenList.Count) then
    Result := TToken(FTokenList.Items[idx]);
end;

function TfmController.GetTokenCount: Integer;
begin
  Result := FTokenList.Count;
end;

function TfmController.GetOverlay(idx: Integer): TBGRABitmap;
begin
  Result := nil;
  if (idx >= 0) and (idx < FOverlayLib.Count) then
  begin
    if FileExists(FOverlayLib.Names[idx]) then
      Result := TBGRABitmap.Create(FOverlayLib.Names[idx]);
  end;
  //  Result := TBGRABitmap(FOverlayLib.Objects[idx]);
end;

procedure TfmController.RemoveToken(token: TToken);
begin
  FTokenList.Remove(token);
  pbViewPort.Invalidate;
  fmDisplay.Invalidate;
end;


procedure TfmController.TokenToFront(token: TToken);
var CurIdx: Integer;
begin
  CurIdx := FTokenList.IndexOf(token);
  if CurIdx >= 0 then
    FTokenList.Move(CurIdx, FTokenList.Count - 1);
end;

procedure TfmController.TokenToBack(token: TToken); 
var CurIdx: Integer;
begin
  CurIdx := FTokenList.IndexOf(token);
  if CurIdx >= 0 then
    FTokenList.Move(CurIdx, 0);
end;

function TfmController.GetInitiative(idx: Integer): TPicture;
begin
  Result := nil;
  if (idx >= 0) and (idx < FInitiativePicList.Count) then
    Result := TPicture(FInitiativePicList.Items[idx]);
end;

function TfmController.GetInitiativeNum(idx: Integer): Integer;
begin
  Result := 0;
  if (idx >= 0) and (idx < FInitiativeNumList.Count) then
    Result := Integer(FInitiativeNumList.Items[idx]);
end;

function TfmController.GetInitiativeCount: Integer;
begin
  Result := FInitiativePicList.Count;
end;

function TfmController.GetCurInitiativeIdx: Integer;
begin
  Result := FCurInitiativeIndex;
end;

{procedure TfmController.SnapAllTokensToGrid;
var
  i: Integer;
  CurToken: TToken;
begin
  if FSnapTokensToGrid then
  begin
    for i := 0 to FTokenlist.Count - 1 do
    begin
      CurToken := TToken(FTokenList[i]);
      CurToken.XPos := CurToken.XEndPos - Round(FOldGridOffsetX + FGridOffsetX);
      CurToken.YPos := CurToken.YEndPos - Round(FOldGridOffsetY + FGridOffsetY);
      CurToken.SnapToGrid(FGridSizeX, FGridSizeY, FGridOffsetX, FGridOffsetY, FGridType);
    end;
  end;
end;}

procedure TfmController.SnapTokenToGrid(Token: TToken);
begin
  if Assigned(Token) and FSnapTokensToGrid then
    Token.SnapToGrid(FGridData);
end;

procedure TfmController.SaveLibraryData;
begin
  FMapLib.SaveToFile(MAPLIBFILE);
  FTokenLib.SaveToFile(TOKENLIBFILE);
  FOverlayLib.SaveToFile(OVERLAYLIBFILE);

  UpdateMapList; // Maybe change just names here?
  UpdateTokenList;
  UpdateOverlayList;
end;

procedure TfmController.AddToInitiative(pName, Path: string; Num, Value: Integer);
var
  tmpItem: TListItem;
begin
  tmpItem := lvInitiative.Items.Add;
  tmpItem.Caption := '';
  tmpItem.SubItems.Add(pName);
  tmpItem.SubItems.Add(IntToStr(Value));
  tmpItem.SubItems.Add(Path);
  tmpItem.Data := Pointer(Num);
end;

function TfmController.GetThumbnailName(filename: string): string;
begin
  Result := FThumbnailDir + 't_' + ExtractFileName(filename);
  Result := ChangeFileExt(Result, '.jpg');
end;

procedure TfmController.UpdateViewport;
var
  DisplayMapWidth, DisplayMapHeight: Integer;
begin
  DisplayMapWidth := fmDisplay.ClientWidth - HMARGIN - HMARGIN - PORTRAITWIDTH - HMARGIN;
  DisplayMapHeight := fmDisplay.ClientHeight - VMARGIN - VMARGIN;

  if Round(pbViewPort.Width / DisplayMapWidth * DisplayMapHeight) <= pbViewPort.Height then
  begin                                                 
    FDisplayScale := pbViewPort.Width / DisplayMapWidth;
    FViewRectWidth := pbViewPort.Width;
    FViewRectHeight := Round(FDisplayScale * DisplayMapHeight);
  end
  else
  begin
    FDisplayScale := pbViewPort.Height / DisplayMapHeight;
    FViewRectWidth := Round(FDisplayScale * DisplayMapWidth);
    FViewRectHeight := pbViewPort.Height;
  end;
  
  FViewRectMaxXOffset := 0;   
  FViewRectMaxYOffset := 0;
  if Assigned(FMapPic) then
  begin
    FViewRectMaxXOffset := Max(0, Round(FMapPic.Width * FDisplayScale * FZoomFactor) - FViewRectWidth);
    FViewRectMaxYOffset := Max(0, Round(FMapPic.Height * FDisplayScale * FZoomFactor) - FViewRectHeight);
  end;
  FViewRectXOffset := EnsureRange(FViewRectXOffset, 0, FViewRectMaxXOffset);
  FViewRectYOffset := EnsureRange(FViewRectYOffset, 0, FViewRectMaxYOffset);
  fmDisplay.MapOffsetX := Round(FViewRectXOffset / FDisplayScale);
  fmDisplay.MapOffsetY := Round(FViewRectYOffset / FDisplayScale);

  pbViewPort.Invalidate;
end;

procedure TfmController.bFullscreenClick(Sender: TObject);
begin
  if tbFullscreen.Down then
  begin
    fmDisplay.WindowState := wsFullScreen;
    fmDisplay.BorderStyle := bsNone;
  end
  else
  begin
    fmDisplay.WindowState := wsNormal;
    fmDisplay.BorderStyle := bsSizeable;
  end;     
  fmDisplay.FormResize(fmDisplay);
end;

procedure TfmController.bRefreshMapsClick(Sender: TObject);
begin
  UpdateMapList;
end;

procedure TfmController.bResetZoomClick(Sender: TObject);
begin
  tbMapZoom.Position := 100;
end;

procedure TfmController.FormCreate(Sender: TObject);
var
  LangID, FallbackLangID, LangName: string;
begin
  FMapPic := nil;
  FTokenList := TList.Create;
  FInitiativePicList := TList.Create;
  FInitiativeNumList := TList.Create;
  FAppSettings := TIniFile.Create('Settings.ini', [ifoWriteStringBoolean]);
  pcMain.ActivePage := tsController;
  FLastClicked := Point(-1, -1);
  FLastLastClicked := Point(-1, -1);
  FWallManager := TWallManager.Create;

  // Load settings
  FMapDir := FAppSettings.ReadString('Settings', 'MapDir', 'Content\Maps\');
  FTokenDir := FAppSettings.ReadString('Settings', 'TokenDir', 'Content\Tokens\');
  FOverlayDir := FAppSettings.ReadString('Settings', 'OverlayDir', 'Content\Overlays\');
  FInitiativeDesc := StrToBoolDef(FAppSettings.ReadString('Settings', 'InitiativeDesc', 'true'), True);
  FTokensStartInvisible := StrToBoolDef(FAppSettings.ReadString('Settings', 'TokensStartInvisible', 'true'), True);
  FTokenRotationStyle := TTokenRotationStyle(FAppSettings.ReadInteger('Settings', 'TokenRotationStyle', 0));

  FThumbnailDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Thumbnails\';
  // Maybe use Appdata instead?
  if not DirectoryExists(FThumbnailDir) then
    MkDir(FThumbnailDir);

  // Set language
  LangName := 'English';
  GetLanguageIDs(LangID, FallbackLangID);
  if SameText(FallbackLangID, 'de') then
    LangName := 'Deutsch';
  LanguageID := FAppSettings.ReadString('Settings', 'Language', LangName);

  Notes.SetLanguage(LanguageID);

  // Settings for the note module
  pcNotesMain.ActivePage := tsDisplay;
  pcNotesMain.ShowTabs := False;
  FNotesList := TEntryList.Create;
  FNotesSaved := True;
  FHistoryList := TStringList.Create;
  FHistoryIdx := 0;
  LoadHTML('Main');
  FHistoryList.Add('Main');
  UpdateHistoryButtons;

  FIsDragging := False;
  FIsDraggingToken := False;
  FIsRotatingToken := False;
  FZoomFactor := 1;
  FGridData.GridSizeX := 100;
  FGridData.GridSizeY := 100;
  FGridData.GridColor := clSilver;
  FGridData.GridAlpha := 255;
  FShowMap := False;
  FShowGrid := True;
  FGridData.GridType := gtRect;
  FShowMarker := False;
  FShowTokens := True;
  FSnapTokensToGrid := False;
  FCombatMode := False;

  // Load / create library files
  FMapLib := TStringList.Create;
  if FileExists(MAPLIBFILE) then
    FMapLib.LoadFromFile(MAPLIBFILE)
  else
    FMapLib.SaveToFile(MAPLIBFILE);
  FTokenLib := TStringList.Create;
  if FileExists(TOKENLIBFILE) then
    FTokenLib.LoadFromFile(TOKENLIBFILE)
  else
    FTokenlib.SaveToFile(TOKENLIBFILE);
  FOverlayLib := TStringList.Create;
  if FileExists(OVERLAYLIBFILE) then
    FOverlayLib.LoadFromFile(OVERLAYLIBFILE)
  else
    FOverlayLib.SaveToFile(OVERLAYLIBFILE);
end;

procedure TfmController.FormDestroy(Sender: TObject);
begin
  if Assigned(FMapPic) then
    FMapPic.Free;
  FTokenList.Free;
  FInitiativePicList.Free;
  FInitiativeNumList.Free;
  FAppSettings.WriteString('Settings', 'MapDir', FMapDir);
  FAppSettings.WriteString('Settings', 'TokenDir', FTokenDir);
  FAppSettings.WriteString('Settings', 'OverlayDir', FOverlayDir);
  FAppSettings.WriteString('Settings', 'InitiativeDesc', BoolToStr(FInitiativeDesc));
  FAppSettings.WriteString('Settings', 'TokensStartInvisible', BoolToStr(FTokensStartInvisible)); 
  FAppSettings.WriteInteger('Settings', 'TokenRotationStyle', Ord(FTokenRotatioNStyle));
  FAppSettings.UpdateFile;
  FAppSettings.Free;
  FWallManager.Free;
  FMapLib.Free;
  FTokenLib.Free;
  FOverlayLib.Free;
  // Notes module
  FNotesList.Free;
  FHistoryList.Free;
end;

procedure TfmController.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 122 then // VK_F11
  begin
    tbFullScreen.Down := not tbFullScreen.Down;
    bFullScreenClick(Sender);
  end
  else if Key = 32 then // VK_SPACE
  begin
    if FCombatMode then
      tbNextCombatantClick(self);
  end;
end;

procedure TfmController.FormResize(Sender: TObject);
begin
  UpdateViewport;
end;

{ TPicLoaderThread }

constructor TPicLoaderThread.Create(CreateSuspended: Boolean; FileList: TStringList; Width, Height: Integer);
begin
  FFileList := FileList;
  Thumbnail := TBitmap.Create;
  Thumbnail.Width := Width;
  Thumbnail.Height := Height;
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

procedure TPicLoaderThread.SetThumbnail;
var tmpItem: TListItem;
begin
  if (CurIdx < 0) or (CurIdx > fmController.lvMaps.Items.Count - 1) then
    Exit;
  tmpItem := fmController.lvMaps.Items[CurIdx];
  tmpItem.ImageIndex := fmController.ilMapIcons.Add(Thumbnail, nil);
  //tmpItem.StateIndex:= 0;
end;

procedure TPicLoaderThread.Execute;
var
  i: Integer;
  FullPic, ScaledPic: TBGRABitmap;
  ThumbnailName: string;
begin
  i := 0;
  //FullPic := TBGRABitmap.Create(0, 0);
  FullPic := nil;
  while (not Terminated) and (i < FFileList.Count) do
  begin
    try
      ThumbnailName := fmController.GetThumbnailName(FFileList[i]);
      //FullPic.LoadFromFile(FFileList[i]);
      if Assigned(FullPic) then
        FreeAndNil(FullPic);
      FullPic := GetMapImage(FFileList[i]);
    except

    end;
    try
      // Create thumbnail, save for next use
      ScaledPic := FullPic.Resample(Thumbnail.Width, Thumbnail.Height, rmSimpleStretch);
      ScaledPic.Draw(Thumbnail.Canvas, Rect(0, 0, Thumbnail.Width, Thumbnail.Height));
      ScaledPic.SaveToFile(ThumbnailName);
    finally
      ScaledPic.Free;
    end;
    CurIdx := LongInt(Pointer(FFileList.Objects[i]));
    Synchronize(@SetThumbnail);

    Inc(i);
  end;
  if Assigned(FullPic) then
    FullPic.Free;
  Thumbnail.Free;
  FFileList.Free;
end;

{ Notes module }

// I wanted to do this in a separate frame, but apparently those are broken in
// Freepascal, too. This does not bode well for the planned plugin interface.

// Handle back / forward-mouse buttons
{procedure TfmController.WMAppCommand(var Msg: TMessage);
begin
  case GET_APPCOMMAND_LPARAM(Msg.LParam) of
    APPCOMMAND_BROWSER_BACKWARD:
    begin
      tbHistoryBackClick(self);
      Msg.Result := 1;
    end;
    APPCOMMAND_BROWSER_FORWARD:
    begin

    end;
  end;
end; }

procedure TfmController.bOkCategoryClick(Sender: TObject);
begin
  FNotesList.Categories.AddStrings(lbCategories.Items, True);
  FNotesSaved := False;
  pcNotesMain.ActivePage := tsDisplay;
end;

procedure TfmController.bOKEntryListClick(Sender: TObject);
var
  i: Integer;
  tmpEntry: TNoteEntry;
begin
  // Check for new entries
  for i := 0 to lbEntryList.Items.Count - 1 do
  begin
    if not FNotesList.HasEntry(lbEntryList.Items[i]) then
    begin
      tmpEntry := TNoteEntry.Create;
      tmpEntry.EntryName := lbEntryList.Items[i];
      tmpEntry.Date := Now;
      FNotesList.AddEntry(tmpEntry); 
      FNotesSaved := False;
    end;
  end;
  // Check for deleted entries
  for i := FNotesList.EntryCount - 1 downto 0 do
  begin
    tmpEntry := FNotesList.GetEntry(i);
    if lbEntryList.Items.IndexOf(tmpEntry.EntryName) < 0 then
    begin
      FNotesList.DeleteEntry(tmpEntry.EntryName);
      tmpEntry.Free;
      FNotesSaved := False;
    end;
  end;
  LoadHTML('Main');
  pcNotesMain.ActivePage := tsDisplay;
end;

procedure TfmController.bCancelCategoryClick(Sender: TObject);
begin
  pcNotesMain.ActivePage := tsDisplay;
end;

procedure TfmController.bAddCategoryClick(Sender: TObject);
begin
  if (eNewCategory.Text <> '') and (lbCategories.Items.IndexOf(eNewCategory.Text) < 0) then
  begin
    lbCategories.Items.Add(eNewCategory.Text);
    FNotesSaved := False;
  end;
end;

procedure TfmController.bAddAnnotationClick(Sender: TObject);
var tmpItem: TListItem;
begin
  tmpItem := lvAnnotations.Items.Add;
  tmpItem.Caption := DateTimeToStr(Now);
  tmpItem.SubItems.Add('');
  tmpItem.SubItems.Add(BoolToStr(False));
  dtpAnnotationTimestamp.Datetime := Now;
  mAnnotationContent.Text := '';
  FNotesSaved := False;
end;

procedure TfmController.bAddEntryClick(Sender: TObject);
begin
  if (eNewEntry.Text <> '') and (lbEntryList.Items.IndexOf(eNewEntry.Text) < 0) then
  begin
    eNewEntry.Text := ReplaceStr(eNewEntry.Text, '|', '');
    lbEntryList.Items.Add(eNewEntry.Text);
    FNotesSaved := False;
  end;
end;

procedure TfmController.bDeleteAnnotationClick(Sender: TObject);
begin
  if Assigned(lvAnnotations.Selected) then
  begin
    lvAnnotations.Items.Delete(lvAnnotations.ItemIndex);
    FNotesSaved := False;
  end;
end;

procedure TfmController.bDeleteCategoryClick(Sender: TObject);
begin
  if (lbCategories.ItemIndex >= 0) and (lbCategories.ItemIndex < lbCategories.Items.Count) then
  begin
    lbCategories.Items.Delete(lbCategories.ItemIndex);
    FNotesSaved := False;
  end;
end;

procedure TfmController.bDeleteEntryClick(Sender: TObject);
begin
  if lbEntryList.ItemIndex >= 0 then
  begin
    lbEntryList.Items.Delete(lbEntryList.ItemIndex);
    FNotesSaved := False;
  end;
end;

procedure TfmController.bEditAnnotationClick(Sender: TObject);
var tmpItem: TListItem;
begin
  tmpItem := lvAnnotations.Selected;
  if Assigned(tmpItem) then
  begin
    tmpItem.Caption := DateTimeToStr(dtpAnnotationTimestamp.DateTime);
    tmpItem.SubItems[0] := mAnnotationContent.Text;
    tmpItem.SubItems[1] := BoolToStr(cbAnnotationDMOnly.Checked);
    FNotesSaved := False;
  end;
end;

procedure TfmController.bEntryCancelClick(Sender: TObject);
begin
  FEditedEntry := nil;
  pcNotesMain.ActivePage := tsDisplay;
end;

procedure TfmController.bEntryOKClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FEditedEntry) then
  begin
    FEditedEntry.Content := mEntryContent.Text;
    FEditedEntry.Date := dtpTimestamp.DateTime;
    FEditedEntry.DMOnly := cbDMOnly.Checked;
    FEditedEntry.Category := cbCategory.Text;
    eEntryName.Text := ReplaceStr(eEntryName.Text, '|', '');
    FEditedEntry.EntryName := eEntryName.Text;

    FEditedEntry.ClearAnnotations;
    for i := 0 to lvAnnotations.Items.Count - 1 do
    begin
      FEditedEntry.AddAnnotation(StrToDateTime(lvAnnotations.Items[i].Caption), lvAnnotations.Items[i].SubItems[0], StrToBool(lvAnnotations.Items[i].SubItems[1]));
    end;

    LoadHTML(FEditedEntry.EntryName);
  end;
  FEditedEntry := nil;
  FNotesSaved := False;
  pcNotesMain.ActivePage := tsDisplay;
end;
 
procedure TfmController.hvNotesDisplayHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  Handled := True;
  if ExecRegExpr('(http(s)?:\/\/.)?(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)', SRC) then
  begin
    // Weblinks
    if MessageDlg(GetString(LangStrings.LanguageID, 'NotesOpenExternalLink'),
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      OpenURL(SRC);
  end
  else
  begin
    if not StartsStr('edit|', SRC) then
    begin
      AddToHistory(SRC);
    end;

    UpdateHistoryButtons;
    LoadHtml(SRC);
  end;
end;

procedure TfmController.hvNotesDisplayImageRequest(Sender: TObject;
  const SRC: ThtString; var Stream: TStream);
begin
  if FileExists(SRC) then
  begin
    try
      Stream := TFileStream.Create(Src, fmOpenRead);
    except
      Stream := nil;
    end;
  end
  else
    Stream := nil;
end;

procedure TfmController.lvAnnotationsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(Item) then
  begin
    dtpAnnotationTimestamp.DateTime := StrToDateTime(Item.Caption);
    mAnnotationContent.Text := Item.SubItems[0];
    cbAnnotationDMOnly.Checked := StrToBool(Item.SubItems[1]);
  end
  else if not Selected then
  begin
    dtpAnnotationTimestamp.DateTime := Now;
    mAnnotationContent.Text := '';
    cbAnnotationDMOnly.Checked := False;
  end;
end;

procedure TfmController.LoadHTML(Entry: string);
var
  fs: TStream;
  str: string;
  idx: Integer;
  tmpEntry: TNoteEntry;
  tmpAnnotation: TNoteAnnotation;
  tmpItem: TListItem;
begin
  if SameText(Entry, 'Main') then
    fs := FNotesList.EntryListToHTML(False)
  else if SameText(Entry, 'Categories') then
    fs := FNotesList.CategoryListToHTML
  else if StartsStr('edit|', Entry) then
  begin
    // Show Editor for selected entry
    fs := nil;
    if SameText(Entry, 'edit|categories') then
    begin
      lbCategories.Items.AddStrings(FNotesList.Categories, True);
      eNewCategory.Text := '';
      pcNotesMain.ActivePage := tsCategoryEditor;
    end
    else if SameText(Entry, 'edit|entrylist') then
    begin
      lbEntryList.Items.Clear;
      for idx := 0 to FNotesList.EntryCount - 1 do
      begin
        lbEntryList.Items.Add(FNotesList.GetEntry(idx).EntryName);
      end;
      eNewEntry.Text := '';

      pcNotesMain.ActivePage := tsEntryListEditor;
    end
    else
    begin
      str := Copy(Entry, Length('edit|') + 1, Length(Entry));
      tmpEntry := FNotesList.GetEntry(str);
      if Assigned(tmpEntry) then
      begin
        eEntryName.Text := tmpEntry.EntryName;
        cbDMOnly.Checked := tmpEntry.DMOnly;
        dtpTimeStamp.DateTime := tmpEntry.Date;
        cbCategory.Items.AddStrings(FNotesList.Categories, True);
        idx := cbCategory.Items.IndexOf(tmpEntry.Category);
        if idx >= 0 then
          cbCategory.ItemIndex := idx
        else
          cbCategory.Text := '';
        mEntryContent.Text := tmpEntry.Content;

        // Set Annotation data
        mAnnotationContent.Lines.Clear;
        dtpAnnotationTimestamp.Date := Now;
        lvAnnotations.Items.Clear;
        for idx := 0 to tmpEntry.GetAnnotationCount - 1 do
        begin
          tmpAnnotation := tmpEntry.GetAnnotation(idx);
          tmpItem := lvAnnotations.Items.Add;
          tmpItem.Caption := DateTimeToStr(tmpAnnotation.Date);
          tmpItem.SubItems.Add(tmpAnnotation.Content);
          tmpItem.SubItems.Add(BoolToStr(tmpAnnotation.DMOnly));
        end;

        FEditedEntry := tmpEntry;
        pcNotesMain.ActivePage := tsEntryEditor;
      end;
    end;
  end
  else if StartsStr('cat|', Entry) then
  begin
    // Show list of all entries of that category - TBI
    str := Copy(Entry, Length('cat|') + 1, Length(Entry));
    fs := FNotesList.EntriesByCategoryToHTML(str, False);
  end
  else if FNotesList.HasEntry(Entry) then
    fs := FNotesList.EntryToHTML(Entry, False)
  else
    fs := FNotesList.NotFoundToHTML;
  try
    if Assigned(fs) then
      hvNotesDisplay.LoadFromStream(fs);
  finally
    if Assigned(fs) then
      fs.Free;
  end;
end;

procedure TfmController.AddToHistory(Entry: string);
begin
  while FHistoryIdx < FHistoryList.Count - 1 do
    FHistoryList.Delete(FHistoryList.Count - 1);

  FHistoryList.Add(Entry);
  Inc(FHistoryIdx);
  UpdateHistoryButtons;
end;

procedure TfmController.UpdateHistoryButtons;
begin
  tbHistoryBack.Enabled := FHistoryIdx > 0;
  tbHistoryForward.Enabled := FHistoryIdx < FHistoryList.Count - 1;
end;

procedure TfmController.tbLoadNotesClick(Sender: TObject);
var
  DlgResult: TModalResult;
  DoLoad: Boolean;
begin
  DoLoad := FNotesSaved;
  if not FNotesSaved then
  begin
    DlgResult := MessageDlg(GetString(LangStrings.LanguageID, 'NotesUnsavedAlert'),
                            mtConfirmation,
                            [mbYes, mbNo, mbCancel],
                            0);
    if DlgResult <> mrCancel then
      DoLoad := True;
    if DlgResult = mrYes then
    begin
      if sdSaveSession.Execute then
      begin
        FNotesList.SaveToFile(sdSaveSession.FileName);
        FNotesSaved := True;
      end
      else
        DoLoad := False;
    end;

  end;
  if DoLoad and odLoadSession.Execute then
  begin
    FNotesList.LoadFromFile(odLoadSession.FileName);    
    FHistoryIdx := 0;
    LoadHTML('Main');
    FHistoryList.Clear;
    FHistoryList.Add('Main');
    UpdateHistoryButtons; 
    FNotesSaved := True;
  end;
end;

procedure TfmController.tbSaveNotesClick(Sender: TObject);
begin
  if sdSaveSession.Execute then
  begin
    FNotesList.SaveToFile(sdSaveSession.FileName);
    FNotesSaved := True;
  end;
end;

procedure TfmController.tbHistoryBackClick(Sender: TObject);
begin
  FHistoryIdx := Max(0, FHistoryIdx - 1);
  LoadHTML(FHistoryList[FHistoryIdx]);
  pcNotesMain.ActivePage := tsDisplay;
  UpdateHistoryButtons;
end;

procedure TfmController.tbHistoryForwardClick(Sender: TObject);
begin
  FHistoryIdx := Min(FHistoryIdx + 1, FHistoryList.Count - 1);
  LoadHTML(FHistoryList[FHistoryIdx]); 
  pcNotesMain.ActivePage := tsDisplay;
  UpdateHistoryButtons;
end;
 
procedure TfmController.tbExportForDMClick(Sender: TObject);
var
  files: TStringList;
  DoExport: Boolean;
begin
  if sddExportDir.Execute then
  begin
    DoExport := True;
    files := FindAllFiles(IncludeTrailingPathDelimiter(sddExportDir.FileName), '*.html;*.css');
    if files.Count > 0 then
      DoExport := MessageDlg(GetString(LangStrings.LanguageID, 'NotesExportOverwrite'),
                             mtConfirmation,
                             [mbOk, mbCancel], 0) = mrOk;
    files.Free;
    if DoExport then
      FNotesList.ExportAll(sddExportDir.FileName, TComponent(Sender).Tag = 1);
  end;
end;

procedure TfmController.eEntryNameKeyPress(Sender: TObject; var Key: char);
begin
  if Key = '|' then
    Key := #0;
end;

procedure TfmController.FormCloseQuery(Sender: TObject; var CanClose: Boolean); 
var
  DlgResult: TModalResult;
begin
  CanClose := FNotesSaved;
  if not FNotesSaved then
  begin
    DlgResult := MessageDlg(GetString(LangStrings.LanguageID, 'NotesUnsavedAlert'),
                            mtConfirmation,
                            [mbYes, mbNo, mbCancel],
                            0);
    if DlgResult <> mrCancel then
      CanClose := True;
    if DlgResult = mrYes then
    begin
      if sdSaveSession.Execute then
      begin
        FNotesList.SaveToFile(sdSaveSession.FileName);
        FNotesSaved := True;
      end
      else
        CanClose := False;
    end;
  end;
end;



end.

