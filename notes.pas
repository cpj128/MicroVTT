unit Notes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type

  TNoteAnnotation = class(TPersistent)
  private
    FDate: TDateTime;
    FDMOnly: Boolean;
    FContent: string;
  public
    procedure LoadFromXML(node: TDOMNode);
    function ToHtml(ForPlayers: Boolean): string;
    function SaveToXML(doc: TXMLDocument): TDOMNode;
    property Date: TDateTime read FDate write FDate;
    property DMOnly: Boolean read FDMOnly write FDMOnly;
    property Content: string read FContent write FContent;
  end;

  TNoteEntry = class(TPersistent)
  private
    FDate: TDateTime;
    FAnnotations: TList;
    FName: string;
    FContent: string;
    FCategory: string;
    FDMOnly: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXML(node: TDOMNode);
    function ToHtml(ForPlayers: Boolean): string;
    function SaveToXML(doc: TXMLDocument): TDOMNode;
    function GetAnnotation(idx: Integer): TNoteAnnotation;
    function GetAnnotationCount: Integer;
    procedure ClearAnnotations;
    procedure AddAnnotation(Date: TDateTime; Content: string; DMOnly: Boolean);
    property EntryName: string read FName write FName;
    property Date: TDateTime read FDate write FDate;
    property Content: string read FContent write FContent;
    property Category: string read FCategory write FCategory;
    property DMOnly: Boolean read FDMOnly write FDMOnly;
  end;

  TEntryList = class(TPersistent)
  private
    FEntries: TList;
    FCategories: TStringList;
    function GetSidebar(EditKey: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(filename: string);
    procedure SaveToFile(filename: string);
    function EntryToHTML(Name: string; ForPlayers: Boolean): TStream;
    function EntryListToHTML(ForPlayers: Boolean): TStream;
    function EntriesByCategoryToHTML(Category: string; ForPlayers: Boolean): TStream;
    function CategoryListToHTML: TStream;
    function NotFoundToHTML: TStream;
    function GetEntryName(idx: Integer): string;
    function GetEntry(idx: Integer): TNoteEntry; overload;
    function GetEntry(Name: string): TNoteEntry; overload;
    function EntryCount: Integer;
    function HasEntry(Name: string): Boolean;
    procedure ClearEntries;
    procedure AddEntry(entry: TNoteEntry);
    procedure DeleteEntry(name: string);
    property Categories: TStringList read FCategories;
  end;

  procedure SetLanguage(LangID: string);

implementation

uses
  LangStrings,
  RegExpr,
  StrUtils,
  XMLRead,
  XMLWrite;

var
  strSidebarList,
  strSidebarEdit,
  strSidebarCategories,
  strNoEntries,
  strEntryNotFound,
  strNone: string;

procedure SetLanguage(LangID: string);
begin
  strSidebarList := GetString(LangID, 'NotesSidebarList');
  strSidebarEdit := GetString(LangID, 'NotesSidebarEdit');
  strSidebarCategories := GetString(LangID, 'NotesSidebarCategories');
  strNoEntries := GetString(LangID, 'NotesContentNoEntries');
  strEntryNotFound := GetString(LangID, 'NotesContentEntryNotFound');
  strNone := GetString(LangID, 'NotesCategoryNone');
end;

{ TNoteAnnotation }

procedure TNoteAnnotation.LoadFromXML(node: TDOMNode);
var
  i: Integer;
begin
  if not SameText(node.nodeName, 'Annotation') then
    Exit;
  FDMOnly := False;
  for i := 0 to node.Attributes.length - 1 do
  begin
    if SameText(node.Attributes.Item[i].NodeName, 'Timestamp') then
      FDate := StrToDateTime(node.Attributes.Item[i].NodeValue)
    else if SameText(node.Attributes.Item[i].NodeName, 'DMOnly') then
      FDMOnly := True;
  end;
  FContent := node.FirstChild.NodeValue;
end;

function TNoteAnnotation.ToHtml(ForPlayers: Boolean): string;
begin
  if ForPlayers and FDMOnly then
    Exit('');
  Result := '<p>' + FContent + '</p>';
  if FDate <> 0 then
    Result := '<p class="timestamp">' + FormatDateTime('ddddd', FDate) + '</p>' + Result;
  Result := '<div class="annotation' + IfThen(FDMOnly, ' dmonly', '') + '">' + Result + '</div>';
end;

function TNoteAnnotation.SaveToXML(doc: TXMLDocument): TDOMNode;
begin
  Result := doc.CreateElement('Annotation');
  if FDMOnly then
    TDOMElement(Result).SetAttribute('DMOnly', 'DMOnly');
    //Result.Attributes['DMOnly'].NodeValue := 'DMOnly';
  //Result.Attributes['Timestamp'].NodeValue := DateTimeToStr(FDate);
  TDOMelement(Result).SetAttribute('Timestamp', DateTimeToStr(FDate));
  Result.AppendChild(Doc.CreateTextNode(FContent));
end;

{ TNoteEntry }

constructor TNoteEntry.Create;
begin
  inherited Create;
  FAnnotations := TList.Create;
end;

destructor TNoteEntry.Destroy;
begin
  ClearAnnotations;
  FAnnotations.Free;
  inherited;
end;

function TNoteEntry.GetAnnotation(idx: Integer): TNoteAnnotation;
begin
  Result := nil;
  if (idx >= 0) and (idx < FAnnotations.Count) then
    Result := TNoteAnnotation(FAnnotations[idx]);
end;

function TNoteEntry.GetAnnotationCount: Integer;
begin
  Result := FAnnotations.Count;
end;

procedure TNoteEntry.ClearAnnotations;
var
  i: Integer;
  tmpAnnotation: TNoteAnnotation;
begin
  for i := FAnnotations.Count - 1 downto 0 do
  begin
    tmpAnnotation := TNoteAnnotation(FAnnotations[i]);
    tmpAnnotation.Free;
    FAnnotations.Delete(i);
  end;
end;

procedure TNoteEntry.AddAnnotation(Date: TDateTime; Content: string; DMOnly: Boolean);
var tmpAnnotation: TNoteAnnotation;
begin
  tmpAnnotation := TNoteAnnotation.Create;
  tmpAnnotation.Date := Date;
  tmpAnnotation.Content := Content;
  tmpAnnotation.DMOnly := DMOnly;
  FAnnotations.Add(tmpAnnotation);
end;

procedure TNoteEntry.LoadFromXML(node: TDOMNode);
var
  ChildNode, AnnotNode: TDOMNode;
  tmpAnnotation: TNoteAnnotation;
  i: Integer;
begin
  if not SameText(node.NodeName, 'Entry') then
    Exit;
  FDMOnly := False;
  for i := 0 to node.Attributes.length - 1 do
  begin
    if SameText(node.Attributes.Item[i].NodeName, 'timestamp') then
      FDate := StrToDateTime(node.Attributes.Item[i].NodeValue)
    else if SameText(node.Attributes.Item[i].NodeName, 'dmonly') then
      FDMOnly := True
    else if SameText(node.Attributes.Item[i].NodeName, 'category') then
      FCategory := node.Attributes.Item[i].NodeValue;
  end;
  ChildNode := node.FirstChild;
  while ChildNode <> nil do
  begin
    if SameText(ChildNode.NodeName, 'Name') then
      FName := ChildNode.FirstChild.NodeValue
    else if SameText(ChildNode.NodeName, 'Content') and Assigned(ChildNode.FirstChild) then
    begin
      FContent := ChildNode.FirstChild.NodeValue
    end
    else if SameText(ChildNode.NodeName, 'Annotations') then
    begin
      ClearAnnotations;
      AnnotNode := ChildNode.FirstChild;
      while AnnotNode <> nil do
      begin
        tmpAnnotation := TNoteAnnotation.Create;
        tmpAnnotation.LoadFromXML(AnnotNode);
        FAnnotations.Add(tmpAnnotation);
        AnnotNode := AnnotNode.NextSibling;
      end;
    end;
    ChildNode := ChildNode.NextSibling;
  end;
end;

function TNoteEntry.ToHtml(ForPlayers: Boolean): string;
var
  i: Integer;
  tmpAnnot: TNoteAnnotation;
begin
  Result := '<p class="header">' + FName + '</p>';
  if FDate <> 0 then
    Result := Result + '<p class="timestamp">' + FormatDateTime('ddddd', FDate) + '</p>';
  if FCategory <> '' then
    Result := Result + '<p class="category"><a href="cat|' + FCategory + '">' + FCategory + '</a></p>';
  if not (ForPlayers and FDMOnly) then
    Result := Result + '<p class="entrytext">' + FContent + '</p>';
  for i := 0 to FAnnotations.Count - 1 do
  begin
    tmpAnnot := TNoteAnnotation(FAnnotations[i]);
    Result := Result + tmpAnnot.ToHtml(ForPlayers);
  end;
  Result := '<div class="Entry' + IfThen(FDMOnly, ' dmonly', '') + '">' + Result + '</div>';
end;

function TNoteEntry.SaveToXML(doc: TXMLDocument): TDOMNode;
var
  SubNode: TDOMNode;
  i: Integer;
  tmpAnnot: TNoteAnnotation;
begin
  Result := doc.CreateElement('entry');
  TDOMelement(Result).SetAttribute('timestamp', DateTimeToStr(FDate));
  if FDMOnly then
    TDOMElement(Result).SetAttribute('dmonly', 'dmonly');
  TDOMElement(Result).SetAttribute('category', FCategory);
  SubNode := doc.CreateElement('name');
  SubNode.AppendChild(doc.CreateTextNode(FName));
  Result.AppendChild(SubNode);
  SubNode := doc.CreateELement('content');
  SubNode.AppendChild(doc.CreateTextNode(FContent));
  Result.AppendChild(SubNode);
  SubNode := doc.CreateElement('annotations');
  Result.AppendChild(SubNode);
  for i := 0 to FAnnotations.Count - 1 do
  begin
    tmpAnnot := TNoteAnnotation(FAnnotations[i]);
    SubNode.AppendChild(tmpAnnot.SaveToXML(doc));
  end;
end;

{ TEntryList }

constructor TEntryList.Create;
begin
  inherited;
  FEntries := TList.Create;
  FCategories := TStringList.Create;
end;

destructor TEntryList.Destroy;
begin
  ClearEntries;
  FEntries.Free;
  FCategories.Free;
  inherited;
end;

function TEntryList.GetSidebar(EditKey: string): string;
begin
  Result := '<a href="main">' + strSidebarList + '</a><br />';
  Result := Result + '<a href="categories">' + strSidebarCategories + '</a><br />';
  if EditKey <> '' then
    Result := Result + '<a href="edit|' + EditKey + '">' + strSidebarEdit + '</a><br />';
  Result := '<div class="sidenav">' + Result + '</div>';
end;

procedure TEntryList.LoadFromFile(filename: string);
var
  Doc: TXMLDocument;
  RootNode, SubNode, EntryNode: TDOMNode;
  tmpEntry: TNoteEntry;
  str: string;
begin
  ClearEntries;
  Doc := TXMLDocument.Create;
  try
    ReadXMLFile(Doc, filename);
    RootNode := Doc.DocumentElement;

    SubNode := RootNode.FirstChild; // categories
    str := SubNode.NodeName;
    EntryNode := SubNode.FirstChild;
    while EntryNode <> nil do
    begin
      str := EntryNode.FirstChild.NodeValue;
      FCategories.Add(str);
      EntryNode := EntryNode.NextSibling;
    end;

    SubNode := SubNode.NextSibling; // Entries
    EntryNode := SubNode.FirstChild;
    while EntryNode <> nil do
    begin
      tmpEntry := TNoteEntry.Create;
      tmpEntry.LoadFromXML(EntryNode);
      AddEntry(tmpEntry);
      EntryNode := EntryNode.NextSibling;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TEntryList.SaveToFile(filename: string);
var
  Doc: TXMLDocument;
  RootNode, SubNode, CatNode: TDOMNode;
  i: Integer;
begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('notes');
    Doc.Appendchild(RootNode);
    SubNode := Doc.CreateElement('categories');
    RootNode.AppendChild(SubNode);
    for i := 0 to FCategories.Count - 1 do
    begin
      CatNode := Doc.CreateElement('category');
      CatNode.AppendChild(Doc.CreateTextNode(FCategories[i]));
      SubNode.AppendChild(CatNode);
    end;

    SubNode := Doc.CreateElement('entries');
    RootNode.AppendChild(SubNode);
    for i := 0 to FEntries.Count - 1 do
      SubNode.AppendChild(GetEntry(i).SaveToXML(doc));
    WriteXMLFile(doc, filename);
  finally
    doc.Free;
  end;
end;

function TEntryList.EntryToHTML(Name: string; ForPlayers: Boolean): TStream;
var
  str: string;
  tmpEntry: TNoteEntry;
begin
  tmpEntry := GetEntry(Name);
  if not Assigned(tmpEntry) then
    Exit(TStringStream.Create(''));
  str := tmpEntry.ToHtml(ForPlayers);
  str :=  '<html><head><title>Liste</title><link rel="stylesheet" href="styles.css"></head><body>' +
            GetSidebar(Name) + '<div class="main">' + str + '</div></body></html>';

  // Rewrite a few things to make everything work better
  // Line breaks to <br />-tags
  str := StringReplace(str, #10, '<br />', [rfReplaceAll]);

  // Wikilinks to <a>-tags
  str := ReplaceRegExpr('\[\[([^\]\|]+)\|([^\]\|]+)\]\]', str, '<a href="$1">$2</a>', True);
  str := ReplaceRegExpr('\[\[([^\]]+)\]\]', str, '<a href="$1">$1</a>', True);

  Result := TStringStream.Create(str);
end;

function TEntryList.EntryListToHTML(ForPlayers: Boolean): TStream;
var
  i: Integer;
  tmpEntry: TNoteEntry;
  str: string;
begin
  str := '';
  for i := 0 to FEntries.Count - 1 do
  begin
    tmpEntry := GetEntry(i);
    if Assigned(tmpEntry) and not (ForPlayers and tmpEntry.DMOnly) then
      str := str + '<li><a href="' + tmpEntry.EntryName + '">' + tmpEntry.EntryName + '</a></li>';
  end;
  str := '<html><head><title>Liste</title><link rel="stylesheet" href="styles.css"></head><body>' + GetSidebar('entryList') +
         '<div class="main"><ul>' + str + '</ul></div></body></html>';


  Result := TStringStream.Create(str);
end;

function TEntryList.EntriesByCategoryToHTML(Category: string; ForPlayers: Boolean): TStream;
var
  i: Integer;
  str: string;
  tmpEntry: TNoteEntry;
begin
  str := '';
  for i := 0 to FEntries.Count - 1 do
  begin
    tmpEntry := GetEntry(i);
    if Assigned(tmpEntry) and
       not (ForPlayers and tmpEntry.DMOnly) and
       (SameText(tmpEntry.Category, Category) or (SameText(Category, 'none') and SameText(tmpEntry.Category, ''))) then
      str := str + '<li><a href="' + tmpEntry.EntryName + '">' + tmpEntry.EntryName + '</a></li>';

  end;
  if str = '' then
    str := '<li>' + strNoEntries + '</li>';

  str := '<html><head><title>Kategorien</title><link rel="stylesheet" href="styles.css"></head><body>' + GetSidebar('') +
         '<div class="main"><ul>' + str + '</ul></div></body></html>';

  Result := TStringStream.Create(str);
end;

function TEntryList.CategoryListToHTML: TStream;
var
  i: Integer;
  str: string;
begin
  str := '<li><a href="cat|none">' + strNone + '</a></li>';

  for i := 0 to FCategories.Count - 1 do
    str := str + '<li><a href="cat|' + FCategories[i] + '">' + FCategories[i] + '</a></li>';
  str := '<html><head><title>Kategorien</title><link rel="stylesheet" href="styles.css"></head><body>' + GetSidebar('categories') +
         '<div class="main"><ul>' + str + '</ul></div></body></html>';

  Result := TStringStream.Create(str);
end;

function TEntryList.NotFoundToHTML: TStream;
var str: string;
begin
  str := '<html><head><title>Not found</title><link rel="stylesheet" href="styles.css"></head><body>' + GetSidebar('') +
         '<div class="main">' + strEntryNotFound + '</div></body></html>';
  Result := TStringStream.Create(str);
end;

function TEntryList.GetEntryName(idx: Integer): string;
begin
  Result := '';
  if Assigned(GetEntry(idx)) then
    Result := GetEntry(idx).EntryName;
end;

function TEntryList.GetEntry(idx: Integer): TNoteEntry;
begin
  Result := nil;
  if (idx >= 0) and (idx < FEntries.Count) then
    Result := TNoteEntry(FEntries[idx]);
end;

function TEntryList.GetEntry(Name: string): TNoteEntry;
var i: Integer;
begin
  Result := nil;
  for i := 0 to FEntries.Count - 1 do
    if SameText(GetEntryName(i), Name) then
      Exit(TNoteEntry(FEntries[i]));
end;

function TEntryList.EntryCount: Integer;
begin
  Result := FEntries.Count;
end;

function TEntryList.HasEntry(Name: string): Boolean;
var tmpEntry: TNoteEntry;
begin
  tmpEntry := GetEntry(Name);
  Result := Assigned(tmpEntry);
end;

procedure TEntryList.ClearEntries;
var
  i: Integer;
  tmpEntry: TNoteEntry;
begin
  for i := FEntries.Count - 1 downto 0 do
  begin
    tmpEntry := TNoteEntry(FEntries[i]);
    tmpEntry.Free;
    FEntries.Delete(i);
  end;
end;

procedure TEntryList.AddEntry(entry: TNoteEntry);
begin
  FEntries.Add(entry);
end;

procedure TEntryList.DeleteEntry(name: string);
var tmpEntry: TNoteEntry;
begin
  tmpEntry := GetEntry(name);
  if Assigned(tmpEntry) then
    FEntries.Remove(tmpEntry);
end;


end.

