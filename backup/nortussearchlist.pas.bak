unit NortusSearchList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Buttons, Graphics, DB,
  LCLType, LMessages, Forms, Dialogs, DBGrids, Grids;

type
  { Enumeração de Estilos de Ícones }
  TIconStyle = (isClassic, isModern, isMinimalist, isBold);

  { TNortusSearchList }

  TSearchClickEvent = procedure(Sender: TObject; const SearchText: string) of object;
  TItemSelectedEvent = procedure(Sender: TObject; const KeyValue: Integer) of object;

  TNortusSearchList = class(TCustomPanel)
  private
    FDataSource: TDataSource;
    FDataKeyField: string;
    FDisplayFields: string;
    FSearchFields: string;
    FSelectedKey: Integer;
    FText: string;
    FMinCharsToSearch: Integer;
    FAutoSearch: Boolean;
    FShowValidationMessage: Boolean;
    FGridHeight: Integer;
    FSelectedColor: TColor;
    FSelectedTextColor: TColor;
    FEditHeight: Integer;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FFieldSeparator: string;
    FSearchDebounceInterval: Integer;
    FIconStyle: TIconStyle;

    // Componentes visuais
    FEditPanel: TPanel;
    FEditSearch: TEdit;
    FBtnDropDown: TSpeedButton;
    FBtnSearch: TSpeedButton;
    FBtnClear: TSpeedButton;
    FDBGrid: TDBGrid;
    FSearchTimer: TTimer;

    // Fontes
    FEditFont: TFont;
    FGridFont: TFont;
    FGridTitleFont: TFont;

    // Eventos
    FOnSearchClick: TSearchClickEvent;
    FOnItemSelected: TItemSelectedEvent;
    FOnClear: TNotifyEvent;
    FOnDropDown: TNotifyEvent;

    // Controle interno
    FLastValidText: string;
    FIsSelecting: Boolean;
    FGridOpened: Boolean;
    FOriginalFilter: string;
    FOriginalFiltered: Boolean;
    FOriginalOnFilterRecord: TFilterRecordEvent;
    FCurrentSearchText: string;
    FColumnsCreated: Boolean;
    FArranging: Boolean;
    FFirstArrange: Boolean;

    procedure SetDataSource(AValue: TDataSource);
    procedure SetDataKeyField(AValue: string);
    procedure SetDisplayFields(AValue: string);
    procedure SetSearchFields(AValue: string);
    procedure SetEditFont(AValue: TFont);
    procedure SetGridFont(AValue: TFont);
    procedure SetGridTitleFont(AValue: TFont);
    procedure SetGridHeight(AValue: Integer);
    procedure SetSelectedColor(AValue: TColor);
    procedure SetSelectedTextColor(AValue: TColor);
    procedure SetEditHeight(AValue: Integer);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetButtonHeight(AValue: Integer);
    procedure SetSearchDebounceInterval(AValue: Integer);
    procedure SetIconStyle(AValue: TIconStyle);
    function GetSelectedKey: Integer;
    procedure SetSelectedKey(const AValue: Integer);
    function GetText: string;

    // Eventos internos
    procedure EditSearchChange(Sender: TObject);
    procedure EditSearchEnter(Sender: TObject);
    procedure EditSearchExit(Sender: TObject);
    procedure EditSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnDropDownClick(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState);
    procedure EditFontChanged(Sender: TObject);
    procedure GridFontChanged(Sender: TObject);
    procedure GridTitleFontChanged(Sender: TObject);
    procedure OnSearchTimer(Sender: TObject);

    // Métodos auxiliares
    procedure CreateComponents;
    procedure ApplyIconStyle;
    procedure CreateGridColumns;
    procedure DestroyAllColumns;
    procedure ArrangeControls;
    procedure ApplyFilter(const AText: string);
    procedure PerformSearch;
    procedure DataSetFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure SelectCurrentRecord;
    procedure ValidateOnExit;
    procedure OpenDropDown;
    procedure CloseDropDown;
    function GetFieldValue(const FieldName: string): string;
    function GetAllDisplayFieldsValue: string;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure DoExit; override;
    procedure Paint; override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure RefreshData;
    procedure SetFocus; override;
    procedure UpdateColumns;
    function LocateByKey(const AKeyValue: Integer): Boolean;

    property SelectedKey: Integer read GetSelectedKey write SetSelectedKey;
    property Text: string read GetText;
    property DBGrid: TDBGrid read FDBGrid;

  published
    // Propriedades de dados
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DataKeyField: string read FDataKeyField write SetDataKeyField;
    property DisplayFields: string read FDisplayFields write SetDisplayFields;
    property SearchFields: string read FSearchFields write SetSearchFields;

    // Propriedades de comportamento
    property MinCharsToSearch: Integer read FMinCharsToSearch write FMinCharsToSearch default 2;
    property AutoSearch: Boolean read FAutoSearch write FAutoSearch default True;
    property ShowValidationMessage: Boolean read FShowValidationMessage write FShowValidationMessage default True;
    property FieldSeparator: string read FFieldSeparator write FFieldSeparator;
    property SearchDebounceInterval: Integer read FSearchDebounceInterval write SetSearchDebounceInterval default 300;
    property IconStyle: TIconStyle read FIconStyle write SetIconStyle default isModern;

    // Fontes
    property EditFont: TFont read FEditFont write SetEditFont;
    property GridFont: TFont read FGridFont write SetGridFont;
    property GridTitleFont: TFont read FGridTitleFont write SetGridTitleFont;

    // Propriedades de tamanho
    property EditHeight: Integer read FEditHeight write SetEditHeight default 34;
    property ButtonsWidth: Integer read FButtonWidth write SetButtonWidth default 28;
    property ButtonsHeight: Integer read FButtonHeight write SetButtonHeight default 26;
    property GridHeight: Integer read FGridHeight write SetGridHeight default 150;

    // Propriedades de cores
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clHighlight;
    property SelectedTextColor: TColor read FSelectedTextColor write SetSelectedTextColor default clHighlightText;

    // Acesso aos botões
    property ButtonDropDown: TSpeedButton read FBtnDropDown;
    property ButtonSearch: TSpeedButton read FBtnSearch;
    property ButtonClear: TSpeedButton read FBtnClear;

    // Eventos
    property OnSearchClick: TSearchClickEvent read FOnSearchClick write FOnSearchClick;
    property OnItemSelected: TItemSelectedEvent read FOnItemSelected write FOnItemSelected;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;

    // Propriedades herdadas
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

uses
  LCLIntf;

procedure Register;
begin
  RegisterComponents('Nortus', [TNortusSearchList]);
end;

{ TNortusSearchList }

constructor TNortusSearchList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 350;
  Height := 34;  // AUMENTEI de 32 para 34
  BevelOuter := bvNone;
  Caption := '';
  Color := clWindow;

  FSelectedKey := -1;
  FText := '';
  FMinCharsToSearch := 2;
  FAutoSearch := True;
  FShowValidationMessage := True;
  FGridHeight := 150;
  FIsSelecting := False;
  FGridOpened := False;
  FLastValidText := '';
  FDataKeyField := '';
  FDisplayFields := '';
  FSearchFields := '';
  FOriginalFilter := '';
  FOriginalFiltered := False;
  FOriginalOnFilterRecord := nil;
  FCurrentSearchText := '';
  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FColumnsCreated := False;
  FEditHeight := 34;  // AUMENTEI de 32 para 34
  FButtonWidth := 28;
  FButtonHeight := 26;
  FArranging := False;
  FFirstArrange := True;
  FFieldSeparator := ' - ';
  FSearchDebounceInterval := 300;
  FIconStyle := isModern;

  FEditFont := TFont.Create;
  FEditFont.OnChange := @EditFontChanged;
  FGridFont := TFont.Create;
  FGridFont.OnChange := @GridFontChanged;
  FGridTitleFont := TFont.Create;
  FGridTitleFont.OnChange := @GridTitleFontChanged;
  FGridTitleFont.Style := [fsBold];

  // Timer para debounce
  FSearchTimer := TTimer.Create(Self);
  FSearchTimer.Enabled := False;
  FSearchTimer.Interval := FSearchDebounceInterval;
  FSearchTimer.OnTimer := @OnSearchTimer;

  CreateComponents;
  ApplyIconStyle;
end;

destructor TNortusSearchList.Destroy;
begin
  FSearchTimer.Free;
  FEditFont.Free;
  FGridFont.Free;
  FGridTitleFont.Free;
  inherited Destroy;
end;

procedure TNortusSearchList.CreateComponents;
var
  topMargin: Integer;
begin
  // Painel do Edit
  FEditPanel := TPanel.Create(Self);
  FEditPanel.Parent := Self;
  FEditPanel.Align := alTop;
  FEditPanel.Height := FEditHeight;
  FEditPanel.BevelOuter := bvNone;
  FEditPanel.Caption := '';
  FEditPanel.Color := clWhite;

  // CÁLCULO CORRIGIDO DO topMargin
  // Garantir que o edit fique centralizado mas com espaço para bordas
  topMargin := (FEditHeight - FButtonHeight) div 2;
  if topMargin < 2 then topMargin := 2;  // MUDEI: mínimo 2 pixels

  // Campo de pesquisa
  FEditSearch := TEdit.Create(Self);
  FEditSearch.Parent := FEditPanel;
  FEditSearch.Left := 4;  // MUDEI: de 2 para 4 pixels de margem
  FEditSearch.Top := 3;   // MUDEI: posição fixa de 3 pixels do topo
  FEditSearch.Width := 200;
  FEditSearch.Anchors := [akLeft, akTop, akRight, akBottom];  // ADICIONEI akBottom
  FEditSearch.TabOrder := 0;
  FEditSearch.TextHint := 'Digite para pesquisar...';
  FEditSearch.OnChange := @EditSearchChange;
  FEditSearch.OnEnter := @EditSearchEnter;
  FEditSearch.OnExit := @EditSearchExit;
  FEditSearch.OnKeyDown := @EditSearchKeyDown;
  FEditSearch.BorderStyle := bsSingle;  // GARANTIR borda

  // Botão DropDown
  FBtnDropDown := TSpeedButton.Create(Self);
  FBtnDropDown.Parent := FEditPanel;
  FBtnDropDown.Width := FButtonWidth;
  FBtnDropDown.Height := FButtonHeight;
  FBtnDropDown.Top := topMargin;
  FBtnDropDown.Anchors := [akTop, akRight];
  FBtnDropDown.Hint := 'Abrir lista';
  FBtnDropDown.ShowHint := True;
  FBtnDropDown.OnClick := @BtnDropDownClick;
  FBtnDropDown.Cursor := crHandPoint;

  // Botão Pesquisar
  FBtnSearch := TSpeedButton.Create(Self);
  FBtnSearch.Parent := FEditPanel;
  FBtnSearch.Width := FButtonWidth;
  FBtnSearch.Height := FButtonHeight;
  FBtnSearch.Top := topMargin;
  FBtnSearch.Anchors := [akTop, akRight];
  FBtnSearch.Hint := 'Pesquisar';
  FBtnSearch.ShowHint := True;
  FBtnSearch.OnClick := @BtnSearchClick;
  FBtnSearch.Cursor := crHandPoint;

  // Botão Limpar
  FBtnClear := TSpeedButton.Create(Self);
  FBtnClear.Parent := FEditPanel;
  FBtnClear.Width := FButtonWidth;
  FBtnClear.Height := FButtonHeight;
  FBtnClear.Top := topMargin;
  FBtnClear.Anchors := [akTop, akRight];
  FBtnClear.Hint := 'Limpar';
  FBtnClear.ShowHint := True;
  FBtnClear.OnClick := @BtnClearClick;
  FBtnClear.Cursor := crHandPoint;

  // DBGrid
  FDBGrid := TDBGrid.Create(Self);
  FDBGrid.Parent := nil;  // Será configurado no SetParent
  FDBGrid.Left := 0;
  FDBGrid.Top := 0;
  FDBGrid.Width := Self.Width;
  FDBGrid.Height := FGridHeight;
  FDBGrid.Anchors := [];  // Sem anchors, posicionamento manual
  FDBGrid.TabOrder := 1;
  FDBGrid.OnDblClick := @DBGridDblClick;
  FDBGrid.OnKeyDown := @DBGridKeyDown;
  FDBGrid.OnCellClick := @DBGridCellClick;
  FDBGrid.OnDrawColumnCell := @DBGridDrawColumnCell;
  FDBGrid.Visible := False;
  FDBGrid.ReadOnly := True;
  FDBGrid.BorderStyle := bsSingle;  // Borda visível
  FDBGrid.Options := [dgAlwaysShowSelection, dgRowSelect];
  FDBGrid.TitleFont.Style := [];
  FDBGrid.Flat := True;
  FDBGrid.FixedCols := 0;
  FDBGrid.Cursor := crHandPoint;
  FDBGrid.DataSource := nil;
end;

procedure TNortusSearchList.SetIconStyle(AValue: TIconStyle);
begin
  if FIconStyle = AValue then Exit;
  FIconStyle := AValue;

  ApplyIconStyle;

  if Assigned(FBtnDropDown) then FBtnDropDown.Invalidate;
  if Assigned(FBtnSearch) then FBtnSearch.Invalidate;
  if Assigned(FBtnClear) then FBtnClear.Invalidate;

  Invalidate;
end;

procedure TNortusSearchList.ApplyIconStyle;
begin
  if not Assigned(FBtnDropDown) then Exit;
  if not Assigned(FBtnSearch) then Exit;
  if not Assigned(FBtnClear) then Exit;

  FBtnDropDown.Glyph.Clear;
  FBtnSearch.Glyph.Clear;
  FBtnClear.Glyph.Clear;

  case FIconStyle of
    isClassic:
      begin
        FBtnDropDown.Caption := '▼';
        FBtnDropDown.Font.Size := 10;
        FBtnDropDown.Font.Name := 'Arial';
        FBtnDropDown.Font.Style := [];
        FBtnDropDown.Font.Color := clBlack;
        FBtnDropDown.Flat := False;

        FBtnSearch.Caption := '⊙';
        FBtnSearch.Font.Size := 12;
        FBtnSearch.Font.Name := 'Arial';
        FBtnSearch.Font.Style := [];
        FBtnSearch.Font.Color := clBlack;
        FBtnSearch.Flat := False;

        FBtnClear.Caption := '✖';
        FBtnClear.Font.Size := 10;
        FBtnClear.Font.Name := 'Arial';
        FBtnClear.Font.Style := [];
        FBtnClear.Font.Color := clBlack;
        FBtnClear.Flat := False;
      end;

    isModern:
      begin
        FBtnDropDown.Caption := '⯆';
        FBtnDropDown.Font.Size := 18;
        FBtnDropDown.Font.Name := 'Segoe UI Symbol';
        FBtnDropDown.Font.Style := [];
        FBtnDropDown.Font.Color := $00808080;
        FBtnDropDown.Flat := True;

        FBtnSearch.Caption := '⌕';
        FBtnSearch.Font.Size := 16;
        FBtnSearch.Font.Name := 'Segoe UI Symbol';
        FBtnSearch.Font.Style := [];
        FBtnSearch.Font.Color := $00404040;
        FBtnSearch.Flat := True;

        FBtnClear.Caption := '⨯';
        FBtnClear.Font.Size := 16;
        FBtnClear.Font.Name := 'Segoe UI Symbol';
        FBtnClear.Font.Style := [];
        FBtnClear.Font.Color := $000000C0;
        FBtnClear.Flat := True;
      end;

    isMinimalist:
      begin
        FBtnDropDown.Caption := '▾';
        FBtnDropDown.Font.Size := 14;
        FBtnDropDown.Font.Name := 'Segoe UI';
        FBtnDropDown.Font.Style := [];
        FBtnDropDown.Font.Color := $00808080;
        FBtnDropDown.Flat := True;

        FBtnSearch.Caption := '◯';
        FBtnSearch.Font.Size := 14;
        FBtnSearch.Font.Name := 'Segoe UI';
        FBtnSearch.Font.Style := [];
        FBtnSearch.Font.Color := $00800000;
        FBtnSearch.Flat := True;

        FBtnClear.Caption := '✕';
        FBtnClear.Font.Size := 12;
        FBtnClear.Font.Name := 'Segoe UI';
        FBtnClear.Font.Style := [];
        FBtnClear.Font.Color := $00000080;
        FBtnClear.Flat := True;
      end;

    isBold:
      begin
        FBtnDropDown.Caption := '▼';
        FBtnDropDown.Font.Size := 14;
        FBtnDropDown.Font.Name := 'Arial Black';
        FBtnDropDown.Font.Style := [fsBold];
        FBtnDropDown.Font.Color := clBlack;
        FBtnDropDown.Flat := True;

        FBtnSearch.Caption := '●';
        FBtnSearch.Font.Size := 16;
        FBtnSearch.Font.Name := 'Arial Black';
        FBtnSearch.Font.Style := [fsBold];
        FBtnSearch.Font.Color := $00FF0000;
        FBtnSearch.Flat := True;

        FBtnClear.Caption := '✖';
        FBtnClear.Font.Size := 14;
        FBtnClear.Font.Name := 'Arial Black';
        FBtnClear.Font.Style := [fsBold];
        FBtnClear.Font.Color := $000000FF;
        FBtnClear.Flat := True;
      end;
  end;

  // Aplicar fontes padrão apenas se não foram customizadas
  if Assigned(FEditSearch) and (FEditFont.Name = 'default') then
  begin
    FEditFont.Name := 'Segoe UI';
    FEditFont.Size := 10;
    FEditFont.Color := clBlack;
  end;

  if Assigned(FDBGrid) then
  begin
    if FGridFont.Name = 'default' then
    begin
      FGridFont.Name := 'Segoe UI';
      FGridFont.Size := 9;
      FGridFont.Color := clBlack;
    end;

    if FGridTitleFont.Name = 'default' then
    begin
      FGridTitleFont.Name := 'Segoe UI';
      FGridTitleFont.Size := 9;
      FGridTitleFont.Style := [fsBold];
      FGridTitleFont.Color := clBlack;
    end;
  end;
end;

procedure TNortusSearchList.Loaded;
begin
  inherited Loaded;

  ApplyIconStyle;
  ArrangeControls;
end;

procedure TNortusSearchList.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  // CORREÇÃO: O DBGrid deve ser filho do componente em design time
  // Isso garante que ele seja destruído junto com o componente
  // Em design time, isso evita que o grid fique "órfão" quando deletamos o componente
  if Assigned(FDBGrid) then
  begin
    if (csDesigning in ComponentState) then
    begin
      // Em design time, o grid é filho do componente
      FDBGrid.Parent := Self;
      FDBGrid.Visible := False;
    end
    else if Assigned(NewParent) then
    begin
      // Em runtime, o grid pode ser irmão para melhor controle de Z-order
      FDBGrid.Parent := NewParent;
    end;
  end;

  if Assigned(NewParent) then
  begin
    ArrangeControls;
    ApplyIconStyle;
  end;
end;

procedure TNortusSearchList.Paint;
begin
  inherited Paint;

  if FFirstArrange then
  begin
    FFirstArrange := False;
    ApplyIconStyle;
    ArrangeControls;
  end;

  // Garantir que o grid esteja escondido em design time
  if (csDesigning in ComponentState) and Assigned(FDBGrid) then
  begin
    if FDBGrid.Parent = Self then
      FDBGrid.Visible := False;
  end;
end;

procedure TNortusSearchList.Resize;
begin
  inherited Resize;
  ArrangeControls;

  // Se o grid estiver aberto, reposicioná-lo
  if FGridOpened and Assigned(FDBGrid) and FDBGrid.Visible then
  begin
    FDBGrid.Left := Self.Left;
    FDBGrid.Top := Self.Top + Self.Height;
    FDBGrid.Width := Self.Width;
  end;
end;

procedure TNortusSearchList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  // Se o grid estiver aberto, reposicioná-lo quando o componente for movido
  if FGridOpened and Assigned(FDBGrid) and FDBGrid.Visible then
  begin
    FDBGrid.Left := ALeft;
    FDBGrid.Top := ATop + AHeight;
    FDBGrid.Width := AWidth;
  end;
end;

procedure TNortusSearchList.DoExit;
var
  FocusedControl: TWinControl;
begin
  inherited DoExit;

  Application.ProcessMessages;

  FocusedControl := Screen.ActiveControl;

  if Assigned(FocusedControl) then
  begin
    if (FocusedControl = FEditSearch) or
       (FocusedControl = FDBGrid) then
      Exit;
  end;

  CloseDropDown;
  ValidateOnExit;
end;

procedure TNortusSearchList.DestroyAllColumns;
var
  I: Integer;
begin
  if not Assigned(FDBGrid) then Exit;

  for I := FDBGrid.Columns.Count - 1 downto 0 do
    FDBGrid.Columns[I].Free;

  FDBGrid.Columns.Clear;
end;

procedure TNortusSearchList.ArrangeControls;
var
  topMargin: Integer;
  totalButtonsWidth: Integer;
  editWidth: Integer;
  editRealHeight: Integer;
begin
  if csDestroying in ComponentState then Exit;
  if FArranging then Exit;

  if not Assigned(FEditPanel) then Exit;
  if not Assigned(FEditSearch) then Exit;
  if not Assigned(FBtnDropDown) then Exit;
  if not Assigned(FBtnSearch) then Exit;
  if not Assigned(FBtnClear) then Exit;

  FArranging := True;
  try
    FEditPanel.Height := FEditHeight;

    // CÁLCULO MELHORADO
    topMargin := (FEditHeight - FButtonHeight) div 2;
    if topMargin < 2 then topMargin := 2;

    totalButtonsWidth := FButtonWidth * 3 + 6;  // MUDEI: +6 ao invés de +4

    editWidth := FEditPanel.ClientWidth - totalButtonsWidth - 8;  // MUDEI: -8 ao invés de -4
    if editWidth < 50 then editWidth := 50;

    // EDIT com altura calculada corretamente
    editRealHeight := FEditHeight - 6;  // 6 pixels de margem total (3 em cima, 3 embaixo)
    if editRealHeight < 20 then editRealHeight := 20;

    FEditSearch.Left := 4;
    FEditSearch.Top := 3;
    FEditSearch.Width := editWidth;
    FEditSearch.Height := editRealHeight;

    FBtnClear.Left := FEditPanel.ClientWidth - FButtonWidth - 4;  // MUDEI: -4 ao invés de -2
    FBtnClear.Top := topMargin;
    FBtnClear.Width := FButtonWidth;
    FBtnClear.Height := FButtonHeight;

    FBtnSearch.Left := FBtnClear.Left - FButtonWidth - 2;
    FBtnSearch.Top := topMargin;
    FBtnSearch.Width := FButtonWidth;
    FBtnSearch.Height := FButtonHeight;

    FBtnDropDown.Left := FBtnSearch.Left - FButtonWidth - 2;
    FBtnDropDown.Top := topMargin;
    FBtnDropDown.Width := FButtonWidth;
    FBtnDropDown.Height := FButtonHeight;

    // O DBGrid é posicionado dinamicamente no OpenDropDown

  finally
    FArranging := False;
  end;
end;

procedure TNortusSearchList.SetDataSource(AValue: TDataSource);
begin
  if FDataSource = AValue then Exit;

  if FDataSource <> nil then
    FDataSource.RemoveFreeNotification(Self);

  FDataSource := AValue;

  if FDataSource <> nil then
    FDataSource.FreeNotification(Self);

  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    if Assigned(FDBGrid) then
    begin
      FDBGrid.DataSource := nil;
      DestroyAllColumns;
      FColumnsCreated := False;

      if Assigned(FDataSource) and Assigned(FDataSource.DataSet) and
         (Trim(FDisplayFields) <> '') then
      begin
        if FDataSource.DataSet.Active then
          CreateGridColumns;
      end;

      if FColumnsCreated then
        FDBGrid.DataSource := FDataSource;
    end;
  end;
end;

procedure TNortusSearchList.SetDataKeyField(AValue: string);
begin
  if FDataKeyField = AValue then Exit;
  FDataKeyField := AValue;
end;

procedure TNortusSearchList.SetDisplayFields(AValue: string);
begin
  if FDisplayFields = AValue then Exit;
  FDisplayFields := AValue;

  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    if Assigned(FDBGrid) then
    begin
      FDBGrid.DataSource := nil;
      DestroyAllColumns;
      FColumnsCreated := False;
      CreateGridColumns;
      if FColumnsCreated and Assigned(FDataSource) then
        FDBGrid.DataSource := FDataSource;
    end;
  end;
end;

procedure TNortusSearchList.SetSearchFields(AValue: string);
begin
  if FSearchFields = AValue then Exit;
  FSearchFields := AValue;
end;

procedure TNortusSearchList.SetEditFont(AValue: TFont);
begin
  FEditFont.Assign(AValue);
end;

procedure TNortusSearchList.SetGridFont(AValue: TFont);
begin
  FGridFont.Assign(AValue);
end;

procedure TNortusSearchList.SetGridTitleFont(AValue: TFont);
begin
  FGridTitleFont.Assign(AValue);
end;

procedure TNortusSearchList.SetGridHeight(AValue: Integer);
begin
  if FGridHeight = AValue then Exit;
  if AValue < 50 then AValue := 50;
  if AValue > 400 then AValue := 400;
  FGridHeight := AValue;
  if Assigned(FDBGrid) then
  begin
    FDBGrid.Height := FGridHeight;
    // Se o grid estiver aberto, reposicioná-lo
    if FGridOpened then
    begin
      FDBGrid.Top := Self.Top + Self.Height;
    end;
  end;
end;

procedure TNortusSearchList.SetSelectedColor(AValue: TColor);
begin
  if FSelectedColor = AValue then Exit;
  FSelectedColor := AValue;
  if Assigned(FDBGrid) then
    FDBGrid.Invalidate;
end;

procedure TNortusSearchList.SetSelectedTextColor(AValue: TColor);
begin
  if FSelectedTextColor = AValue then Exit;
  FSelectedTextColor := AValue;
  if Assigned(FDBGrid) then
    FDBGrid.Invalidate;
end;

procedure TNortusSearchList.SetEditHeight(AValue: Integer);
begin
  if FEditHeight = AValue then Exit;
  if AValue < 24 then AValue := 24;  // MUDEI: mínimo 24 ao invés de 20
  if AValue > 60 then AValue := 60;
  FEditHeight := AValue;
  ArrangeControls;
end;

procedure TNortusSearchList.SetButtonWidth(AValue: Integer);
begin
  if FButtonWidth = AValue then Exit;
  if AValue < 15 then AValue := 15;
  if AValue > 50 then AValue := 50;
  FButtonWidth := AValue;
  ArrangeControls;
end;

procedure TNortusSearchList.SetButtonHeight(AValue: Integer);
begin
  if FButtonHeight = AValue then Exit;
  if AValue < 15 then AValue := 15;
  if AValue > 50 then AValue := 50;
  FButtonHeight := AValue;
  ArrangeControls;
end;

procedure TNortusSearchList.SetSearchDebounceInterval(AValue: Integer);
begin
  if FSearchDebounceInterval = AValue then Exit;
  if AValue < 0 then AValue := 0;
  if AValue > 2000 then AValue := 2000;
  FSearchDebounceInterval := AValue;
  if Assigned(FSearchTimer) then
    FSearchTimer.Interval := FSearchDebounceInterval;
end;

function TNortusSearchList.GetSelectedKey: Integer;
begin
  Result := FSelectedKey;
end;

procedure TNortusSearchList.SetSelectedKey(const AValue: Integer);
begin
  if FSelectedKey = AValue then Exit;

  if AValue = -1 then
  begin
    Clear;
    Exit;
  end;

  if LocateByKey(AValue) then
  begin
    FSelectedKey := AValue;
  end
  else
  begin
    FSelectedKey := -1;
    FText := '';
    FLastValidText := '';
    if Assigned(FEditSearch) then
      FEditSearch.Text := '';
  end;

  CloseDropDown;
end;

function TNortusSearchList.LocateByKey(const AKeyValue: Integer): Boolean;
var
  DS: TDataSet;
  WasOpen: Boolean;
begin
  Result := False;

  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then Exit;
  if Trim(FDataKeyField) = '' then Exit;
  if AKeyValue = -1 then Exit;

  DS := FDataSource.DataSet;
  WasOpen := FGridOpened;

  if FGridOpened then
    CloseDropDown;

  if not DS.Active then
  begin
    try
      DS.Open;
    except
      Exit;
    end;
  end;

  DS.DisableControls;
  try
    Result := DS.Locate(FDataKeyField, AKeyValue, []);

    if Result then
      SelectCurrentRecord;

  finally
    DS.EnableControls;
  end;

  if not WasOpen then
    CloseDropDown;
end;

function TNortusSearchList.GetText: string;
begin
  Result := FText;
end;

procedure TNortusSearchList.EditFontChanged(Sender: TObject);
begin
  if Assigned(FEditSearch) then
    FEditSearch.Font.Assign(FEditFont);
end;

procedure TNortusSearchList.GridFontChanged(Sender: TObject);
begin
  if Assigned(FDBGrid) then
    FDBGrid.Font.Assign(FGridFont);
end;

procedure TNortusSearchList.GridTitleFontChanged(Sender: TObject);
begin
  if Assigned(FDBGrid) then
    FDBGrid.TitleFont.Assign(FGridTitleFont);
end;

procedure TNortusSearchList.EditSearchChange(Sender: TObject);
begin
  if not Assigned(FEditSearch) then Exit;
  if FIsSelecting then Exit;

  FText := FEditSearch.Text;

  if FAutoSearch then
  begin
    FSearchTimer.Enabled := False;

    if Length(FEditSearch.Text) >= FMinCharsToSearch then
    begin
      FSearchTimer.Enabled := True;
    end
    else if FEditSearch.Text = '' then
    begin
      ApplyFilter('');
      CloseDropDown;
    end;
  end;
end;

procedure TNortusSearchList.OnSearchTimer(Sender: TObject);
begin
  FSearchTimer.Enabled := False;
  PerformSearch;
end;

procedure TNortusSearchList.PerformSearch;
begin
  if not Assigned(FEditSearch) then Exit;

  ApplyFilter(FEditSearch.Text);

  if Assigned(FDataSource) and Assigned(FDataSource.DataSet) and
     (FDataSource.DataSet.RecordCount > 0) then
    OpenDropDown
  else
    CloseDropDown;
end;

procedure TNortusSearchList.EditSearchEnter(Sender: TObject);
begin
  // Não abre automaticamente
end;

procedure TNortusSearchList.EditSearchExit(Sender: TObject);
begin
  if FIsSelecting then Exit;
  if FGridOpened then Exit;

  Application.ProcessMessages;

  if Assigned(FDBGrid) and FDBGrid.Focused then
    Exit;
end;

procedure TNortusSearchList.EditSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = Shift then;

  if not Assigned(FDBGrid) or not Assigned(FDataSource) then Exit;
  if not Assigned(FDataSource.DataSet) then Exit;

  case Key of
    VK_DOWN:
      begin
        if not FGridOpened then
          OpenDropDown;

        if FDataSource.DataSet.RecordCount > 0 then
        begin
          FDBGrid.SetFocus;
          if FDataSource.DataSet.Bof then
            FDataSource.DataSet.First;
        end;
        Key := 0;
      end;
    VK_UP:
      begin
        if FGridOpened and (FDataSource.DataSet.RecordCount > 0) then
        begin
          FDBGrid.SetFocus;
          if not FDataSource.DataSet.Bof then
            FDataSource.DataSet.Prior;
        end;
        Key := 0;
      end;
    VK_RETURN:
      begin
        if FGridOpened and (FDataSource.DataSet.RecordCount > 0) then
        begin
          SelectCurrentRecord;
          CloseDropDown;
        end;
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        CloseDropDown;
        Key := 0;
      end;
  end;
end;

procedure TNortusSearchList.BtnDropDownClick(Sender: TObject);
begin
  if not Assigned(FEditSearch) then Exit;

  if FGridOpened then
    CloseDropDown
  else
  begin
    ApplyFilter('');
    OpenDropDown;
  end;

  if Assigned(FOnDropDown) then
    FOnDropDown(Self);

  FEditSearch.SetFocus;
end;

procedure TNortusSearchList.BtnSearchClick(Sender: TObject);
begin
  if not Assigned(FEditSearch) then Exit;

  if Assigned(FOnSearchClick) then
    FOnSearchClick(Self, FEditSearch.Text);

  if not FAutoSearch then
  begin
    PerformSearch;
  end;
end;

procedure TNortusSearchList.BtnClearClick(Sender: TObject);
begin
  Clear;

  CloseDropDown;

  if Assigned(FOnClear) then
    FOnClear(Self);

  if Assigned(FEditSearch) then
    FEditSearch.SetFocus;
end;

procedure TNortusSearchList.DBGridDblClick(Sender: TObject);
begin
  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then Exit;

  if FDataSource.DataSet.RecordCount > 0 then
  begin
    SelectCurrentRecord;
    CloseDropDown;
    if Assigned(FEditSearch) then
      FEditSearch.SetFocus;
  end;
end;

procedure TNortusSearchList.DBGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = Shift then;

  case Key of
    VK_RETURN:
      begin
        if Assigned(FDataSource) and Assigned(FDataSource.DataSet) and
           (FDataSource.DataSet.RecordCount > 0) then
        begin
          SelectCurrentRecord;
          CloseDropDown;
          if Assigned(FEditSearch) then
            FEditSearch.SetFocus;
        end;
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        CloseDropDown;
        if Assigned(FEditSearch) then
        begin
          FEditSearch.Text := FLastValidText;
          FEditSearch.SetFocus;
        end;
        Key := 0;
      end;
    VK_UP, VK_DOWN:
      begin
        // Permite navegação
      end;
  end;
end;

procedure TNortusSearchList.DBGridCellClick(Column: TColumn);
begin
  if Column = Column then;
end;

procedure TNortusSearchList.DBGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Grid: TDBGrid;
begin
  if DataCol = DataCol then;

  Grid := Sender as TDBGrid;

  if gdSelected in State then
  begin
    Grid.Canvas.Brush.Color := FSelectedColor;
    Grid.Canvas.Font.Color := FSelectedTextColor;
    Grid.Canvas.Font.Style := [fsBold];
    Grid.Canvas.FillRect(Rect);

    if Assigned(Column.Field) then
    begin
      Grid.Canvas.TextOut(Rect.Left + 4, Rect.Top + 2, Column.Field.DisplayText);
    end;
  end
  else
  begin
    Grid.Canvas.Brush.Color := clWindow;
    Grid.Canvas.Font.Color := clWindowText;
    Grid.Canvas.Font.Style := [];
    Grid.Canvas.FillRect(Rect);

    if Assigned(Column.Field) then
    begin
      Grid.Canvas.TextOut(Rect.Left + 4, Rect.Top + 2, Column.Field.DisplayText);
    end;
  end;
end;

procedure TNortusSearchList.OpenDropDown;
begin
  if FGridOpened then Exit;
  if not Assigned(FDBGrid) then Exit;
  if not Assigned(FEditPanel) then Exit;

  if not FColumnsCreated then
  begin
    CreateGridColumns;
    if FColumnsCreated and Assigned(FDataSource) then
      FDBGrid.DataSource := FDataSource;
  end;

  FGridOpened := True;

  // Posicionar o grid corretamente baseado se é filho ou irmão
  if FDBGrid.Parent = Self then
  begin
    // Grid é filho do componente (design time)
    FDBGrid.Left := 0;
    FDBGrid.Top := Self.Height;
    FDBGrid.Width := Self.Width;
    FDBGrid.Height := FGridHeight;
  end
  else
  begin
    // Grid é irmão do componente (runtime)
    FDBGrid.Left := Self.Left;
    FDBGrid.Top := Self.Top + Self.Height;
    FDBGrid.Width := Self.Width;
    FDBGrid.Height := FGridHeight;
  end;

  FDBGrid.Visible := True;
  FDBGrid.BringToFront;  // Garante que fique no topo do Z-order

  if Assigned(FBtnDropDown) and FBtnDropDown.Glyph.Empty then
  begin
    case FIconStyle of
      isClassic:
        begin
          FBtnDropDown.Caption := '▲';
          FBtnDropDown.Font.Size := 10;
        end;
      isModern:
        begin
          FBtnDropDown.Caption := '⯅';
          FBtnDropDown.Font.Size := 18;
        end;
      isMinimalist:
        begin
          FBtnDropDown.Caption := '▴';
          FBtnDropDown.Font.Size := 14;
        end;
      isBold:
        begin
          FBtnDropDown.Caption := '▲';
          FBtnDropDown.Font.Size := 14;
        end;
    end;
  end;

  if Assigned(FDataSource) and Assigned(FDataSource.DataSet) then
  begin
    if FDataSource.DataSet.Active then
      FDataSource.DataSet.First;
  end;
end;

procedure TNortusSearchList.CreateGridColumns;
var
  FieldList: TStringList;
  I: Integer;
  FieldName: string;
  Field: TField;
  MaxLen: Integer;
  DS: TDataSet;
  Col: TColumn;
begin
  if not Assigned(FDBGrid) then Exit;
  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then Exit;

  DS := FDataSource.DataSet;

  if Trim(FDisplayFields) = '' then
  begin
    FColumnsCreated := False;
    Exit;
  end;

  if not DS.Active then
  begin
    try
      DS.Open;
    except
      FColumnsCreated := False;
      Exit;
    end;
  end;

  FDBGrid.BeginUpdate;
  try
    DestroyAllColumns;

    FieldList := TStringList.Create;
    try
      FieldList.Delimiter := ';';
      FieldList.StrictDelimiter := True;
      FieldList.DelimitedText := FDisplayFields;

      for I := 0 to FieldList.Count - 1 do
      begin
        FieldName := Trim(FieldList[I]);

        if FieldName = '' then
          Continue;

        Field := DS.FindField(FieldName);

        if Assigned(Field) then
        begin
          MaxLen := Field.DisplayWidth * 8;
          if MaxLen < 60 then MaxLen := 60;
          if MaxLen > 300 then MaxLen := 300;

          Col := FDBGrid.Columns.Add;
          Col.FieldName := Field.FieldName;
          Col.Title.Caption := Field.DisplayLabel;
          Col.Width := MaxLen;
          Col.Visible := True;
        end;
      end;

      FColumnsCreated := (FDBGrid.Columns.Count > 0);

    finally
      FieldList.Free;
    end;

  finally
    FDBGrid.EndUpdate;
  end;
end;

procedure TNortusSearchList.CloseDropDown;
begin
  if not FGridOpened then Exit;
  if not Assigned(FDBGrid) then Exit;
  if not Assigned(FEditPanel) then Exit;

  FGridOpened := False;
  FDBGrid.Visible := False;

  if Assigned(FBtnDropDown) and FBtnDropDown.Glyph.Empty then
  begin
    case FIconStyle of
      isClassic:
        begin
          FBtnDropDown.Caption := '▼';
          FBtnDropDown.Font.Size := 10;
        end;
      isModern:
        begin
          FBtnDropDown.Caption := '⯆';
          FBtnDropDown.Font.Size := 18;
        end;
      isMinimalist:
        begin
          FBtnDropDown.Caption := '▾';
          FBtnDropDown.Font.Size := 14;
        end;
      isBold:
        begin
          FBtnDropDown.Caption := '▼';
          FBtnDropDown.Font.Size := 14;
        end;
    end;
  end;

  FCurrentSearchText := '';

  if Assigned(FDataSource) and Assigned(FDataSource.DataSet) then
  begin
    if FDataSource.DataSet.Active then
    begin
      try
        FDataSource.DataSet.OnFilterRecord := FOriginalOnFilterRecord;
        FDataSource.DataSet.Filter := FOriginalFilter;
        FDataSource.DataSet.Filtered := FOriginalFiltered;
        FDataSource.DataSet.First;
      except
      end;
    end;
  end;
end;

procedure TNortusSearchList.ApplyFilter(const AText: string);
var
  DS: TDataSet;
begin
  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then Exit;

  DS := FDataSource.DataSet;
  if not DS.Active then Exit;

  if not FGridOpened then
  begin
    FOriginalFilter := DS.Filter;
    FOriginalFiltered := DS.Filtered;
    FOriginalOnFilterRecord := DS.OnFilterRecord;
  end;

  if Trim(AText) = '' then
  begin
    DS.OnFilterRecord := FOriginalOnFilterRecord;
    DS.Filtered := FOriginalFiltered;
    DS.Filter := FOriginalFilter;
    DS.First;
    FCurrentSearchText := '';
    Exit;
  end;

  FCurrentSearchText := UpperCase(Trim(AText));
  DS.OnFilterRecord := @DataSetFilterRecord;
  DS.Filtered := True;
  DS.First;
end;

procedure TNortusSearchList.DataSetFilterRecord(DataSet: TDataSet; var Accept: Boolean);
var
  SearchFieldsList: TStringList;
  I: Integer;
  FieldValue: string;
  FieldName: string;
  Field: TField;
begin
  Accept := False;

  if FCurrentSearchText = '' then
  begin
    Accept := True;
    Exit;
  end;

  SearchFieldsList := TStringList.Create;
  try
    SearchFieldsList.Delimiter := ';';
    SearchFieldsList.StrictDelimiter := True;

    if Trim(FSearchFields) <> '' then
    begin
      SearchFieldsList.DelimitedText := FSearchFields;
    end
    else if Trim(FDisplayFields) <> '' then
    begin
      SearchFieldsList.DelimitedText := FDisplayFields;
    end
    else
    begin
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        if DataSet.Fields[I].Visible then
          SearchFieldsList.Add(DataSet.Fields[I].FieldName);
      end;
    end;

    for I := 0 to SearchFieldsList.Count - 1 do
    begin
      FieldName := Trim(SearchFieldsList[I]);

      if FieldName = '' then
        Continue;

      try
        Field := DataSet.FindField(FieldName);

        if Assigned(Field) and not Field.IsNull then
        begin
          FieldValue := UpperCase(Trim(Field.AsString));

          if Pos(FCurrentSearchText, FieldValue) > 0 then
          begin
            Accept := True;
            Break;
          end;
        end;
      except
        Continue;
      end;
    end;

  finally
    SearchFieldsList.Free;
  end;
end;

function TNortusSearchList.GetAllDisplayFieldsValue: string;
var
  FieldList: TStringList;
  I: Integer;
  FieldName: string;
  Field: TField;
  FieldValue: string;
begin
  Result := '';

  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then Exit;
  if not FDataSource.DataSet.Active then Exit;

  if Trim(FDisplayFields) <> '' then
  begin
    FieldList := TStringList.Create;
    try
      FieldList.Delimiter := ';';
      FieldList.StrictDelimiter := True;
      FieldList.DelimitedText := FDisplayFields;

      for I := 0 to FieldList.Count - 1 do
      begin
        FieldName := Trim(FieldList[I]);

        if FieldName = '' then
          Continue;

        Field := FDataSource.DataSet.FindField(FieldName);

        if Assigned(Field) and not Field.IsNull then
        begin
          FieldValue := Trim(Field.AsString);

          if FieldValue <> '' then
          begin
            if Result <> '' then
              Result := Result + FFieldSeparator;

            Result := Result + FieldValue;
          end;
        end;
      end;

    finally
      FieldList.Free;
    end;
  end;
end;

procedure TNortusSearchList.SelectCurrentRecord;
var
  DS: TDataSet;
  DisplayValue: string;
  KeyValue: string;
begin
  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then Exit;
  if not Assigned(FEditSearch) then Exit;

  DS := FDataSource.DataSet;
  if DS.RecordCount = 0 then Exit;

  FIsSelecting := True;
  try
    KeyValue := GetFieldValue(FDataKeyField);
    FSelectedKey := StrToIntDef(KeyValue, -1);

    DisplayValue := GetAllDisplayFieldsValue;

    if DisplayValue <> '' then
      FText := DisplayValue
    else
      FText := KeyValue;

    FLastValidText := FText;

    if Assigned(FEditSearch) then
    begin
      FEditSearch.Text := FText;
      FEditSearch.Modified := False;
    end;

    if Assigned(FOnItemSelected) then
      FOnItemSelected(Self, FSelectedKey);
  finally
    FIsSelecting := False;
  end;
end;

procedure TNortusSearchList.ValidateOnExit;
var
  DS: TDataSet;
  Found: Boolean;
  SearchText: string;
  DisplayValue: string;
  OriginalText: string;
begin
  if not Assigned(FEditSearch) then Exit;

  if Assigned(FEditSearch) and FEditSearch.Focused then Exit;
  if Assigned(FDBGrid) and FDBGrid.Focused then Exit;

  OriginalText := Trim(FEditSearch.Text);

  if OriginalText = '' then
  begin
    Clear;
    Exit;
  end;

  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then
  begin
    Clear;
    Exit;
  end;

  DS := FDataSource.DataSet;

  if not DS.Active then
  begin
    Clear;
    Exit;
  end;

  if (FSelectedKey <> -1) and (OriginalText = Trim(FLastValidText)) then
    Exit;

  SearchText := UpperCase(OriginalText);
  Found := False;

  DS.OnFilterRecord := FOriginalOnFilterRecord;
  DS.Filtered := FOriginalFiltered;
  DS.Filter := FOriginalFilter;

  DS.DisableControls;
  try
    DS.First;
    while not DS.EOF do
    begin
      DisplayValue := UpperCase(Trim(GetAllDisplayFieldsValue));

      if DisplayValue = SearchText then
      begin
        Found := True;
        SelectCurrentRecord;
        Break;
      end;
      DS.Next;
    end;
  finally
    DS.EnableControls;
  end;

  if not Found then
  begin
    if FShowValidationMessage and (OriginalText <> '') then
      ShowMessage('Valor "' + OriginalText + '" não encontrado na lista!');

    Clear;
  end;
end;

function TNortusSearchList.GetFieldValue(const FieldName: string): string;
var
  Field: TField;
begin
  Result := '';

  if not Assigned(FDataSource) or not Assigned(FDataSource.DataSet) then Exit;
  if not FDataSource.DataSet.Active then Exit;

  if FieldName = '' then
    Exit;

  Field := FDataSource.DataSet.FindField(FieldName);
  if Assigned(Field) then
    Result := Field.AsString;
end;

procedure TNortusSearchList.Clear;
begin
  if FGridOpened and Assigned(FDBGrid) and FDBGrid.Focused then
    Exit;

  if Assigned(FSearchTimer) then
    FSearchTimer.Enabled := False;

  FSelectedKey := -1;
  FText := '';
  FLastValidText := '';
  FCurrentSearchText := '';

  if Assigned(FEditSearch) then
  begin
    FEditSearch.Text := '';
    FEditSearch.Modified := False;
  end;

  if Assigned(FDataSource) and Assigned(FDataSource.DataSet) then
  begin
    if FDataSource.DataSet.Active then
    begin
      try
        FDataSource.DataSet.OnFilterRecord := FOriginalOnFilterRecord;
        FDataSource.DataSet.Filter := FOriginalFilter;
        FDataSource.DataSet.Filtered := FOriginalFiltered;
        FDataSource.DataSet.First;
      except
      end;
    end;
  end;

  CloseDropDown;
end;

procedure TNortusSearchList.RefreshData;
begin
  if Assigned(FDataSource) and Assigned(FDataSource.DataSet) then
  begin
    if FDataSource.DataSet.Active then
      FDataSource.DataSet.Refresh;
  end;
end;

procedure TNortusSearchList.SetFocus;
begin
  if Assigned(FEditSearch) and FEditSearch.CanFocus then
    FEditSearch.SetFocus;
end;

procedure TNortusSearchList.UpdateColumns;
begin
  if Assigned(FDBGrid) then
  begin
    FDBGrid.DataSource := nil;
    DestroyAllColumns;
    FColumnsCreated := False;
    CreateGridColumns;
    if FColumnsCreated and Assigned(FDataSource) then
      FDBGrid.DataSource := FDataSource;
  end;
end;

procedure TNortusSearchList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FDataSource) then
    FDataSource := nil;
end;

end.
