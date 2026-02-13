unit uNortusAliquotaEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Buttons, Graphics, LCLType;

type
  TTipoAliquota = (taPercentual, taValor);

  { TNortusAliquotaEdit }

  TNortusAliquotaEdit = class(TCustomControl)
  private
    FEdit: TEdit;
    FBtn: TSpeedButton;
    FTipo: TTipoAliquota;
    FOnChange: TNotifyEvent;
    FInternalUpdating: Boolean;

    // Propriedades básicas
    FMinValue: Currency;
    FMaxValue: Currency;
    FMaxPercentual: Currency;
    FStepValue: Currency;
    FStepPercentual: Currency;
    FReadOnly: Boolean;
    FButtonEnabled: Boolean;
    FTextPercentual: string;
    FTextValor: string;
    FDecimalPlaces: Integer;
    FButtonWidth: Integer;
    FAutoFormat: Boolean;
    FAllowNegative: Boolean;

    // Métodos privados
    procedure BtnClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditExit(Sender: TObject);
    procedure EditEnter(Sender: TObject);

    // Setters
    procedure SetTipo(const AValue: TTipoAliquota);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetButtonEnabled(const AValue: Boolean);
    procedure SetTextPercentual(const AValue: string);
    procedure SetTextValor(const AValue: string);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetMinValue(const AValue: Currency);
    procedure SetMaxValue(const AValue: Currency);
    procedure SetMaxPercentual(const AValue: Currency);
    procedure SetDecimalPlaces(const AValue: Integer);
    procedure SetAutoFormat(const AValue: Boolean);
    procedure SetAllowNegative(const AValue: Boolean);

    // Getters
    function GetValue: Currency;
    procedure SetValue(AValue: Currency);
    function GetText: string;
    procedure SetText(const AValue: string);

    // Métodos auxiliares
    function TryParseValue(const S: string; out V: Currency): Boolean;
    procedure UpdateButtonText;
    procedure ArrangeControls;
    function ValidateValue(var AValue: Currency): Boolean;
    procedure FormatValue;
    function GetStepValue: Currency;

  protected
    procedure Paint; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure DoOnResize; override;
    procedure CreateWnd; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure SelectAll;
    procedure SetFocus; override;

    // Métodos utilitários
    function IsEmpty: Boolean;
    function IsValid: Boolean;
    procedure ToggleTipo;

  published
    // Propriedades principais
    property Tipo: TTipoAliquota read FTipo write SetTipo default taPercentual;
    property Value: Currency read GetValue write SetValue;
    property Text: string read GetText write SetText;

    // Propriedades de validação
    property MinValue: Currency read FMinValue write SetMinValue;
    property MaxValue: Currency read FMaxValue write SetMaxValue;
    property MaxPercentual: Currency read FMaxPercentual write SetMaxPercentual;
    property AllowNegative: Boolean read FAllowNegative write SetAllowNegative default False;

    // Propriedades de comportamento
    property StepValue: Currency read FStepValue write FStepValue;
    property StepPercentual: Currency read FStepPercentual write FStepPercentual;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ButtonEnabled: Boolean read FButtonEnabled write SetButtonEnabled default True;
    property AutoFormat: Boolean read FAutoFormat write SetAutoFormat default True;

    // Propriedades de aparência
    property TextPercentual: string read FTextPercentual write SetTextPercentual;
    property TextValor: string read FTextValor write SetTextValor;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 30;
    property DecimalPlaces: Integer read FDecimalPlaces write SetDecimalPlaces default 2;

    // Eventos
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    // Propriedades herdadas
    property Align;
    property Anchors;
    property Color default clWindow;
    property Enabled;
    property Font;
    property Height default 25;
    property ParentColor default False;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Width default 120;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Nortus', [TNortusAliquotaEdit]);
end;

{ TNortusAliquotaEdit }

constructor TNortusAliquotaEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Configuração básica do controle
  Width := 120;
  Height := 25;
  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];

  // Inicializa propriedades
  FInternalUpdating := False;
  FMinValue := 0;
  FMaxValue := 999999.99;
  FMaxPercentual := 100;
  FStepValue := 0.01;
  FStepPercentual := 1;
  FReadOnly := False;
  FButtonEnabled := True;
  FTextPercentual := '%';
  FTextValor := 'R$';
  FDecimalPlaces := 2;
  FButtonWidth := 30;
  FAutoFormat := True;
  FAllowNegative := False;
  FTipo := taPercentual;

  // Criação do Edit
  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.TabOrder := 0;
  FEdit.BorderStyle := bsNone;
  FEdit.Alignment := taRightJustify;
  FEdit.OnChange := @EditChange;
  FEdit.OnKeyDown := @EditKeyDown;
  FEdit.OnKeyPress := @EditKeyPress;
  FEdit.OnExit := @EditExit;
  FEdit.OnEnter := @EditEnter;

  // Criação do botão
  FBtn := TSpeedButton.Create(Self);
  FBtn.Parent := Self;
  FBtn.Cursor := crHandPoint;
  FBtn.OnClick := @BtnClick;
  FBtn.Flat := False;

  // Define texto inicial do botão
  UpdateButtonText;

  // Posiciona os controles
  ArrangeControls;
end;

destructor TNortusAliquotaEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TNortusAliquotaEdit.CreateWnd;
begin
  inherited CreateWnd;
  ArrangeControls;
end;

procedure TNortusAliquotaEdit.Paint;
begin
  inherited Paint;

  // Desenha o fundo do controle
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);

  // Desenha a borda
  Canvas.Pen.Color := clBtnShadow;
  Canvas.Pen.Style := psSolid;
  Canvas.Rectangle(ClientRect);
end;

procedure TNortusAliquotaEdit.ArrangeControls;
var
  editWidth: Integer;
begin
  if not Assigned(FEdit) or not Assigned(FBtn) then Exit;
  if csDestroying in ComponentState then Exit;

  editWidth := ClientWidth - FButtonWidth - 1;

  // Posiciona o Edit com margem interna
  FEdit.SetBounds(2, 2, editWidth - 2, ClientHeight - 4);

  // Posiciona o botão
  FBtn.SetBounds(editWidth, 1, FButtonWidth, ClientHeight - 2);
end;

procedure TNortusAliquotaEdit.DoOnResize;
begin
  inherited DoOnResize;
  ArrangeControls;
  Invalidate;
end;

function TNortusAliquotaEdit.TryParseValue(const S: string; out V: Currency): Boolean;
var
  tmp: string;
  d: Extended;
begin
  Result := False;
  tmp := Trim(S);

  if tmp = '' then
  begin
    V := 0;
    Result := True;
    Exit;
  end;

  // Remove símbolos de moeda se presentes
  tmp := StringReplace(tmp, 'R$', '', [rfReplaceAll]);
  tmp := StringReplace(tmp, '%', '', [rfReplaceAll]);
  tmp := StringReplace(tmp, ' ', '', [rfReplaceAll]);
  tmp := Trim(tmp);

  // Tenta com as regras da localidade
  if TryStrToCurr(tmp, V) then
    Exit(True);

  // Tenta trocar vírgula por ponto
  tmp := StringReplace(tmp, ',', '.', [rfReplaceAll]);
  if TryStrToFloat(tmp, d) then
  begin
    V := d;
    Exit(True);
  end;

  Result := False;
end;

procedure TNortusAliquotaEdit.UpdateButtonText;
begin
  if not Assigned(FBtn) then Exit;

  if FTipo = taPercentual then
    FBtn.Caption := FTextPercentual
  else
    FBtn.Caption := FTextValor;

  FBtn.Hint := 'Clique para alternar entre percentual e valor';
end;

function TNortusAliquotaEdit.ValidateValue(var AValue: Currency): Boolean;
begin
  Result := True;

  if not FAllowNegative and (AValue < 0) then
  begin
    AValue := 0;
    Result := False;
  end;

  if FTipo = taPercentual then
  begin
    if AValue > FMaxPercentual then
    begin
      AValue := FMaxPercentual;
      Result := False;
    end;
    if AValue < 0 then
    begin
      AValue := 0;
      Result := False;
    end;
  end
  else
  begin
    if AValue > FMaxValue then
    begin
      AValue := FMaxValue;
      Result := False;
    end;
    if AValue < FMinValue then
    begin
      AValue := FMinValue;
      Result := False;
    end;
  end;
end;

procedure TNortusAliquotaEdit.FormatValue;
var
  v: Currency;
begin
  if not FAutoFormat then Exit;
  if FInternalUpdating then Exit;

  if TryParseValue(FEdit.Text, v) then
  begin
    ValidateValue(v);
    SetValue(v);
  end;
end;

function TNortusAliquotaEdit.GetStepValue: Currency;
begin
  if FTipo = taPercentual then
    Result := FStepPercentual
  else
    Result := FStepValue;
end;

procedure TNortusAliquotaEdit.EditEnter(Sender: TObject);
begin
  if not FReadOnly then
    FEdit.SelectAll;
end;

procedure TNortusAliquotaEdit.EditExit(Sender: TObject);
begin
  if FAutoFormat then
    FormatValue;
end;

procedure TNortusAliquotaEdit.EditKeyPress(Sender: TObject; var Key: Char);
var
  decSep: Char;
  hasDecimal: Boolean;
begin
  if FReadOnly then
  begin
    Key := #0;
    Exit;
  end;

  decSep := FormatSettings.DecimalSeparator;
  hasDecimal := (Pos(',', FEdit.Text) > 0) or (Pos('.', FEdit.Text) > 0);

  // Permite caracteres de controle
  if Key in [#8, #13, #27] then Exit;

  // Permite dígitos
  if (Key >= '0') and (Key <= '9') then Exit;

  // Permite sinal negativo apenas se permitido e no início
  if FAllowNegative and (Key = '-') and (FEdit.SelStart = 0) and (Pos('-', FEdit.Text) = 0) then
    Exit;

  // Permite separador decimal apenas uma vez
  if (Key = ',') or (Key = '.') then
  begin
    if not hasDecimal then
    begin
      Key := decSep;
      Exit;
    end;
  end;

  // Bloqueia todo o resto
  Key := #0;
end;

procedure TNortusAliquotaEdit.EditChange(Sender: TObject);
begin
  if FInternalUpdating then Exit;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNortusAliquotaEdit.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  v: Currency;
  step: Currency;
begin
  if FReadOnly then Exit;

  case Key of
    VK_UP, VK_DOWN:
      begin
        if FButtonEnabled and (ssCtrl in Shift) then
        begin
          // Ctrl + Setas = Alterna tipo
          ToggleTipo;
          Key := 0;
        end
        else
        begin
          // Setas = Incrementa/Decrementa
          if TryParseValue(FEdit.Text, v) then
          begin
            step := GetStepValue;
            if Key = VK_UP then
              v := v + step
            else
              v := v - step;

            ValidateValue(v);
            SetValue(v);
            if Assigned(FOnChange) then
              FOnChange(Self);
          end;
          Key := 0;
        end;
      end;
    VK_F2:
      begin
        if FButtonEnabled then
          ToggleTipo;
        Key := 0;
      end;
  end;
end;

procedure TNortusAliquotaEdit.BtnClick(Sender: TObject);
begin
  if not FButtonEnabled then Exit;
  ToggleTipo;
  if FEdit.CanFocus then
    FEdit.SetFocus;
end;

procedure TNortusAliquotaEdit.ToggleTipo;
var
  v: Currency;
begin
  if not TryParseValue(FEdit.Text, v) then
    v := 0;

  if FTipo = taPercentual then
    SetTipo(taValor)
  else
    SetTipo(taPercentual);

  SetValue(v);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNortusAliquotaEdit.SetTipo(const AValue: TTipoAliquota);
begin
  if FTipo = AValue then Exit;

  FTipo := AValue;
  UpdateButtonText;

  if FAutoFormat then
    FormatValue;
end;

procedure TNortusAliquotaEdit.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly = AValue then Exit;

  FReadOnly := AValue;

  if Assigned(FEdit) then
    FEdit.ReadOnly := AValue;
end;

procedure TNortusAliquotaEdit.SetButtonEnabled(const AValue: Boolean);
begin
  if FButtonEnabled = AValue then Exit;

  FButtonEnabled := AValue;

  if Assigned(FBtn) then
    FBtn.Enabled := AValue;
end;

procedure TNortusAliquotaEdit.SetTextPercentual(const AValue: string);
begin
  if FTextPercentual = AValue then Exit;

  FTextPercentual := AValue;

  if FTipo = taPercentual then
    UpdateButtonText;
end;

procedure TNortusAliquotaEdit.SetTextValor(const AValue: string);
begin
  if FTextValor = AValue then Exit;

  FTextValor := AValue;

  if FTipo = taValor then
    UpdateButtonText;
end;

procedure TNortusAliquotaEdit.SetButtonWidth(const AValue: Integer);
begin
  if FButtonWidth = AValue then Exit;
  if AValue < 20 then Exit; // Largura mínima

  FButtonWidth := AValue;
  ArrangeControls;
  Invalidate;
end;

procedure TNortusAliquotaEdit.SetMinValue(const AValue: Currency);
begin
  FMinValue := AValue;

  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
end;

procedure TNortusAliquotaEdit.SetMaxValue(const AValue: Currency);
begin
  FMaxValue := AValue;

  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
end;

procedure TNortusAliquotaEdit.SetMaxPercentual(const AValue: Currency);
begin
  if AValue < 0 then
    FMaxPercentual := 0
  else
    FMaxPercentual := AValue;
end;

procedure TNortusAliquotaEdit.SetDecimalPlaces(const AValue: Integer);
begin
  if (AValue >= 0) and (AValue <= 4) then
  begin
    FDecimalPlaces := AValue;

    if FAutoFormat then
      FormatValue;
  end;
end;

procedure TNortusAliquotaEdit.SetAutoFormat(const AValue: Boolean);
begin
  FAutoFormat := AValue;
end;

procedure TNortusAliquotaEdit.SetAllowNegative(const AValue: Boolean);
begin
  FAllowNegative := AValue;

  if not AValue and (GetValue < 0) then
    SetValue(0);
end;

function TNortusAliquotaEdit.GetValue: Currency;
begin
  if not TryParseValue(FEdit.Text, Result) then
    Result := 0;
end;

procedure TNortusAliquotaEdit.SetValue(AValue: Currency);
var
  formatStr: string;
begin
  ValidateValue(AValue);

  FInternalUpdating := True;
  try
    if FTipo = taPercentual then
    begin
      formatStr := FloatToStrF(AValue, ffFixed, 15, FDecimalPlaces);
      FEdit.Text := formatStr;
    end
    else
    begin
      formatStr := CurrToStrF(AValue, ffFixed, FDecimalPlaces);
      FEdit.Text := formatStr;
    end;
  finally
    FInternalUpdating := False;
  end;
end;

function TNortusAliquotaEdit.GetText: string;
begin
  if Assigned(FEdit) then
    Result := FEdit.Text
  else
    Result := '';
end;

procedure TNortusAliquotaEdit.SetText(const AValue: string);
begin
  if not Assigned(FEdit) then Exit;

  FInternalUpdating := True;
  try
    FEdit.Text := AValue;
  finally
    FInternalUpdating := False;
  end;
end;

procedure TNortusAliquotaEdit.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);

  if Assigned(FEdit) then
    FEdit.Enabled := Value;

  if Assigned(FBtn) then
    FBtn.Enabled := Value and FButtonEnabled;

  Invalidate;
end;

procedure TNortusAliquotaEdit.Clear;
begin
  if not Assigned(FEdit) then Exit;

  FInternalUpdating := True;
  try
    FEdit.Text := '';
  finally
    FInternalUpdating := False;
  end;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNortusAliquotaEdit.SelectAll;
begin
  if Assigned(FEdit) then
    FEdit.SelectAll;
end;

procedure TNortusAliquotaEdit.SetFocus;
begin
  if Assigned(FEdit) and FEdit.CanFocus then
    FEdit.SetFocus
  else
    inherited SetFocus;
end;

function TNortusAliquotaEdit.IsEmpty: Boolean;
begin
  if Assigned(FEdit) then
    Result := Trim(FEdit.Text) = ''
  else
    Result := True;
end;

function TNortusAliquotaEdit.IsValid: Boolean;
var
  v: Currency;
begin
  Result := TryParseValue(FEdit.Text, v);
end;

end.
