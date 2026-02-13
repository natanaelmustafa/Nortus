unit NortusEditAliquota;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, LMessages, Forms, LCLType,
  LCLIntf, Spin;

type
  TMarcadorTipo = (mtReais, mtPorcentagem);

  TNortusEditAliquota = class(TFloatSpinEdit)
  private
    FTipoMarcador: TMarcadorTipo;
    FPodeAlternar: Boolean;
    FRetanguloMarcador: TRect;

    procedure DefinirTipoMarcador(const NovoTipo: TMarcadorTipo);
    function ObterTextoMarcador: string;
    procedure CalcularPosicaoMarcador;
    function CliqueNaMarcador(X, Y: Integer): Boolean;
    procedure TrocarMarcador;

  protected
    procedure WMPaint(var Mensagem: TLMPaint); message LM_PAINT;
    procedure MouseDown(Botao: TMouseButton; Modificadores: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Tecla: Word; Modificadores: TShiftState); override;
    procedure Resize; override;

  public
    constructor Create(Proprietario: TComponent); override;

  published
    property MarcadorTipo: TMarcadorTipo read FTipoMarcador write DefinirTipoMarcador default mtReais;
    property PermiteAlternarMarcador: Boolean read FPodeAlternar write FPodeAlternar default True;
  end;

procedure Register;

implementation

{ TNortusEditAliquota }

constructor TNortusEditAliquota.Create(Proprietario: TComponent);
begin
  inherited Create(Proprietario);

  // Configurações iniciais
  FTipoMarcador := mtReais;
  FPodeAlternar := True;

  // Configurar como editor de moeda
  DecimalPlaces := 2;
  MinValue := 0;
  MaxValue := 999999999;
  Increment := 1;

  // Configurações de aparência
  Width := 150;
  Height := 25;
  Alignment := taRightJustify;

  // Inicializar área do marcador
  FRetanguloMarcador := Rect(0, 0, 0, 0);
end;

procedure TNortusEditAliquota.DefinirTipoMarcador(const NovoTipo: TMarcadorTipo);
begin
  if FTipoMarcador <> NovoTipo then
  begin
    FTipoMarcador := NovoTipo;
    CalcularPosicaoMarcador;
    Invalidate;
  end;
end;

function TNortusEditAliquota.ObterTextoMarcador: string;
begin
  case FTipoMarcador of
    mtReais: Result := 'R$';
    mtPorcentagem: Result := '%';
  else
    Result := 'R$';
  end;
end;

procedure TNortusEditAliquota.CalcularPosicaoMarcador;
var
  TextoMarcador: string;
  LarguraTexto, AlturaTexto: Integer;
  CanvasTemp: TCanvas;
begin
  if not HandleAllocated then Exit;

  CanvasTemp := TCanvas.Create;
  try
    CanvasTemp.Handle := GetDC(Handle);
    CanvasTemp.Font.Assign(Font);

    TextoMarcador := ObterTextoMarcador;
    LarguraTexto := CanvasTemp.TextWidth(TextoMarcador);
    AlturaTexto := CanvasTemp.TextHeight(TextoMarcador);

    FRetanguloMarcador.Left := Width - LarguraTexto - 12;
    FRetanguloMarcador.Top := (Height - AlturaTexto) div 2;
    FRetanguloMarcador.Right := Width - 4;
    FRetanguloMarcador.Bottom := FRetanguloMarcador.Top + AlturaTexto;

    ReleaseDC(Handle, CanvasTemp.Handle);
  finally
    CanvasTemp.Free;
  end;
end;

function TNortusEditAliquota.CliqueNaMarcador(X, Y: Integer): Boolean;
begin
  Result := (X >= FRetanguloMarcador.Left) and (X <= FRetanguloMarcador.Right) and
            (Y >= FRetanguloMarcador.Top) and (Y <= FRetanguloMarcador.Bottom);
end;

procedure TNortusEditAliquota.TrocarMarcador;
begin
  if FPodeAlternar then
  begin
    if FTipoMarcador = mtReais then
      DefinirTipoMarcador(mtPorcentagem)
    else
      DefinirTipoMarcador(mtReais);
  end;
end;

procedure TNortusEditAliquota.WMPaint(var Mensagem: TLMPaint);
var
  TextoMarcador: string;
  AreaMarcador: TRect;
  DC: HDC;
  CanvasTemp: TCanvas;
begin
  // Desenhar o componente base primeiro
  inherited;

  // Desenhar o marcador por cima
  if HandleAllocated then
  begin
    CalcularPosicaoMarcador;

    DC := GetDC(Handle);
    try
      CanvasTemp := TCanvas.Create;
      try
        CanvasTemp.Handle := DC;

        TextoMarcador := ObterTextoMarcador;

        // Desenhar fundo do marcador
        CanvasTemp.Brush.Color := clBtnFace;
        AreaMarcador := FRetanguloMarcador;
        InflateRect(AreaMarcador, 1, 0);
        CanvasTemp.FillRect(AreaMarcador);

        // Desenhar texto do marcador
        CanvasTemp.Font.Assign(Font);
        CanvasTemp.Font.Color := clWindowText;
        CanvasTemp.Brush.Style := bsClear;
        CanvasTemp.TextOut(FRetanguloMarcador.Left + 2, FRetanguloMarcador.Top, TextoMarcador);

        // Desenhar borda do marcador
        CanvasTemp.Pen.Color := clBtnShadow;
        CanvasTemp.Pen.Style := psSolid;
        CanvasTemp.Rectangle(AreaMarcador);

        CanvasTemp.Handle := 0;
      finally
        CanvasTemp.Free;
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TNortusEditAliquota.MouseDown(Botao: TMouseButton; Modificadores: TShiftState; X, Y: Integer);
begin
  if (Botao = mbLeft) and CliqueNaMarcador(X, Y) then
  begin
    TrocarMarcador;
    Exit;
  end;

  inherited MouseDown(Botao, Modificadores, X, Y);
end;

procedure TNortusEditAliquota.KeyDown(var Tecla: Word; Modificadores: TShiftState);
begin
  if FPodeAlternar and (Modificadores = []) then
  begin
    if (Tecla = VK_UP) or (Tecla = VK_DOWN) then
    begin
      TrocarMarcador;
      Tecla := 0;
      Exit;
    end;
  end;

  inherited KeyDown(Tecla, Modificadores);
end;

procedure TNortusEditAliquota.Resize;
begin
  inherited Resize;
  CalcularPosicaoMarcador;
  Invalidate;
end;

// Registro do componente
procedure Register;
begin
  RegisterComponents('Nortus', [TNortusEditAliquota]);
end;

initialization
  Register;

end.
