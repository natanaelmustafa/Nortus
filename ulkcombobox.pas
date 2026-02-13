unit uLKComboBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Graphics, LCLType, LMessages, LCLIntf, Forms;

const
  CB_GETTOPINDEX = $015b;

type
  // 🔹 TLKComboBox: ComboBox customizado que trunca texto e mostra tooltip com texto completo
  TLKComboBox = class(TComboBox)
  private
    FHintWindow: THintWindow;
    FLastHintVisible: Boolean;
    procedure ShowHintWindow(const AText: string; X, Y: Integer);
    procedure HideHintWindow;
    procedure UpdateSelectedHint(X, Y: Integer);
  protected
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Nortus', [TLKComboBox]);
end;

{ TLKComboBox }

constructor TLKComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;  // 🔹 necessário para truncamento e desenho manual
  FHintWindow := THintWindow.Create(Self);
  FLastHintVisible := False;
end;

destructor TLKComboBox.Destroy;
begin
  HideHintWindow;
  FHintWindow.Free;
  inherited Destroy;
end;

procedure TLKComboBox.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  TextToDraw: string;
  bgColor, fontColor: TColor;
begin
  // 🔹 Define cores conforme o estado e respeita a fonte do Inspetor
  Canvas.Font.Assign(Font);
  Canvas.Brush.Color := Color;

  if odSelected in State then
  begin
    bgColor := clHighlight;
    fontColor := clHighlightText;
  end
  else
  begin
    bgColor := Color;
    fontColor := Font.Color;
  end;

  Canvas.Brush.Color := bgColor;
  Canvas.FillRect(ARect);
  Canvas.Font.Color := fontColor;

  // 🔹 Trunca o texto se necessário
  TextToDraw := Items[Index];
  while (Canvas.TextWidth(TextToDraw) > (ARect.Width - 6)) and (Length(TextToDraw) > 1) do
  begin
    Delete(TextToDraw, Length(TextToDraw), 1);
    if Canvas.TextWidth(TextToDraw + '...') <= (ARect.Width - 6) then
    begin
      TextToDraw := TextToDraw + '...';
      Break;
    end;
  end;

  // 🔹 Desenha o texto centralizado verticalmente
  Canvas.TextOut(
    ARect.Left + 2,
    ARect.Top + ((ARect.Height - Canvas.TextHeight(TextToDraw)) div 2),
    TextToDraw
  );
end;

procedure TLKComboBox.ShowHintWindow(const AText: string; X, Y: Integer);
var
  HintRect: TRect;
begin
  HideHintWindow;
  if AText = '' then Exit;

  HintRect := FHintWindow.CalcHintRect(200, AText, nil);
  OffsetRect(HintRect, X + 10, Y + 20);
  FHintWindow.ActivateHint(HintRect, AText);
  FLastHintVisible := True;
end;

procedure TLKComboBox.HideHintWindow;
begin
  if FLastHintVisible then
  begin
    FHintWindow.Hide;
    FLastHintVisible := False;
  end;
end;

procedure TLKComboBox.UpdateSelectedHint(X, Y: Integer);
var
  TextToDraw: string;
  AvailableWidth: Integer;
begin
  if DroppedDown or (ItemIndex < 0) then
  begin
    HideHintWindow;
    Exit;
  end;

  TextToDraw := Items[ItemIndex];
  AvailableWidth := Width - 18; // margem para o botão dropdown

  if Canvas.TextWidth(TextToDraw) > AvailableWidth then
    ShowHintWindow(TextToDraw, ClientOrigin.X, ClientOrigin.Y)
  else
    HideHintWindow;
end;

procedure TLKComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  UpdateSelectedHint(X, Y);
end;

procedure TLKComboBox.MouseLeave;
begin
  inherited MouseLeave;
  HideHintWindow;
end;

end.

