unit uNortusTabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ImgList, Types, LCLType, ComCtrls;

type
  TUnderlineMode = (umNone, umActiveOnly, umAll);
  TNortusTabActivatedEvent = procedure (Sender: TObject; Index: Integer; const ACaption: string) of object;

  { TNortusTabs }
  TNortusTabs = class(TCustomControl)
  private
    FLinkedPageControl: TPageControl;
    FImages: TImageList;

    // Aparência
    FActiveTextColor: TColor;
    FInactiveTextColor: TColor;
    FHoverTextColor: TColor;
    FPressedTextColor: TColor;
    FUseHoverColor: Boolean;

    FActiveTabBGColor: TColor;
    FInactiveTabBGColor: TColor;
    FPageBGColor: TColor;

    FUnderlineMode: TUnderlineMode;
    FUnderlineActiveColor: TColor;
    FUnderlineInactiveColor: TColor;
    FUnderlineThickness: Integer;

    FIconTextSpacing: Integer;
    FTabHPadding: Integer;
    FTabVPadding: Integer;
    FTabSpacing: Integer;
    FTabCornerRadius: Integer;

    // Estado
    FHotTab: Integer;
    FPressedTab: Integer;

    // Eventos
    FOnTabActivated: TNortusTabActivatedEvent;
    FOldPCOnChange: TNotifyEvent;

    procedure SetLinkedPageControl(const AValue: TPageControl);
    procedure SetImages(const AValue: TImageList);
    procedure SetPageBGColor(const AValue: TColor);
    procedure PCChanged(Sender: TObject);

    function MinI(a, b: Integer): Integer; inline;
    function MaxI(a, b: Integer): Integer; inline;

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function  TabRectByIndex(Index: Integer): TRect;
    function  HitTestTab(X, Y: Integer): Integer;
    function  TabCaption(Index: Integer): string;
    function  TabImageIndex(Index: Integer): Integer;
    procedure ApplyPageBGColor;

  public
    constructor Create(AOwner: TComponent); override;

    procedure ActivateTab(Index: Integer);
    procedure SetTabEnabled(Index: Integer; AEnabled: Boolean);
    procedure SetTabVisible(Index: Integer; AVisible: Boolean);

  published
    property LinkedPageControl: TPageControl read FLinkedPageControl write SetLinkedPageControl;
    property Images: TImageList read FImages write SetImages;

    property ActiveTextColor: TColor read FActiveTextColor write FActiveTextColor default clWindowText;
    property InactiveTextColor: TColor read FInactiveTextColor write FInactiveTextColor default clGrayText;
    property HoverTextColor: TColor read FHoverTextColor write FHoverTextColor default clHighlight;
    property PressedTextColor: TColor read FPressedTextColor write FPressedTextColor default clHighlight;
    property UseHoverColor: Boolean read FUseHoverColor write FUseHoverColor default True;

    property ActiveTabBGColor: TColor read FActiveTabBGColor write FActiveTabBGColor default clBtnFace;
    property InactiveTabBGColor: TColor read FInactiveTabBGColor write FInactiveTabBGColor default clBtnFace;
    property PageBGColor: TColor read FPageBGColor write SetPageBGColor default clWindow;

    property UnderlineMode: TUnderlineMode read FUnderlineMode write FUnderlineMode default umActiveOnly;
    property UnderlineActiveColor: TColor read FUnderlineActiveColor write FUnderlineActiveColor default clHighlight;
    property UnderlineInactiveColor: TColor read FUnderlineInactiveColor write FUnderlineInactiveColor default clBtnShadow;
    property UnderlineThickness: Integer read FUnderlineThickness write FUnderlineThickness default 3;

    property IconTextSpacing: Integer read FIconTextSpacing write FIconTextSpacing default 6;
    property TabHPadding: Integer read FTabHPadding write FTabHPadding default 14;
    property TabVPadding: Integer read FTabVPadding write FTabVPadding default 10;
    property TabSpacing: Integer read FTabSpacing write FTabSpacing default 10;
    property TabCornerRadius: Integer read FTabCornerRadius write FTabCornerRadius default 6;

    property OnTabActivated: TNortusTabActivatedEvent read FOnTabActivated write FOnTabActivated;

    // propriedades padrão
    property Align;
    property Anchors;
    property Color default clBtnFace;
    property Font;
    property ParentColor;
    property ParentFont;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Nortus', [TNortusTabs]);
end;

{ TNortusTabs }

constructor TNortusTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csOpaque];

  Color := clBtnFace;
  Height := 56;
  Width  := 400;

  FActiveTextColor   := clWindowText;
  FInactiveTextColor := clGrayText;
  FHoverTextColor    := clHighlight;
  FPressedTextColor  := clHighlight;
  FUseHoverColor     := True;

  FActiveTabBGColor   := clBtnFace;
  FInactiveTabBGColor := clBtnFace;
  FPageBGColor := clWindow;

  FUnderlineMode          := umActiveOnly;
  FUnderlineActiveColor   := clHighlight;
  FUnderlineInactiveColor := clBtnShadow;
  FUnderlineThickness     := 3;

  FIconTextSpacing := 6;
  FTabHPadding     := 14;
  FTabVPadding     := 10;
  FTabSpacing      := 10;
  FTabCornerRadius := 6;

  FHotTab := -1;
  FPressedTab := -1;
  FOldPCOnChange := nil;
end;

procedure TNortusTabs.SetLinkedPageControl(const AValue: TPageControl);
begin
  if FLinkedPageControl = AValue then Exit;

  if Assigned(FLinkedPageControl) then
  begin
    FLinkedPageControl.OnChange := FOldPCOnChange;
    FOldPCOnChange := nil;
  end;

  FLinkedPageControl := AValue;

  if Assigned(FLinkedPageControl) then
  begin
    FOldPCOnChange := FLinkedPageControl.OnChange;
    FLinkedPageControl.OnChange := @PCChanged;
    ApplyPageBGColor;
  end;

  Invalidate;
end;

procedure TNortusTabs.SetImages(const AValue: TImageList);
begin
  if FImages = AValue then Exit;
  FImages := AValue;
  Invalidate;
end;

procedure TNortusTabs.SetPageBGColor(const AValue: TColor);
begin
  if FPageBGColor = AValue then Exit;
  FPageBGColor := AValue;
  ApplyPageBGColor;
  Invalidate;
end;

procedure TNortusTabs.ApplyPageBGColor;
begin
  if (FLinkedPageControl <> nil) and (FLinkedPageControl.ActivePage <> nil) then
    FLinkedPageControl.ActivePage.Color := FPageBGColor;
end;

procedure TNortusTabs.PCChanged(Sender: TObject);
var ix: Integer; ACaption: string;
begin
  ApplyPageBGColor;
  if Assigned(FOldPCOnChange) then
    FOldPCOnChange(Sender);

  if (FLinkedPageControl <> nil) and (FLinkedPageControl.ActivePage <> nil) then
  begin
    ix  := FLinkedPageControl.ActivePage.PageIndex;
    ACaption := FLinkedPageControl.ActivePage.Caption;
    if Assigned(FOnTabActivated) then
      FOnTabActivated(Self, ix, ACaption);
  end;

  Invalidate;
end;

procedure TNortusTabs.Resize;
begin
  inherited Resize;
  Invalidate;
end;

function TNortusTabs.MinI(a, b: Integer): Integer; inline;
begin
  if a < b then Result := a else Result := b;
end;

function TNortusTabs.MaxI(a, b: Integer): Integer; inline;
begin
  if a > b then Result := a else Result := b;
end;

function TNortusTabs.TabCaption(Index: Integer): string;
begin
  if (FLinkedPageControl <> nil) and (Index >= 0) and (Index < FLinkedPageControl.PageCount) then
    Result := FLinkedPageControl.Pages[Index].Caption
  else
    Result := '';
end;

function TNortusTabs.TabImageIndex(Index: Integer): Integer;
begin
  if (FLinkedPageControl <> nil) and (Index >= 0) and (Index < FLinkedPageControl.PageCount) then
    Result := FLinkedPageControl.Pages[Index].ImageIndex
  else
    Result := -1;
end;

function TNortusTabs.TabRectByIndex(Index: Integer): TRect;
var
  C: TCanvas;
  Txt: string;
  TextW: Integer;
  ImgW: Integer;
  ContentW: Integer;
  x: Integer;
  i: Integer;
  imgIdx: Integer;
begin
  C := Canvas;
  C.Font.Assign(Font);
  x := FTabSpacing;

  for i := 0 to Index do
  begin
    Txt   := TabCaption(i);
    TextW := C.TextWidth(Txt);

    imgIdx := TabImageIndex(i);
    if Assigned(FImages) and (imgIdx >= 0) then ImgW := FImages.Width else ImgW := 0;

    ContentW := TextW + 2*FTabHPadding;
    if ImgW > 0 then
      ContentW := ContentW + ImgW + FIconTextSpacing;

    if i < Index then
      Inc(x, ContentW + FTabSpacing)
    else
    begin
      Result := Rect(x, FTabVPadding div 2, x + ContentW, Height - FTabVPadding);
      Exit;
    end;
  end;

  Result := Rect(FTabSpacing, FTabVPadding div 2, FTabSpacing + 100, Height - FTabVPadding);
end;

function TNortusTabs.HitTestTab(X, Y: Integer): Integer;
var
  i: Integer;
  R: TRect;
begin
  Result := -1;
  if (FLinkedPageControl = nil) then Exit;
  for i := 0 to FLinkedPageControl.PageCount - 1 do
  begin
    R := TabRectByIndex(i);
    if PtInRect(R, Point(X, Y)) then
      Exit(i);
  end;
end;

procedure TNortusTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHot: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  NewHot := HitTestTab(X, Y);
  if NewHot <> FHotTab then
  begin
    FHotTab := NewHot;
    if FUseHoverColor then Invalidate;
  end;
end;

procedure TNortusTabs.MouseLeave;
begin
  inherited MouseLeave;
  if FHotTab <> -1 then
  begin
    FHotTab := -1;
    if FUseHoverColor then Invalidate;
  end;
end;

procedure TNortusTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FPressedTab := HitTestTab(X, Y);
    if FPressedTab >= 0 then Invalidate;
  end;
end;

procedure TNortusTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ix: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    ix := HitTestTab(X, Y);
    if (ix >= 0) and (FLinkedPageControl <> nil) then
      ActivateTab(ix);
    FPressedTab := -1;
    Invalidate;
  end;
end;

procedure TNortusTabs.ActivateTab(Index: Integer);
begin
  if (FLinkedPageControl <> nil) and
     (Index >= 0) and (Index < FLinkedPageControl.PageCount) and
     (FLinkedPageControl.Pages[Index].Enabled) and
     (FLinkedPageControl.Pages[Index].TabVisible) then
  begin
    FLinkedPageControl.ActivePageIndex := Index;
  end;
end;

procedure TNortusTabs.SetTabEnabled(Index: Integer; AEnabled: Boolean);
begin
  if (FLinkedPageControl <> nil) and (Index >= 0) and (Index < FLinkedPageControl.PageCount) then
  begin
    FLinkedPageControl.Pages[Index].Enabled := AEnabled;
    Invalidate;
  end;
end;

procedure TNortusTabs.SetTabVisible(Index: Integer; AVisible: Boolean);
begin
  if (FLinkedPageControl <> nil) and (Index >= 0) and (Index < FLinkedPageControl.PageCount) then
  begin
    FLinkedPageControl.Pages[Index].TabVisible := AVisible;
    Invalidate;
  end;
end;

procedure TNortusTabs.Paint;
var
  i: Integer;
  C: TCanvas;
  R, RLine: TRect;
  Txt: string;
  TextW, TextH: Integer;
  ImgIdx: Integer;
  ImgW, ImgH: Integer;
  DrawX, DrawY: Integer;
  Active: Boolean;
  ColText: TColor;
  NeedUnderline: Boolean;
  spacingAfterIcon: Integer;
  contentW: Integer;
  bgCol: TColor;
begin
  C := Canvas;

  C.Brush.Style := bsSolid;
  C.Brush.Color := Color;
  C.FillRect(ClientRect);

  if (FLinkedPageControl = nil) or (FLinkedPageControl.PageCount = 0) then Exit;

  C.Font.Assign(Font);

  for i := 0 to FLinkedPageControl.PageCount - 1 do
  begin
    R := TabRectByIndex(i);
    Active := (i = FLinkedPageControl.ActivePageIndex);

    if Active then bgCol := FActiveTabBGColor else bgCol := FInactiveTabBGColor;
    C.Brush.Color := bgCol;
    C.Pen.Style := psClear;
    C.RoundRect(R.Left, R.Top, R.Right, R.Bottom, FTabCornerRadius, FTabCornerRadius);

    if (i = FPressedTab) then
      ColText := FPressedTextColor
    else if FUseHoverColor and (i = FHotTab) and (not Active) then
      ColText := FHoverTextColor
    else if Active then
      ColText := FActiveTextColor
    else
      ColText := FInactiveTextColor;

    Txt   := TabCaption(i);
    TextW := C.TextWidth(Txt);
    TextH := C.TextHeight(Txt);

    ImgIdx := TabImageIndex(i);
    if Assigned(FImages) and (ImgIdx >= 0) and (ImgIdx < FImages.Count) then
    begin
      ImgW := FImages.Width; ImgH := FImages.Height;
    end
    else
    begin
      ImgIdx := -1; ImgW := 0; ImgH := 0;
    end;

    if ImgW > 0 then spacingAfterIcon := FIconTextSpacing else spacingAfterIcon := 0;

    contentW := TextW + ImgW + spacingAfterIcon;
    DrawX := R.Left + (R.Right - R.Left - contentW) div 2;

    if ImgIdx >= 0 then
    begin
      DrawY := R.Top + (R.Bottom - R.Top - ImgH) div 2 - 1;
      FImages.Draw(C, DrawX, DrawY, ImgIdx, True);
      Inc(DrawX, ImgW + spacingAfterIcon);
    end;

    C.Brush.Style := bsClear;
    C.Font.Color := ColText;
    DrawY := R.Top + (R.Bottom - R.Top - TextH) div 2 - 1;
    C.TextOut(DrawX, DrawY, Txt);

    NeedUnderline := (FUnderlineMode = umAll) or ((FUnderlineMode = umActiveOnly) and Active);
    if NeedUnderline and (FUnderlineThickness > 0) then
    begin
      C.Pen.Style := psSolid;
      C.Pen.Width := FUnderlineThickness;
      if Active then C.Pen.Color := FUnderlineActiveColor
                else C.Pen.Color := FUnderlineInactiveColor;

      RLine := R;
      C.MoveTo(RLine.Left + 6, RLine.Bottom - MaxI(1, FUnderlineThickness));
      C.LineTo(RLine.Right - 6, RLine.Bottom - MaxI(1, FUnderlineThickness));
    end;
  end;
end;

end.

