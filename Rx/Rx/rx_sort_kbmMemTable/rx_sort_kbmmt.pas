{ RxDBGrid sort engine module for FBDataSet

  Copyright (C) 2005-2021 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rx_sort_kbmmt;

{$I rx.inc}

interface

uses
  Classes, SysUtils, DB, RxDBGrid;

type

  { TKBMMemTableSortEngine }

  TKBMMemTableSortEngine = class(TRxDBGridSortEngine)
  protected
  public
    procedure Sort(FieldName: string; ADataSet:TDataSet; Asc:boolean; SortOptions:TRxSortEngineOptions);override;
    procedure SortList(ListField:string; ADataSet:TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions);override;
  end;


implementation
uses kbmMemTable;

{ TKBMMemTableSortEngine }

procedure TKBMMemTableSortEngine.Sort(FieldName: string; ADataSet: TDataSet;
  Asc: boolean; SortOptions: TRxSortEngineOptions);
var
  D: TkbmMemTableCompareOptions;
begin
  D:=[];
  if not Asc then
    D:=D + [mtcoDescending];
  if Assigned(ADataSet) then
    (ADataSet as TkbmMemTable).SortOn(FieldName, D);
end;

procedure TKBMMemTableSortEngine.SortList(ListField: string;
  ADataSet: TDataSet; Asc: array of boolean; SortOptions: TRxSortEngineOptions);
var
  S: String;
  C: SizeInt;
  i: Integer;
begin
  if not Assigned(ADataSet) then exit;

  S:='';
  C:=Pos(';', ListField);
  i:=0;
  while C>0 do
  begin
    if S<>'' then S:=S+';';
    S:=S + {FixFieldName(}Copy(ListField, 1, C-1){)};
    Delete(ListField, 1, C);

    if (i<=High(Asc)) and (not Asc[i]) then
      S:=S + ':D';
    C:=Pos(';', ListField);
    inc(i);
  end;

  if ListField<>'' then
  begin
    if S<>'' then S:=S+';';
    S:=S + {FixFieldName(}ListField{)};
    if (i<=High(Asc)) and (not Asc[i]) then
      S:=S + ':D';
  end;

  (ADataSet as TkbmMemTable).SortOn(S, [mtcoCaseInsensitive]);
end;


initialization
  RegisterRxDBGridSortEngine(TKBMMemTableSortEngine, 'TkbmMemTable');
end.

