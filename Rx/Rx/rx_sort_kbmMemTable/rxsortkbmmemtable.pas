unit RxSortKbmMemTable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRxSortKbmMemTable = class(TComponent)
  private

  protected

  public

  published

  end;

procedure Register;

implementation
uses rx_sort_kbmmt;

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxSortKbmMemTable]);
end;

end.
