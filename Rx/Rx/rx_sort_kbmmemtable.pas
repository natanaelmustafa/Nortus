{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rx_sort_kbmmemtable;

{$warn 5023 off : no warning about unused units}
interface

uses
  rx_sort_kbmmt, RxSortKbmMemTable, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RxSortKbmMemTable', @RxSortKbmMemTable.Register);
end;

initialization
  RegisterPackage('rx_sort_kbmmemtable', @Register);
end.
