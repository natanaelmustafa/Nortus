{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NortusPack;

{$warn 5023 off : no warning about unused units}
interface

uses
  uNortusTabs, uNortusAliquotaEdit, NortusSearchList, uLKComboBox, 
  FlexiSwitch, outsourced, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uNortusTabs', @uNortusTabs.Register);
  RegisterUnit('uNortusAliquotaEdit', @uNortusAliquotaEdit.Register);
  RegisterUnit('NortusSearchList', @NortusSearchList.Register);
  RegisterUnit('uLKComboBox', @uLKComboBox.Register);
  RegisterUnit('FlexiSwitch', @FlexiSwitch.Register);
end;

initialization
  RegisterPackage('NortusPack', @Register);
end.
