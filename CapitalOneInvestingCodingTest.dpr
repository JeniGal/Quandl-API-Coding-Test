program CapitalOneInvestingCodingTest;

uses
  Vcl.Forms,
  QuandlWIKIStockPriceDisplay in 'QuandlWIKIStockPriceDisplay.pas' {Form2},
  SecurityUtilityUnit in 'SecurityUtilityUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
