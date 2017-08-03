program CapitalOneInvestingCodingTest;

uses
  Vcl.Forms,
  QuandlWIKIStockPriceDisplay in 'QuandlWIKIStockPriceDisplay.pas' {StockPriceStatisticsForm},
  SecurityUtilityUnit in 'SecurityUtilityUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TStockPriceStatisticsForm, StockPriceStatisticsForm);
  Application.Run;
end.
