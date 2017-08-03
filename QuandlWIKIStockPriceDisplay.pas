unit QuandlWIKIStockPriceDisplay;
{Jennifer Gallegos @ 8/3/2017}
{Interface to process Quandl WIKI Stock Price data with SecurityUtilityUnit methods.}

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  Vcl.StdCtrls, VCL.ComCtrls, System.Net.URLClient,
  SecurityUtilityUnit, VCL.Dialogs, System.IOUtils, MidasLib;

type
  TStockPriceStatisticsForm = class(TForm)
    NetHTTPRequest1: TNetHTTPRequest;
    NetHTTPClient1: TNetHTTPClient;
    Run: TButton;
    ResultsDisplay: TMemo;
    LblSecurities: TLabel;
    SecuritiesList: TEdit;
    MaxDailyProfit: TCheckBox;
    BusyDay: TCheckBox;
    BiggestLoser: TCheckBox;
    StartPick: TDateTimePicker;
    EndPick: TDateTimePicker;
    DateStart: TLabel;
    DateEnd: TLabel;
    MonthlyAvg: TCheckBox;
    procedure RunClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StockPriceStatisticsForm : TStockPriceStatisticsForm;
  securities : TSecuritiesUtility;
  //list if fields to extract from Quandl
  dbFields   : Array of String = ['ticker','date','open','close','high','low','volume'];

implementation

{$R *.dfm}
//Press Run Quandl button to run the program
procedure TStockPriceStatisticsForm.RunClick(Sender: TObject);
var
    quandlData : IHTTPResponse;
    quandlQry  : string;
    quandlCSVString : string;
    tickList : TStringList;
    resultList : TList;
    item, flds : string;
    j : integer;

procedure LoadMemo(title : string);
var
    i : integer;
begin
    if resultList <> nil then begin
        ResultsDisplay.Lines.Add(title);
        for i := 0 to resultList.count-1 do
            ResultsDisplay.Lines.Add(TStringList(resultList[i]).DelimitedText);
        end;
end;

begin
    //exit program if no checkboxes are chosen, no methods were run
    if not MonthlyAvg.Checked and not MaxDailyProfit.Checked and
       not BusyDay.Checked and not BiggestLoser.Checked then begin
        ResultsDisplay.Lines.Add('Choose at least one checkbox and press the '+
          '"Run Quandl" button to display a calculated a result.');
        ResultsDisplay.Lines.Add('Hover over each checkbox to learn more.');
        exit;
        end;

    //convert list of fields from an array to a string
    flds := dbFields[0];
    for j := 1 to Length(dbFields)-1 do
        flds := flds + ',' + dbFields[j];
    //prepare request for data from Quandl API
    quandlQry := 'https://www.quandl.com/api/v3/datatables/WIKI/PRICES.csv?date.gte=$STRT$&date.lte=$END$&ticker=$TLIST$&qopts.columns='+flds+'&api_key=s-GMZ_xkw6CrkGYUWs1p';
    quandlQry := quandlQry.Replace('$TLIST$',SecuritiesList.Text);
    quandlQry := quandlQry.Replace('$STRT$',FormatDateTime('yyyymmdd',StartPick.DateTime));
    quandlQry := quandlQry.Replace('$END$',FormatDateTime('yyyymmdd',EndPick.DateTime));
  try
    //retrieve data from the internet
    quandlData := NetHTTPRequest1.Get(quandlQry);
    securities.LoadDataSetfromCSV(quandlData.ContentAsString());
  except
    //if online data not available, prompt for a file
    with TOpenDialog.Create(self) do begin
        if Execute then begin
            Caption := 'Select CSV File';
            quandlCSVString := TFile.ReadAllText(FileName);
            securities.LoadDataSetfromCSV(quandlCSVString);
            end
        else begin
            raise Exception.Create('No Data Selected.');
            end;
        end;
  end;

    tickList := TStringList.Create;
    tickList.DelimitedText := SecuritiesList.Text;
    ResultsDisplay.Clear;
    //loop through all security ticker codes chosen by the user
    for item in ticklist do begin
        //calculate statistics requested (checked) by the user

        if MonthlyAvg.Checked then begin
            resultList := securities.MonthlyAverageValue(item, StartPick.DateTime,EndPick.DateTime);
            LoadMemo('Monthly Average Open and Close Values for '+item);
            ResultsDisplay.Lines.Add('');
            end;

        if MaxDailyProfit.Checked then begin
            resultList := securities.MaxDailyProfit(item);
            LoadMemo('Highest Profit Date for '+item);
            ResultsDisplay.Lines.Add('');
            end;

        if BusyDay.Checked then begin
            resultList := securities.BusyDay(item, 10);
            if resultList <> nil then
                LoadMemo(IntToStr(resultList.Count) +
                  ' Days with Volume over 10% of the Average for '+item);
            ResultsDisplay.Lines.Add('');
            end;

        ResultsDisplay.Lines.Add('*********************************************************');
        end; //For each ticker code

    if BiggestLoser.Checked then begin
        resultList := securities.BiggestLoser;
        if resultList <> nil then
            LoadMemo(TStringList(resultList[0])[0] +
               ' Has the most days ('+TStringList(resultList[0])[1]+
               ') where closing price is less than opening price.');
        end;
end;

procedure TStockPriceStatisticsForm.FormActivate(Sender: TObject);
begin
    securities := TSecuritiesUtility.Create(dbFields);
end;

procedure TStockPriceStatisticsForm.FormDestroy(Sender: TObject);
begin
    securities.Destroy;
end;

end.
