unit QuandlWIKIStockPriceDisplay;
{Jennifer Gallegos @ 8/3/2017}
{Interface to process Quandl WIKI Stock Price data.}

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  Vcl.StdCtrls, VCL.ComCtrls, System.Net.URLClient,
  SecurityUtilityUnit;

type
  TForm2 = class(TForm)
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
  Form2      : TForm2;
  quandlQry  : String;
  securities : TSecuritiesUtility;
  dbFields   : Array of String = ['ticker','date','open','close','high','low','volume'];

implementation

{$R *.dfm}

procedure TForm2.RunClick(Sender: TObject);
var
    quandlData : IHTTPResponse;
    tickList : TStringList;
    resultList : TList;
    item, flds : string;
    j : integer;

procedure LoadMemo(title : string);
var
    i : integer;
begin
    if resultList = nil then exit;

    ResultsDisplay.Lines.Add(title);
    for i := 0 to resultList.count-1 do begin
        ResultsDisplay.Lines.Add(TStringList(resultList[i]).DelimitedText);
        end;
end;

begin
    if not MonthlyAvg.Checked and not MaxDailyProfit.Checked and
       not BusyDay.Checked and not BiggestLoser.Checked then begin
        ResultsDisplay.Lines.Add('Chose at least one checkbox and press the '+
          '"Run Quandl" button to display a calculated a result.');
        ResultsDisplay.Lines.Add('Hover over each checkbox to learn more.');
        exit;
        end;

    flds := dbFields[0];
    for j := 1 to Length(dbFields)-1 do
        flds := flds + ',' + dbFields[j];

    quandlQry := 'https://www.quandl.com/api/v3/datatables/WIKI/PRICES.csv?date.gte=$STRT$&date.lte=$END$&ticker=$TLIST$&qopts.columns='+flds+'&api_key=s-GMZ_xkw6CrkGYUWs1p';
    quandlQry := quandlQry.Replace('$TLIST$',SecuritiesList.Text);
    quandlQry := quandlQry.Replace('$STRT$',FormatDateTime('yyyymmdd',StartPick.DateTime));
    quandlQry := quandlQry.Replace('$END$',FormatDateTime('yyyymmdd',EndPick.DateTime));
//??? if text is blank, will get this error {"quandl_error":{"code":"QESx02","message":"You are missing a value after operator 'eq'."}}
  try
    quandlData := NetHTTPRequest1.Get(quandlQry);
//??? what if internet is not connected?    ENETURIException of quandlQry is blank

    securities.LoadDataSetfromCSV(quandlData.ContentAsString());
  except
     //Message about internet connection, or no data?  bad data?
  end;

    tickList := TStringList.Create;
    tickList.DelimitedText := SecuritiesList.Text;
    ResultsDisplay.Clear;
    for item in ticklist do begin
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

procedure TForm2.FormActivate(Sender: TObject);
begin
    securities := TSecuritiesUtility.Create(dbFields);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
    securities.Destroy;
end;

end.
