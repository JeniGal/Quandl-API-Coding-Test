unit TestBusyDayObj;

interface
uses
  DUnitX.TestFramework,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  SecurityUtilityUnit, System.Classes, System.SysUtils;

type

  [TestFixture]
  TTestBusyDayObj = class(TObject)
  strict private
    aNetHTTPRequest: TNetHTTPRequest;
    aNetHTTPClient: TNetHTTPClient;
    aSecurityUtil : TSecuritiesUtility;
    aCSVData : string;
    procedure InitData(newpath : string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [TestCase('BadChars'  ,'..--@@,15')]
    [TestCase('BadTicker' ,'BADVALUE,10')]
//    [TestCase('InvalidPct','COF,-5.06.0')]
    procedure TestBusyDay_BadParam(tkr : string; pct : extended);

    [TestCase('FifteenPct' ,'COF,15')]
    [TestCase('DecimalPct' ,'MSFT,17.53')]
    [TestCase('DecimalPct' ,'COF,75')]
    [TestCase('DecimalPct' ,'GOOGL,150')]
    [TestCase('FivePctLess','COF,-5')]
    [TestCase('ZeroPct'    ,'GOOGL,0')]
    procedure TestBusyDay_ThresholdPct(tkr : string; pct : extended);
  end;

implementation

procedure TTestBusyDayObj.InitData(newpath: string);
begin
    aCSVData := aNetHTTPRequest.Get(newPath).ContentAsString();
    aSecurityUtil.LoadDataSetfromCSV(aCSVData);

    Assert.IsTrue(aSecurityUtil.SecuritiesData.Active,'DataSet is not active');
    Assert.IsTrue(aSecurityUtil.SecuritiesData.RecordCount > 0,'DataSet has no Records');
end;

procedure TTestBusyDayObj.Setup;
begin
    aSecurityUtil := TSecuritiesUtility.Create(['ticker','date','open','close','high','low','volume']);
    aNetHTTPRequest := TNetHTTPRequest.Create(nil);
    aNetHTTPClient := TNetHTTPClient.Create(nil);
    aNetHTTPRequest.Client := aNetHTTPClient;
    InitData('https://www.quandl.com/api/v3/datatables'+
      '/WIKI/PRICES.csv?date.gte=20170101&date.lte=20170630&ticker=COF,GOOGL,MSFT'+
      '&qopts.columns=ticker,date,open,close,high,low,volume&'+
      'api_key=s-GMZ_xkw6CrkGYUWs1p');
end;

procedure TTestBusyDayObj.TearDown;
begin
    aSecurityUtil.Destroy;
    aNetHTTPClient.Destroy;
    aNetHTTPRequest.Destroy;
end;

procedure TTestBusyDayObj.TestBusyDay_BadParam(tkr : string; pct : extended);
var
    testList : TList;
begin
    testList := aSecurityUtil.BusyDay(tkr,pct);
    Assert.IsNull(testList,tkr + FloatToStr(pct) + ': should have returned nil.');
end;

procedure TTestBusyDayObj.TestBusyDay_ThresholdPct(tkr : string; pct : extended);
var
    testList : TList;
    volume, tmp : string;
    avgVolume : string;
    i : integer;
begin
    testList := aSecurityUtil.BusyDay(tkr,pct);
    for i := 0 to testList.Count-1 do begin
        tmp := TStringList(testList[i])[2];
        volume := Copy(tmp, Pos(':',tmp)+1, Length(tmp)-Pos(':',tmp)+1);
        tmp := TStringList(testList[i])[3];
        avgVolume := Copy(tmp, Pos(':',tmp)+1, Length(tmp)-Pos(':',tmp)+1);
        Assert.IsTrue(StrToFloat(volume)>StrToFloat(avgVolume)*(1+pct/100),
          'Failed:'+TStringList(testList[i]).DelimitedText);
        end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBusyDayObj);
end.
