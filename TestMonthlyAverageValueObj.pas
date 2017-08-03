unit TestMonthlyAverageValueObj;

interface
uses
  DUnitX.TestFramework,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  SecurityUtilityUnit, System.Classes, System.SysUtils;

type

  [TestFixture]
  TTestMonthlyAverageValueObj = class(TObject)
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

    [Test]
    procedure TestMonthlyAverageValue_EndBeforeStart;
    [Test]
    procedure TestMonthlyAverageValue_BadTicker;
    [Test]
    procedure TestMonthlyAverageValue_OneMonth;
    [Test]
    procedure TestMonthlyAverageValue_PartialMonth;
    [Test]
    procedure TestMonthlyAverageValue_MultiPartialMonths;
  end;

implementation

procedure TTestMonthlyAverageValueObj.InitData(newpath: string);
begin
    aCSVData := aNetHTTPRequest.Get(newPath).ContentAsString();
    aSecurityUtil.LoadDataSetfromCSV(aCSVData);

    Assert.IsTrue(aSecurityUtil.SecuritiesData.Active,'DataSet is not active');
    Assert.IsTrue(aSecurityUtil.SecuritiesData.RecordCount > 0,'DataSet has no Records');
end;

procedure TTestMonthlyAverageValueObj.Setup;
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

procedure TTestMonthlyAverageValueObj.TearDown;
begin
    aSecurityUtil.Destroy;
    aNetHTTPClient.Destroy;
    aNetHTTPRequest.Destroy;
end;

procedure TTestMonthlyAverageValueObj.TestMonthlyAverageValue_BadTicker;
var
    testList : TList;
begin
    testList := aSecurityUtil.MonthlyAverageValue('BADVALUE',StrToDate('1/1/17'),StrToDate('1/31/17'));
    Assert.IsNull(testList,'Using a bad security ticker code should have returned nil.');
end;

procedure TTestMonthlyAverageValueObj.TestMonthlyAverageValue_EndBeforeStart;
var
    testList : TList;
begin
    testList := aSecurityUtil.MonthlyAverageValue('MSFT',StrToDate('1/30/17'),StrToDate('1/1/17'));
    Assert.IsNull(testList,'End Date before Start Date should have returned nil.');
end;

procedure TTestMonthlyAverageValueObj.TestMonthlyAverageValue_OneMonth;
var
    testList : TList;
begin
    testList := aSecurityUtil.MonthlyAverageValue('GOOGL',StrToDate('3/1/17'),StrToDate('3/31/17'));
    Assert.AreEqual(TStringList(testList[0])[2],'average_open:853.86',
      TStringList(testList[0])[2] + '<> average_open:853.86 -> Calculation Error');
    Assert.AreEqual(TStringList(testList[0])[3],'average_close:853.79',
      TStringList(testList[0])[3] + '<> average_open:853.79 -> Calculation Error');
end;

procedure TTestMonthlyAverageValueObj.TestMonthlyAverageValue_PartialMonth;
var
    testList : TList;
    startDt, endDt : string;
begin
    testList := aSecurityUtil.MonthlyAverageValue('GOOGL',StrToDate('3/5/17'),StrToDate('3/15/17'));
    Assert.AreEqual(TStringList(testList[0])[2],'average_open:857.02',
      'GOOGL avg open -> Calculation Error');
    Assert.AreEqual(TStringList(testList[0])[3],'average_close:858.77',
      'GOOGL avg close -> Calculation Error');
end;

procedure TTestMonthlyAverageValueObj.TestMonthlyAverageValue_MultiPartialMonths;
var
    testList : TList;
begin
    testList := aSecurityUtil.MonthlyAverageValue('COF',StrToDate('2/5/17'),StrToDate('5/8/17'));
    //Month of February for COF
    Assert.AreEqual(TStringList(testList[0])[0],'COF',
      TStringList(testList[0])[0] + '<> COF -> Ticker Not Expected');
    Assert.AreEqual(TStringList(testList[0])[2],'average_open:90.28',
      TStringList(testList[0])[2] + '<> average_open:90.28 -> Calculation Error');
    Assert.AreEqual(TStringList(testList[0])[3],'average_close:90.75',
      TStringList(testList[0])[3] + '<> average_open:90.75 -> Calculation Error');

    testList := aSecurityUtil.MonthlyAverageValue('GOOGL',StrToDate('2/5/17'),StrToDate('5/8/17'));
    //Month of May for GOOGL
    Assert.AreEqual(TStringList(testList[3])[0],'GOOGL',
      TStringList(testList[3])[0] + '<> GOOGL -> Ticker Not Expected');
    Assert.AreEqual(TStringList(testList[3])[2],'average_open:941.32',
      TStringList(testList[3])[2] + '<> average_open:941.32 -> Calculation Error');
    Assert.AreEqual(TStringList(testList[3])[3],'average_close:947.01',
      TStringList(testList[3])[3] + '<> average_open:947.01 -> Calculation Error');

    testList := aSecurityUtil.MonthlyAverageValue('MSFT',StrToDate('2/5/17'),StrToDate('5/8/17'));
    //Month of March for MSFT
    Assert.AreEqual(TStringList(testList[1])[0],'MSFT',
      TStringList(testList[1])[0] + '<> MSFT -> Ticker Not Expected');
    Assert.AreEqual(TStringList(testList[1])[2],'average_open:64.76',
      TStringList(testList[1])[2] + '<> average_open:64.76 -> Calculation Error');
    Assert.AreEqual(TStringList(testList[1])[3],'average_close:64.84',
      TStringList(testList[1])[3] + '<> average_open:64.84 -> Calculation Error');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMonthlyAverageValueObj);
end.
