unit SecurityUtilityUnit;
{Jennifer Gallegos @ 8/3/2017}
{Utility routines for Stock pricing data stored in a TDataSet.}

interface

uses
  System.Classes, System.Generics.Collections,
  System.SysUtils, Data.DB, DataSnap.DBClient;

type

  TTickerCnt = array of record
               Tkr: String;
               Cnt : Integer;
               end;

  TSecuritiesUtility = class (TObject)
    private
      fldTicker         : string;
      fldTrxDate        : string;
      fldOpen, fldClose : string;
      fldHigh, fldLow   : string;
      fldVolume         : string;
      securitiesDataInt    : TClientDataSet;
      function GetSecuritiesData: TClientDataSet;
    public
      constructor Create(flds : Array of String);
      property SecuritiesData : TClientDataSet read GetSecuritiesData;
      procedure LoadFields(flds : Array of String);
      function LoadDataSetfromCSV(csvString : string): boolean;
      function AverageValue(ds : TDataSet; fldAvg : string): Extended;
      function MonthlyAverageValue(ticker : string; startDt, endDt : TDateTime) : TList;
      function MaxDailyProfit(ticker : string) : TList;
      function BusyDay(ticker : string; thresholdPct : extended): TList;
      function BiggestLoser: TList;
    end;

implementation

{ SecuritiesUtility }

{Create SecuritiesUtility by passing the fields needed to create the dataset.
  If fields are unknown at the time of creation, use LoadFields method.
  LoadFields could be called to load the headers when the data is imported.
@param flds - fields names in order of the expected data import
  flds are expected in this order: ticker,date,open,close,high,low,volume}
constructor TSecuritiesUtility.Create(flds : Array of String);
begin
    inherited Create;
    securitiesDataInt    := TClientDataSet.Create(nil);

    if Length(flds) <> 0 then
        LoadFields(flds);
end;

{if field names were not available at Creation, use LoadFields to initialize.
  Most likely to be done from headers when importing the data.
@param flds - Field Names in order of the expected data import.
  flds are expected in this order: ticker,date,open,close,high,low,volume}
procedure TSecuritiesUtility.LoadFields(flds : Array of String);
begin
    if SecuritiesData.Active or (SecuritiesData.FieldDefs.Count > 0) then begin
        SecuritiesData.Close;
        SecuritiesData.FieldDefs.Clear;   //???what if fielddefs already exist? Best way to re-set?  What if dataset is active?
        end;

    fldTicker  := flds[0];
    fldTrxDate := flds[1];
    fldOpen    := flds[2];
    fldClose   := flds[3];
    fldHigh    := flds[4];
    fldLow     := flds[5];
    fldVolume  := flds[6];

    SecuritiesData.FieldDefs.Add(fldTicker,ftString,10,true);
    SecuritiesData.FieldDefs.Add(fldTrxDate,ftDate,0,true);
    SecuritiesData.FieldDefs.Add(fldOpen,ftExtended,0,false);
    SecuritiesData.FieldDefs.Add(fldClose,ftExtended,0,false);
    SecuritiesData.FieldDefs.Add(fldHigh,ftExtended,0,false);
    SecuritiesData.FieldDefs.Add(fldLow,ftExtended,0,false);
    SecuritiesData.FieldDefs.Add(fldVolume,ftExtended,0,false);
end;

{@return - SecuritiesData TClientDataSet}
function TSecuritiesUtility.GetSecuritiesData: TClientDataSet;
begin
     Result := securitiesDataInt;
end;

{Loads data from the string into an internal TClientDataSet
@Param data - expected format for data is a csv converted to a string.
  The first line of the csv should contain headers. }
function TSecuritiesUtility.LoadDataSetfromCSV(csvString : string): boolean;
var
    dataList, itemList : TStringList;
    year, month, day : string;
    i, j : integer;
begin
    dataList := TStringList.Create;
    dataList.Delimiter := #$A;
    dataList.DelimitedText := csvString;

    if not SecuritiesData.Active then
        SecuritiesData.CreateDataSet
    else
        SecuritiesData.EmptyDataSet;

    itemList := TStringList.Create;
    //init SecuritiesData with values from csvString
    for i := 1 to dataList.Count-1 do begin  //headers have already been loaded, skip them
        SecuritiesData.Append;              //??? code option to load headers here from first row, if not yet done
        itemList.DelimitedText := dataList[i];
        for j := 0 to itemList.Count-1 do begin
            if SecuritiesData.FieldDefs[j].DataType = ftDate then begin
                //date values come in as yyyy-mm-dd
                year := Copy(itemList[j],0,itemList[j].IndexOf('-'));
                month := Copy(itemList[j],itemList[j].IndexOf('-')+2,2);
                day := Copy(itemList[j],itemList[j].LastIndexOf('-')+2,2);
                SecuritiesData.Fields[j].AsString := month+'/'+day+'/'+year;
                end
            else
                SecuritiesData.Fields[j].AsString := itemList[j];
            end;
        SecuritiesData.Post;
        end;
end;

{Returns the average value (mean) of all the fields in the fldAvg column
@param ds - DataSet of values to be averaged
@param fldAvg - Field Name for the mean calculation
@result - mean of all fldAvg values  }
function TSecuritiesUtility.AverageValue(ds: TDataSet; fldAvg: string): Extended;
var
    sum : extended;

begin
    if (not ds.Active) or (ds.RecordCount <= 0) then
        raise Exception.Create('Cannot divide by zero.');     //???
    sum := 0;
    ds.First;
    while not ds.Eof do begin
        sum := sum + ds.FieldByName(fldAvg).AsExtended;
        ds.Next;
        end;
    Result := sum/ds.RecordCount;
end;

{Returns the average monthly close and open prices for each month
  within the given date range for the requested ticker within SecuritiesData.
  Average value will only be returned for each month in the data, so if a
  partial month is sent, only the average for this partial month will be returned.
  SecuritiesData can be filtered if a smaller set of data is required.
@param ticker - the security ticker symbol requested
@param startDt - start date for the monthly average
@param endDt - end date for the monthly average
@result - returns a TList in the following format for each unique ticker/month:
  ticker,'month:'month,'average_open:'averageOpen,'average_close:'averageClose
  returns nil if there is no data or the dataset is not active}
function TSecuritiesUtility.MonthlyAverageValue(ticker : string; startDt, endDt : TDateTime) : TList;
var
    saveFilter : string;
    curMonth, curYear, endMonth, endYear, day : word;
    ix : integer;

begin
    if SecuritiesData.Filtered then
        saveFilter := SecuritiesData.Filter;
    SecuritiesData.Filtered := False;
    SecuritiesData.Filter := '(ticker='+QuotedStr(ticker)+')';
    SecuritiesData.Filtered := True;

    if (not SecuritiesData.Active) or (SecuritiesData.RecordCount <= 0) or
       (startDt > endDt) then begin
        Result := nil;
        exit;
        end;

    Result := TList.Create;
    try
      DecodeDate(endDt, endYear, endMonth, day);
      DecodeDate(startDt, curYear, curMonth, day);
      while (curYear <= endYear) and
            (curMonth <= endMonth) do begin
          SecuritiesData.Filtered := False;
          //filter - calculate only using the requested Month and date range
          SecuritiesData.Filter := '(ticker='+QuotedStr(ticker)+')'
                                  +' and (Year(date)='+curYear.ToString+')'
                                  +' and (Month(date)='+curMonth.ToString+')'
                                  +' and (date>='+QuotedStr(DateToStr(startDt))+')'
                                  +' and (date<='+QuotedStr(DateToStr(endDt))+')';
          SecuritiesData.Filtered := True;

          SecuritiesData.First;
          if SecuritiesData.RecordCount > 0 then begin
              //Load Return Result data
              ix := Result.Add(TStringList.Create);
              TStringList(Result[ix]).add(
                SecuritiesData.FieldByName(fldTicker).AsString);
              TStringList(Result[ix]).add(
                'month:'+curYear.ToString+'-'+Format('%.2d',[curMonth]));
              TStringList(Result[ix]).add(
                'average_open:'+Format('%.2f',[AverageValue(SecuritiesData,fldOpen)]));
              TStringList(Result[ix]).add(
                'average_close:'+Format('%.2f',[AverageValue(SecuritiesData,fldClose)]));
              end;

          //increment the month and year
          curMonth := curMonth + 1;
          if curMonth > 12 then begin
              curYear  := curYear + 1;
              curMonth := 1;
              end;
          end; //while months exist within range
    finally
      //if no data was found within the month range requested, return nil
      if Result.Count <= 0 then
          Result := nil;
      // re-attach the original filter
      if saveFilter > '' then begin
          SecuritiesData.Filtered := False;
          SecuritiesData.Filter := saveFilter;
          SecuritiesData.Filtered := True;
          end
      else begin
          SecuritiesData.Filter := '';
          SecuritiesData.Filtered := false;
          end;
    end;
end;

{Returns the day where the difference between the low price and high price
  provides the greatest profit.
  SecuritiesData can be filtered if a smaller set of data is required.
@param ticker - the security ticker symbol requested
@result - returns a TList in the following format for the largest profit found:
  ticker,'date:'profitDate,'profit:'profitValue
  returns nil if there is no data or the dataset is not active}
function TSecuritiesUtility.MaxDailyProfit(ticker : string) : TList;
var
    curDif, profit : extended;
    proDate : TDateTime;
    proTick : string;
    ix : integer;
    savefilter : string;
begin
    if SecuritiesData.Filtered then
        saveFilter := SecuritiesData.Filter;
    SecuritiesData.Filtered := False;
    SecuritiesData.Filter := '(ticker='+QuotedStr(ticker)+')';
    SecuritiesData.Filtered := True;

    if (not SecuritiesData.Active) or (SecuritiesData.RecordCount <= 0) then begin
        Result := nil;
        exit;
        end;

    Result := TList.Create;
    SecuritiesData.First;
    profit := 0;
    curDif := 0;
    while not SecuritiesData.Eof do begin
        curDif := SecuritiesData.FieldByName(fldHigh).AsExtended -
                  SecuritiesData.FieldByName(fldLow).AsExtended;
        //profit is always the largest positive difference between high and low
        if profit < curDif then begin
            profit  := curDif;
            proTick := SecuritiesData.FieldByName(fldTicker).AsString;
            proDate := SecuritiesData.FieldByName(fldTrxDate).AsDateTime;
            end;
        SecuritiesData.Next;
        end;

    //Load Return Result data
    ix := Result.Add(TStringList.Create);
    TStringList(Result[ix]).add(proTick);
    TStringList(Result[ix]).add('date:'+FormatDateTime('mm/dd/yyyy',proDate));
    TStringList(Result[ix]).add('profit:'+profit.ToString);

    //if no data was found within the month range requested, return nil
    if Result.Count <= 0 then
        Result := nil;
    // re-attach the original filter
    if saveFilter > '' then begin
        SecuritiesData.Filtered := False;
        SecuritiesData.Filter := saveFilter;
        SecuritiesData.Filtered := True;
        end
    else begin
        SecuritiesData.Filter := '';
        SecuritiesData.Filtered := false;
        end;
end;

{Returns the days where the volume is more than thresholdPct higher than
  the data's average volume for a single security ticker code.
  Also returns the total average volume value calculated. A negative threshold
  will return all volumes greater than a percentage less than the average volume.
  For example, -5 will return all volumes greater than 95% of the average volume.
  SecuritiesData can be filtered if a smaller set of data is required.
@param ticker - the security ticker symbol requested
@param thresholdPct - the percentage that determines the cutoff for the list of
  volumes returned.  This value is in percent format, so if requesting all
  volumes greater than 10% of the average, enter 10.
@result - returns a TList in the following format for the volumes found:
  ticker,'date:'volumeDate,'volume:'volumeValue,'average_volume:'avgVolume
  returns nil if there is no data or the dataset is not active}
function TSecuritiesUtility.BusyDay(ticker : string; thresholdPct: extended): TList;
var
    ix : integer;
    avgVolume, avgVolumePct, sum : extended;
    savefilter : string;

begin
    if SecuritiesData.Filtered then
        saveFilter := SecuritiesData.Filter;
    SecuritiesData.Filtered := False;
    SecuritiesData.Filter := 'ticker='+QuotedStr(ticker);
    SecuritiesData.Filtered := True;

    if (not SecuritiesData.Active) or (SecuritiesData.RecordCount <= 0) then begin
        Result := nil;
        exit;
        end;

    Result := TList.Create;
    sum := 0;
    SecuritiesData.First;
    //calculate the overall average volume and thresholdPct more than average
    while not SecuritiesData.Eof do begin
        sum := sum + SecuritiesData.FieldByName(fldVolume).AsExtended;
        SecuritiesData.Next;
        end;
    avgVolume := sum / SecuritiesData.RecordCount;
    avgVolumePct := avgVolume*(1+thresholdPct/100);

    SecuritiesData.First;
    while not SecuritiesData.Eof do begin
        if SecuritiesData.FieldByName(fldVolume).AsExtended > avgVolumePct then begin
            //Load Return Result data
            ix := Result.Add(TStringList.Create);
            TStringList(Result[ix]).add(SecuritiesData.FieldByName(fldTicker).AsString);
            TStringList(Result[ix]).add('date:'+SecuritiesData.FieldByName(fldTrxDate).AsString);
            TStringList(Result[ix]).add('volume:'+SecuritiesData.FieldByName(fldVolume).AsString);
            TStringList(Result[ix]).add(//'average_volume:'+avgVolume.ToString);
              'average_volume:'+Format('%.2f',[avgVolume]));
            end;
        SecuritiesData.Next;
        end;

    //if no data was found within the month range requested, return nil
    if Result.Count <= 0 then
        Result := nil;
    // re-attach the original filter
    if saveFilter > '' then begin
        SecuritiesData.Filtered := False;
        SecuritiesData.Filter := saveFilter;
        SecuritiesData.Filtered := True;
        end
    else begin
        SecuritiesData.Filter := '';
        SecuritiesData.Filtered := false;
        end;
end;

{Returns the security ticker that has the most days where the closing price is
  less than the opening price.  Also returns the total number of losing days.
  SecuritiesData can be filtered if a smaller set of data is required.
@result - returns a TList in the following format for the loser found:
  ticker,'days:'totalLosingDays
  returns nil if there is no data or the dataset is not active}
function TSecuritiesUtility.BiggestLoser: TList;
var
    losers : TTickerCnt;
    tkr : string;
    tkrIx, lasti, maxi, i : integer;
    worstVal, worstIx, ix : integer;
begin
    if (not SecuritiesData.Active) or (SecuritiesData.RecordCount <= 0) then begin
        Result := nil;
        exit;
        end;

    Result := TList.Create;
    SetLength(losers,1000);
    lasti := 0;
    maxi := 0;

    SecuritiesData.First;
    while not SecuritiesData.Eof do begin
        tkr := SecuritiesData.FieldByName(fldTicker).AsString;
        //always point to the correct ticker for incrementing
        if losers[lasti].Tkr <> tkr then begin
            tkrIx := low (losers);
            while (tkrIx <= maxi) and (losers [tkrIx].Tkr <> tkr) do
                Inc(tkrIx);
            if (tkrIx > maxi) then begin
                maxi := tkrIx;
                losers[tkrIx].Tkr := tkr;
                end;
            lasti := tkrIx;
            end
        else
            tkrIx := lasti;
        //per ticker, increment count when close price is less than open price
        if SecuritiesData.FieldByName(fldClose).AsExtended <
           SecuritiesData.FieldByName(fldOpen).AsExtended then
            Inc(losers[tkrIx].Cnt);
        SecuritiesData.Next;
        end;
    worstVal := 0;
    worstIx := 0;
    //locate the ticker with the highest count (most days where close < open)
    for i := low(losers) to high(losers) do begin
        if worstVal < losers[i].cnt then begin
            worstVal := losers[i].cnt;
            worstIx := i;
            end;
        end;
    //Load Return Result data
    ix := Result.Add(TStringList.Create);
    TStringList(Result[ix]).add(losers[worstIx].Tkr);
    TStringList(Result[ix]).add('days:'+IntToStr(losers[worstIx].Cnt));
end;

end.
