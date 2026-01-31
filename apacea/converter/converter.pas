program JSONToBinarySimple;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpjson, jsonparser;

type
  TEmployeeRecord = packed record
    ID: Integer;
    FirstName: string[30];
    LastName: string[30];
    Age: Byte;
    Email: string[50];
    Department: string[20];
    Position: string[40];
    Salary: Double;
    HireDate: string[10];
    Active: Boolean;
    EmployeeCode: string[10];
    City: string[30];
    ZipCode: string[10];
    Phone: string[20];
    EmergencyContact: string[20];
    PerformanceScore: Single;
    YearsExperience: Byte;
    PrimarySkill: string[20];
    SecondarySkill: string[20];
    CertificationCount: Byte;
    BonusEligible: Boolean;
    ManagerID: Integer;
    TrainingHours: Word;
    LastReviewDate: string[10];
  end;

function LoadJSONFromFile(const FileName: string): TJSONData;
var
  JSONStringList: TStringList;
  JSONString: string;
begin
  Result := nil;
  
  if not FileExists(FileName) then
  begin
    WriteLn('Error: JSON file "', FileName, '" not found!');
    Exit;
  end;
  
  JSONStringList := TStringList.Create;
  try
    JSONStringList.LoadFromFile(FileName);
    JSONString := JSONStringList.Text;
    
    try
      Result := GetJSON(JSONString);
      WriteLn('JSON file loaded successfully');
    except
      on E: Exception do
      begin
        WriteLn('JSON parsing error: ', E.Message);
        Result := nil;
      end;
    end;
  finally
    JSONStringList.Free;
  end;
end;

function JSONObjectToEmployee(JSONObj: TJSONObject): TEmployeeRecord;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  Result.ID := JSONObj.Get('id', 0);
  Result.FirstName := JSONObj.Get('first_name', '');
  Result.LastName := JSONObj.Get('last_name', '');
  Result.Age := JSONObj.Get('age', 0);
  Result.Email := JSONObj.Get('email', '');
  Result.Department := JSONObj.Get('department', '');
  Result.Position := JSONObj.Get('position', '');
  Result.Salary := JSONObj.Get('salary', 0.0);
  Result.HireDate := JSONObj.Get('hire_date', '');
  Result.Active := JSONObj.Get('active', True);
  Result.EmployeeCode := JSONObj.Get('employee_code', '');
  Result.City := JSONObj.Get('city', '');
  Result.ZipCode := JSONObj.Get('zip_code', '');
  Result.Phone := JSONObj.Get('phone', '');
  Result.EmergencyContact := JSONObj.Get('emergency_contact', '');
  Result.PerformanceScore := JSONObj.Get('performance_score', 0.0);
  Result.YearsExperience := JSONObj.Get('years_experience', 0);
  Result.PrimarySkill := JSONObj.Get('primary_skill', '');
  Result.SecondarySkill := JSONObj.Get('secondary_skill', '');
  Result.CertificationCount := JSONObj.Get('certification_count', 0);
  Result.BonusEligible := JSONObj.Get('bonus_eligible', False);
  Result.ManagerID := JSONObj.Get('manager_id', 0);
  Result.TrainingHours := JSONObj.Get('training_hours', 0);
  Result.LastReviewDate := JSONObj.Get('last_review_date', '');
end;

procedure ConvertJSONToBinary(const JSONFileName, BinaryFileName: string);
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  BinaryFile: File of TEmployeeRecord;
  Employee: TEmployeeRecord;
  i: Integer;
  JSONObj: TJSONObject;
begin
  WriteLn('Converting JSON to Binary...');
  
  JSONData := LoadJSONFromFile(JSONFileName);
  if JSONData = nil then
    Exit;
  
  try
    if JSONData.JSONType <> jtArray then
    begin
      WriteLn('Error: JSON root must be an array');
      Exit;
    end;
    
    JSONArray := TJSONArray(JSONData);
    WriteLn('Found ', JSONArray.Count, ' records');
    
    Assign(BinaryFile, BinaryFileName);
    Rewrite(BinaryFile);
    
    for i := 0 to JSONArray.Count - 1 do
    begin
      if JSONArray.Items[i].JSONType = jtObject then
      begin
        JSONObj := TJSONObject(JSONArray.Items[i]);
        Employee := JSONObjectToEmployee(JSONObj);
        Write(BinaryFile, Employee);
        
        if (i + 1) mod 50 = 0 then
          WriteLn('Processed ', i + 1, ' records...');
      end;
    end;
    
    Close(BinaryFile);
    WriteLn('Conversion completed! Records: ', JSONArray.Count);
    
  finally
    JSONData.Free;
  end;
end;

procedure TestBinaryFile(const BinaryFileName: string);
var
  BinaryFile: File of TEmployeeRecord;
  Employee: TEmployeeRecord;
  Count: Integer;
begin
  WriteLn('Testing binary file...');
  
  if not FileExists(BinaryFileName) then
  begin
    WriteLn('Binary file not found');
    Exit;
  end;
  
  Assign(BinaryFile, BinaryFileName);
  Reset(BinaryFile);
  
  Count := 0;
  while not EOF(BinaryFile) do
  begin
    Read(BinaryFile, Employee);
    Inc(Count);
    
    if Count <= 3 then
    begin
      WriteLn('Record ', Count, ': ', Employee.FirstName, ' ', Employee.LastName);
      WriteLn('  Department: ', Employee.Department);
      WriteLn('  Salary: $', Employee.Salary:0:2);
    end;
  end;
  
  Close(BinaryFile);
  WriteLn('Test completed. Total records: ', Count);
end;

begin
  WriteLn('JSON to Binary Converter');
  WriteLn('========================');
  
  ConvertJSONToBinary('employee_data.json', 'employee_data.dat');
  TestBinaryFile('employee_data.dat');
  
  WriteLn('Done!');
end.
