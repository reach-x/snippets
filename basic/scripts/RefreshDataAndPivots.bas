Attribute VB_Name = "Module1"
Sub RefreshDataAndPivots()
    Dim ws As Worksheet
    Dim pt As PivotTable
    Dim lastUpdatedCell As Range
    Dim btn As Button
    
    ' Reference the button on the Dashboard sheet by its name
    Set btn = Sheets("Configuration").Buttons("Button 3")
    
    ' Change the button text to "Running Update" and disable it
    btn.Caption = "Running Update"
    btn.Enabled = False
  
      
    ' Refresh all data connections
    ' Set ws = ThisWorkbook.Sheets("SMP Dataset")
    ' Clear all content in the sheet
    ' ws.Cells.Clear
    
    ' Refresh the Power Query
     ' ThisWorkbook.Connections("SMP Data").Refresh
    
    ' Loop through all worksheets and pivot tables to refresh them
    For Each ws In ThisWorkbook.Worksheets
        On Error Resume Next
        For Each pt In ws.PivotTables
            pt.RefreshTable
        Next pt
        On Error GoTo 0
    Next ws
    
    ' Update the LAST_UPDATED cell
    Set lastUpdatedCell = ThisWorkbook.Names("LAST_UPDATED").RefersToRange
    lastUpdatedCell.Value = Now
    
    Set lastDatasetCell = ThisWorkbook.Names("LAST_DATASET").RefersToRange
    lastDatasetCell.Value = ThisWorkbook.Names("START_DATE").RefersToRange _
        & " TO " _
        & ThisWorkbook.Names("END_DATE").RefersToRange
    
    ' Restore the button text to "Update" and re-enable it
    btn.Caption = "Update"
    btn.Enabled = True
End Sub

