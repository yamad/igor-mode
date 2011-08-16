Function IsPackageLoaded(ByRef IgorApp, packageName)
        full_packagePath = "root:Packages:" & packageName
        If IgorApp.DataFolderExists(full_packagePath) Then
                IsPackageLoaded = True
        Else
                IsPackageLoaded = False
        End If
End Function

Function ConnectToIgorProServer()
        Dim IgorApp
        On Error Resume Next
        Set IgorApp = GetObject(, "IgorPro.Application")
        If Not Err.Number = 429 Then
                Set ConnectToIgorProServer = IgorApp
        Else
                Set ConnectToIgorProServer = Nothing
        End If
End Function

Sub addVariable(ByRef IgorApp, path, varName, varValue)
        typeDouble = 4
        Set df = IgorApp.DataFolder(path)
        Set vars = df.Variables
        Set v = vars.Add(varName, typeDouble, 1)
        v.SetNumericValue varValue, 0
End Sub

Sub deleteVariable(ByRef IgorApp, path, varName)
        Set df = IgorApp.DataFolder(path)
        Set vars = df.Variables
        For i = 0 To vars.Count - 1
                Set v = vars.Item(i)
                If v.Name = varName Then
                        vars.Remove i
                End If
        Next
End Sub