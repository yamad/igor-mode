Sub ReloadPackage_unload(ByRef IgorApp, packageName)
        quoted_packageName = """" & packageName & """"
        If IsPackageLoaded(IgorApp, packageName) Then
                addReloadMark IgorApp, packageName
                IgorApp.Execute("Unload" + packageName + "Package()")
                WScript.Sleep 500
        End If
End Sub

Sub ReloadPackage_load(ByRef IgorApp, packageName)
        quoted_packageName = """" & packageName & """"
        If IsReloadMarkExists(IgorApp, packageName) Then
                deleteReloadMark IgorApp, packageName
                IgorApp.Execute("Load" + packageName + "Package()")
        End If
End Sub

mark_path = "root:"
mark_suffix = "_RELOAD"
Sub addReloadMark(ByRef IgorApp, packageName)
        mark_varname = packageName & mark_suffix
        addVariable IgorApp, mark_path, mark_varname, 1
End Sub

Sub deleteReloadMark(ByRef IgorApp, packageName)
        mark_varname = packageName & mark_suffix
        deleteVariable IgorApp, mark_path, mark_varname
End Sub

Function IsReloadMarkExists(IgorApp, packageName)
        mark_varname = packageName & mark_suffix
        Set df = IgorApp.DataFolder(mark_path)
        Set vars = df.Variables
        If vars.VariableExists(mark_varname) Then
                IsReloadMarkExists = True
        Else
                IsReloadMarkExists = False
        End If
End Function