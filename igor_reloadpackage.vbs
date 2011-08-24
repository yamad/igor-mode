Sub ReloadPackage_unload(ByRef IgorApp, packageName)
        quoted_packageName = """" & packageName & """"
        If IsPackageLoaded(IgorApp, packageName) Then
                addReloadPackageMark IgorApp, packageName
                IgorApp.Execute("Unload" + packageName + "Package()")
                WScript.Sleep 500
        End If
End Sub

Sub ReloadPackage_load(ByRef IgorApp, packageName)
        quoted_packageName = """" & packageName & """"
        If IsReloadPackageMarkExists(IgorApp, packageName) Then
                deleteReloadPackageMark IgorApp, packageName
                IgorApp.Execute("Load" + packageName + "Package()")
        End If
End Sub

mark_path = "root:"
mark_suffix = "_RELOAD_PACKAGE"
Sub addReloadPackageMark(ByRef IgorApp, packageName)
        mark_varname = packageName & mark_suffix
        addVariable IgorApp, mark_path, mark_varname, 1
End Sub

Sub deleteReloadPackageMark(ByRef IgorApp, packageName)
        mark_varname = packageName & mark_suffix
        deleteVariable IgorApp, mark_path, mark_varname
End Sub

Function IsReloadPackageMarkExists(IgorApp, packageName)
        mark_varname = packageName & mark_suffix
        Set df = IgorApp.DataFolder(mark_path)
        Set vars = df.Variables
        If vars.VariableExists(mark_varname) Then
                IsReloadPackageMarkExists = True
        Else
                IsReloadPackageMarkExists = False
        End If
End Function