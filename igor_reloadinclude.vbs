Sub ReloadInclude_unload(ByRef IgorApp, includeName)
        quoted_includeName = """" & includeName & """"
        If IsIncludeDefined(IgorApp, includeName) Then
                addReloadIncludeMark IgorApp, includeName
                deleteIncludeVariable IgorApp, includeName
                IgorApp.DeleteInclude 0, quoted_includeName
                IgorApp.CompileProcedures(0)
'                WScript.Sleep 500
        End If
End Sub

Sub ReloadInclude_load(ByRef IgorApp, includeName)
        quoted_includeName = """" & includeName & """"
        If IsReloadIncludeMarkExists(IgorApp, includeName) Then
                deleteReloadIncludeMark IgorApp, includeName
                IgorApp.InsertInclude 0, quoted_includeName
                IgorApp.CompileProcedures(0)
        End If
End Sub

include_path = "root:"
include_suffix = "_INCLUDE"
Sub deleteIncludeVariable(ByRef IgorApp, includeName)
        include_varname = includeName & include_suffix
        deleteVariable IgorApp, include_path, include_varname
End Sub

Function IsIncludeDefined(IgorApp, includeName)
        include_varname = includeName & include_suffix
        IsIncludeDefined = IsVariableExists(include_path, include_varname)
End Function

incmark_path = "root:"
incmark_suffix = "_RELOAD_INCLUDE"
Sub addReloadIncludeMark(ByRef IgorApp, packageName)
        mark_varname = packageName & incmark_suffix
        addVariable IgorApp, incmark_path, mark_varname, 1
End Sub

Sub deleteReloadIncludeMark(ByRef IgorApp, packageName)
        mark_varname = packageName & incmark_suffix
        deleteVariable IgorApp, incmark_path, mark_varname
End Sub

Function IsReloadIncludeMarkExists(IgorApp, packageName)
        mark_varname = packageName & incmark_suffix
        IsReloadIncludeMarkExists = IsVariableExists(incmark_path, mark_varname)
End Function