;;; igor-mode.el --- Major mode for editing Igor Pro procedure files

;; Copyright (C) 2011

;; Author:   Jason Yamada-Hanff <jyamada1@gmail.com>
;; Keywords: languages

;;; Commentary:
;;
;; Provides syntax highlighting, indentation, and
;; autoloading/unloading for Igor Pro procedure files.
;;
;; Written for Igor Pro 6.22A.
;;
;; Code was initially based heavily on Fred White's visual-basic-mode
;; <http://www.emacswiki.org/cgi-bin/wiki/visual-basic-mode.el>

;;; Install:
;;
;; Add the following lines to your .emacs (or initialization file):
;;
;; (add-to-list 'load-path "<path/to/igor-mode>")
;; (require 'igor-mode)

;;; Autoload/unload:
;;
;; Procedure files are set to read-only while they are loaded in
;; Igor. To work around this, when igor-mode saves an *.ipf file, it
;; unloads the procedure file from Igor, saves the files, and then
;; reloads the file. It should work on both Windows and Mac OS X
;; (Igor's supported platforms). On Windows, a Python distribution
;; with pywin32 is required (see igor-exec.el). I have not tested
;; these procedures very hard, so if you find a bug, let me know.
;;
;; Note: This behavior only works for files that are loaded by
;; #include, but that is how most procedure files ought to be loaded
;; anyhow.


;;; Code:
(require 'cl)

;; Custom variables
(defcustom igor-tab-width 4
  "Indent width for Igor"
  :type 'integer
  :group 'igor)

(defcustom igor-use-autoreload t
  "Remove and re-insert files loaded in Igor when saving if non-nil"
  :type 'boolean
  :group 'igor)

(defvar igor-mode-hook nil)

(defvar igor-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for IgorPro major mode")

;; Autoload for igor files
(setq auto-mode-alist
      (append '(("\\.ipf$" . igor-mode)) auto-mode-alist))

(defun igor-wrap-re-startline (word-re)
  "Wrap a regexp to require WORD-RE to be at the start of a line"
  (concat "^[ \t]*" word-re))

;; Igor Pro Language Keywords and Built-ins
(defvar igor-procdec-keywords
  '("End" "EndMacro" "EndStructure"
    "Function" "Macro" "Picture"
    "Proc" "Structure" "Window")
  "IgorPro Procedure Declaration Keywords")

(defvar igor-procsub-keywords
  '("ButtonControl"
    "CheckBoxControl"
    "CursorStyle"
    "FitFunc"
    "Graph"
    "GraphMarquee"
    "GraphStyle"
    "GridStyle"
    "Layout"
    "LayoutMarquee"
    "LayoutStyle"
    "Panel"
    "PopupMenuControl"
    "SetVariableControl"
    "Table"
    "TableStyle")
  "IgorPro Procedure Subtype Keywords")

(defvar igor-objrefs-keywords
  '("DFREF"
    "FUNCREF"
    "NVAR"
    "STRUCT"
    "SVAR"
    "WAVE")
  "IgorPro Object Reference Keywords")

(defvar igor-flowcontrol-keywords
  '("AbortOnRTE"
    "AbortOnValue"
    "break"
    "catch"
    "continue"
    "default"
    "do"
    "while"
    "endtry"
    "for"
    "endfor"
    "if"
    "else"
    "elseif"
    "endif"
    "return"
    "strswitch"
    "case"
    "endswitch"
    "switch"
    "try")
  "IgorPro Flow Control Keywords")

(defvar igor-hash-keywords
  '("#define"
    "#if"
    "#elif"
    "#endif"
    "#ifdef"
    "#endif"
    "#ifndef"
    "#include"
    "#pragma"
    "#undef")
  "IgorPro Hash Keywords")

(defvar igor-other-keywords
  '("Constant"
    "DoPrompt"
    "GalleryGlobal"
    "IgorVersion"
    "IndependentModule"
    "Menu"
    "ModuleName"
    "MultiThread"
    "Override"
    "popup"
    "ProcGlobal"
    "Prompt"
    "root"
    "rtGlobals"
    "Static"
    "Strconstant"
    "String"
    "Submenu"
    "ThreadSafe"
    "Variable"
    "version")
  "IgorPro Other Keywords")

(defconst igor-builtin-functions
  '("AddListItem" "AiryA" "AiryAD" "AiryB" "AiryBD" "AnnotationInfo"
    "AnnotationList" "AxisInfo" "AxisList" "AxisValFromPixel"
    "Besseli" "Besselj" "Besselk" "Bessely" "BinarySearch"
    "BinarySearchInterp" "CTabList" "CaptureHistory"
    "CaptureHistoryStart" "CheckName" "ChildWindowList" "CleanupName"
    "ContourInfo" "ContourNameList" "ContourNameToWaveRef" "ContourZ"
    "ControlNameList" "CountObjects" "CountObjectsDFR" "CreationDate"
    "CsrInfo" "CsrWave" "CsrWaveRef" "CsrXWave" "CsrXWaveRef"
    "DDEExecute" "DDEInitiate" "DDEPokeString" "DDEPokeWave"
    "DDERequestString" "DDERequestWave" "DDEStatus" "DDETerminate"
    "DataFolderDir" "DataFolderExists" "DataFolderRefStatus"
    "DataFolderRefsEqual" "DateToJulian" "Dawson" "DimDelta"
    "DimOffset" "DimSize" "FetchURL" "FindDimLabel" "FindListItem"
    "FontList" "FontSizeHeight" "FontSizeStringWidth" "FresnelCos"
    "FresnelSin" "FuncRefInfo" "FunctionInfo" "FunctionList"
    "FunctionPath" "Gauss" "Gauss1D" "Gauss2D" "GetDataFolder"
    "GetDataFolderDFR" "GetDefaultFont" "GetDefaultFontSize"
    "GetDefaultFontStyle" "GetDimLabel" "GetErrMessage" "GetFormula"
    "GetIndependentModuleName" "GetIndexedObjName"
    "GetIndexedObjNameDFR" "GetKeyState" "GetRTErrMessage"
    "GetRTError" "GetRTLocInfo" "GetRTLocation" "GetRTStackInfo"
    "GetScrapText" "GetUserData" "GetWavesDataFolder"
    "GetWavesDataFolderDFR" "GrepList" "GrepString" "GuideInfo"
    "GuideNameList" "Hash" "HyperG0F1" "HyperG1F1" "HyperG2F1"
    "HyperGNoise" "HyperGPFQ" "IgorInfo" "IgorVersion" "ImageInfo"
    "ImageNameList" "ImageNameToWaveRef" "IndependentModuleList"
    "IndexedDir" "IndexedFile" "Inf" "Integrate1D" "Interp2D"
    "Interp3D" "ItemsInList" "JulianToDate" "Laguerre" "LaguerreA"
    "LaguerreGauss" "LayoutInfo" "LegendreA" "ListMatch" "LowerStr"
    "MacroList" "MandelbrotPoint" "MarcumQ" "MatrixDet" "MatrixDot"
    "MatrixRank" "MatrixTrace" "ModDate" "NVAR_Exists" "NaN"
    "NameOfWave" "NewFreeDataFolder" "NewFreeWave" "NumVarOrDefault"
    "NumberByKey" "OperationList" "PICTInfo" "PICTList" "PadString"
    "ParamIsDefault" "ParseFilePath" "PathList" "Pi"
    "PixelFromAxisVal" "PolygonArea" "PossiblyQuoteName"
    "ProcedureText" "RemoveByKey" "RemoveEnding" "RemoveFromList"
    "RemoveListItem" "ReplaceNumberByKey" "ReplaceString"
    "ReplaceStringByKey" "SVAR_Exists" "ScreenResolution" "Secs2Date"
    "Secs2Time" "SelectNumber" "SelectString" "SortList"
    "SpecialCharacterInfo" "SpecialCharacterList" "SpecialDirPath"
    "SphericalBessJ" "SphericalBessJD" "SphericalBessY"
    "SphericalBessYD" "SphericalHarmonics" "StartMSTimer"
    "StatsBetaCDF" "StatsBetaPDF" "StatsBinomialCDF"
    "StatsBinomialPDF" "StatsCMSSDCDF" "StatsCauchyCDF"
    "StatsCauchyPDF" "StatsChiCDF" "StatsChiPDF" "StatsCorrelation"
    "StatsDExpCDF" "StatsDExpPDF" "StatsEValueCDF" "StatsEValuePDF"
    "StatsErlangCDF" "StatsErlangPDF" "StatsErrorPDF" "StatsExpCDF"
    "StatsExpPDF" "StatsFCDF" "StatsFPDF" "StatsFriedmanCDF"
    "StatsGammaCDF" "StatsGammaPDF" "StatsGeometricCDF"
    "StatsGeometricPDF" "StatsHyperGCDF" "StatsHyperGPDF"
    "StatsInvBetaCDF" "StatsInvBinomialCDF" "StatsInvCMSSDCDF"
    "StatsInvCauchyCDF" "StatsInvChiCDF" "StatsInvDExpCDF"
    "StatsInvEValueCDF" "StatsInvExpCDF" "StatsInvFCDF"
    "StatsInvFriedmanCDF" "StatsInvGammaCDStatsInvGeometricCDF"
    "StatsInvKuiperCDF" "StatsInvLogNormalCDF" "StatsInvLogisticCDF"
    "StatsInvMaxwellCDF" "StatsInvMooreCDF" "StatsInvNBinomialCDF"
    "StatsInvNCChiCDF" "StatsInvNCFCDF" "StatsInvNormalCDF"
    "StatsInvParetoCDF" "StatsInvPoissonCDF" "StatsInvPowerCDF"
    "StatsInvQCDF" "StatsInvQpCDF" "StatsInvRayleighCDF"
    "StatsInvRectangularCDF" "StatsInvSpearmanCDF"
    "StatsInvStudentCDF" "StatsInvTopDownCDF" "StatsInvTriangularCDF"
    "StatsInvUsquaredCDF" "StatsInvVonMisesCDF" "StatsInvWeibullCDF"
    "StatsKuiperCDF" "StatsLogNormalCDF" "StatsLogNormalPDF"
    "StatsLogisticCDF" "StatsLogisticPDF" "StatsMaxwellCDF"
    "StatsMaxwellPDF" "StatsMedian" "StatsMooreCDF"
    "StatsNBinomialCDF" "StatsNBinomialPDF" "StatsNCChiCDF"
    "StatsNCChiPDF" "StatsNCFCDF" "StatsNCFPDF" "StatsNCTCDF"
    "StatsNCTPDF" "StatsNormalCDF" "StatsNormalPDF" "StatsParetoCDF"
    "StatsParetoPDF" "StatsPermute" "StatsPoissonCDF"
    "StatsPoissonPDF" "StatsPowerCDF" "StatsPowerNoise"
    "StatsPowerPDF" "StatsQCDF" "StatsQpCDF" "StatsRayleighCDF"
    "StatsRayleighPDF" "StatsRectangularCDF" "StatsRectangularPDF"
    "StatsRunsCDF" "StatsSpearmanRhoCDF" "StatsStudentCDF"
    "StatsStudentPDF" "StatsTopDownCDF" "StatsTriangularCDF"
    "StatsTriangularPDF" "StatsTrimmedMean" "StatsUSquaredCDF"
    "StatsVonMisesCDF" "StatsVonMisesNoise" "StatsVonMisesPDF"
    "StatsWaldCDF" "StatsWaldPDF" "StatsWeibullCDF" "StatsWeibullPDF"
    "StopMSTimer" "StrVarOrDefault" "StringByKey" "StringFromList"
    "StringList" "StudentA" "StudentT" "TableInfo" "TagVal"
    "TagWaveRef" "TextFile" "ThreadGroupCreate" "ThreadGroupGetDF"
    "ThreadGroupGetDFR" "ThreadGroupRelease" "ThreadGroupWait"
    "ThreadProcessorCount" "ThreadReturnValue" "TraceFromPixel"
    "TraceInfo" "TraceNameList" "TraceNameToWaveRef" "URLDecode"
    "URLEncode" "UnPadString" "UniqueName" "UpperStr" "VariableList"
    "Variance" "WNoise" "WaveCRC" "WaveDims" "WaveExists" "WaveInfo"
    "WaveList" "WaveMax" "WaveMin" "WaveName" "WaveRefIndexed"
    "WaveRefIndexedDFR" "WaveRefsEqual" "WaveTextEncoding" "WaveType"
    "WaveUnits" "WhichListItem" "WinList" "WinName" "WinRecreation"
    "WinType" "XWaveName" "XWaveRefFromTrace" "ZernikeR" "abs" "acos"
    "acosh" "alog" "area" "areaXY" "asin" "asinh" "atan" "atan2"
    "atanh" "bessi" "bessj" "bessk" "bessy" "beta" "betai" "binomial"
    "binomialNoise" "binomialln" "cabs" "ceil" "cequal" "char2num"
    "chebyshev" "chebyshevU" "cmplx" "cmpstr" "conj" "cos" "cosh"
    "cot" "coth" "cpowi" "csc" "date" "date2secs" "datetime" "defined"
    "deltax" "digamma" "e" "ei" "enoise" "equalWaves" "erf" "erfc"
    "erfcw" "exists" "exp" "expInt" "expNoise" "factorial" "fakedata"
    "faverage" "faverageXY" "floor" "gamma" "gammaInc" "gammaNoise"
    "gammln" "gammp" "gammq" "gcd" "gnoise" "hcsr" "hermite"
    "hermiteGauss" "i" "ilim" "imag" "interp" "inverseERF"
    "inverseERFC" "j" "jlim" "leftx" "limit" "ln" "log"
    "logNormalNoise" "lorentzianNoise" "magsqr" "max" "mean" "min"
    "mod" "norm" "note" "num2char" "num2istr" "num2str" "numpnts"
    "numtype" "p" "p2rect" "pcsr" "pnt2x" "poissonNoise" "poly"
    "poly2D" "q" "qcsr" "r" "r2polar" "real" "rightx" "round" "s"
    "sawtooth" "sec" "sign" "sin" "sinc" "sinh" "sqrt" "str2num"
    "stringCRC" "stringmatch" "strlen" "strsearch" "sum" "t" "tan"
    "tanh" "ticks" "time" "trunc" "vcsr" "x" "x2pnt" "xcsr" "y" "z"
    "zcsr")
  "Igor Pro 6 Built-in Functions")

(defconst igor-builtin-operations
  '("Abort" "AddFIFOData" "AddFIFOVectData" "AddMovieAudio"
    "AddMovieFrame" "AdoptFiles" "APMath" "Append" "AppendImage"
    "AppendLayoutObject" "AppendMatrixContour" "AppendText"
    "AppendToGraph" "AppendToLayout" "AppendToTable" "AppendXYZContour"
    "AutoPositionWindow" "BackgroundInfo" "Beep" "BoundingBall"
    "BrowseURL" "BuildMenu" "Button" "cd" "Chart" "CheckBox"
    "CheckDisplayed" "ChooseColor" "Close" "CloseMovie" "CloseProc"
    "ColorScale" "ColorTab2Wave" "Concatenate" "ControlBar"
    "ControlInfo" "ControlUpdate" "ConvexHull" "Convolve" "CopyFile"
    "CopyFolder" "CopyScales" "Correlate" "CreateAliasShortcut" "Cross"
    "CtrlBackground" "CtrlFIFO" "CtrlNamedBackground" "Cursor"
    "CurveFit" "CustomControl" "CWT" "Debugger" "DebuggerOptions"
    "DefaultFont" "DefaultGuiControls" "DefaultGuiFont" "DefineGuide"
    "DelayUpdate" "DeleteFile" "DeleteFolder" "DeletePoints"
    "Differentiate" "dir" "Display" "DisplayHelpTopic"
    "DisplayProcedure" "DoAlert" "DoIgorMenu" "DoUpdate" "DoWindow"
    "DoXOPIdle" "DrawAction" "DrawArc" "DrawBezier" "DrawLine"
    "DrawOval" "DrawPICT" "DrawPoly" "DrawRect" "DrawRRect" "DrawText"
    "DSPDetrend" "DSPPeriodogram" "Duplicate" "DuplicateDataFolder"
    "DWT" "EdgeStats" "Edit" "ErrorBars" "Execute" "ExecuteScriptText"
    "ExperimentModified" "Extract" "FastGaussTransform" "FastOp"
    "FBinRead" "FBinWrite" "FFT" "FIFO2Wave" "FIFOStatus" "FilterFIR"
    "FilterIIR" "FindLevel" "FindLevels" "FindPeak" "FindPointsInPoly"
    "FindRoots" "FindSequence" "FindValue" "FPClustering" "fprintf"
    "FReadLine" "FSetPos" "FStatus" "FTPCreateDirectory" "FTPDelete"
    "FTPDownload" "FTPUpload" "FuncFit" "FuncFitMD" "GetAxis"
    "GetFileFolderInfo" "GetLastUserMenuInfo" "GetMarquee" "GetMouse"
    "GetSelection" "GetWindow" "GraphNormal" "GraphWaveDraw"
    "GraphWaveEdit" "Grep" "GroupBox" "Hanning" "HideIgorMenus"
    "HideInfo" "HideProcedures" "HideTools" "HilbertTransform"
    "Histogram" "IFFT" "ImageAnalyzeParticles" "ImageBlend"
    "ImageBoundaryToMask" "ImageEdgeDetection" "ImageFileInfo"
    "ImageFilter" "ImageFocus" "ImageFromXYZ" "ImageGenerateROIMask"
    "ImageHistModification" "ImageHistogram" "ImageInterpolate"
    "ImageLineProfile" "ImageLoad" "ImageMorphology" "ImageRegistration"
    "ImageRemoveBackground" "ImageRestore" "ImageRotate" "ImageSave"
    "ImageSeedFill" "ImageSnake" "ImageStats" "ImageThreshold"
    "ImageTransform" "ImageUnwrapPhase" "ImageWindow" "IndexSort"
    "InsertPoints" "Integrate" "IntegrateODE" "Interp3DPath"
    "Interpolate3D" "KillBackground" "KillControl" "KillDataFolder"
    "KillFIFO" "KillFreeAxis" "KillPath" "KillPICTs" "KillStrings"
    "KillVariables" "KillWaves" "KillWindow" "KMeans" "Label" "Layout"
    "Legend" "LinearFeedbackShiftRegister" "ListBox" "LoadData"
    "LoadPackagePreferences" "LoadPICT" "LoadWave" "Loess"
    "LombPeriodogram" "Make" "MakeIndex" "MarkPerfTestTime"
    "MatrixConvolve" "MatrixCorr" "MatrixEigenV" "MatrixFilter"
    "MatrixGaussJ" "MatrixInverse" "MatrixLinearSolve"
    "MatrixLinearSolveTD" "MatrixLLS" "MatrixLUBkSub" "MatrixLUD"
    "MatrixMultiply" "MatrixOP" "MatrixSchur" "MatrixSolve"
    "MatrixSVBkSub" "MatrixSVD" "MatrixTranspose" "MeasureStyledText"
    "Modify" "ModifyContour" "ModifyControl" "ModifyControlList"
    "ModifyFreeAxis" "ModifyGraph" "ModifyImage" "ModifyLayout"
    "ModifyPanel" "ModifyTable" "ModifyWaterfall" "MoveDataFolder"
    "MoveFile" "MoveFolder" "MoveString" "MoveSubwindow" "MoveVariable"
    "MoveWave" "MoveWindow" "NeuralNetworkRun" "NeuralNetworkTrain"
    "NewDataFolder" "NewFIFO" "NewFIFOChan" "NewFreeAxis" "NewImage"
    "NewLayout" "NewMovie" "NewNotebook" "NewPanel" "NewPath"
    "NewWaterfall" "Note" "Notebook" "NotebookAction" "Open"
    "OpenNotebook" "Optimize" "ParseOperationTemplate" "PathInfo"
    "PauseForUser" "PauseUpdate" "PCA" "PlayMovie" "PlayMovieAction"
    "PlaySnd" "PlaySound" "PopupContextualMenu" "PopupMenu"
    "Preferences" "PrimeFactors" "Print" "printf" "PrintGraphs"
    "PrintLayout" "PrintNotebook" "PrintSettings" "PrintTable" "Project"
    "PulseStats" "PutScrapText" "pwd" "Quit" "RatioFromNumber"
    "Redimension" "Remove" "RemoveContour" "RemoveFromGraph"
    "RemoveFromLayout" "RemoveFromTable" "RemoveImage"
    "RemoveLayoutObjects" "RemovePath" "Rename" "RenameDataFolder"
    "RenamePath" "RenamePICT" "RenameWindow" "ReorderImages"
    "ReorderTraces" "ReplaceText" "ReplaceWave" "Resample"
    "ResumeUpdate" "Reverse" "Rotate" "Save" "SaveData" "SaveExperiment"
    "SaveGraphCopy" "SaveNotebook" "SavePackagePreferences" "SavePICT"
    "SaveTableCopy" "SetActiveSubwindow" "SetAxis" "SetBackground"
    "SetDashPattern" "SetDataFolder" "SetDimLabel" "SetDrawEnv"
    "SetDrawLayer" "SetFileFolderInfo" "SetFormula" "SetIgorHook"
    "SetIgorMenuMode" "SetIgorOption" "SetMarquee" "SetProcessSleep"
    "SetRandomSeed" "SetScale" "SetVariable" "SetWaveLock"
    "SetWaveTextEncoding" "SetWindow" "ShowIgorMenus" "ShowInfo"
    "ShowTools" "Silent" "Sleep" "Slider" "Smooth" "SmoothCustom" "Sort"
    "SoundInRecord" "SoundInSet" "SoundInStartChart" "SoundInStatus"
    "SoundInStopChart" "SphericalInterpolate" "SphericalTriangulate"
    "SplitString" "sprintf" "sscanf" "Stack" "StackWindows"
    "StatsAngularDistanceTest" "StatsANOVA1Test" "StatsANOVA2NRTest"
    "StatsANOVA2RMTest" "StatsANOVA2Test" "StatsChiTest"
    "StatsCircularCorrelationTest" "StatsCircularMeans"
    "StatsCircularMoments" "StatsCircularTwoSampleTest"
    "StatsCochranTest" "StatsContingencyTable" "StatsDIPTest"
    "StatsDunnettTest" "StatsFriedmanTest" "StatsFTest"
    "StatsHodgesAjneTest" "StatsJBTest" "StatsKendallTauTest"
    "StatsKSTest" "StatsKWTest" "StatsLinearCorrelationTest"
    "StatsLinearRegression" "StatsMultiCorrelationTest" "StatsNPMCTest"
    "StatsNPNominalSRTest" "StatsQuantiles" "StatsRankCorrelationTest"
    "StatsResample" "StatsSample" "StatsScheffeTest" "StatsSignTest"
    "StatsSRTest" "StatsTTest" "StatsTukeyTest" "StatsVariancesTest"
    "StatsWatsonUSquaredTest" "StatsWatsonWilliamsTest"
    "StatsWheelerWatsonTest" "StatsWilcoxonRankTest"
    "StatsWRCorrelationTest" "String" "StructGet" "StructPut"
    "TabControl" "Tag" "TextBox" "ThreadGroupPutDF" "ThreadStart" "Tile"
    "TileWindows" "TitleBox" "ToCommandLine" "ToolsGrid" "Triangulate3d"
    "Unwrap" "ValDisplay" "Variable" "WaveMeanStdv" "WaveStats"
    "WaveTransform" "wfprintf" "WignerTransform" "WindowFunction")
  "Igor Pro 6 Built-in Operations")

;; Regexp optimized versions of word lists
(defvar igor-procdec-keywords-re
  (regexp-opt igor-procdec-keywords 'words))
(defvar igor-procsub-keywords-re
  (regexp-opt igor-procsub-keywords 'words))
(defvar igor-objrefs-keywords-re
  (regexp-opt igor-objrefs-keywords 'words))
(defvar igor-flowcontrol-keywords-re
  (regexp-opt igor-flowcontrol-keywords 'words))
(defvar igor-hash-keywords-re
  (regexp-opt igor-hash-keywords 'words))
(defvar igor-other-keywords-re
  (regexp-opt igor-other-keywords 'words))

(defconst igor-builtin-functions-re
  (regexp-opt igor-builtin-functions 'words))
(defconst igor-builtin-operations-re
  (regexp-opt igor-builtin-operations 'words))

(defconst igor-defun-start-words
  '("Function" "Macro" "Picture" "Proc"
    "Static" "Structure" "Window")
  "Words that define the beginning of a definition block")

(defconst igor-defun-end-words
  '("End" "EndMacro" "EndStructure")
  "Words that define the end of a definition block")

(defconst igor-blank-re "^[ \t]*$")
(defconst igor-comment-re "^[ \t]*\/\/.*$")

(defconst igor-integer-re
  "\\(?:\\+\\|-\\)?[0-9]+"
  "Integer syntax in Igor")
(defconst igor-number-re
  (concat
   "\\_<"
   "\\(?:\\+\\|-\\)?"                   ; sign part
   "\\(?:[0-9]*\\.\\)?[0-9]+"           ; integer or decimal
   "\\(?:e" igor-integer-re "\\)?"      ; exponent part
   "\\_>")
   "Number syntax in Igor")
(defconst igor-hex-num-re
  (concat
   "\\_<"
   "0x"                                 ; hex prefix
   "\\(?:[0-9A-F]+\\)"
   "\\_>")
  "Hex number syntax in Igor")
(defconst igor-name-start-re "[a-zA-Z]"
  "Match first character of an identifier name")
(defconst igor-name-re
  (concat
   igor-name-start-re
   "[a-zA-Z0-9_]*")
  "Legal object names in Igor")

(defconst igor-parameter-re
  (concat
   "\\[?[ \t]*" igor-name-re "[ \t]*\\]?")
  "Parameter name with optional brackets")

(defconst igor-parameter-list-re
  (concat
   igor-parameter-re
   "\\(?:[ \t]*\\,[ \t]*" igor-parameter-re "\\)*")
  "Procedure parameter list")

(defconst igor-defun-static-re
  "\\(?:Static[ \t]+\\)")

(defconst igor-defun-start-function
  (concat
   igor-defun-static-re "?"
   "\\(?:Function"
   "\\(\\/\\(?:C\\|D\\|S\\|DF\\|WAVE\\)\\)?\\)"))

(defconst igor-defun-start-picture
  (concat
   igor-defun-static-re "?"
   "\\(?:Picture\\)"))

(defconst igor-defun-start-re
  (concat
   "^[ \t]*"
   "\\(?:Macro\\|Proc\\|Structure\\|Window" ; non-static start keywords
   "\\|\\(?:" igor-defun-start-function "\\)"
   "\\|\\(?:" igor-defun-start-picture "\\)\\)")
  "Regexp for procedure start. Must define manually to handle
   'Static' prefix cleanly. Function and Picture can have Static
   prefix, but other keywords cannot.")

(defconst igor-defun-end-re
  (concat "^[ \t]*" (regexp-opt igor-defun-end-words 'words)))

(defvar igor-defun-re
  (concat
   "^[ \t]*\\(" igor-defun-start-re "\\)[ \t]+" ; procedure type
   "\\(" igor-name-re "\\)[ \t]*"         ; procedure name
   "\\(([ \t]*"
   "\\(?:" igor-parameter-list-re "\\)?" ; parameter list
   "[ \t]*)\\)"
   "\\([ \t]*:[ \t]*" igor-procsub-keywords-re "[ \t]*\\)?" ; procedure subtype
   )
  "Regexp for definition line of Igor functions/macros/etc.")

;; Syntax Highlighting

;; Syntax Table
(defvar igor-syntax-table
  (let ((st (make-syntax-table)))
    ;; Single-line comments "//"
    (modify-syntax-entry ?/  ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    ;; strings
    (modify-syntax-entry ?\" "\"" st)   ; string literals
    (modify-syntax-entry ?\' "\"" st)   ; literal names
    ;; make underscores part of words
    (modify-syntax-entry ?_  "w" st)
    ;; operators
    (modify-syntax-entry ?$  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?^  "." st)
    (modify-syntax-entry ?*  "." st)
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?!  "." st)
    (modify-syntax-entry ?~  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ??  "." st)
    (modify-syntax-entry ?:  "." st)
    (modify-syntax-entry ?,  "." st)
    (modify-syntax-entry ?\; "." st)
    ;; parens
    (modify-syntax-entry ?\( "(" st)
    (modify-syntax-entry ?\) ")" st)
    (modify-syntax-entry ?[ "(" st)
    (modify-syntax-entry ?] ")" st)
    st)
  "Syntax table used while in `igor-mode'")

(defvar igor-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Function names
     (list igor-defun-re
           '(1 font-lock-keyword-face)        ; procedure type
           '(2 font-lock-type-face t t)       ; return type
           '(3 font-lock-function-name-face)  ; procedure name
           '(7 font-lock-keyword-face nil t)  ; procedure subtype
           )
     (cons igor-procdec-keywords-re 'font-lock-keyword-face)
     (cons igor-procsub-keywords-re 'font-lock-keyword-face)
     (cons igor-objrefs-keywords-re 'font-lock-type-face)
     (cons igor-flowcontrol-keywords-re 'font-lock-keyword-face)
     (cons igor-other-keywords-re 'font-lock-type-face)
     (cons igor-builtin-functions-re 'font-lock-builtin-face)
     (cons igor-builtin-operations-re 'font-lock-builtin-face)
     ;; Numbers
     (cons igor-hex-num-re 'font-lock-constant-face)
     (cons igor-number-re 'font-lock-constant-face)
     (cons igor-hash-keywords-re 'font-lock-preprocessor-face))))

(defvar igor-font-lock-keywords-2
  `(append igor-font-lock-keywords-1
          (,igor-other-keywords-re . font-lock-keyword-face)))

(defvar igor-font-lock-keywords-default
  igor-font-lock-keywords-1)

(defvar igor-font-lock-keywords
  '(igor-font-lock-keywords-default     ; mode default
    igor-font-lock-keywords-1           ; level 1
    igor-font-lock-keywords-2           ; level 2
    ))

(defvar igor-font-lock-defaults
  '(igor-font-lock-keywords-1        ; keyword list
    nil                              ; perform syntactic fontification
    t                                ; ignore case
    nil))                            ; use buffer syntax table

;; Imenu support
(setq igor-imenu-generic-expression
      `(("Procedures" ,igor-defun-re 3)))

;; Indentation
(defvar igor-closeblock-words
  '("End" "EndMacro" "EndStructure"
   "while" "endtry" "endfor" "elseif"
   "endif" "endswitch" "#elif" "#endif")
  "Words that decrease indentation level")

(defvar igor-openblock-words
  '("Function" "Macro" "Menu" "Picture" "Proc" "Static" "Structure" "Window"
    "default" "do" "for" "if" "else" "elseif" "case" "switch"
    "try" "catch" "#if" "#elif" "#ifdef" "#ifndef")
  "Words that increase indentation level")

(defvar igor-closeblock-re
  (concat "^[ \t]*" (regexp-opt igor-closeblock-words 'words)))

(defvar igor-openblock-re
  (concat "^[ \t]*" (regexp-opt igor-openblock-words 'words)))

;; Movement related commands
;; ==================================================

(defun igor-beginning-of-defun()
  "Set the pointer at the beginning of the Function/Macro/etc within which the pointer is located."
  (interactive)
  (re-search-backward igor-defun-start-re))

(defun igor-end-of-defun()
  "Set the pointer at the end of the Function/Macro/etc within which the pointer is located."
  (interactive)
  (re-search-forward igor-defun-end-re))

(defun igor-mark-defun()
  "Set the region pointer around Function/Macro/etc within which the pointer is located."
  (interactive)
  (beginning-of-line)
  (igor-end-of-defun)
  (set-mark (point))
  (igor-beginning-of-defun))


;; Indentation related commands
;; ==================================================

(defun igor-previous-line-of-code ()
  "Set point on previous line of code, skipping any blank or comment lines."
  (interactive)
  (igor-navigate-line-of-code -1 (lambda () (not (bobp)))))

(defun igor-next-line-of-code ()
  "Set point on next line of code, skipping any blank or comment lines."
  (interactive)
  (igor-navigate-line-of-code +1  (lambda () (not (eobp)))))

(defun igor-navigate-line-of-code (increment predicate)
  "Set point on a new line of code.

INCREMENT is the number of code lines away from the current
line (e.g. 2 is two lines further, -1 is the previous
line). PREDICATE is a condition that must be true to navigate to
the new line. Blank lines and comment lines are skipped."
  (interactive)
  (if (funcall predicate)
      (forward-line increment))
  (while (and (funcall predicate)
              (or (looking-at igor-comment-re)
                  (looking-at igor-blank-re)))
    (forward-line increment)))

(defun igor-build-match-list (inlist &optional no-orig-end-values)
  "Return an indentation matching list

INLIST must be a list of cons cells which hold a start keyword in
car and one or more matching end keywords in cdr.

The match list format transforms this list into a mapping with
end keywords as the keys. The value of each cell is a list of
cells holding the matching start keyword in car and all valid end
keywords for that start keyword. This is structure is necessary
because an ending keyword may have multiple start keywords, and
each valid start keyword may have a different set of valid end
keywords.
"
  (let (newlist)
    (dolist (pair inlist newlist)
      (dolist (endkey (cdr pair))
        (let ((match-cell (assoc endkey newlist))
              (newpair (if no-orig-end-values
                           (cons (car pair) '())
                         pair)))
          (if match-cell
              (push newpair (cdr match-cell))
            (push (cons endkey (list newpair)) newlist)))))))

(defun igor-build-and-append-match-list (alist append-list)
  "Return a match list with keys based on the cdr values of ALIST
and values based on both ALIST and APPEND-LIST values for start
keywords."
  (igor-append-to-match-list
   (igor-build-match-list
    (igor-compress-alist-keys alist))
   append-list))

(defun igor-match-list-to-re (matchlist)
  "Return a copy of the match list MATCHLIST with all strings as
regular expressions."
  (let ((relist (copy-tree matchlist t)))
    (dolist (match-cell relist relist)
      (progn
        (setcar match-cell
                (igor-convert-word-to-indent-re
                 (car match-cell)))
        (dolist (start-cell (cdr match-cell))
          (progn
            (setcar start-cell
                    (igor-convert-word-to-indent-re
                     (car start-cell)))
            (setcdr start-cell
                    (cons
                     (igor-convert-list-to-indent-re
                      (cdr start-cell)) '()))))))))

(defun igor-convert-word-to-indent-re (word)
  "Convert a string WORD into an optimzed regexp for indentation
matching"
  (igor-convert-list-to-indent-re (list word)))

(defun igor-convert-list-to-indent-re (wordlist)
  "Convert a list of words WORDLIST into an optimized regexp for
indentation matching"
  (igor-wrap-re-startline
   (regexp-opt wordlist 'words)))

(defun igor-append-pairs (curlist inlist &optional exists-only)
  "Adds the pairs in INLIST to the CURLIST, adding the cdr of the
list to a pre-exisiting pair if it already exists.

If EXISTS-ONLY is non-nil, only pre-existing keys are appended to
"
  (let ((newlist (copy-tree curlist t)))
    (dolist (new-pair inlist newlist)
      (let ((match-cell
             (assoc (car new-pair) newlist)))
        (if match-cell
            (dolist (new-elt (cdr new-pair))
              (if (not (rassoc new-elt match-cell))
                  (push new-elt (cdr match-cell))))
          (if (not exists-only)
              (push new-pair newlist)))))))

(defun igor-append-to-match-list (match-list append-list)
  (let (newlist)
    (dolist (match-cell match-list newlist)
      (push (igor-append-to-match-cell
             match-cell append-list)
            newlist))))

(defun igor-append-to-match-cell (match-cell append-list)
  "Return a copy of MATCH-CELL whose cdr is joined with
APPEND-LIST using the function `igor-append-to-alist'"
  (cons (car match-cell)
        (igor-append-to-alist
         (cdr match-cell)
         append-list t)))

(defun igor-append-to-alist (alist append-list &optional exists-only)
  "Return a joined copy of association lists ALIST and APPEND-LIST

If the same key appears in both lists, the values are collected
and returned as the new value for the key.

If EXISTS-ONLY is non-nil, only pre-existing keys from ALIST are
included in the result"
  (let (newlist)
    (setq append-list (igor-compress-alist-keys append-list))
    (setq newlist
          (dolist (acell alist (nreverse newlist))
            (push (igor-append-to-pair acell append-list)
                  newlist)
            (setq append-list (igor-remove-alist-key
                               (car acell) append-list))))
    (if exists-only
        newlist
      (append newlist append-list))))

(defun igor-append-to-pair (acell append-alist)
  "Adds to ACELL the value of associations found in APPEND-ALIST
  that have the same key as the association cell ACELL."
  (let ((matched-assoc
         (assoc (car acell)
                (igor-compress-alist-keys append-alist))))
    (if matched-assoc
        (cons (car acell)
              (delete-dups
               (append
                (igor-convert-to-list (cdr matched-assoc))
                (igor-convert-to-list (cdr acell)))))
      (igor-convert-to-list acell))))

(defun igor-compress-alist-keys (alist)
  "Returns compressed form of the association list ALIST.

Repeated keys in ALIST are pruned so that a single copy of the
key holds a list with all values (e.g. '((1 2) (1 3) (5 6)) -->
'((1 2 3) (5 6))"
  (if (null alist)
      nil
    (cons (igor-compress-alist-key
           (caar alist) alist)
          (igor-compress-alist-keys
           (igor-remove-alist-key (caar alist) alist)))))

(defun igor-compress-alist-key (key alist)
  "Returns compressed form of all associations for KEY in
ALIST, where KEY is the car and all found values for key in ALIST
is the cdr. (e.g. '((1 2) (1 3) (5 6)) --> '(1 2 3))"
  (cons key
        (apply 'append
        (mapcar
         'igor-convert-to-list
         (mapcar 'cdr
                 (igor-alist-all-assoc
                  key alist))))))

(defun igor-remove-alist-key (key alist)
  "Return a copy of ALIST with all associations by KEY removed"
  (let ((newlist alist)
        (assocs (igor-alist-all-assoc key alist)))
    (dolist (cell assocs newlist)
      (setq newlist (remove cell newlist)))))

(defun igor-alist-all-assoc (key alist)
  "Returns a list of all associations for KEY in ALIST"
  (let ((found-assoc
         (assoc key alist)))
    (if found-assoc
        (cons found-assoc
         (igor-alist-all-assoc
          key (remove found-assoc alist)))
      nil)))

(defun igor-convert-to-list (maybe-list)
  "Returns a list version of the object MAYBE-LIST

This function can be used to ensure that dot notation pairs or
strings become lists. This is useful, for instance, when using
the function `append` to concatenate results.
"
  (if (nlistp maybe-list)
      (cons maybe-list '())
    (if (null (cdr maybe-list))
        maybe-list
      (cons (car maybe-list)
            (igor-convert-to-list
             (cdr maybe-list))))))

;;; Indentation pairs
;; When a keyword is encountered that defines a block, it may
;;  * start a construct (following lines are indented)
;;  * end a construct (unindent to start level)
;;  * start a new section of the construct (a mid-level keyword), which may:
;;     * unindent to start and be used multiple times (e.g., if-elseif)
;;     * unindent to start and be used only once (e.g., if-else)
;;     * indent from start and be used multiple times (e.g., switch-case)
;;     * indent from start and be used only once (e.g., switch-default)
;;  * no keyword is found (indent as previous line)
(defconst igor-start-end-pairs
  '(("Function" "End")
    ("Static Function" "End")
    ("Macro" "End" "EndMacro")
    ("Menu" "End")
    ("Picture" "End" "EndMacro")
    ("Static Picture" "End" "EndMacro")
    ("Proc" "End" "EndMacro")
    ("Structure" "End" "EndStructure")
    ("Window" "End" "EndMacro")
    ("if" "endif")
    ("for" "endfor")
    ("do" "while")
    ("switch" "endswitch")
    ("strswitch" "endswitch")
    ("try" "endtry")
    ("#if" "#endif")
    ("#ifdef" "#endif")
    ("#ifndef" "#endif"))
  "List of cons cells of start and end keywords for indentation
  blocks. cdr holds all valid end keywords of the car keyword.")

(defconst igor-start-middle-pairs
  '(("if" "else")
    ("try" "catch"))
  "List of cons cells of start and single-use mid-level keywords
  for same-level indentation.")

(defconst igor-start-middle-many-pairs
  '(("if" "elseif")
    ("#if" "#elif"))
  "List of cons cells of start and multi-use mid-level keywords
  for same-level indentation.")

(defconst igor-start-middle-inc-pairs
  '(("switch" "default")
    ("strswitch" "default"))
  "List of cons cells of start and single-use mid-level keywords
  for increased-level indentation.")

(defconst igor-start-middle-many-inc-pairs
  '(("switch" "case")
    ("strswitch" "case"))
  "List of cons cells of start and multi-use mid-level keywords
  for increased-level indentation.")

(defconst igor-outdent-single-pairs
  (igor-append-to-alist
   (igor-compress-alist-keys igor-start-middle-pairs)
   (igor-compress-alist-keys igor-start-end-pairs))
  "List of cons cells of start and single-use keywords that close
a block.")

(defconst igor-indent-single-pairs
  (igor-append-to-alist
   (igor-compress-alist-keys igor-start-middle-inc-pairs)
   (igor-compress-alist-keys igor-outdent-single-pairs))
  "List of cons cells of start and single-use keywords that
indent relative to the start keyword")

(defconst igor-outdent-end-match-list
  (igor-build-match-list
   igor-start-end-pairs)
  "Match list for end keywords that unambiguously ends the given
  start keywords")
(defconst igor-outdent-end-match-list-re
  (igor-match-list-to-re
   igor-outdent-end-match-list))

(defconst igor-outdent-single-match-list
  (igor-append-to-match-list
   (igor-build-match-list
    (igor-compress-alist-keys igor-start-middle-pairs) t)
   igor-start-end-pairs)
  "Match list for single use keywords that outdent to the same
level as the start keyword")
(defconst igor-outdent-single-match-list-re
  (igor-match-list-to-re
   igor-outdent-single-match-list))

(defconst igor-outdent-many-match-list
  (igor-append-to-match-list
   (igor-build-match-list
    (igor-compress-alist-keys igor-start-middle-many-pairs) t)
   igor-start-end-pairs)
  "Match list for multi-use mid-level keywords that outdent to
  the same level as the start keyword. Multi-use keywords are not
  included in the close statement keyword list.")
(defconst igor-outdent-many-match-list-re
  (igor-match-list-to-re
   igor-outdent-many-match-list))

(defconst igor-outdent-match-list
  (append
   igor-outdent-end-match-list
   igor-outdent-single-match-list
   igor-outdent-many-match-list)
  "Match list for all keywords that outdent to the start keyword
indent level")
(defconst igor-outdent-match-list-re
  (igor-match-list-to-re
   igor-outdent-match-list))

(defconst igor-indent-single-match-list
  (igor-build-and-append-match-list
   igor-start-middle-inc-pairs
   igor-outdent-single-pairs)
  "Match list for single use keywords that indent relative to the
start keyword")
(defconst igor-indent-single-match-list-re
  (igor-match-list-to-re
   igor-indent-single-match-list))

(defconst igor-indent-many-match-list
  (igor-append-to-match-list
   (igor-build-match-list
    (igor-compress-alist-keys igor-start-middle-many-inc-pairs) t)
   igor-indent-single-pairs)
  "Match list for multi use keywords that indent relative to the
start keyword. Multi-use keywords are not included in the close
statement keyword list.")
(defconst igor-indent-many-match-list-re
  (igor-match-list-to-re
   igor-indent-many-match-list))

(defconst igor-indent-match-list
  (append
   igor-indent-single-match-list
   igor-indent-many-match-list)
  "Match list for all keywords that indent past the start keyword
indent level")
(defconst igor-indent-match-list-re
  (igor-match-list-to-re
   igor-indent-match-list))

(defun igor-looking-at-re-list (re-list)
  "Return the positions of the regular expression in the list
RE-LIST that matches the current line. Returns nil if no match"
  (positions-all t (mapcar 'looking-at re-list)))

(defun positions-all (item seq &optional count)
  "Return a list of all positions of ITEM in the sequence
SEQ. COUNT is used internally in the recursion to keep track of
how much of the original SEQ has already been processed."
  (let ((pos-idx (if (not count) 0 count))
        (found-position (position item seq)))
    (if found-position
        (progn
          (setq pos-idx (+ found-position pos-idx))
          (cons pos-idx
                (positions-all item
                               (nthcdr (+ 1 found-position) seq)
                               (+ 1 pos-idx))))
      nil)))

(defun igor-match-list-end-keys (match-list)
  (mapcar 'igor-match-cell-end-key match-list))

(defun igor-match-list-start-keys (match-list)
  (mapcar 'igor-match-cell-start-keys match-list))

(defun igor-match-list-end-start-keys (match-list)
  (mapcar 'igor-match-cell-end-start-keys match-list))

(defun igor-match-cell-end-key (match-cell)
  (car match-cell))

(defun igor-match-cell-start-keys (match-cell)
  (mapcar 'car (cdr match-cell)))

(defun igor-match-cell-end-start-keys (match-cell)
  (mapcar 'cdr (cdr match-cell)))

(defun igor-find-unclosed-start (match-cell)
  "Navigate to the first occurrence of an unclosed start keyword
that matches the MATCH-CELL from a match list"
  (let ((close-counts
         (make-vector
          (length (cdr match-cell)) 0)))
    (while (and
            (not (find -1 close-counts))
            (not (bobp)))
      (igor-previous-line-of-code)
      (let ((close-match
             (igor-close-match-match-cell match-cell)))
        (if close-match
            (igor-update-close-match-count
             close-counts close-match))
          (let ((open-match
                 (igor-open-match-match-cell match-cell)))
            (if open-match
                (igor-update-open-match-count
                 close-counts open-match)))))))

(defun igor-close-match-match-cell (match-cell)
  (igor-looking-at-re-list
   (mapcar 'car
           (igor-match-cell-end-start-keys match-cell))))

(defun igor-open-match-match-cell (match-cell)
  (igor-looking-at-re-list
   (igor-match-cell-start-keys match-cell)))

(defun igor-update-close-match-count (counts match-positions)
  (igor-change-vector-elements counts match-positions '1+))

(defun igor-update-open-match-count (counts match-positions)
  (igor-change-vector-elements counts match-positions '1-))

(defun igor-change-vector-elements (array idxs func)
  (mapcar (lambda (idx)
            (igor-change-vector-element
             array idx func)) idxs))

(defun igor-change-vector-element (array idx func)
  (aset array idx
        (funcall func (elt array idx))))

(defun igor-find-indent-match (match-list-re match-idx)
  "Returns the indentation of the first occurence of a start
keyword from the match cell specified by the index MATCH-IDX in
the match list MATCH-LIST-RE."
  (let ((matched-cell
         (elt match-list-re match-idx)))
    (igor-find-unclosed-start matched-cell)
    (current-indentation)))

(defun igor-current-line-matches (match-list-re)
  "Returns the position of the first match-cell in the
MATCH-LIST-RE that matches the current line; nil if no match"
  (car (igor-looking-at-re-list
        (igor-match-list-end-keys match-list-re))))

(defun igor-calculate-indent ()
  "Return indent count for the line of code containing pointer."
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; If first line, no indentation
       ((bobp)
        0)

       ;; Outdent keywords (decrease indentation)
       ((setq matched-cell
              (igor-current-line-matches
               igor-outdent-match-list-re))
        (igor-find-indent-match
         igor-outdent-match-list-re matched-cell))

       ;; Indent keywords (increase indentation)
       ((setq matched-cell
              (igor-current-line-matches
               igor-indent-match-list-re))
        (setq start-indent
              (igor-find-indent-match
               igor-indent-match-list-re matched-cell))
        (if (not (bobp))                ; if no start keyword was found, do not indent
            (+ igor-tab-width start-indent)
          start-indent))

       ;; Cases depending on previous line indent
       (t
        (igor-previous-line-of-code)
        ;; Block open stmts increase next line indent
        (if (looking-at igor-openblock-re)
            (+ (current-indentation) igor-tab-width)
          ;; By default, just copy indent from prev line
          (current-indentation)))))))

(defun igor-indent-to-column (col)
  "Indent line of code containing pointer up to column COL."
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at igor-blank-re))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))

(defun igor-indent-line ()
  "Indent current line for IgorPro."
  (interactive)
  (igor-indent-to-column (igor-calculate-indent)))

;;; Autoload/unload
(require 'igor-exec)

(defvar igor-reload-include-list ()
  "List to hold names of include files to load after saving")
(defvar igor-reopen-window-list ()
  "List to hold names of windows to open after saving")

(defun igor-unload-igor-procedure ()
  (message "Unloading Igor Procedure")
  (if (igor-is-should-autoload)
      (let ((curr-include
             (igor-curr-filename-no-ext))
            (local-include-list
             (igor-exec-local-includes-list "Procedure"))
            (full-include-list
             (igor-exec-include-list))
            (open-windows
             (igor-exec-open-proc-window-list)))
        (if (member curr-include full-include-list)
            (progn
              (setq igor-reload-include-list
                    (append igor-reload-include-list local-include-list))
              (setq igor-reopen-window-list
                    (append igor-reopen-window-list open-windows))
              (dolist (this-window open-windows)
                (igor-exec-cmd-close-procedure this-window))
              (dolist (this-include local-include-list)
                (igor-exec-execute
                 (igor-exec-cmd-delete-include this-include)))
              (igor-exec-execute
               (igor-exec-cmd-compileprocedures))
              (igor-wait-for-procs-compiled))
          nil)
    nil)))

(defun igor-reload-igor-procedure ()
  (message "Reloading Igor Procedure")
  (if (igor-is-should-autoload)
      (let ((curr-include
             (igor-curr-filename-no-ext)))
        (if (> (length igor-reload-include-list) 0)
            (progn
              (dolist (this-include igor-reload-include-list)
                (igor-exec-execute
                 (igor-exec-cmd-insert-include this-include))
                (setq igor-reload-include-list
                      (delete this-include igor-reload-include-list)))
              (igor-exec-execute
               (igor-exec-cmd-compileprocedures)))
          nil))
    nil))

(defun igor-is-should-autoload ()
  "Returns t if autoloading is appropriate, nil if not"
  (and
   igor-use-autoreload
   (equal "ipf" (file-name-extension buffer-file-name))
   (igor-exec-is-igor-running)))

(defun igor-is-proc-need-reload (include-name)
  "Returns t if INCLUDE-NAME needs to be loaded back into Igor, nil if not"
  (member include-name igor-reload-include-list))

(defun igor-wait-for-procs-compiled ()
  (let ((wait-time 0))
    (progn
      (while (and (< wait-time 10)
                 (not (equal (igor-exec-is-procs-compiled) t)))
        (progn
          (setq wait-time (+ wait-time 0.5))
          (sleep-for 0.5))))))

(defun igor-curr-filename-no-ext ()
  "Returns the current buffer's filename without its extension"
  (file-name-nondirectory
   (file-name-sans-extension
    (buffer-file-name))))
(defun igor-curr-filename ()
  "Returns the current buffer's filename (by itself)"
  (file-name-nondirectory
   (buffer-file-name)))

(add-hook 'igor-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook
                       'igor-unload-igor-procedure nil t)))
(add-hook 'igor-mode-hook
          '(lambda ()
             (add-hook 'after-save-hook
                       'igor-reload-igor-procedure nil t)))

;; Clear memory of keyword lists (which are now saved in regexps)
(setq igor-procdec-keywords nil)
(setq igor-procsub-keywords nil)
(setq igor-objrefs-keywords nil)
(setq igor-flowcontrol-keywords nil)
(setq igor-hash-keywords nil)
(setq igor-other-keywords nil)
(setq igor-builtin-functions nil)
(setq igor-builtin-operations nil)

;; Define this mode
(define-derived-mode igor-mode fundamental-mode "Igor"
  "Major mode for editing IgorPro procedure files."
  (set (make-local-variable 'font-lock-defaults) igor-font-lock-defaults)
  (set-syntax-table igor-syntax-table)
  (set (make-local-variable 'indent-line-function) 'igor-indent-line)
  (set (make-local-variable 'tab-width) igor-tab-width)
  (set (make-local-variable 'beginning-of-defun-function) 'igor-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'igor-end-of-defun)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")
  (setq imenu-generic-expression igor-imenu-generic-expression)
  (imenu-add-to-menubar "Igor"))

(provide 'igor-mode)
;;; igor-mode.el ends here
