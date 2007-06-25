; AKFQuiz
; Script for NSIS 2.06 (Nullsoft Scriptable Install System)
; http://nsis.sf.net/

!include akfquiz.nsh

!define PUBLISHER "AKFoerster"
!define PUBLISHERURL "http://akfoerster.de/"
!define SUPPORTURL "http://savannah.nongnu.org/p/akfquiz"
!define OUTFILE "akfquiz-${VERSION}-w32.exe"
!define COMMENT_EN "The original AKFQuiz is available for GNU/Linux"
!define COMMENT_DE "Das original AKFQuiz ist für GNU/Linux erhältlich"

Name "${NAME} ${VERSION}"
OutFile "${OUTFILE}"

SetCompressor lzma
XPStyle on

InstallDir "$PROGRAMFILES\akfquiz"

InstallDirRegKey HKLM \
  "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
  "InstallLocation"

LoadLanguageFile "${NSISDIR}\Contrib\Language files\English.nlf"
LoadLanguageFile "${NSISDIR}\Contrib\Language files\German.nlf"

VIProductVersion "${VERSION}.${PKGVERSION}"

VIAddVersionKey /LANG=${LANG_ENGLISH} FileVersion "${VERSION}"
VIAddVersionKey /LANG=${LANG_ENGLISH} FileDescription "${Name} Setup"
VIAddVersionKey /LANG=${LANG_ENGLISH} OriginalFilename "${OUTFILE}"
VIAddVersionKey /LANG=${LANG_ENGLISH} ProductName "${NAME}"
VIAddVersionKey /LANG=${LANG_ENGLISH} ProductVersion "${VERSION}"
VIAddVersionKey /LANG=${LANG_ENGLISH} CompanyName "${PUBLISHER}"
VIAddVersionKey /LANG=${LANG_ENGLISH} LegalCopyright \
       "Copyright AKFoerster, GPL v2 or later"
VIAddVersionKey /LANG=${LANG_ENGLISH} Comments "${COMMENT_EN}"

VIAddVersionKey /LANG=${LANG_GERMAN} FileVersion "${VERSION}"
VIAddVersionKey /LANG=${LANG_GERMAN} FileDescription "${Name} Installation"
VIAddVersionKey /LANG=${LANG_GERMAN} OriginalFilename "${OUTFILE}"
VIAddVersionKey /LANG=${LANG_GERMAN} ProductName "${NAME}"
VIAddVersionKey /LANG=${LANG_GERMAN} ProductVersion "${VERSION}"
VIAddVersionKey /LANG=${LANG_GERMAN} CompanyName "${PUBLISHER}"
VIAddVersionKey /LANG=${LANG_GERMAN} LegalCopyright \
       "Copyright AKFoerster, GPL V2 oder später"
VIAddVersionKey /LANG=${LANG_GERMAN}  Comments "${COMMENT_DE}"

LangString HOMEPAGE   ${LANG_ENGLISH} "http://akfquiz.nongnu.org/"
LangString HOMEPAGE   ${LANG_GERMAN}  "http://akfquiz.nongnu.org/deutsch.html"
LangString INTERNET   ${LANG_ENGLISH} "${NAME} on the Internet"
LangString INTERNET   ${LANG_GERMAN}  "${NAME} im Internet"
LangString REQUIRED   ${LANG_ENGLISH} "(required)"
LangString REQUIRED   ${LANG_GERMAN}  "(benötigt)"
LangString MENUSHORTC ${LANG_ENGLISH} "menu shortcuts"
LangString MENUSHORTC ${LANG_GERMAN}  "Menü-Einträge"
LangString DTICONS    ${LANG_ENGLISH} "desktop icons"
LangString DTICONS    ${LANG_GERMAN}  "Symbole auf dem Arbeitsplatz"
LangString EXTRAQF    ${LANG_ENGLISH} "extra quiz-files"
LangString EXTRAQF    ${LANG_GERMAN}  "zusätzliche Quiz-Dateien"
LangString QUIZDIR    ${LANG_ENGLISH} "AKFQuiz directory"
LangString QUIZDIR    ${LANG_GERMAN}  "AKFQuiz Verzeichnis"
LangString MKQUIZ     ${LANG_ENGLISH} "&mkquiz: update HTML file (JavaScript)"
LangString MKQUIZ     ${LANG_GERMAN}  "&mkquiz: HTML-Datei erneuern (JavaScript)"
LangString SCRQUIZ    ${LANG_ENGLISH} "&scrquiz: run quiz on textconsole"
LangString SCRQUIZ    ${LANG_GERMAN}  "&scrquiz: Quiz auf Textkonsole starten"
LangString LINEQUIZ   ${LANG_ENGLISH} "&linequiz: run quiz line-oriented"
LangString LINEQUIZ   ${LANG_GERMAN}  "&linequiz: Quiz zeilen-orientiert starten"
LangString GRQUIZ     ${LANG_ENGLISH} "&grquiz: run quiz in graphic mode"
LangString GRQUIZ     ${LANG_GERMAN}  "&grquiz: Quiz im Grafik-Modus starten"
LangString EDITQUIZ   ${LANG_ENGLISH} "&edit quiz"
LangString EDITQUIZ   ${LANG_GERMAN}  "Quiz &bearbeiten"
LangString UPDATEHTML ${LANG_ENGLISH} "update HTML files"
LangString UPDATEHTML ${LANG_GERMAN}  "HTML-Dateien auffrischen"
LangString USAGE      ${LANG_ENGLISH} "usage instructions"
LangString USAGE      ${LANG_GERMAN}  "Benutzungs-Hinweise"
LangString USAGEFILE  ${LANG_ENGLISH} "windows-en.txt"
LangString USAGEFILE  ${LANG_GERMAN}  "windows-de.txt"
LangString DOC        ${LANG_ENGLISH} "Documentation"
LangString DOC        ${LANG_GERMAN}  "Dokumentation"
LangString LICENSE    ${LANG_ENGLISH} "License"
LangString LICENSE    ${LANG_GERMAN}  "Lizenz"
LangString HTMLQUIZ   ${LANG_ENGLISH} "HTML-Quiz (JavaScript)"
LangString HTMLQUIZ   ${LANG_GERMAN}  "HTML-Quiz (JavaScript)"
LangString LOCINST    ${LANG_ENGLISH} "integrate into system"
LangString LOCINST    ${LANG_GERMAN}  "System-Integration"
LangString UNINSTALL  ${LANG_ENGLISH} "uninstall"
LangString UNINSTALL  ${LANG_GERMAN}  "deinstallieren"
LangString MKUNINST   ${LANG_ENGLISH} "automatically uninstallable"
LangString MKUNINST   ${LANG_GERMAN}  "automatisch deinstallierbar"
LangString FASSOC     ${LANG_ENGLISH} "file-associations"
LangString FASSOC     ${LANG_GERMAN}  "Dateinamen verknüpfen"
LangString LICFILE    ${LANG_ENGLISH} "LICENSE.txt"
LangString LICFILE    ${LANG_GERMAN}  "LIZENZ.txt"
LangString COMMENT    ${LANG_ENGLISH} "${COMMENT_EN}"
LangString COMMENT    ${LANG_GERMAN}  "${COMMENT_DE}"

; subdirectories in doc
LangString LANGDOC    ${LANG_ENGLISH} "doc\english"
LangString LANGDOC    ${LANG_GERMAN}  "doc\deutsch"

LicenseLangString LICENSETEXT ${LANG_ENGLISH} "LICENSE.txt"
LicenseLangString LICENSETEXT ${LANG_GERMAN}  "LIZENZ.txt"
LicenseLangString USAGETEXT   ${LANG_ENGLISH} "windows-en.txt"
LicenseLangString USAGETEXT   ${LANG_GERMAN}  "windows-de.txt"

LicenseData "$(LICENSETEXT)"
LicenseForceSelection off


PageEx license
  Caption ": $(USAGE)"
  LicenseText "$(USAGE)" "$(^NextBtn)"
  LicenseData "$(USAGETEXT)"
  LicenseForceSelection off
PageExEnd


Page license
Page components
Page directory
Page instfiles
UninstPage uninstConfirm
UninstPage instfiles


Section "!${NAME} $(REQUIRED)" AKFQuiz

  SectionIn RO  ; cannot be disabled

  SetOutPath "$INSTDIR\bin"
  FILE "..\mkquiz.exe"
  FILE "..\scrquiz.exe"
  FILE "..\linequiz.exe"
  FILE "..\grquiz.exe"
  FILE "..\quizstat"

  SetOutPath "$INSTDIR\share\akfquiz\sound"
  FILE "..\..\share\akfquiz\sound\README"
  FILE "..\..\share\akfquiz\sound\introsnd.ub"
  FILE "..\..\share\akfquiz\sound\infosnd.ub"
  FILE "..\..\share\akfquiz\sound\errorsnd.ub"
  FILE "..\..\share\akfquiz\sound\neutralsnd.ub"
  FILE "..\..\share\akfquiz\sound\rightsnd.ub"
  FILE "..\..\share\akfquiz\sound\wrongsnd.ub"
  
  ; only GPL-compatible quiz-files here
  SetOutPath "$INSTDIR\share\akfquiz\quiz"
  FILE "Linux-en.akfquiz"
  FILE "Linux-de.akfquiz"
  FILE "Schokolade-de.akfquiz"
  FILE "Christentum-de.akfquiz"
  FILE "Landtechnik.akfquiz"

  SetOutPath "$INSTDIR\share\akfquiz\quiz"
  CreateShortCut "$INSTDIR\share\akfquiz\quiz\$(UPDATEHTML).lnk" \
    "$INSTDIR\bin\mkquiz.exe" '--out "$INSTDIR\html" --index --auto' \
    "$INSTDIR\AKFQuiz.ico" 0
  CreateShortCut "$INSTDIR\share\akfquiz\quiz\$(HTMLQUIZ).lnk" \
    "$INSTDIR\html\index.html"

  SetOutPath "$INSTDIR\html"
  FILE "..\..\html\*.*"

  SetOutPath "$INSTDIR\doc"
  FILE "..\..\doc\*.*"
  FILE "*.txt"
  WriteINIStr "$INSTDIR\doc\akfquiz.url" InternetShortcut URL "$(HOMEPAGE)"

  SetOutPath "$INSTDIR\doc\english"
  FILE "..\..\doc\english\*.*"
  FILE "template"

  SetOutPath "$INSTDIR\doc\deutsch"
  FILE "..\..\doc\deutsch\*.*"
  FILE /oname=template "template-de"
  
  SetOutPath "$INSTDIR"
  FILE "AKFQuiz.ico"
SectionEnd


Section "libSDL $(REQUIRED)" SDL
  SetOutPath "$INSTDIR\bin"
  FILE /nonfatal "..\..\..\SDL\SDL.dll"
  FILE /nonfatal "..\..\..\SDL\README-SDL.txt"
SectionEnd


; not GPL-compatible quiz-files
Section "$(EXTRAQF)" extra
  SetOutPath "$INSTDIR\share\akfquiz\quiz"
  FILE /nonfatal "GPL-Quiz-en.akfquiz"
  FILE /nonfatal "debian-en.akfquiz"
  FILE /nonfatal "Amerika-de.akfquiz"
SectionEnd


; local installation; register in system
SectionGroup "$(LOCINST)" 

  ; make uninstaller
  Section "$(MKUNINST)" createUninstaller

    SetOutPath "$INSTDIR"
    WriteUninstaller "uninstall.exe"

    ; Write the uninstall keys for Windows
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "DisplayName" "${NAME}"
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "DisplayVersion" "${VERSION}"
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "Comments" "$(COMMENT)"
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "DisplayIcon" "$INSTDIR\AKFQuiz.ico"
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "Publisher" "${PUBLISHER}"
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "UninstallString" '"$INSTDIR\uninstall.exe"'
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "InstallLocation" "$INSTDIR"
    WriteRegDWORD HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "NoModify" 1
    WriteRegDWORD HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "NoRepair" 1
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "URLInfoAbout" "${PUBLISHERURL}"
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "URLUpdateInfo" "$(HOMEPAGE)"
    WriteRegStr HKLM \
      "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz" \
      "HelpLink" "${SUPPORTURL}"
  SectionEnd


  ; create file-associations
  Section "$(FASSOC)" fassoc
    WriteRegStr HKCR ".akfquiz" "" "AKFQuiz"
    WriteRegStr HKCR ".akfquiz" "Content Type" "application/x-akfquiz"
    WriteRegStr HKCR ".aqz" "" "AKFQuiz"
    WriteRegStr HKCR ".aqz" "Content Type" "application/x-akfquiz"

    WriteRegStr HKCR ".akfquiz\ShellNew" "FileName" \
      "$INSTDIR\$(LANGDOC)\template"
    WriteRegStr HKCR ".akfquiz\ShellNew" "command" 'notepad.exe "%1"'

    WriteRegStr HKCR "AKFQuiz" "" "AKFQuiz"
    WriteRegStr HKCR "AKFQuiz\DefaultIcon" "" "$INSTDIR\AKFQuiz.ico"
    WriteRegStr HKCR "AKFQuiz\shell\open" "" "$(GRQUIZ)"
    WriteRegStr HKCR "AKFQuiz\shell\open\command" "" \
      '$INSTDIR\bin\grquiz.exe "%1"'
    WriteRegStr HKCR "AKFQuiz\shell\js" "" "$(MKQUIZ)"
    WriteRegStr HKCR "AKFQuiz\shell\js\command" "" \
      '$INSTDIR\bin\mkquiz.exe --out "$INSTDIR\html\" "%1"'
    WriteRegStr HKCR "AKFQuiz\shell\console" "" "$(SCRQUIZ)"
    WriteRegStr HKCR "AKFQuiz\shell\console\command" "" \
      '$INSTDIR\bin\scrquiz.exe "%1"'
    WriteRegStr HKCR "AKFQuiz\shell\line" "" "$(LINEQUIZ)"
    WriteRegStr HKCR "AKFQuiz\shell\line\command" "" \
      '$INSTDIR\bin\linequiz.exe "%1"'
    WriteRegStr HKCR "AKFQuiz\shell\edit" "" "$(EDITQUIZ)"
    WriteRegStr HKCR "AKFQuiz\shell\edit\command" "" 'notepad.exe "%1"'
  SectionEnd

  ; menu-shortcuts
  Section "$(MENUSHORTC)" menu
    CreateDirectory "$SMPROGRAMS\${NAME}"

    SetOutPath "$INSTDIR" ; sets the Working directory
    CreateShortCut "$SMPROGRAMS\${NAME}\$(INTERNET).lnk" \
      "$INSTDIR\doc\akfquiz.url"
    IfFileExists "$INSTDIR\uninstall.exe" 0 +2
      CreateShortCut "$SMPROGRAMS\${NAME}\$(UNINSTALL).lnk" \
        "$INSTDIR\uninstall.exe"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(QUIZDIR).lnk" \
      "$INSTDIR\share\akfquiz\quiz\"
  
    SetOutPath "$INSTDIR\bin" ; sets the Working directory
    CreateShortCut "$SMPROGRAMS\${NAME}\grquiz.lnk" \
      "$INSTDIR\bin\grquiz.exe"
    CreateShortCut "$SMPROGRAMS\${NAME}\scrquiz.lnk" \
      "$INSTDIR\bin\scrquiz.exe"
    CreateShortCut "$SMPROGRAMS\${NAME}\linequiz.lnk" \
      "$INSTDIR\bin\linequiz.exe"

    SetOutPath "$INSTDIR\share\akfquiz" ; sets the Working directory
    CreateShortCut "$SMPROGRAMS\${NAME}\$(UPDATEHTML).lnk" \
      "$INSTDIR\bin\mkquiz.exe" '--out "$INSTDIR\html" --index --auto'
    CreateShortCut "$SMPROGRAMS\${NAME}\$(HTMLQUIZ).lnk" \
      "$INSTDIR\html\index.html"

    CreateDirectory "$SMPROGRAMS\${NAME}\$(DOC)"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\$(USAGE).lnk" \
      "$INSTDIR\doc\$(USAGEFILE)"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\$(LICENSE).lnk" \
      "$INSTDIR\doc\$(LICFILE)"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\COPYING.lnk" \
      "$INSTDIR\doc\COPYING.txt"
    StrCmp $LANGUAGE ${LANG_GERMAN} 0 +2
      CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\COPYING (Übersetzung).lnk" \
        "$INSTDIR\doc\deutsch\gpl-ger.html"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\akfquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\akfquiz.html"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\grquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\grquiz.html"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\scrquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\scrquiz.html"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\mkquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\mkquiz.html"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\linequiz.lnk" \
      "$INSTDIR\$(LANGDOC)\linequiz.html"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\quizstat.lnk" \
      "$INSTDIR\$(LANGDOC)\quizstat.html"

    CreateDirectory "$SMPROGRAMS\${NAME}\$(DOC)\PDF"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\PDF\akfquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\akfquiz.pdf"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\PDF\grquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\grquiz.pdf"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\PDF\scrquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\scrquiz.pdf"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\PDF\mkquiz.lnk" \
      "$INSTDIR\$(LANGDOC)\mkquiz.pdf"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\PDF\linequiz.lnk" \
      "$INSTDIR\$(LANGDOC)\linequiz.pdf"
    CreateShortCut "$SMPROGRAMS\${NAME}\$(DOC)\PDF\quizstat.lnk" \
      "$INSTDIR\$(LANGDOC)\quizstat.pdf"
  SectionEnd


  Section "$(DTICONS)" desktop
    SetOutPath "$INSTDIR\bin" ; sets the Working directory
    CreateShortCut "$DESKTOP\grquiz.lnk" \
      "$INSTDIR\bin\grquiz.exe"

    CreateShortCut "$DESKTOP\$(QUIZDIR).lnk" \
      "$INSTDIR\share\akfquiz\quiz\"
  SectionEnd
SectionGroupEnd

; execute mkquiz to prepare the html directory
Section
  SetOutPath "$INSTDIR\share\akfquiz\quiz"
  ExecWait '"$INSTDIR\bin\mkquiz.exe" --out "$INSTDIR\html" --index --auto'
SectionEnd


Section "Uninstall"
  ; registry
  DeleteRegKey HKLM \
    "Software\Microsoft\Windows\CurrentVersion\Uninstall\akfquiz"
  
  DeleteRegKey HKCR ".akfquiz"
  DeleteRegKey HKCR ".aqz"
  DeleteRegKey HKCR "AKFQuiz"

  ; StartMenu
  RMDir /r "$SMPROGRAMS\${NAME}"
  
  ; Desktop
  Delete "$DESKTOP\grquiz.lnk"
  Delete "$DESKTOP\$(QUIZDIR).lnk"

  ; bin
  Delete "$INSTDIR\bin\*.*"
  RMDir  "$INSTDIR\bin"
  
  ; html
  Delete "$INSTDIR\html\*.*"
  RMDir  "$INSTDIR\html"
  
  ; doc
  Delete "$INSTDIR\doc\english\*.*"
  RMDir  "$INSTDIR\doc\english"
  Delete "$INSTDIR\doc\deutsch\*.*"
  RMDir  "$INSTDIR\doc\deutsch"
  Delete "$INSTDIR\doc\*.*"
  RMDir  "$INSTDIR\doc"
  
  ; quizfiles - only those installed by this installer
  Delete "$INSTDIR\share\akfquiz\quiz\Linux-en.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\Linux-de.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\Schokolade-de.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\Christentum-de.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\Landtechnik.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\GPL-Quiz-en.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\debian-en.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\Amerika-de.akfquiz"
  Delete "$INSTDIR\share\akfquiz\quiz\$(UPDATEHTML).lnk"
  Delete "$INSTDIR\share\akfquiz\quiz\$(HTMLQUIZ).lnk"
  RMDir  "$INSTDIR\share\akfquiz\quiz"
  Delete "$INSTDIR\share\akfquiz\sound\*.*"
  RMDir  "$INSTDIR\share\akfquiz\sound"
  RMDir  "$INSTDIR\share\akfquiz"
  RMDir  "$INSTDIR\share"
  
  ; root
  Delete "$INSTDIR\AKFQuiz.ico"
  Delete "$INSTDIR\uninstall.exe"
  RMDIR  "$INSTDIR"
SectionEnd

