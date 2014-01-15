{ akfquiz - style sheet data }
{ this file is in the public domain }

unit styles;

Interface

procedure StylePrint;
procedure StyleSchool;
procedure StyleColor(color: integer);


Implementation

procedure StylePrint;
begin
WriteLn;
WriteLn('/* styles for printing */');
WriteLn('@media print {');
WriteLn('  html { color:black; background-color:white; }');
WriteLn('  body { margin-top:1.7cm; margin-bottom:1.4cm; '
        + 'margin-left:2cm; margin-right:2cm; }');
WriteLn('  a:link, a:visited { color:black; text-decoration:none; }');
WriteLn('  h1 { font-weight:bold; text-align:center; '
        + 'text-decoration:underline; page-break-after:avoid; }');
WriteLn('  .error { text-align:center; font-weight:bold; font-style:italic; }');
WriteLn('  .comment { clear:both; font-style:italic; }');
WriteLn('  #metadata, .hint, .assessment {');
WriteLn('     margin-top:1ex; padding:.5ex; border:solid thin black; '
        + 'font-style:italic; }');
WriteLn('  .assessment { clear:both }');
WriteLn('  .question { clear:both; font-weight:bold; font-style:italic; '
        + 'page-break-after:avoid; }');
WriteLn('  .answer { margin-left:1em; margin-right:1em; }');

{ bug-workaround }
WriteLn('  .answer input { background-color:transparent; color:black; }');
WriteLn('  .correct { font-weight:bold; }');
WriteLn('  .points { text-align:center; font-style:italic; }');
WriteLn('  .assessment { clear:both; text-align:center; }');
WriteLn('  .wrong { text-decoration:line-through; }');
WriteLn('  .result { font-weight:bold; font-style:italic; }');
WriteLn('  .buttons, .resultlink, .home, noscript { display:none; }');
WriteLn('  .made { font-family:sans-serif; font-size:x-small; '
        + 'text-align:right; }');
WriteLn('}');
WriteLn
end; { StylePrint }

procedure StyleSchool;
begin
{ common to all media }
WriteLn('html { font-family:cursive; }');
WriteLn('h1 { text-decoration:underline; text-align:center; }');
WriteLn('.result { text-align:center; '
        + 'font-weight:bold; font-size:larger; }');
WriteLn('.resultlink, .home { text-align:center; }');
WriteLn('.comment { clear:both; margin-top:1ex; padding:.5ex; '
        + 'font-family:sans-serif; }');
WriteLn('#metadata, .hint { margin-top:1ex; padding:.5ex; '
        + 'font-family:sans-serif; }');
WriteLn('.question { clear:both; font-size:larger; font-weight:bold; }');
WriteLn('.answer { margin-left:1em; margin-right:1em; }');
WriteLn('.wrong { text-decoration:line-through; }');
WriteLn('.points { text-align:center; font-weight:bold; }');
WriteLn('.assessment { clear:both; margin-top:1ex; padding:.5ex; '
        + 'text-align:center; font-family:sans-serif; }');
WriteLn('.error { text-align:center; font-weight:bold; font-style:italic; '
        + 'font-size:larger; }');
WriteLn('.buttons { margin-top:2em; margin-bottom:2em; text-align:center; }');
WriteLn('.made { font-family:sans-serif; font-size:x-small; '
        + 'text-align:right; }');

{ styles for screens }
WriteLn;
WriteLn('/* styles for screens */');
WriteLn('@media screen {');
WriteLn('  html { color:#0000aa; background-image:url(school.png); ');
WriteLn('         position:relative; /* workaround for IE bug */ }');
WriteLn('  body { margin:1ex 8%; }');
WriteLn('  .error { color:red; }');
WriteLn('  #metadata, .comment, .hint, .assessment {');
WriteLn('     border-style:outset; border-width:medium; ');
WriteLn('     color:black; background-color:#eee; border-color:#eee; }');
WriteLn('  .answer input { color:black; }');
WriteLn('  .correct { background-color:#dfd; color:black; }');
WriteLn('  .buttons input { color:black; background-color:#ccc; }');
WriteLn('}')
end; { StyleSchool }

procedure StyleColor(color: integer);
begin
WriteLn('/* akfquiz - styles for blue theme */');
WriteLn('/* this file is in the public domain */');

{ common to all media }
WriteLn('h1, .result { text-align:center; font-weight:bold; }');
WriteLn('.error { text-align:center; font-weight:bold; '
        + 'font-style:italic; }');
WriteLn('.question { clear:both;  font-weight:bold; }');
WriteLn('#metadata, .comment { clear:both; font-style:italic; }');
WriteLn('.hint { margin-top:1ex; font-style:italic; }');
WriteLn('.answer { margin-left:1em; margin-right:1em; }');
WriteLn('.wrong { text-decoration:line-through; }');
WriteLn('.points { text-align:center; font-style:italic; }');
WriteLn('.assessment { clear:both; text-align:center; }');
WriteLn('.made { font-family:sans-serif; font-size:x-small; '
        + 'text-align:right; }');

WriteLn;
WriteLn('/* styles for screens */');
WriteLn('@media screen {');
case color of
 1: WriteLn('  html { color:black; background-color:#bbbbff; }');
 otherwise
    WriteLn('  html { color:black; background-color:#d8d0c8; }');
 end;
WriteLn('  body { margin:1ex 8%; }');
WriteLn('  h1, .result {');
WriteLn('     border:thick outset; padding:12px; margin:1em 15%;');
Write('     ');
case color of
 1: WriteLn('color:#ffffdd; background-color:#000055; border-color:#5555AA;}');
 otherwise
    WriteLn('color:#ffffdd; background-color:#605030; border-color:#605030;}');
 end;
WriteLn('  .hint, .assessment {');
WriteLn('     border:thick inset; font-weight:bold; padding:1ex;');
Write('     ');
case color of
 1: WriteLn('color:#ffffdd; background-color:#000055; border-color:#5555AA; }');
 otherwise
    WriteLn('color:#ffffdd; background-color:#605030; border-color:#605030; }');
 end;
WriteLn('  .resultlink, .home { text-align:center; }');
WriteLn('  .error { color:red; }');
WriteLn('  .question { color:#000088; }');
case color of
 1: WriteLn('  .answer input { color:black; background-color:#bbbbff; }');
 otherwise
    WriteLn('  .answer input { color:black; background-color:#d8d0c8; }');
 end;
case color of
 1: WriteLn('  .correct { background-color:#ddd; color:black; }');
 otherwise
    WriteLn('  .correct { background-color:#a8a098; color:black; }');
 end;
 
WriteLn('  .buttons { margin-top:2em; margin-bottom:2em; '
        + 'text-align:center; }');
case color of
 1: WriteLn('  .buttons input { color:#ffffdd; background-color:#000055; }');
 otherwise
    WriteLn('  .buttons input { color:#ffffdd; background-color:#605030; }');
 end;
WriteLn('}')
end; { StyleColor }

end.