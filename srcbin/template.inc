{ templates for AKFQuiz
  public domain }

const template_en =
  'AKFQuiz'+nl+
  nl+
  '# the title is important. Use a unique one'+nl+
  'title: Unnamed'+nl+
  'author:'+nl+
  'copyright:'+nl+
  'translator:'+nl+
  'default: I don''t know'+nl+
  'language: en'+nl+
  'charset: '+sys_charset+nl+
  'rtl: no'+nl+
  'neutral: no'+nl+
  'baseURI:'+nl+
  'layout: q-blue.css'+nl+
  'noindex: no'+nl+
  nl+
  nl+
  '#######################################################################'+nl+
  '# The rest up to "end" are examples. Please delete them.'+nl+
  nl+
  'comment:'+nl+
  'You can put a comment nearly anywhere.'+nl+
  '.'+nl+
  'A dot alone on one line means a new paragraph'+nl+
  nl+
  nl+
  '# introduce questions with just one correct answer with "question:"'+nl+
  '# As score values you can use any deliberate integer'+nl+
  nl+
  'question:'+nl+
  'What was the question?'+nl+
  nl+
  '0 Answer 1'+nl+
  '1 Answer 2'+nl+
  '0 Answer 3'+nl+
  nl+
  nl+
  '# introduce questions with more than one possible correct'+nl+
  '# answers with "query:"'+nl+
  '# As score values you should not use 0!'+nl+
  nl+
  'query:'+nl+
  'What was the question?'+nl+
  '(more than one possible answers)'+nl+
  nl+
  '-1 Answer 1'+nl+
  '+1 Answer 2'+nl+
  '+1 Answer 3'+nl+
  nl+
  'end'+nl;


const template_de =
  'AKFQuiz'+nl+
  nl+
  '# Der Titel ist wichtig. Benutze einen eindeutigen.'+nl+
  'Titel: Unbenannt'+nl+
  'Autor:'+nl+
  'Copyright:'+nl+
  'Uebersetzer:'+nl+
  'StandardAntwort: Weiß ich nicht'+nl+
  'Sprache: de'+nl+
  'Zeichensatz: '+sys_charset+nl+
  'rtl: no'+nl+
  'Neutral: nein'+nl+
  'HauptURI:'+nl+
  'Layout: q-blue.css'+nl+
  'NoIndex: nein'+nl+
  nl+
  nl+
  '#######################################################################'+nl+
  '# Der Rest bis zum "Ende" sind nur Beispiele. Bitte löschen.'+nl+
  nl+
  'Kommentar:'+nl+
  'Ein Kommentar kann an beliebiger Stelle erfolgen.'+nl+
  '.'+nl+
  'Ein einzelner Punkt in einer Zeile leitet einen neun Abschnitt ein.'+nl+
  nl+
  nl+
  '# Fragen mit nur einer Antwortmöglichkeit werden mit "Frage:" eingeleitet'+nl+
  '# Als Punktzahlen kann beliebige Ganz-Zahlen verwenden'+nl+
  nl+
  'Frage:'+nl+
  'Wie lautet die Frage?'+nl+
  nl+
  '0 Antwort 1'+nl+
  '1 Antwort 2'+nl+
  '0 Antwort 3'+nl+
  nl+
  nl+
  '# Fragen mit mehreren gültigen Antwortmöglichkeiten werden mit'+nl+
  '# "Anfrage:" oder "Multi:" eingeleitet'+nl+
  '# Als Punktzahlen sollte man nie die 0 verwenden'+nl+
  nl+
  'Anfrage:'+nl+
  'Wie war nochmal die Frage?'+nl+
  '(mehr als eine Antwort möglich)'+nl+
  nl+
  '-1 Antwort 1'+nl+
  '+1 Antwort 2'+nl+
  '+1 Antwort 3'+nl+
  nl+
  'Ende'+nl;
