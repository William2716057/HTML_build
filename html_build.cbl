      IDENTIFICATION DIVISION.
       PROGRAM-ID. HTML-BUILDER.
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HTML-FILE ASSIGN TO "index.html"
               ORGANIZATION IS LINE SEQUENTIAL.
 
       DATA DIVISION.
       FILE SECTION.
       FD  HTML-FILE.
       01  HTML-LINE               PIC X(1000).
 
       WORKING-STORAGE SECTION.
       01  WS-CHOICE               PIC X(2).
       01  WS-ELEMENT-TYPE         PIC X(2).
       01  WS-CONTINUE             PIC X VALUE "Y".
       01  WS-PAGE-TITLE           PIC X(100).
       01  WS-TEXT                 PIC X(500).
       01  WS-TAG                  PIC X(10).
       01  WS-LABEL                PIC X(20).
 
      *> Style building: one property at a time, appended via pointer
       01  WS-STYLE                PIC X(700).
       01  WS-STYLE-PTR            PIC 9(4).
       01  WS-PROP                 PIC X(20).
       01  WS-VALUE                PIC X(100).
       01  WS-YN                   PIC X.
 
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY " "
           DISPLAY "=============================================="
           DISPLAY "          COBOL HTML PAGE BUILDER"
           DISPLAY "=============================================="
           DISPLAY " "
           DISPLAY "Enter the title of your web page:"
           ACCEPT WS-PAGE-TITLE
 
           OPEN OUTPUT HTML-FILE
           PERFORM WRITE-HTML-HEADER
           PERFORM WRITE-CSS
 
           MOVE "Y" TO WS-CONTINUE
           PERFORM UNTIL WS-CONTINUE = "N"
               PERFORM DISPLAY-MAIN-MENU
               ACCEPT WS-CHOICE
               EVALUATE WS-CHOICE
                   WHEN "1"
                       PERFORM CREATE-ELEMENT
                   WHEN "2"
                       PERFORM SAVE-PAGE
                       MOVE "N" TO WS-CONTINUE
                   WHEN OTHER
                       DISPLAY " "
                       DISPLAY "Invalid choice."
               END-EVALUATE
           END-PERFORM
 
           CLOSE HTML-FILE
 
           DISPLAY "=============================================="
           DISPLAY "        HTML PAGE CREATED SUCCESSFULLY"
           DISPLAY "=============================================="
           DISPLAY "File: index.html"
           STOP RUN.
 
       DISPLAY-MAIN-MENU.
           DISPLAY " "
           DISPLAY "============== PAGE BUILDER =============="
           DISPLAY "1. Add element"
           DISPLAY "2. Save and exit"
           DISPLAY "Choice: ".
 
       CREATE-ELEMENT.
           DISPLAY " "
           DISPLAY "============== ADD ELEMENT ==============="
           DISPLAY "1. Heading"
           DISPLAY "2. Paragraph"
           DISPLAY "3. Button"
           DISPLAY "4. Div / Container"
           DISPLAY "Element type: "
           ACCEPT WS-ELEMENT-TYPE
 
           EVALUATE WS-ELEMENT-TYPE
               WHEN "1"
                   MOVE "h1" TO WS-TAG
                   MOVE "Enter heading text:" TO WS-LABEL
               WHEN "2"
                   MOVE "p" TO WS-TAG
                   MOVE "Enter paragraph text:" TO WS-LABEL
               WHEN "3"
                   MOVE "button" TO WS-TAG
                   MOVE "Enter button text:" TO WS-LABEL
               WHEN "4"
                   MOVE "div" TO WS-TAG
                   MOVE "Enter container text:" TO WS-LABEL
               WHEN OTHER
                   DISPLAY "Invalid element type."
                   MOVE SPACES TO WS-TAG
           END-EVALUATE
 
           IF WS-TAG NOT = SPACES
               DISPLAY " "
               DISPLAY WS-LABEL
               MOVE SPACES TO WS-TEXT
               ACCEPT WS-TEXT
               PERFORM GET-STYLES
               PERFORM WRITE-ELEMENT
               DISPLAY "Element added."
           END-IF.
 
      *> Writes one element on one line, e.g. <p style="...">text</p>
       WRITE-ELEMENT.
           MOVE SPACES TO HTML-LINE
           IF WS-STYLE = SPACES
               STRING
                   "<" FUNCTION TRIM(WS-TAG) ">"
                   FUNCTION TRIM(WS-TEXT)
                   "</" FUNCTION TRIM(WS-TAG) ">"
                   DELIMITED BY SIZE
                   INTO HTML-LINE
               END-STRING
           ELSE
               STRING
                   "<" FUNCTION TRIM(WS-TAG)
                   " style=""" FUNCTION TRIM(WS-STYLE) """>"
                   FUNCTION TRIM(WS-TEXT)
                   "</" FUNCTION TRIM(WS-TAG) ">"
                   DELIMITED BY SIZE
                   INTO HTML-LINE
               END-STRING
           END-IF
           WRITE HTML-LINE.
 
      *> Appends "prop:value;" to WS-STYLE if WS-VALUE is not blank
       APPEND-STYLE.
           IF WS-VALUE NOT = SPACES
               STRING
                   FUNCTION TRIM(WS-PROP) ":"
                   FUNCTION TRIM(WS-VALUE) ";"
                   DELIMITED BY SIZE
                   INTO WS-STYLE
                   WITH POINTER WS-STYLE-PTR
               END-STRING
           END-IF.
 
       GET-STYLES.
           MOVE SPACES TO WS-STYLE
           MOVE 1 TO WS-STYLE-PTR
           DISPLAY " "
           DISPLAY "Styling (press ENTER to skip any option)"
 
           MOVE "font-size" TO WS-PROP
           DISPLAY "Font size (example: 24px):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           MOVE "color" TO WS-PROP
           DISPLAY "Text colour (example: #333333):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           MOVE "background-color" TO WS-PROP
           DISPLAY "Background colour (example: #eeeeee):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           MOVE "text-align" TO WS-PROP
           DISPLAY "Alignment (left / center / right):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           MOVE "padding" TO WS-PROP
           DISPLAY "Padding (example: 20px):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           MOVE "margin" TO WS-PROP
           DISPLAY "Margin (example: 10px):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           MOVE "border" TO WS-PROP
           DISPLAY "Border (example: 1px solid black):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           MOVE "border-radius" TO WS-PROP
           DISPLAY "Border radius (example: 8px):"
           MOVE SPACES TO WS-VALUE
           ACCEPT WS-VALUE
           PERFORM APPEND-STYLE
 
           DISPLAY "Bold? Y/N:"
           MOVE SPACE TO WS-YN
           ACCEPT WS-YN
           IF FUNCTION UPPER-CASE(WS-YN) = "Y"
               MOVE "font-weight" TO WS-PROP
               MOVE "bold" TO WS-VALUE
               PERFORM APPEND-STYLE
           END-IF
 
           DISPLAY "Italic? Y/N:"
           MOVE SPACE TO WS-YN
           ACCEPT WS-YN
           IF FUNCTION UPPER-CASE(WS-YN) = "Y"
               MOVE "font-style" TO WS-PROP
               MOVE "italic" TO WS-VALUE
               PERFORM APPEND-STYLE
           END-IF.
 
       WRITE-HTML-HEADER.
           MOVE "<!DOCTYPE html>" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "<html lang=""en"">" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "<head>" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "<meta charset=""UTF-8"">" TO HTML-LINE
           WRITE HTML-LINE
 
           MOVE SPACES TO HTML-LINE
           STRING
               "<meta name=""viewport"" "
               "content=""width=device-width, initial-scale=1.0"">"
               DELIMITED BY SIZE
               INTO HTML-LINE
           END-STRING
           WRITE HTML-LINE
 
           MOVE SPACES TO HTML-LINE
           STRING
               "<title>" FUNCTION TRIM(WS-PAGE-TITLE) "</title>"
               DELIMITED BY SIZE
               INTO HTML-LINE
           END-STRING
           WRITE HTML-LINE
 
           MOVE "</head>" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "<body>" TO HTML-LINE
           WRITE HTML-LINE.
 
       WRITE-CSS.
           MOVE "<style>" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "body {" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "font-family: Arial, sans-serif;" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "margin: 40px;" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "background-color: #ffffff;" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "color: #222222;" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "}" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "button {" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "cursor: pointer;" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "}" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "</style>" TO HTML-LINE
           WRITE HTML-LINE.
 
       SAVE-PAGE.
           MOVE "</body>" TO HTML-LINE
           WRITE HTML-LINE
           MOVE "</html>" TO HTML-LINE
           WRITE HTML-LINE
           DISPLAY " "
           DISPLAY "Saving HTML page..."
           DISPLAY "index.html is ready.".
