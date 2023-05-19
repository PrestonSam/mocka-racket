#lang brag

body : /NEWLINE* [include-statement /NEWLINE] /NEWLINE* statements /NEWLINE*

include-statement : /"INCLUDE" STRING-LITERAL (/"," STRING-LITERAL)*

statements : definition (/NEWLINE+ definition)*

@definition : single-definition
            | nested-definition


single-definition : /"DEF" IDENTIFIER /"=" value
                  | /"DEF" IDENTIFIER /NEWLINE /TAB1 /"=" /"ONEOF" (/NEWLINE /TAB1 /"|" weighted-value)+


nested-definition : using-ids? /"DEF" assign-ids ((/NEWLINE match-1)+ | (/NEWLINE assign-1)+)

using-ids : /"USING" names

assign-ids : names

@names : IDENTIFIER (/"," IDENTIFIER)*



match-1 : /TAB1 "?" matcher-1 ((/NEWLINE assign-2)+ | (/NEWLINE match-2)+)

matcher-1 : value
          | /"ONEOF" (/NEWLINE /TAB1 /"|" value)+

assign-1 : /TAB1 "=" weight? value-1 (/NEWLINE assign-2)+

value-1 : value
        | /"ONEOF" (/NEWLINE /TAB1 /"|" weighted-value)+



match-2 : /TAB2 "?" matcher-2 ((/NEWLINE assign-3)* | (/NEWLINE match-3)*)

matcher-2 : value
          | /"ONEOF" (/NEWLINE /TAB2 /"|" value)+

assign-2 : /TAB2 "=" weight? value-2 (/NEWLINE assign-3)*

value-2 : value
        | /"ONEOF" (/NEWLINE /TAB2 /"|" weighted-value)+



match-3 : /TAB3 "?" matcher-3 ((/NEWLINE assign-4)* | (/NEWLINE match-4)*)

matcher-3 : value
          | /"ONEOF" (/NEWLINE /TAB3 /"|" value)+

assign-3 : /TAB3 "=" weight? value-3 (/NEWLINE assign-4)*

value-3 : value
        | /"ONEOF" (/NEWLINE /TAB3 /"|" weighted-value)+



match-4 : /TAB4 "?" matcher-4 (/NEWLINE assign-4)+

matcher-4 : value
          | /"ONEOF" (/NEWLINE /TAB4 /"|" value)+

assign-4 : /TAB4 "=" weight? value-4 (/NEWLINE assign-5)*

value-4 : value
        | /"ONEOF" (/NEWLINE /TAB4 /"|" weighted-value)+



assign-5 : /TAB5 "=" weight? value-5

value-5 : value
        | /"ONEOF" (/NEWLINE /TAB5 /"|" weighted-value)+



weight : WEIGHT

weighted-value : WEIGHT? value

@value : timestamp-date-value
       | identifier-value
       | literal-value
       | integer-value
       | string-value
       | real-value
       | join-value
       | any-value

timestamp-date-value : /"timestamp/date" DATE-LITERAL DATE-LITERAL

identifier-value : IDENTIFIER

literal-value : STRING-LITERAL

integer-value : /"integer" INTEGER-LITERAL [INTEGER-LITERAL]

string-value : /"string" INTEGER-LITERAL INTEGER-LITERAL

real-value : /"real" REAL-LITERAL [REAL-LITERAL]

join-value : /"join" (identifier-value | literal-value)+

any-value : /"any"

