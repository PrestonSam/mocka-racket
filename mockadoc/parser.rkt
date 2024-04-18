#lang brag

event : /NEWLINE* /"EVENT" STRING typedef /NEWLINE columns /NEWLINE*

typedef : /"TYPEDEF" STRING

columns : (column /NEWLINE*)+

column : /"-" template-name internal-name druid-type generator-name suffix? /NEWLINE

@template-name : STRING

@internal-name : IDENTIFIER

@druid-type : DRUID-TYPE

@generator-name : IDENTIFIER

suffix : /"AS" suffix-modifier (/"AND" suffix-modifier)*

primary-timestamp-suffix : /"PRIMARY" /"TIMESTAMP"

personal-suffix : /"PERSONAL"

unique-suffix : /"UNIQUE"

suffix-modifier : primary-timestamp-suffix
                | personal-suffix
                | unique-suffix
