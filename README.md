prolog_parser
=============
Top Down parser

   Implementation of simple calculator using  top down parser
in prolog.


Usage:
(debuggig the sample evaluation(i.e ex1,pex1) with gspy is recommended
for demonstration of parsing.

  use ex1,ex2,ex3 .. as pass example of top down parsing with
	grammer rules for simple calculator.

	use fl1,fl2 as failed example


	pex1,pex2...as pass exapmles using parse table
	fex1,pex2.. as failed exapmle using parse table

	compile -for using the second pass of the parser..it evaluates
	the explicitly created syntax tree with mknode and mkleaf.
	will also evaluate and print results

	ct- automatic parse table creation(will output to file 			table.pl)(experimental)