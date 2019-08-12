# PPX to CPS/Defuncionalization

Projeto developed under supervision of Simão Melo de Sousa and Mário Pereira

This project has the following package dependecies:

+ ppx_tools;
+ compiler-libs.

Both the packages can be obtained through opam, the package manager for OCaml.

The editor used for this project was *Visual Studio Code*, so by cloning the source code, in **PPX_CPS** of this git repository, to a vscode diretory of one's choice, the PPX is ready to use (assuming all dependencies were taken care of).

In order to transform code to CPS (Continuation Passing Style) or Defuncionalization, one's must had %CPS or %Defunc, respectively, to the expression to transform. With this, using **make** will trigger the commands in Makefile resulting in the transformation of code and its execution, presenting the result in the terminal. 

By default, the file with %CPS or %Defunc is Testes/test.ml and the resulting code transformation is in Resultados/result. Both this files can be changed by altering the variables **FILE** and **RESULT**, respectively, in Makefile. PPX_CPS.ml, inside PPX_CPS, is the file responsible to performe all the necessary changes in the code in order to have it transformed into CPS or Defuncionalization.

If one's wishes to see the effect of the transformation of code, without having to install and configure all the dependencies, there are examples of tests in directory Testes (using %CPS and/or %Defunc) and the resulting code transformated, of some of the tests, in Resultados.

