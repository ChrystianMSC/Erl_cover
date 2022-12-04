# [Erlang/OTP](https://www.erlang.org) **Branch Coverage Tool**

**Erlang** is a programming language and runtime system for building massively scalable soft real-time systems with requirements on high availability.

**OTP** is a set of Erlang libraries, which consists of the Erlang runtime system, a number of ready-to-use components mainly written in Erlang, and a set of design principles for Erlang programs. [Learn more about Erlang and OTP](http://erlang.org/doc/system_architecture_intro/sys_arch_intro.html).

**Cover** is a module from OTP that provides a set of functions for coverage analysis of Erlang programs, counting how many times each executable line of code is executed when a program is run.
An executable line contains an Erlang expression such as a matching or a function call. A blank line or a line containing a comment, function head or pattern in a case- or receive statement is not executable.

**This project** aims to expand the cover tool and support branch coverage for Erlang (and Elixir). A branch a possible execution path the code can take after a decision such as an if statement, thus giving a more detailed output on test coverage.

It was made in UFMG at the compilers Lab in partnership with the Elixir/Dashbit team

## How to use it

In order to use this coverage tool, you need to clone this repository, access it, and then open ans erl terminal, and compile the parser module, after this you can run this function using the target file name as the parameter:

```c(parser).
evaluate_expression("FileName").
```

In the future, we intend to make the compilation process more scalable using a MakeFile.

Thank you for acessing our project
