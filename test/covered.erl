if 1 == 2 -> if 1 == 1 -> true; true -> false end; true -> false end,
if 1 == 1 -> if 1 == 1 -> true; true -> false end; true -> false end,
A = 5,
case A of 
    5 -> case A of 
            5 -> io:fwrite("The value of A is 5~n");
            6 -> io:fwrite("The value of A is 6") 
        end;
    6 -> io:fwrite("The value of A is 6") 
end,
case A of 
    5 -> io:fwrite("The value of A is 5~n");
    6 -> io:fwrite("The value of A is 6")
end,
true andalso begin 1, true end,
true orelse true.