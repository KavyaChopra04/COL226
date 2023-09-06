program SumOfTwoNumbers;
var a, b, sum : real;
begin
    write ('Enter the value of a: ');
    readln (a);
    write ('Enter the value of b: ');
    readln (b);
    sum := a+b;
    writeln ('Value of sum: ', sum:0:6);
    writeln;
    write ('Press any key to finish . . . ');
    readkey;
end.
