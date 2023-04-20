structure CalcLrVals = CalcLrValsFun(
structure Token = LrParser.Token);
structure CalcLex = CalcLexFun(
structure Tokens = CalcLrVals.Tokens);
structure CalcParser = Join(
structure ParserData = CalcLrVals.ParserData
structure Lex=CalcLex
structure LrParser=LrParser);