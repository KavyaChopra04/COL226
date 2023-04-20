structure DataTypes = 
struct
datatype AST = PROG of BLOCK
and BLOCK = BLOCK of (DEC list * PROCDEF list * CMD list)
and DEC = DEC of ( (string list) * TYPE )
and PROCDEF = PROCDEF of ( string * BLOCK)
and TYPE = RATIONAL | BOOL | INT | STRING
and CMD = SET of (string*EXP) | WHILE of (EXP* CMD list)| IFFI of (EXP * CMD list * CMD list) | Print of EXP | Read of string | Call of string
 (*and SCOPE_TABLE = SCOPE_TABLE of (string, (Type, Value)) HashTable
and Value = ExpOp.rational | Bool | BigInt.bigint *)
and EXP =   INVERSE of EXP |
            MAKERAT of EXP |
            RAT of EXP |
            SHOWRAT of EXP |
            SHOWDECIMAL of EXP |
            FROMDECIMAL of EXP |
            TODECIMAL of EXP |
            NOT of EXP |
            AND of EXP*EXP |
            OR of EXP*EXP |
            RATPLUS of EXP*EXP |
            RATSUB of EXP*EXP |
            RATTIMES of EXP*EXP |
            RATDIV of EXP*EXP |
            NEG of EXP |
            PLUS of EXP*EXP |
            SUB of EXP*EXP |
            TIMES of EXP*EXP |
            DIV of EXP*EXP |
            MOD of EXP*EXP |
            LT of EXP*EXP |
            LE of EXP*EXP |
            GT of EXP*EXP |
            GE of EXP*EXP |
            EQ of EXP*EXP |
            NE of EXP*EXP |
            TRUE |
            FALSE |
            INTEGER_VALUE of BigInt.bigint |
            IDENTIFIER of string |
            RATIONAL_VALUE of ExpOp.rational 
        
end ; 