private package Concorde.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier,
       Tok_Integer_Constant, Tok_Float_Constant,
       Tok_String_Constant, Tok_Character_Constant,

       Tok_Delay, Tok_Sin, Tok_Smooth,

       Tok_Comma, Tok_Colon, Tok_Semicolon, Tok_Dot, Tok_Percent,
       Tok_Left_Paren, Tok_Right_Paren,
       Tok_Left_Arrow,
       Tok_Plus, Tok_Minus, Tok_Asterisk, Tok_Slash, Tok_Power,
       Tok_EQ, Tok_NE, Tok_GT, Tok_LT, Tok_GE, Tok_LE);

end Concorde.Parser.Tokens;
