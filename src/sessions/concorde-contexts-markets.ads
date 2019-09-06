package Concorde.Contexts.Markets is

   function Top_Level_Container return Context_Type;

   function Market_Context
     (Market : Concorde.Db.Market_Reference)
      return Context_Type;

end Concorde.Contexts.Markets;
