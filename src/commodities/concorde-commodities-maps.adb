package body Concorde.Commodities.Maps is

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Map;
      Commodity : Commodity_Reference)
      return Boolean
   is
   begin
      return Container.Contains (Tag (Commodity));
   end Contains;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Map;
      Commodity : Commodity_Reference)
      return Element_Type
   is
   begin
      return Commodity_Maps.Map (Container).Element (Tag (Commodity));
   end Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Commodity : Commodity_Reference;
      Element   : Element_Type)
   is
   begin
      Container.Insert (Tag (Commodity), Element);
   end Insert;

end Concorde.Commodities.Maps;
