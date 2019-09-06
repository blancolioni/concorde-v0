private with WL.String_Maps;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Concorde.Commodities.Maps is

   type Map is tagged private;

   function Contains
     (Container : Map;
      Commodity : Commodity_Reference)
      return Boolean;

   function Element
     (Container : Map;
      Commodity : Commodity_Reference)
      return Element_Type;

   procedure Insert
     (Container : in out Map;
      Commodity : Commodity_Reference;
      Element   : Element_Type);

private

   package Commodity_Maps is
     new WL.String_Maps (Element_Type, "=");

   type Map is
     new Commodity_Maps.Map with null record;

end Concorde.Commodities.Maps;
