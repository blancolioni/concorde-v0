--  with Concorde.Primitives.Aggregates;
with Concorde.Primitives.Operators;

package body Concorde.Primitives.Loader is

   ---------------------
   -- Load_Primitives --
   ---------------------

   procedure Load_Primitives is
   begin
      Map.Insert ("max", Concorde.Primitives.Operators.Max);
      Map.Insert ("min", Concorde.Primitives.Operators.Min);
--        Map.Insert ("minimum", Concorde.Primitives.Aggregates.Minimum);
--        Map.Insert ("maximum", Concorde.Primitives.Aggregates.Maximum);
--        Map.Insert ("sum", Concorde.Primitives.Aggregates.Sum);
--        Map.Insert ("product", Concorde.Primitives.Aggregates.Product);
   end Load_Primitives;

end Concorde.Primitives.Loader;
