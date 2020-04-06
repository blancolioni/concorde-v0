private with WL.String_Maps;

with Concorde.Signals;

package Concorde.Transformers is

   type Transformer_Type is abstract tagged private;

private

   package Parameter_Maps is
     new WL.String_Maps (Concorde.Signals.Signal_Type'Class,
                         Concorde.Signals."=");

   type Transformer_Type is abstract tagged
      record
         Input_Signals  : Parameter_Maps.Map;
         Output_Signals : Parameter_Maps.Map;
      end record;

end Concorde.Transformers;
